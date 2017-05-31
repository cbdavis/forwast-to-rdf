#never ever convert strings to factors
options(stringsAsFactors = FALSE)

library(SPARQL)

# want to add in prefixes where possible
addInPrefixes <- function(df, prefixes){
  # do a search and replace
  for (i in c(1:length(prefixes))){
    for (j in c(1:ncol(df))){
      fullURL = prefixes[i]
      prefix = paste(names(prefixes)[i], ":", sep="")
      df[,j] = gsub(fullURL, prefix, df[,j], fixed=TRUE) # no regular expression, do literal replacement
      df[,j] = gsub("^<|>$", "", df[,j])
    }
  }
  return(df)
}

addPrefixesToQuery <- function(prefixString, queryString){
  return(paste(prefixString, queryString, sep="\n"))
}

getAllSubjectTypes <- function(endpoint, prefixes){
  queryString = addPrefixesToQuery(prefixString, 
                                   "select ?subjectType (count(?subjectType) as ?subjectTypeCount) where {
                                   ?s rdf:type ?subjectType
                                   } group by ?subjectType")
  queryResults = SPARQL(url=endpoint, query=queryString)
  df = queryResults$results
  df$subjectType = gsub("<|>", "", df$subjectType)
  return(df)    
}

getAllDataTypeProperties <- function(endpoint, prefixes, subjectType){
  queryString = addPrefixesToQuery(prefixString, 
                                   paste0("select ?p (count(?p) as ?count) (?dataType as ?object) where {
                                    BIND(<", subjectType, "> as ?subjectType) . 
                                    ?s rdf:type ?subjectType . 
                                    ?s ?p ?o . 
                                    filter(?p != rdf:type) . 
                                    OPTIONAL {?o rdf:type ?z} . 
                                    FILTER(!BOUND(?z)) . 
                                    BIND(datatype(?o) as ?dataType) . 
                                    } group by ?subjectType ?p ?dataType order by ?subjectType ?p ?dataType"))
  queryResults = SPARQL(url=endpoint, query=queryString)
  df = queryResults$results
  df$object = gsub("<|>", "", df$object)
  df$p = gsub("<|>", "", df$p)
  return(df)    
}

getAllObjectProperties <- function(endpoint, prefixes, subjectType){
  queryString = addPrefixesToQuery(prefixString, 
                                   paste0("select ?p (count(?p) as ?count) (?objType as ?object) where {
                                            BIND(<", subjectType, "> as ?subjectType) . 
                                            ?s rdf:type ?subjectType . 
                                            ?s ?p ?o . 
                                            filter(?p != rdf:type) . 
                                            ?o rdf:type ?objType .
                                          } group by ?subjectType ?p ?objType order by ?p ?object"))
  queryResults = SPARQL(url=endpoint, query=queryString)
  df = queryResults$results
  df$object = gsub("<|>", "", df$object)
  df$p = gsub("<|>", "", df$p)
  return(df)    
}

# clean up characters so that graphviz will be able to make a link to a part of the table
makeLinkPort <- function(text){
  text = gsub(":|\\.|/", "_", text)
  return(text)
}

endpoint = "http://localhost:9999/blazegraph/sparql"

prefixes = c()
prefixes["rdfs"] = "http://www.w3.org/2000/01/rdf-schema#"
prefixes["rdf"] = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
prefixes["skos"] = "http://www.w3.org/2004/02/skos/core#"
prefixes["fw"] = "http://bonsai.uno/data/FORWAST"
prefixes["fwprop"] = "http://bonsai.uno/data/FORWAST/Property/"

prefixString = paste("PREFIX ", names(prefixes), ": <", prefixes, ">\n", collapse="", sep="")

# figure out how many objects we have for each type of subject count
subjectTypesAndCounts = getAllSubjectTypes(endpoint, prefixes)

table_text = 'digraph G {\nrankdir=LR\nsize="50,50"\n\n'

for (i in sequence(nrow(subjectTypesAndCounts))){
  subjectType = subjectTypesAndCounts$subjectType[i]
  subjectCount = subjectTypesAndCounts$subjectTypeCount[i]

  print(subjectType)
    
  table_text = paste0(table_text, "node [shape=plaintext]\n",
                      '"', makeLinkPort(subjectType), '" [label=<\n',
                      '<TABLE border="0" cellborder="1" cellspacing="0">\n',
                      '<tr>',
                      '<td colspan="3" port="', makeLinkPort(subjectType), '" bgcolor="yellow">',
                      subjectType, ' (Count: ', as.character(as.numeric(subjectCount)), ')',
                      '</td></tr>\n',
                      '<tr><td colspan="3" port="PROPERTIES" bgcolor="pink">PROPERTIES</td></tr>\n', 
                      '<tr>',
                      '<td><FONT FACE="Times-Italic">Count</FONT></td>',
                      '<td><FONT FACE="Times-Italic">Name</FONT></td>',
                      '<td><FONT FACE="Times-Italic">Type</FONT></td>',
                      '</tr>\n')
  
  df1 = getAllDataTypeProperties(endpoint, prefixes, subjectType)
  df2 = getAllObjectProperties(endpoint, prefixes, subjectType)
  df = rbind(df1, df2)
  
  linkText = ""
  
  for (j in sequence(nrow(df))){
    print(paste0("     ", df$p[j]))
    table_text = paste0(table_text, '<tr>', 
                        '<td>', as.character(as.numeric(df$count[j])), '</td>', 
                        '<td ALIGN="LEFT">', df$p[j], '</td>', 
                        '<td port="', makeLinkPort(df$p[j]), '">', df$object[j], '</td>', 
                        '</tr>\n')
    
    if (df$object[j] %in% subjectTypes){
      linkText = paste0(linkText, 
                        makeLinkPort(subjectType), ':', makeLinkPort(df$p[j]), ' -> ', 
                        makeLinkPort(df$object[j]), ":", makeLinkPort(df$object[j]), '\n')
    }

  }

  table_text = paste0(table_text, '</TABLE>>];\n\n')
  table_text = paste0(table_text, linkText)
  table_text = paste0(table_text, '\n\n')
  
}

table_text = paste0(table_text, '}\n')

write(table_text, file="/home/cbdavis/Desktop/forwast.dot")

# run this to generate the schema visualization
# dot -Tpng -o forwast.png forwast.dot