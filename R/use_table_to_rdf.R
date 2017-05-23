use_table_to_rdf <- function(store, country, year, file_name, sheet_name, cell_range, prefixes){
  
  product_prefix = prefixes$product_prefix
  activity_prefix = prefixes$activity_prefix
  
  if (sheet_name == "Monetary"){
    use_table_type = "Monetary_Use_Table"
  } else if (sheet_name == "T dry"){
    use_table_type = "Physical_Use_Table_Dry"
  } else if (sheet_name == "V'T and UT data entry"){    
    use_table_type = "Physical_Use_Table_Wet"
  } else {
    stop("Not sure which type of use table (monetary or physical) will be processed")
  }

  use_table = read_xls(file_name, sheet=sheet_name, range=cell_range, col_names=FALSE)
  non_zero_indices_use_table = which(use_table != 0, arr.ind=TRUE)
  
  # data frame representing non-zero values in the use table
  use_df = data.frame(product = paste0(product_prefix, non_zero_indices_use_table[,1]), 
                      activity = paste0(activity_prefix, non_zero_indices_use_table[,2]), 
                      cell_id = paste0("http://bonsai.uno/data/FORWAST/", 
                                       country, "/", 
                                       year, "/", 
                                       use_table_type, 
                                       "/Product/",non_zero_indices_use_table[,1],
                                       "/Activity/", non_zero_indices_use_table[,2]), 
                      value = as.matrix(use_table)[non_zero_indices_use_table])
  
  use_table = paste0("http://bonsai.uno/data/FORWAST/", 
                     country, "/", 
                     year, "/", 
                     use_table_type)
  
  add.triple(store,
             subject=use_table,
             predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             object = paste0("http://bonsai.uno/data/FORWAST/", use_table_type))
  
  add.data.triple(store,
                  subject=use_table,
                  predicate = "http://bonsai.uno/data/FORWAST/Property/Country",
                  data = country)
  
  add.data.triple(store,
                  subject=use_table,
                  predicate = "http://bonsai.uno/data/FORWAST/Property/Year",
                  data = paste0(as.character(year), "^^xsd:integer"))
  
  for (i in sequence(nrow(use_df))){
    subject = use_df$cell_id[i]
    value = use_df$value[i]
    product = use_df$product[i]
    activity = use_df$activity[i]
    
    add.triple(store,
               subject=subject,
               predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
               object = "http://bonsai.uno/data/FORWAST/TableCell")
    
    add.data.triple(store,
                    subject=subject,
                    predicate = "http://bonsai.uno/data/FORWAST/Property/Value",
                    data = paste0(as.character(value), "^^xsd:double"))
    
    add.triple(store,
               subject=subject,
               predicate = "http://bonsai.uno/data/FORWAST/Property/UseTable",
               object = use_table)
    
    add.triple(store,
               subject=subject,
               predicate = "http://bonsai.uno/data/FORWAST/Property/Product",
               object = product)
    
    add.triple(store,
               subject=subject,
               predicate = "http://bonsai.uno/data/FORWAST/Property/Activity",
               object = activity)
  }
  
  return(store)
}