supply_table_to_rdf <- function(store, country, year, file_name, sheet_name, cell_range, prefixes){
  
  product_prefix = prefixes$product_prefix
  activity_prefix = prefixes$activity_prefix
  
  if (sheet_name == "Monetary"){
    supply_table_type = "Monetary_Supply_Table"
  } else if (sheet_name == "T dry"){
    supply_table_type = "Physical_Supply_Table_Dry"
  } else if (sheet_name == "V'T and UT data entry"){    
    supply_table_type = "Physical_Supply_Table_Wet"
  } else {
    stop("Not sure which type of supply table (monetary or physical) will be processed")
  }
  
  supply_table = read_xls(file_name, sheet=sheet_name, range=cell_range, col_names=FALSE)
  
  non_zero_indices_supply_table = which(supply_table != 0, arr.ind=TRUE)
  
  # data frame representing non-zero values in the supply table
  supply_df = data.frame(product = paste0(product_prefix, non_zero_indices_supply_table[,1]), 
                         activity = paste0(activity_prefix, non_zero_indices_supply_table[,2]), 
                         cell_id = paste0("http://bonsai.uno/data/FORWAST/", 
                                          country, "/", 
                                          year, "/", 
                                          supply_table_type, 
                                          "/Product/", non_zero_indices_supply_table[,1],
                                          "/Activity/", non_zero_indices_supply_table[,2]), 
                         
                         value = as.matrix(supply_table)[non_zero_indices_supply_table])
  
  supply_table = paste0("http://bonsai.uno/data/FORWAST/", 
                        country, "/", 
                        year, 
                        "/", supply_table_type)
  
  add.triple(store,
             subject=supply_table,
             predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             object = paste0("http://bonsai.uno/data/FORWAST/", supply_table_type))
  
  add.data.triple(store,
                  subject=supply_table,
                  predicate = "http://bonsai.uno/data/FORWAST/Property/Country",
                  data = country)
  
  add.data.triple(store,
                  subject=supply_table,
                  predicate = "http://bonsai.uno/data/FORWAST/Property/Year",
                  data = paste0(as.character(year), "^^xsd:integer"))
  
  for (i in sequence(nrow(supply_df))){
    subject = supply_df$cell_id[i]
    value = supply_df$value[i]
    product = supply_df$product[i]
    activity = supply_df$activity[i]
    
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
               predicate = "http://bonsai.uno/data/FORWAST/Property/SupplyTable",
               object = supply_table)
    
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