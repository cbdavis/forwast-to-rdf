#' @import dplyr
#' @import tidyr
supply_table_to_rdf <- function(store, country, year, file_name, sheet_name, cell_range, prefixes){
  
  product_prefix = prefixes$product_prefix
  activity_prefix = prefixes$activity_prefix
  
  if (sheet_name == "Monetary"){
    supply_table_type = "Monetary_Supply_Table"
  } else if (sheet_name == "T dry"){
    supply_table_type = "Physical_Supply_Table_Dry"
  } else if (sheet_name == "V'T and UT data entry"){    
    supply_table_type = "Physical_Supply_Table_Wet"
  } else if (sheet_name == "P"){    
    supply_table_type = "Price_Supply_Table"
  } else {
    stop("Not sure which type of supply table (monetary or physical) will be processed")
  }
  
  # sheet names may have upper case and lower case variants: "T dry" and "T Dry"
  # make sure that we can match on both
  sheet_index = which(tolower(excel_sheets(file_name)) == tolower(sheet_name))
  
  supply_table = readxl::read_xls(file_name, sheet=sheet_index, range=cell_range, col_names=FALSE)
  
  if (nrow(supply_table) > 0){
    
    non_zero_indices_supply_table = which(supply_table != 0, arr.ind=TRUE)
    
    if (nrow(non_zero_indices_supply_table) > 0){
      
      
      
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
      
      rrdf::add.triple(store,
                       subject=supply_table,
                       predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                       object = paste0("http://bonsai.uno/data/FORWAST/", supply_table_type))
      
      rrdf::add.data.triple(store,
                            subject=supply_table,
                            predicate = "http://bonsai.uno/data/FORWAST/Property/Country",
                            data = country)
      
      rrdf::add.data.triple(store,
                            subject=supply_table,
                            predicate = "http://bonsai.uno/data/FORWAST/Property/Year",
                            data = paste0(as.character(year), "^^xsd:integer"))
      
      for (i in sequence(nrow(supply_df))){
        subject = supply_df$cell_id[i]
        value = supply_df$value[i]
        product = supply_df$product[i]
        activity = supply_df$activity[i]
        
        rrdf::add.triple(store,
                         subject=subject,
                         predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                         object = "http://bonsai.uno/data/FORWAST/TableCell")
        
        rrdf::add.data.triple(store,
                              subject=subject,
                              predicate = "http://bonsai.uno/data/FORWAST/Property/Value",
                              data = paste0(as.character(value), "^^xsd:double"))
        
        rrdf::add.triple(store,
                         subject=subject,
                         predicate = "http://bonsai.uno/data/FORWAST/Property/SupplyTable",
                         object = supply_table)
        
        rrdf::add.triple(store,
                         subject=subject,
                         predicate = "http://bonsai.uno/data/FORWAST/Property/Product",
                         object = product)
        
        rrdf::add.triple(store,
                         subject=subject,
                         predicate = "http://bonsai.uno/data/FORWAST/Property/Activity",
                         object = activity)
      }
    } else {
      warning(paste("No non-zero data found for table - file:", file_name, ", sheet:", sheet_name, ", country:", country))
    }
    
  } else {
    warning(paste("No data found for table - file:", file_name, ", sheet:", sheet_name, ", country:", country))   
  }
  return(store)
}