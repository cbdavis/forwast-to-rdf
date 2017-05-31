activities_to_rdf <- function(store, file_name, sheet_name, cell_range_activity_names, cell_range_activity_numbers, prefixes){

  activity_prefix = prefixes$activity_prefix
  
  activity_names = read_xls(file_name, 
                            sheet=sheet_name, 
                            range=cell_range_activity_names, 
                            col_names=FALSE) %>% 
    unlist() %>% as.character()
  
  activity_numbers = read_xls(file_name, 
                              sheet=sheet_name, 
                              range=cell_range_activity_numbers, 
                              col_names=FALSE) %>% 
    unlist() %>% as.numeric()

  for (activity_number in sequence(activity_numbers)){
    
    subject = paste0(activity_prefix, activity_number)
    
    add.data.triple(store,
                    subject=subject,
                    predicate = "http://www.w3.org/2004/02/skos/core#notation",
                    data = as.character(activity_number))
    
    add.data.triple(store,
                    subject=subject,
                    predicate = "http://www.w3.org/2004/02/skos/core#prefLabel",
                    data = activity_names[activity_number])

    # activities are types of products
    add.triple(store,
               subject = subject,
               predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
               object = gsub("/$", "", activity_prefix))
    
  }
  return(store)
}