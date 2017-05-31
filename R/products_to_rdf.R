products_to_rdf <- function(store, file_name, sheet_name, cell_range_product_names, cell_range_product_numbers, cell_range_nace_links, prefixes){
  
  product_prefix = prefixes$product_prefix
  nace_prefix = prefixes$nace_prefix
  
  #### This is only done for the supply table
  nace_link = read_xls(file_name, sheet=sheet_name, range=cell_range_nace_links, col_names=FALSE) %>% unlist() %>% as.character()
  product_numbers = read_xls(file_name, sheet=sheet_name, range=cell_range_product_numbers, col_names=FALSE) %>% unlist() %>% as.numeric()
  product_names = read_xls(file_name, sheet=sheet_name, range=cell_range_product_names, col_names=FALSE) %>% unlist() %>% as.character()
  
  # if the nace_link is just numbers and periods, then no extra processing has to be done
  
  links_to_parse = which(!grepl("^[0-9\\.]+$", nace_link))
  direct_matching_links = which(grepl("^[0-9\\.]+$", nace_link))
  
  # need to split based on "+"
  # links to NACE can look like "45.1(disaggr.)+45.21(disaggr.)+45.22+45.3+45.4+45.5(disaggr.)"
  for (index in direct_matching_links){
    subject = paste0(product_prefix, index)
    
    add.data.triple(store,
                    subject=subject,
                    predicate = "http://www.w3.org/2004/02/skos/core#notation",
                    data = as.character(index))
    
    add.data.triple(store,
                    subject=subject,
                    predicate = "http://www.w3.org/2004/02/skos/core#prefLabel",
                    data = product_names[index])
    
    item = nace_link[index]
    
    object = paste0(nace_prefix, item)
    
    add.triple(store,
               subject=subject,
               predicate = "http://www.w3.org/2004/02/skos/core#exactMatch",
               object = object)
    
    # products are types of products
    add.triple(store,
               subject = subject,
               predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
               object = product_prefix)
    
  }
  
  for (index in links_to_parse){
    
    subject = paste0(product_prefix, index)
    
    add.data.triple(store,
                    subject=subject,
                    predicate = "http://www.w3.org/2004/02/skos/core#notation",
                    data = as.character(index))
    
    add.data.triple(store,
                    subject=subject,
                    predicate = "http://www.w3.org/2004/02/skos/core#prefLabel",
                    data = product_names[index])

    # products are types of products
    add.triple(store,
               subject = subject,
               predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
               object = gsub("/$", "", product_prefix))
    
    link_text = nace_link[index]
    items = unlist(strsplit(link_text, "\\+"))
    for (item in items){
      if (grepl("\\(disaggr\\.\\)", item)) { # disaggregation of existing NACE code, (likely) shared with another product code
        print(paste(item, "disaggregation"), sep=" - ")
        
        object = paste0(nace_prefix, gsub("\\(disaggr\\.\\)", "", item))
        
        add.triple(store,
                   subject=subject,
                   predicate = "http://www.w3.org/2004/02/skos/core#skos:relatedMatch",
                   object = object)
        
        add.triple(store,
                   subject=object,
                   predicate = "http://www.w3.org/2004/02/skos/core#skos:relatedMatch",
                   object = subject)
        
      } else if (grepl("\\(ext\\.\\)", item)) { # extended(?) 
        print(paste(item, "extended"), sep=" - ")
        
        object = paste0(nace_prefix, gsub("\\(ext\\.\\)", "", item))
        
        # TODO need to figure out how to map "ext" in a better way
        
        add.triple(store,
                   subject=subject,
                   predicate = "http://www.w3.org/2004/02/skos/core#skos:narrowMatch",
                   object = object)
        
        add.triple(store,
                   subject=object,
                   predicate = "http://www.w3.org/2004/02/skos/core#skos:broadMatch",
                   object = subject)
        
      } else if (grepl("n\\.a\\.", item)) { # no mapping to NACE code
        print(paste(item, "no mapping"), sep=" - ")
        
        # Nothing needed here
        
      } else if (grepl("^[0-9\\.]+$", item)) { # just a number by itself
        print(paste(item, "just a number"), sep=" - ")
        
        object = paste0(nace_prefix, item)
        
        add.triple(store,
                   subject=subject,
                   predicate = "http://www.w3.org/2004/02/skos/core#skos:narrowMatch",
                   object = object)
        
        add.triple(store,
                   subject=object,
                   predicate = "http://www.w3.org/2004/02/skos/core#skos:broadMatch",
                   object = subject)
      } else {  # not sure what we're looking at
        stop(print(paste(item, "not sure how to interpret product code notation"), sep=" - "))
      }
    }
  }

  return(store)
}