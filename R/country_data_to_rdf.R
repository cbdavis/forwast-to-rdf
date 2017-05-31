country_data_to_rdf <- function(store, country, file_name, year, prefixes){

  # can merge the supply and use code together into a single function, just pass some extra parameters
  # should at least have some code for table_to_rdf
  # should pass the function vectors that allow for construction of the cell IDs
  
  # replace spaces with underscores - needed for valid URIs
  country = gsub(" ", "_", country)
  
  store = supply_table_to_rdf(store, country, year, file_name, sheet_name="Monetary", cell_range="G7:DS123", prefixes)
  store = supply_table_to_rdf(store, country, year, file_name, sheet_name="T dry", cell_range="G7:DS123", prefixes)
  store = supply_table_to_rdf(store, country, year, file_name, sheet_name="V'T and UT data entry", cell_range="G7:DS123", prefixes)
  store = supply_table_to_rdf(store, country, year, file_name, sheet_name="P", cell_range="G7:DS123", prefixes)
  
  store = use_table_to_rdf(store, country, year, file_name, sheet_name="Monetary", cell_range="G128:DS244", prefixes)
  store = use_table_to_rdf(store, country, year, file_name, sheet_name="T dry", cell_range="G128:DS244", prefixes)
  store = use_table_to_rdf(store, country, year, file_name, sheet_name="V'T and UT data entry", cell_range="G128:DS244", prefixes)
  store = use_table_to_rdf(store, country, year, file_name, sheet_name="P", cell_range="G128:DS244", prefixes)
 
  # D is Product Transfer Coefficients
  # P is Price Matrix
  # K is Material Composition Matrix
  
  return(store)
}