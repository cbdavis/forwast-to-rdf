#never ever convert strings to factors
#options(stringsAsFactors = FALSE)

#library(readxl)
#library(dplyr)
#library(tidyr)

# See https://github.com/egonw/rrdf
#library(rrdf)

forwast_to_rdf <- function(output_file = "FORWAST.ttl"){
  
  # create new RDF store
  store = new.rdf()
  
  ############## Assumptions ############## 
  # * Supply & Use tables have same dimensions (not including summary columns)
  
  # Data Entry URLs will be like: 
  # http://bonsai.uno/data/FORWAST/Country/Year/Supply/Product/1/Activity/2
  # http://bonsai.uno/data/FORWAST/Country/Year/Use/Product/1/Activity/2
  
  # Products and Activities:
  # http://bonsai.uno/data/FORWAST/Product/1
  # http://bonsai.uno/data/FORWAST/Activity/1
  
  prefixes = list(product_prefix = "http://bonsai.uno/Data/FORWAST/Product/", 
                  activity_prefix = "http://bonsai.uno/Data/FORWAST/Activity/",
                  nace_prefix = "http://bonsai.uno/Classifications/NACE/")
  
  # Also need to take care of summary statistics present in the tables
  
  year = 2010
  
  # products and activities are only read from a single Excel file, assume that they're the same for all spreadsheets
  store = products_to_rdf(store, file_name="./Forwast_D32_databasesWP3_1(Austria+Denmark)/Austria Supply_and_Use_Table_Master_9_RMA_20100202(3).xls", sheet_name="Monetary", 
                          cell_range_product_names="F7:F123", 
                          cell_range_product_numbers="E7:E123", 
                          cell_range_nace_links="D7:D123", 
                          prefixes)
  
  store = activities_to_rdf(store, file_name="./Forwast_D32_databasesWP3_1(Austria+Denmark)/Austria Supply_and_Use_Table_Master_9_RMA_20100202(3).xls", sheet_name="Monetary", 
                            cell_range_activity_names="G5:DS5", 
                            cell_range_activity_numbers="G4:DS4", 
                            prefixes)
  
  store = country_data_to_rdf(store, country="Austria", 
                              file_name="./Forwast_D32_databasesWP3_1(Austria+Denmark)/Austria Supply_and_Use_Table_Master_9_RMA_20100202(3).xls", 
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "Denmark", 
                              file_name = "./Forwast_D32_databasesWP3_1(Austria+Denmark)/DK Supply and Use Table Master 12 (117x117) 20090915.xls",
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "France", 
                              file_name = "./Forwast_D32_databasesWP3_2(France+Germany)/Fr_Supply and Use Table Master 11_06_10_2009.xls",
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "Germany",
                              file_name = "./Forwast_D32_databasesWP3_2(France+Germany)/GER_Supply_and_Use_Table_Master_9_ISWA.xls",
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "Greece",
                              file_name = "./Forwast_D42_DatabasesWP4_3(GR+LU+MT)/GREECE-Supply and Use Table Master 11 (117x117)(21-10-2009).xls",
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "Netherlands",
                              file_name = "./Forwast_D42_DatabasesWP4_4(NL+PL)/NL_Supply and Use Table Master 11_30_09_2009.xls",
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "Poland",
                              file_name = "./Forwast_D42_DatabasesWP4_4(NL+PL)/POLAND Master 11_ 15.04.2009.xls",
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "Sweden",
                              file_name = "./Forwast_D42_DatabasesWP4_5(PT+RO+SW)/Sweden Supply and Use Table Master 12 (117x117)20090901.xls",
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "Ireland",
                              file_name = "./Forwast_D42_DatabasesWP4_NV1(IR+IT)/IRL Supply and Use Master 9 ISWA 10E06.xls",
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "Slovenia",
                              file_name = "./Forwast_D42_DatabasesWP4_NV2(SL+SP+UK)/Slovenia Supply_and_Use_Table_Master_9_RMA_2009_09_17.xls",
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "Spain",
                              file_name = "./Forwast_D42_DatabasesWP4_NV2(SL+SP+UK)/Spain SUT 04112009.xls",
                              year, prefixes)
  
  store = country_data_to_rdf(store, country = "United Kingdom",
                              file_name = "./Forwast_D42_DatabasesWP4_NV2(SL+SP+UK)/UK Supply and Use Master 9 ISWA 10E05.xls",
                              year, prefixes)
  
  # Need to figure out how to read these
  # 57x57
  # These seem to just use NACE codes
  # /home/cbdavis/Desktop/svn/ChrisDavis/Projects/Bonsai/FORWAST/Forwast_D42_DatabasesWP4_2((CZ+SL+HU)+(EE+LV+LT)+FI)/CZECH-SLOVAKIA-HUNGARY-Master 11 (57x57) 25.06.2009.xls
  # /home/cbdavis/Desktop/svn/ChrisDavis/Projects/Bonsai/FORWAST/Forwast_D42_DatabasesWP4_2((CZ+SL+HU)+(EE+LV+LT)+FI)/EE-LV-LT Master 11(57x57) 19.10.2009.xls
  # /home/cbdavis/Desktop/svn/ChrisDavis/Projects/Bonsai/FORWAST/Forwast_D42_DatabasesWP4_2((CZ+SL+HU)+(EE+LV+LT)+FI)/FINLAND Supply and Use Table Master 11 57x57 5.10.2009.xls
  # /home/cbdavis/Desktop/svn/ChrisDavis/Projects/Bonsai/FORWAST/Forwast_D42_DatabasesWP4_3(GR+LU+MT)/LUxembourg_Supply and Use Table Master 11 (57x57).xls
  # /home/cbdavis/Desktop/svn/ChrisDavis/Projects/Bonsai/FORWAST/Forwast_D42_DatabasesWP4_3(GR+LU+MT)/MALTA_Supply and Use Table Master 11 (57x57).xls
  # /home/cbdavis/Desktop/svn/ChrisDavis/Projects/Bonsai/FORWAST/Forwast_D42_DatabasesWP4_5(PT+RO+SW)/PORTUGAL-Supply and Use Table Master 11 (57x57)(02-10-2009).xls
  # /home/cbdavis/Desktop/svn/ChrisDavis/Projects/Bonsai/FORWAST/Forwast_D42_DatabasesWP4_5(PT+RO+SW)/RO_Supply and Use Table Master 11 (57x57)_30_09_2009.xls
  # /home/cbdavis/Desktop/svn/ChrisDavis/Projects/Bonsai/FORWAST/Forwast_D42_DatabasesWP4_NV1(IR+IT)/ITaly Supply and Use Master 11 (57x57).xls
  
  save.rdf(store, filename=output_file, format="TURTLE")
  
  ### Need to have a few demos showing why RDF may be useful
  # links to NACE could be interesting
  
}