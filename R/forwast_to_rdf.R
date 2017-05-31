#never ever convert strings to factors
#options(stringsAsFactors = FALSE)

#library(readxl)
#library(dplyr)
#library(tidyr)

# See https://github.com/egonw/rrdf
#library(rrdf)

#' @import rrdf
#' @import readxl
#' @import dplyr
#' @import tidyr
forwast_to_rdf <- function(output_file = "forwast.ttl", config_file = "./inst/files.yaml"){
  download_forwast()
  # create new RDF store
  store = rrdf::new.rdf()
  
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
  
  initialized_products_and_activities = FALSE
  
  config = yaml::yaml.load_file(config_file)
  
  for (file in config$files){
    for (spreadsheet in file$spreadsheets){
      if (spreadsheet$dim == 117){
        file_name = spreadsheet$name
        print(spreadsheet$country)
        store = country_data_to_rdf(store, country = spreadsheet$country, 
                                    file_name = spreadsheet$name, 
                                    year, prefixes)
        
        if (!initialized_products_and_activities){
          # products and activities are only read from a single Excel file, assume that they're the same for all spreadsheets
          store = products_to_rdf(store, file_name, sheet_name="Monetary", 
                                  cell_range_product_names="F7:F123", 
                                  cell_range_product_numbers="E7:E123", 
                                  cell_range_nace_links="D7:D123", 
                                  prefixes)
          
          store = activities_to_rdf(store, file_name, sheet_name="Monetary", 
                                    cell_range_activity_names="G5:DS5", 
                                    cell_range_activity_numbers="G4:DS4", 
                                    prefixes)
          
          initialized_products_and_activities = TRUE
        }
      }
    }
  }

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