download_forwast <- function(){

  # should just dump the list of files in a YAML file to enable easier configuration
    
  if (!file.exists("Forwast_D32_databasesWP3_1(Austria+Denmark).zip")){
    download.file("http://forwast.brgm.fr/Documents/Deliverables/Forwast_D32_databasesWP3_1(Austria+Denmark).zip")
  }

  if (!file.exists("Forwast_D32_databasesWP3_2(France+Germany).zip")){
    download.file("http://forwast.brgm.fr/Documents/Deliverables/Forwast_D32_databasesWP3_2(France+Germany).zip")  
  }

  if (!file.exists("Forwast_D42_DatabasesWP4_1(BE+BU+CY).zip")){
    download.file("http://forwast.brgm.fr/Documents/Deliverables/Forwast_D42_DatabasesWP4_1(BE+BU+CY).zip")  
  }
  
  if (!file.exists("Forwast_D42_DatabasesWP4_2((CZ+SL+HU)+(EE+LV+LT)+FI).zip")){
    download.file("http://forwast.brgm.fr/Documents/Deliverables/Forwast_D42_DatabasesWP4_2((CZ+SL+HU)+(EE+LV+LT)+FI).zip")  
  }
  
  if (!file.exists("Forwast_D42_DatabasesWP4_3(GR+LU+MT).zip")){
    download.file("http://forwast.brgm.fr/Documents/Deliverables/Forwast_D42_DatabasesWP4_3(GR+LU+MT).zip")  
  }
  
  if (!file.exists("Forwast_D42_DatabasesWP4_4(NL+PL).zip")){
    download.file("http://forwast.brgm.fr/Documents/Deliverables/Forwast_D42_DatabasesWP4_4(NL+PL).zip")  
  }
  
  if (!file.exists("Forwast_D42_DatabasesWP4_5(PT+RO+SW).zip")){
    download.file("http://forwast.brgm.fr/Documents/Deliverables/Forwast_D42_DatabasesWP4_5(PT+RO+SW).zip")  
  }
  
  if (!file.exists("Forwast_D42_DatabasesWP4_NV1(IR+IT).zip")){
    download.file("http://forwast.brgm.fr/Documents/Deliverables/Forwast_D42_DatabasesWP4_NV1(IR+IT).zip")  
  }
  
  if (!file.exists("Forwast_D42_DatabasesWP4_NV2(SL+SP+UK).zip")){
    download.file("http://forwast.brgm.fr/Documents/Deliverables/Forwast_D42_DatabasesWP4_NV2(SL+SP+UK).zip")  
  }
  
}