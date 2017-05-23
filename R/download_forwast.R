download_forwast <- function(config_file = "./inst/files.yaml"){
  config = yaml::yaml.load_file(config_file)
  for (file in config$files){
    url = file$url
    short_file_name = tail(strsplit(url, "/")[[1]], n=1)
    if (!file.exists(short_file_name)){
      print(paste("Downloading", short_file_name))
      download.file(url, short_file_name)
    } else {
      print(paste("Already downloaded", short_file_name))
    }
    unzip(short_file_name)
  }
}