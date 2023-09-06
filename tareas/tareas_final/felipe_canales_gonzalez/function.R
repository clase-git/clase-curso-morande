
#Funciones

# Función para extraer nombres de archivos a descargar ##

extract_name = function(urls){
  str_extract_all(urls,"esi-20\\d{2}")  %>% 
    str_replace_all("-", "_")
} 

#Funcion para descargar archivos 
download_esi_data <- function(urls, file_names, directory) {
  file_directory <- paste0(directory, "/", file_names)
  curl_download(urls,  file_directory)
}

# Funcion para abrir archivos
read_esi_data <- function(directory) {
  fread(directory)
}
