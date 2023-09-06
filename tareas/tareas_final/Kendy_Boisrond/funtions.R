

# Función para extraer nombres de archivo de las URL
extract_name <- function(url) {
  file_name <- str_extract(url, pattern = "\\w+-\\d+---.\\w+.[a-z]{3}")
  return(file_name)
}

file_names <- map(urls, extract_name)

download_esi_data <- function(url, file_name, directory) {
  download.file(url, destfile = file.path(directory, file_name), mode = "wb")
}


# Función para leer archivos CSV con detección automática de separador
read_esi_data <- function(file_path) {
  data <- fread(file_path)
  return(data)
}

# Leer los archivos descargados y almacenarlos en una lista
data_esi <- map(file.path(directory, file_names), read_esi_data)













