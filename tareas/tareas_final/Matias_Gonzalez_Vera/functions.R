# Archivo de funciones #
# Curso de r intermedio #

# Crear función para extraer nombres de url
extract_name <- function(url){
  name <- str_extract({{url}}, pattern = "[a-z]{3}-\\d{4}-+[a-z]{8}.[a-z]{3}")
  return(name)
}

nombres <- extract_name(urls)

# Crear función para descargar bases

download_esi_data <- function(url, file_name, directory){
  if(!dir.exists(directory)) dir.create(directory)
  directorio <- paste0(directory,file_name)
  download.file(url, directorio)
}

# Crear función para lectura de datos
read_esi_data <- function(ruta){
  files <- list.files(ruta, full.names = T)
  bases_esi <<- files %>% 
    map(fread)
}

# Crear función para calcular promedio en data.table
promedio_dt <- function(lista){
  base <- bind_rows(lista)
  base[, .(promedio = mean(ing_t_p, na.rm = T)), by = version]
}


