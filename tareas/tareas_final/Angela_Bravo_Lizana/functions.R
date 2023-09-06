# 1.1 Función extract_name

extract_name <- function(data) {
  data %>%  
    str_extract(pattern = "esi-[[:digit:]]{4}---personas.csv")
}


# 1.2 Función download_esi_data

download_esi_data <- function(url, file_name, directory = "/data") {
  suppressWarnings(try(dir.create("data")))           # al crear directamente la carpeta "data" en
  directory <- paste0(getwd(),directory)              # la función, se ejecuta un warning cuando
  file_path <- file.path(directory, file_name)        # lee los demás archivos, ya que la carpeta
  download(url, destfile = file_path, mode = "wb")    # ya se encuentra creada, por tanto se usa
}                                                     # "suppressWarnings" para evitar esto


# 2 Función read_esi_data

read_esi_data<- function(ruta = "data/") {
  files <- list.files("data/", full.names = T) %>% 
    set_names(str_extract(., "esi-[[:digit:]]{4}---personas.csv")) %>% 
    imap(~fread(.x))
}

# ~fread(.), ~read.csv(.), ~read_csv(.): se intentó leer con estas 3 funciones y 
# fread fue el que permitió leer distintos separados de csv automaticamente


# 4.3 Lista de tablas: calcular promedio con herramientas de purrr, utilizando una función 
# creada por ti, que utilice data.table.

funcion <- function(data) {
  results <- map_df(list(data), ~.x %>% select("version", "ingreso")) %>% 
    group_by(version) %>% summarize(mean(ingreso))
}

