extract_name <- function(url){
  str_extract_all(url, "esi-20\\d{2}---[a-z]{8}.csv")
}


download_esi_data <- function(url, file_name, directory="/data"){
  suppressWarnings(try(dir.create("data")))
  dir_final <- paste0(dir_base, directory)
  destino <- file.path(dir_final, file_name)
  download(url, destfile = destino, mode = "wb")
}

read_esi_data <- function(ruta){
  archivo <- paste0(dir_base,"/" , ruta)
  read_csv(archivo)
} 

mi_data_table <- function(df){
  df_dt <- as.data.table(df)
  dr_res <- df_dt[,mean({{ing_t_p}}),by = {{version}}]
}
