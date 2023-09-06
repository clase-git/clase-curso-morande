# Trabajo Final

# EJERCICIO 1:


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               dplyr,
               haven,
               data.table,
               tictoc,
               renv)

## Lista de librarias
library(dplyr)
library(stringr)
library(ggplot2)
library(purrr)
library(data.table)
library(renv)
library(readr)
library(httr)

urls <- c(
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

# Función para extraer nombres de archivo de las URL
extract_name <- function(url) {
  file_name <- str_extract(url, pattern = "\\w+-\\d+---.\\w+.[a-z]{3}")
  return(file_name)
}

file_names <- map(urls, extract_name)

download_esi_data <- function(url, file_name, directory) {
  download.file(url, destfile = file.path(directory, file_name), mode = "wb")
}


directory <- "C:/Users/Kendy Boisrond/Desktop/data"

file_names <- map(urls, extract_name)
map2(urls, file_names, download_esi_data, directory = directory)

# EJERCICIO 2:

# Función para leer archivos CSV con detección automática de separador
read_esi_data <- function(file_path) {
  data <- fread(file_path)
  return(data)
}

# Leer los archivos descargados y almacenarlos en una lista
data_esi <- map(file.path(directory, file_names), read_esi_data)


# EJERCICIO 3:

library(dplyr)

# Crear un dataframe con las versiones, número de personas y número de hogares
version_info <- data.frame(
  version = paste0("esi_", 2016:2021),
  n_personas = map_int(data_esi, ~nrow(.x)),
  n_hogares = map_int(data_esi, ~length(unique(.x$id_identificacion)))
)


# Crear una función para calcular estadísticas del factor de expansión
calculate_stats <- function(data) {
  stats <- summary(data$fact_cal_esi)
  return(stats)
}

# Obtener estadísticas del factor de expansión para cada versión
fact_cal_stats <- map(data_esi, calculate_stats)
fact_cal_stats_df <- bind_rows(fact_cal_stats, .id = "version")


# Función para contar estratos con una sola unidad primaria de muestreo
count_single_strata <- function(data) {
  single_strata <- sum(table(data$estrato) == 1)
  return(single_strata)
}

# Obtener el número de estratos con una sola unidad primaria de muestreo por versión
single_strata_count <- map_int(data_esi, count_single_strata)
strata_info <- data.frame(
  version = paste0("esi_", 2016:2021),
  single_strata = single_strata_count
)

# Función para calcular estadísticas de ingresos del trabajo principal
calculate_income_stats <- function(data) {
  stats <- summary(data$ing_t_p * data$fact_cal_esi)
  return(stats)
}

# Obtener estadísticas de ingresos del trabajo principal para cada versión
income_stats <- map(data_esi, calculate_income_stats)
income_stats_df <- bind_rows(income_stats, .id = "version")

# EJERCICIO 4:
library(microbenchmark)
library(tictoc)

# Estrategia 1: Lista de tablas con purrr
tictoc::tic()
strategy_1 <- function(data_esi) {
  map_dbl(data_esi, ~mean(.x$ing_t_p))
}
tictoc::toc()

# Estrategia 2: Tablas apiladas con dplyr

tictoc::tic()
strategy_2 <- function(data_esi) {
  data_combined <- bind_rows(data_esi)
  result <- data_combined %>%
    group_by(version) %>%
    summarise(mean_income = mean(ing_t_p))
  return(result)
}
tictoc::toc()

# Estrategia 3: Lista de tablas con data.table
tictoc::tic()
strategy_3 <- function(data_esi) {
  result <- map_dbl(data_esi, ~mean(.x$ing_t_p))
  return(result)
}
tictoc::toc()

# Estrategia 4: Tablas apiladas con data.table

tictoc::tic()
strategy_4 <- function(data_esi) {
  data_combined <- rbindlist(data_esi)
  result <- data_combined[, .(mean_income = mean(ing_t_p)), by = version]
  return(result)
}
tictoc::toc()


# Primera forma con microbenhcmark:

benchmark_result_1 <- microbenchmark(
  "Strategy 1 (purrr)" = strategy_1(data_esi),
  "Strategy 2 (dplyr)" = strategy_2(data_esi),
  "Strategy 3 (purrr + data.table)" = strategy_3(data_esi),
  "Strategy 4 (data.table)" = strategy_4(data_esi),
  times = 5  # Número de iteraciones
)

# Imprimir el resultado del benchmark
print(benchmark_result_1)

# Segunda forma con microbenhcmark:

benchmark_result_2 <- microbenchmark(
  "Strategy 1 (purrr)" = lapply(strategy_1),
  "Strategy 2 (dplyr)" = lapply(strategy_2),
  "Strategy 3 (purrr + data.table)" = lapply(strategy_3),
  "Strategy 4 (data.table)" = (strategy_4),
  times = 5  # Número de iteraciones
)

# Imprimir el resultado del benchmark
print(benchmark_result_2)

# CONCLUSIONES A TRAVÉS DE TICTOC:
## Utilizando el paquete tictoc para decidir sobre la eficiencia, llegamos a la conclusión que 
## las estrategias lista de tablas con purr (1) y lista de tabla con "data.table"(3) son aquellas 
## que demoraron menos tiempo (alrededr de 0 secundo), por lo tanto mucho más eficiente que las 
## estratgias 2 y 4 cuyo resultados son de 0.01 y 0.02 segundo, respectivamente. Finalmente, los 
## no me es posible determinar la eficiencia con el enfoque de benchmark por errores de formato... 
## Por lo tanto, aconsejaría a mi jefatura desde el enfoque tictoc, utilizar lista de tabla con purr
## o lista de tabla con data.table de forma indiferente, por encima de las demás estrategias. 

