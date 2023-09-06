# 0. Encabezado ---------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,
               tidyverse,                          
               data.table,
               downloader,
               microbenchmark)

source(file = "functions.R", encoding = "UTF-8")

# 1. Ejercicio 1:descargar archivos -------------------------------------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

# 1.1 Función extract_name  y files Names

files_names <- extract_name(urls)

# 1.2 Función download_esi_data

# functions.R

# 1.3 Download data

walk2(urls, files_names, ~download_esi_data(.x, .y, directory = "/data"))


# 2. Ejercicio 2: leer archivos --------------------------------------------------------

esi <- read_esi_data(ruta = "data/")                 

# 3. Ejercicio 3: obtener datos --------------------------------------------------------

version <- list.files("data/", full.names = T) %>% 
  str_extract(., "[[:digit:]]{4}") %>% 
  str_replace(pattern = "-", replacement = "_")

esi <- map2(esi,
            version,
            ~.x %>% 
              mutate(version = .y))

# 3.1 Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion). 
# En la columna version debes usar la siguiente estructura: esi_{año}. Ejemplo: esi_2017

esi_tabla1 <- esi %>% 
  map(~.x %>% 
        select("version","idrph","id_identificacion"))

## solucion parche a error: Error in `bind_rows()`:
##! Can't combine `..1$id_identificacion` <double> and `..5$id_identificacion` <integer64>.

esi_tabla1[[5]] <- purrr::map_at(esi_tabla1[[5]], 'id_identificacion', as.integer) # solución parche

### df unificado

esi_tabla1_df <- esi_tabla1 %>% 
  bind_rows()

# 3.2 Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi) 
# para cada versión. Debes considerar una fila por hogar (id_identificacion) e incluir la columna version 

esi_tabla2 <- esi %>% 
  map(~.x %>% 
        select("version","id_identificacion","fact_cal_esi") %>% 
        unique(by = "id_identificacion"))

## solucion parche a error: Error in `bind_rows()`:
##! Can't combine `..1$id_identificacion` <double> and `..5$id_identificacion` <integer64>.

esi_tabla2[[5]] <- purrr::map_at(esi_tabla2[[5]], 'id_identificacion', as.integer) # solución parche

### df unificado

esi_tabla2_df <- esi_tabla2 %>% 
  bind_rows()

### mínimo con data.table

esi_tabla2_min <- esi_tabla2_df[,lapply(.SD, min),                   
                         by = .(version),           
                         .SDcols = c('id_identificacion','fact_cal_esi')] 

colnames(esi_tabla2_min)[3] <- "min"

### máximo con data.table

esi_tabla2_max <- esi_tabla2_df[,lapply(.SD, max),                   
                           by = .(version),          
                           .SDcols = c('id_identificacion','fact_cal_esi')] 

colnames(esi_tabla2_max)[3] <- "max"

### mean con data.table

esi_tabla2_mean <- esi_tabla2_df[,lapply(.SD, mean),                    
                           by = .(version),           
                           .SDcols = c('id_identificacion','fact_cal_esi')] 

colnames(esi_tabla2_mean)[3] <- "mean"

### median con data.table

esi_tabla2_median <- esi_tabla2_df[,lapply(.SD, median),                   
                           by = .(version),          
                           .SDcols = c('id_identificacion','fact_cal_esi')]

colnames(esi_tabla2_median)[3] <- "median"

### p10 con dplyr

esi_tabla2_p10 <- esi_tabla2_df %>%
  group_by(version) %>%
  summarize(p10 = quantile(fact_cal_esi, probs = 0.1))


### p90 con dplyr

esi_tabla2_p90 <- esi_tabla2_df %>%
  group_by(version) %>%
  summarize(p90 = quantile(fact_cal_esi, probs = 0.9))


### esi_3_2: df con min, max, mean, median, p10 y p90 unificado

df_list <- list(esi_tabla2_min, esi_tabla2_max, esi_tabla2_mean, 
                esi_tabla2_median, esi_tabla2_p10, esi_tabla2_p90)

esi_3_2 <- df_list %>% reduce(full_join, by= c('version'))

esi_3_2 <- esi_3_2[,-c(2,4,6,8)]


# 3.3 Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro 
# (conglomerado).Debes incluir la columna version.

esi_tabla3 <- esi %>% 
  map(~.x %>% 
        select("version","estrato", "conglomerado"))

### df unificado

esi_tabla3_df <- esi_tabla3 %>% 
  bind_rows() 

### tabla con número de conglomerados por estrato con su respectiva version

esi_3_3 <- esi_tabla3_df %>% 
  group_by(version,estrato) %>% 
  count(conglomerado)


# 3.4 Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal 
# (ing_t_p) para cada versión. Esta tabla debe ser construida a nivel persona, utilizando el factor 
# de expansión (fact_cal_esi).

esi_tabla4 <- esi %>% 
  map(~.x %>% 
        select("version","idrph","ing_t_p","fact_cal_esi"))

### df unificado

esi_tabla4_df <- esi_tabla4 %>% 
  bind_rows() %>% 
  mutate(ingreso = ing_t_p*fact_cal_esi) %>% 
  filter(ingreso > 0)

### mínimo con data.table

esi_tabla4_min <- esi_tabla4_df[,lapply(.SD, min),                     
                           by = .(version),          
                           .SDcols = c('idrph','ingreso')] 

colnames(esi_tabla4_min)[3] <- "min"

### máximo con data.table

esi_tabla4_max <- esi_tabla4_df[,lapply(.SD, max),                   
                           by = .(version),           
                           .SDcols = c('idrph','ingreso')] 

colnames(esi_tabla4_max)[3] <- "max"

### mean con data.table

esi_tabla4_mean <- esi_tabla4_df[,lapply(.SD, mean),                   
                            by = .(version),          
                            .SDcols = c('idrph','ingreso')] 

colnames(esi_tabla4_mean)[3] <- "mean"

### median con data.table

esi_tabla4_median <- esi_tabla4_df[,lapply(.SD, median),                   
                              by = .(version),          
                              .SDcols = c('idrph','ingreso')] 

colnames(esi_tabla4_median)[3] <- "median"

### p10 con dplyr

esi_tabla4_p10 <- esi_tabla4_df %>%
  group_by(version) %>%
  summarize(p10 = quantile(ingreso, probs = 0.1))


### p90 con dplyr

esi_tabla4_p90 <- esi_tabla4_df %>%
  group_by(version) %>%
  summarize(p90 = quantile(ingreso, probs = 0.9))

### esi_3_4: df con min, max, mean, median, p10 y p90 unificado

df_list <- list(esi_tabla4_min, esi_tabla4_max, esi_tabla4_mean, 
                esi_tabla4_median, esi_tabla4_p10, esi_tabla4_p90)

esi_3_4 <- df_list %>% reduce(full_join, by= c('version'))

esi_3_4 <- esi_3_4[,-c(2,4,6,8)]

# 4. Ejercicio 4: mejorando el código --------------------------------------------------------

# Calcula el promedio de ingresos en las tablas de la ESI (ing_t_p) mediante las siguientes estrategias:

esi_3_3 <- esi_tabla3_df %>% 
  group_by(version,estrato) %>% 
  count(conglomerado)

  
# 4.1 Lista de tablas: calcular promedio con herramientas de purrr (como en el ejercicio anterior)

benchmark_1 <- microbenchmark(
  tidyverse.test1 = map_df(list(esi_tabla4_df), ~.x %>% select("version", "ingreso")) %>% 
    group_by(version) %>% summarize(mean(ingreso)),
  times = 5)
benchmark_1

# 4.2 Tablas apiladas: calcular promedio con group_by() %>% summarise() (apila una tabla sobre otra en un dataframe)

benchmark_2 <- microbenchmark(
  dplyr.test2 = esi_tabla4_df %>% group_by(version) %>% 
    summarise(mean = mean(ingreso)),
  times = 5)
benchmark_2

# 4.3 Lista de tablas: calcular promedio con herramientas de purrr, utilizando una función creada por ti, que utilice data.table.

benchmark_3 <- microbenchmark(
  funcion.test3 <- funcion(esi_tabla4_df),
  times = 5)
benchmark_3

# 4.4 Tablas apiladas: calcular promedio con data.table.

benchmark_4 <- microbenchmark(
  datatable.test4 = esi_tabla4_df[,lapply(.SD, mean),                  
                               by = .(version),         
                               .SDcols = c('idrph','ingreso')],
  times = 5)

benchmark_4

# 4.5 Comparración Benchmark

benchmark <- microbenchmark(
  tidyverse.test1 = map_df(list(esi_tabla4_df), ~.x %>% select("version", "ingreso")) %>% 
    group_by(version) %>% summarize(mean(ingreso)),
  dplyr.test2 = esi_tabla4_df %>% group_by(version) %>% 
    summarise(mean = mean(ingreso)),
  funcion.test3 <- funcion(esi_tabla4_df),
  datatable.test4 = esi_tabla4_df[,lapply(.SD, mean),                  
                                by = .(version),         
                                .SDcols = c('idrph','ingreso')],
  times = 5)

benchmark


#¿Existen diferencias importantes entre las distintas estrategias? 
# Yo creo que las diferencias no son significativas. En función de la base de datos, las 4
# estrategias responden en un tiempo similar. 

#¿Hay alguna más eficiente que otra? ¿Usar group_by versus map hace alguna diferencia?
# Al parecer la más eficiente es data.table, la cual no usa ni map ni group by, sino mas bien
# lapply y by. A mi parecer la forma de programación con data.table es la mas eficiente en varios
# sentidos, como por ejemplo, menor cantidad de líneas de código
