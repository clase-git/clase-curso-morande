# Tarea final - R intermedio - Agosto 2023
# Claudia Allendes Varas
# 06-09-22023

rm(list = ls())
getwd()

install.packages("tidyverse")
library(tidyverse)
install.packages("data.table")
library(data.table)
install.packages("janitor")
library(janitor)
install.packages('microbenchmark')
library(microbenchmark)

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

str(urls)

########################################
# 1. Ejercicio 1: descargar archivos
########################################
# prueba para rescatar nombre
nombre <- str_extract_all("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/1esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
                          "esi-\\d{4}---[a-z]{8}\\.[a-z]{3}")
nombre

# función para extraer nombres de los archivos desde el urls
extract_name <- function(urls){
  map_chr(urls, ~str_extract(.x, pattern = "esi\\-\\d{4}---[a-z]{8}\\.[a-z]{3}"))
}
file_names <- extract_name(urls)
file_names

# funcion download_esi_data para descargar los archivos
download_esi_data <- function(urls, file_names, directory) {
  ruta <- paste0(directory, file_names)
  walk2(urls, ruta, 
        ~download.file(url = .x, 
                       destfile = .y))
}
download_esi_data(urls, file_names, "data/")



########################################
# 2. Ejercicio 2: leer archivos
########################################
files <- list.files("data/", full.names = TRUE)
files
names_esi <- list.files("data/") %>%
  str_extract_all(pattern = "esi-\\d{4}---[a-z]{8}")
names_esi
# leyendo los datos
varios_esi <- map(files, ~read.csv(.x))
names(varios_esi) <-  paste(names_esi)
nombres_lista_esi <-  imap(varios_esi, ~.y)
nombres_lista_esi

# creando la variable version
version <- str_extract_all(nombres_lista_esi, pattern = "esi-\\d{4}") %>%
  str_replace_all("-", "_")
version

# se agrega variable version a listado de esi's
varios_esi1 <- map2(varios_esi,
                    version,
                    ~.x %>% 
                      mutate(version =.y))


########################################
# 3. Ejercicio 3: obtener datos
########################################

# tabla 3.1
#############
#dt_varios_esi1 <- as.data.table(varios_esi1)
#class(dt_varios_esi1)

# cálculo para esi_2017
tabla_3_1_esi_2017 <- varios_esi1[[2]] %>%
  summarise(version = min(version),
            n_personas = sum(!duplicated(idrph), na.rm =T),
            n_hogares = sum(!duplicated(id_identificacion), na.rm =T ))

# cálculo para todas las esi's
tabla_3_1_varios_esi1 <- map(varios_esi1,
                             ~mutate(.x, 
                                     n_personas = n_distinct(idrph),
                                     n_hogares = n_distinct(id_identificacion)) %>%
                               distinct(version, n_personas, n_hogares))
tabla_3_1_varios_esi1_resultado <- bind_rows(tabla_3_1_varios_esi1)


# tabla 3.2
#############
# para esi-2017
tabla_3_2_esi_2017 <- varios_esi1[[2]] %>%
  summarise(esi_año = min(version),
            min_fact_exp = min(fact_cal_esi),
            max_fact_exp = max(fact_cal_esi),
            media_fact_exp = mean(fact_cal_esi),
            mediana_fact_exp = median(fact_cal_esi),
            p10_fact_exp = quantile(fact_cal_esi, 0.1),
            p90_fact_exp = quantile(fact_cal_esi, 0.9))


# cálculo para todas las esi's
tabla_3_2_varios_esi <- map(varios_esi1, 
                            ~distinct(.x, version, fact_cal_esi) %>%
                              mutate(min_fact_exp = min(fact_cal_esi, na.rm = T),
                                     max_fact_exp = max(fact_cal_esi, na.rm = T),
                                     media_fact_exp = mean(fact_cal_esi, na.rm = T),
                                     mediana_fact_exp = median(fact_cal_esi, na.rm = T), 
                                     p10_fact_exp = quantile(fact_cal_esi, 0.1, na.rm = T),
                                     p90_fact_exp = quantile(fact_cal_esi, 0.9, na.rm = T)) %>%
                              distinct(version, min_fact_exp, max_fact_exp, media_fact_exp, mediana_fact_exp, 
                                       p10_fact_exp, p90_fact_exp))
tabla_3_2_varios_esi_resultados <- bind_rows(tabla_3_2_varios_esi)

# Al comparar los resutados de la tabla anterior se observa que el valor máximo del factor de expansión ha caído
# fuertemente a través de los años, mientras que el percentil 10 y 90 ha aumentado.



# tabla 3.3
#############
tabla_3_3_varios_esi <- map(varios_esi1, ~distinct(.x, version, estrato, conglomerado) %>%
                              group_by(estrato) %>%
                              mutate(num_conglomerados = n()) %>%
                              ungroup() %>%
                              filter(num_conglomerados == 1) %>%
                              group_by(version) %>%
                              summarise(num_estratos = n()) %>%
                              ungroup())
tabla_3_3_varios_esi_resultados <- bind_rows(tabla_3_3_varios_esi)


# tabla 3.4
#############
# función para ponderar una variable
ponderar <- function(var, fact){
  {{var}}*{{fact}}
}

# probando la ponderación de ing_t_p por fact_cal_esi solo para la esi2017
varios_esi1[[2]] %>%
  mutate(ing_t_p_exp = ponderar(ing_t_p, fact_cal_esi)) %>%
  select(ing_t_p_exp) %>% 
  slice(1:3)

# ponderando para todas las listas de las esi's
varios_esi2 <- map(varios_esi1, 
                   ~mutate(.x, ing_t_p_exp = ponderar(ing_t_p, fact_cal_esi)))

# tabla de ing_t_p sin ponderar
Tabla_3_4 <- map(varios_esi1, ~select(.x, version, ing_t_p) %>%
                   mutate(min_ing_t_p = min(ing_t_p, na.rm = T),
                          max_ing_t_p = max(ing_t_p, na.rm = T),
                          media_ing_t_p = mean(ing_t_p, na.rm = T), 
                          mediana_ing_t_p = median(ing_t_p, na.rm = T), 
                          p10_ing_t_p = quantile(ing_t_p, 0.1, na.rm = T), 
                          p90_ing_t_p = quantile(ing_t_p, 0.9, na.rm = T)) %>%
                   distinct(version, min_ing_t_p, max_ing_t_p, media_ing_t_p, mediana_ing_t_p,
                            p10_ing_t_p, p90_ing_t_p))
Tabla_3_4_sin_ponderar <- bind_rows(Tabla_3_4)

# tabla de ing_t_p con ponderación => ing_t_p_exp
Tabla_3_5 <- map(varios_esi2, ~select(.x, version, ing_t_p_exp) %>%
                   mutate(min_ing_t_p_exp = min(ing_t_p_exp, na.rm = T),
                          max_ing_t_p_exp = max(ing_t_p_exp, na.rm = T),
                          media_ing_t_p_exp = mean(ing_t_p_exp, na.rm = T), 
                          mediana_ing_t_p_exp = median(ing_t_p_exp, na.rm = T), 
                          p10_ing_t_p_exp = quantile(ing_t_p_exp, 0.1, na.rm = T), 
                          p90_ing_t_p_exp = quantile(ing_t_p_exp, 0.9, na.rm = T)) %>%
                   distinct(version, min_ing_t_p_exp, max_ing_t_p_exp, media_ing_t_p_exp, mediana_ing_t_p_exp,
                            p10_ing_t_p_exp, p90_ing_t_p_exp))
Tabla_3_5_con_ponderar <- bind_rows(Tabla_3_5)


########################################
# 4. Ejercicio 4: mejorando el código
########################################

# ejercicio 4.1
##################
resultado_4_1 <- microbenchmark::microbenchmark(
  map(varios_esi1,
      ~summarise(.x, 
                 media_ing_t_p = mean(ing_t_p))),
  times = 5)

# ejercicio 4.2
##################
tabla_4_2_varios_esi <- map(varios_esi1 %>%
                              group_by(version) %>>%
                              summarise(.x, media_ing_t_p = mean(ing_t_p)))
tabla_4_2_varios_esi_resultados <- bind_rows(tabla_4_2_varios_esi)

tabla_4_2_varios_esi <- map(varios_esi1 %>%
                              group_by(version) %>%
                              summarise(.x, media_ing_t_p = mean(ing_t_p)))
tabla_4_2_varios_esi_resultados <- bind_rows(tabla_4_2_varios_esi)


tabla_4_2_varios_esi <- map(varios_esi1, ~select(.x, version) %>%
                              group_by(.x, version) %>%
                              summarise(.x, media_ing_t_p = mean(ing_t_p)))
tabla_4_2_varios_esi_resultados <- bind_rows(tabla_4_2_varios_esi)




# ejercicio 4.3
##################
dt_esi <- data.table(varios_esi1)

tabla_4_3_dt_esi <- dt_esi[, purrr::map(.SD, mean("ing_t_p"), na.rm = TRUE, by = version)]
# no funciona


# ejercicio 4.4
##################
