#####
library(tidyverse)
library(data.table)
library(microbenchmark)


urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

# Extracción de nombres de archivo y alojar en objeto "nombres"
nombres <- extract_name(urls)

# Descarga de bases y asignación de nombres
walk2(urls, nombres, ~download_esi_data(url = .x, file_name = .y, directory = 'data'))

# Lectura de archivos
bases_esi <- read_esi_data("data")


# Crear tablas
 
## Procesar datos para generación de tablas
bases_esi_r <- map(bases_esi, ~mutate(.x, 
                                      version = paste0('esi_', ano_trimestre), 
                                      fact_cal_esi = str_replace(fact_cal_esi, ',', '.'), 
                                      ing_t_p = str_replace(ing_t_p, ',', '.'),
                                      id_identificacion = as.numeric(id_identificacion), 
                                      fact_cal_esi = as.numeric(fact_cal_esi), 
                                      ing_t_p = as.numeric(ing_t_p)))

## Tabla 1: Número de hogares y personas por cada encuesta ESI ##
tabla_1 <- map(bases_esi_r, ~mutate(.x, n_personas = n_distinct(idrph), 
                                 n_hogares = n_distinct(id_identificacion)) %>% 
                 distinct(version, n_personas, n_hogares)) %>% 
  bind_rows()
tabla_1
  

## Tabla 2: Descripción de factores de expansión
tabla_2 <- map(bases_esi_r, ~distinct(.x, id_identificacion, fact_cal_esi, version) %>%
                 mutate(min = min(fact_cal_esi, na.rm = T), 
                        max = max(fact_cal_esi, na.rm = T), 
                        media = mean(fact_cal_esi, na.rm = T), 
                        mediana = median(fact_cal_esi, na.rm = T), 
                        p10 = quantile(fact_cal_esi, 0.1, na.rm = T), 
                        p90 = quantile(fact_cal_esi, 0.9, na.rm = T)) %>%
                 distinct(version, min, max, media, mediana, p10, p90)) %>% 
  bind_rows()
tabla_2 ## Valores mínimos y máximos son atípicos repecto a medidas centrales, e incluso, a los deciles de los extremos


## Tabla 3: Número de estraos en UPM por versión
tabla_3 <- map(bases_esi_r, ~distinct(.x, version, estrato, conglomerado) %>%
                 group_by(estrato) %>% 
                 mutate(n_conglomerado = n()) %>% 
                 ungroup() %>% 
                 filter(n_conglomerado == 1) %>% 
                 group_by(version) %>% 
                 summarize(n_estrato = n()) %>% 
                 ungroup()) %>% 
  bind_rows()
tabla_3


## Tabla 4: Descripción de los ingresos del trabajo principal por versión
tabla_4 <- map(bases_esi_r, ~select(.x, idrph, fact_cal_esi, version, ing_t_p) %>%
                   mutate(ing_t_p = ifelse(ing_t_p == 0, NA, ing_t_p)) %>%      # valores 0 se convierten en perdidos
                   mutate(min = min(ing_t_p, na.rm = T), 
                          max = max(ing_t_p, na.rm = T), 
                          media = mean(ing_t_p, na.rm = T), 
                          mediana = median(ing_t_p, na.rm = T), 
                          p10 = quantile(ing_t_p, 0.1, na.rm = T), 
                          p90 = quantile(ing_t_p, 0.9, na.rm = T)) %>%
                   distinct(version, min, max, media, mediana, p10, p90)) %>% 
  bind_rows()
tabla_4


# Comparar tiempo de ejecución
resultados <- microbenchmark(
forma_1 <- map(bases_esi_r, ~select(.x, version, ing_t_p) %>%      
                 mutate(media = mean(ing_t_p, na.rm = T)) %>%
                 distinct(version, media)) %>% 
                   bind_rows(),

forma_2 <- bases_esi_r %>% 
  bind_rows() %>% 
  group_by(version) %>% 
  summarise(promedio = mean(ing_t_p, na.rm = T)),

forma_3 <- promedio_dt(bases_esi_r),
  
forma_4 <- bind_rows(bases_esi_r)[, .(promedio = mean(ing_t_p, na.rm = T)), by = version]

,times = 5)

resultados %>% 
  group_by(expr) %>% 
  summarise(tiempo_promedio = mean(time))
