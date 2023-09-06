# Paquetes necesarios ####

if (!require('tidyverse')) install.packages('tidyverse')
if (!require('reader')) install.packages(('reader'))
if (!require('readr')) install.packages(('readr'))
if (!require('curl')) install.packages('curl')
if (!require('data.table')) install.packages(('data.table'))
if (!require('microbenchmark')) install.packages(('microbenchmark'))
if (!require('Hmisc')) install.packages(('Hmisc'))


# Librerias ####

source('function.R')
library('tidyverse')
library('reader')
library('readr')
library('curl')
library('data.table')
library('microbenchmark')
library('Hmisc')


# Ejercicio 1:descargar archivos####

# 1.1 

#se crea carpeta datos: 

if (!dir.exists('data')) dir.create('data')

#se guardan url en vector 

urls <- c('https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true',
          'https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true',
          'https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true',
          'https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true',
          'https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true',
          'https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true')

#  Se crea función extract_name en script 'function' 

# se crea el  vector con nombres

file_names <- extract_name (urls)

# 1.2 Se crea función download_esi_data en script 'function' 

# 1.3

#se descargan los archivos 

walk2(urls, file_names, ~download_esi_data(urls = .x, file_names = .y, directory = 'data'))

# Ejercicio 2: leer archivos

#  Se crea función read_esi_data en script 'function' 

base_esi_2016 <- read_esi_data('data/esi_2016')
base_esi_2017 <- read_esi_data('data/esi_2017')
base_esi_2018 <- read_esi_data('data/esi_2018')
base_esi_2019 <- read_esi_data('data/esi_2019')
base_esi_2020 <- read_esi_data('data/esi_2020')
base_esi_2021 <- read_esi_data('data/esi_2021')

#Cargo todos los archivos en una lista

archivos <-map(paste("data", file_names, sep = "/"), read_esi_data)

# Ejecicio 3: obtener datos

#3.1
Base_archivos<- map(archivos, ~mutate(.x, version = paste0('esi_', ano_trimestre),
                                      id_identificacion = as.numeric(id_identificacion),
                                      fact_cal_esi = str_replace(fact_cal_esi, ",", "."),
                                      fact_cal_esi = as.numeric(fact_cal_esi),
                                      ing_t_p = str_replace(ing_t_p, ",", "."),
                                      ing_t_p = as.numeric(ing_t_p),
                                      idrph = str_replace(idrph, ",", "."),
                                      idrph = as.numeric(idrph)))


Tabla1 <- map(Base_archivos, ~mutate(.x, n_personas = n_distinct(idrph),
                                     n_hogares = n_distinct(id_identificacion)) %>% 
                distinct(version, n_personas, n_hogares)) %>% 
  bind_rows()

#3.2

options(scipen = 999)

Tabla2 <-  map(Base_archivos, ~.x %>% 
                 select(id_identificacion,version,fact_cal_esi) %>% 
                 distinct() %>%
                 summarise(version,
                           minimo = min(fact_cal_esi,na.rm=T),
                           percent10 = quantile(fact_cal_esi, probs = 0.1,na.rm=T),
                           media = mean(fact_cal_esi, na.rm=T),
                           mediana = median(fact_cal_esi,na.rm=T),
                           percent90 = quantile(fact_cal_esi, probs = 0.9,na.rm=T),
                           maximo = max(fact_cal_esi,na.rm=T)) %>%
                 distinct())%>%
  bind_rows()



#3.3

Tabla_3 <- map(Base_archivos, ~.x %>% 
                 select(version, estrato, conglomerado) %>%
                 distinct() %>% 
                 group_by(estrato) %>% 
                 mutate(n_conglomerado = n()) %>% 
                 ungroup() %>% 
                 subset(n_conglomerado == 1) %>% 
                 summarise(version,
                           n_estrato= n()) %>% 
                 distinct()) %>% 
  bind_rows()



#3.4
Tabla_4 <- map(Base_archivos, ~.x %>% 
                 select(idrph, fact_cal_esi, version, ing_t_p) %>%
                 summarise(version,
                           minimo = min(ing_t_p, na.rm = T),
                           p10 = wtd.quantile(ing_t_p, weights=fact_cal_esi,probs=0.1),
                           media = wtd.mean(ing_t_p,weights=fact_cal_esi,na.rm=T),
                           mediana = wtd.quantile(ing_t_p, weights=fact_cal_esi,probs=0.5),
                           p90 = wtd.quantile(ing_t_p, weights=fact_cal_esi,probs=0.9),
                           maximo = max(ing_t_p, na.rm = T))) %>% 
  bind_rows() %>% 
  distinct() 

#Ejercicio 4 : Mejorando el código ####

base_Casos <-  map(Base_archivos, ~.x %>% 
                     select(version, ing_t_p))

bd <- data.table(base_Casos %>% 
                   bind_rows())

Casos <- microbenchmark(
  Caso1 <- (map(base_Casos, ~.x %>%
                  summarise(version,
                            promedio = mean(ing_t_p))%>%
                  distinct()) %>% 
              bind_rows()),
  
  Caso2 <- base_Casos %>% 
    bind_rows() %>% 
    group_by(version) %>% 
    summarise(media = mean(ing_t_p)),
  
  
  
  Caso3 <- bd[, .(Promedio = mean(ing_t_p, na.rm = T)), by = version],
  
  
  
  times = 5)





