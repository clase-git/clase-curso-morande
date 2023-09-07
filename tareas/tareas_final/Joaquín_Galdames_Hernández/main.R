#*************************************************************************************************************************************************************
# 0. Identificación ----------------------------------
#Título: Tarea de R Intermedio
#Institucion: Instituto Nacional de Estadísticas
#Encargado: Joaquín E Galdames Hernández - Analista Socioeconómico
#*************************************************************************************************************************************************************

renv::deactivate(project = NULL)

# 1. Cargar paquetes ------------------------
if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar

pacman::p_load(
  tidyverse,
  glue,
  purrr,
  stringr
)

##Eliminar notación científica
options(scipen=999)

##Abrir funciones
source(file = "functions.R", encoding = "UTF-8")

### Ejercicio 1: descargar archivos-------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

walk(urls, ~ download_esi_data(.x, "data", extract_name(.x)))
  

### Ejercicio 2: leer archivos-------

esi <- map(file.path("data",list.files(path = "data/")), ~ read_esi_data(.x)) 

### creo un vector para ponerle los nombres
nombres <- str_extract(list.files(path = "data/"), pattern = "esi-\\d{4}")

### pongo nombres al objeto dentro de la lista
esi <- esi %>% set_names(nombres)

remove(nombres)

### Ejercicio 3: obtener datos-------

imap_dfr(esi , ~ .x %>% 
          summarize(
            version = .y,
            n_personas = nrow(.x),
            n_hogares = length(unique(id_identificacion))
            
          )
          
          )
