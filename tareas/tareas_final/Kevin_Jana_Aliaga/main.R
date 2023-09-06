# Limpieza de ambiente -----------------------------------------------------------------------------
# rm(list = ls())
gc()

# Setear opciones ----------------------------------------------------------------------------------


options(OutDec= ",")
options(scipen=999)


# Instalación y/o carga de paquetes requeridos -----------------------------------------------------


options(repos = "http://cran.rstudio.com")

paquetes <- c("data.table","tinytex", "magrittr", 
              "stringr", "survey", "tidyverse", 
              "haven", "scales", "dplyr", "readxl",
              "tidyr", "openxlsx", "ggplot2","tibble",
              "purrr", "curl", "glue", "hdd", "knitr",
              "srvyr", "microbenchmark", "survey")

for(p in paquetes) {
  if (!require(p,character.only = TRUE))    install.packages(p);    library(p,character.only = TRUE)
}



# Descarga archivos ----------------------------------------------------------


urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true", #borre un 1 que no permitia descarga
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

file_names <- extract_name(urls)

folder <- ifelse(!dir.exists(file.path(paste0("data"))), dir.create(paste0("data")), FALSE)

walk2(urls,file_names, ~download_esi_data(url = .x, file_name = .y, directory = "./data"))


# Leer archivos -----------------------------------------------------------


# Obtener las direcciones de los archivos
direcciones <- list.files("./data")
direcciones <- paste0("./data/", direcciones)

# Leer los archivos
archivos_esi <- map(direcciones, ~read_esi_data(.x))

nombres_esi <- str_extract(direcciones, pattern = "[a-z]{3}-[[:digit:]]{4}---[a-z]{8}")

names(archivos_esi) <- nombres_esi


# Obtener datos -----------------------------------------------------------

tabla_a <- imap_dfr(archivos_esi, ~tabla_1(x = .x, name = .y))
tabla_a

tabla_b <- imap_dfr(archivos_esi, ~tabla_2(x = .x))
tabla_b

  # Se puede ver algunos factores tienen valores más altos y más bajos para los periodos
  # hasta el 2020, pero a diferencia de los periodos restantes se pueden observar que las 
  # colas del 10% y 90% tiene valores más altos y es probable que sea por temas de pandemia
  # y cada factor sea mas alto para tener una representatividad igual a la anterior y mantener
  # distibución poblacional.

tabla_c <- imap_dfr(archivos_esi, ~tabla_3(x = .x, name = .y))
tabla_c

tabla_d <- imap_dfr(archivos_esi, ~tabla_4(x = .x, name = .y))
tabla_d


# Mejorando el código -----------------------------------------------------


#Promedio con purrr
lista_promedio <- imap(archivos_esi, ~.x %>%
                         as_survey_design(ids = conglomerado,
                                          strata = estrato,
                                          weights = fact_cal_esi) %>%
                         filter(ocup_ref==1) %>% 
                         summarise(ingreso_medio = survey_mean(ing_t_p, na.rm=T)) %>%
                         select(ingreso_medio))

#Promedio con tablas apiladas 
tablas_promedio <-  
  lapply(archivos_esi, function(x) select(x,
                                          ano_trimestre, 
                                          ing_t_p, 
                                          conglomerado, 
                                          estrato, 
                                          fact_cal_esi,
                                          ocup_ref)) %>%
  bind_rows() %>%
  as_survey_design(id = conglomerado,
                   strata = estrato,
                   weights = fact_cal_esi) %>%
  filter(ocup_ref==1) %>%
  group_by(ano_trimestre) %>%
  summarise(ingreso_medio = survey_mean(ing_t_p, rm.na = T)) %>% 
  select(ano_trimestre, ingreso_medio)
  
#Promedio con map y data table
tabla_archivos_esi <- map(archivos_esi, ~data.table(.x))
lista_dt_promedio <- map(tabla_archivos_esi, ~promedio_dt(.x))
lista_dt_promedio

#Promedio con tablas apiladas y data table
tablas_dt_promedio <-  
  lapply(archivos_esi, function(x) select(x,
                                          ano_trimestre, 
                                          ing_t_p, 
                                          conglomerado, 
                                          estrato, 
                                          fact_cal_esi,
                                          ocup_ref)) %>%
  bind_rows() %>% 
  as.data.table()  

tablas_dt_promedio[ocup_ref==1,weighted.mean(x = ing_t_p,w = fact_cal_esi, na.rm = TRUE),by = ano_trimestre]


#Benchmark a ocupar 

mi_benchamark <- 
  microbenchmark(list = alist(
    bm_lista_promedio = imap(archivos_esi, ~.x %>% as_survey_design(ids = conglomerado, strata = estrato,weights = fact_cal_esi) %>% filter(ocup_ref==1) %>%  summarise(ingreso_medio = survey_mean(ing_t_p, na.rm=T)) %>%select(ingreso_medio)),
    bm_tablas_promedio = lapply(archivos_esi, function(x) select(x,ano_trimestre, ing_t_p, conglomerado, estrato, fact_cal_esi,ocup_ref)) %>% bind_rows() %>% as_survey_design(id = conglomerado, strata = estrato, weights = fact_cal_esi) %>% filter(ocup_ref==1) %>% group_by(ano_trimestre) %>% summarise(ingreso_medio = survey_mean(ing_t_p, rm.na = T)) %>% select(ano_trimestre, ingreso_medio),
    bm_lista_dt_promedio = map(tabla_archivos_esi, ~promedio_dt(.x)),
    bm_tablas_dt_promedio = tablas_dt_promedio[ocup_ref==1,weighted.mean(x = ing_t_p,w = fact_cal_esi, na.rm = TRUE),by = ano_trimestre]),
    times = 10)
mi_benchamark

grafico_bm <- ggplot2::autoplot(mi_benchamark)

# En el ultimo grafico muestra la distribución de tiempo para todos los modelos anteriores
# esto nos dice que los modelos que se hacen con data.table tienen un tiempo de procesamiento
# mucho a menor a los que se estiman con modelos sin. Entre group_by y map la verdad no es mucha
# la diferencia pero si es notoria lo mencionado anteriormente.