# -------------------------------------------------------------------------
# Tarea final---------------------------------- ---------------------------
# -------------------------------------------------------------------------

# Cargar paquetes y funciones ---------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               data.table,
               tictoc, stringr, curl, downloader, microbenchmark)

source("functions.R")

# Ejercicio 1: Descargar archivos -----------------------------------------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)


file_names <- map(urls,~extract_name(.x))

dir_base <- getwd()

map2(urls,file_names,~download_esi_data(.x, .y))

# Ejercicio 2: Leer archivos ---------------------------------------------


data <- map(file_names,~read_esi_data(paste0("data/",.x))) 


#Para nombrar los elementos de la lista y asignar una variable según el nombre

anio <- str_extract_all(file_names, "esi-20\\d{2}") 
anio <- str_replace_all(anio, "-","_")
anio <- as.list(anio)
names(data) <- anio

data <- map2(data, anio, ~.x %>% 
       mutate(version = .y))


# Ejercicio 3: Obtener datos ---------------------------------------------

tabla1 <- map_df(data, ~.x %>%
       select("version", "idrph", "id_identificacion") %>% 
         group_by(version) %>% 
         summarize(npersonas=n_distinct(idrph),nhogares=n_distinct(id_identificacion))
                )


tabla2 <- map_df(data, ~.x %>%
                   select("version", "fact_cal_esi", "id_identificacion") %>%
                   distinct(id_identificacion, .keep_all= TRUE) %>%
                   summarize(min_fe=min(fact_cal_esi),
                             max_fe=max(fact_cal_esi),
                             media_fe=mean(fact_cal_esi),
                             mediana_fe=median(fact_cal_esi),
                             p10_fe=quantile(fact_cal_esi,probs = 0.1),
                             p90_fe=quantile(fact_cal_esi,probs = 0.9),
                             .by = version, .groups = NULL)
                 )

##Se observan valores atípicos según lo que indica la variable máximo. Esto porque los máximos
##exceden en muchísimo el valor del percentil 90

tabla3 <- map_df(data, ~.x %>%
                   select("version", "estrato", "conglomerado") %>% 
                   group_by(version,estrato) %>%
                    summarize(nconglomerados=n_distinct(conglomerado)) %>%
                    mutate(casos = ifelse(nconglomerados == 1,1,0)) %>% 
                   group_by(version) %>% 
                   summarize(sum(casos))
                  ) 
 
#version 1: considera a todas las personas
  
tabla4.v1 <- map_df(data, ~.x %>%
                   select("version", "fact_cal_esi", "id_identificacion", "ing_t_p") %>%
                   mutate(ing_exp=ing_t_p*fact_cal_esi) %>%
                   group_by(version)  %>%
                   summarize(min_ing_exp=min(ing_exp),
                             max_ing_exp=max(ing_exp),
                             media_ing_exp=mean(ing_exp),
                             mediana_ing_exp=median(ing_exp),
                             p10_ing_exp=quantile(ing_exp,probs = 0.1),
                             p90_ing_exp=quantile(ing_exp,probs = 0.9))
                  )

#version 2: considera solo los que tienen ingresos

tabla4.v2 <- map_df(data, ~.x %>%
                      select("version", "fact_cal_esi", "id_identificacion", "ing_t_p") %>%
                      mutate(ing_exp=ing_t_p*fact_cal_esi) %>%
                      subset(ing_exp>0) %>%
                      group_by(version)  %>%
                      summarize(min_ing_exp=min(ing_exp),
                                max_ing_exp=max(ing_exp),
                                media_ing_exp=mean(ing_exp),
                                mediana_ing_exp=median(ing_exp),
                                p10_ing_exp=quantile(ing_exp,probs = 0.1),
                                p90_ing_exp=quantile(ing_exp,probs = 0.9))
                )

# Ejercicio 4: Mejorando el código ---------------------------------------------          

#benchmarking

resultados <- microbenchmark(
  mb1 = map_df(data, ~.x %>% select("version", "ing_t_p")) %>% group_by(version) %>% summarize(mean(ing_t_p)),
  
  mb2 = {
    for (i in 1:6) {
      assign(paste0("v", i), as.data.frame(data[[i]]$version))
    }
    v <- bind_rows(v1, v2, v3, v4, v5, v6)
    
    for (i in 1:6) {
      assign(paste0("ing", i), as.data.frame(data[[i]]$ing_t_p))
    }
    ing <- bind_rows(ing1, ing2, ing3, ing4, ing5, ing6)
    
    ta <- bind_cols(v,ing)
    colnames(ta) = c("version","ing_t_p")
    ta %>% group_by(version) %>% summarize(mean(ing_t_p))},
  
  mb3 =     map_df(data, ~mi_data_table(.x)),
  
  mb4 = {
    for (i in 1:6) {
      assign(paste0("v", i), as.data.frame(data[[i]]$version))
    }
    v <- bind_rows(v1, v2, v3, v4, v5, v6)
    
    for (i in 1:6) {
      assign(paste0("ing", i), as.data.frame(data[[i]]$ing_t_p))
    }
    ing <- bind_rows(ing1, ing2, ing3, ing4, ing5, ing6)
    
    ta <- bind_cols(v,ing)
    colnames(ta) = c("version","ing_t_p")
    dt_ta <- as.data.table(ta)
    dt_ta[,mean(ing_t_p),by = version]},
  times = 5)

print(resultados)
autoplot(resultados)

## De las distintas estrategias utilizadas se encuentra que la más eficiente es calcular directamente con data table (mb4)
## (incluso tomando el tiempo adicional de construir la BBDD en ese formato) y la menos
## es la de utilizar purr llamando a una funcion de datatable. En segundo lugar y bastante cerca es más eficiente 
## usar group_by (caso 2) versus purr (caso1), pero la diferencia no es tan marcada.De todas formas en terminos de consistencia
## la forma una es la con menor varianza.
