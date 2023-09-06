# función 1: extracción nombres -------------------------------------------

extract_name <- function(x){
  url_esi <- str_extract(x, pattern = "[a-z]{3}-[[:digit:]]{4}---[a-z]{8}.[a-z]{3}")
  url_esi <- as.character(url_esi)
}

# función 2: descarga archivos --------------------------------------------

download_esi_data <- function(url,file_name,directory){
    curl_download(url = url, destfile = glue("{directory}/{file_name}"))
  }

# función 3: leer archivos ------------------------------------------------

read_esi_data <- function(ruta){
  a <-fread(ruta, data.table=FALSE)
}


# función 4: tablas varias ------------------------------------------------

tabla_1 <- function(x, name){
  n_personas <- x %>% group_by(idrph) %>% summarise(n=n()) %>% count()
  n_hogares <- x %>% group_by(id_identificacion) %>% summarise(n=n()) %>% count()
  ano <- str_extract(name,pattern ="[[:digit:]]{4}")
  version <- paste0("esi_",ano)
  aux <- as.data.frame(cbind(version, n_personas, n_hogares))
  names(aux) <- c("version","n_personas","n_hogares")
  return(aux)
}

tabla_2 <- function(x){
  aux <- x %>% 
    group_by(id_identificacion) %>%
    summarise(fact_cal_esi = mean(fact_cal_esi),
              ano = mean(ano_trimestre)) %>%
    ungroup() %>%
    summarise(minimo = min(fact_cal_esi, na.rm = TRUE),
              maximo = max(fact_cal_esi, na.rm = TRUE),
              media = mean(fact_cal_esi, na.rm = TRUE),
              mediana = quantile(fact_cal_esi, probs = 0.5, na.rm = TRUE),
              p90 = quantile(fact_cal_esi, probs = 0.9, na.rm = TRUE),
              p10 = quantile(fact_cal_esi, probs = 0.1, na.rm = TRUE),
              ano = mean(ano)) %>%
    mutate(version = paste0("esi_",ano)) %>%
    select(version,minimo,maximo,media,mediana,p10,p90)
  return(aux)
}

tabla_3 <- function(x,name){
  aux <- x %>% 
    group_by(estrato,conglomerado) %>%
    tally() %>% 
    group_by(estrato) %>%
    tally() %>% 
    filter(n==1) %>% 
    select(estrato) %>% 
    summarise(suma_estratos = n())
  
  ano <- str_extract(name,pattern ="[[:digit:]]{4}")
  version <- paste0("esi_",ano)
  
  aux <- cbind(version,aux)
}

tabla_4 <- function(x,name){
  aux <- x %>% as_survey_design(ids = conglomerado,
                                strata = estrato,
                                weights = fact_cal_esi) %>%
    filter(activ==1) %>% 
    summarise(media = survey_mean(ing_t_p, na.rm=T),
              mediana = survey_median(ing_t_p, na.rm = TRUE),
              quantile = survey_quantile(ing_t_p, quantile = c(0.1, 0.9), na.rm =T),
              minimo = min(ing_t_p,na.rm =T),
              maximo = max(ing_t_p, na.rm =T) ) %>%
    select(minimo,maximo,media,mediana,quantile_q10, quantile_q90)
  
  ano <- str_extract(name,pattern ="[[:digit:]]{4}")
  version <- paste0("esi_",ano)
  
  aux <- cbind(version,aux)
  return(aux)
}


# Función - Mejorando código ----------------------------------------------


promedio_dt <- function(x){
  x <- x[activ==1,weighted.mean(x = ing_t_p,w = fact_cal_esi, na.rm = TRUE)]
}
