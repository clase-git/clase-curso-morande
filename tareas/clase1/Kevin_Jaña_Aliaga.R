### Tarea 1 ------------------------
# Kevin Jaña Aliaga

paquetes <- c("plotly","data.table","janitor",
              "data.table","odbc","raster",
              "rworldxtra","raster","rgdal",
              "descr","tidylog","tinytex", 
              "magrittr", "stringr", "lubridate", 
              "knitr", "tidylog","survey", 
              "tidyverse", "haven", "scales", 
              "dplyr", "officer", "sjlabelled", 
              "readxl", "tidyr", "openxlsx", 
              "ggplot2", "ggrepel", "tibble", 
              "xts","Rmpfr")

for(p in paquetes) {
  
  if (!require(p,character.only = TRUE))    install.packages(p);    library(p,character.only = TRUE)
  
}

rm("paquetes","p")

### Pregunta 1 ---------------------

get_cv <- function(x,y){
  if (length(x)<=1) {
    "El vector debe ser más de un valor"
  }else if(!is.numeric(x)){
    "El vector debe ser númerico"
  } else if (y==TRUE) {
    cv_resultado <- sd(x,na.rm = y)/mean(x,na.rm=y)
     return(paste0("El coeficiente de variación es: ",cv_resultado ))
  } else{
    cv_resultado <- sd(x,na.rm = y)/mean(x,na.rm=y)
    return(paste0("El coeficiente de variación es: ",cv_resultado, ". Deberias remover los NA"))
  }
}


prob_na <- 0.2

set.seed(123)
runif(100)
aux <- data.frame(valor = ifelse(runif(num_values) > prob_na, random_values, NA))


get_cv(aux$valor,FALSE)

### Pregunta 2 ---------------------

build_adress <- function(street, numero,departamento=NULL){
  
  street <- street %>% 
    str_replace_all ( "[[:punct:]]", "") %>%
    str_replace_all(c("avenida" = "", 
                      "av." = "",
                      "av" = "",
                      "pasaje" = "", 
                      "psje" = "",
                      "pje" = "",
                      "calle" = "")) %>% 
    str_squish()
  
  numero <- numero %>% str_extract("\\d+")
  
  if (!is.null(departamento)) {
    departamento <- departamento %>% str_extract("\\d+")
  }
  
  if (is.null(departamento)) {
    return(paste0(street," ",numero))
  }else{
    return(paste0(street," ",numero,", departamento ",departamento))
  }
    
}
  
street <- "calle Los Alerces"
number <- "número 123"

build_adress(street,number)

### Pregunta 3 ---------------------

df <- tribble(~calle,              ~numero,      ~depto,
                "calle Hemingway",   "num 345",    "depto. 345",
                "av. Albert Camus",  "número 123", "123",
                "Manuel Rojas",      "234",        "departamento 231",
                "Nicanor Parra",     "678",        NULL
  ) 
df  

df %<>% 
  rowwise() %>% 
  mutate(nombre = build_adress(calle,numero,depto))
  
df
