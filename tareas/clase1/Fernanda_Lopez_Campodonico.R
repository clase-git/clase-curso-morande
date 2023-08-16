library(tidyverse)
library(stringr)

#----------------------------------------------------------------

#Ejercicio 1

get_cv <- function(n,VF=FALSE){
  if(VF == TRUE ) {
    cv = sd(n, na.rm = TRUE)/mean(n, na.rm = TRUE)
      } else {
        cv = sd(n)/mean(n)
  }
} 

#Evaluación ejercicio 1

vector1 <- c(1,2,3)
vector2 <- c(1,2,NA)

prueba1 <- get_cv(vector1)
prueba1

prueba2 <-get_cv(vector1,TRUE)
prueba2

prueba3 <- get_cv(vector2)
prueba3

prueba4 <-get_cv(vector2,TRUE)
prueba4

#----------------------------------------------------------------

##Ejercicio 2

build_address  <- function(street,number,apart=''){
  
  street <- str_to_lower(street)
  r_street = c('calle'='','av.'='','avenida'='','pasaje'='')
  street <- str_replace_all(street, r_street) 
  
  r_number = c('número'='','num'='','n'='','número'='')
  number <- str_replace_all(number, r_number) 
  
  r_apart = c('departamento'='', 'depto.'='')
  apart <- str_replace_all(apart, r_apart) 
  
  paste(street,",",number,",",apart)
} 


#Evaluación ejercicio 2

street <- "calle Los Alerces"
number <- "número 123"
build_address(street, number)

#----------------------------------------------------------------

##Ejercicio 3

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df

df <- df %>% rowwise() %>% mutate(dirección=build_address(calle, numero, depto))
df


