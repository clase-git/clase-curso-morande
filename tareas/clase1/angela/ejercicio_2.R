#install.packages("tm")
library(tm) # Funciones de text mining
library(dplyr)
library(tidyverse)

build_address <- function(street, number, apartment = NULL) {
  #street <- removeWords(street, words = stopwords("spanish"))
  street <- street %>% str_replace_all(., "[[:cntrl:]]", " ") %>% 
    str_to_lower() %>% removePunctuation() %>% str_replace_all(., "—", " ")
  
  a_retirar <- c("numero", "número", "num", "n ", "avenida ", "av ", 
                 "av. ", "calle ", "pasaje ","depto ","departamento ",
                 "depto. ")
  street <- removeWords(street, words = a_retirar)
  number <- removeWords(number, words = a_retirar)
  
  # Verificar si se proporcionó un número de departamento
  if (!is.null(apartment)) {
    address <- paste(street, number, "depto.", apartment)
  } else {
    address <- paste(street, number)
  }
  
  return(address)
}

# Ejemplo 1: Sin departamento
address1 <- build_address("Pasaje Los Alerces", "n 123")
print(address1)  # Imprimirá la dirección sin departamento

# Ejemplo 2: Con departamento
address2 <- build_address("Pasaje Los Alerces", "n 123", "A4")
print(address2)  # Imprimirá la dirección con departamento



df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df


df2 <- df %>% rowwise() %>% 
  mutate(direccion = build_address(calle, numero, depto)) 
print(df2)





