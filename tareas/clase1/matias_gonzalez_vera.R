### Tarea Clase 1


## Ejercicio 1:
# Construye una función llamada get_cv que calcule el coeficiente de variación de un vector. get_cv debe recibir 2 parámetros:

## un vector de tamaño n
## un valor boolean que indique si los valores NA deben ser removidos. Por defecto, la función no debe remover dichos valores.

get_cv <- function(vector, remover = F){
  media <- ifelse(remover == T, mean(vector, na.rm = T),  mean(vector, na.rm = F))
  de <- ifelse(remover == T, sd(vector, na.rm = T),  sd(vector, na.rm = F))
  cv <- de/media
  print(cv)
  }


vector_prueba <- c(101, NA, 105, 110, 103)
vector_p2 <- c(100, 100, 100)


## Ejercicio 2:

# Crea una función llamada build_address que construya una dirección en base a algunos inputs, siguiendo un formato establecido. build_address recibe 2 parámetros obligatorios y uno opcional.

## Parámetros obligatorios:
  
##  street: string que contiene el nombre de la calle
##  number: string que contiene el número de la dirección
##  El parámetro opcional es apartment y contiene el número del departamento.


df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df


build_adress <- function(calle, numero, depa){
  nombre_calle <- gsub('calle ' , '',
                       gsub('av. ', '', calle))
  
  numero_calle <- gsub("num ", "",
                       gsub("número ", "", numero))
  
  numero_depto <- gsub("depto. ", "",
                       gsub("departamento ", "", depa))

  numero_depto <- sub("NULL", NA, numero_depto)

  print(paste0(nombre_calle," ", numero_calle, ifelse(is.na(numero_depto),"", paste(", depto.", numero_depto))) ) 

}


build_adress(df$calle, df$numero, df$depto)
