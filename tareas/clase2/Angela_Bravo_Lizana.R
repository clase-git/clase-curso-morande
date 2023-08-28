library(gapminder)
library(tidyverse)
library(purrr)
library(feather)

rm(list=ls())

# función sum_something
sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

# función plot_table
plot_table <- function(table, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

gapminder_list <- split(gapminder, gapminder$year)

## EJERCICIO 1 
# Crea una función llamada plot_with_purrr que reciba una lista de tablas y 
# devuelva una lista de gráficos. Pista: La función imap puede ser de gran ayuda

plot_with_purrr <- function(tablas) {
  plots <- imap(tablas, function(.x, .y) {
    table <- sum_something(.x, continent, pop)
    plot_table(table, continent, n, paste(
      "Población mundial, según continente. Año", .x$year[1]))
  })
  return(plots)
}

ejercicio1 <- plot_with_purrr(gapminder_list)

# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# La función responde a la pregunta correctamente, además se aprecia la construcción de funciones dentro del map.
# La función se vuelve muy especifica cuando lo único que podemos cambiar son las tablas, por eso una buena alternativa puede ser agregar argumento a la función para que aplique en otros contextos también
# Como alternativa muestra una versión usando el formato de función purrr (con ~)

# Nótese algunas cosas:
# Primero es que como estoy trabajando con listas nombradas puedo encadenar map o walk sin problemas. Por eso 
# En una primera instancia se calcula sum_something con map y luegp plot_table con iwalk
# Segundo para que no se genere una lista vacía con los años uso iwalk en vez de imap, pero para que estoy funcione
# es importante agregar un print() al final para que imprima cada gráfico
# Por último como estoy usando iwalk puedo hacer refencia al nombre del elemento de la lista con .y y así puedo
# agregar el año al titulo de una forma más limpia

# Alternativa usando ~
plot_with_purrr <- function(tablas, grupo, variable, titulo) {
  map(tablas, ~sum_something(.x, {{grupo}}, {{variable}})) %>% 
    iwalk(~plot_table(.x, {{grupo}}, n, 
                      paste(titulo, ". Año", .y)) %>% 
            print)
}

plot_with_purrr(gapminder_list, continent, pop, titulo = "Población mundial, según continente")




## Ejercicio 2
# Modifica la función plot_table para que el año del gráfico aparezca en el subtítulo 
# y no en el título.

#función plot_table modificada
plot_table2 <- function(table, x_var, y_var,  input_title, year_subtitle ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle = year_subtitle)
}

# función plot_with_purrr con subtítulo
plot_with_purrr2 <- function(tablas) {
  plots <- imap(tablas, function(.x, .y) {
    table <- sum_something(.x, continent, pop)
    plot_table2(table, continent, n, paste("Población mundial, según continente"), paste("Año", .x$year[1]))
  })
  return(plots)
}

ejercicio2 <- plot_with_purrr2(gapminder_list)

# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# Se modifica correctamente la función y luego se aplica exitosamente en la función anterior.
# Mismo comentario anterior sobre que sería ideal generalizar un poco más la función
# Como alternativa muestra una versión usando el formato de función purrr (con ~)

plot_with_purrr2 <- function(tablas, grupo, variable) {
  map(tablas, ~sum_something(.x, {{grupo}}, {{variable}})) %>% 
    iwalk(~plot_table2(.x, {{grupo}}, n, 
                       paste("Población mundial, según continente."),
                       paste("Año", .y)) %>% 
            print)
}

ejercicio2 <- plot_with_purrr2(gapminder_list, continent, pop)

## Ejercicio 3
# Escribir una función llamada nested_map que utilice una sintaxis de purrr. 
# La función debe recibir dos vectores numéricos (de igual o distinto largo) e 
# imprimir pares de número.

nested_map <- function(x, y) {
  l <- (length({{y}})*length({{x}}))
  xy <- map2(rep({{x}},len = l), rep({{y}},len = l), sum)
  return(xy)
}

nested_map(1:3, 5:8)

# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# En este ejercicio la idea era anidar dos map/walk en un solo ejercicio ya que esa es la unica forma para poder iterar sobre una iteración
# En la solución presentada se puede ver que lo que hace la función es tomar un vector y luego itera sobre cada elemento de ese vector iterando sobre cada elemento del siguiente vector
# Además se utiliza walk en vez de map para obtener el mismo resultado que nos da el 'for'

nested_map <- function(vector_1, vector_2) {
  # Se va a iterar sobre cada elemento del vector 1
  walk(vector_1, 
       # Se toma el elemento del vector 1 y se itera con cada elemento del vector 2
       ~ walk2(.x, vector_2, 
               # La interación consiste en pegar cada elemento del 1 con cada elemento 2
              ~print(paste(.x, .y))))
} 

nested_map(1:3, 5:8)

## BONUS TRACK
# Carga todos los trimestres de 2022 en una lista

files <- list.files("~/clase-curso-morande/02_funcionales/data/datos_ene", full.names = T)
trimestres <- list.files("~/clase-curso-morande/02_funcionales/data/datos_ene") %>% 
  str_extract(pattern = "-[[:digit:]]{2}-") %>% 
  str_remove_all("-")

varios_ene <-  map(files, read_csv2)
names(varios_ene) <-  paste0("trimestre_", trimestres)  
nombres_lista <-  imap(varios_ene, ~.y)
nombres_lista

# Escribe una función llamada get_employment_sample que reciba un dataframe y 
# devuelva la cantidad de ocupados sin expandir. La categoría 1 de la variable 
# activ muestra a los ocupados.

get_employment_sample <- function(tablas) {
  dfs <- imap(tablas, function(.x, .y) {
    table <- count(.x, activ== 1)
  })
  return(dfs)
}

tarea4_2 <- get_employment_sample(varios_ene)

