
# Solución Ejercicio 1:

library(gapminder)
library(dplyr)
library(tidyverse)
library(purrr)
library(ggplot2)


gapminder_list <- split(gapminder, gapminder$year)


plot_with_purr <- function(data, year){
  table <- data %>%
    group_by(continent) %>%
    summarize(population = sum(pop))
  
  plot <- ggplot(table, aes(x = continent, y = population, fill = continent)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Poblacion mundial, según continente. Año", year))
  
  return(plot)
}


years <- unique(gapminder$year)


plot_list <- imap(years, ~ plot_with_purr(gapminder_list[[as.character(.)]], .))
print(plot_list)


# Solución Ejercicio 2:

gapminder_list <- split(gapminder, gapminder$year)

plot_with_purr <- function(data, year){
  table <- data %>%
    group_by(continent) %>%
    summarize(population = sum(pop))
  
  subtitulo <- paste("Año:", year)
  
  plot <- ggplot(table, aes(x = continent, y = population, fill = continent)) +
    geom_bar(stat = "identity") +
    labs(title = "Poblacion mundial, según continente.",
         subtitle = subtitulo,
         x = "Continente", y = "Población") 
  
  return(plot)
}

years <- unique(gapminder$year)

plot_list <- imap(years, ~ plot_with_purr(gapminder_list[[as.character(.)]], .))
print(plot_list)


# Solución Ejercicio 3:

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2) {
      
      print(paste(x, y))
    }
  }
}

nested_for(1:3, 5:8)

## Comprobamos con la función "map" de la sintaxis "purr"

nested_map <- function(v1, v2) {
  map(v1, function(x) {
    map(v2, function(y) {
      print(paste(x, y))
    })
  })
}

nested_map(1:3, 5:8)

# OBSERVACIONES: Se puede observar que los resultados de "nested_for" son distintos que "nested_map" al aplicar la función map() de purr. Tratamos con otra función.


## Comprobamos ahora con la función "walk" de la misma sintaxis "purr"

nested_map1 <- function(v1, v2) {
  walk(v1, function(x) {
    walk(v2, function(y) {
      print(paste(x, y))
    })
  })
}

nested_map1(1:3, 5:8)

# CONCLUSIÓN: Al utilizar la función walk(), llevamos al mismo resultado que la función nested_for. Por lo tanto, es importante destacar que la sintaxís "purr"
# puede ofrecer bastante dinamismo a una lista de objeto, y por consiguiente es el trabajo del analista de datos de saber que resultado para plantear la correcta
# función en el ambiente de "purr".


