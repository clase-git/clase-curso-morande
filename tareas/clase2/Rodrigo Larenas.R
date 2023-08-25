# TAREA 1 y 2 -------------------------------------

library(gapminder)
library(dplyr)
library(purrr)
library(ggplot2)

gapminder %>% View()

gapminder_lista <- split(gapminder, gapminder$year)

plot_with_purr <- function(data, year){
  tabla <- data %>%
    group_by(continent) %>%
    summarize(population = sum(pop))
  
  plot <- ggplot(tabla, aes(x = continent, y = population, color = continent)) +
    geom_bar(stat = "identity", lwd = 2, fill = "white") +
    geom_text(aes(label = population), vjust = 2, colour = "black") +
    ggtitle("Poblacion mundial, según Continente.") +
    labs(title = "Poblacion mundial, según Continente.", subtitle = paste("Año", year))
  
  return(plot)
}


anios <- unique(gapminder$year)

lista_graficos <- imap(anios, ~ plot_with_purr(gapminder_lista[[as.character(.)]], .))
print(lista_graficos)



# Ejercicio 3  ------------------------------------------------------------------

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8)


nested_map <- function(v1, v2) {
  walk(v1, function(x) {
    walk(v2, function(y) {
      print(paste(x, y))
    })
  })
}

nested_map(1:3, 5:9)

# Ejercicio extra -------------------------------------------------------------------------

library(readr)


# Carga todos los trimestres de 2022 en una lista

cargar_archivos <- function(file_paths) {
  csv_lista <- map(file_paths, ~ read.csv(.x))
  return(csv_lista)
}

# Lista de rutas de archivos Excel

file_paths <- c("ene-2022-01-def.csv", "ene-2022-02-efm.csv", "ene-2022-03-fma.csv", "ene-2022-04-mam.csv", "ene-2022-05-amj.csv", "ene-2022-06-mjj.csv")

# Cargar los archivos CSV en una lista
cargar_archivos <- cargar_archivos(file_paths)


get_employment_sample <- function(cargar_archivos) {
  cant_ocupados <- sum(cargar_archivos$activ == 1, na.rm = TRUE)
  return(cant_ocupados)
}

data <- cargar_archivos

cant_ocupados <- get_employment_sample(data)
print(cant_ocupados)