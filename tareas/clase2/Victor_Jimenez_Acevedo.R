#Tarea_2 Victor_Jimenez

# Instalacion de paquetes
install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse,
               gapminder,
               rlang,
               feather,
               guaguas,
               purrr)

# Creacion de las funciones a utilizar

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

plot_table <- function(table, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

gapminder_list <- split(gapminder, gapminder$year)


#Ejercicio_1


plot_with_purrr <- function(list){
  plot <- list %>% 
  map(~count_something(.,continent,pop)) %>% 
  imap(plot, ~plot_table(.,continent,n,paste("Población mundial, según continente. Año",.y)))
  
}

graficos <-plot_with_purrr(gapminder_list)

graficos[["2007"]]


# Ejercicio 2  ------------------------------------------------------------


plot_table <- function(table, x_var, y_var,  input_title, input_subtitle ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle = input_subtitle)
}

plot_with_purr2 <- function(list){
  plot <- map(list,~count_something(.,continent,pop)) 
  imap(plot_table, ~plot_table(.x,continent,n,paste("Población mundial, según continente."),paste("Año",.y)))
  
}

graficos <-plot_with_purr2(gapminder_list)

graficos[["1957"]]


#no alcance a realizar las demas, por temas de tiempo :( )