librerias <- c("tidyverse", "gapminder", "rlang", "feather", "guaguas")

carga_librerias <- function(librerias) {
  for (i in librerias) {
    esta_disponible <- require(i, character.only = TRUE, quietly= TRUE)
    if(esta_disponible){
      library(i, character.only = TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

carga_librerias(librerias = librerias)

#------------------------------------------------------

gapminder_list <- split(gapminder, gapminder$year) #separa en listas con la info por año 

'sum_something agrupa por una variable y suma otra. '

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

'plot_table debe recibir la tabla creada por sum_something y
devolver un gráfico de barras.'

plot_table <- function(table, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

#-------------------------------------------------------

# Ejercicio 1

plot_with_purr <- function(tablas){
  plots <- tablas %>%
    imap(~sum_something(.x, continent, pop)) %>% 
    imap(~plot_table(.x, continent, n, paste("Poblacion mundial, segun continente. Año", names(tablas[.y]))))
  return(plots)
}

plots_purr <- plot_with_purr(gapminder_list)


## Ejercicio 2

plot_table <- function(table, x_var, y_var,  input_title, input_subtitle ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle=input_subtitle) + theme(plot.title = element_text(hjust= 0.5), plot.subtitle = element_text(hjust= 0.5))
}

plot_with_purr2 <- function(tablas){
  plots <- tablas %>%
    imap(~sum_something(.x, continent, pop)) %>% 
    imap(~plot_table(.x, continent, n, "Poblacion mundial, segun continente", paste("Año",names(tablas[.y]))))
  return(plots)
}

plots_purr2 <- plot_with_purr2(gapminder_list)

## Ejercicio 3

nested_map <- function(v1, v2) {
  walk(v1, ~walk(v2, ~print(paste(.y, .x)),.y=.x))
}

nested_map(1:3, 5:8)

