# Kevin Jaña Aliaga
# Taria 2

# Previo ------------------------------------------------------------------

paquetes <- c("tidyverse","gapminder","rlang","feather","guaguas")

for(p in paquetes) {
  
  if (!require(p,character.only = TRUE))    install.packages(p);    library(p,character.only = TRUE)
  
}

rm("paquetes","p")


# Funciones ---------------------------------------------------------------

count_something <- function(data, group_var, var) {
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


# Ejercicio 1 -------------------------------------------------------------


plot_with_purrr <- function(lista){
  nueva_lista <- map(lista,~count_something(.,continent,pop)) 
  imap(nueva_lista, ~plot_table(.,continent,n,paste("Población mundial, según continente. Año",.y)))
  
}

graficos <-plot_with_purrr(gapminder_list)

graficos[["2007"]]


# Ejercicio 2  ------------------------------------------------------------


plot_table <- function(table, x_var, y_var,  input_title, input_subtitle ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title,
         subtitle = input_subtitle)
}

plot_with_purrr <- function(lista){
  nueva_lista <- map(lista,~count_something(.,continent,pop)) 
  imap(nueva_lista, ~plot_table(.,continent,n,paste("Población mundial, según continente."),paste("Año",.y)))
  
}

graficos <-plot_with_purrr(gapminder_list)

graficos[["1957"]]


# Ejercicio 3 -------------------------------------------------------------


nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}

nested_for(1:3,5:8)

nested_map <- function(v1, v2) {

  a = rep(v1, times = length(v2))
  b = rep(v2, times = length(v1))

 walk2(list(a),list(b),print(paste(a,b)))
  
}

nested_map(1:3,5:8)
