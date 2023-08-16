# Tarea 2 Curso R Intermedio

library(tidyverse)
library(gapminder)


# Ejercicio 1 -------------------------------------------------------------

gapminder_list <- split(gapminder, gapminder$year)


# función a replicar

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

plot_with_for <- function(tablas){
  plots <- list(vector(length = length(tablas) ))
  i <- 1
  for (plot in tablas) {
    table <- sum_something(plot, continent, pop)
    plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1] )  )
    i <-  i + 1
  }
  return(plots)
}

plots <- plot_with_for(gapminder_list)
plots[1]


# replica utilizando purrr

plot_with_purrr <- function(tablas){
  plots <- tablas %>% 
    map(~sum_something(.x, continent, pop)) %>% 
    imap(~plot_table(.x, continent, n, paste0("Población mundial, según continente. Año ", names(tablas[.y]))))
  return(plots)
}
 
prueba <- plot_with_purrr(gapminder_list)
prueba[3]



# Ejercicio 2 -------------------------------------------------------------

"Modifica la función plot_table para que el año del gráfico aparezca en el subtítulo y no en el título. 
La función modificada debería recibir un parámetro extra llamado subtitulo, que permita agregar el año al subtítulo del gráfico.
Una vez que hayas modificado tu función, utilízala dentro de plot_with_purrr. Cada gráfico debería tener el año correspondiente en el subtítulo."


# función original
plot_table <- function(table, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

# función modificada
plot_table_mod <- function(table, x_var, y_var, titulo, subtitulo ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = titulo,
         subtitle = subtitulo)
}

# ajuste a plot_with_purrr
plot_with_purrr_mod <- function(tablas){
  plots <- tablas %>% 
    map(~sum_something(.x, continent, pop)) %>% 
    imap(~plot_table_mod(.x, continent, n, "Población mundial, según continente", paste0("Año ",names(tablas[.y]))))
  return(plots)
}

prueba2 <- plot_with_purrr_mod(gapminder_list)
prueba2[3]



# Ejercicio 3 -------------------------------------------------------------

"El siguiente for anidado genera pares de x e y. 
El ejercicio consiste en escribir una función llamada nested_map que utilice una sintaxis de purrr. 
La función debe recibir dos vectores numéricos (de igual o distinto largo) e imprimir pares de número.

Es posible que la sintaxis llegue a ser un poco confusa. Reflexiona sobre la pertinencia de purrr para tareas de este tipo."

# función a replicar
nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8)

# replica con purrr
nested_map <- function(v1, v2) {
  walk(v1, ~map(v2, ~print(paste(.y,.x)), .y = .x))
}

nested_map(1:3, 5:8)

## Ejecutar a través de un for loop es más intuititivo que usar purrr para este caso
