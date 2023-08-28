# Tarea 1
# Claudia Allendes Varas

library(tidyverse)
library(gapminder)

# Pregunta 1


sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

plot_table1 <- function(table, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

gapminder_list <- split(gapminder, gapminder$year)
nombres_lista <- imap(gapminder_list, ~.y)
names(nombres_lista)

plot_with_purr1 <- function(list){
  plots_by_year1 <- list %>% 
    map(sum_something, continent, pop) %>% 
    map(plot_table1, continent, n, paste0("Población mundial, según continente. Año ", nombres_lista))
  return(plots_by_year1)
}

plots_purr1 <- plot_with_purr1(gapminder_list)
walk(plots_purr1, print)


# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# Es una buena solución al ejercicio la función da el resultado correcto y se usa map correctamente.
# Además es un buen uso de walk para que imprima los gráficos pero no la lista en la consola.
# Como comentario es importante generalizar las funciones para que pueda ser usadas en otras instancias.
# Segundo comentario, debido a que nuestra lista es nombrada podemos usar imap/iwalk para poner el año
# A continuación se presenta una alternativa a la función generalizando los argumentos y utilizando iwalk

# Debe notar que se agrega el walk a la función junto con el print luego de usar plot_tabla1. Esto es una alternativa a lo antes presentado.

plot_with_purr1 <- function(list, grupo, variable){
  list %>% 
    map(sum_something, {{grupo}}, {{variable}}) %>% 
    iwalk(~plot_table1(.x, {{grupo}}, n,  paste0("Población mundial, según continente. Año ", .y)) %>% 
            print)
}

plot_with_purr1(gapminder_list, continent, pop)


# Pregunta 2

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}


plot_table2 <- function(table, x_var, y_var,  input_title, input_subtitle ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle = input_subtitle)
}


plot_with_purr2 <- function(list){
  plots_by_year2 <- list %>% 
    map(sum_something, continent, pop) %>% 
    map(plot_table2, continent, n, "Población mundial, según continente", paste0("Año ", nombres_lista))
  return(plots_by_year2)
}

plots_purr2 <- plot_with_purr2(gapminder_list)
walk(plots_purr2, print)

# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# Mismo comentario para el ejercicio anterior, se presenta una actualización del comentario anterior:


plot_with_purr1 <- function(list, grupo, variable){
  list %>% 
    map(sum_something, {{grupo}}, {{variable}}) %>% 
    iwalk(~plot_table2(.x, {{grupo}}, n,  
                       paste0("Población mundial, según continente"),
                       paste0("Año ", .y)) %>% 
            print)
}

plot_with_purr1(gapminder_list, continent, pop)

# Pregunta 3

# Sea;
nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8)

# Crear función nested_map

nested_map <- function(v1, v2) {
  walk(v1,~ walk(v2, ~ print(paste(.y, .x)),.y=.x))
}
nested_map(1:3, 5:8)

# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# Excelente respuesta se logra replicar el for, falta la reflexión sobre el uso de map en este tipo de ejercicios
# Se presenta una solución alternativa usando walk2

nested_map <- function(vector_1, vector_2) {
  # Se va a iterar sobre cada elemento del vector 1
  walk(vector_1, 
       # Se toma el elemento del vector 1 y se itera con cada elemento del vector 2
       ~ walk2(.x, vector_2, 
               # La interación consiste en pegar cada elemento del 1 con cada elemento 2
               ~print(paste(.x, .y))))
} 

nested_map(1:3, 5:8)