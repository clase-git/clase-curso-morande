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

# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# La función responde a la pregunta correctamente. se puede hacer mini correcciones por ejemplo como estamos usando imap en el plot_table y la lista de tablas
# tiene nombre entonces podemos usar directamente .y y no es necesario agregar: names(tablas[.y])
# Otro comentario es que la función se vuelve muy especifica al dejar contienen y pop dentro de la misma. En algunas ocasiones esto está bien, pero
# a modo de ejemplo la alternativa de respuesta siguiente intenta generalizar la función agregando más argumentos:

plot_with_purrr <- function(tablas, grupo, variable, titulo) {
  map(tablas, ~sum_something(.x, {{grupo}}, {{variable}})) %>% 
    iwalk(~plot_table(.x, {{grupo}}, n, 
                      paste(titulo, ". Año", .y)) %>% 
            print)
}

plot_with_purrr(gapminder_list, continent, pop, titulo = "Población mundial, según continente")

# Nótese algunas cosas:
# Se agregan 3 argumentos que varian dependiendo de la variable que queramos revisar y tambien se deja titulo ya que depende de las variables también.
# Primero para que no se genere una lista vacía con los años uso iwalk en vez de imap, pero para que estoy funcione
# es importante agregar un print() al final para que imprima cada gráfico
# Por último como estoy usando iwalk puedo hacer refencia al nombre del elemento de la lista con .y y así puedo
# agregar el año al titulo de una forma más limpia



## Ejercicio 2

plot_table <- function(table, x_var, y_var,  input_title, input_subtitle ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle=input_subtitle) + 
    theme(plot.title = element_text(hjust= 0.5), plot.subtitle = element_text(hjust= 0.5))
}

plot_with_purr2 <- function(tablas){
  plots <- tablas %>%
    imap(~sum_something(.x, continent, pop)) %>% 
    imap(~plot_table(.x, continent, n, "Poblacion mundial, segun continente", paste("Año",names(tablas[.y]))))
  return(plots)
}

plots_purr2 <- plot_with_purr2(gapminder_list)


# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# Se modifica correctamente la función y luego se aplica exitosamente en la función anterior.
# Mismo comentario anterior sobre que sería ideal generalizar un poco más la función

plot_with_purrr2 <- function(tablas, grupo, variable, titulo) {
  map(tablas, ~sum_something(.x, {{grupo}}, {{variable}})) %>% 
    iwalk(~plot_table2(.x, {{grupo}}, n, 
                      titulo, 
                      paste("Año", .y)) %>% 
            print)
}

plot_with_purrr2(gapminder_list, continent, pop, titulo = "Población mundial, según continente")



## Ejercicio 3

nested_map <- function(v1, v2) {
  walk(v1, ~walk(v2, ~print(paste(.y, .x)),.y=.x))
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