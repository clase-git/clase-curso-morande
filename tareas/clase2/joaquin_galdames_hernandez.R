# -------------------------------------------------------------------------
# Tarea Clase 2: 
# Joaquín Galdames Hernández
# Análista Socioeconómico ENUT
# -------------------------------------------------------------------------

# 00.1 Carga de paquetes ---------------------------------------------------
librerias <- c("tidyverse", "rlang", "gapminder","purrr")

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

# 00.2 Funciones de tareas ---------------------------------------------------

sum_something <- function(datos, grupo, variable){
  datos %>% 
    group_by({{grupo}}) %>% 
    summarise({{variable}} := sum({{variable}}))
}

plot_table <- function(table, x, y, titulo){
  
  table %>% 
    ggplot(aes(x = {{x}}, y = {{y}})) +
    geom_col() +
    ggtitle(titulo)
  
}

### 01. Ejercicio 1 ---------------------------------------------------

###Pasar esto a purrr
# gapminder_list <- split(gapminder, gapminder$year)
# plot_with_for <- function(tablas){
#   plots <- list(vector(length = length(tablas) ))
#   i <- 1
#   for (plot in tablas) {
#     table <- sum_something(plot, continent, pop)
#     plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1] )  )
#     i <-  i + 1
#   }
#   return(plots)
# }

gapminder_list <- gapminder %>% split(.$year)

# Respuesta: 
plot_with_for <-  function(list, var_1, var_2)
{
  plots <- list %>%
    map(~sum_something(.x, {{var_1}}, {{var_2}})) %>%
    imap(~plot_table(.x, {{var_1}}, {{var_2}}, paste("Población mundial, según continente. Año", .y )))
    
  return(plots)
}

plot_with_for(gapminder_list, continent, pop)

# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# Excelente respuesta cumple con lo solicitado y además generaliza la función para que pueda funcionar con otras variables
# Dos comentarios:
# 1. Creo que sería una buena idea usar iwalk en el segundo paso, de esa forma no se imprime la lista con los años. Pero para ello
# se debe agregar un print al final del gráfico. Esto se ve en el ejemplo a continuación

#2. Otro comentario es que la generalización no afecta al titulo del gráfico entonces sería raro cambiar las variables
# y que el gráfico se mantenga igual. Creo que la solución sería abstraer el titulo como argumento también:

plot_with_purrr <- function(tablas, grupo, variable, titulo) {
  map(tablas, ~sum_something(.x, {{grupo}}, {{variable}})) %>% 
    iwalk(~plot_table(.x, {{grupo}}, n, 
                      paste(titulo, ". Año", .y)) %>% 
            print)
}

plot_with_purrr(gapminder_list, continent, pop, titulo = "Población mundial, según continente")



### 02. Ejercicio 2 ---------------------------------------------------

# Respuesta: 
plot_table2 <- function(table, x, y, titulo, subtitulo){
  
  table %>% 
    ggplot(aes(x = {{x}}, y = {{y}})) +
    geom_col() +
    labs (
      title = titulo,
      subtitle = subtitulo
    )
    
}

plot_with_for2 <-  function(list, var_1, var_2)
{
  plots <- list %>%
    map(~sum_something(.x, {{var_1}}, {{var_2}})) %>%
    imap(~plot_table2(.x, {{var_1}}, {{var_2}}, "Población mundial, según continente", 
                     glue::glue("Año: {.y}"))
         )
  
  return(plots)
}

plot_with_for2(gapminder_list, continent, pop)

# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# Mismo comentarios anteriores, se modifica correctamente la función y la generalización está bien.
# Este sería la respuesta alternativa:

plot_with_purrr2 <- function(tablas, grupo, variable, titulo) {
  map(tablas, ~sum_something(.x, {{grupo}}, {{variable}})) %>% 
    iwalk(~plot_table2(.x, {{grupo}}, n, 
                       titulo, 
                       paste("Año", .y)) %>% 
            print)
}

plot_with_purrr2(gapminder_list, continent, pop, titulo = "Población mundial, según continente")


### 03. Ejercicio 3 ---------------------------------------------------

### for loops inicial
# nested_for <- function(v1, v2) {
#   for (x in v1) {
#     for (y in v2){
#       print(paste(x, y))
#     }
#   }
# }
# 
# nested_for(1:3, 5:8)

# Respuesta: 
map_df(1:3,
         ~ paste(.x, 5:8)) %>% 
  print()


# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# Desgraciadamente el código no funciona usando map_df ya que el resultado es un vector y no lo logra forzar a data.frame
# Utilizando map en vez de map_df no entrega el resultado solicitado.

# En este ejercicio la idea era anidar dos map/walk en un solo código ya que esa es la unica forma para poder iterar sobre una iteración
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



# Lo práctico de utilizar purrr es que permite hacer un código más fácil de leer 😊

### 04. Ejercicio 4 ---------------------------------------------------
####No hice la 4 porque no tenía tiempo y porque no cacho de Markdown 😔


