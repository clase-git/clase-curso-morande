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
map_df(.x = 1:3,
         ~ paste(.x, 5:8)) %>% 
  print()

# Lo práctico de utilizar purrr es que permite hacer un código más fácil de leer 😊

### 04. Ejercicio 4 ---------------------------------------------------
####No hice la 4 porque no tenía tiempo y porque no cacho de Markdown 😔


