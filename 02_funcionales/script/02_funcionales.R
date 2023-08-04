# -------------------------------------------------------------------------
# Clase 2: evaluacion no estandar y funcionales ---------------------------
# -------------------------------------------------------------------------

# 00. Carga de paquetes ---------------------------------------------------
librerias <- c("tidyverse", "gapminder", "rlang", "feather")

carga_librerias <- function(librerias = librerias) {
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


# 01. Cargar datos --------------------------------------------------------

casen <- read_feather("02_funcionales/data/casen_2020_edit.feather")

# 02. Ejemplo con ERROR --------------------------------------------------

calcular_cosas <- function(data, var) {
  data %>% 
    summarise(min = min(var),
              max = max(var),
              mean = mean(var),
              median = median(var)
    )
}

calcular_cosas(gapminder, pop)


# 03. Solucionando el error -----------------------------------------------

calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var)
  data %>% 
    summarise(min = min(!!enquo_var),
              max = max(!!enquo_var),
              mean = mean(!!enquo_var),
              median = median(!!enquo_var)
    )
}
calcular_cosas(gapminder, pop)


# 04. Encapsular expresiones ----------------------------------------------

expr(x + y)

# muy literal
devolver_expresion <- function(x) {
  expr(x)}
devolver_expresion(a + b)

# Ahora si funciona
devolver_expresion <- function(x) {
  enexpr(x)
}
devolver_expresion(a + b)


# 05. Evaluar expresiones encapsuladas ------------------------------------

# Operador bang bang = !!
calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var) 
  data %>% 
    summarise(min = min(!!enquo_var))
}
calcular_cosas(gapminder, pop)


# 06. Usar el nombre de un parámetro para nombrar una variable ------------

# Idea 1 (no funciona)
calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var) 
  data %>% 
    summarise(var = min(!!enquo_var))
}
calcular_cosas(gapminder, pop)

# Idea 2 (error)
calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var) 
  data %>% 
    summarise(!!enquo_var = min(!!enquo_var))
}
calcular_cosas(gapminder, pop)

# Se resuleve con un nuevo operador
calcular_cosas <- function(data, var) {
  enquo_var <- enexpr(var) 
  data %>% 
    summarise(!!enquo_var := min(!!enquo_var))}
calcular_cosas(gapminder, pop)


# 07. Convertir strings en simbolos ---------------------------------------

calcular_cosas <- function(data, var) {
  enquo_var <- sym(var)
  data %>% 
    summarise(!!enquo_var  := min(!!enquo_var))}
calcular_cosas(gapminder, "pop")

#Otra opción con rlang
calcular_cosas <- function(data, var) {
  enquo_var <- parse_expr(var)
  data %>% 
    summarise(!!enquo_var  := min(!!enquo_var))
}
calcular_cosas(gapminder, "pop")


# 08. Uso del nuevo operador {{}} -----------------------------------------

calcular_cosas <- function(data, var) {
  data %>% 
    summarise({{var}} := min({{var}}))
}
calcular_cosas(gapminder, pop)


# EJERCICIO 1 -------------------------------------------------------------

'Trabajaremos con los datos de Casen. Escriba una función llamada sum_something que agrupe por una variable y sume otra. 
Por ejemplo, sum_something(casen, region, ytotcor) debería devolver lo siguiente:'

## # A tibble: 3 × 2
##   region          n
##    <dbl>      <dbl>
## 1      1 2526228202
## 2      2 2577850836
## 3      3 2157490923

# Solucion

'...'


# Parte II
'Crea una función plot_table que acompañe a sum_something. plot_table debe recibir la tabla creada por sum_something y
devolver un gráfico de barras. Es importante que la función reciba el nombre de la variable x y la variable y. Además, 
debe existir un parámetro para agregar un título al gráfico. Pruebe el llamado a la función 
plot_table(table, region, n, "Total del ingreso por región")'



'...'



# 01. Introducción a funcionales ------------------------------------------


# “To understand computations in R, two slogans are helpful:
#   
#   • Everything that exists is an object.
# • Everything that happens is a function call."
# 
# — John Chambers