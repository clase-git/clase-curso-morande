
librerias <- c("tidyverse", "gapminder", "rlang", "feather", "guaguas")

gapminder_list <- split(gapminder, gapminder$year)

#----------------------------------------------------------------------------

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

#----------------------------------------------------------------------------

  #1.-
  
  plot_with_purrr <- function(tablas ) {
    
    plot_<-tablas %>% 
      
      imap(~sum_something(.x, {{continent}}, {{pop}})) %>% 
      imap(~plot_table(.x, {{continent}}, {{n}}, paste("Poblacion mundial, segun continente. Año", tablas[.y])))
    
    return(plot)
  }

# -------------------------------------------------------------------------
# COMENTARIO REVISION: ----------------------------------------------------
# -------------------------------------------------------------------------

# Es importante agregar el signo # al comienzo de cada linea para que se silencie la linea si solo se dejan ----- se corta el código
# La función no entrega resultados y la razón de esto son las {{}} que abrazan las variables.
# Estas llaves se deben usar en argumentos de la función, en el ejemplo que pongo acontinuación se debe notar que agrego
# argumentos llamados grupo y variable que se utilizan para generalizar la función y poder ingresar el nombre de las variables
# al llamar la función

plot_with_purrr <- function(tablas, grupo, variable) {
  
  plot_<-tablas %>% 
    # Notar que se abrazan {{}} las variables para aplicar la función
    imap(~sum_something(.x, {{grupo}}, {{variable}})) %>% 
    # Notar que se cambia n por {{variable}} ya que en la función de más arriba se le cambia el nombre a la columna
    # Además notar que como estamos usando imap podemos llamar el año como nombre de cada tabla con .y
    imap(~plot_table(.x, {{grupo}}, {{variable}}, paste("Poblacion mundial, segun continente. Año", .y)))
  
  return(plot_)
}

plot_with_purrr(gapminder_list, grupo = continent, variable = pop)



#-----------------------------------------------------------------------------
  
  #2.- 
    
    
    plot_table_Modificado <- function(table, x, y, titulo, subtitulo){
      
      table %>% 
        ggplot(aes(x = {{x}}, y = {{y}})) +
        geom_col() +
        labs(title = titulo, subtitle= subtitulo)
      
    }



   # aplicacion

  plot_with_purrr_modificado <- function(tablas ) {
  
  plot_<-tablas %>% 
    
    imap(~sum_something(.x, {{continent}}, {{pop}})) %>% 
    imap(~plot_table_Modificado(.x, {{continent}}, {{n}}, paste("Poblacion mundial, segun continente. Año", tablas[.y])))
  
  return(plot_)
  
  }
  
  # -------------------------------------------------------------------------
  # COMENTARIO REVISION: ----------------------------------------------------
  # -------------------------------------------------------------------------
  
  # Mismos comentarios anteriores
  # Se da una alternativa con las modificaciones
  
  plot_with_purrr <- function(tablas, grupo, variable) {
    
    plot_<-tablas %>% 
      # Notar que se abrazan {{}} las variables para aplicar la función
      imap(~sum_something(.x, {{grupo}}, {{variable}})) %>% 
      # Notar que se cambia n por {{variable}} ya que en la función de más arriba se le cambia el nombre a la columna
      # Además notar que como estamos usando imap podemos llamar el año como nombre de cada tabla con .y
      imap(~plot_table_Modificado(.x, {{grupo}}, {{variable}}, 
                                  paste("Poblacion mundial, segun continente"),
                                  # Notar que se agregar el argumento para el subtitulo
                                  paste("Año :", .y)))
    
    return(plot_)
  }
  
  plot_with_purrr(gapminder_list, grupo = continent, variable = pop)
  
  
  #---------------------------------------------------------------------------
    
    # 3.-
    
    nested_map <- function(v1, v2){}

  # -------------------------------------------------------------------------
  # COMENTARIO REVISION: ----------------------------------------------------
  # -------------------------------------------------------------------------
  
  # En este ejercicio la idea era anidar dos map/walk en un solo ejercicio ya que esa es la unica forma para poder iterar sobre una iteración
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