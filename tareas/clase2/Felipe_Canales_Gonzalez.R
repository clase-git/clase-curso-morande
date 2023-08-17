
librerias <- c("tidyverse", "gapminder", "rlang", "feather", "guaguas")

gapminder_list <- split(gapminder, gapminder$year)

----------------------------------------------------------------------------

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

----------------------------------------------------------------------------

  #1.-
  
  plot_with_purrr <- function(tablas ) {
    
    plot_<-tablas %>% 
      
      imap(~sum_something(.x, {{continent}}, {{pop}})) %>% 
      imap(~plot_table(.x, {{continent}}, {{n}}, paste("Poblacion mundial, segun continente. Año", tablas[.y])))
    
    return(plot)
  }
    
 -----------------------------------------------------------------------------
  
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
  }
  
  return(plot)
  
  ---------------------------------------------------------------------------
    
    # 3.-
    
    nested_map <- function(v1, v2){}
  
  