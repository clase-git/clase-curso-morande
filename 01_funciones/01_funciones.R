rm(list=ls())

# Clase 01: funciones con dplyr -------------------------------------------


# Cargar paquetes ---------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               haven,
               tictoc,
               feather)


# Cargar datos ------------------------------------------------------------
casen = read_feather("data/casen_2020_edit.feather")

demon <- read_csv("data/demon_slayer.csv") %>% 
  janitor::clean_names()

categorias <- read_csv("data/categorias_imc.csv")

# rowwise() ---------------------------------------------------------------

mtcars %>% 
  group_by(gear) %>% 
  summarise(media = mean(hp))
  
mtcars2 <- mtcars %>% 
  rowwise() %>% 
  mutate(suma = sum(cyl, disp, hp,drat)) %>% 
  ungroup()

mtcars2 %>% select(cyl, disp, hp,drat, suma)

casen <- casen %>% 
  rowwise() %>% 
  mutate(suma = sum(c_across(starts_with("yta")))) # funciona extremadamente lento

mtcars2 <- mtcars %>% 
  rowwise() %>% 
  mutate(suma = sum(c_across(where(is.numeric)))) %>% 
  ungroup()

mtcars2 %>% select_if(is.numeric)

df <- tibble(
  x = list(1, 2:3, 4:6)
)

df %>% 
  mutate(largo = length(x))

df %>% 
  mutate(largo = map_int(x, length))

df %>% 
  mutate(largo = sapply(x, length))

df %>% 
  rowwise() %>% 
  mutate(largo = length(x))

df <- tribble(
  ~ n, ~ min, ~ max,
  4,     0,     1,
  2,    10,   100,
  3,   100,  1000,
)

df2 <- df %>% 
  mutate(data = runif(n, min, max)) #No crea lo que busco
df2

df2 <- df %>% 
  rowwise() %>% 
  mutate(data = list(runif(n, min, max)))
df2$data[c(1,2,3)]

tic()
casen <- casen %>% 
  rowwise() %>% 
  mutate(suma = sum(yautcorh, ytrabajocor, ymonecorh, yaimcorh, ytotcorh, yoprcor)) %>% 
  ungroup()
toc()

tic()
casen <- casen %>% 
  mutate(sum = rowSums(select(., yautcorh, ytrabajocor, ymonecorh, yaimcorh, ytotcorh, yoprcor))) 
toc()


## Ejercicio con rowwise() (diapo 12) -------------------------------------

casen <- casen %>%
  rowwise() %>% 
  mutate(max = max(y2803c, ytro, y0101c, y0701c, na.rm = T)) %>% 
  group_by(folio) %>% 
  mutate(maximo = max(max)) %>% 
  ungroup()

casen %>% select(folio, max, maximo, y2803c, ytro, y0101c, y0701c)


# "Loops" en dplyr --------------------------------------------------------

mtcars2 <- mtcars
head(mtcars2)
for (var in names(mtcars)) {
  mtcars2[[var]] <- (mtcars[[var]] - min(mtcars[[var]])) / (max(mtcars[[var]]) - min(mtcars[[var]]))
}
head(mtcars2)

mtcars2 <- mtcars %>% 
  mutate_all(~ (. - min(.)) / (max(.) - min(.)) )
head(mtcars2)

mtcars2 <- mtcars %>% 
  mutate_at(vars(mpg, disp), ~ (. - min(.)) / (max(.) - min(.)) )
head(mtcars2)

names(mtcars2)
mtcars2 <- mtcars %>% 
  mutate_at(vars(mpg, disp),  list(norm = ~(. - min(.)) / (max(.) - min(.)) ))

names(mtcars2)

mtcars2 <- mtcars %>% 
  mutate(across(c("mpg", "disp") , .fns =  list(norm = ~(. - min(.)) / (max(.) - min(.)) )))

mtcars2 <- mtcars %>% 
  mutate(across(c("mpg", "disp") , .fns =  list(norm = ~(. - min(.)) / (max(.) - min(.)) ),
                .names = "{.col}_{.fn}"))
names(mtcars2)

mtcars2 <- mtcars %>% 
  mutate_if(is.numeric,  list(norm =  ~(. - min(.)) / (max(.) - min(.)) ))
names(mtcars2)

## Ejercicio mutate(across()) (diapo17) ----------------------------------------------

casen <- casen %>% 
  group_by(folio) %>% 
  mutate(across(c("ytotcor", "yautcor", "ytrabajocor", "yotp"),
                .fns = list(hog = ~sum(.))  ,
                .names = "{.col}_{.fn}")) %>% 
  ungroup()

casen %>% 
  select(folio, ytotcor, ytotcor_hog, yautcor, yautcor_hog) %>% 
  slice(1:4)


## Operaciones complejas ---------------------------------------------------

mtcars2 <- mtcars %>% 
  mutate_at(vars(mpg, disp),  list(norm = ~(. - min(.)) / (max(.) - min(.)) )) %>% 
  mutate_at(vars(mpg_norm, disp_norm),  ~(. + mean(.))) %>% 
  mutate_at(vars(mpg_norm, disp_norm),  ~(. / median(.)))

head(mtcars2 %>% select(mpg, mpg_norm, disp, disp_norm))

do_silly_stuff <- function(x) {
  normalizar <-  (x - min(x)) / (max(x) - min(x))
  norm_media <-  normalizar + mean(normalizar)
  norm_mediana <- norm_media / median(norm_media)
  return(norm_mediana)
}

mtcars2 <- mtcars %>% 
  mutate(across(c("mpg", "disp"),
                .fns = list(norm = ~do_silly_stuff(.))  ))

head(mtcars2 %>% select(mpg, mpg_norm, disp, disp_norm))

#Alternativamente

mtcars2 <- mtcars %>% 
  mutate(across(c("mpg", "disp"),
                .fns = list(norm = do_silly_stuff  )))


head(mtcars2 %>% select(mpg, mpg_norm, disp, disp_norm))

# Repaso "breve" de funciones ---------------------------------------------

sumar_xy <- function(x, y) {
  x + y
}

body(sumar_xy)

formals(sumar_xy)  #argumentos

environment(sumar_xy)

wrapper <- function() {
  sumar_xy <- function(x, y) {
    x + y
  }
  return(environment(sumar_xy))  
}

wrapper

z <- 3
crear_z <- function() {
  z <- 100
  return(z)
}

crear_z()
 
print(z) #Son diferentes

sumar_xy <- function(x, y) {
  x + y + z #z ya estaba definidio
}

sumar_xy(1, 2) 

z <- 1
sumar_xy <- function(x, y) {
  z <- 100
  x + y + z
}
sumar_xy(1, 2)

z <- 1
sumar_xy <- function(x, y) {
  z <- 100
  interna <- function(){
    c(x + y + z)
  }
  interna()
}
sumar_xy(1, 2)

z <- 1
externa <- function(){
  c(x + y + z)
}
sumar_xy <- function(x, y) {
  z <- 100
  externa()
}
sumar_xy(1, 2)

formals(externa) 

z <- 1
externa <- function(x, y){ #agregamos argumentos
  c(x + y + z)
}

sumar_xy <- function(x, y) {
  z <- 100
  externa(x, y)
}
sumar_xy(1, 2)

sumar_xyz <- function(x, y, z) {
  x + y
}
sumar_xyz(1, 2)

sumar_xyz <- function(x, y, z) {
  x + y + z
}
sumar_xyz(1, 2)

sumar_xyz <- function(x, y, z = 5) {
  x + y + z
}
sumar_xyz(1, 2)

sumar_xyz(1, 2, 0.4)


# Ejercicio funciones 1 (diapo 32) -----------------------------------------------------

get_imc <- function(weight, height) {
  imc <- weight /(height / 100) **2
  return(imc)
}

demon <- demon %>% 
  mutate(imc = get_imc(weight = weight_kg, height = height_cm))

demon %>% select(name, imc)

categorias

get_label <- function(imc) {
  if (imc <= 18.5) {
    label <- "bajo peso"
  } else if (imc <= 24.9) {
    label <- "peso normal"
  } else if (imc <= 29.9) {
    label <- "sobrepeso"
  } else if (imc > 29.9) {
    label <- "obesidad"
  }
  return(label)
}

get_label(23)

demon <- demon %>% 
  mutate(imc = get_imc(weight = weight_kg, height = height_cm)) %>% 
  mutate(label = get_label(imc =  imc) )

demon <- demon %>% 
  mutate(imc = get_imc(weight = weight_kg, height = height_cm)) %>% 
  rowwise() %>%
  mutate(label = get_label(imc =  imc) ) %>% 
  ungroup()

demon %>% select(name, imc, label) 

get_label <- function(imc) {
  generate_label <- function(imc) {
    if (imc <= 18.5) {
      label <- "bajo peso"
    } else if (imc <= 24.9) {
      label <- "peso normal"
    } else if (imc <= 29.9) {
      label <- "sobrepeso"
    } else if (imc > 29.9) {
      label <- "obesidad"
    }
    return(label)    
  }
  return(map(imc, generate_label))
}

demon <- demon %>% 
  mutate(imc = get_imc(weight = weight_kg, height = height_cm)) %>% 
  mutate(label = get_label(imc =  imc) )

demon %>% select(name, imc, label) %>% 
  unnest()

# Ejercicio funciones 2 (diapo 37) ----------------------------------------

rnorm(10000)

get_sd <- function(x) {
  cuadrados <- (x - mean(x))**2 # distancias respecto a la media
  suma_cuadrados <- sum(cuadrados) # suma de cuadrados
  n <- length(x) # n
  sqrt(suma_cuadrados / (n - 1)) # salida
}

set.seed(123)
vector <- rnorm(n = 10000)
head(vector)
get_sd(vector)


# Funciones de orden superior ---------------------------------------------

vectorized_function <- function(x, func) {
  new_x <-  do_silly_stuff(x)
  out <-  func(new_x)
  return(out)
}

vectorized_function(c(rnorm(10)), mean)
vectorized_function(c(rnorm(10)), median)

sum_integers <- function(n) {
  total <- 0
  for (i in 1:n) {
    total <- total + i
  }
  return(total)
}

sum_integers(7)


sum_log10 <- function(n) {
  total <- 0
  for (i in 1:n) {
    total <- total + log10(i)
  }
  return(total)
}

sum_log10(5)

sum_power <- function(n) {
  total <- 0
  for (i in 1:n) {
    total <- total + i**2
  }
  return(total)
}

sum_power(3)

#Función genérica
sum_something <- function(n, func) {
  total <- 0
  for (i in 1:n) {
    total <- total + func(i)
  }
  return(total)
}

power2 <- function(x) {x**2}

identity <- function(x) {x}

sum_something(10, log10)

sum_something(10, power2)


# Fábrica de funciones ----------------------------------------------------

factory_root <- function(power_input) {
  new_power <- function(x) {
    x**(1/power_input)    
  }
  return(new_power)
}
root2 <- factory_root(2)
root2(100)
root3 <- factory_root(3)
root3(100)
root4 <- factory_root(4)
root4(100)

general_root <- function(x, power) {x**(1/power)}
general_root(100, 2)
general_root(100, 3)
general_root(100, 4)

add_print <- function(func) {
  string_function <-  rlang::as_string(rlang::ensym(func))
  new_function <- function(x) {
    result <- func(x)
    print(paste0("El resultado de ", string_function,  ": ", result ))
    return(result)
  }
  return(new_function)
}

mean_print <- add_print(mean)

mean_print(c(1,2,3))


# Operadores infix --------------------------------------------------------

3 + 2
`+`(3, 2)

3 * 2
`*`(3, 2)

"perro" + " gato"
3 * " perro"

`+` <- function(x, y) {
  paste(x , y)
}
"perro" + "gato"

3 + 2

rm(`+`)

ambiente_esoterico <- rlang::env()
ambiente_esoterico$`+` <- function(x, y) {
  paste(x , y)
}

rlang::eval_tidy(expr("un" + "saludo"), env =  ambiente_esoterico)

"un" + "saludo"
