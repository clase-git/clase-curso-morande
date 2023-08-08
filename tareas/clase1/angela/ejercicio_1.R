x <- c(1, 2, 3, 5, 8, 13)

y <- c(4, 8, 12, NA, 99, - 20, NA)


get_cv <- function(vector, remove_na = FALSE) {
  if (remove_na) {
    vector <- vector[!is.na(vector)]
  }
  
  cv <- sd(vector)/mean(vector)
  return(cv)
}

get_cv(x)
get_cv(y)





