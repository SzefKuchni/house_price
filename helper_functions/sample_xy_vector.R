#prepare x y vectors

xy_vector <- function(explained_variable,ignored_variables) {
  y <<- explained_variable
  x <- setdiff(names(train), y)
  x <<- setdiff(x, ignored_variables)
}