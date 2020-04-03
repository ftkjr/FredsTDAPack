#' @title Pairwisedist
#'
#' @param x vector of x values
#' @param y vector of y values
#'
#' @return A matrix of pairwise distances
#'
#' @examples Pairwisedist(x_vector, y_vector)
Pairwisedist <- function(x, y){

  ##### Packages ####
  if (!require(magrittr)) install.packages("magrittr")
  library(magrittr)

  ##### Vector Length ####
  vec_length <- length(x)

  ##### Populate Distance Matrix ####
  distmat <- PairwisedistMatrix(x, y) %>%
    matrix(nrow = vec_length,
           ncol = vec_length,
           dimnames = list(
             # Rownames
             paste("Point", c(1:vec_length)),
             # Colnames
             paste("Point", c(1:vec_length))
           ))

  ##### Return Pairwise Distance Matrix ####
  return(distmat)
}
