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

  ##### Errors ####
  if (length(x) != length(y)) {
    stop("Pairwisedist: x and y vectors not of equal length.\n
         Are you sure everything's paired?")
  }

  ##### Vector Length ####
  vec_length <- length(x)

  ##### Populate Distance Matrix ####
  distmat <- PairwisedistMatrix(x, y) %>%
    matrix(nrow = vec_length,
           ncol = vec_length,
           dimnames = list(
             # Rownames
             paste0("P", c(1:vec_length)),
             # Colnames
             paste0("P", c(1:vec_length))
           ))

  ##### Return Pairwise Distance Matrix ####
  return(distmat)
}
