#' @title Pairwisedist
#'
#' @param x vector of x values
#' @param y vector of y values
#'
#' @return A matrix of pairwise distances
#'
#' @examples Pairwisedist(x_vector, y_vector)
Pairwisedist <- function(x, y){
  ##### Source functions ####
  sourceCpp('src/euclideandist.cpp')
  ##### Initialize Empty Matrix ####
  distmat <- matrix(nrow = length(x),
                    ncol = length(y))

  ##### Populate Distance Matrix ####
  for (i in c(1:length(x))) {
    for (j in c(1:length(y))) {
      distmat[i, j] <- euclideandist(x[i], y[i],
                                     x[j], y[j])
    }
  }
  ##### Return Pairwise Distance Matrix ####
  return(distmat)
}
