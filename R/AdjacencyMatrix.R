#' @title AdjacencyMatrix
#'
#' @param pairwise_distance_matrix A matrix of pairwise distances
#' @param epsilon A given distance
#'
#' @return A matrix of 1's and 0's where 1's denote adjacency between
#'          points as determined by the given distance, epsilon
#'
AdjacencyMatrix <- function(pairwise_distance_matrix, epsilon,
                            top_right = FALSE){

  ##### Matrix Dimensions ####
  n <- length(pairwise_distance_matrix) ^ (0.5)

  ##### Copy for Replacement ####
  adjacency_mat <- pairwise_distance_matrix

  ##############################################################
  ##### If points (xi, yi) and (xj, yj) are within epsilon    ##
  ##### then assign a 1 to the cell in the adjacency matrix   ##
  ##### else assign a 0                                       ##
  for (i in c(1:n)) {
    for (j in c(1:n)) {
      if ( i == j) {
        adjacency_mat[i, j] <- 0
      } else if (pairwise_distance_matrix[i, j] < epsilon) {
        adjacency_mat[i, j] <- 1
      } else {
        adjacency_mat[i, j] <- 0
      }

      if (top_right == TRUE) {
        if (i >= j) {
          adjacency_mat[i, j] <- NA
        }

      }
    }
  }
  return(adjacency_mat)
}
