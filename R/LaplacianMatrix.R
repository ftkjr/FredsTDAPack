#' @title LaplacianMatrix
#'
#' @param adjacency_matrix an n X n matrix of boolean values
#'
#' @return A Laplacian Matrix for some graph
#'
#' @examples distmat %>% AdjacencyMatrix(epsilon) %>% LaplacianMatrix()
LaplacianMatrix <- function(adjacency_matrix) {
  ##### Initialize New Matrix ####
  laplacian <- (-1) * adjacency_matrix
  ourRowSums <- rowSums(laplacian)

  for (p in 1:nrow(lplacian)) {
    laplacian[p, p] <- ourRowSums[p]
  }

  return(laplacian)
}
