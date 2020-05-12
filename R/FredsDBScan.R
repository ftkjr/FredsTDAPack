FredsDBScan <- function(x, y, epsilon, min_cluster_size) {
  ##### Packages ####
  library(magrittr)

  ##### Error Message ####
  if (length(x) != length(y)) {
    stop("The x and y vectors must be the same length")
  } else if (!is.numeric(x) | !is.numeric(y)) {
    stop("The x and y vectors must be numeric")
  }

  ##### Named Frame ####
  df <- data.frame(
    x = x,
    y = y,
    row.names = paste0("P", c(1:length(x)))
  )

  ##### Adjacency Matrix ####
  adjacency_matrix <- Pairwisedist(x, y) %>%        # Distance Matrix
    AdjacencyMatrix(epsilon, top_right = TRUE) %>%  # Adjacency Matrix
    RemoveNoise()                                   # Strip Noise

  v <- vector()
  l <- list()
  for (i in ncol(adjacency_matrix)) {
    for (j in ncol(adjacency_matrix)) {
      if (i < j) {
        if (adjacency_matrix[i, j] == 1) {
          v <- rbind(v, j)
        }#endif
      }#endif
    }#endfor_j
    l <- append(l, v)
  }#endfor_i

  ##### Return Something ####
  return(l)

}


