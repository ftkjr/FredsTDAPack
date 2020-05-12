Noise <- function(adjacency_matrix, min_cluster_size) {
  ##### Noise ####
  # If the sum of row i in the adjacency matrix is
  # less than the min cluster size, we consider
  # it as noise
  noisy_points <- 0
  for(i in c(1:ncol(adjacency_matrix))) {
    connections <- sum(adjacency_matrix[, i], na.rm = T)
    if (connections < min_cluster_size) {
      noisy_points <- rbind(noisy_points, i) # Noise by Point Position
    }
  }

  ##### Return List of Noisy Points ####
  return(noisy_points[noisy_points > 0])
}
