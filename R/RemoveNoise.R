RemoveNoise <- function(adjacency_matrix) {
  ##### Remove Noise ####
  # If there's a colum who's sum is 0,
  # the point is unconnected and removed
  # as noise

  ##### Initialize Variables ####
  N <- ncol(adjacency_matrix)
  noise_vector <- vector()

  ##### Determine Noise ####
  # If the sum of the connections is 0, the point is noise
  for (i in c(1:N)) {
    if (sum(adjacency_matrix[, i]) == 0) {
      noise_vector <- rbind(noise_vector, i)
    }
  }

  # Assuming there are points to remove
  if (length(noise_vector) > 0) {
    ##### Remove Points ####
    adjacency_matrix <- adjacency_matrix[-noise_vector, -noise_vector]

    ##### Inform User of Number of Points Removed ####
    # New Matrix col length
    n <- ncol(adjacency_matrix)
    if (n < N) {
      message("RemoveNoise: ", N - n, " Points Removed as Noise")
    }


  }



  ##### Return Adjacency Matrix of N > 0 Clusters ####
  return(adjacency_matrix)
}
