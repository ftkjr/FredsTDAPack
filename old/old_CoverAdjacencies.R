old_CoverAdjacencies <- function(list_of_covers, minimum_overlap = 1) {

  ##### Initialize Adjacency Matrix ####
  # We have an $n X n$ matrix where
  #   $n$ is the number of covers
  ncovers <- length(list_of_covers)

  cover_adjacency_matrix <- matrix(
    nrow = ncovers,
    ncol = ncovers
  )

  ##### Iterate through our covers ####
  # If Cover $i$ and Cover $j$ have
  #   $minimum_overlap points$ in
  #   common, then adjacency_matrix[i, j]
  #   gets a 1, otherwise it gets a zero
  for ( i in c(1:ncovers) ) {
    for ( j in c(1:ncovers)) {
      # Concatenate the points which aren't repeated in a set
      tempframe <- rbind(unique(list_of_covers[[i]]), unique(list_of_covers[[j]]))
      # Strip out repeats
      strippedframe <- unique(tempframe)
      # Difference is the points in common
      points_in_common <- nrow(tempframe) - nrow(strippedframe)

      # If they have the right number of points in common
      #   put a 1 in the matrix cell
      if (points_in_common >= minimum_overlap) {
        cover_adjacency_matrix[i, j] <- 1
      } else {
        cover_adjacency_matrix[i, j] <- 0
      }
    }
  }

  return(cover_adjacency_matrix)

}
