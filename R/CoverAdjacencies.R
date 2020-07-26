CoverAdjacencies <- function(list_of_covers, minimum_overlap = 1) {


  ##### n Covers choose 2 ####
  combinations <- our_pairs <- matrix(combn(length(list_of_covers), 2),
                                      ncol = 2,
                                      byrow = T)

#   cat("\nn: ", length(list_of_covers), "\n")
#   cat("\nn choose 2: ", nrow(combinations), "\n")

  ##### Initialize Adjacency Matrix ####
  cover_adjacency_matrix <- matrix(0,
    nrow = length(list_of_covers),
    ncol = length(list_of_covers)
    )

  ###### Iterate through our covers ####
  for (element1 in combinations[,1]) {
    for (element2 in combinations[, 2]) {
      # cat("\ne1: ", element1)
      # cat("\ne2: ", element2, "\n")

      if (element1 == element2) {
        cover_adjacency_matrix[element1, element2] <- 0
      } else if (length(intersect(list_of_covers[[element1]], list_of_covers[[element2]])) >= minimum_overlap) {
        # cat("\nElement ", element1, " and element ", element2, " are adjacent\n")
        cover_adjacency_matrix[element1, element2] <- 1
        cover_adjacency_matrix[element2, element1] <- 1
      }
    }
  }

  ##### Return it ####
  return(cover_adjacency_matrix)

}
