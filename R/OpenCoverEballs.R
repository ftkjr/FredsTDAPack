#' @title OpenCoverEballs
#'
#' @param adjacency_matrix an n X n matrix of 1's and 0's
#'
#' @return a list of open covers
#'
#' @examples some_vector %>% dist() %>% AdjacencyMatrix(some_epsilon) %>% OpenCoverEballs()
OpenCoverEballs <- function(adjacency_matrix, messages = FALSE) {
  ###########################
  # Given adjacency matrix
  # Steps:
  # 1) For the ith cover
  # 2) Pick a random (unallocated) point
  # 3) Points adjacent are allocated to that cover
  # 4) Return a list of point indices

  ##### Initialize ####
  i <- 1
  unallocated_points <- c(1:nrow(adjacency_matrix))
  if (messages == TRUE) cat("Unallocated points:\n", unallocated_points,"\n")
  covers <- list()

  ##### Allocate Points to Covers ####
  while (length(unallocated_points) > 0) {
    if (messages == TRUE) cat("\nThe ", length(unallocated_points), " points Remaining:\n", unallocated_points, "\n")

    ##### Pick Random Point ####
    if (length(unallocated_points) > 1) {
      center_point <- sample(unallocated_points, 1)
    } else {
      center_point <- unallocated_points
    }

    if (messages == TRUE) cat("\nCenter Point of Cluster ", i, " is ", center_point, "\n")

    ##### Which Points are Adjacent? ####
    covered_points <- which(adjacency_matrix[center_point, ] == 1)
    if (messages == TRUE) cat("\nThe points in this cover are: \n", covered_points, "\n")

    ##### Add to List of Covers ####
    covers[[i]] <- c(center_point, covered_points)
    if (messages == TRUE) cat("\nThe Cover Contains: ", covers[[i]], '\n')

    ##### Remove Points from Unallocated ####
    unallocated_points <- unallocated_points[! unallocated_points %in% covers[[i]]]
    if (messages == TRUE) cat("\nPoints Remaining: ", length(unallocated_points), "\n")


    ##### Bump Up Iterator ####
    i <- i + 1
  }

  return(covers)

}
