#' @title AdjacencyTidyFrame
#'
#' @param pairwise_distance_matrix A matrix of pairwise distances
#' @param epsilon A given distance
#'
#' @return A tidy frame of points and their adjacencies
#'
#' @examples frame <- AdjacencyTidyFrame(pwdm, ep)
AdjacencyTidyFrame <- function(pairwise_distance_matrix, epsilon,
                               groups = TRUE){
  ##### Packages ####
  if (!require(reshape2)) install.packages("reshape2")
  library(reshape2)
  if (!require(magrittr)) install.packages("magrittr")
  library(magrittr)

  ##### Create Adjacency Matrix ####
  adjacency_matrix <- AdjacencyMatrix(pairwise_distance_matrix, epsilon)

  ##### Iterate through and replace lower half and diagonal with -1 ####
  # n by n so take squareroot
  n <- length(adjacency_matrix) ^ 0.5
  for (i in c(1:n)) {            # row
    for (j in c(1:n)) {          # column
      if (i >= j) {
        adjacency_matrix[i, j] <- -1
      }
    }
  }

  ##### Consolidate matrix into one frame ####
  adjacency_frame <- adjacency_matrix %>%
    melt()

  ##### Change Column and Row Names ####
  colnames(adjacency_frame) <- c("Point_1", "Point_2", "Connection")
  rownames(adjacency_frame) <- c(1:nrow(adjacency_frame))

  ##### Remove Lower Half of Matrix and Diagonal ####
  adjacency_frame <- adjacency_frame[adjacency_frame$Connection > -1, ]

  adjacent_pairs <- AdjacentPairs(adjacency_matrix)

  adjacency_frame <- adjacency_frame %>%
    left_join(adjacent_pairs, by = c("Point_1", "Point_2", "Connection"))

  ##### Return Upper Right Triangle of the Matrix ####
  return(adjacency_frame)

}
