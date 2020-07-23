#' @title CountComponents
#'
#' @param adjacency_matrix - an n X n matrix of 1's and 0's indicating adjacencies
#'
#' @return Returns the total number of components within a graph
#'
CountComponents <- function(adjacency_matrix) {

  library(magrittr)

  ##### Step 1: Count Single Points ####
  # 1. Which row sums are 0?
  # 2. If the number of row sums equal to 0
  #      is the number of rows, return the number of rows
  # 3. Else, remove the single points from the adjacency matrix
  # 4. Reset the dimension names for Freds DBSCAN
  singlePoints <- which(rowSums(adjacency_matrix) == 0, useNames = F)
  # cat("\n......sp's: ",length(singlePoints), "...........\n")

  if (length(singlePoints) == nrow(adjacency_matrix)) {
    return(nrow(adjacency_matrix))
  } else if (length(singlePoints) > 0) {
      adjacency_matrix <- adjacency_matrix[-singlePoints, -singlePoints]
      dimnames(adjacency_matrix) <- list(1:nrow(adjacency_matrix), 1:nrow(adjacency_matrix))
  }


  # cat("\n...nrows: ", nrow(adjacency_matrix),"....\n.....ncol: ", ncol(adjacency_matrix),".......\n")



  ##### Step 2: DBSCAN ####
  # Use our already made DBSCAN function
  # to find connected components
  connectedComponents <- adjacency_matrix %>%
    FredsDBSCAN(1) %>%
    length()

  ##### Step 3: Total Components ####
  # Count how many components we have found
  totalComponents <- length(singlePoints) + connectedComponents

  ##### Step 4: Return the Number of Components ####
  return(totalComponents)
}
