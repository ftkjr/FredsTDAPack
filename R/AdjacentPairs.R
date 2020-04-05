#' @title AdjacentPairs
#'
#' @param adjacency_matrix An adjacency matrix of 0's and 1's
#'
#' @return A list of pairs of points that are adjacent according to
#'          some distance
#'
#' @examples
AdjacentPairs <- function(adjacency_matrix,
                          groups = TRUE){
  ##### Packages #####
  if (!require(magrittr)) install.packages("magrittr")
  library(magrittr)
  if (!require(reshape2)) install.packages("reshape2")
  library(reshape2)
  if (!require(dplyr)) install.packages("dplyr")
  library(dplyr)

  ##### Iterate through and replace lower half and diagonal with 0 ####
  # n by n so take squareroot
  n <- length(adjacency_matrix) ^ 0.5
  for (i in c(1:n)) {            # row
    for (j in c(1:n)) {          # column
      if (i >= j) {
        adjacency_matrix[i, j] <- 0
      }
    }
  }

  ##### Consolidate matrix into one frame ####
  adjacency_frame <- adjacency_matrix %>%
    melt()

  ##### Select Paired Points ####
  adjacency_frame <- adjacency_frame[adjacency_frame$value == 1, ]

  ##### Change Column and Row Names ####
  colnames(adjacency_frame) <- c("Point_1", "Point_2", "Connection")
  rownames(adjacency_frame) <- c(1:nrow(adjacency_frame))

  ##### Add Pair Number ####
  if (groups == TRUE) {
    adjacency_frame$group <- adjacency_frame$Connection %>%
      row_number() %>%
      as.factor()
  }

  ##### Return Frame ####
  return(adjacency_frame)
}
