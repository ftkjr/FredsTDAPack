#' @title CreateEpsilonFrame
#'
#' @param dissimilarity_matrix an n X n matrix of pairwise distances
#' @param epsilon_vector an ordered vector of epsilon values
#' @param minimum_connections the minimum number of connections for a point to
#'                              be allocated to a cluster
#'
#' @return a length(epsilon_vector) X 3 dataframe
#'
CreateEpsilonFrame <- function(dissimilarity_matrix, epsilon_vector, zero_to_1 = TRUE) {

  ##### Packages ####
  library(magrittr)
  library(FredsVietorisRips)

  ##### Initialize Frame ####
  epsilon_frame <- matrix(
    byrow = F,
    ncol = 3,
    nrow = length(epsilon_vector),
    dimnames = list(NULL, c("Epsilon", "Connections", "Components"))
  )

  ##### Column 1 is Epsilon Vector ####
  epsilon_frame[, 1] <- epsilon_vector

  ##### For each Epsilon Value Populate Frame ####
  # Column 2 is # of connections
  # Column 3 is # of connected components

  for (ep in c(1:nrow(epsilon_frame))) {

    ##### Count Connections ####
    epsilon_frame[ep, 2] <- dissimilarity_matrix %>%
      AdjacencyMatrix(epsilon_frame[ep, 1], top_right = TRUE) %>%
      rowSums(na.rm = T) %>%
      sum()

    ##### Connected components ####
    # Wait till we actually have connections
    # to look for connected components
      epsilon_frame[ep, 3] <- dissimilarity_matrix %>%
        AdjacencyMatrix(epsilon_frame[ep, 1]) %>%
        CountComponents()
  }

  if (zero_to_1 == TRUE) {
    epsilon_frame[, 2] <- epsilon_frame[, 2] / choose(nrow(dissimilarity_matrix), 2)
    epsilon_frame[, 3] <- epsilon_frame[, 3] / nrow(dissimilarity_matrix)
  }

  ##### Return the Epsilon Frame ####
  epsilon_frame %>%
    as.data.frame() %>%
    return()
}

