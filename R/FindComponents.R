#' @title FindComponents
#'
#' @param list_of_covers a list of data frames of x and y coordinates
#' @param epsilon distance to determine adjacencies
#' @param minimum_connections minimum number of adjacent points to make a cluster
#'
#' @return a list of the indices of the points in each respective cover
#'
FindComponents <- function(list_of_covers, epsilon, minimum_connections, notebook = FALSE) {
  Source_FVR_py(notebook)

  ##### How many components do we have? #####
  components <- vector("list", length = length(list_of_covers))
  for (cover in c(1:length(list_of_covers))) {
    components[[cover]] <- Pairwisedist(list_of_covers[[cover]]$x, list_of_covers[[cover]]$y) %>%  # Create a distance matrix
      AdjacencyMatrix(epsilon) %>%                                                               # Determine Adjacent Points
      py$FredsDBSCAN(min_connections)                                                               # Scan for Clusters
  }

  return(components)
}
