#' @title FredsDBSCAN
#'
#' @param adjacency_matrix an n X n matrix of adjacencies predetermined by some
#'                           epsilon
#' @param minimum_connections points with at least this many connections are
#'                              considered part of a cluster
#' @param untraversed_points a vector of points to be checked for allocation to
#'                             a cluster
#'
#' @return a list of clusters
#'
FredsDBSCAN <- function(adjacency_matrix, minimum_connections, untraversed_points = NULL) {
  ##### Initialize Containers ####
  cluster_list <- list()
  cluster <- vector()
  if (is.null(untraversed_points)) untraversed_points <- c(1:length(adjacency_matrix))

  ##### Scan through the adjacency matrix for clusters ####
  # Initialize counter to populate the cluster list
  # Run get_cluster to find clusters based on the remaining untraversed points
  # If get_cluster returns NULL there are no remaining clusters
  # Otherwise populate the cluster list with clusters
  # Then update the remaining points
  # And increase the list entry counter
  i <- 1
  while (length(untraversed_points >= minimum_connections)) {
    cluster_info <- get_cluster(adjacency_matrix, minimum_connections, untraversed_points)
    if (is.null(cluster_info[[1]]) | is.null(cluster_info[[2]])) {
      return(cluster_list)
    }


    cluster_list[i] <- cluster_info[1]
    untraversed_points <- cluster_info[[2]]
    i <- i + 1

  }

  ##### Return a list of clusters ####
  return(cluster_list)

}
