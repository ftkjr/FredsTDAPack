#' @title get_cluster
#'
#' @param adjacency_matrix An n by n matrix of 1's and 0's where 1 indicates
#'                           two points are adjacent
#' @param minimum_connections points must have at least this many connections
#'                              to be added to the cluster
#' @param untraversed_points a list of points which have not been checked for
#'                             adjacencies
#'
#' @return a list where entry one the cluster of points (where each point is
#'           epsilon close to another point in the cluster) and entry two are
#'           the remaining untraversed points
#'
get_cluster <- function(adjacency_matrix, minimum_connections, untraversed_points) {

  ##### Initialize Empty Container ####
  cluster <- vector()

  ##### Find the starting point ####
  # find_starting_point returns list object with 2 entries
  #  - first entry is the starting point of the cluster
  #  - second entry is the remaining list of untraversed points
  starting_point <- find_starting_point(adjacency_matrix, minimum_connections, untraversed_points)

  ##### If we can't find a starting point ####
  # Return null
  if (is.null(starting_point)) {
    return(NULL)
  }

  ##### Extract information from starting point list ####
  # Append the starting point to the cluster
  # Then extract the untraversed points
  cluster <- append(cluster, starting_point)
  # cat("\nStarting Point: ", starting_point, "\n")
  # cat("\n cluster: ", cluster, "\n")
  untraversed_points <- untraversed_points[untraversed_points != starting_point]
  # cat("\n utp: ", untraversed_points, "\n")

  ##### For each point in our cluster ####
  point <- 1
  while (point <= length(cluster)) {
    # cat("\n cluster: ", cluster, "\n")
    # cat("\nPoint:", cluster[point], "\n")

    ##### Determine which untraversed points are adjacent to the cluster points ####
    for (other_point in untraversed_points) {
      # cat("\n  other point: ", other_point, "\n")

      ##### If point and other_point are adjacent ####
      # Remove other_point from untraversed points list
      if (adjacency_matrix[cluster[point], other_point] == 1) {
        untraversed_points <- untraversed_points[untraversed_points != other_point]
        # cat("\nConnected Point: ", other_point, "\n")

        ##### If the point has at least the minimum number of connections ####
        # Append it to the cluster
        if (sum(adjacency_matrix[, other_point]) >= minimum_connections) {
          cluster <- append(cluster, other_point)
          # cat("\nClustered Point: ", other_point, "\n")
          # cat("\n cluster: ", cluster, "\n")
        }
      }
    }
    point <- point + 1

  }
  ##### Return the desired information ####
  # Entry one is the discovered cluster
  # Entry two is the remaining untraversed points
  cluster_info <- list(cluster, untraversed_points)
  return(cluster_info)
}
