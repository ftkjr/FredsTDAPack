#' @title find_starting_point
#'
#' @param adjacency_matrix An n by n matrix of 1's and 0's where 1 indicates
#'                           two points are adjacent
#' @param minimum_connections points must have at least this many connections
#'                              to be added to the cluster
#' @param untraversed_points a list of points which have not been checked for
#'                             adjacencies
#'
#' @return If there is no starting point the function returns NULL.
#'           Else it returns the starting point and the points to be traversed
#'
find_starting_point <- function(adjacency_matrix, minimum_connections, untraversed_points) {

  ##### Look through the list of points we haven't checked yet ####
  for (starting_point in untraversed_points) {

    ##### If the point has at least the minimum number of connections ####
    # 1. Drop the point from the list of untraversed points
    # 2. Create a list of two entries
    #  - Entry 1: The starting point of the cluster
    #  - Entry 2: The remaining points to check
    # 3. Return the list
    if (sum(adjacency_matrix[starting_point, ]) >= minimum_connections) {
      untraversed_points <- untraversed_points[untraversed_points != starting_point]
      starting_info <- list(starting_point, untraversed_points)
      return(starting_info)
    }

    ##### If we hit the last point in the list at this point ####
    # We haven't found a sufficiently connected point to start our cluster
    # Return NULL
    if (starting_point == untraversed_points[length(untraversed_points)]) {
      return(NULL)
    }
  }
}
