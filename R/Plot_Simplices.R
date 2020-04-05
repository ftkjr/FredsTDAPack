#' @title Plot_Simplices
#'
#' @param x A vector of x coordinates
#' @param y A vector of y coordinates
#' @param epsilon A distance for determining connectedness
#'
#' @return A graph of 0 and 1 Simplices
#'
#' @examples Plot_Simplices(runif(100), runif(100), 0.5)
Plot_Simplices <- function(x, y, epsilon){
  # Flagship function, this is the doall

  ##### Stop: Check Pairing ####
  if (length(x) > length(y)) {
    stop("Vector of x coordinates is longer than y coordinates.
          Points must be entered in pairs.")
  } else if (length(x) < length(y)) {
    stop("Vector of y coordinates is longer than x coordinates.
          Points must be entered in pairs.")
  }

  ##### Packages ####
  if (!require(reshape2)) install.packages("reshape2")
  library(reshape2)
  if (!require(magrittr)) install.packages("magrittr")
  library(magrittr)
  if (!require(ggplot2)) install.packages("ggplot2")
  library(ggplot2)

  ##### Create Data Frame from Vectors ####
  df <- data.frame(
    x = x,
    y = y,
    Point = paste0("P", c(1:length(x)))
  )

  ##### Pairwise Distance Matrix ####
  pair_dist_mat <- Pairwisedist(df$x, df$y)

  ##### Adjacency Matrix ####
  adjacency_matrix <- AdjacencyMatrix(pwdmat, epsilon)

  ##### Adjacent Pairs ####
  paired_points <- AdjacentPairs(adjacency_matrix)

  ##### Melt Frame and Plot it ####
  graph <- paired_points %>%
    melt(measure.vars = c("Point_1", "Point_2")) %>%
    select(group, value) %>%
    rename(Point = value) %>%
    left_join(df, by = "Point") %>%
    ggplot() +
    geom_line(aes(x, y, group = group)) + # Plot the 1-simplexes
    geom_point(data = df, aes(x, y)) +    # Plot the 0-simplexes
    ggtitle("0 and 1 Simplices")

  ##### Return Graph ####
  return(graph)
}
