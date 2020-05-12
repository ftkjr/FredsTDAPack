#' @title Plot_Simplices
#'
#' @param x A vector of x coordinates
#' @param y A vector of y coordinates
#' @param epsilon A distance for determining connectedness
#'
#' @return A graph of 0 and 1 Simplices
#'
#' @examples Plot_Simplices(runif(100), runif(100), 0.5)
Plot_1_Simplices <- function(x, y, epsilon){
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
  pwdmat <- Pairwisedist(df$x, df$y)

  ##### Adjacency Matrix ####
  adjacency_matrix <- AdjacencyMatrix(pwdmat, epsilon)

  ##### Adjacent Pairs ####
  paired_points <- AdjacentPairs(adjacency_matrix)

  ##### Combine Paired Points with their Coordinates ####
  ppc <- PairedPointCoordinates(paired_points, df)

  ##### Create Graph of 0-Simplices ####
  graph <- Plot_0_Simplices(df$x, df$y)

  ##### Melt Frame and Plot it ####
  graph <- graph +
    geom_line(data = ppc,
              aes(x, y, group = group)) + # Plot the 1-simplexes
    ggtitle("0 and 1 Simplices")

  ##### Return Graph ####
  return(graph)
}
