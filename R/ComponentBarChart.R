#' @title ComponentBarChart
#'
#' @param component_list list of components
#'
#' @return bar chart of the number of components in each cover
#'
ComponentBarChart <- function(component_list) {
  library(magrittr)
  library(ggplot2)

  nComponents <- component_list %>%
    lapply(length) %>%
    unlist()

  component_chart <- data.frame(
    index = c(1:length(component_list)),
    ncomp = nComponents
  ) %>%
    ggplot(aes(x = index,
               y = ncomp)) +
    geom_col() +
    xlab("Ith Cover") +
    ylab("Number of Components") +
    coord_flip() +
    ggtitle("Number of Components in Cover I")

  return(component_chart)

}
