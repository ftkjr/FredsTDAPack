CoveredPoints <- function(list_of_covers, cover_elements) {
  library(magrittr)

  covered_points <- list_of_covers[cover_elements] %>%
    unlist() %>%
    unique()

  return(covered_points)
}
