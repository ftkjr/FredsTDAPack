#' Title PullBackCovers
#'
#' @param x vector of x coordinates
#' @param y vector of y coordinates
#' @param resolution
#' @param gain overlap in terms of percentage
#'
#' @return
#' @export
#'
#' @examples
PullBackCovers <- function(x, y, resolution, gain) {

  ##### Initial Info ####
  y_max <- max(df$y)                                 # When to stop
  increment <- resolution * (1 - gain)               # Lower bound iterations
  n_sets <- ceiling(y_max / increment)               # Number of Sets

  ##### Cover Boundaries ####
  # Initialize Variables
  lower <- 0 - increment
  upper <- lower + resolution
  covers <- vector("list", n_sets)
  for (cover_number in c(1:n_sets)) {
    covers[[cover_number]] <- df[df$y > lower & df$y < upper, ] # List entry cover
    lower <- (increment * cover_number)                         # Lower Bound
    upper <- lower + resolution                                 # Upper Bound

  }
  ##### Meaningful List Entry Names ####
  names(covers) <- paste0("Cover_", c(1:n_sets))

  ##### Return the list ####
  return(covers)
}
