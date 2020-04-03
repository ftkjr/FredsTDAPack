\name{Pairwisedist}
\alias{Pairwisedist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Create Pairwise Distance Matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Creates a matrix of pairwise distances of a given set of points in the plane
}
\usage{
Pairwisedist(x_vector, y_vector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A vector of x values
}
  \item{y}{
A vector of y values
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
For use in caluclating the Vietoris-Rips complex for a given set of points.
1-simplexes can be represented as those points in the matrix whose distances
are less than a given epsilon.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a matrix of the pairwise distances of a given set of x and y coordinates
}
\references{
%% ~put references to the literature/web site here ~
Fred's Github <- github.com/ftkjr
}
\author{
%%  ~~who you are~~
Frederick T. Kaesmann, Jr.

Dani Rosenberg

Kun Woo Lee
}
\note{
%%  ~~further notes~~
Developed for Dr. Aaron Clark's MAT 499 Topological Data Analysis Independent
Study, Spring of 2020

Developed During the Great Covid Quarantine of 2020
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
df <- data.frame(
  x = runif(4),
  y = runif(4)
)

Pairwisedist(df$x, df$y)


## The function is currently defined as
#' @title Pairwisedist
#'
#' @param x vector of x values
#' @param y vector of y values
#'
#' @return A matrix of pairwise distances
#'
#' @examples Pairwisedist(x_vector, y_vector)
Pairwisedist <- function(x, y){
  ##### Source functions ####
  sourceCpp('R/euclideandist.cpp')
  ##### Initialize Empty Matrix ####
  distmat <- matrix(nrow = length(x),
                    ncol = length(y))

  ##### Populate Distance Matrix ####
  for (i in c(1:length(x))) {
    for (j in c(1:length(y))) {
      distmat[i, j] <- euclideandist(x[i], y[i],
                                     x[j], y[j])
    }
  }
  ##### Return Pairwise Distance Matrix ####
  return(distmat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }% use one of  RShowDoc("KEYWORDS")
\keyword{ graphs }% __ONLY ONE__ keyword per line
