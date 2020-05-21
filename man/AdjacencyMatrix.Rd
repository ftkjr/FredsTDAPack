\name{AdjacencyMatrix}
\alias{AdjacencyMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Determine point adjacencies by some distance epsilon
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Given a pairwise distance matrix and some distance, epsilon, we determine which
points are adjacent.
}
\usage{
AdjacencyMatrix(pairwise_distance_matrix, epsilon, top_right = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pairwise_distance_matrix}{
%%     ~~Describe \code{pairwise_distance_matrix} here~~
An n by n matrix of distances between points
}
  \item{epsilon}{
%%     ~~Describe \code{epsilon} here~~
Some given distance. Points within this distance are determined to be adjacent.
}
  \item{top_right}{
%%     ~~Describe \code{top_right} here~~
Boolean value. If FALSE returns the entire n by n adjacency matrix. If TRUE
returns only the top right ((n-1) choose 2) values from the matrix.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns an n by n matrix of 1's and 0's indicating point adjacencies.
}
\references{
%% ~put references to the literature/web site here ~
My Github -> github.com/ftkjr
}
\author{
%%  ~~who you are~~
Frederick T. Kaesmann Jr.
}
\note{
%%  ~~further notes~~
Originally developed during the great Coronavirus Quarantine of 2020.

Additionally, developed for use in and during Dr. Aaron Clark's MAT 499
Topological Data Analysis Independent Study.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
--   Check Back Here!!  --
-- Examples Coming Soon --

## The function is currently defined as
function (pairwise_distance_matrix, epsilon, top_right = FALSE)
{
    n <- length(pairwise_distance_matrix)^(0.5)
    adjacency_mat <- pairwise_distance_matrix
    for (i in c(1:n)) {
        for (j in c(1:n)) {
            if (i == j) {
                adjacency_mat[i, j] <- 0
            }
            else if (pairwise_distance_matrix[i, j] < epsilon) {
                adjacency_mat[i, j] <- 1
            }
            else {
                adjacency_mat[i, j] <- 0
            }
            if (top_right == TRUE) {
                if (i >= j) {
                  adjacency_mat[i, j] <- NA
                }
            }
        }
    }
    return(adjacency_mat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
