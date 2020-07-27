\name{CountComponents}
\alias{CountComponents}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Count the number of distinct components within an adjacency matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Count the number of floating points and connected components to get the total
number of components within some graph.
}
\usage{
d \%>\% AdjacencyMatrix(epsilon) \%>\% CountComponents()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{adjacency_matrix}{
%%     ~~Describe \code{adjacency_matrix} here~~
An n X n matrix of 1's and 0's where 1 indicates a connection between two points
and 0 indicates not connected
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
Returns an integer indicating the number of components within a graph
}
\references{
%% ~put references to the literature/web site here ~
My Github -> github.com/ftkjr
}
\author{
%%  ~~who you are~~
Frederick T. Kaesmann, Jr.
}
\note{
%%  ~~further notes~~
Initially developed during the Great Coronavirus Quarantine of 2020
(still here, in July)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Create a data frame of x and y components
df <- data.frame(
  x = runif(20),
  y = runif(20)
)

# Turn the data frame into a distance matrix
d <- dist(df) \%>\%
  as.matrix()

# Distance matrix to Adjacency Matrix
# Then count the components
d \%>\%
  AdjacencyMatrix(0.25) \%>\%
  CountComponents()


## The function is currently defined as
function (adjacency_matrix)
{
    library(magrittr)
    singlePoints <- which(rowSums(adjacency_matrix) == 0, useNames = F)
    cat("\n......sp's: ", length(singlePoints), "...........\n")
    if (length(singlePoints) == nrow(adjacency_matrix)) {
        return(nrow(adjacency_matrix))
    }
    else if (length(singlePoints) > 0) {
        adjacency_matrix <- adjacency_matrix[-singlePoints, -singlePoints]
        dimnames(adjacency_matrix) <- list(1:nrow(adjacency_matrix),
            1:nrow(adjacency_matrix))
    }
    cat("\n...nrows: ", nrow(adjacency_matrix), "....\n.....ncol: ",
        ncol(adjacency_matrix), ".......\n")
    connectedComponents <- adjacency_matrix \%>\% FredsDBSCAN(1) \%>\%
        length()
    totalComponents <- length(singlePoints) + connectedComponents
    return(totalComponents)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
