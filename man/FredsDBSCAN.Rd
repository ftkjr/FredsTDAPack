\name{FredsDBSCAN}
\alias{FredsDBSCAN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Identify clusters as determined by a
minimum number of connections.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Given an n by n adjacency matrix and a minimum number of adjacencies we
allocate points to cluster based on whether or not they meet the minimum
adjacency requirement.
We iterate through the list of available points until all points which meet
the minimum adjacency requirement have been allocated to a cluster.
}
\usage{
list_of_clusters <- FredsDBSCAN(adjacency_matrix, minimum_connections, untraversed_points = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{adjacency_matrix}{
%%     ~~Describe \code{adjacency_matrix} here~~
An n by n matrix where 1's represent a connection between two points
}
  \item{minimum_connections}{
%%     ~~Describe \code{minimum_connections} here~~
The minimum number of connections required to be allocated to a cluster
}
  \item{untraversed_points}{
%%     ~~Describe \code{untraversed_points} here~~
An index of points to be allocated to clusters
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
Returns a list of point indices where each list entry is a different component
cluster
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
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Example Coming Soon!

## The function is currently defined as
function (adjacency_matrix, minimum_connections, untraversed_points = NULL)
{
    cluster_list <- list()
    cluster <- vector()
    if (is.null(untraversed_points))
        untraversed_points <- c(1:length(adjacency_matrix))
    i <- 1
    while (length(untraversed_points >= minimum_connections)) {
        cluster_info <- get_cluster(adjacency_matrix, minimum_connections,
            untraversed_points)
        if (is.null(cluster_info[[1]]) | is.null(cluster_info[[2]])) {
            return(cluster_list)
        }
        cluster_list[i] <- cluster_info[1]
        untraversed_points <- cluster_info[[2]]
        i <- i + 1
    }
    return(cluster_list)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
