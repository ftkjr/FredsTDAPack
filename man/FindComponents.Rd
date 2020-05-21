\name{FindComponents}
\alias{FindComponents}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Find Connected Components Inside a List of Covers
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
FindComponents iterates through a list of pull back covers (currently of
x and y coordinates) to determine which points are connected and
differentiates components based on minimum connections from an adjacency matrix.
The adjacency matrix is determined by which points are within epsilon from each
other.
}
\usage{
FindComponents(list_of_covers, epsilon, minimum_connections)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list_of_covers}{
%%     ~~Describe \code{list_of_covers} here~~
A list of pullback covers, in this case iterations of a height function
}
  \item{epsilon}{
%%     ~~Describe \code{epsilon} here~~
Some distance to determine connections between points
}
  \item{minimum_connections}{
%%     ~~Describe \code{minimum_connections} here~~
The minimum number of connections required to define a component
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This should be used after using PullBackCovers, which creates a list of covers
of x and y coordinates
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a list of components by cover. However many entries were in the list
of covers entered, then we have that many entries with some number of comopnents
in each entry.
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
function (list_of_covers, epsilon, minimum_connections)
{
    library(magrittr)
    components <- vector("list", length = length(list_of_covers))
    for (cover in c(1:length(list_of_covers))) {
        components[[cover]] <- Pairwisedist(list_of_covers[[cover]]$x,
            list_of_covers[[cover]]$y) \%>\% AdjacencyMatrix(epsilon) \%>\%
            FredsDBSCAN(min_connections)
    }
    return(components)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
