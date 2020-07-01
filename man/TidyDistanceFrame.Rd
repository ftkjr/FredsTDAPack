\name{TidyDistanceFrame}
\alias{TidyDistanceFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Consolidate Matrix of Distances into Tidy Data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Turns an n by n pairwise distance matrix into a tidy data set of n choose 2
observations
}
\usage{
TidyDistanceFrame(pairwise_distance_matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
A matrix of distances, or really any n by n matrix which you would like to
consolidate.
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
Returns a tidy data frame
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
Developed June 15, 2020 during the Great Coronavirus Quarantine
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## The function is currently defined as
function (mat)
{
    df <- matrix(combn(c(1:ncol(mat)), 2), byrow = TRUE, ncol = 2)
    for (r in c(1:nrow(mat))) {
        df[r, 3] <- mat[df[r, 1], df[r, 2]]
    }
    colnames(df) <- c("Element1", "Element2", "Value")
    df <- as.data.frame(df)
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
