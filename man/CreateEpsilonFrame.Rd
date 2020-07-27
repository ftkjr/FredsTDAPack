\name{CreateEpsilonFrame}
\alias{CreateEpsilonFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Create a Data Frame of statistics from a matrix of distances
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Creates an m X 3 Data Frame from an input vector of m epsilon values.
For each of the m epsilons we determine the number of points connected and
divide that by the total number of connections possible
(nconnections / n choose 2), then count the number of components present
where the components are determined by a minimum density (minimum number of
points connected)
}
\usage{
CreateEpsilonFrame(dissimilarity_matrix, epsilon_vector, minimum_connections)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dissimilarity_matrix}{
%%     ~~Describe \code{dissimilarity_matrix} here~~
An n X n matrix of pairwise distances, including the 0 diagonal
}
  \item{epsilon_vector}{
%%     ~~Describe \code{epsilon_vector} here~~
A vector of m epsilon values (preferably ordered and m > 1, though these are
just suggestions)
}
  \item{minimum_connections}{
%%     ~~Describe \code{minimum_connections} here~~
The minimum number of points connections required to be considered a component
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
A Data Frame with one row for each of the m epsilon values
}
\references{
%% ~put references to the literature/web site here ~
My Github <- github.com/ftkjr
}
\author{
%%  ~~who you are~~
Frederick T. Kaesmann, Jr.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##### Epsilon Frame from Tidy Text Data ####
  ##### Epsilon Vector ####
  epsilon <- c(seq(from = 0.6, to = 1, by = 0.025))

  ##### Create Epsilon Frame ####
  # Start with Tidy Text
  #
  epsilonFrame <- SouthParkEpsiodText %>%
    JaccardDissimilarityMatrix() %>%
    CreateEpsilonFrame(epsilon, 2)


## The function is currently defined as
function (dissimilarity_matrix, epsilon_vector, minimum_connections)
{
    library(magrittr)
    library(FredsVietorisRips)
    epsilon_frame <- matrix(byrow = F, ncol = 3, nrow = length(epsilon_vector),
        dimnames = list(NULL, c("Epsilon", "Connections", "Components")))
    epsilon_frame[, 1] <- epsilon_vector
    for (ep in c(1:nrow(epsilon_frame))) {
        epsilon_frame[ep, 2] <- dissimilarity_matrix \%>\% AdjacencyMatrix(epsilon_frame[ep,
            1], top_right = TRUE) \%>\% rowSums(na.rm = T) \%>\%
            sum()/choose(nrow(epsilon_frame), 2)
        if (!is.na(epsilon_frame)) {
            epsilon_frame[ep, 3] <- dissimilarity_matrix \%>\%
                AdjacencyMatrix(epsilon_frame[ep, 1]) \%>\% FredsDBSCAN(minimum_connections) \%>\%
                length()
        }
    }
    epsilon_frame \%>\% as.data.frame() \%>\% return()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
