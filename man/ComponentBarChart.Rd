\name{ComponentBarChart}
\alias{ComponentBarChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Chart the Number of Components in Each Cover
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
We chart the number of distinct components per cover, as determined by
some distance and minimum number of connections.
}
\usage{
ComponentBarChart(component_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{component_list}{
%%     ~~Describe \code{component_list} here~~
A list of components
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Designed to be used after FindComponents, to visualize the number of components
in each cover.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Chart of the number of components in each cover.
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
function (component_list)
{
    library(magrittr)
    library(ggplot2)
    nComponents <- component_list \%>\% lapply(length) \%>\% unlist()
    component_chart <- data.frame(index = c(1:length(component_list)),
        ncomp = nComponents) \%>\% ggplot(aes(x = index, y = ncomp)) +
        geom_col() + xlab("Ith Cover") + ylab("Number of Components") +
        coord_flip() + ggtitle("Number of Components in Cover I")
    return(component_chart)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
