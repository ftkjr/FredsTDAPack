\name{Eudlideandist}
\alias{Eudlideandist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Determine Euclidean distance between two points
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the Euclidean distance in $\mathbb{R}^2$ and returns it.
}
\usage{
d <- Eudlideandist(x1, y1, x2, y2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x} here~~
Initial x value
}
\item{y1}{
Initial y value
}
\item{x2}{
The next x value
}
\item{y2}{
The next y value
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
Returns the Euclidean distance between two points (x1, y1) and (x2, y2)
}
\references{
%% ~put references to the literature/web site here ~
My Github <- github.com/ftkjr
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
Frederick T. Kaesmann Jr.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Given a set of points (x1, y1) and (x2, y2)
x1 <- 0
y1 <- 0

x2 <- 1
y2 <- 1

Euclideandist(x1, y1, x2, y2)

## The function is currently defined as
float Euclideandist(float x1,
                    float y1,
                    float x2,
                    float y2){
      float d;
      d = pow(pow((x1 - x2),2) + pow((y1 - y2),2), 0.5);
      return d;
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
