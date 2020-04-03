# include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector centroid(NumericVector x,
                       NumericVector y){
            NumericVector c(2);
            c[1] = (x[1] + x[2] + x[3]) / 3;
            c[2] = (y[1] + y[2] + y[3]) / 3;
            return c;
}

