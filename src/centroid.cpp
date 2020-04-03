# include <Rcpp.h>
using namespace Rcpp;
NumericVector centroid(float x1,
                       float y1,
                       float x2,
                       float y2,
                       float x3,
                       float y3){
            NumericVector c(2);
            c[1] = (x1 + x2 + x3) / 3;
            c[2] = (y1 + y2 + y3) / 3;
            return c;
}

