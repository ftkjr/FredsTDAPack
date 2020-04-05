# include <Rcpp.h>
using namespace Rcpp;
//` EuclideanDist
//`
//` @param x1 initial x
//` @param y1 initial y
//` @param x2 next x in sequence
//` @param y2 next y in sequence
//` @return d the euclidean distance between two points

// [[Rcpp::export]]
float Euclideandist(float x1,
                    float y1,
                    float x2,
                    float y2){
  float d;
  d = pow(pow((x1 - x2),2) + pow((y1 - y2),2), 0.5);
  return d;
}
