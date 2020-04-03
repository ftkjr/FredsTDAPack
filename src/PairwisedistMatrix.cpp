# include <Rcpp.h>
# include "PairwisedistMatrix.h"
using namespace Rcpp;
// [[Rcpp::export]]

NumericMatrix PairwisedistMatrix(NumericVector x,
                                 NumericVector y){

  int i, j;
  int n = x.size();
  NumericMatrix pwdmat(n);

  for(i = 0; i < n; i++){
    for(j = 0; j < n; j++){
      pwdmat(i, j) = pow(pow((x[i] - x[j]),2) + pow((y[i] - y[j]),2), 0.5);
    }
  }
  return pwdmat;
}
