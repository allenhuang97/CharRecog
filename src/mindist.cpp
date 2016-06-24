#include <math.h>
#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
double mindist(NumericMatrix train, NumericMatrix test){
  int colNum = train.ncol();
  int rowNum = train.nrow();
  arma::mat trainArma = as<arma::mat>(train);
  arma::mat testArma = as<arma::mat>(test);
  arma::mat resultArma(rowNum,colNum);
  resultArma = trainArma - testArma;
  resultArma*=resultArma;
  double dis = 0;
  for(int c = 0; c < colNum; c++){
    for(int r = 0; r < rowNum; r++){
      dis+=resultArma.at(r,c);
    }
  }
  return sqrt(dis);
}