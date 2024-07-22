#include <Rcpp.h>
using namespace Rcpp;

// @noRd
// [[Rcpp::export]]
double log_lik_beta_binom2(int x, int size, double prob, double theta) {
  double alpha = prob * 2 * (1 / theta);
  double beta = (1 - prob) * 2 * (1 / theta);
  double lbeta_binom = lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1) +
    lgamma(x + alpha) + lgamma(size - x + beta) - lgamma(size + alpha + beta) +
    lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta);

  bool use_binom = theta == 0;

  if (use_binom) {
    return(R::dbinom(x, size, prob, 1));
  }
  if (((x == 0 & prob == 0) | (x == size & prob == 1))) {
    return(0.0);
  }
  if ((x != 0) & (prob == 0)) {
    return(R_NegInf);
  }
  if ((x != size) & (prob == 1)) {
    return(R_NegInf);
  }
  if ((x > size)) {
    return(R_NegInf);
  }
  if ((theta < 0)) {
    return(R_NaN);
  }
  return(lbeta_binom);
}
