#include <Rcpp.h>
using namespace Rcpp;

// @noRd
// [[Rcpp::export]]
double log_lik_beta_binom2(int x, int size, double prob, double theta) {
  double alpha = prob * 2 * (1 / theta);
  double beta = (1 - prob) * 2 * (1 / theta);
  double lbeta_binom = R::lgammafn(size + 1) - R::lgammafn(x + 1) - R::lgammafn(size - x + 1) +
    R::lgammafn(x + alpha) + R::lgammafn(size - x + beta) - R::lgammafn(size + alpha + beta) +
    R::lgammafn(alpha + beta) - R::lgammafn(alpha) - R::lgammafn(beta);

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
