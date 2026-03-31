#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector rbuffon_cpp(int n) {
  // Simulate center distance from nearest line: Uniform[0, 0.5]
  NumericVector center_dist = runif(n, 0.0, 0.5);

  // Simulate angle with parallel lines: Uniform[0, pi/2]
  NumericVector angle = runif(n, 0.0, M_PI / 2.0);

  // Crossing condition: 0.5 * sin(angle) >= center_dist
  IntegerVector crosses(n);
  for (int i = 0; i < n; i++) {
    crosses[i] = (0.5 * sin(angle[i]) >= center_dist[i]) ? 1 : 0;
  }

  return crosses;
}

// [[Rcpp::export]]
int rbuffon_count_cpp(int n) {
  // Simulate center distance from nearest line: Uniform[0, 0.5]
  NumericVector center_dist = runif(n, 0.0, 0.5);

  // Simulate angle with parallel lines: Uniform[0, pi/2]
  NumericVector angle = runif(n, 0.0, M_PI / 2.0);

  // Crossing condition: 0.5 * sin(angle) >= center_dist
  int crossings = 0;
  for (int i = 0; i < n; i++) {
    crossings += (0.5 * sin(angle[i]) >= center_dist[i]) ? 1 : 0;
  }

  return crossings;
}
