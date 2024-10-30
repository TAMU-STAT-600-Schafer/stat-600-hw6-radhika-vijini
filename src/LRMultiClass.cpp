// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// For simplicity, no test data, only training data, and no error calculation.
// X - n x p data matrix
// y - n length vector of classes, from 0 to K-1
// numIter - number of iterations, default 50
// eta - damping parameter, default 0.1
// lambda - ridge parameter, default 1
// beta_init - p x K matrix of starting beta values (always supplied in right format)
// [[Rcpp::export]]
Rcpp::List LRMultiClass_c(const arma::mat &X, const arma::uvec &y, const arma::mat &beta_init,
                          int numIter = 50, double eta = 0.1, double lambda = 1)
{
  // All input is assumed to be correct

  // Initialize some parameters
  int K = max(y) + 1; // number of classes
  int p = X.n_cols;
  int n = X.n_rows;
  arma::mat beta = beta_init;       // to store betas and be able to change them if needed
  arma::vec objective(numIter + 1); // to store objective values

  // Initialize anything else that you may need
  // Indicator matrix for y
  arma::mat ind_train = arma::zeros(n, K);
  for (int i = 0; i < n; ++i)
  {
    ind_train(i, y(i)) = 1.0;
  }

  // Compute initial pk
  arma::mat X_beta = X * beta;                 // store Xbeta matrix
  arma::mat exp_Xb = arma::exp(X_beta);        // intermediate storage of exp(Xb)
  arma::vec row_sums = arma::sum(exp_Xb, 1);   // perform row sums
  arma::mat pk = exp_Xb.each_col() / row_sums; // calculate corresponding pk

  // Compute initial objective value
  arma::mat log_pk = arma::log(pk);                                                               // log objective value
  double obj = -arma::accu(ind_train % log_pk) + (lambda / 2.0) * arma::accu(arma::square(beta)); // calculate current objective value
  objective(0) = obj;                                                                             // set first value of objective as this initial obj

  // Newton's method cycle - implement the update EXACTLY numIter iterations

  for (int iter = 0; iter < numIter; ++iter)
  {
    // Compute W, as given formula in the pdf
    arma::mat W = pk % (1.0 - pk);

    // Update beta for each class
    // Within one iteration: perform the update, calculate updated objective function
    // beta update
    for (int j = 0; j < K; ++j)
    {
      arma::vec Wj = W.col(j);
      arma::mat XWj = X.each_col() % Wj;
      arma::mat Hkk = X.t() * XWj + lambda * arma::eye(p, p);
      arma::vec grad = X.t() * (pk.col(j) - ind_train.col(j)) + lambda * beta.col(j);
      arma::mat Hkk_inv = arma::inv_sympd(Hkk);         // Use inv_sympd for symmetric positive-definite matrices
      beta.col(j) = beta.col(j) - eta * Hkk_inv * grad; // damped newton's update
    }

    // Update pk
    X_beta = X * beta;                 // store Xbeta matrix
    exp_Xb = arma::exp(X_beta);        // intermediate storage of exp(Xb)
    row_sums = arma::sum(exp_Xb, 1);   // perform row sums
    pk = exp_Xb.each_col() / row_sums; // calculate corresponding pk

    // Update objective value
    log_pk = arma::log(pk);                                                                  // log objective values
    obj = -arma::accu(ind_train % log_pk) + (lambda / 2.0) * arma::accu(arma::square(beta)); // calculate objective value
    objective(iter + 1) = obj;                                                               // next iteration
  }

  // Create named list with betas and objective values
  return Rcpp::List::create(Rcpp::Named("beta") = beta,
                            Rcpp::Named("objective") = objective);
}
