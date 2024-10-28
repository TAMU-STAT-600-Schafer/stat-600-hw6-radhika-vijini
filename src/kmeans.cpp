// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
#include <stdlib.h>

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::uvec MyKmeans_c(const arma::mat& X, int K,
                            const arma::mat& M, int numIter = 100){
    // All input is assumed to be correct
    
    // Initialize some parameters
    int n = X.n_rows;
    //int p = X.n_cols;
    arma::uvec Y(n); // to store cluster assignments
    
    // Initialize any additional parameters if needed
    arma::uvec oldY(n);
    // For loop with kmeans algorithm
    for (int iter = 0; iter < numIter; ++iter) {
      oldY = Y;  // Store previous assignments
      
      // Compute distances from each point to each centroid
      arma::mat distances(n, K, arma::fill::zeros);
      
    // Returns the vector of cluster assignments
    return(Y+1); //add 1 so we can back to r indexing
}

