// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
#include <stdlib.h>

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// X - n by p matrix containing n data points to cluster
// K - integer specifying number of clusters
// M - (optional) K by p matrix of cluster centers
// Y - returns the vector Y of length n of cluster assignments (numbers from 1 to K)
// numIter - number of maximal iterations for the algorithm, the default value is 100

// [[Rcpp::export]]
arma::uvec MyKmeans_c(const arma::mat& X, int K,
                            const arma::mat& M, int numIter = 100){
    // All input is assumed to be correct
    
    // Initialize some parameters
    int n = X.n_rows;
    //int p = X.n_cols;
    arma::uvec Y(n, arma::fill::zeros); // to store cluster assignments
    arma::mat M1;
    M1 = M;
    
    // Initialize any additional parameters if needed
    arma::uvec oldY(n);
    // start for loop
    for (int iter = 0; iter < numIter; ++iter) {
      oldY = Y;  // Store previous assignments
      
      // get distances from each point to each centroid
      arma::mat distances(n, K, arma::fill::zeros);
      for (int k = 0; k < K; ++k) {
        distances.col(k) = arma::sum(arma::square(X.each_row() - M1.row(k)), 1);
      }
      
      // assign each data point to the cluster which is the smallest distance away from the point
      Y = arma::index_min(distances, 1);  
      
      // convergence check: 
      //convergence criterion: check if the cluster centroids are not changing anymore
      //if they are not changing we need to break out of the iteration
      if (arma::all(Y == oldY)) break;
      
      // perform updating of the centroids
      for (int k = 0; k < K; ++k) {
        arma::uvec idx = arma::find(Y == k);
        if (idx.is_empty()) {
          Rcpp::stop("Error: Change your value of M, at least one of the clusters have disappeared.");
        }
        
        M1.row(k) = arma::rowvec(arma::mean(X.rows(idx), 0));
        
      }
      
    }
    
    // Returns the vector of cluster assignments
    return(Y+1); //add 1 so we can back to r indexing
}

