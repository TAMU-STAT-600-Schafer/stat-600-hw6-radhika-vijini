# Function that implements K-means algorithm. The default number of maximal iterations is 100.
# X - n by p matrix containing n data points to cluster
# K - integer specifying number of clusters
# M - (optional) K by p matrix of cluster centers
# Y - returns the vector Y of length n of cluster assignments (numbers from 1 to K)
# numIter - number of maximal iterations for the algorithm, the default value is 100
MyKmeans_R <- function(X, K, M = NULL, numIter = 100) {
  X <- as.matrix(X) #initialize X as a matrix and not a data frame or other type
  colnames(X) <- NULL
  
  #EDGE CASE CHECKS
  
  if (any(is.numeric(X)) == FALSE) {
    #if X is not numeric then user should check for error
    stop("Error1: X must be of numeric value.")
  }
  
  if (!is.numeric(K)) {
    #if K is not numeric then user should check for error
    stop("Error2: K must be of numeric value.")
  }
  
  if (is.numeric(numIter) == FALSE) {
    #if numIter is not numeric then user should check for error
    stop("Error3: Number of iterations must be of numeric value.")
  }
  
  if (all(X == 0)) {
    #check that X is not empty
    stop("Error4: X cannot be empty")
    
  }
  if (any(is.na(X))) {
    #check that X cannot contain any NAs
    stop("Error5: X cannot contain NA")
    
  }
  
  if (K > nrow(X)) {
    #check that number of clusters is not more than the data points.
    stop("Error6: You cannot have more clusters than data points. Check your data or K value. ")
  }
  
  
  # Check whether M (matrix of centroids) is NULL or not. If NULL, initialize based on K randomly selected points from X.
  # If not NULL, check for compatibility with X dimensions and K.
  if (is.null(M)) {
    M <- X[sample(1:nrow(X), K), , drop = FALSE] #randomly select K values w/o replacement from the n data points of X - these are indices, then subset the rows (data points) of X which are initial clusters
    
  }
  if (is.null(M) == FALSE) {
    #check if M is not NULL
    
    if ((nrow(M) != K) || (ncol(X) != ncol(M))) {
      #first part of if statement checks if M is not null then the number of rows of M must match the number of clusters
      #second part of if statement checks if M is not null and covariate number in M is not same as covariate number in X matrix then give error
      stop(
        #paste("Number of rows of M are:", nrow(M), "and K is: ", K),
        "Error: Check your M matrix, it seems the number of cluster centers (rows of M) are not the same as the number of clusters K.\n OR the number of columns of M are not the same as the number of covariates (columns of X)"
      )
    }
    
  }
  M <- as.matrix(M)
  # Implement K-means algorithm.
  # It should stop when either
  # (i) the centroids don't change from one iteration to the next (exactly the same), or
  # (ii) the maximal number of iterations was reached, or
  # (iii) one of the clusters has disappeared after one of the iterations (in which case the error message is returned)
  
  
  #initialize number of centroids
  Y  <- matrix(0, nrow = nrow(X), ncol = 1)
  
  for (i in 1:numIter) {
    oldY = Y
    
    #for each cluster, we need to calculate distances between points and centers of clusters and store in the distance matrix
    
    #euclidean distance formula is ((X-M)^2), we want to minimize this
    #open the square we get X^2 + M^2 + -2*XM'
    #when optimizing we don't need to minimize X^2 since it is the data points they don't change from column to column of distance matrix
    row_sum <- .Internal(rowSums(M ^ 2, nrow = nrow(M ^ 2), ncol = ncol(M ^
                                                                          2), FALSE)) #Euclidean distance formula, squared and multiplied out sum^2
    cross_terms <- sweep(#sweeping operation sum for M^2 and cross terms-2 * tcrossprod(X, M),
      #use tcrossprod to reduce time
      -2 * tcrossprod(X, M),
      MARGIN = 2 ,
      STATS = row_sum ,
      FUN = "+")
    
    #assign each data point to the cluster which is the smallest distance away from the point
    #use apply function and which.min()
    Y <- apply(cross_terms, MARGIN  = 1, FUN = which.min)
    
    
    #convergence criterion:
    #check if the cluster centroids are not changing anymore
    #if they are not changing we need to break out of the iteration
    
    if (all(oldY == Y)) {
      #print("cluster centroids are not changing, break out of iteration")
      break
      
    }
    
    #get the new cluster centers
    for (k in 1:K) {
      kth_cluster <- X[Y == k, , drop = FALSE]
      
      #cluster disappearance check:
      #K is number of clusters so if a cluster doesn't have any points assigned to it then it disappears
      #check if the number of clusters in Y is less than K
      if (length(unique(Y)) < K) {
        stop("Error: Change your value of M, at least one of the clusters have disappeared. ")
      }
      #now assign the mean centroid (using colMeans) to the rows of M
      M[k, ] <- .Internal(colMeans(
        kth_cluster,
        nrow(kth_cluster),
        ncol(kth_cluster),
        na.rm = FALSE
      ))
    }
    
    
  }
  
  # Return the vector of assignments Y
  return(as.vector(Y))
}


## TEST R CODE IMPLEMENTATION- no C++
#random data with random init (i.e. M = NULL)
set.seed(123)
X <- matrix(rnorm(100), nrow = 20)  # 20 points, 5 dimensions

# K-means with K = 3 clusters and random initialization
result <- MyKmeans_R(X, K = 3)
print(result)

##TEST C++ IMPLEMENTATION
Rcpp::sourceCpp("src/kmeans.cpp")
source("~/stat-600-hw6-radhika-vijini/R/Kmeans_wrapper.R", echo=TRUE)
set.seed(123)
X2 <- matrix(rnorm(100), nrow = 20)  # 20 points, 5 dimensions

# K-means with K = 3 clusters and random initialization
result2 <- MyKmeans(X2, K = 3)
print(result2)

library(microbenchmark)
require(fossil)

#microbenchmark::microbenchmark(MyKmeans(X, K = 3), MyKmeans(X2, K = 3), times = 10)
# Check R Algorithm

# #another with M specified this time
# set.seed(123)
# X <- matrix(rnorm(100), nrow = 20)  # 20 points, 5 dimensions
# 
# # set M as the first 3 points in X
# M_specific <- X[1:3, ]
# 
# # K-means with K = 3 clusters, starting with specific M
# result <- MyKmeans(X, K = 3, M = M_specific)
# print(result)


## TEST TIME for C++ vs R IMPLEMENTATIONS

set.seed(123)
X <- matrix(rnorm(1000), nrow = 100)  # 100 points with 10 dimensions
K <- 3  

# Run benchmark for C++ implementation vs. R implementation
benchmark_results <- microbenchmark(
  Rcpp_implementation = MyKmeans(X, K),       
  R_implementation = MyKmeans_R(X, K),
  times = 10  
)

print(benchmark_results)
summary(benchmark_results)
