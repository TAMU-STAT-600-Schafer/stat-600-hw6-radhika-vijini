#' K-Means Clustering Wrapper Function
#'
#' @param X n by p matrix containing n data points to cluster.
#' @param K An integer specifying number of clusters.
#' @param M K by p matrix of cluster centers.
#' @param numIter number of maximal iterations for the algorithm, the default value is 100.
#'
#' @return Explain return- return all values of class assignments as a vector 
#' @export
#'
#' @examples
#' # Give example

#' set.seed(123)
#' X <- matrix(rnorm(100), nrow = 20)  # 20 points, 5 dimensions
#' # K-means with K = 3 clusters and random initialization
#' result <- MyKmeans(X, K = 3)
#' print(result)
#'  [,1]
#'  [1,]    1
#'  [2,]    1
#'  [3,]    3
#'  [4,]    1
#'  [5,]    1
#'  [6,]    3
#'  [7,]    3
#'  [8,]    1
#'  [9,]    1
#' [10,]    3
#' [11,]    3
#' [12,]    1
#' [13,]    3
#' [14,]    1
#' [15,]    3
#' [16,]    2
#' [17,]    3
#' [18,]    1
#' [19,]    3
#' [20,]    1

MyKmeans <- function(X, K, M = NULL, numIter = 100){
  
  n = nrow(X) # number of rows in X
  
  # Check whether M is NULL or not. If NULL, initialize based on K random points from X. If not NULL, check for compatibility with X dimensions.
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
  
  # Call C++ MyKmeans_c function to implement the algorithm
  Y = MyKmeans_c(X, K, M, numIter)
  
  # Return the class assignments
  return(Y)
}

