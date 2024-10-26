#' Title
#'
#' @param X 
#' @param K 
#' @param M 
#' @param numIter 
#'
#' @return Explain return
#' @export
#'
#' @examples
#' # Give example
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