

#' Multi-Class Logistic Regression
#'
#' @param X
#' @param y
#' @param numIter
#' @param eta
#' @param lambda
#' @param beta_init
#'
#' @return
#' @export
#'
#' @examples
#' # Give example
LRMultiClass <- function(X,
                         y,
                         beta_init = NULL,
                         numIter = 50,
                         eta = 0.1,
                         lambda = 1) {
  n <- nrow(X) # save variable of nrows as n, number of observations
  p <- ncol(X) # save variable of ncols as p, number of predictors
  K <- length(unique(y)) #number of class labels
  X <- as.matrix(X)
  
  # Compatibility checks from HW3
  
  # Check that the first column of X is 1s, if not - display appropriate message and stop execution.
  if (any(X[, 1] != 1)) {
    stop("Error: check that the first column of X is 1s.")
  }
  # Check for compatibility of dimensions between X and Y
  if (n != length(y)) {
    stop("Error: check that the dimensions of X and Y are compatible.")
  }
  # Check eta is positive
  if (eta <= 0) {
    stop("Error: Eta must be positive! Change your value of eta.")
  }
  # Check lambda is non-negative
  if (lambda < 0) {
    stop("Error: lambda must be nonnegative! Change your value of lambda.")
  }
  
  #Initialization of beta_init
  
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  if (is.null(beta_init)) {
    beta_init <- matrix(0, nrow = p, ncol = K)
  }
  if ((is.null(beta_init) == FALSE) &&
      ((nrow(beta_init) != p) || ncol(beta_init) != K)) {
    stop("Error: Check that the dimensions of beta are p x K.")
  }
  
  # Call C++ LRMultiClass_c function to implement the algorithm
  out = LRMultiClass_c(X, y, numIter, eta, lambda, beta_init)
  
  # Return the class assignments
  return(out)
}