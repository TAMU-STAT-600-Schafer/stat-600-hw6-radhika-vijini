

#' Multi-Class Logistic Regression
#'
#' @param X input matrix of covariates, n x p training data, 1st column should be 1s to account for intercept
#' @param y n length vector of classes, from 0 to K-1
#' @param numIter number of FIXED iterations of the algorithm, default value is 50
#' @param eta learning rate, default value is 0.1
#' @param lambda ridge parameter, default value is 1
#' @param beta_init p x K matrix, initial starting values of beta for the algorithm
#'
#' @return returns a list of two matrices: beta matrix (p x K matrix of estimated beta values after numIter iterations) and objective values matrix ((numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value))
#' @export
#'
#' @examples
#' # Give example
#' 
#' set.seed(42)
#' n <- 100  
#' p <- 3    
#' K <- 2    
#' X <- cbind(1, matrix(rnorm(n * (p - 1)), nrow = n))  
#' y <- sample(0:(K-1), size = n, replace = TRUE)
#' LRMultiClass(X = X, y = y, numIter = 50, eta = 0.1, lambda = 1)  

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
  beta <- beta_init
  # Call C++ LRMultiClass_c function to implement the algorithm
  out = LRMultiClass_c(X = X, y = y, beta_init = beta, numIter = numIter, eta = eta, lambda = lambda)
  
  # Return the class assignments
  return(out)
}


