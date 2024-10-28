

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
#' 
#' set.seed(42)
#' n <- 100  # Number of observations
#' p <- 3    # Number of predictors (including intercept)
#' K <- 2    # Number of classes
#' X <- cbind(1, matrix(rnorm(n * (p - 1)), nrow = n))  # Generate predictors
#' beta_true <- matrix(rnorm(p * K), nrow = p)   # True beta coefficients
#' eta_mat <- X %*% beta_true    # Compute linear predictors
#' exp_eta <- exp(eta_mat)
#' probs <- exp_eta / rowSums(exp_eta)   # Compute probabilities using softmax
#' y <- apply(probs, 1, function(prob) sample(0:(K - 1), 1, prob = prob))   # Generate response variable y
#' y <- as.integer(y)   # Convert y to integer type
#' beta_init <- matrix(0, nrow = p, ncol = K)   # Initialize beta_init with zeros
#' LRMultiClass_R <- LRMultiClass(X, y, beta_init, numIter = 10, eta = 0.1, lambda = 1)  # Run the R implementation
#' beta_R <- LRMultiClass_R$beta    #Output beta
#' objective_R <- LRMultiClass_R$objective    #Output objectives

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
  out = LRMultiClass_c(X, y, beta_init, numIter, eta, lambda)
  
  # Return the class assignments
  return(out)
}
