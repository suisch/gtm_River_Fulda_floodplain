# this code relies heavily on https://github.com/SAFEtoolbox/SAFE-R/blob/main/
#' Model execution
#'
#' This function executes the model
#'
#' @param fun_test name of the function implementing the model (string)
#' @param X matrix \code{(N, M)} of \code{N} sampled input factors
#' @param ... other parameters to be passed in \code{fun_test}
#' 
#' @return \code{Y} matrix \code{(N, P)} of associated model ouputs (\code{P} being the number of scalar model outputs associated to each sampled input combination), \code{comp_time} total computing time for model execution (sec)  
#' 
#' @export

model_execution_gtm <- function(fun_test, X, ...){# X is the whole parameter space, from the range.
  
  ###############
  # Check inputs
  ###############
  
  stopifnot(is.character(fun_test),
            is.matrix(X))
  
  ####################
  # Model execution
  ####################
  comp_time <- system.time(Y <- t(apply(X, 1, fun_test, ...))) 
  # apply takes row for row (second argument = 1) of the X parameter combinations, and applies function fun_test
  
  Y <- matrix(Y, nrow = nrow(X))
  
  attributes(Y) <- list(dim = dim(Y),  comp_time = comp_time)
  
  rownames(Y) <- rownames(X)
  
  return(Y)
}