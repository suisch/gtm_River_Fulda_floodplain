#heavily leaning on https://github.com/SAFEtoolbox/SAFE-R/blob/main/R/hymod_MulObj.R

#' gtm RMSE and BIAS
#'
#' This function runs the groundwater trophic gtm model and returns 2 metrics of model performance: RMSE and BIAS

#' @param x vector  of model parameters \code{(row for row in parameter space X, the row for row is done by apply(..,1,..)  )}
#' @param dat dataset containing {temperature} vector  time series of extrapolated temperature
#' 
#'@return \code{Y}  vector \code{(2)} of objective functions \code{(RMSE, BIAS)}

#' @seealso \code{\link{hymod_sim}}

#' @export

gtm_MulObj <- function(x, dat){
  # at first place x, i.e. parameter space, then dat , e.g. results_g
  
  BOC_sim <- gtm_sim(dat$gw_temp, x)
  
  warmup <- 30 #  warmup period to be discarded
  
  
  BOCs <- BOC_sim[-(1:warmup)]
  
  BOCo <- dat$BOC_mol_COD_L[-(1:warmup)] #measured values after the warmup period
  
  Y_ <- numeric(2)
  
  
  # BOCo exists only for a fraction of the dates - but the functions work without filtering to nonNA
  Y_[1] <- sqrt(mean((BOCs - BOCo)^2, na.rm =TRUE)) # RMSE
  Y_[2] <- abs(mean(BOCs - BOCo, na.rm =TRUE)) # BIAS
  
  return(Y_)
  
}