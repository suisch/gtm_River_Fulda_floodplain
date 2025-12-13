#heavily leaning on https://github.com/SAFEtoolbox/SAFE-R/blob/main/R/hymod_MulOut.R

#' gtm RMSE and BIAS
#'
#' This function runs the groundwater trophic gtm model and returns 
#' 
#' @param x vector \code{ } of model parameters \code{}
#' @param dat dataset containing a \code{temperature} vector \code{T} time series of extrapolated temperature, and modelled BOC (biologically oxidizable carbon).  


#' @export

gtm_MulOut <- function(x, dat){
  # at first place x, then dat -  dat would be results_g and X the parameters
  
  BOC_sim <- gtm_sim(dat$gw_temp, x)
  
  warmup <- 30 #  warmup period to be discarded
  BOCs <- BOC_sim[-(1:warmup)] #simulated values
  
  BOCo <- dat$BOC_mol_COD_L[-(1:warmup)] #measured, observed values, but they are not on the same date scale, I should do a join first
  
  Y <- numeric(6)
  
  # BOCo exists only for a fraction of the dates - but the functions work without filtering to nonNA
  Y[1] <- sqrt(mean((BOCs - BOCo)^2, na.rm = TRUE)) # RMSE
  Y[2] <- abs(mean(BOCs - BOCo, na.rm = TRUE)) # BIAS
  Y[3] <- mean(BOCs) # MEAN
  Y[4] <- sd(BOCs) # STANDARD DEVIATION
  Y[5] <- var(BOCs) # VARIANCE
  Y[6] <- max(BOCs) # MAX
  
  return(Y)
  
}