#heavily leaning on https://github.com/SAFEtoolbox/SAFE-R/blob/main/R/hymod_nse.R

#' gtm NSE

#' This function runs the groundwater trophic gtm model and returns the associated Nash-Sutcliffe Efficiency

#' @param x vector \code{ } of model parameters \code{( parameters - row for row in parameter space X;  row-wise done by apply(..,1,..)  )}
#' @param dat dataset containing  \code{temperature} vector \code{T} time series of extrapolated temperature, and modelled BOC (biologically oxidizable carbon).  

#'@return \code{y}  Nash-Sutcliffe Efficiency 
#sis nse only available for a few dates, not continuously

#' @export

gtm_nse <- function(x, dat){
  # at first place x, then dat - dat would be results_g, and x the parameter space
  
  BOC_sim <- gtm_sim(dat$gw_temp, x)
  
  warmup <- 30 #  warmup period to be discarded
  BOCs <- BOC_sim[-(1:warmup)] #modelled values
  
  BOCo <- dat$BOC_mol_COD_L[-(1:warmup)] #measured values after the warmup period
  
  # BOCo exists only for a fraction of the dates - but the functions work without filtering to nonNA
  
  y <- 1 - var(BOCs - BOCo, na.rm = TRUE) / var(BOCo, na.rm = TRUE)
  
  return(y)
  
}