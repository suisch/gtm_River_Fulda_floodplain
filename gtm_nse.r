#heavily leaning on https://github.com/SAFEtoolbox/SAFE-R/blob/main/R/hymod_nse.R

#' gtm NSE

#' This function runs the groundwater trophic gtm model and returns returns the associated Nash-Sutcliffe Efficiency

#' @param x vector \code{5} of model parameters \code{(sis  microbial parameters - row for row in parameter space X, the row for row is done by apply(..,1,..)  )}
#' @param dat dataset containing sis \code{temperature} vector \code{T} time series of extrapolated temperature, and modelled BOC (biologically oxidizable carbon).  

#'@return \code{y}  Nash-Sutcliffe Efficiency 
#sis nse only available for a few dates, not continuously

#' @export

gtm_nse <- function(x, dat){

#TODO at first place x, then dat - careful, which is which? dat would be results_g
#dat = results_g
#Qsim <- gtm_sim(dat$rain, dat$evaporation, x)
#param <- X[1]
#x <- X[1]
BOC_sim <- gtm_sim(dat$TT_TER, x)
#BOC_sim <- gtm_sim(results_g$TT_TER, param)

warmup <- 30 #  warmup period to be discarded
#Qs <- Qsim[-(1:warmup)]
#
#Qo <- dat$flow[-(1:warmup)]
BOCs <- BOC_sim[-(1:warmup)]

BOCo <- dat$BOC_mol_COD_L[-(1:warmup)] #measured values, but they are not on the same date scale, I should do a join first

# BOCo exists only for a fraction of the dates - but the functions work without filtering to nonNA

y <- 1 - var(BOCs - BOCo, na.rm = TRUE) / var(BOCo, na.rm = TRUE)


#return(Y_)
return(y)

}