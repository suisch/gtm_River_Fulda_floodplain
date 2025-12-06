#heavily leaning on https://github.com/SAFEtoolbox/SAFE-R/blob/main/R/hymod_MulOut.R

#' gtm RMSE and BIAS
#'
#' This function runs the groundwater trophic gtm model and returns 
#' 
#' @param x vector \code{5} of model parameters \code{(sis so far 5 microbial parameters - row for row in parameter space X, the row for row is done by apply(..,1,..)  )}
#' @param dat dataset containing sis \code{temperature} vector \code{T} time series of extrapolated temperature, and modelled BOC (biologically oxidizable carbon).  


#' @export

gtm_MulOut <- function(x, dat){

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

#Y_ <- numeric(2)
Y <- numeric(6)

#Y[1] <- sqrt(mean((Qs - Qo)^2)) # RMSE
#Y[2] <- abs(mean(Qs - Qo)) # BIAS

# BOCo exists only for a fraction of the dates - but the functions work without filtering to nonNA
#Y_[1] <- sqrt(mean((BOCs - BOCo)^2, na.rm =TRUE)) # RMSE
#Y_[2] <- abs(mean(BOCs - BOCo, na.rm =TRUE)) # BIAS
Y[1] <- sqrt(mean((BOCs - BOCo)^2, na.rm = TRUE)) # RMSE
Y[2] <- abs(mean(BOCs - BOCo, na.rm = TRUE)) # BIAS
Y[3] <- mean(BOCs) # MEAN
Y[4] <- sd(BOCs) # STANDARD DEVIATION
Y[5] <- var(BOCs) # VARIANCE
Y[6] <- max(BOCs) # MAX

#return(Y_)
return(Y)

}