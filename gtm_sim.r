#leaning heavily on https://github.com/SAFEtoolbox/SAFE-R/blob/main/R/hymod_sim.R
#' gtm groundwater trophic model
#'
#' This function simulates the groundwater trophic model
#'
#' BOC_sim = time series of simulated BOC ; was flow_sim, but flow_sim is not mentioned after this ?! what was meant here is probably Qsim
#' This function requires that the time series of temperature #was rainfall (\code{rain})
#' was #and potential evaporation (\code{evap}) 
#' be defined as global variables
#'
#' @param temperature #was rain vector \code{(T)} time series of rainfall
#' was #@param evap vector \code{(T)} time series of potential evaporation
#' @param param 5 elements vector of model parameters \code{(yield_ac, K_ac, factor_CC_MO, rMO_COD_uptake_per_day_at_lab_temperature, microbe_loss_factor_when_no_fauna)}
#TODO but how to derive that from the X which is the whole parameter space? that is done in apply(..., 1, ...)
#' @param group 1 element vector : 4 groups in Fulda floodplain which vary largely
#' 
#' TODO was @references  Boyle, D. (2001). Multicriteria calibration of hydrological models. PhD thesis, Dep. of Hydrol. and Water Resour., Univ. of Ariz., Tucson.
#'
#' TODO Wagener, T., Boyle, D., Lees, M., Wheater, H., Gupta, H., and Sorooshian, S. (2001). A framework for development and application of hydrological models. Hydrol. Earth Syst. Sci., 5, 13-26.
#' 
#' @export

gtm_sim <- function(temperature, param){ #was rain evap

#TODO temperature <- results_g$TT_TER
#TODO param <- X[1,]

## --------------------------
## Recover model parameters:
## --------------------------
#Sm <- max(.Machine$double.eps, param[1]) # Maximum Soil Moisture (cannot be zero!)
#beta <- param[2] # Exponential parameter in soil routine [-]
#alfa <- param[3] # Partitioning factor [-]
#Rs   <- param[4] # Slow reservoir outflow coefficient (ratio) [1/Dt]  
#Rf   <- param[5] # Fast reservoir outflow coefficient (ratio) [1/Dt] 

yield_ac <- param[1]    #yield of microbes feeding on acetate at lab temperature - taken as a starting point for microbes feeding on BOC
K_ac <- param[2] #K half saturation concentration of microbes feeding on acetate  at lab temperature , see above
factor_CC_MO <- param[3] # factor with which maximum measured biomass is multiplied to derive capacity 
rMO_COD_uptake_per_day_at_lab_temperature <- param[4] #growth rate of micrbes
microbe_loss_factor_when_no_fauna <- param[5] # maintenance energy and other losses which hinder the microbes from growing 


N_step <- length(temperature) # number of time steps in the simulation horizon

## -----------------------
## Initialize variables:
## ---------------------- 
# Pe  <- numeric(N_step) # Recharge from the soil [mm/Dt]
# Ea  <- numeric(N_step) # Actual Evapotranspiration [mm/Dt]
# sm <- numeric(N_step + 1) # Soil Moisture [mm]
# sL <- numeric(N_step + 1) # Slow reservoir moisture [mm]
# sF1 <- numeric(N_step + 1) # Fast reservoir 1 moisture [mm]
# sF2 <- numeric(N_step + 1) # Fast reservoir 2 moisture [mm]
# sF3 <- numeric(N_step + 1) # Fast reservoir 3 moisture [mm]

# QsL <- numeric(N_step)  # Slow flow [mm/Dt]
# QsF <- numeric(N_step)  # Fast flow [mm/Dt]


#TODO results_g contained these already .. does it make sense to use existing results_g and not the variables here
DETRITUS <- numeric(N_step) #mol COD / L

BOC_sim <- numeric(N_step) #mol COD / L

#attribute the first group-wise value
#eval(paste0("BOC_gr", g, "_t0"))
BOC_gr_x_t0 <- eval(sym(paste0("BOC_gr", g, "_t0")) )# sym takes string and turns into symbol. eval evaluates an R expression and returns computed value
BOC_sim[1] <- BOC_gr_x_t0

MO_het <- numeric(N_step) #mol COD / L
#attribute the first group-wise value
MO_het_gr_x_t0 <- eval(sym(paste0("MO_het_gr", g, "_t0")) )# sym takes string and turns into symbol. eval evaluates an R expression and returns computed value
MO_het[1] <- MO_het_gr4_t0

FAUNA <- numeric(N_step) #mol COD / L
#attribute the first group-wise value
fauna_gr_x_t0 <- eval(sym(paste0("fauna_gr", g, "_t0")) )# sym takes string and turns into symbol. eval evaluates an R expression and returns computed value
FAUNA[1] <- fauna_gr_x_t0

growthrate <- numeric(N_step)
BOC_import_from_detritus <- numeric(N_step)
RECHARGE_COD <- numeric(N_step) # _mol_per_m3_per_day

N_step_test <- N_step-1000
# simulation:
#TODO dont use t because that is also a function name - use ti instead
for (ti in 2:(N_step_test)){ #TODO in hyd_mod this starts at 1, but I give the values for 1 and need them for further calculations - in hyd_mod there is t+1, I do t-1
#TODO for some reason the last step fails and does not find Fauna_ti_
#ti <- 2	
# 	# --------------------------
# 	#   Soil Moisture Dynamics:
# 	# --------------------------

# 	FF  <- 1 - ( 1 - sm[t] / Sm )^beta
# 	Pe[t] <- FF * rain[t] # Compute the value of the outflow 
# 	# (we assumed that this process is faster than evaporation)
	
# 	sm_temp  = max(min(sm[t] + rain[t] - Pe[t], Sm), 0) # Compute the water 
#     # balance with the value of the outflow 
#     Pe[t] = Pe[t] + max(sm[t] + rain[t] - Pe[t] - Sm, 0) + min(sm[t] + rain[t] - Pe[t], 0)
#     # adjust Pe by an amount equal to the possible negative sm amount or 
#     # to the possible sm amount above Sm.
    
#     W = min(abs(sm[t] / Sm ),1) # Correction factor for evaporation
#     Ea[t] = W * evap[t] # Compute the evaporation
#     sm[t+1] = max(min(sm_temp - Ea[t], Sm), 0) # Compute the water balance 
#     Ea[t]= Ea[t] + max(sm_temp - Ea[t] - Sm, 0) + min(sm_temp - Ea[t], 0) # adjust Ea 
#     # by an amount equal to the possible negative sm amount or to the 
#     # possible sm amount above Sm 
#    # -------------------------
#   #   Groundwater Dynamics:
#   # -------------------------
   
#     # slow flow
#     QsL[t] <- Rs * sL[t]
# 	sL[t+1] <- sL[t] + (1 - alfa) * Pe[t] - QsL[t]
# 	# fast flow
# 	sF1[t+1] <- sF1[t] +  alfa * Pe[t] - Rf * sF1[t]
# 	sF2[t+1] <- sF2[t] +  Rf * sF1[t] - Rf * sF2[t]
# 	QsF[t]  <- Rf * sF3[t]
# 	sF3[t+1] <- sF3[t] +  Rf * sF2[t] - QsF[t]
	

       # -------------------------
       #groundwater temperature
       # -------------------------
    #GWTEMP_ti <- Fulda_daily_temp_joh_long_g$TT_TER [Fulda_daily_temp_joh_long_g$dateRi == results$dateRi[i]]
    GWTEMP_ti <- results_g$TT_TER [ti] #TODO is that safe?  warm up period is only deleted later, so it should e safe
    GWTEMP_ti <- results_g$TT_TER [results_g$dateRi == uniquedatevector[ti] ] #
    
       # -------------------------
       #recharge of carbon into groundwater from precipitation carbon
       # -------------------------
    RECHARGE_COD_mol_per_m3_per_day_df_ti <- RECHARGE_COD_mol_per_m3_per_day_df$RECHARGE_COD_mol_per_m3_per_day [RECHARGE_COD_mol_per_m3_per_day_df$dateRi == uniquedatevector[ti]]
    
    RECHARGE_COD_mol_per_L_per_day_df_ti <- RECHARGE_COD_mol_per_m3_per_day_df_ti /1000
    
    # -------------------------
    #processes based on values from the previous step
    # -------------------------
    #take the respective field of the  Date == Date[i] aund group == group[g] 
    DETRITUS_ti_minus_1 <- results_g$DETRITUS[results_g$dateRi == uniquedatevector[ti-1] ]     
    BOC_ti_minus_1 <- results_g$BOC[results_g$dateRi == uniquedatevector[ti-1] ]
    
    MO_het_ti_minus_1 <- results_g$MO_het[results_g$dateRi == uniquedatevector[ti-1] ] 
    
    Fauna_ti_minus_1 <- results_g$fauna[results_g$dateRi == uniquedatevector[ti-1] ] 
    
    
    BOC_import_from_detritus_ti <- dBOC_from_detritus_dt(k1, DETRITUS_ti_minus_1) #mol COD / L
    
    
    if (scenario_with_1_or_without_0_fauna == 1) {
      
      Excretion <- dExcretion_dt(excretionRate, Fauna_ti_minus_1 )
      
      Mortality <- dMortality_dt(mortalityRate, mortalityFraction_per_degree, Fauna_ti_minus_1, GWTEMP_ti  ) 
      
      #f_S_fauna is calculated within dMO_fauna_uptake_dt in the after next line, however, is also needed for calculating MO_het - thus, not redundant here
      f_S_fauna <- dMO_fauna_degradation_factor_dt(MO_het_ti_minus_1,   rFauna_MO_uptake_per_day_at_TEMP , K_MO_at_temp, Fauna_ti_minus_1, delta_t, growth_model_fauna_type, CC_group_fauna_g)
      
      Fauna_ti_list <- dMO_fauna_uptake_dt(MO_het_ti_minus_1,   rFauna_MO_uptake_per_day_at_TEMP , K_MO_at_temp, Fauna_ti_minus_1, delta_t, growth_model_fauna_type, CC_group_fauna_g, yield_MO, Excretion , Mortality)
      
      Fauna_growth <- Fauna_ti_list[[1]]
      Fauna_ti <- Fauna_ti_list[[2]]
      

    }else{
      f_S_fauna <- 0
      
      Fauna_ti <- 0
      Fauna_growth <- 0
      Excretion <- 0
      Mortality <- 0
    }
    
    
    rMO_BOC_uptake_per_day_at_TEMP <- d_BOC_MO_het_uptake_dt_per_day_per_temperature( rMO_BOC_uptake_per_day_at_lab_temperature,  GWTEMP_ti, lab_temp) 
    
    K_ac_at_TEMP <- d_K_ac_per_temperature(K_ac, lab_temp, GWTEMP_ti) 
    
    #how much COD = BOC degraded 
    f_S_MO <- dBOC_degradation_factor_dt(BOC_ti_minus_1,   rMO_BOC_uptake_per_day_at_TEMP, K_ac_at_TEMP, MO_het_ti_minus_1, delta_t, growth_model_MO_type, CC_group_MO_g)
    
    MO_het_ti_list  <- dBOC_MO_degradation_dt(BOC_ti_minus_1,   rMO_BOC_uptake_per_day_at_TEMP,  K_ac_at_TEMP, MO_het_ti_minus_1, yield_ac, f_S_fauna, delta_t, growth_model_MO_type, CC_group_MO_g) 
    MO_growth <- MO_het_ti_list[[1]]
    MO_het_ti <- MO_het_ti_list[[2]]
    
    BOC_ti_interim <- dBOC_stock_dt(BOC_ti_minus_1,  f_S_MO, BOC_import_from_detritus_ti, Excretion) 
    BOC_ti            <- max(0, BOC_ti_interim)      # to avoid errors when COD = BOC becomes slightly negative.. From Soetaert (2008)
    
    
    #since this happens in this time step, the new Detritus is not used for further reactions in this time step - the detritus from the time step before is used, because that is what the organisms perceive
    DETRITUS_ti <- dDETRITUS_dt( k1, DETRITUS_ti_minus_1, RECHARGE_COD_mol_per_L_per_day_df_ti, Mortality ) 
    

#TODO design - do I write into a complete results, or into a results_g ? I need results_g for the error measures
    #TODO for the time being, some redundancy, to comply with the Hyd_mode structure
    results_g$MO_het[results_g$dateRi == uniquedatevector[ti] ] <- MO_het_ti
    MO_het[ti] <- MO_het_ti
    results_g$fauna[results_g$dateRi == uniquedatevector[ti] ] <- Fauna_ti
    FAUNA[ti] <- Fauna_ti    
    results_g$BOC[results_g$dateRi == uniquedatevector[ti] ] <- BOC_ti
    BOC_sim[ti] <- BOC_ti
    results_g$growthrate[results_g$dateRi == uniquedatevector[ti] ] <- rMO_BOC_uptake_per_day_at_TEMP
    growthrate[ti] <- rMO_BOC_uptake_per_day_at_TEMP
    results_g$DETRITUS[results_g$dateRi == uniquedatevector[ti] ] <- DETRITUS_ti 
    DETRITUS[ti] <- DETRITUS_ti
    results_g$import_from_detritus[results_g$dateRi == uniquedatevector[ti] ] <- BOC_import_from_detritus_ti 
    BOC_import_from_detritus[ti] <- BOC_import_from_detritus_ti
    RECHARGE_COD[ti] <-  RECHARGE_COD_mol_per_m3_per_day_df_ti
}#end time
#return(results_g)


#Qsim <- QsL + QsF

#STATES <- list(sm = sm,sL = sL, sF1 = sF1, sF2 = sF2, sf3 = sF3)
#FLUXES <- list(Pe = Pe, Ea = Ea, QsL = QsL, QsF = QsF)

STATES <- list(TT_TER = results_g$TT_TER,Prec=results_g$value, DETRITUS = DETRITUS, BOC_sim = BOC_sim, MO_het = MO_het, FAUNA = FAUNA)
FLUXES <- list(BOC_import_from_detritus = BOC_import_from_detritus, growthrate = growthrate, RECHARGE_COD = RECHARGE_COD_mol_per_m3_per_day_df$RECHARGE_COD_mol_per_m3_per_day)

attributes(BOC_sim) <- list(STATES = STATES, FLUXES = FLUXES)

return(BOC_sim)
}