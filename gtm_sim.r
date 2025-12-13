#leaning heavily on https://github.com/SAFEtoolbox/SAFE-R/blob/main/R/hymod_sim.R
#' gtm groundwater trophic model
#function in gtm v 2.0.0 for running the time step wise groundwater trophic model, based on groundwater_trophic_model from v. 1.0.0 
#'
#'
#' BOC_sim = time series of simulated BOC 
#' This function requires that the time series of temperature 
#' be defined as global variable
#'
#' @param temperature 
#' @param param  elements vector of model parameters 
#' 
#' 
#' @export

gtm_sim <- function(temperature, param){ 
  
  # taking the most competitive acetate degrader as of Schmidt et al. 2018
  #maximal specific growth rate 0.43 h-1, half- saturation constant of 4.3 microM [19]
  # gdrymass_MO  gacetate^-1 Gerritse et al. (1992)
  yield_ac <- param[1]    #yield of microbes feeding on acetate at lab temperature - taken as a starting point for microbes feeding on BOC
  K_ac <- param[2] #K half saturation concentration of microbes feeding on acetate  at lab temperature , see above
  factor_CC_MO <- param[3] # factor with which maximum measured biomass is multiplied to derive capacity 
  
  #microbe maximum growth rate 	Rate constant for bacterial uptake and oxidation of acetate	Gerritse et al. (1992), ASSUMING COD = BOC CAN BE DEGRADED LIKE ACETATE, which is wrong !!
  rMO_BOC_uptake_per_day_at_lab_temperature <- param[4] #growth rate of micrbes
  # per day # #micro ac 	Rate constant for bacterial uptake and oxidation of acetate	4.7E-05	s-1	Gerritse et al. (1992) ****
  #that's at 30 degrees;  in Schmidt et al. (2018): Comamonas testosterone  specific growth rate back calculated to  10degC (4.7 * 10-5 s-1)
  
  
  #here, a few things need to be done again which were already done in the v.1.0.0 gtm in preparation files
  
  average_precipitation_mm_yr <- param[5]
  average_precipitation_mm_yr <- as.numeric(average_precipitation_mm_yr)
  average_precipitation_m_yr<- average_precipitation_mm_yr/1000
  
  recharge_fraction_of_precipitation <- param[6]
  
  TOC_COD_mol_m3_precipitation <-    param[7]
  
  mortalityRate <- param[15]  # 
  
  yield_MO <- param[12] 
  
  
  K_MO_at_temp <- param[11] 
  
  #groundwater fauna does not survive at 30 degrees - see paper and SI. Therefore, take the maximum growth rate rate given and assume that it is valid within the narrow band of temperatures in gw
  rFauna_MO_uptake_per_day_at_TEMP <- param[13] 
  
  mortalityFraction_per_degree <- param[17]
  
  aquifer_depth <- param[10]
  
  factor_CC_fauna <- param[16] #
  
  ##########
  #fractions / factors
  ##########
  #fraction of DETRITUS  becoming  BOC -  assumption: independent of temperature
  
  k1 <-  param[9]
  
  #DETRITUS recharge in dependence on precipitation
  #  x times as much detritus mobilized as TOC from preciptation
  factor_how_many_times_Detritus_compared_to_TOC <- param[8]
  
  ##########
  #rates
  ##########
  
  #excretionRate = 0.01  # /day # from Soetaert & Herman 2008 
  excretionRate <-  param[14]
  
  
  
  ##########
  #calculating variables based on parameters
  ##########
  
  Detritus_COD_mol_m3_precipitation = factor_how_many_times_Detritus_compared_to_TOC * TOC_COD_mol_m3_precipitation
  
  RECHARGE_COD_mol_per_m2_per_day        = ifelse(Fulda_daily_prec$RS >0, (TOC_COD_mol_m3_precipitation + Detritus_COD_mol_m3_precipitation)/365 * Fulda_daily_prec$RS/1000 *recharge_fraction_of_precipitation, 0) #  mol per m2
  
  RECHARGE_COD_mol_per_m3_per_day <- RECHARGE_COD_mol_per_m2_per_day / aquifer_depth 
  
  RECHARGE_COD_mol_per_m3_per_day_df <- data.frame(cbind("dateRi" = as.Date(Fulda_daily_prec$dateRi), RECHARGE_COD_mol_per_m3_per_day))
  
  RECHARGE_COD_mol_per_m3_per_day_df$dateRi <-Fulda_daily_prec$dateRi
  
  
  fulda_variables_read_in <- fulda_variables(run, factor_CC_MO, factor_CC_fauna)
  names(fulda_variables_read_in) <- c("Fulda_daily_prec", "Fulda_daily_temp_", "chem_ordered_per_date_1978_1981", "chem_ordered_per_date_1978_1981_mean_per_group", "fauna_deep_PerSamplPerTaxonWide_bm_sum", "fauna_deep_PerSamplPerTaxon_bm_mean_per_group", "t_0", "t_max", "DETRITUS_gr1_t0", "DETRITUS_gr2_t0", "DETRITUS_gr3_t0", "DETRITUS_gr4_t0", "BOC_gr1_t0", "BOC_gr2_t0", "BOC_gr3_t0", "BOC_gr4_t0", "MO_het_gr1_t0", "MO_het_gr2_t0", "MO_het_gr3_t0", "MO_het_gr4_t0", "fauna_gr1_t0", "fauna_gr2_t0", "fauna_gr3_t0", "fauna_gr4_t0",  "CC_table_MO", "CC_table_fauna", "Fulda_daily_temp_joh_long") 
  
  list2env(fulda_variables_read_in, globalenv())
  
  
  N_step <- length(temperature) # number of time steps in the simulation horizon
  
  ## -----------------------
  ## Initialize variables:
  ## ----------------------- 
  
  
  DETRITUS <- numeric(N_step) #mol COD / L
  
  BOC_sim <- numeric(N_step) #mol COD / L
  
  #attribute the first group-wise value
  BOC_gr_x_t0 <- eval(sym(paste0("BOC_gr", g, "_t0")) )# sym takes string and turns into symbol. eval evaluates an R expression and returns computed value
  BOC_sim[1] <- BOC_gr_x_t0
  
  MO_het <- numeric(N_step) #mol COD / L
  #attribute the first group-wise value
  MO_het_gr_x_t0 <- eval(sym(paste0("MO_het_gr", g, "_t0")) )# sym takes string and turns into symbol. eval evaluates an R expression and returns computed value
  MO_het[1] <- MO_het_gr_x_t0
  
  FAUNA <- numeric(N_step) #mol COD / L
  #attribute the first group-wise value
  fauna_gr_x_t0 <- eval(sym(paste0("fauna_gr", g, "_t0")) )# sym takes string and turns into symbol. eval evaluates an R expression and returns computed value
  FAUNA[1] <- fauna_gr_x_t0
  
  growthrate <- numeric(N_step)
  BOC_import_from_detritus <- numeric(N_step)
  RECHARGE_COD <- numeric(N_step) # _mol_per_m3_per_day
  
  N_step_test <- N_step-1000
  # simulation:
  # dont use t because that is also a function name - use ti instead
  for (ti in 2:(N_step_test)){ #the startign values are given for ti = 1, therefore the model starts at time step 2
    
    # -------------------------
    #groundwater temperature
    # -------------------------
    GWTEMP_ti <- results_g$gw_temp [results_g$dateRi == uniquedatevector[ti] ] #
    
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
  
  BOC_sim = BOC_sim #redundant, but just to show what we use for applying the attributes to
  
  STATES <- list(TT_TER = results_g$TT_TER,Prec=results_g$value, DETRITUS = DETRITUS, MO_het = MO_het, FAUNA = FAUNA)
  FLUXES <- list(BOC_import_from_detritus = BOC_import_from_detritus, growthrate = growthrate, RECHARGE_COD = RECHARGE_COD_mol_per_m3_per_day_df$RECHARGE_COD_mol_per_m3_per_day)
  
  attributes(BOC_sim) <- list(STATES = STATES, FLUXES = FLUXES)
  
  return(BOC_sim)
}
