# function which returns ca. 30 variables 
read_parameters<- function(run){
  
  delta_t          = parameters$dt[run] #usually days
  
  max_t          = as.Date(parameters$max_t[run], origin = "1899-12-30") #date - depends on operating system!!
  
  #depth of gw aquifer
  aquifer_depth <- parameters$aquifer_depth[run] #m
  
  #dummy; for further developments
  import_MO_het <- parameters$import_MO_het[run] #
  
  scenario_with_1_or_without_0_MO<- parameters$scenario_with_1_or_without_0_MO[run]
  
  scenario_with_1_or_without_0_fauna <- parameters$scenario_with_1_or_without_0_fauna[run]  #1 for with fauna, or 0 for without fauna 
  
  k_temp <- parameters$k_temp[run]   #k_temp <- 30 # temperature in Gerritse et al. (1992)
  
  K_ac <- parameters$K_ac[run]
  
  K_MO_at_temp <- parameters$K_MO_at_temp[run]
  
    TOC_COD_mol_m3_precipitation <- parameters$TOC_COD_mol_m3_precipitation[run]
  
  ##########
  #fractions
  ##########
  #fraction of DETRITUS  becoming  BOC -  assumption: independent of temperature
  k1 <- parameters$k1[run]
  
  recharge_fraction_of_precipitation <- parameters$recharge_fraction_of_precipitation[run]
  
  # taking the most competitive acetate degrader as of Schmidt et al. 2018, assuming that it can also break down humic acids competitively with similar parameters, see SI
  #maximal specific growth rate 0.43 h-1, half- saturation constant of 4.3 microM [19]
  yield_ac <- parameters$yield_ac[run] # gdrymass_MO  gacetate^-1 Gerritse et al. (1992)
  
  yield_MO <- parameters$yield_MO[run] 
  
  ##########
  #rates
  ##########
  #microbe maximum growth rate 	Rate constant for bacterial uptake and oxidation of acetate	Gerritse et al. (1992), ASSUMING COD = BOC CAN BE DEGRADED LIKE ACETATE, bold assumption !!
  # per day # #micro ac 	Rate constant for bacterial uptake and oxidation of acetate	4.7E-05	s-1	Gerritse et al. (1992) ****
  #that's at 30 degrees;  in Schmidt et al. (2018): Comamonas testosterone  specific growth rate back calculated to  10degC (4.7 * 10-5 s-1)
  rMO_BOC_uptake_per_day_at_lab_temperature <- parameters$rMO_COD_uptake_per_day_at_lab_temperature[run]
  
  #groundwater fauna does not survive at 30 degrees - see paper and SI. Therefore, take the maximum growth rate rate given and assume that it is valid within the narrow band of temperatures in gw
  rFauna_MO_uptake_per_day_at_TEMP <- parameters$rFauna_MO_uptake_per_day_at_TEMP[run]
  
  mortalityRate <- parameters$mortalityRate[run]  # 
  
  import_fauna <- parameters$import_fauna[run] 
  
  #excretionRate = 0.01  # /day # in Soetaert & Herman 2008 
  excretionRate <- parameters$excretionRate[run]
  
  average_precipitation_mm_yr <- parameters$average_precipitation_mm_yr[run]
  average_precipitation_mm_yr <- as.numeric(average_precipitation_mm_yr)
  
  average_precipitation_m_yr<- average_precipitation_mm_yr/1000
  
  #DETRITUS recharge in dependence on precipitation
  #  x times as much detritus mobilized within soil, as TOC enters from preciptation
  factor_how_many_times_Detritus_compared_to_TOC <- parameters$factor_how_many_times_Detritus_compared_to_TOC[run]
  
  Detritus_COD_mol_m3_precipitation = parameters$Detritus_COD_mol_m3_precipitation[run]
  
  RECHARGE_COD_mol_per_m2_per_day        = ifelse(Fulda_daily_prec$RS >0, (TOC_COD_mol_m3_precipitation + Detritus_COD_mol_m3_precipitation)/365 * Fulda_daily_prec$RS/1000 *recharge_fraction_of_precipitation, 0) #  mol per m2
  
  RECHARGE_COD_mol_per_m3_per_day <- RECHARGE_COD_mol_per_m2_per_day / aquifer_depth 
  
  RECHARGE_COD_mol_per_m3_per_day_df <- data.frame(cbind("dateRi" = as.Date(Fulda_daily_prec$dateRi), RECHARGE_COD_mol_per_m3_per_day))
  
  RECHARGE_COD_mol_per_m3_per_day_df$dateRi <-Fulda_daily_prec$dateRi
  
  growth_model_MO_type <- parameters$growth_model_MO[run]
  growth_model_fauna_type <- parameters$growth_model_fauna[run]
  
  mortalityFraction_per_degree <- parameters$mortalityFraction_per_degree[run]
  
  microbe_loss_factor_when_no_fauna <- parameters$microbe_loss_factor_when_no_fauna[run]
  
  return(list(delta_t, max_t, aquifer_depth, import_MO_het, scenario_with_1_or_without_0_fauna, scenario_with_1_or_without_0_MO, mortalityRate, import_fauna, yield_ac, yield_MO, K_MO_at_temp, rMO_BOC_uptake_per_day_at_lab_temperature, rFauna_MO_uptake_per_day_at_TEMP, k1, excretionRate,  RECHARGE_COD_mol_per_m3_per_day_df, k_temp, K_ac, growth_model_MO_type, growth_model_fauna_type, mortalityFraction_per_degree, microbe_loss_factor_when_no_fauna))
}

