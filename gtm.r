#function in gtm v 2.0.0 for running the time step wise groundwater trophic model, based on groundwater_trophic_model from v. 1.0.0 
gtm <- function() {
for (g in 1:length(unique(results$group))){
  CC_group_MO_g <- CC_table_MO$CC[CC_table_MO$group == uniquegroupvector[g]]
  CC_group_fauna_g <- CC_table_fauna$CC[CC_table_fauna$group == uniquegroupvector[g]]
  group_letter_g <- unique(results$group_letter[results$group == uniquegroupvector[g]])
  
  Fulda_daily_temp_joh_long_g <- Fulda_daily_temp_joh_long %>%
    dplyr::filter(group_letter == group_letter_g)
  results_g <- results %>%
    dplyr::filter(group_letter == group_letter_g)
  
  for (i in 2:(length(uniquedatevector))) {
    
    #groundwater temp.
    #GWTEMP_ti <- Fulda_daily_temp_joh_long_g$TT_TER [Fulda_daily_temp_joh_long_g$dateRi == results$dateRi[i]]
    GWTEMP_ti <- results_g$TT_TER [results_g$dateRi == results$dateRi[i]]
    
    RECHARGE_COD_mol_per_m3_per_day_df_ti <- RECHARGE_COD_mol_per_m3_per_day_df$RECHARGE_COD_mol_per_m3_per_day [RECHARGE_COD_mol_per_m3_per_day_df$dateRi == uniquedatevector[i]]
    
    RECHARGE_COD_mol_per_L_per_day_df_ti <- RECHARGE_COD_mol_per_m3_per_day_df_ti /1000
    
    #take the respective field of the  Date == Date[i] aund group == group[g] 
    DETRITUS_ti_minus_1 <- results$DETRITUS[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]]     
    BOC_ti_minus_1 <- results$BOC[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]]
    
    MO_het_ti_minus_1 <- results$MO_het[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]] 
    
    Fauna_ti_minus_1 <- results$fauna[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]] 
    
    
    BOC_import_from_detritus_ti <- dBOC_from_detritus_dt(k1, DETRITUS_ti_minus_1) #mol COD / L
    
    
    if (scenario_with_1_or_without_0_fauna == 1) {
      
      Excretion <- dExcretion_dt(excretionRate, Fauna_ti_minus_1 )
      
      Mortality <- dMortality_dt(mortalityRate, mortalityFraction_per_degree, Fauna_ti_minus_1, GWTEMP_ti  ) 
      
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
    
    
    #since this happens in this time step, the new Detritus is not used for further reactions in this time step - the detritus from the time step before is used
    DETRITUS_ti <- dDETRITUS_dt( k1, DETRITUS_ti_minus_1, RECHARGE_COD_mol_per_L_per_day_df_ti, Mortality ) 
    
    results$MO_het[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- MO_het_ti
    
    results$fauna[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- Fauna_ti
    
    results$BOC[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- BOC_ti
    
    results$growthrate[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- rMO_BOC_uptake_per_day_at_TEMP
    
    results$DETRITUS[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- DETRITUS_ti 
    
    results$import_from_detritus[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- BOC_import_from_detritus_ti 

  } #end groups
}#end time
return(results)
}