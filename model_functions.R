##########
#reactions
##########


# physical degradation of detritus to biologically degradable Carbon (BDC), from detritus 
dBOC_from_detritus_dt    <- function(k1, DETRITUS_ti_minus_1 ) {
  BOC_import_from_detritus_ti <- k1*DETRITUS_ti_minus_1   
  return(BOC_import_from_detritus_ti)
}


# detritus at time step is the result of Detritus from the preious time step, plus detritus recharged from precipitation minus what is broken down to BDC, plus Import from freshly dead biomass (mortality). 
dDETRITUS_dt    = function(k1, DETRITUS_ti_minus_1 ,RECHARGE_BOC_mol_per_L_per_day_df_ti , Mortality) {
  Detritus_broken_down_ti <- dBOC_from_detritus_dt(k1, DETRITUS_ti_minus_1)
 DETRITUS_ti <- DETRITUS_ti_minus_1 - Detritus_broken_down_ti + RECHARGE_BOC_mol_per_L_per_day_df_ti + Mortality 
  return(DETRITUS_ti)
}

# rMO_BOC_uptake_per_day is temperature dependent, and needs to be rescaled depending on what the temperature was at which the rate was determined originally
d_BOC_MO_het_uptake_dt_per_day_per_temperature    = function(rMO_BOC_uptake_per_day_at_lab_temperature, GWTEMP_ti, lab_temp) {
  # change the rate by the factor by which it deviates from the rate measured at lab temperature 
    TEMP_related_to_lab_temperature = lab_temp - GWTEMP_ti
  rMO_BOC_uptake_per_day_at_TEMP <- rMO_BOC_uptake_per_day_at_lab_temperature  / (TEMP_related_to_lab_temperature * 0.9696) #0.9696  is the factor in Schmidt et al. 2018 to calculate respective temperature influence in 1 degree steps - assuming boldly that such relationship is directly transferable
  
  return(rMO_BOC_uptake_per_day_at_TEMP)   }


# make K_ac temperature dependent
d_K_ac_per_temperature    = function(K_ac, lab_temp, GWTEMP_ti) {
  
  # change the rate by the factor by which it deviates from the rate measured at lab temperature 
    TEMP_related_to_lab_temperature = lab_temp-GWTEMP_ti
  K_ac_at_TEMP <- K_ac  / (TEMP_related_to_lab_temperature * 0.9696) #0.9696  is the factor in Schmidt et al. 2018 to calcualte respective temperature in 1 degree steps
  
  return(K_ac_at_TEMP)
}



dBOC_degradation_factor_dt    = function(BOC_ti_minus_1,   rMO_BOC_uptake_per_day_at_TEMP,  K_ac_at_TEMP, MO_het_ti_minus_1, delta_t, growth_model_MO_type, CC_group_MO_g) {
  
 
  if (growth_model_MO_type == "MM") { #Michaelis Menten / Monod type
    f_S_MO <- rMO_BOC_uptake_per_day_at_TEMP *  BOC_ti_minus_1 / (BOC_ti_minus_1 + K_ac_at_TEMP)* MO_het_ti_minus_1 *delta_t
    }
  
  if (growth_model_MO_type == "L") { #logistic growth
    f_S_MO <- rMO_BOC_uptake_per_day_at_TEMP *  (CC_group_MO_g - MO_het_ti_minus_1)/CC_group_MO_g *delta_t
  }
  
  if (growth_model_MO_type == "HMML") { # hybrid MM Logistic, acc. to Xu 2019, used in Schlogelhofer, H.L., Peaudecerf, F.J., Bunbury, F., Whitehouse, M.J., Foster, R.A., Smith, A.G., Croze, O.A., 2021. Combining SIMS and mechanistic modelling to reveal nutrient kinetics in an algal-bacterial mutualism. PLOS ONE 16, e0251643. https://doi.org/10.1371/journal.pone.0251643
    f_S_MO <- rMO_BOC_uptake_per_day_at_TEMP * BOC_ti_minus_1 / (BOC_ti_minus_1 + K_ac_at_TEMP)* MO_het_ti_minus_1 *delta_t * (CC_group_MO_g - MO_het_ti_minus_1)/CC_group_MO_g *delta_t
    
  }
  
  return(f_S_MO)
  
}


# combining  with yield and check that MO not negative
dBOC_MO_degradation_dt    = function(BOC_ti_minus_1,   rMO_BOC_uptake_per_day_at_TEMP,  K_ac_at_TEMP, MO_het_ti_minus_1, yield_ac, f_S_fauna, delta_t, growth_model_MO_type, CC_group_MO_g) {
  f_S_MO <- dBOC_degradation_factor_dt(BOC_ti_minus_1,   rMO_BOC_uptake_per_day_at_TEMP,  K_ac_at_TEMP, MO_het_ti_minus_1, delta_t, growth_model_MO_type, CC_group_MO_g)

  MO_growth <- yield_ac*f_S_MO 
  
  MO_het_ti <- MO_het_ti_minus_1 + MO_growth - f_S_fauna

  if(MO_het_ti <= 0){
    MO_het_ti <- .00000000001 #MO_het_ti_minus_1 artificially , MO do NOT vanish - they hide from the grazing presure.  that feeds the fauna artificially. so take a very small value
  }else{
    MO_het_ti <- MO_het_ti   }

  return(list(MO_growth, MO_het_ti )) # 
  
}




dMO_fauna_degradation_factor_dt    = function(MO_het_ti_minus_1,   rFauna_MO_uptake_per_day_at_TEMP,  K_MO_at_temp, Fauna_ti_minus_1, delta_t, growth_model_fauna_type, CC_group_fauna_g) {
  
  if (growth_model_fauna_type == "MM") {
    #  Michaelis Menten / Monod
    f_S_fauna_Petzoldelta_t_2018 <- rFauna_MO_uptake_per_day_at_TEMP *  MO_het_ti_minus_1/(MO_het_ti_minus_1 + K_MO_at_temp) *Fauna_ti_minus_1 *delta_t
  }
  
  if (growth_model_fauna_type == "L") {
    f_S_fauna <- rFauna_MO_uptake_per_day_at_TEMP *  (CC_group_fauna_g - MO_het_ti_minus_1)/CC_group_fauna_g *delta_t 
  }
  
  if (growth_model_fauna_type == "HMML") { # hybrid MM Logistic, acc. to Xu 2019, used in Schlogelhofer, H.L., Peaudecerf, F.J., Bunbury, F., Whitehouse, M.J., Foster, R.A., Smith, A.G., Croze, O.A., 2021. Combining SIMS and mechanistic modelling to reveal nutrient kinetics in an algal-bacterial mutualism. PLOS ONE 16, e0251643. https://doi.org/10.1371/journal.pone.0251643
    f_S_fauna <- rFauna_MO_uptake_per_day_at_TEMP * MO_het_ti_minus_1 / (MO_het_ti_minus_1 + K_MO_at_temp)* Fauna_ti_minus_1 *delta_t * (CC_group_fauna_g - Fauna_ti_minus_1)/CC_group_fauna_g *delta_t
  }
    
  return(f_S_fauna) 
  
}


# growth of fauna

dFauna_dt    = function(f_S_fauna, Fauna_ti_minus_1 , yield_MO, Excretion , Mortality) {
  fauna_growth <- f_S_fauna * yield_MO 
  Fauna_ti <- Fauna_ti_minus_1 + fauna_growth - Excretion - Mortality
  
  return(Fauna_ti, )
}


#like for microbes, make one fct from this. used
dMO_fauna_uptake_dt      = function(MO_het_ti_minus_1,   rFauna_MO_uptake_per_day_at_TEMP,  K_MO_at_temp, Fauna_ti_minus_1, delta_t, growth_model_fauna_type, CC_group_fauna_g, yield_MO, Excretion , Mortality) {
  
  f_S_fauna <- dMO_fauna_degradation_factor_dt (MO_het_ti_minus_1,   rFauna_MO_uptake_per_day_at_TEMP,  K_MO_at_temp, Fauna_ti_minus_1, delta_t, growth_model_fauna_type, CC_group_fauna_g) 
    
  fauna_growth <- f_S_fauna * yield_MO 
  
  Fauna_ti <- Fauna_ti_minus_1 + fauna_growth - Excretion - Mortality
  
  # 
  if(Fauna_ti < 0){ 
      Fauna_ti <- .000000001#
    }else{
      Fauna_ti <- Fauna_ti 
    }

  return( list(fauna_growth, Fauna_ti )) #
  
}




#  physical degradation of detritus to BOC , which is then added on what is present
dBOC_stock_dt    <- function(BOC_ti_minus_1, f_S_MO, BOC_import_from_detritus_ti, Excretion) {
  
  BOC_stock_ti_diff <-  BOC_ti_minus_1 - f_S_MO 
  
    BOC_stock_ti <- BOC_stock_ti_diff + BOC_import_from_detritus_ti + Excretion
  return(BOC_stock_ti) #
}



# equivalent to (part of) maintenance
dExcretion_dt    = function(excretionRate, Fauna_ti_minus_1 ) {
  Excretion      <- excretionRate * Fauna_ti_minus_1  
  return(Excretion)
}


#Avramov thesis 2013: "For the amphipod N. inopinatus, the LT50,24h (± standard error) was equal to 27.1°C (±0.5), and for the isopod P.cavaticus the same parameter was equal to 23.0 °C (± 0.1). 
#Relatively soon, after five days of exposure, the values dropped to reach an LT50,5d of 23.3 °C (± 2.9) for N. inopinatus, and 16.6 °C (± 3.8) for P. cavaticus (details are given in Publication II). While these results already indicate a slightly higher susceptibility to thermal stress in the isopods, the difference between the two crustacean species becomes even more distinct when the temperatures tolerated by the entire tested group of each species are compared. The highest temperature that could be tolerated by all isopods for 24 hours was 16 °C, while the amphipods tolerated 20 °C. Moreover, the amphipods endured being exposed to 20 °C for a total of 20 days without a single animal dying. In contrast, in the isopod group, the same temperature led to 20% mortality within 24 hours of exposure. 

#Avramov (2013): ambient temperature :  p. 35: temperature prevailed, to which they had been previously acclimated ( ca. 12 °C)

#The highest temperature treatment that could be tolerated by all isopods and for the longest time was 12 °C for 5 days in our study. The highest temperature that was tolerated by all amphipods for the longest time was 16 °C (for > 50 days)." Avramov 2013

# since it is not easy to extrapolate the parameters from LT50 curves, and since they are often not provided in the literature, very roughly let's assume linear response.
# Thus: 100% at 12 degrees, between 50 and 100% at 16 degrees, between 0 and50% at 20 degrees (Brielmann 2011). Assuming that half of the fauna population behavoes like amphipods and half like asellids, 100% survival at 12 degrees, (75% survival at 16 degrees,) 25 % at 20 degrees. Taking the endpoint 20 degrees, 0.25 of the population survives aafter an increase by 8 degrees. This means that 0.75 have died. Thus , for each degree of increase, 0.75/8 = 0.09375 , roughly 1 % of the individuals die. Let's assume that it is the same below 12 deg

dMortality_dt    = function(mortalityRate, mortalityFraction_per_degree, Fauna_ti_minus_1, GWTEMP_ti ) {
  Mortality      <- mortalityRate * Fauna_ti_minus_1 * Fauna_ti_minus_1  
  
  if(is.na(mortalityFraction_per_degree)){
    Mortality <- Mortality
    return(Mortality)
  }
  if(!is.na(mortalityFraction_per_degree)){
    if(GWTEMP_ti  -12 < 0){
     Mortality      <- Mortality 
    }else {
      Mortality      <- Mortality + mortalityFraction_per_degree * abs(GWTEMP_ti  -12)* Fauna_ti_minus_1 
  }
  }
  return(Mortality)
}

