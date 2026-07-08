#Load required packages
library(dplyr)
library(tidyr)
library(openxlsx)#read.xlsx
library(DescTools)#year()
library(patchwork) #for sticking together different plots 
library(ggpubr)#ggarrange. 
library(cowplot)
library(colorspace)
#from https://stackoverflow.com/questions/67219891/create-additional-independent-legends-in-ggplot2
library(ggnewscale) #new_scale_colour


#install package for color blind-safe plots
# install.packages("ggokabeito")
# #You can alternatively install the development version of ggokabeito from GitHub with:
#   devtools::install_github("malcolmbarrett/ggokabeito")
library(ggokabeito) # #scale_fill_okabe_ito
library(ggplot2)
#install.packages("ggtext") #
library(ggtext)#for exponents in ylab in ggplot


rm(list = ls()) #remove any variables and data created before, to make sure that this scenarios with fresh data

##########
#the scenarios with the different parameters are listed in the excel file "parameters.xlsx"; 1 to 8. Change accordingly
##########
scenario <- 9

# define these paths for saving your result text files locally
#gw_FuldaEcosystemServices_plots_path <-""
#gw_FuldaEcosystemServices_results_txt_path <-""
#e.g.
gw_FuldaEcosystemServices_plots_path <-"D:"
gw_FuldaEcosystemServices_results_txt_path <-"D:"

##########
#preparing read-in of data
##########

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_data.R"
source(urlfiletext)  ##fulda_variables

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_parameters.R"
source(urlfiletext) # Fulda parameters

########
#functions reactions
urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/model_functions.R"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_prec_plot.R"
source(urlfiletext)

#get function "error_measures" from git
urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/error_measures.R"
source(urlfiletext)

#get function "gtm" from git
urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/gtm.r"
source(urlfiletext)

##########
#parameters
##########
#parameters from excel file - crucial for reading in the data with the respective temperature scenario

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/parameters.xlsx"
parameters <- read.xlsx(urlfiletext, startRow = 3, sheet = 1)


##########
#reading in Fulda data from file
##########

scenario_with_1_or_without_0_MO <-  parameters$scenario_with_1_or_without_0_MO[scenario]  
scenario_with_1_or_without_0_fauna <- parameters$scenario_with_1_or_without_0_fauna[scenario]  

factor_CC_MO <-  parameters$factor_CC_MO[scenario]
factor_CC_fauna <-  parameters$factor_CC_fauna[scenario]

#this depends on the two scenario variables scenario_with_1_or_without_0_MO and scenario_with_1_or_without_0_fauna
fulda_variables_read_in <- fulda_variables(scenario, factor_CC_MO, factor_CC_fauna)
names(fulda_variables_read_in) <- c("Fulda_daily_prec", "Fulda_daily_temp", "chem_ordered_per_date_1978_1981", "chem_ordered_per_date_1978_1981_mean_per_group", "fauna_deep_PerSamplPerTaxonWide_bm_sum", "fauna_deep_PerSamplPerTaxon_bm_mean_per_group", "t_0", "t_max", "DETRITUS_gr1_t0", "DETRITUS_gr2_t0", "DETRITUS_gr3_t0", "DETRITUS_gr4_t0", "BOC_gr1_t0", "BOC_gr2_t0", "BOC_gr3_t0", "BOC_gr4_t0", "MO_het_gr1_t0", "MO_het_gr2_t0", "MO_het_gr3_t0", "MO_het_gr4_t0", "fauna_gr1_t0", "fauna_gr2_t0", "fauna_gr3_t0", "fauna_gr4_t0",  "CC_table_MO", "CC_table_fauna", "Fulda_daily_temp_joh_long") 

list2env(fulda_variables_read_in, globalenv())

#read in chemical data; prepare for join
chem_ordered_per_date_1978_1981$group <- chem_ordered_per_date_1978_1981$kmeans4gr
chem_ordered_per_date_1978_1981$group_letter <- ifelse(chem_ordered_per_date_1978_1981$group ==  2, "P", ifelse(chem_ordered_per_date_1978_1981$group == 3, "R" , ifelse (chem_ordered_per_date_1978_1981$group == 4, "A", ifelse (chem_ordered_per_date_1978_1981$group == 1, "M", NA))))   
chem_ordered_per_date_1978_1981$group_letter <- factor(chem_ordered_per_date_1978_1981$group_letter , levels = c("R", "M", "P", "A"))

#read parameters  from file. requires Fulda_daily_prec and thus, needs to be run after the data are read in

#read parameters variables  from file
parameters_read_in <- read_parameters(scenario)
names(parameters_read_in) <- c("delta_t", "max_t", "aquifer_depth", "import_MO_het", "scenario_with_1_or_without_0_fauna", "scenario_with_1_or_without_0_MO", "mortalityRate", "import_fauna", "yield_ac", "yield_MO", "K_MO_at_temp", "rMO_BOC_uptake_per_day_at_lab_temperature", "rFauna_MO_uptake_per_day_at_TEMP", "k1", "excretionRate",  "RECHARGE_COD_mol_per_m3_per_day_df", "lab_temp", "K_ac", "growth_model_MO_type", "growth_model_fauna_type", "mortalityFraction_per_degree", "microbe_loss_factor_when_no_fauna") 

#based on the data read in above and the parameters read in here, some further variables are derived within this function: e.g. "RECHARGE_COD_mol_per_m3_per_day_df"

list2env(parameters_read_in, globalenv())

if(is.na(max_t)){
  t_max = t_max
}else{
  t_max = max_t #set another end date than the one in the Fulda study; max_t is read in from parameters
}

##########
#creating temperature scenarios
##########

temperature_scenario <- parameters$temperature_scenario[scenario]

Fulda_daily_temp_joh_long$dateRi <- as.Date(Fulda_daily_temp_joh_long$dateRi)
#not used at this instance:
#Fulda_daily_temp_joh_long$TT_TER <- Fulda_daily_temp_joh_long$TT_TER + temperature_scenario #air temperature
names(Fulda_daily_temp_joh_long) <- sub("value" , "gw_temp", names(Fulda_daily_temp_joh_long))
Fulda_daily_temp_joh_long$gw_temp <- Fulda_daily_temp_joh_long$gw_temp + temperature_scenario #daily groundwater temperature extrapolated from air temperature


###################
#preparing in situ data for plotting and error measures and sensitivity analysis

#detritus
#prepare the detritus data for join 
DETR_1978_1981_mean_per_group_and_date <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(OS_mol_COD_L)) %>%
  dplyr::group_by(Date, kmeans4gr)%>%
  dplyr::summarise(OS_mol_COD_L = mean(OS_mol_COD_L, na.rm = TRUE)) %>%
  dplyr::mutate(ID = paste(Date, kmeans4gr, sep = "_"))%>%
  dplyr::mutate(x = OS_mol_COD_L)

#BOC biologically oxidizable carbon
chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(BOC_mol_COD_L)) %>%
  dplyr::group_by(Date, kmeans4gr)%>%
  dplyr::summarise(BOC_mol_COD_L = mean(BOC_mol_COD_L, na.rm = TRUE)) 

#microbes
MO_het_1978_1981_mean_per_group_and_date <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(total_Prok_mol_COD_L)) %>%
  dplyr::group_by(Date, kmeans4gr)%>%
  dplyr::summarise(MO_het_mol_COD_L = mean(total_Prok_mol_COD_L, na.rm = TRUE)) %>%
  dplyr::mutate(ID = paste(Date, kmeans4gr, sep = "_"))%>%
  dplyr::mutate(x = MO_het_mol_COD_L)

#fauna
fauna_deep_PerSamplPerTaxonWide_bm_sum$group <- fauna_deep_PerSamplPerTaxonWide_bm_sum$kmeans4gr
fauna_deep_PerSamplPerTaxonWide_bm_sum$group_letter <- ifelse(fauna_deep_PerSamplPerTaxonWide_bm_sum$group ==  2, "P", ifelse(fauna_deep_PerSamplPerTaxonWide_bm_sum$group == 3, "R" , ifelse (fauna_deep_PerSamplPerTaxonWide_bm_sum$group == 4, "A", ifelse (fauna_deep_PerSamplPerTaxonWide_bm_sum$group == 1, "M", NA))))
fauna_deep_PerSamplPerTaxonWide_bm_sum$group_letter <- factor(fauna_deep_PerSamplPerTaxonWide_bm_sum$group_letter , levels = c("R", "M", "P", "A"))

#for visualization - explained in text and caption of the respective figures
fauna_deep_PerSamplPerTaxonWide_bm_sum_no_high_biomass  <- fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
  dplyr::filter(bm_mol_COD_perL < 0.00015) 
maxfaunaplot <-max(fauna_deep_PerSamplPerTaxonWide_bm_sum_no_high_biomass$bm_mol_COD_perL)

fauna_deep_PerSamplPerTaxonWide_bm_sum_for_plot <- fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
  dplyr::filter(!is.na(bm_mol_COD_perL))%>%
  dplyr::filter(!is.na(kmeans4gr))

fauna_mean_per_group_and_date <- fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
  dplyr::filter(!is.na(bm_mol_COD_perL)) %>%
  dplyr::group_by(dateRi, kmeans4gr)%>%
  dplyr::summarise(bm_mol_COD_perL = mean(bm_mol_COD_perL, na.rm = TRUE)) %>%
  dplyr::mutate(ID = paste(dateRi, kmeans4gr, sep = "_"))%>%
  dplyr::mutate(x = bm_mol_COD_perL)

##########
#container for results of the error measures derivation
##########

error_measures_data_table <- data.frame(scenario = NA, group = NA, variable = NA, R2 = NA, MAE = NA, RMSE = NA, MB = NA, NSE = NA, N = NA)

##########
#reading in Fulda precitiation plot - does not depend on model variables, but depends on Fulda_daily_prec in fulda_variables_read_in, and thus, cannot be read in earlier than this
##########

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_prec_plot.R"
source(urlfiletext)

Fulda_prec_plotted <- Fulda_prec_plot(enddate = t_max)

##########
# preparing data frame with temperature and precipitation which will also take the results
##########

# group 1
results1 <- Fulda_daily_temp_joh_long %>%
    dplyr::filter(group_letter == "M" & 
    Fulda_daily_temp_joh_long$dateRi <= t_max
     & Fulda_daily_temp_joh_long$dateRi >= t_0)

#check whether that's consistent with the dates in parameters:
if(results1$dateRi[1] != t_0){
    print("results_1 for group 1 - check input t_0 - the temperature data series might start later than t_0, or the date is not formatted correctly")
    }

if(results1$dateRi[dim(results1)[1]] != t_max){
    print("results_1 for group 1 - check input t_max - the temperature data series might finish earlier than t_0, or the date is not formatted correctly")
    }

results1 <- as.data.frame(results1)

results1$DETRITUS[1] <- DETRITUS_gr1_t0 #mol COD / L, was OS
results1$import_from_detritus[1] <- 0
results1$BOC[1] <- BOC_gr1_t0
results1$MO_het[1] <- MO_het_gr1_t0
results1$fauna[1] <- fauna_gr1_t0


# group 2

results2 <- Fulda_daily_temp_joh_long %>%
    dplyr::filter(group_letter == "P" & 
    Fulda_daily_temp_joh_long$dateRi <= t_max
     & Fulda_daily_temp_joh_long$dateRi >= t_0)

#check whether that's consistent with the dates in parameters:
if(results2$dateRi[1] != t_0){
    print("results_2 for group 2 - check input t_0 - the temperature data series might start later than t_0, or the date is not formatted correctly")
    }

if(results2$dateRi[dim(results2)[1]] != t_max){
    print("results_2 for group 2 - check input t_max - the temperature data series might finish earlier than t_0, or the date is not formatted correctly")
    }

results2 <- as.data.frame(results2)

results2$DETRITUS[1] <- DETRITUS_gr2_t0 #mol COD / L
results2$import_from_detritus[1] <- 0
results2$BOC[1] <- BOC_gr2_t0
results2$MO_het[1] <- MO_het_gr2_t0
results2$fauna[1] <- fauna_gr2_t0


# group 3
results3 <- Fulda_daily_temp_joh_long %>%
    dplyr::filter(group_letter == "R" & 
    Fulda_daily_temp_joh_long$dateRi <= t_max
     & Fulda_daily_temp_joh_long$dateRi >= t_0)

#check whether that's consistent with the dates in parameters:
if(results3$dateRi[1] != t_0){
    print("results_3 for group 3 - check input t_0 - the temperature data series might start later than t_0, or the date is not formatted correctly")
    }

if(results3$dateRi[dim(results3)[1]] != t_max){
    print("results_3 for group 3 - check input t_max - the temperature data series might finish earlier than t_0, or the date is not formatted correctly")
    }

results3 <- as.data.frame(results3)

results3$DETRITUS[1] <- DETRITUS_gr3_t0 #mol COD / L
results3$import_from_detritus[1] <- 0
results3$BOC[1] <- BOC_gr3_t0
results3$MO_het[1] <- MO_het_gr3_t0
results3$fauna[1] <- fauna_gr3_t0


# group 4
results4 <- Fulda_daily_temp_joh_long %>%
    dplyr::filter(group_letter == "A" & 
    Fulda_daily_temp_joh_long$dateRi <= t_max
     & Fulda_daily_temp_joh_long$dateRi >= t_0)

#check whether that's consistent with the dates in parameters:
if(results4$dateRi[1] != t_0){
    print("results_4 for group 4 - check input t_0 - the temperature data series might start later than t_0, or the date is not formatted correctly")
    }

if(results4$dateRi[dim(results4)[1]] != t_max){
    print("results_4 for group 4 - check input t_max - the temperature data series might finish earlier than t_0, or the date is not formatted correctly")
    }

results4 <- as.data.frame(results4)

results4$DETRITUS[1] <- DETRITUS_gr4_t0 #mol COD / L
results4$import_from_detritus[1] <- 0
results4$BOC[1] <- BOC_gr4_t0
results4$MO_het[1] <- MO_het_gr4_t0
results4$fauna[1] <- fauna_gr4_t0

results <- rbind(results1, results2, results3, results4)

#results$group_letter <- ifelse(results$group ==  2, "P", ifelse(results$group == 3, "R" , ifelse (results$group == 4, "A", ifelse (results$group == 1, "M", NA))))

results$group_letter <- factor(results$group_letter , levels = c("R", "M", "P", "A"))

uniquedatevector <- unique(results$dateRi)
uniquegroupvector <- unique(results$group)
uniquegrouplettervector <- unique(results$group_letter)

results_ <- results[1,] #make a copy of this container' header that the model results get written into

#the following for loop is largely consistent with v. 1 . In v. 2 the for loop here can be replaced by a call to the function gtm, see below the loop 
for (g in 1:length(unique(results$group))){
  CC_group_MO_g <- CC_table_MO$CC[CC_table_MO$group == uniquegroupvector[g]]
  CC_group_fauna_g <- CC_table_fauna$CC[CC_table_fauna$group == uniquegroupvector[g]]
  group_letter_g <- unique(results$group_letter[results$group == uniquegroupvector[g]])
  
  results_g <- results %>%
    dplyr::filter(group_letter == group_letter_g)
    
    for (i in 2:(length(uniquedatevector))) {
      
    #groundwater temp.
    GWTEMP_ti <- results_g$gw_temp [results_g$dateRi == results$dateRi[i]]
    
    RECHARGE_COD_mol_per_m3_per_day_df_ti <- RECHARGE_COD_mol_per_m3_per_day_df$RECHARGE_COD_mol_per_m3_per_day [RECHARGE_COD_mol_per_m3_per_day_df$dateRi == uniquedatevector[i]]
    
    RECHARGE_COD_mol_per_L_per_day_df_ti <- RECHARGE_COD_mol_per_m3_per_day_df_ti /1000
    
    #take the respective field of the  Date == Date[i] aund group == group[g] 
    #DETRITUS_ti_minus_1 <- results$DETRITUS[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]]     
    #BOC_ti_minus_1 <- results$BOC[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]]
    #MO_het_ti_minus_1 <- results$MO_het[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]] 
    #Fauna_ti_minus_1 <- results$fauna[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]] 
    #DETRITUS_ti_minus_1 <- results$DETRITUS[results$dateRi == uniquedatevector[i-1] & results$group == uniquegroupvector[g]]     
    
    BOC_ti_minus_1 <- results_g$BOC[results_g$dateRi == uniquedatevector[i-1]]
    
    MO_het_ti_minus_1 <- results_g$MO_het[results_g$dateRi == uniquedatevector[i-1]]
    
    Fauna_ti_minus_1 <- results_g$fauna[results_g$dateRi == uniquedatevector[i-1]]
    
    DETRITUS_ti_minus_1 <- results_g$DETRITUS[results_g$dateRi == uniquedatevector[i-1]]     

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
    
    #results$MO_het[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- MO_het_ti
    #results$fauna[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- Fauna_ti
    #results$BOC[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- BOC_ti
    #results$growthrate[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- rMO_BOC_uptake_per_day_at_TEMP
    #results$DETRITUS[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- DETRITUS_ti 
    #results$import_from_detritus[results$dateRi == uniquedatevector[i] & results$group == uniquegroupvector[g]] <- BOC_import_from_detritus_ti 
    
    results_g$MO_het[results_g$dateRi == uniquedatevector[i] ] <- MO_het_ti
    
    results_g$fauna[results_g$dateRi == uniquedatevector[i]] <- Fauna_ti
    
    results_g$BOC[results_g$dateRi == uniquedatevector[i]] <- BOC_ti
    
    #results_g$growthrate[results_g$dateRi == uniquedatevector[i] ] <- rMO_BOC_uptake_per_day_at_TEMP
    
    results_g$DETRITUS[results_g$dateRi == uniquedatevector[i] ] <- DETRITUS_ti 
    
    results_g$import_from_detritus[results_g$dateRi == uniquedatevector[i] ] <- BOC_import_from_detritus_ti 
    
  } #end groups
  results_ <- rbind(results_, results_g)
}#end time




#this for loop can be replaced by a call to the function gtm() implemented in v. 2.0.0 of the github code
#results_ <- gtm()

#ggplot requires the data to be in data frame
results_df <- as.data.frame(results_)

#saving this scenario's data for later use
setwd(gw_FuldaEcosystemServices_results_txt_path) 
write.table(results_df, paste0("results_df_scenario_",scenario,".txt"), row.names = FALSE)

#for plotting several variables, make long form of the results data frame
results_df_long <- results_df %>%
  tidyr::pivot_longer(cols = c(BOC, DETRITUS, MO_het, fauna), names_to = "variable") 

write.table(results_df, paste0("results_df_long_scenario_",scenario,".txt"), row.names = FALSE)


unified_axes <- 1 # 1 = make the same axis for all four subplots , representing the four groups . 0 = axes reflect the groups' minima and maxima
unified_axes_fauna <- 1


#combine measured data with modelled data to calculate model accuracy - for that, join model results to measured values
DETR_1978_1981_mean_per_group_and_date_joined <- left_join(DETR_1978_1981_mean_per_group_and_date, results_df, by =  c("Date" = "dateRi", "kmeans4gr" = "group"))

error_measures_data_DETRITUS_groups <- error_measures_data_table

for (i in 1: length(unique(DETR_1978_1981_mean_per_group_and_date_joined$kmeans4gr))) {
  DETR_1978_1981_mean_per_group_and_date_joined_group <- DETR_1978_1981_mean_per_group_and_date_joined %>%
    dplyr::filter(kmeans4gr == i)
  error_measures_data_DETRITUS_group_i <- error_measures(DETR_1978_1981_mean_per_group_and_date_joined_group, "OS_mol_COD_L", "DETRITUS")
  error_measures_data_DETRITUS_group_i$variable = "DETRITUS"
  error_measures_data_DETRITUS_group_i$group <- i
  error_measures_data_DETRITUS_groups <- rbind(error_measures_data_DETRITUS_groups, error_measures_data_DETRITUS_group_i)
}

error_measures_data_DETRITUS_groups <- error_measures_data_DETRITUS_groups %>%
  dplyr::filter(!is.na(N))

geomtexttable_DETRITUS <- as.data.frame(cbind("group" = error_measures_data_DETRITUS_groups$group, "x" = rep("1980-04-01",4), "y" = rep(max(chem_ordered_per_date_1978_1981$OS_mol_COD_L, results_df$DETRITUS, na.rm = TRUE)*.95, 4),
                                              "R2" = error_measures_data_DETRITUS_groups$R2 ,
                                              "MAE" = error_measures_data_DETRITUS_groups$MAE ,
                                              "RMSE" = error_measures_data_DETRITUS_groups$RMSE ,
                                              "NSE" = error_measures_data_DETRITUS_groups$NSE ,
                                              "MB" = error_measures_data_DETRITUS_groups$MB ,"N" = error_measures_data_DETRITUS_groups$N ))

geomtexttable_DETRITUS$group_letter = ifelse(geomtexttable_DETRITUS$group ==  2, "P", ifelse(geomtexttable_DETRITUS$group == 3, "R" , ifelse (geomtexttable_DETRITUS$group == 4, "A", ifelse (geomtexttable_DETRITUS$group == 1, "M", NA))))   

geomtexttable_DETRITUS$group_letter <- factor(geomtexttable_DETRITUS$group_letter, levels = c("R", "M", "P", "A"))

results_df$group_letter <- factor(results_df$group_letter, levels = c("R", "M", "P", "A"))




#  preparing statistics for plotting the results

geomtexttable_DETRITUS$x <- as.Date(geomtexttable_DETRITUS$x)
geomtexttable_DETRITUS$y <- as.numeric(geomtexttable_DETRITUS$y)
geomtexttable_DETRITUS$y_R2 <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, DETR_1978_1981_mean_per_group_and_date_joined$DETRITUS, na.rm = TRUE)*.05
geomtexttable_DETRITUS$y_MAE <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, na.rm = TRUE)*.15
geomtexttable_DETRITUS$y_RMSE <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, na.rm = TRUE)*.35
geomtexttable_DETRITUS$y_NSE <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, na.rm = TRUE)*.55
geomtexttable_DETRITUS$y_MB <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, na.rm = TRUE)*.75
geomtexttable_DETRITUS$y_N <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, na.rm = TRUE)*.9
geomtexttable_DETRITUS$R2 <- paste("R2 =", geomtexttable_DETRITUS$R2)
geomtexttable_DETRITUS$MAE <- paste("MAE =", geomtexttable_DETRITUS$MAE)
geomtexttable_DETRITUS$RMSE <- paste("RMSE =", geomtexttable_DETRITUS$RMSE)
geomtexttable_DETRITUS$NSE <- paste("NSE =", geomtexttable_DETRITUS$NSE)
geomtexttable_DETRITUS$MB <- paste("MB =", geomtexttable_DETRITUS$MB)
geomtexttable_DETRITUS$N <- paste("N =", geomtexttable_DETRITUS$N)



Fulda_Detritus_partOrganics_plot <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(OS_mol_COD_L))%>%
  ggplot() +
  geom_point(data = results_df, aes(x = dateRi, y = DETRITUS, colour = as.factor(group_letter)),  pch = 16, size = 0.5, show.legend = FALSE 
  )+
  geom_point(aes(x = Date, y = OS_mol_COD_L, fill = as.factor(group_letter)),  pch = 21 , colour = "black", show.legend = FALSE
  )+
  scale_x_date(limits = c(t_0, t_max) )+
  #labs(x = "Date", y = "Detritus\n [mol COD/ L]\n measured [o] and\nmodelled [.]")+ 
  labs(x = "Date", y = "Detritus<br> [mol COD L<sup>-1</sup>]<br> measured [o] and <br>modelled [.]")+ # https://forum.posit.co/t/exponent-numbers-in-ggplot-labels/171969/2 library ggtext
  scale_fill_okabe_ito()+ 
  scale_color_okabe_ito()+

  geom_text(data = geomtexttable_DETRITUS,
            aes(x = x, y = y, label = R2), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_DETRITUS,
            aes(x = x, y = y_MAE, label = MAE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_DETRITUS,
            aes(x = x, y = y_RMSE, label = RMSE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_DETRITUS,
            aes(x = x, y = y_NSE, label = NSE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_DETRITUS,
            aes(x = x, y = y_MB, label = MB), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_DETRITUS,
            aes(x = x, y = y_N, label = N), hjust = 0, size = 2)+
  
  theme(panel.background = element_rect(fill = "white",  colour = "black",   
                                        linetype = "solid" ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.4),
        axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
  )

if (unified_axes == 1) {
  (Fulda_Detritus_partOrganics_plot <- Fulda_Detritus_partOrganics_plot+
     facet_grid(.~group_letter   
     )
  )
}else {
  (Fulda_Detritus_partOrganics_plot <- Fulda_Detritus_partOrganics_plot+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4)
  )
}


my.formula <- y ~ x

(Fulda_Detritus_partOrganics_plot_trends <- Fulda_Detritus_partOrganics_plot +
    geom_smooth(method = "lm",
                data = results_df, aes(x = dateRi, y = DETRITUS),
                se=TRUE,  formula = my.formula, lwd = 0.3) +
    stat_smooth(method = "lm",
                data = results_df, aes(x = dateRi, y = DETRITUS)
                ,  formula = my.formula, lwd = 0.3)
)


#the maximum is calculated from the non-aggregated data to be able to see the individual points
max_BOC <- max(chem_ordered_per_date_1978_1981$BOC_mol_COD_L, results_df$BOC, na.rm =TRUE)

#combine measured data with modelled data to calculate model accuracy - for that, join model results to measured values
#there is one model per group, thus, this will be compared with the mean per group
chem_ordered_per_date_1978_1981_BOC_joined <- left_join(chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date, results_df, by =  c("Date" = "dateRi", "kmeans4gr" = "group"))

error_measures_data_BOC_groups <- error_measures_data_table

for (i in 1: length(unique(chem_ordered_per_date_1978_1981_BOC_joined$kmeans4gr))) {
  chem_ordered_per_date_1978_1981_BOC_joined_group <- chem_ordered_per_date_1978_1981_BOC_joined %>%
    dplyr::filter(kmeans4gr == i)
  error_measures_data_BOC_group_i <- error_measures(chem_ordered_per_date_1978_1981_BOC_joined_group, "BOC_mol_COD_L", "BOC")
  error_measures_data_BOC_group_i$variable = "BOC"
  error_measures_data_BOC_group_i$group <- i
  error_measures_data_BOC_groups <- rbind(error_measures_data_BOC_groups, error_measures_data_BOC_group_i)
}

error_measures_data_BOC_groups <- error_measures_data_BOC_groups %>%
  dplyr::filter(!is.na(N))


#in the sensitivity analysis version of this file, this join is done before the sensitivity analysis
chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date_joined <- dplyr::left_join(results, chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date, by =  c( "dateRi" = "Date" , "group" = "kmeans4gr"  ))


#make a table with 4x the same coordinates x, y, and for the label: the column BOC
geomtexttable_BOC <- as.data.frame(cbind("group" = error_measures_data_BOC_groups$group, 
"x" = rep("1980-04-01",4), 
"y" = rep(max_BOC*.95, 4),
                                         "R2" = error_measures_data_BOC_groups$R2 ,
                                         "MAE" = error_measures_data_BOC_groups$MAE ,
                                         "RMSE" = error_measures_data_BOC_groups$RMSE ,
                                         "NSE" = error_measures_data_BOC_groups$NSE ,
                                         "MB" = error_measures_data_BOC_groups$MB ,"N" = error_measures_data_BOC_groups$N ))

geomtexttable_BOC$group_letter = ifelse(geomtexttable_BOC$group ==  2, "P", ifelse(geomtexttable_BOC$group == 3, "R" , ifelse (geomtexttable_BOC$group == 4, "A", ifelse (geomtexttable_BOC$group == 1, "M", NA))))   

geomtexttable_BOC$x <- as.Date(geomtexttable_BOC$x)
geomtexttable_BOC$y <- as.numeric(geomtexttable_BOC$y)
geomtexttable_BOC$y_R2 <- geomtexttable_BOC$y-max_BOC*.05
geomtexttable_BOC$y_MAE <- geomtexttable_BOC$y-max_BOC*.15
geomtexttable_BOC$y_RMSE <- geomtexttable_BOC$y-max_BOC*.35
geomtexttable_BOC$y_NSE <- geomtexttable_BOC$y-max_BOC*.55
geomtexttable_BOC$y_MB <- geomtexttable_BOC$y-max_BOC*.75
geomtexttable_BOC$y_N <- geomtexttable_BOC$y-max_BOC*.95
geomtexttable_BOC$R2 <- paste("R2 =", geomtexttable_BOC$R2)
geomtexttable_BOC$MAE <- paste("MAE =", geomtexttable_BOC$MAE)
geomtexttable_BOC$RMSE <- paste("RMSE =", geomtexttable_BOC$RMSE)
geomtexttable_BOC$NSE <- paste("NSE =", geomtexttable_BOC$NSE)
geomtexttable_BOC$MB <- paste("MB =", geomtexttable_BOC$MB)
geomtexttable_BOC$N <- paste("N =", geomtexttable_BOC$N)

geomtexttable_BOC$group_letter <- factor(geomtexttable_BOC$group_letter, levels = c("R", "M", "P", "A"))

Fulda_BOC_plot <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(BOC_mol_COD_L)) %>%
  ggplot() +
  geom_point(data = results_df, aes(x = dateRi, y = BOC, colour = as.factor(group_letter)),   pch = 16, size = 0.5, show.legend = FALSE)+
  geom_point(aes(x = Date, y = BOC_mol_COD_L, fill = as.factor(group_letter)),  pch = 21  , colour = "black", show.legend = FALSE)  +
  scale_x_date(limits = c(t_0, t_max) )+
  labs(x = "Date", y = "BOC<br> [mol COD L<sup>-1</sup>]<br> measured [o] and <br> modelled [.]")+ 
  scale_fill_okabe_ito()+ 
  scale_color_okabe_ito()+
  
  geom_text(data = geomtexttable_BOC,
            aes(x = x, y = y, label = R2), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_BOC,
            aes(x = x, y = y_MAE, label = MAE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_BOC,
            aes(x = x, y = y_RMSE, label = RMSE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_BOC,
            aes(x = x, y = y_NSE, label = NSE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_BOC,
            aes(x = x, y = y_MB, label = MB), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_BOC,
            aes(x = x, y = y_N, label = N), hjust = 0, size = 2)+
  
  theme(panel.background = element_rect(fill = "white",  colour = "black",   
                                        linetype = "solid" ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.4
        ),
        legend.key = element_blank(),
        axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
  )


if (unified_axes == 1) {
  (Fulda_BOC_plot <- Fulda_BOC_plot+
     facet_grid(.~group_letter   
     )
  )
}else {
  (Fulda_BOC_plot <- Fulda_BOC_plot+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4)
  )
}

my.formula <- y ~ x

(Fulda_BOC_plot_trends <- Fulda_BOC_plot +
    geom_smooth(method = "lm",
                data = results_df, aes(x = dateRi, y = BOC),
                se=TRUE,  formula = my.formula, lwd = 0.3) 
) 



#combine measured data with modelled data to calculate model accuracy - for that, join model results to the average per group and date of measured values for microbial numbers
MO_het_1978_1981_mean_per_group_and_date_joined <- left_join(MO_het_1978_1981_mean_per_group_and_date, results_df, by =  c("Date" = "dateRi", "kmeans4gr" = "group"))

max_MO_het_for_plot <- max(chem_ordered_per_date_1978_1981$total_Prok_mol_COD_L, results_df$MO_het, na.rm =TRUE)

error_measures_data_MO_het_groups <- error_measures_data_table
for (i in 1: length(unique(MO_het_1978_1981_mean_per_group_and_date_joined$kmeans4gr))) {
  MO_het_1978_1981_mean_per_group_and_date_joined_group <- MO_het_1978_1981_mean_per_group_and_date_joined %>%
    dplyr::filter(kmeans4gr == i)
  error_measures_data_MO_het_group_i <- error_measures(MO_het_1978_1981_mean_per_group_and_date_joined_group, "MO_het_mol_COD_L", "MO_het")
  error_measures_data_MO_het_group_i$variable = "MO_het"
  error_measures_data_MO_het_group_i$group <- i
  error_measures_data_MO_het_groups <- rbind(error_measures_data_MO_het_groups, error_measures_data_MO_het_group_i)
}

error_measures_data_MO_het_groups <- error_measures_data_MO_het_groups %>%
  dplyr::filter(!is.na(N))

geomtexttable_MO <- as.data.frame(cbind("group" = error_measures_data_MO_het_groups$group, 
                                        "x" = rep("1980-04-01",4), 
                                        "y" = rep(max_MO_het_for_plot*.95, 4),
                                        "R2" = error_measures_data_MO_het_groups$R2 ,
                                        "MAE" = error_measures_data_MO_het_groups$MAE ,
                                        "RMSE" = error_measures_data_MO_het_groups$RMSE ,
                                        "NSE" = error_measures_data_MO_het_groups$NSE ,
                                        "MB" = error_measures_data_MO_het_groups$MB ,"N" = error_measures_data_MO_het_groups$N ))

geomtexttable_MO$group_letter = ifelse(geomtexttable_MO$group ==  2, "P", ifelse(geomtexttable_MO$group == 3, "R" , ifelse (geomtexttable_MO$group == 4, "A", ifelse (geomtexttable_MO$group == 1, "M", NA))))   

geomtexttable_MO$group_letter <- factor(geomtexttable_MO$group_letter, levels = c("R", "M", "P", "A"))

chem_ordered_per_date_1978_1981$group_letter <- factor(chem_ordered_per_date_1978_1981$group_letter, levels = c("R", "M", "P", "A"))

results_df$group_letter <- factor(results_df$group_letter, levels = c("R", "M", "P", "A"))

geomtexttable_MO$x <- as.Date(geomtexttable_MO$x)
geomtexttable_MO$y <- as.numeric(geomtexttable_MO$y)
geomtexttable_MO$y_R2 <- geomtexttable_MO$y-max(results_df$MO_het, MO_het_1978_1981_mean_per_group_and_date_joined$MO_het_mol_COD_L)*.01
geomtexttable_MO$y_MAE <- geomtexttable_MO$y-max(results_df$MO_het, MO_het_1978_1981_mean_per_group_and_date_joined$MO_het_mol_COD_L)*.15
geomtexttable_MO$y_RMSE <- geomtexttable_MO$y-max(results_df$MO_het, MO_het_1978_1981_mean_per_group_and_date_joined$MO_het_mol_COD_L)*.35
geomtexttable_MO$y_NSE <- geomtexttable_MO$y-max(results_df$MO_het, MO_het_1978_1981_mean_per_group_and_date_joined$MO_het_mol_COD_L)*.55
geomtexttable_MO$y_MB <- geomtexttable_MO$y-max(results_df$MO_het, MO_het_1978_1981_mean_per_group_and_date_joined$MO_het_mol_COD_L)*.75
geomtexttable_MO$y_N <- geomtexttable_MO$y-max(results_df$MO_het, MO_het_1978_1981_mean_per_group_and_date_joined$MO_het_mol_COD_L)*.95
geomtexttable_MO$R2 <- paste("R2 =", geomtexttable_MO$R2)
geomtexttable_MO$MAE <- paste("MAE =", geomtexttable_MO$MAE)
geomtexttable_MO$RMSE <- paste("RMSE =", geomtexttable_MO$RMSE)
geomtexttable_MO$NSE <- paste("NSE =", geomtexttable_MO$NSE)
geomtexttable_MO$MB <- paste("MB =", geomtexttable_MO$MB)
geomtexttable_MO$N <- paste("N =", geomtexttable_MO$N)


Fulda_MO_plot <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(total_Prok_mol_COD_L))%>%
  ggplot(
  ) +
  geom_point(data = results_df, aes(x = dateRi, y = MO_het, colour = as.factor(group_letter)),   pch = 16,  size = 0.5 , show.legend = FALSE
  )+
  geom_point(aes(x = Date, y = total_Prok_mol_COD_L, fill = as.factor(group_letter)),  pch = 21, colour = "black", show.legend = FALSE
  )+
  
  scale_x_date(limits = c(t_0, t_max) )+
  
  scale_y_continuous (limits = c(0, max_MO_het_for_plot) )+
  
  #labs(x = "Date", y = "Microbial dry mass\n[mol COD / L]\nmeasured [o] and\nmodelled [.]")+ 
  labs(x = "Date", y = "Microbial dry mass<br> [mol COD L<sup>-1</sup>]<br> measured [o] and <br>modelled [.]")+ 
  
  scale_fill_okabe_ito()+ 
  scale_color_okabe_ito()+
  
  geom_text(data = geomtexttable_MO,
            aes(x = x, y = y, label = R2), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_MO,
            aes(x = x, y = y_MAE, label = MAE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_MO,
            aes(x = x, y = y_RMSE, label = RMSE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_MO,
            aes(x = x, y = y_NSE, label = NSE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_MO,
            aes(x = x, y = y_MB, label = MB), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_MO,
            aes(x = x, y = y_N, label = N), hjust = 0, size = 2)+
  
  theme(panel.background = element_rect(fill = "white",  colour = "black",   
                                        linetype = "solid" ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.4),
        axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
  )


if (unified_axes == 1) {
  (Fulda_MO_plot <- Fulda_MO_plot+
     facet_grid(.~group_letter   
     )
  )
}else {
  (Fulda_MO_plot <- Fulda_MO_plot+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4)
  )
}


(Fulda_MO_plot_trends <- Fulda_MO_plot +
    geom_smooth(method = "lm",
                data = results_df, aes(x = dateRi, y = MO_het
                ),
                se=TRUE,  formula = my.formula, lwd = 0.3) 
) 




#combine measured data with modelled data to calculate model accuracy - for that, join model results to measured values for fauna data
fauna_mean_per_group_and_date_joined <- left_join(fauna_mean_per_group_and_date, results_df, by =  c("dateRi" = "dateRi", "kmeans4gr" = "group"))

error_measures_data_fauna_groups <- error_measures_data_table
for (i in 1: length(unique(fauna_mean_per_group_and_date_joined$kmeans4gr))) {
  fauna_mean_per_group_and_date_joined_group <- fauna_mean_per_group_and_date_joined %>%
    dplyr::filter(kmeans4gr == i)
  error_measures_data_fauna_group_i <- error_measures(fauna_mean_per_group_and_date_joined_group, "bm_mol_COD_perL", "fauna")
  error_measures_data_fauna_group_i$variable = "fauna"
  error_measures_data_fauna_group_i$group <- i
  error_measures_data_fauna_groups <- rbind(error_measures_data_fauna_groups, error_measures_data_fauna_group_i)
}

error_measures_data_fauna_groups <- error_measures_data_fauna_groups %>%
  dplyr::filter(!is.na(N))

geomtexttable_fauna <- as.data.frame(cbind("group" = error_measures_data_fauna_groups$group, "x" = rep("1980-04-01",4), "y" = rep(maxfaunaplot*.95, 4),
                                           "R2" = error_measures_data_fauna_groups$R2 ,
                                           "MAE" = error_measures_data_fauna_groups$MAE ,
                                           "RMSE" = error_measures_data_fauna_groups$RMSE ,
                                           "NSE" = error_measures_data_fauna_groups$NSE ,
                                           "MB" = error_measures_data_fauna_groups$MB ,"N" = error_measures_data_fauna_groups$N ))

geomtexttable_fauna$group_letter = ifelse(geomtexttable_fauna$group ==  2, "P", ifelse(geomtexttable_fauna$group == 3, "R" , ifelse (geomtexttable_fauna$group == 4, "A", ifelse (geomtexttable_fauna$group == 1, "M", NA))))   

geomtexttable_fauna$group_letter <- factor(geomtexttable_fauna$group_letter, levels = c("R", "M", "P", "A"))

#should be redundant here
results_df$group_letter <- factor(results_df$group_letter, levels = c("R", "M", "P", "A"))

geomtexttable_fauna$x <- as.Date(geomtexttable_fauna$x)
geomtexttable_fauna$y <- as.numeric(geomtexttable_fauna$y)
geomtexttable_fauna$y_R2 <- geomtexttable_fauna$y-maxfaunaplot*.01
geomtexttable_fauna$y_MAE <- geomtexttable_fauna$y-maxfaunaplot*.15
geomtexttable_fauna$y_RMSE <- geomtexttable_fauna$y-maxfaunaplot*.35
geomtexttable_fauna$y_NSE <- geomtexttable_fauna$y-maxfaunaplot*.55
geomtexttable_fauna$y_MB <- geomtexttable_fauna$y-maxfaunaplot*.75
geomtexttable_fauna$y_N <- geomtexttable_fauna$y-maxfaunaplot*.9
geomtexttable_fauna$R2 <- paste("R2 =", geomtexttable_fauna$R2)
geomtexttable_fauna$MAE <- paste("MAE =", geomtexttable_fauna$MAE)
geomtexttable_fauna$RMSE <- paste("RMSE =", geomtexttable_fauna$RMSE)
geomtexttable_fauna$NSE <- paste("NSE =", geomtexttable_fauna$NSE)
geomtexttable_fauna$MB <- paste("MB =", geomtexttable_fauna$MB)
geomtexttable_fauna$N <- paste("N =", geomtexttable_fauna$N)



Fulda_fauna_plot <-fauna_deep_PerSamplPerTaxonWide_bm_sum_for_plot %>%
  ggplot( ) +
  geom_point(data = results_df, aes(x = dateRi, y = fauna, colour = as.factor(group_letter)),   pch = 16,  size = 0.5 , show.legend = FALSE  )+
  geom_point(data = fauna_deep_PerSamplPerTaxonWide_bm_sum_for_plot, aes(x = dateRi, y = bm_mol_COD_perL , fill = as.factor(group_letter)),  pch = 21, colour = "black", show.legend = FALSE )+
  lims(y = c(0,maxfaunaplot))+
  scale_x_date(limits = c(t_0, t_max)  )+
  labs(x = "Date", y = "Fauna dry mass<br> [mol COD L<sup>-1</sup>]<br> measured [o] and <br>modelled [.]")+ 
  
  scale_fill_okabe_ito()+ 
  scale_color_okabe_ito()+

  geom_text(data = geomtexttable_fauna,
            aes(x = x, y = y, label = R2), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_fauna,
            aes(x = x, y = y_MAE, label = MAE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_fauna,
            aes(x = x, y = y_RMSE, label = RMSE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_fauna,
            aes(x = x, y = y_NSE, label = NSE), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_fauna,
            aes(x = x, y = y_MB, label = MB), hjust = 0, size = 2)+
  geom_text(data = geomtexttable_fauna,
            aes(x = x, y = y_N, label = N), hjust = 0, size = 2)+
  
  theme(panel.background = element_rect(fill = "white",  colour = "black",   
                                        linetype = "solid" ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.4),
        axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
  )


if (unified_axes_fauna == 1) {
  (Fulda_fauna_plot <- Fulda_fauna_plot+
     facet_grid(.~group_letter   
     )
  )
}else {
  (Fulda_fauna_plot <- Fulda_fauna_plot+
     #  + 
     facet_wrap(.~group_letter, scales="free_y", ncol = 4)
  )
}


(Fulda_fauna_plot_trends <- Fulda_fauna_plot +
    geom_smooth(method = "lm",
                data = results_df, aes(x = dateRi, y = fauna
                ),
                se=TRUE,  formula = my.formula, lwd = 0.3) 
) 



Fulda_prec_plotted +  Fulda_Detritus_partOrganics_plot  + Fulda_BOC_plot + Fulda_MO_plot + Fulda_fauna_plot + plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "a", tag_suffix = ")")


setwd(gw_FuldaEcosystemServices_plots_path)
if (unified_axes == 1) {
  #save as png AND as pdf - for some journals, pdf are preferred, for others, png
  ggsave(paste0("model_measured_scenario_",scenario,"_one_y_scale_w_model_qual.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_scenario_",scenario,"_one_y_scale_w_model_qual.pdf"), width = 10, height = 9)
}else{
  #save as png AND as pdf
  ggsave(paste0("model_measured_scenario_",scenario,"_free_y_scale_w_model_qual.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_scenario_",scenario,"_free_y_scale_w_model_qual.pdf"), width = 10, height = 9)
}


(Fulda_trends <- Fulda_prec_plotted +  Fulda_Detritus_partOrganics_plot_trends  + Fulda_BOC_plot_trends + Fulda_MO_plot_trends + Fulda_fauna_plot_trends + plot_layout(ncol = 1) +
    plot_annotation(tag_levels = "a", tag_suffix = ")"))

setwd(gw_FuldaEcosystemServices_plots_path)
if (unified_axes == 1) {
  ggsave(paste0("model_measured_trend_scenario_",scenario,"_one_y_scale_w_model_qual.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_trend_scenario_",scenario,"_one_y_scale_w_model_qual.pdf"), width = 10, height = 9)
}else{
  ggsave(paste0("model_measured_trend_scenario_",scenario,"_free_y_scale_w_model_qual.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_trend_scenario_",scenario,"_free_y_scale_w_model_qual.png"), width = 10, height = 9)
}


# for the comparison of scenarios, read in files to combine them to one data set
setwd(gw_FuldaEcosystemServices_results_txt_path)

results_df_1 <- read.table( "results_df_scenario_1.txt", header = TRUE)
results_df_2 <- read.table( "results_df_scenario_2.txt", header = TRUE)
results_df_3 <- read.table( "results_df_scenario_3.txt", header = TRUE)
results_df_4 <- read.table( "results_df_scenario_4.txt", header = TRUE)
results_df_5 <- read.table( "results_df_scenario_5.txt", header = TRUE)
results_df_6 <- read.table( "results_df_scenario_6.txt", header = TRUE)
results_df_7 <- read.table( "results_df_scenario_7.txt", header = TRUE)
results_df_8 <- read.table( "results_df_scenario_8.txt", header = TRUE)
results_df_9 <- read.table( "results_df_scenario_9.txt", header = TRUE)
results_df_10 <- read.table( "results_df_scenario_10.txt", header = TRUE)
results_df_1$scenario <- 1
results_df_2$scenario <- 2
results_df_3$scenario <- 3
results_df_4$scenario <- 4
results_df_5$scenario <- 5
results_df_6$scenario <- 6
results_df_7$scenario <- 7
results_df_8$scenario <- 8
results_df_9$scenario <- 9
results_df_10$scenario <- 10

#this block is for using those data that were read-in using the lines above, as data for a one-scenario plot, the code for which follow directly below -
# outcomment this block for fresh data !

#if using one of the saved ones for producing the above figures, prepare the respective data, by replacing xx with the number of the scenario
#results_df <- results_df_xx
results_df$dateRi <- as.Date(results_df$dateRi)
results_df$group_letter <- factor(results_df$group_letter , levels = c("R", "M", "P", "A"))
scenario <- xx #only relevant for plotting ONE scenario, not for combining scenarios
#END block read in data - outcomment for fresh data !



# done only once
#results_df__for_overview_all <- rbind(results_df_1, results_df_2, results_df_3, results_df_4, results_df_5, results_df_6, results_df_7, results_df_8, results_df_9, results_df_10)
results_df__for_overview_all$dateRi <- as.Date(results_df__for_overview_all$dateRi)

results_df__for_overview_all$group_letter <- factor(results_df__for_overview_all$group_letter , levels = c("R", "M", "P", "A"))

#setwd(gw_FuldaEcosystemServices_results_txt_path)

setwd(gw_FuldaEcosystemServices_results_txt_path)
#do only once
#write.table(results_df__for_overview_all, "results_df__for_overview_all__1_2_3_4_5_6_7_8_9_10.txt", row.names = FALSE)

### END writing the complete set of scneario runs
########


########
###  reading in the complete set of scenario runs, for calculating the linear model fits to the GTM model scenarios

setwd(gw_FuldaEcosystemServices_results_txt_path)
results_df__for_overview_all <- read.table("results_df__for_overview_all__1_2_3_4_5_6_7_8_9_10.txt", header = TRUE)

results_df__for_overview_all$dateRi <- as.Date(results_df__for_overview_all$dateRi)

results_df__for_overview_all$group_letter <- factor(results_df__for_overview_all$group_letter , levels = c("R", "M", "P", "A"))

results_df__for_overview_all_long <- results_df__for_overview_all %>%
  dplyr::group_by(dateRi, group, group_letter, scenario) %>%
  tidyr::pivot_longer(cols = c(DETRITUS, BOC, MO_het, fauna), names_to = "variable", values_to = "value")


#calculate trends from the six main runs and the four extreme runs for the two most sensitive parameters
lm_i_df <- lm_i_list <- NULL
i <- j <- 1 

for(i in c(1:length(unique(results_df__for_overview_all_long$scenario)))){
  
  for(j in c(1:length(unique(results_df__for_overview_all_long$group)))){
    
    for(k in c(1:length(unique(results_df__for_overview_all_long$variable)))){
      
      scenario_i <- unique(results_df__for_overview_all_long$scenario)[i]
      group_j <- unique(results_df__for_overview_all_long$group)[j]
      variable_k <- unique(results_df__for_overview_all_long$variable)[k]
      
      results_df__for_overview_all_long_i <- results_df__for_overview_all_long %>%
        dplyr::filter(scenario == scenario_i  & group == group_j &
                        variable == variable_k) 
      
      lm_i <- lm(results_df__for_overview_all_long_i$value ~  results_df__for_overview_all_long_i$dateRi) 
      lm_i_df$coefficient_intercept <- lm_i$coefficients[1][[1]]
      lm_i_df$coefficient_slope <- lm_i$coefficients[2][[1]]
      
      if(length(anova(lm_i)$F_value[1][[1]] > 0)) {
        lm_i_df$F_value <- anova(lm_i)$F_value[1][[1]]
      }else{
        lm_i_df$F_value <- "NA"
      }
      lm_i_df$p_val <- anova(lm_i)$"Pr(>F)"[1]
      lm_i_df$scenario <- scenario_i
      lm_i_df$group <- unique(results_df__for_overview_all_long_i$group)
      lm_i_df$variable <- variable_k
      
      lm_i_df$max_ <- max(results_df__for_overview_all_long_i$value, na.rm = TRUE)
      lm_i_df$conc_t_0 <- results_df__for_overview_all_long_i$value[1]
      lm_i_df$conc_t_max <- results_df__for_overview_all_long_i$value[length(results_df__for_overview_all_long_i$value)]
      lm_i_df$fitted_conc_t_0 <- lm_i$fitted.values[1]
      lm_i_df$fitted_conc_t_max <- lm_i$fitted.values[length(lm_i$fitted.values)]
      lm_i_df$diff_over_observation_period <- lm_i_df$conc_t_max - lm_i_df$conc_t_0
      lm_i_df$fitted_diff_over_observation_period <- lm_i$fitted.values[length(lm_i$fitted.values)]-lm_i$fitted.values[1]
      #lm_i_df$slope_per_year <- lm_i_df$fitted_diff_over_observation_period/lm_i_df$time_span
      lm_i_df_unlist <- unlist(lm_i_df) #here, the group letter becomes transformed into a number, for whatever reasons
      names(lm_i_df_unlist) <- names(lm_i_df)
      lm_i_list <- rbind(lm_i_list, as.data.frame(t(lm_i_df_unlist))) 
    }
  }
}

lm_i_list_ <- lm_i_list %>%
  dplyr::mutate(group_letter = ifelse(group==3,"R",ifelse(group==2,"P",ifelse(group==4,"A",ifelse(group==1,"M","z"))))) %>%
  dplyr::select(c("scenario", "group_letter", "variable", "coefficient_intercept", "coefficient_slope",  "F_value", "p_val", "diff_over_observation_period", "fitted_diff_over_observation_period", "conc_t_0", "conc_t_max", "fitted_conc_t_0", "fitted_conc_t_max", "group", "max_"))

setwd(gw_FuldaEcosystemServices_results_txt_path)
write.table(lm_i_list_, "results_df__lm_1_2_3_4_5_6_7_8_9_10.txt", row.names = FALSE)

#END  linear model fits to the GTM model scenarios


#for plots of the lm trends of the first 6 scenarios
#first make data set
# for this: reading in the already existing file with all the GTM model scenarios, and delimit it to the wanted data  

results_df__for_overview_all_1_to_6 <- read.table("results_df__for_overview_all__1_2_3_4_5_6_7_8_9_10.txt", header = TRUE) %>%
dplyr::filter(scenario %in% c(1:6))

results_df__for_overview_all_1_to_6$dateRi <- as.Date(results_df__for_overview_all_1_to_6$dateRi)

#colors chosen according to https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible

results_df__for_overview_all_1_to_6$colour_for_plot <- ifelse(results_df__for_overview_all_1_to_6$scenario == 1, "#009E73",ifelse(results_df__for_overview_all_1_to_6$scenario == 3, "#0072B2", ifelse(results_df__for_overview_all_1_to_6$scenario == 5, "#56B4E9", ifelse(results_df__for_overview_all_1_to_6$scenario == 2, "#F0E442", ifelse(results_df__for_overview_all_1_to_6$scenario == 4, "#E69F00", ifelse(results_df__for_overview_all_1_to_6$scenario == 6, "#D55E00", "black"))))))

results_df__for_overview_all_1_to_6$colour_for_plot <- factor(results_df__for_overview_all_1_to_6$colour_for_plot, level = c("#009E73", "#F0E442", "#0072B2",   "#E69F00", "#56B4E9",   "#D55E00"))

results_df__for_overview_all_1_to_6$line <- ifelse(results_df__for_overview_all_1_to_6$scenario == 1, 1,ifelse(results_df__for_overview_all_1_to_6$scenario == 3, 2, ifelse(results_df__for_overview_all_1_to_6$scenario == 5, 3,  ifelse(results_df__for_overview_all_1_to_6$scenario == 2, 5, ifelse(results_df__for_overview_all_1_to_6$scenario == 4, 6, ifelse(results_df__for_overview_all_1_to_6$scenario == 6, 4, 1))))))

results_df__for_overview_all_1_to_6$line <- factor(results_df__for_overview_all_1_to_6$line, level = c(1, 5, 2,  6, 3,   4))

results_df__for_overview_all_1_to_6$linewidth <- ifelse(results_df__for_overview_all_1_to_6$scenario == 1, 1.2,ifelse(results_df__for_overview_all_1_to_6$scenario == 3, .82, ifelse(results_df__for_overview_all_1_to_6$scenario == 5, .31, ifelse(results_df__for_overview_all_1_to_6$scenario == 2, 1.1, ifelse(results_df__for_overview_all_1_to_6$scenario == 4, .81, ifelse(results_df__for_overview_all_1_to_6$scenario == 6, .3, 1))))))

results_df__for_overview_all_1_to_6$linewidth <- factor(results_df__for_overview_all_1_to_6$linewidth, level = c(1.2, 1.1, .82, .81, .31, .3))

results_df__for_overview_all_1_to_6$size <- ifelse(results_df__for_overview_all_1_to_6$scenario == 1, 1.2,ifelse(results_df__for_overview_all_1_to_6$scenario == 3, .82, ifelse(results_df__for_overview_all_1_to_6$scenario == 5, .31, ifelse(results_df__for_overview_all_1_to_6$scenario == 2, 1.1, ifelse(results_df__for_overview_all_1_to_6$scenario == 4, .81, ifelse(results_df__for_overview_all_1_to_6$scenario == 6, .3, 1))))))
results_df__for_overview_all_1_to_6$size <- factor(results_df__for_overview_all_1_to_6$size, level = c(1.2, 1.1, .82, .81, .31, .3))

#plotting - here, a switch is used to either include the trend lines (only Sx Figure Sx, adding trend lines to Fig. 2 of the main paper), or not (Fig. 2, 3, 4 in the main paper)
elevated_temperature_scenarios <- FALSE#TODO  set to TRUE if the workaround for the six scenarios works which is not the case yet

override.col <- c( "#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00")
override.line <- c(1, 5, 2, 6, 3, 4)
override.linewidth <- c(2.2, 2.1, 1.02, 1.01,  .41,  .4)#this is redundant to setting linewidth in the data
override.size <-  c(2.2, 2.1, 1.02, 1.01,  .41,  .4)

my.formula <- y ~ x

(plot_trends_BOC <- ggplot(data = results_df__for_overview_all_1_to_6)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_6, aes(x = dateRi, y = BOC, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth) ), se=FALSE,  formula = my.formula )+ 
    #geom_point(data = data_from_lm_i_list_fitted_conc_t_0_six_runs, aes(x = dateRi, y = BOC, colour = as.factor(colour_for_plot)#, shape = as.factor(shape_for_plot), size = size 
    #) )+ 
    scale_colour_manual("Scenario",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                        labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linetype_manual("Scenario", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    
    scale_linewidth_manual("Scenario", values = c(2.2, 2.1, 1.02, 1.01,  .41,  .4),
                           labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_size_manual("Scenario", values = c(2.2, 2.1, 1.02, 1.01,  .41,  .4),
                           labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_shape_manual("Scenario", values = c(2.2, 2.1, 1.02, 1.01,  .41,  .4),
                           labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    
    guides(colour = guide_legend(override.aes = list(line = override.line, colour = override.col, linewidth = override.linewidth))) +
    
    #labs(x = "Date", y = "BOC [mol COD / L]") +
    labs(x = "Date", y = "BOC [mol COD L<sup>-1</sup>]")+ 
    theme(panel.background = element_rect(fill = "white",  colour = "black", 
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )
)

if (unified_axes == 1) {
  if(elevated_temperature_scenarios == TRUE) {
(plot_trends_BOC <- plot_trends_BOC +
     facet_wrap(elevated_temperature_scenarios~group_letter, ncol = 4))
  
  }else{
  (plot_trends_BOC <- plot_trends_BOC +
     facet_wrap(.~group_letter, ncol = 4))
  }
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_BOC <- plot_trends_BOC +
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__free_y_scale.pdf"), width = 8, height = 3)
}



(plot_trends_MO <- ggplot(data = results_df__for_overview_all_1_to_6)+
    
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_6, aes(x = dateRi, y = MO_het, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth)
                ),
                se=FALSE,  formula = my.formula )+ 
    scale_colour_manual("Scenario",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                        labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linetype_manual("Scenario", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
      
    scale_linewidth_manual("Scenario", values = c(2.2, 2.1, 1.02, 1.01,  .41,  .4),
                           labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    
    guides(colour = guide_legend(override.aes = list(line = override.line, colour = override.col, linewidth = override.linewidth))) +
    
    labs(x = "Date", y = "Microorganisms [mol COD / L]") +
    theme(panel.background = element_rect(fill = "white",  colour = "black",  linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )
    )


if (unified_axes == 1) {
  
  (plot_trends_MO <- plot_trends_MO +
     facet_wrap(.~group_letter, ncol = 4))
  
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6__one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6__one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_MO <- plot_trends_MO +
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6__free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6__free_y_scale.pdf"), width = 8, height = 3)
}



(plot_trends_fauna <- ggplot(data = results_df__for_overview_all_1_to_6)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_6, aes(x = dateRi, y = fauna, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth)  ), 
                se=FALSE,  formula = my.formula )+
    scale_colour_manual("Scenario",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                        labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linetype_manual("Scenario", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+   
    scale_linewidth_manual("Scenario", values = c(2.2, 2.1, 1.02, 1.01,  .41,  .4),
                           labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
   
    guides(colour = guide_legend(override.aes = list(line = override.line, colour = override.col, linewidth = override.linewidth))) +
    
    #labs(x = "Date", y = "Fauna [mol COD / L]") +
    labs(x = "Date", y = "Fauna dry mass [mol COD L<sup>-1</sup>]")+ 
    theme(panel.background = element_rect(fill = "white",  colour = "black",  linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )
    )


setwd(gw_FuldaEcosystemServices_plots_path)
  
if (unified_axes == 1) {
  
  (plot_trends_fauna <- plot_trends_fauna +
     facet_wrap(.~group_letter, ncol = 4))
  
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6__one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6__one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_fauna <- plot_trends_fauna +
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6__free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6__free_y_scale.pdf"), width = 8, height = 3)
}



######
#trend plots with the extreme k1 
######
#the code from here requires reading in the results as in the block marked for outcommenting above 
results_df__for_overview_all_1_to_8 <- read.table("results_df__for_overview_all__1_2_3_4_5_6_7_8_9_10.txt", header = TRUE) %>%
dplyr::filter(scenario %in% c(1:8))


results_df__for_overview_all_1_to_8$dateRi <- as.Date(results_df__for_overview_all_1_to_8$dateRi)

results_df__for_overview_all_1_to_8$group_letter <- factor(results_df__for_overview_all_1_to_8$group_letter , levels = c("R", "M", "P", "A"))

results_df__for_overview_all_1_to_8_long <- results_df__for_overview_all_1_to_8 %>%
  dplyr::group_by(dateRi, group_letter, scenario) %>%
  tidyr::pivot_longer(cols = c(DETRITUS, BOC, MO_het, fauna), names_to = "variable", values_to = "value")


# now the frist 6 in grey because they had been plotted in detail already, and the extreme scenarios in other colours
#colors chosen according to https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
#colours of scenarios 1 to 6 in grey
results_df__for_overview_all_1_to_8$colour_for_plot <- ifelse(results_df__for_overview_all_1_to_8$scenario == 1, "#1a1b1a",ifelse(results_df__for_overview_all_1_to_8$scenario == 3, "#2c2c2c", ifelse(results_df__for_overview_all_1_to_8$scenario == 5, "#353636", ifelse(results_df__for_overview_all_1_to_8$scenario == 2, "#373838", ifelse(results_df__for_overview_all_1_to_8$scenario == 4, "#009E73", ifelse(results_df__for_overview_all_1_to_8$scenario == 6, "#2d2e2e",
   ifelse(results_df__for_overview_all_1_to_8$scenario == 7, "#0072B2", 
   ifelse(results_df__for_overview_all_1_to_8$scenario == 8, "#E69F00", "black"))))))))

results_df__for_overview_all_1_to_8$line <- ifelse(results_df__for_overview_all_1_to_8$scenario == 1, 2,ifelse(results_df__for_overview_all_1_to_8$scenario == 3, 2, ifelse(results_df__for_overview_all_1_to_8$scenario == 5, 2,  ifelse(results_df__for_overview_all_1_to_8$scenario == 2, 2, ifelse(results_df__for_overview_all_1_to_8$scenario == 4, 2, ifelse(results_df__for_overview_all_1_to_8$scenario == 6, 2, ifelse(results_df__for_overview_all_1_to_8$scenario == 7, 1, ifelse(results_df__for_overview_all_1_to_8$scenario == 8, 1, 1))))))))

results_df__for_overview_all_1_to_8$linewidth <- ifelse(results_df__for_overview_all_1_to_8$scenario == 1, .8,ifelse(results_df__for_overview_all_1_to_8$scenario == 3, .8, ifelse(results_df__for_overview_all_1_to_8$scenario == 5, .8, ifelse(results_df__for_overview_all_1_to_8$scenario == 2, .8, ifelse(results_df__for_overview_all_1_to_8$scenario == 4, .8, ifelse(results_df__for_overview_all_1_to_8$scenario == 6, .8, ifelse(results_df__for_overview_all_1_to_8$scenario == 7, 2.1, ifelse(results_df__for_overview_all_1_to_8$scenario == 8, 2.11, 1))))))))


df2 = results_df__for_overview_all_1_to_8 %>%
  dplyr::filter(scenario == 7)%>%
  data.frame()

df3 = results_df__for_overview_all_1_to_8 %>%
  dplyr::filter(scenario == 8)%>%
  data.frame()

results_df__for_overview_all_1_to_6 = results_df__for_overview_all %>%
  dplyr::filter(scenario %in% c(1:6))

(plot_trends_BOC <- ggplot(data = results_df__for_overview_all_1_to_8)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_8, aes(x = dateRi, y = BOC, group = as.factor(colour_for_plot)#, #lty =  as.factor(line), lwd = as.factor(linewidth) 
                ), 
                se=FALSE,  formula = my.formula, show.legend = FALSE, colour = "grey", lwd = .5
                 )+ 
   
  new_scale_colour() + # requires library(ggnewscale)
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "k1 minimum (0.0000001)")), 
             data = df2,# alpha = 0.4, 
                se=FALSE,  formula = my.formula) +
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "k1 maximum (1)")),            data = df3, #alpha = 0.4
                se=FALSE,  formula = my.formula) +
#  scale_colour_discrete(
 #   name = ""
  #) +

    #labs(x = "Date", y = "BOC [mol COD / L]") +
    labs(x = "Date", y = "BOC [mol COD L<sup>-1</sup>]")+ 

    theme(panel.background = element_rect(fill = "white",  colour = "black", 
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )
)


if (unified_axes == 1) {
  
  (plot_trends_BOC <- plot_trends_BOC +
     facet_wrap(.~group_letter, ncol = 4))
  
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_7_8_one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_7_8_one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_BOC <- plot_trends_BOC+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_7_8_free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_7_8_free_y_scale.pdf"), width = 8, height = 3)
}


    

(plot_trends_MO <- ggplot(data = results_df__for_overview_all_1_to_8)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_8, aes(x = dateRi, y = MO_het, group = as.factor(colour_for_plot)#, #lty =  as.factor(line), lwd = as.factor(linewidth) 
                ), 
                se=FALSE,  formula = my.formula, show.legend = FALSE, colour = "grey", lwd = .5
                 )+ 
   
  new_scale_colour() + # requires library(ggnewscale)
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "k1 minimum (0.0000001)")), 
             data = df2,# alpha = 0.4, 
                se=FALSE,  formula = my.formula) +
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "k1 maximum (1)")),            data = df3, #alpha = 0.4
                se=FALSE,  formula = my.formula) +
  scale_colour_discrete(
    name = ""
  ) +

    #labs(x = "Date", y = "BOC [mol COD / L]") +
    labs(x = "Date", y = "BOC [mol COD L<sup>-1</sup>]")+ 

    theme(panel.background = element_rect(fill = "white",  colour = "black", 
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )+
    #labs(x = "Date", y = "Microorganisms [mol COD / L]") +
    labs(x = "Date", y = "Microbial dry mass [mol COD L<sup>-1</sup>]")
)

if (unified_axes == 1) {
  
  (plot_trends_MO <- plot_trends_MO +
     facet_wrap(.~group_letter, ncol = 4))
  
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6_7_8_one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6_7_8_one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_MO <- plot_trends_MO+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6_7_8_free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6_7_8_free_y_scale.pdf"), width = 8, height = 3)
}

 
 (plot_trends_fauna <- ggplot(data = results_df__for_overview_all_1_to_8)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_8, aes(x = dateRi, y = fauna, group = as.factor(colour_for_plot)#, #lty =  as.factor(line), lwd = as.factor(linewidth) 
                ), 
                se=FALSE,  formula = my.formula, show.legend = FALSE, colour = "grey", lwd = .5
                 )+ 
   
  new_scale_colour() + ## requires library(ggnewscale)
  geom_smooth(method = "lm",
   (aes(x = dateRi, y = BOC, colour = "k1 minimum (0.0000001)")), 
             data = df2,# alpha = 0.4, 
                se=FALSE,  formula = my.formula) +
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "k1 maximum (1)")),             data = df3, #alpha = 0.4
                se=FALSE,  formula = my.formula) +
  scale_colour_discrete(
    name = ""
  ) +

    theme(panel.background = element_rect(fill = "white",  colour = "black", 
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )+
    #labs(x = "Date", y = "Fauna [mol COD / L]") +
    labs(x = "Date", y = "Fauna dry mass [mol COD L<sup>-1</sup>]")
 )

if (unified_axes == 1) {
  
  (plot_trends_fauna <- plot_trends_fauna +
     facet_wrap(.~group_letter, ncol = 4))
  
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_7_8_one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_7_8_one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_fauna <- plot_trends_fauna+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_7_8_free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_7_8_free_y_scale.pdf"), width = 8, height = 3)
}




######
#trend plots with the extreme Yac
######

#the code from here requires reading in the results as in the block marked for outcommenting above 
setwd(gw_FuldaEcosystemServices_results_txt_path)
results_df__for_overview_all_1_to_6_9_10 <- read.table("results_df__for_overview_all__1_2_3_4_5_6_7_8_9_10.txt", header = TRUE) %>%
dplyr::filter(scenario %in% c(1:6,9,10))


results_df__for_overview_all_1_to_6_9_10$dateRi <- as.Date(results_df__for_overview_all_1_to_6_9_10$dateRi)

results_df__for_overview_all_1_to_6_9_10$group_letter <- factor(results_df__for_overview_all_1_to_6_9_10$group_letter , levels = c("R", "M", "P", "A"))

#TODO not needed, correct?
#results_df__for_overview_all_long <- results_df__for_overview_all %>%
#  dplyr::group_by(dateRi, group_letter, run) %>%
#  tidyr::pivot_longer(cols = c(DETRITUS, BOC, MO_het, fauna), names_to = "variable", values_to = "value")


#colors chosen according to https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
#colours of runs 1 to 6 in grey
results_df__for_overview_all_1_to_6_9_10$colour_for_plot <- 
ifelse(results_df__for_overview_all_1_to_6_9_10$run == 1, "#1a1b1a",
ifelse(results_df__for_overview_all_1_to_6_9_10$run == 3, "#2c2c2c", 
ifelse(results_df__for_overview_all_1_to_6_9_10$run == 5, "#353636",
 ifelse(results_df__for_overview_all_1_to_6_9_10$run == 2, "#373838", 
ifelse(results_df__for_overview_all_1_to_6_9_10$run == 4, "#009E73", 
ifelse(results_df__for_overview_all_1_to_6_9_10$run == 6, "#2d2e2e",
   ifelse(results_df__for_overview_all_1_to_6_9_10$run == 7, "#0072B2", 
   ifelse(results_df__for_overview_all_1_to_6_9_10$run == 8, "#E69F00", "black"))))))))

results_df__for_overview_all_1_to_6_9_10$line <- ifelse(results_df__for_overview_all_1_to_6_9_10$run == 1, 2,ifelse(results_df__for_overview_all_1_to_6_9_10$run == 3, 2, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 5, 2,  ifelse(results_df__for_overview_all_1_to_6_9_10$run == 2, 2, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 4, 2, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 6, 2, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 7, 1, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 8, 1, 1))))))))


results_df__for_overview_all_1_to_6_9_10$linewidth <- ifelse(results_df__for_overview_all_1_to_6_9_10$run == 1, .8,ifelse(results_df__for_overview_all_1_to_6_9_10$run == 3, .8, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 5, .8, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 2, .8, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 4, .8, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 6, .8, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 7, 1, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 8, 1.1, 1))))))))

df2 = results_df__for_overview_all_1_to_6_9_10 %>%
  dplyr::filter(run == 9)%>%
  data.frame()

df3 = results_df__for_overview_all_1_to_6_9_10 %>%
  dplyr::filter(run == 10)%>%
  data.frame()

results_df__for_overview_all_1_to_6 = results_df__for_overview_all %>%
  dplyr::filter(run %in% c(1:6))

(plot_trends_BOC <- ggplot(data = results_df__for_overview_all_1_to_6_9_10)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_6_9_10, aes(x = dateRi, y = BOC, group = as.factor(colour_for_plot)#, #lty =  as.factor(line), lwd = as.factor(linewidth) 
                ), 
                se=FALSE,  formula = my.formula, show.legend = FALSE, colour = "grey", lwd = .5
                 )+ 
   
  new_scale_colour() +
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "Yield minimum (0.001)")), 
             data = df2,# alpha = 0.4, 
                se=FALSE,  formula = my.formula) +
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "Yield maximum (0.8)")),
             data = df3, #alpha = 0.4
                se=FALSE,  formula = my.formula) +
  scale_colour_discrete(
    name = ""
  ) +

    #labs(x = "Date", y = "BOC [mol COD / L]") +
    labs(x = "Date", y = "BOC [mol COD L<sup>-1</sup>]")+ 

    theme(panel.background = element_rect(fill = "white",  colour = "black", 
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )
)


if (unified_axes == 1) {
  
  (plot_trends_BOC <- plot_trends_BOC +
     facet_wrap(.~group_letter, ncol = 4))
  
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_9_10_one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_9_10_one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_BOC <- plot_trends_BOC+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_9_10_free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_9_10_free_y_scale.pdf"), width = 8, height = 3)
}



(plot_trends_MO <- ggplot(data = results_df__for_overview_all_1_to_6_9_10)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_6_9_10, aes(x = dateRi, y = MO_het, group = as.factor(colour_for_plot)#, #lty =  as.factor(line), lwd = as.factor(linewidth) 
                ), 
                se=FALSE,  formula = my.formula, show.legend = FALSE, colour = "grey", lwd = .5
                 )+ 
   
  new_scale_colour() +
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "Yield minimum (0.001)")), 
             data = df2,# alpha = 0.4, 
                se=FALSE,  formula = my.formula) +
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "Yield maximum (0.8)")),
             data = df3, #alpha = 0.4
                se=FALSE,  formula = my.formula) +
  scale_colour_discrete(
    name = ""
  ) +

    #labs(x = "Date", y = "BOC [mol COD / L]") +
    labs(x = "Date", y = "BOC [mol COD L<sup>-1</sup>]")+ 

    theme(panel.background = element_rect(fill = "white",  colour = "black", 
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )+
    #labs(x = "Date", y = "Microorganisms [mol COD / L]") +
    labs(x = "Date", y = "Microbial dry mass [mol COD L<sup>-1</sup>]")
)

if (unified_axes == 1) {
  
  (plot_trends_MO <- plot_trends_MO +
     facet_wrap(.~group_letter, ncol = 4))
  
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6_9_10_one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6_9_10_one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_MO <- plot_trends_MO+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6_9_10_free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_MO_1_2_3_4_5_6_9_10_free_y_scale.pdf"), width = 8, height = 3)
}

 
 (plot_trends_fauna <- ggplot(data = results_df__for_overview_all_1_to_6_9_10)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_6_9_10, aes(x = dateRi, y = fauna, 
                group = as.factor(colour_for_plot)#, #lty =  as.factor(line), lwd = as.factor(linewidth) 
                ), 
                se=FALSE,  formula = my.formula, show.legend = FALSE, colour = "grey", lwd = .5
                 )+ 

    new_scale_colour() +
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "Yield minimum (0.001)")), 
             data = df2,# alpha = 0.4, 
                se=FALSE,  formula = my.formula) +
  geom_smooth(method = "lm",
    (aes(x = dateRi, y = BOC, colour = "Yield maximum (0.8)")),
             data = df3, #alpha = 0.4
                se=FALSE,  formula = my.formula) +
  scale_colour_discrete(
    name = ""
  ) +

    theme(panel.background = element_rect(fill = "white",  colour = "black", 
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )+
    #labs(x = "Date", y = "Fauna [mol COD / L]") +
    labs(x = "Date", y = "Fauna dry mass [mol COD L<sup>-1</sup>]")
 )

if (unified_axes == 1) {
  
  (plot_trends_fauna <- plot_trends_fauna +
     facet_wrap(.~group_letter, ncol = 4))
  
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_9_10_one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_9_10_one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_fauna <- plot_trends_fauna+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_9_10_free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_9_10_free_y_scale.pdf"), width = 8, height = 3)
}
