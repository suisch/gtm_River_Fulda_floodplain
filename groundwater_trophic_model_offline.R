#Load required packages
library(dplyr)
library(tidyr)
library(openxlsx)#read.xlsx
library(DescTools)#year()
library(patchwork) #for sticking together different plots 
library(ggpubr)#ggarrange. 
library(cowplot)
library(colorspace)

#install package for color blind-safe plots
# install.packages("ggokabeito")
# #You can alternatively install the development version of ggokabeito from GitHub with:
#   devtools::install_github("malcolmbarrett/ggokabeito")
library(ggokabeito) # #scale_fill_okabe_ito
library(ggplot2)
#install.packages("ggtext") #
library(ggtext)#for exponents in ylab in ggplot


rm(list = ls()) #remove any variables and data created before, to make sure that this runs with fresh data

##########
#the runs with the different parameters are listed in the excel file "parameters.xlsx"; 1 to 8. Change accordingly
##########
run <- 1

#TODO define these paths for saving your result text files locally
#TODO offline see below within block read-in depending on operating system
# gw_FuldaEcosystemServices_plots_path <-""
# gw_FuldaEcosystemServices_results_txt_path <-""
# #e.g.
# gw_FuldaEcosystemServices_plots_path <-"D:"
# gw_FuldaEcosystemServices_results_txt_path <-"D:"

##########
#preparing read-in of data
##########

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

whichosrightnow <- get_os()

extHD_on_UFZ <- TRUE

if(whichosrightnow == "windows") {
  if(extHD_on_UFZ){
    
    gw_FuldaEcosystemServices_results_txt_path <-"G:\\freefilesyncHBU\\projects\\gw_ecosystem_services_Fulda_plain\\results\\txt"
    gw_FuldaEcosystemServices_plots_path <-"G:\\freefilesyncHBU\\projects\\gw_ecosystem_services_Fulda_plain\\results\\plots"
    
    setwd("G:\\freefilesyncHBU\\projects\\git\\gw_ecosystem_services\\Fulda_plains\\R_git\\gtm_River_Fulda_floodplain\\gtm_River_Fulda_floodplain")
    
  }else{
    
    gw_FuldaEcosystemServices_results_txt_path <-"d:\\work\\projects\\gw_ecosystem_services_Fulda_plain\\results\\txt"
    gw_FuldaEcosystemServices_plots_path <-"d:\\work\\projects\\gw_ecosystem_services_Fulda_plain\\results\\plots"
    
    setwd("d:\\work\\projects\\git\\gw_ecosystem_services\\Fulda_plains\\R_git\\gtm_River_Fulda_floodplain\\gtm_River_Fulda_floodplain")
  }  
}else{
  gw_FuldaEcosystemServices_plots_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain/results/plots"
  gw_FuldaEcosystemServices_results_txt_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain/results/txt"
  
  
  setwd("/Users/susanneschmidt/Documents/head/Arbeit/projects/git/gw_ecosystem_services/Fulda_plains/R_git/gtm_River_Fulda_floodplain/gtm_River_Fulda_floodplain")
  
  gw_FuldaEcosystemServices_plots_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain/results/plots"
  gw_FuldaEcosystemServices_results_txt_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain/results/txt"
  
}

source("gtm.R")
#TODO should be separate folder, as it was   setwd("/Users/susanneschmidt/Documents/head/Arbeit/projects/git/gw_ecosystem_services/Fulda_plains/R_git/functions")
source("River_Fulda_floodplain_data_offline.R") #required for parameters variables
source("River_Fulda_floodplain_parameters_offline.R")            

# urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_prec_plot.R"
#source(urlfiletext)
source("River_Fulda_floodplain_prec_plot.R") #function Fulda_prec_plot


#urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_data.R"
#source(urlfiletext)  ##fulda_variables

#urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_parameters.R"
#source(urlfiletext)

########
#functions reactions
#see above urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/model_functions.R"
#source(urlfiletext)
source("model_functions.R")


#TODO the following block can be outcommented?
#done in fulda_data_variables Fulda_daily_temp <- read.table("produkt_tu_termin_18850101_20231231_01526.txt" ,sep = ";", header = TRUE)  #needed for Fulda_variables
#TODO now read in with fulda_variables
#TODO done above #urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/Fulda_daily_temp_joh_long.txt"
#Fulda_daily_temp_joh_long <- read.table(urlfiletext,sep = " ", header = TRUE)  
#Fulda_daily_temp_joh_long <- read.table( "Fulda_daily_temp_joh_long.txt", header = TRUE) #written in model_concrete_rates_with_Fulda_a_la_Soetaert_2025_06_30_gw_temperature_extrapolation in /Users/susanneschmidt/Documents/head/Arbeit/projects/git/gw_ecosystem_services/Fulda_plains/R_git
#Fulda_daily_temp_joh_long$dateRi <- as.Date(Fulda_daily_temp_joh_long$dateRi)

#need to update
urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/error_measures.R"
source(urlfiletext)
source("error_measures.R")

##########
#parameters
##########
#parameters from excel file - crucial for reading in the data with the respective temperature scenario
#see above urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/parameters.xlsx"
#parvar <- read.xlsx(urlfiletext, startRow = 3, sheet = 1)
##fulda_variables TODO offline
parvar <- read.xlsx("parameters.xlsx", startRow = 3, sheet = 1)  


##########
#reading in Fulda data from file
##########

scenario_with_1_or_without_0_MO <-  parvar$scenario_with_1_or_without_0_MO[run]  
scenario_with_1_or_without_0_fauna <- parvar$scenario_with_1_or_without_0_fauna[run]  

factor_CC_MO <-  parvar$factor_CC_MO[run]
factor_CC_fauna <-  parvar$factor_CC_fauna[run]

#this depends on the two scenario variables scenario_with_1_or_without_0_MO and scenario_with_1_or_without_0_fauna
fulda_variables_read_in <- fulda_variables(run, factor_CC_MO, factor_CC_fauna)
names(fulda_variables_read_in) <- c("Fulda_daily_prec", "Fulda_daily_temp_", "chem_ordered_per_date_1978_1981", "chem_ordered_per_date_1978_1981_mean_per_group", "fauna_deep_PerSamplPerTaxonWide_bm_sum", "fauna_deep_PerSamplPerTaxon_bm_mean_per_group", "t_0", "t_max", "DETRITUS_gr1_t0", "DETRITUS_gr2_t0", "DETRITUS_gr3_t0", "DETRITUS_gr4_t0", "BOC_gr1_t0", "BOC_gr2_t0", "BOC_gr3_t0", "BOC_gr4_t0", "MO_het_gr1_t0", "MO_het_gr2_t0", "MO_het_gr3_t0", "MO_het_gr4_t0", "fauna_gr1_t0", "fauna_gr2_t0", "fauna_gr3_t0", "fauna_gr4_t0",  "CC_table_MO", "CC_table_fauna", "Fulda_daily_temp_joh_long") 

list2env(fulda_variables_read_in, globalenv())

#read in chemical data; prepare for join
chem_ordered_per_date_1978_1981$group <- chem_ordered_per_date_1978_1981$kmeans4gr
chem_ordered_per_date_1978_1981$group_letter <- ifelse(chem_ordered_per_date_1978_1981$group ==  2, "P", ifelse(chem_ordered_per_date_1978_1981$group == 3, "R" , ifelse (chem_ordered_per_date_1978_1981$group == 4, "A", ifelse (chem_ordered_per_date_1978_1981$group == 1, "M", NA))))   
chem_ordered_per_date_1978_1981$group_letter <- factor(chem_ordered_per_date_1978_1981$group_letter , levels = c("R", "M", "P", "A"))

#read parameters  from file. requires Fulda_daily_prec and thus, needs to be run after the data are read in

#read parameters variables  from file
parameters_read_in <- read_parameters(run)
names(parameters_read_in) <- c("delta_t", "max_t", "aquifer_depth", "import_MO_het", "scenario_with_1_or_without_0_fauna", "scenario_with_1_or_without_0_MO", "mortalityRate", "import_fauna", "yield_ac", "yield_MO", "K_MO_at_temp", "rMO_BOC_uptake_per_day_at_lab_temperature", "rFauna_MO_uptake_per_day_at_TEMP", "k1", "excretionRate", "TOC_COD_mol_m2_yr_precipitation", "RECHARGE_COD_mol_per_m3_per_day_df", "lab_temp", "K_ac", "growth_model_MO_type", "growth_model_fauna_type", "mortalityFraction_per_degree", "microbe_loss_factor_when_no_fauna") 

list2env(parameters_read_in, globalenv())

if(is.na(max_t)){
  t_max = t_max
}else{
  t_max = max_t #set another end date than the one in the Fulda study; max_t is read in from parameters
}

##########
#creating temperature scenarios
##########

temperature_scenario <- parvar$temperature_scenario[run]

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
  dplyr::summarise(BOC_mol_COD_L = mean(BOC_mol_COD_L, na.rm = TRUE)) %>%
  dplyr::mutate(ID = paste(Date, kmeans4gr, sep = "_"))%>%
  dplyr::mutate(x = BOC_mol_COD_L)
#TODO the ID not needed when the more generic join is used
#results_df_ <- results_df %>%
#dplyr::mutate(ID = paste(dateRi, group, sep = "_"))%>%
#dplyr::mutate(y = BOC)

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

fauna_deep_PerSamplPerTaxonWide_bm_sum_no_high_biomass  <- fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
  dplyr::filter(bm_mol_COD_perL < 0.00001) #for visualization - explained in text and caption
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
#container for results of the model fit
##########


error_measures_data_table <- data.frame(run = NA, group = NA, variable = NA, R2 = NA, MAE = NA, RMSE = NA, MB = NA, NSE = NA, N = NA)

##########
#reading in Fulda precitiation plot - does not depend on model variables, but depends on Fulda_daily_prec in fulda_variables_read_in, and thus, cannot be read in earlier than this
##########

#urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_prec_plot.R"
#source(urlfiletext)

#TODO done above for offline

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

#TODO for just the modelling, it is sufficient to do this here. However, for the function and sensitivity modelling, it needs to be done earlier ?!
#chem_ordered_per_date_1978_1981_BOC_joined <- left_join(chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date, results_df, by =  c("Date" = "dateRi", "kmeans4gr" = "group"))
results_joined <- dplyr::left_join(results, chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date, by =  c( "dateRi" = "Date" , "group" = "kmeans4gr"  ))
#TODO 29.11. hang on - this is now superflusous because I have constructed the results file from - no, from meteo - I do need to join the BOC data - or do I do that from the start ?! for SA yes

#calling the function of the time step wise groundwater trophic model (gtm) 
#the following for loop is largely consistent with v. 1 . In v. 2 the for loop here can be replaced by a call to the function gtm, see below the loop 

results <- gtm()

#ggplot requires the data to be in data frame
results_df <- as.data.frame(results)

#saving this run's data for later use
setwd(gw_FuldaEcosystemServices_results_txt_path) 
write.table(results_df, paste0("results_df_run_",run,"_2025_12_03.txt"), row.names = FALSE)

#for plotting several variables, make long form of the results data frame
results_df_long <- results_df %>%
  tidyr::pivot_longer(cols = c(BOC, DETRITUS, MO_het, fauna), names_to = "variable") 
write.table(results_df, paste0("results_df_long_run_",run,"_2025_12_03.txt"), row.names = FALSE)



#this block is for read in data - outcomment this block for fresh data !
#in order to produce plots which compare the runs, read the run results from files saved previously
setwd(gw_FuldaEcosystemServices_results_txt_path)
#TODO with date ?!
results_df_1 <- read.table( "results_df_run_1_2025_12_03.txt", header = TRUE)
results_df_2 <- read.table( "results_df_run_2_2025_12_03.txt", header = TRUE)
results_df_3 <- read.table( "results_df_run_3_2025_12_03.txt", header = TRUE)
results_df_4 <- read.table( "results_df_run_4_2025_12_03.txt", header = TRUE)
results_df_5 <- read.table( "results_df_run_5_2025_12_03.txt", header = TRUE)
results_df_6 <- read.table( "results_df_run_6_2025_12_03.txt", header = TRUE)
results_df_7 <- read.table( "results_df_run_7_2025_12_03.txt", header = TRUE)
results_df_8 <- read.table( "results_df_run_8_2025_12_03.txt", header = TRUE)

results_df_1$run <- 1
results_df_2$run <- 2
results_df_3$run <- 3
results_df_4$run <- 4
results_df_5$run <- 5
results_df_6$run <- 6
results_df_7$run <- 7
results_df_8$run <- 8

#if using one of the saved ones for producing the  figures below, prepare the respective data, by replacing xx with the number of the run
# results_df <- results_df_xx
# results_df$dateRi <- as.Date(results_df$dateRi)
# results_df$group_letter <- factor(results_df$group_letter , levels = c("R", "M", "P", "A"))
# run <- xx

# e.g. for plotting from previously saved file
run <- 1
results_df <- results_df_1
results_df$dateRi <- as.Date(results_df$dateRi)
results_df$group_letter <- factor(results_df$group_letter , levels = c("R", "M", "P", "A"))
#END block read in data - outcomment for fresh data !


unified_axes <- 1 # 1 = make the same axis for all four subplots , representing the four groups . 0 = axes reflect the groups' minima and maxima

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

geomtexttable_DETRITUS$x <- as.Date(geomtexttable_DETRITUS$x)
geomtexttable_DETRITUS$y <- as.numeric(geomtexttable_DETRITUS$y)
geomtexttable_DETRITUS$y_R2 <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, DETR_1978_1981_mean_per_group_and_date_joined$DETRITUS, na.rm = TRUE)*.05
geomtexttable_DETRITUS$y_MAE <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, na.rm = TRUE)*.15#.025
geomtexttable_DETRITUS$y_RMSE <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, na.rm = TRUE)*.35#.05
geomtexttable_DETRITUS$y_NSE <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, na.rm = TRUE)*.55#.075
geomtexttable_DETRITUS$y_MB <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, na.rm = TRUE)*.75#.1
geomtexttable_DETRITUS$y_N <- geomtexttable_DETRITUS$y-max(DETR_1978_1981_mean_per_group_and_date_joined$OS_mol_COD_L, na.rm = TRUE)*.9#.125
geomtexttable_DETRITUS$R2 <- paste("R2 =", geomtexttable_DETRITUS$R2)
geomtexttable_DETRITUS$MAE <- paste("MAE =", geomtexttable_DETRITUS$MAE)
geomtexttable_DETRITUS$RMSE <- paste("RMSE =", geomtexttable_DETRITUS$RMSE)
geomtexttable_DETRITUS$NSE <- paste("NSE =", geomtexttable_DETRITUS$NSE)
geomtexttable_DETRITUS$MB <- paste("MB =", geomtexttable_DETRITUS$MB)
geomtexttable_DETRITUS$N <- paste("N =", geomtexttable_DETRITUS$N)


# ylablist <- list( bquote( paste( "Detritus") ) ,
#                bquote( paste( "[mol COD  ", L^{-1}, " ]" ) ) ,
#                bquote(paste("measured [o] and")),
#                bquote("modelled [.]")
#                )

Fulda_Detritus_partOrganics_plot <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(OS_mol_COD_L))%>%
  ggplot() +
  geom_point(data = results_df, aes(x = dateRi, y = DETRITUS, colour = as.factor(group_letter)),  pch = 16, size = 0.5, show.legend = FALSE 
  )+
  geom_point(aes(x = Date, y = OS_mol_COD_L, fill = as.factor(group_letter)),  pch = 21 , colour = "black", show.legend = FALSE
  )+
  scale_x_date(limits = c(t_0, t_max) )+
  #labs(x = "Date", y = "Detritus\n [mol COD/ L]\n measured [o] and\nmodelled [.]")+ 
  #cannot combine expression(paste and \n labs(x = "Date", y = expression(paste("Detritus\n [mol COD  ", L^{-1}, " ]\n measured [o] and\n modelled [.]")))+
  #error labs(x = "Date", y = parse("Detritus\n [mol~COD~L^{-1}~]*\nmeasured~[*o*]~and\nmodelled~[*.*]"))+
#\n in "" does not do anything and \n without "" produces error  labs(x = "Date", y = expression(Detritus*"\n"~"["*mol~COD~L^{"-1"}~"]"~\nmeasured~"["*o*"]"~and~modelled~"["*.*"]"))+
  #works but each consecutive line becomes smaller because they need to fit into the same space  labs(x = "Date", y = expression(atop("Detritus",atop("[mol COD "*L^{-1}*"]",atop("measured [o] and"," modelled [.]")))))+
  #only prints the first line  labs(x = "Date", y = do.call(expression, ylablist))+ #https://stackoverflow.com/questions/18237134/line-break-in-expression/18237295#18237295
  #labs(x = "Date", y = "Detritus\n [mol COD  L<sup>-1</sup>]\n measured [o] and\n modelled [.]")+ # https://forum.posit.co/t/exponent-numbers-in-ggplot-labels/171969/2 library ggtext
  #labs(x = "Date", y = "Detritus [mol COD  L<sup>-1</sup>] measured [o] and modelled [.]")+ # https://forum.posit.co/t/exponent-numbers-in-ggplot-labels/171969/2 library ggtext
  #cannot deal with line breaks \n
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
max_BOC <- max(chem_ordered_per_date_1978_1981$BOC_mol_COD_L, results$BOC, na.rm =TRUE)

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

my.formula <- y ~ x

#in the sensitivity analysis version of this file, this join is done before the sensitivity analysis
results_joined <- dplyr::left_join(results, chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date, by =  c( "dateRi" = "Date" , "group" = "kmeans4gr"  ))

max_BOC <- max(chem_ordered_per_date_1978_1981$BOC_mol_COD_L, results_joined$BOC, na.rm =TRUE)



#make a table with 4x the same coordinates x, y, and for the label: the column BOC
geomtexttable_BOC <- as.data.frame(cbind("group" = error_measures_data_BOC_groups$group, "x" = rep("1980-04-01",4), "y" = rep(max_BOC*.95, 4),
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
  #labs(x = "Date", y = "BOC\n [mol COD / L]\nmeasured [o] and\nmodelled [.]")+ 
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


(Fulda_BOC_plot_trends <- Fulda_BOC_plot +
    geom_smooth(method = "lm",
                data = results_df, aes(x = dateRi, y = BOC),
                se=TRUE,  formula = my.formula, lwd = 0.3) 
) 




#combine measured data with modelled data to calculate model accuracy - for that, join model results to measured values for microbial numbers
MO_het_1978_1981_mean_per_group_and_date_joined <- left_join(MO_het_1978_1981_mean_per_group_and_date, results_df, by =  c("Date" = "dateRi", "kmeans4gr" = "group"))

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

geomtexttable_MO <- as.data.frame(cbind("group" = error_measures_data_MO_het_groups$group, "x" = rep("1980-04-01",4), "y" = rep(max(chem_ordered_per_date_1978_1981$MO_het_mol_COD_L, results_df$MO_het)*.95, 4),
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
  #labs(x = "Date", y = "Fauna dry mass\n[mol COD / L]\nmeasured [o] and\nmodelled [.]")+ 
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

if (unified_axes == 1) {
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
  ggsave(paste0("model_measured_run_",run,"_with_MO_one_y_scale_w_model_qual_2025_12_03.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_run_",run,"_with_MO_one_y_scale_w_model_qual_2025_12_03.pdf"), width = 10, height = 9)
}else{
  #save as png AND as pdf
  ggsave(paste0("model_measured_run_",run,"_with_MO_free_y_scale_w_model_qual_2025_12_03.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_run_",run,"_with_MO_free_y_scale_w_model_qual_2025_12_03.pdf"), width = 10, height = 9)
}


(Fulda_trends <- Fulda_prec_plotted +  Fulda_Detritus_partOrganics_plot_trends  + Fulda_BOC_plot_trends + Fulda_MO_plot_trends + Fulda_fauna_plot_trends + plot_layout(ncol = 1) +
    plot_annotation(tag_levels = "a", tag_suffix = ")"))

setwd(gw_FuldaEcosystemServices_plots_path)
if (unified_axes == 1) {
  ggsave(paste0("model_measured_trend_run_",run,"_with_MO_one_y_scale_w_model_qual_2025_12_03.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_trend_run_",run,"_with_MO_one_y_scale_w_model_qual_2025_12_03.pdf"), width = 10, height = 9)
}else{
  ggsave(paste0("model_measured_trend_run_",run,"_with_MO_free_y_scale_w_model_qual_2025_12_03.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_trend_run_",run,"_with_MO_free_y_scale_w_model_qual_2025_12_03.png"), width = 10, height = 9)
}



#the code from here requires reading in the results as in the block marked for outcommenting above 
results_df__for_overview_all <- rbind(results_df_1, results_df_2, results_df_3, results_df_4, results_df_5, results_df_6)


setwd(gw_FuldaEcosystemServices_results_txt_path)

write.table(results_df__for_overview_all, "results_df__for_overview_all_for_barplot_1_2_3_4_5_6___2025_12_03.txt", row.names = FALSE)

results_df__for_overview_all$dateRi <- as.Date(results_df__for_overview_all$dateRi)

results_df__for_overview_all$group_letter <- factor(results_df__for_overview_all$group_letter , levels = c("R", "M", "P", "A"))

#TODO now redundant ?! names(results_df__for_overview_all) <- sub("value", "value_from_merge", names(results_df__for_overview_all))
results_df__for_overview_all_long <- results_df__for_overview_all %>%
  dplyr::group_by(dateRi, group_letter, run) %>%
  tidyr::pivot_longer(cols = c(DETRITUS, BOC, MO_het, fauna), names_to = "variable", values_to = "value")

#calculate trends 
lm_i_df <- lm_i_list <- NULL
i <- j <- 1 

for(i in c(1:length(unique(results_df__for_overview_all_long$run)))){
  
  for(j in c(1:length(unique(results_df__for_overview_all_long$group_letter)))){
    
    for(k in c(1:length(unique(results_df__for_overview_all_long$variable)))){
      
      run_i <- unique(results_df__for_overview_all_long$run)[i]
      group_letter_j <- unique(results_df__for_overview_all_long$group_letter)[j]
      variable_k <- unique(results_df__for_overview_all_long$variable)[k]
      
      results_df__for_overview_all_long_i <- results_df__for_overview_all_long %>%
        dplyr::filter(run == run_i  & group_letter == group_letter_j &
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
      lm_i_df$run <- run_i
      lm_i_df$group_letter <- group_letter_j
      lm_i_df$variable <- variable_k
      
      lm_i_df$max_ <- max(results_df__for_overview_all_long_i$value, na.rm = TRUE)
      lm_i_df$conc_t_0 <- results_df__for_overview_all_long_i$value[1]
      lm_i_df$conc_t_max <- results_df__for_overview_all_long_i$value[length(results_df__for_overview_all_long_i$value)]
      lm_i_df$fitted_conc_t_0 <- lm_i$fitted.values[1]
      lm_i_df$fitted_conc_t_max <- lm_i$fitted.values[length(lm_i$fitted.values)]
      lm_i_df$fitted_diff_over_observation_period <- lm_i$fitted.values[length(lm_i$fitted.values)]-lm_i$fitted.values[1]
      lm_i_df$slope_per_year <- lm_i_df$diff_over_observation_period/lm_i_df$time_span
      lm_i_list <- rbind(lm_i_list, as.data.frame(t(unlist(lm_i_df)))) 
    }
  }
}

names(lm_i_list) <- sub("diff_over_observation_period.1342", "fitted_diff_over_observation_period", names(lm_i_list) )
names(lm_i_list) <- sub("fitted_fitted_diff_over_observation_period", "fitted_diff_over_observation_period", names(lm_i_list) )
names(lm_i_list) <- sub("fitted_conc_t_0.1", "fitted_conc_t_0", names(lm_i_list) )
names(lm_i_list) <- sub("fitted_conc_t_max.1342", "fitted_conc_t_max", names(lm_i_list) )

lm_i_list_ <- lm_i_list %>%
  dplyr::mutate(group = ifelse(group_letter==3,"R",ifelse(group_letter==2,"P",ifelse(group_letter==4,"A",ifelse(group_letter==1,"M","z"))))) %>%
  #dplyr::select(c("run", "group", "variable", "coefficient_intercept", "coefficient_slope",  "F_value", "p_val", "fitted_diff_over_observation_period", "conc_t_0", "conc_t_max", "fitted_conc_t_0", "fitted_conc_t_max", "group_letter", "max_"))
  dplyr::select(c("run", "group_letter", "variable", "coefficient_intercept", "coefficient_slope",  "F_value", "p_val", "fitted_diff_over_observation_period", "conc_t_0", "conc_t_max", "fitted_conc_t_0", "fitted_conc_t_max", "group_letter", "max_"))

setwd(gw_FuldaEcosystemServices_results_txt_path)
write.table(lm_i_list_, "results_df__lm_1_2_3_4_5_6__fp_2025_12_03.txt", row.names = FALSE)
#reading in an already existing file with results
# lm_i_list<- read.table( "results_df__lm_1_2_3_4_5_6__fp_2025_12_03.txt", header = TRUE)

#colors chosen according to https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
results_df__for_overview_all$colour_for_plot <- ifelse(results_df__for_overview_all$run == 1, "#009E73",ifelse(results_df__for_overview_all$run == 3, "#0072B2", ifelse(results_df__for_overview_all$run == 5, "#56B4E9", ifelse(results_df__for_overview_all$run == 2, "#F0E442", ifelse(results_df__for_overview_all$run == 4, "#E69F00", ifelse(results_df__for_overview_all$run == 6, "#D55E00", "black"))))))

results_df__for_overview_all$colour_for_plot <- factor(results_df__for_overview_all$colour_for_plot, level = c("#009E73", "#F0E442", "#0072B2",   "#E69F00", "#56B4E9",   "#D55E00"))

results_df__for_overview_all$line <- ifelse(results_df__for_overview_all$run == 1, 1,ifelse(results_df__for_overview_all$run == 3, 2, ifelse(results_df__for_overview_all$run == 5, 3,  ifelse(results_df__for_overview_all$run == 2, 5, ifelse(results_df__for_overview_all$run == 4, 6, ifelse(results_df__for_overview_all$run == 6, 4, 1))))))

results_df__for_overview_all$line <- factor(results_df__for_overview_all$line, level = c(1, 5, 2,  6, 3,   4))

results_df__for_overview_all$linewidth <- ifelse(results_df__for_overview_all$run == 1, 1.2,ifelse(results_df__for_overview_all$run == 3, .82, ifelse(results_df__for_overview_all$run == 5, .31, ifelse(results_df__for_overview_all$run == 2, 1.1, ifelse(results_df__for_overview_all$run == 4, .81, ifelse(results_df__for_overview_all$run == 6, .3, 1))))))

results_df__for_overview_all$linewidth <- factor(results_df__for_overview_all$linewidth, level = c(1.2, 1.1, .82, .81, .31, .3))


override.col <- c( "#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00")
override.line <- c(1, 5, 2, 6, 3, 4)
override.linewidth <- c(1.2, 1.1, .82, .81,  .41,  .4)

(plot_trends_BOC <- ggplot(data = results_df__for_overview_all)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all, aes(x = dateRi, y = BOC, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth) ), se=FALSE,  formula = my.formula )+ 
    scale_colour_manual("Run",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                        labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linetype_manual("Run", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    
    scale_linewidth_manual("Run", values = c(1.2, 1.1, .82, .81,  .41,  .4),
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
  
  (plot_trends_BOC <- plot_trends_BOC +
     facet_wrap(.~group_letter, ncol = 4))
  
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__one_y_scale_2025_12_03.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__one_y_scale_2025_12_03.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_COD <- plot_trends_COD+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__free_y_scale_2025_12_03.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__free_y_scale_2025_12_03.pdf"), width = 8, height = 3)
}



(plot_trends_MO <- ggplot(data = results_df__for_overview_all)+
    
    geom_smooth(method = "lm",
                data = results_df__for_overview_all, aes(x = dateRi, y = MO_het, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth)
                ),
                se=FALSE,  formula = my.formula )+ 
    scale_colour_manual("Run",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                        labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linetype_manual("Run", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    
    scale_linewidth_manual("Run", values = c(1.2, 1.1, .82, .81,  .41,  .4),
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
    )+
    facet_wrap(.~group_letter, scales="free_y", ncol = 4)
)
setwd(gw_FuldaEcosystemServices_plots_path)
ggsave("plot_trends_MO_1_2_3_4_5_6___2025_12_03.png", width = 8, height = 2.5)
ggsave("plot_trends_MO_1_2_3_4_5_6___2025_12_03.pdf", width = 8, height = 2.5)



(plot_trends_fauna <- ggplot(data = results_df__for_overview_all)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all, aes(x = dateRi, y = fauna, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth)  ), 
                se=FALSE,  formula = my.formula )+
    scale_colour_manual("Run",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                        labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linetype_manual("Run", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linewidth_manual("Run", values = c(1.2, 1.1, .82, .81,  .41,  .4),
                           labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    
    guides(colour = guide_legend(override.aes = list(line = override.line, colour = override.col, linewidth = override.linewidth))) +
    
    #labs(x = "Date", y = "Fauna [mol COD / L]") +
    labs(x = "Date", y = "Fauna dry mass [mol COD L<sup>-1</sup>])+ 
    theme(panel.background = element_rect(fill = "white",  colour = "black",  linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
        axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )+
    facet_wrap(.~group_letter, scales="free_y", ncol = 4)
)
setwd(gw_FuldaEcosystemServices_plots_path)
ggsave("plot_trends_fauna_1_2_3_4_5_6___2025_12_03.png", width = 8, height = 2.5)
ggsave("plot_trends_fauna_1_2_3_4_5_6___2025_12_03.pdf", width = 8, height = 2.5)



######
#trend plots with the extreme Yac and K_ac
######
#the code from here requires reading in the results as in the block marked for outcommenting above 
results_df__for_overview_all <- rbind(results_df_1, results_df_2, results_df_3, results_df_4, results_df_5, results_df_6, results_df_7, results_df_8)


setwd(gw_FuldaEcosystemServices_results_txt_path)

write.table(results_df__for_overview_all, "results_df__for_overview_all_for_barplot_1_2_3_4_5_6_7_8__2025_12_03.txt", row.names = FALSE)

results_df__for_overview_all$dateRi <- as.Date(results_df__for_overview_all$dateRi)

results_df__for_overview_all$group_letter <- factor(results_df__for_overview_all$group_letter , levels = c("R", "M", "P", "A"))

#TODO now redundant ?! names(results_df__for_overview_all) <- sub("value", "value_from_merge", names(results_df__for_overview_all))
results_df__for_overview_all_long <- results_df__for_overview_all %>%
  dplyr::group_by(dateRi, group_letter, run) %>%
  tidyr::pivot_longer(cols = c(DETRITUS, BOC, MO_het, fauna), names_to = "variable", values_to = "value")

#calculate trends 
lm_i_df <- lm_i_list <- NULL
i <- j <- 1 

for(i in c(1:length(unique(results_df__for_overview_all_long$run)))){
  
  for(j in c(1:length(unique(results_df__for_overview_all_long$group_letter)))){
    
    for(k in c(1:length(unique(results_df__for_overview_all_long$variable)))){
      
      run_i <- unique(results_df__for_overview_all_long$run)[i]
      group_letter_j <- unique(results_df__for_overview_all_long$group_letter)[j]
      variable_k <- unique(results_df__for_overview_all_long$variable)[k]
      
      results_df__for_overview_all_long_i <- results_df__for_overview_all_long %>%
        dplyr::filter(run == run_i  & group_letter == group_letter_j &
                        variable == variable_k) 
      
      lm_i <-   lm(results_df__for_overview_all_long_i$value ~  results_df__for_overview_all_long_i$dateRi) 
      lm_i_df$coefficient_intercept <- lm_i$coefficients[1][[1]]
      lm_i_df$coefficient_slope <- lm_i$coefficients[2][[1]]
      
      if(length(anova(lm_i)$F_value[1][[1]] > 0)) {
        lm_i_df$F_value <- anova(lm_i)$F_value[1][[1]]
      }else{
        lm_i_df$F_value <- "NA"
      }
      lm_i_df$p_val <- anova(lm_i)$"Pr(>F)"[1]
      lm_i_df$run <- run_i
      lm_i_df$group_letter <- group_letter_j
      lm_i_df$variable <- variable_k
      
      lm_i_df$max_ <- max(results_df__for_overview_all_long_i$value, na.rm = TRUE)
      lm_i_df$conc_t_0 <- results_df__for_overview_all_long_i$value[1]
      lm_i_df$conc_t_max <- results_df__for_overview_all_long_i$value[length(results_df__for_overview_all_long_i$value)]
      lm_i_df$fitted_conc_t_0 <- lm_i$fitted.values[1]
      lm_i_df$fitted_conc_t_max <- lm_i$fitted.values[length(lm_i$fitted.values)]
      lm_i_df$fitted_diff_over_observation_period <- lm_i$fitted.values[length(lm_i$fitted.values)]-lm_i$fitted.values[1]
      lm_i_df$slope_per_year <- lm_i_df$diff_over_observation_period/lm_i_df$time_span
      lm_i_list <- rbind(lm_i_list, as.data.frame(t(unlist(lm_i_df)))) 
    }
  }
}

names(lm_i_list) <- sub("diff_over_observation_period.1342", "fitted_diff_over_observation_period", names(lm_i_list) )
names(lm_i_list) <- sub("fitted_fitted_diff_over_observation_period", "fitted_diff_over_observation_period", names(lm_i_list) )
names(lm_i_list) <- sub("fitted_conc_t_0.1", "fitted_conc_t_0", names(lm_i_list) )
names(lm_i_list) <- sub("fitted_conc_t_max.1342", "fitted_conc_t_max", names(lm_i_list) )

lm_i_list_ <- lm_i_list %>%
  dplyr::mutate(group = ifelse(group_letter==3,"R",ifelse(group_letter==2,"P",ifelse(group_letter==4,"A",ifelse(group_letter==1,"M","z"))))) %>%
  #dplyr::select(c("run", "group", "variable", "coefficient_intercept", "coefficient_slope",  "F_value", "p_val", "fitted_diff_over_observation_period", "conc_t_0", "conc_t_max", "fitted_conc_t_0", "fitted_conc_t_max", "group_letter", "max_"))
  dplyr::select(c("run", "group_letter", "variable", "coefficient_intercept", "coefficient_slope",  "F_value", "p_val", "fitted_diff_over_observation_period", "conc_t_0", "conc_t_max", "fitted_conc_t_0", "fitted_conc_t_max", "group_letter", "max_"))

setwd(gw_FuldaEcosystemServices_results_txt_path)
write.table(lm_i_list_, "results_df__lm_1_2_3_4_5_6_7_8__fp_2025_12_03.txt", row.names = FALSE)
# lm_i_list<- read.table( "results_df__lm_1_2_3_4_5_6__fp_2025_12_03.txt", header = TRUE)

#colors chosen according to https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
#colours of runs 1 to 6 in grey
results_df__for_overview_all$colour_for_plot <- ifelse(results_df__for_overview_all$run == 1, "#1a1b1a",ifelse(results_df__for_overview_all$run == 3, "#2c2c2c", ifelse(results_df__for_overview_all$run == 5, "#353636", ifelse(results_df__for_overview_all$run == 2, "#373838", ifelse(results_df__for_overview_all$run == 4, "#009E73", ifelse(results_df__for_overview_all$run == 6, "#2d2e2e",
   ifelse(results_df__for_overview_all$run == 7, "#0072B2", 
   ifelse(results_df__for_overview_all$run == 8, "#E69F00", "black"))))))))

#does not work results_df__for_overview_all$colour_for_plot <- factor(results_df__for_overview_all$colour_for_plot, level = c("#2f3030" ,"#2f3030" ,"#2f3030" ,"#2f3030" ,"#2f3030" ,"#2f3030" , "#0072B2",   "#E69F00"))

results_df__for_overview_all$line <- ifelse(results_df__for_overview_all$run == 1, 2,ifelse(results_df__for_overview_all$run == 3, 2, ifelse(results_df__for_overview_all$run == 5, 2,  ifelse(results_df__for_overview_all$run == 2, 2, ifelse(results_df__for_overview_all$run == 4, 2, ifelse(results_df__for_overview_all$run == 6, 2, ifelse(results_df__for_overview_all$run == 7, 1, ifelse(results_df__for_overview_all$run == 8, 1, 1))))))))

#does not work results_df__for_overview_all$line <- factor(results_df__for_overview_all$line, level = c(1, 5, 2,  6, 3,   4))

results_df__for_overview_all$linewidth <- ifelse(results_df__for_overview_all$run == 1, .8,ifelse(results_df__for_overview_all$run == 3, .8, ifelse(results_df__for_overview_all$run == 5, .8, ifelse(results_df__for_overview_all$run == 2, .8, ifelse(results_df__for_overview_all$run == 4, .8, ifelse(results_df__for_overview_all$run == 6, .8, ifelse(results_df__for_overview_all$run == 7, 1, ifelse(results_df__for_overview_all$run == 8, 1.1, 1))))))))

#does not workresults_df__for_overview_all$linewidth <- factor(results_df__for_overview_all$linewidth, level = c(1.2, 1.1, .82, .81, .31, .3))

#the following block not used in the end
override.col <- c( "#1a1b1a", "#2c2c2c", "#353636", "#373838", "#009E73", "#2d2e2e",  "#0072B2", "#D55E00")
#override.line <- c(2,2,2,2,2,2, 1, 1)
override.linewidth <- c(.8, .81, .82, .83, .84, .85, 1, 1)

#the following leads to not all line sbeing shown.
#how cna i make a legend that contains less elements than groups?
override.col <- c( "#009E73", "#0072B2", "#D55E00")
override.line <- c(1, 2, 3)
override.linewidth <- c( .8, 1, 1.1)
#and this is wrong sequence

#TODO Insufficient values in manual scale. 6 needed but only 3 provided.
#it should be 8 - why is 8 not found?
# END the following block not used in the end


#from https://stackoverflow.com/questions/67219891/create-additional-independent-legends-in-ggplot2
library(ggnewscale)
df2 = results_df__for_overview_all %>%
  dplyr::filter(run == 7)%>%
  data.frame()

df3 = results_df__for_overview_all %>%
  dplyr::filter(run == 8)%>%
  data.frame()

results_df__for_overview_all_1_to_6 = results_df__for_overview_all %>%
  dplyr::filter(run %in% c(1:6))

(plot_trends_BOC <- ggplot(data = results_df__for_overview_all_1_to_6)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_6, aes(x = dateRi, y = BOC, group = as.factor(colour_for_plot)#, #lty =  as.factor(line), lwd = as.factor(linewidth) 
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
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_7_8_one_y_scale_2025_12_03.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_7_8_one_y_scale_2025_12_03.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_COD <- plot_trends_COD+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_7_8_free_y_scale_2025_12_03.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6_7_8_free_y_scale_2025_12_03.pdf"), width = 8, height = 3)
}



(plot_trends_MO <- ggplot(data = results_df__for_overview_all)+
    
    geom_smooth(method = "lm",
                data = results_df__for_overview_all, aes(x = dateRi, y = MO_het, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth)
                ),
                se=FALSE,  formula = my.formula )+ 
    scale_colour_manual("Run",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                        labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linetype_manual("Run", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    
    scale_linewidth_manual("Run", values = c(1.2, 1.1, .82, .81,  .41,  .4),
                           labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    
    guides(colour = guide_legend(override.aes = list(line = override.line, colour = override.col, linewidth = override.linewidth))) +
    
    #labs(x = "Date", y = "Microorganisms [mol COD / L]") +
    labs(x = "Date", y = "Microbial dry mass [mol COD L<sup>-1</sup>]")+
    theme(panel.background = element_rect(fill = "white",  colour = "black",  linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
        axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )+
    facet_wrap(.~group_letter, scales="free_y", ncol = 4)
)
setwd(gw_FuldaEcosystemServices_plots_path)
ggsave("plot_trends_MO_1_2_3_4_5_6___2025_12_03.png", width = 8, height = 2.5)
ggsave("plot_trends_MO_1_2_3_4_5_6___2025_12_03.pdf", width = 8, height = 2.5)



(plot_trends_fauna <- ggplot(data = results_df__for_overview_all)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all, aes(x = dateRi, y = fauna, colour = as.factor(colour_for_plot), lty =  as.factor(line), lwd = as.factor(linewidth)  ), 
                se=FALSE,  formula = my.formula )+
    scale_colour_manual("Run",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
                        labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linetype_manual("Run", values = c(1, 5, 2, 6, 3, 4),
                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    scale_linewidth_manual("Run", values = c(1.2, 1.1, .82, .81,  .41,  .4),
                           labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
    
    guides(colour = guide_legend(override.aes = list(line = override.line, colour = override.col, linewidth = override.linewidth))) +
    
    #labs(x = "Date", y = "Fauna [mol COD / L]") +
    labs(x = "Date", y = "Fauna dry mass [mol COD L<sup>-1</sup>])+ 

    theme(panel.background = element_rect(fill = "white",  colour = "black",  linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
        axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )+
    facet_wrap(.~group_letter, scales="free_y", ncol = 4)
)
setwd(gw_FuldaEcosystemServices_plots_path)
ggsave("plot_trends_fauna_1_2_3_4_5_6___2025_12_03.png", width = 8, height = 2.5)
ggsave("plot_trends_fauna_1_2_3_4_5_6___2025_12_03.pdf", width = 8, height = 2.5)

