#Load required packages
library(dplyr)
library(tidyr)
library(openxlsx)#read.xlsx
library(DescTools)#year()
library(patchwork) #for sticking together different plots 
library(ggpubr)#ggarrange. 
library(cowplot)
library(colorspace)
library(devtools)

#install package for color blind-safe plots
# install.packages("ggokabeito")
# #You can alternatively install the development version of ggokabeito from GitHub with:
#   devtools::install_github("malcolmbarrett/ggokabeito")
library(ggokabeito) # #scale_fill_okabe_ito  
library(ggplot2)

#sensitivity analysis
#### Step 1 (load the packages) ####

library(caTools)
#do once install.packages("http://www.maths.bris.ac.uk/~mazjcr/calibrater_0.51.tar.gz", repos = NULL, type = "source")
library(calibrater)
library(gridExtra)
library(matrixStats)
#do once install.packages("FRACTION")
library(FRACTION)

#do once install.packages("ps") #needed to install_github(SAFER)
#install_github("SAFEtoolbox/SAFE-R")
library(SAFER)

rm(list = ls()) #remove any variables and data created before, to make sure that this runs with fresh data

##########
#the runs with the different parameters are listed in the excel file "parameters.xlsx"; 1 to 6. Change accordingly
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
    #   source("D:\\work\\projects\\BIGFE_nc\\nc_ufz_home\\R\\codeByOthers\\Ded_Vilem_czechEncoding_.R")
    #   source("D:\\work\\projects\\BIGFE_nc\\nc_ufz_home\\R\\functions\\fixTediousNames.R")
    #   #source("d:\\work\\R\\fixTediousNames_2ndround.R")
    #   source("D:\\work\\projects\\BIGFE_nc\\nc_ufz_home\\R\\codeByOthers\\dbSafeNames.R")
    #   source("d:\\work\\projects\\gw_sampling\\R\\chmi_prepare_data.R")
    #   source("d:\\work\\projects\\gw_sampling\\R\\chmi_read_into_db.R")
    #   figwd <- "D:\\work\\projects\\gw_sampling\\results\\plots"
    
    #   source("d:\\work\\projects\\gw_sampling\\R\\names_chmi_prepare.R")
    
    
    #   #the followign is postgreSQL 13 and I cannot start it anzmore - dunno why
    #   #conPostGres = dbConnect(RPostgres::Postgres(), user="postgres", password="postgre",
    #   #                       host="localhost", port=5432, dbname="postgres") #
    
    #   #the following is when postgresqql 14 is started, on 5433
    #   conPostGres = dbConnect(RPostgres::Postgres(), user="postgres", password="postgre",
    #                           host="localhost", port=5433, dbname="postgres") #
    
    gw_FuldaEcosystemServices_results_txt_path <-"G:\\freefilesyncHBU\\projects\\gw_ecosystem_services_Fulda_plain\\results\\txt"
    #TODO not used ?!   gw_FuldaAuePublished_path <- "d:\\work\\projects\\gw_FuldaAue"
    gw_FuldaEcosystemServices_plots_path <-"G:\\freefilesyncHBU\\projects\\gw_ecosystem_services_Fulda_plain\\results\\plots"
    #TODO not used ?!   gw_FuldaAue_ecosystem_services_path <- "G:\\freefilesyncHBU\\projects\\gw_ecosystem_services_Fulda_plain"
    
    setwd("G:\\freefilesyncHBU\\projects\\git\\gw_ecosystem_services\\Fulda_plains\\R_git\\gtm_River_Fulda_floodplain\\gtm_River_Fulda_floodplain")
    #source("River_Fulda_floodplain_parameters_offline_extHD.R")    #TODO removed path         
    
  }else{
    #   source("D:\\work\\projects\\BIGFE_nc\\nc_ufz_home\\R\\codeByOthers\\Ded_Vilem_czechEncoding_.R")
    #   source("D:\\work\\projects\\BIGFE_nc\\nc_ufz_home\\R\\functions\\fixTediousNames.R")
    #   #source("d:\\work\\R\\fixTediousNames_2ndround.R")
    #   source("D:\\work\\projects\\BIGFE_nc\\nc_ufz_home\\R\\codeByOthers\\dbSafeNames.R")
    #   source("d:\\work\\projects\\gw_sampling\\R\\chmi_prepare_data.R")
    #   source("d:\\work\\projects\\gw_sampling\\R\\chmi_read_into_db.R")
    #   figwd <- "D:\\work\\projects\\gw_sampling\\results\\plots"
    
    #   source("d:\\work\\projects\\gw_sampling\\R\\names_chmi_prepare.R")
    
    
    #   #the followign is postgreSQL 13 and I cannot start it anzmore - dunno why
    #   #conPostGres = dbConnect(RPostgres::Postgres(), user="postgres", password="postgre",
    #   #                       host="localhost", port=5432, dbname="postgres") #
    
    #   #the following is when postgresqql 14 is started, on 5433
    #   conPostGres = dbConnect(RPostgres::Postgres(), user="postgres", password="postgre",
    #                           host="localhost", port=5433, dbname="postgres") #
    
    gw_FuldaEcosystemServices_results_txt_path <-"d:\\work\\projects\\gw_ecosystem_services_Fulda_plain\\results\\txt"
    #TODO not used ?!   gw_FuldaAuePublished_path <- "d:\\work\\projects\\gw_FuldaAue"
    gw_FuldaEcosystemServices_plots_path <-"d:\\work\\projects\\gw_ecosystem_services_Fulda_plain\\results\\plots"
    #TODO not used ?!   gw_FuldaAue_ecosystem_services_path <- "d:\\work\\projects\\gw_ecosystem_services_Fulda_plain"
    
    #TODO 17.10.25 no, just the one folder where the recent files are for upload
    setwd("d:\\work\\projects\\git\\gw_ecosystem_services\\Fulda_plains\\R_git\\gtm_River_Fulda_floodplain\\gtm_River_Fulda_floodplain")
  }  
}else{
  #TODO not used ?! gw_FuldaAue_ecosystem_services_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain"
  #TODO not used ?! gw_FuldaAuePublished_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/0_finished/2021_gw_FuldaAue"
  gw_FuldaEcosystemServices_plots_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain/results/plots"
  gw_FuldaEcosystemServices_results_txt_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain/results/txt"
  
  #   # #TODO 5.12.21 is that still true: CAREFUL !!! ON MAC SO FAR PGADMIN NOT INSTALLED !
  #   # if(whichosrightnow == "mac") {
  #   # mac
  #   # ACHTUNG !!! die neueste Datei laeuft nicht auf dem mac
  #   #source("/Users/susanneschmidt/Documents/head/Arbeit/projects/BIGFE_nc/nc_ufz_home/R/codeByOthers/Ded_Vilem_czechEncoding_.R")
  #   source("/Users/susanneschmidt/Documents/head/Arbeit/projects/BIGFE_nc/nc_ufz_sharedwithme/BIGFE/Daten/R/functions/codeByOthers/Ded_Vilem_czechEncoding.R")   
  #   #source("/Users/susanneschmidt/Documents/head/Arbeit/projects/BIGFE_nc/nc_ufz_home/R/functions/fixTediousNames.R")
  #   source("/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_sampling/R/chmi_prepare_data.R")
  #   source("/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_sampling/R/chmi_read_into_db.R")
  #   source("/Users/susanneschmidt/Documents/head/Arbeit/projects/BIGFE_nc/nc_ufz_sharedwithme/BIGFE/Daten/R/functions/codeByOthers/dbSafeNames.R")
  #   #setwd("/Users/susanneschmidt/Documents/head/Arbeit/Forschung/HBU/projects/runoff_from_streams/data/preparation_of_data_from_Voda 2002_2017kor")
  
  #   setwd("/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain")
  
  setwd("/Users/susanneschmidt/Documents/head/Arbeit/projects/git/gw_ecosystem_services/Fulda_plains/R_git/gtm_River_Fulda_floodplain/gtm_River_Fulda_floodplain")
  
  #TODO not used ?! gw_FuldaAue_ecosystem_services_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain"
  #TODO not sued ?! gw_FuldaAuePublished_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/0_finished/2021_gw_FuldaAue"
  gw_FuldaEcosystemServices_plots_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain/results/plots"
  gw_FuldaEcosystemServices_results_txt_path <-"/Users/susanneschmidt/Documents/head/Arbeit/projects/gw_ecosystem_services_Fulda_plain/results/txt"
  
}


#  #30.6.25 does not work conPostGres = dbConnect(RPostgres::Postgres(), user="postgres", password="postgre",
#                        #   host="localhost", port=5434, dbname="postgres") # 5432 was apparently busy - didnt know how to turn it off
# #30.6.25
#   conPostGres = dbConnect(RPostgres::Postgres(), user="postgres", password="postgre",
#                           host="localhost", port=5432, dbname="postgres") # 

#   setwd("/Users/susanneschmidt/Documents/head/Arbeit/projects/0_finished/2021_gw_FuldaAue")


##fulda_variables TODO offline


#TODO should be separate folder, as it was   setwd("/Users/susanneschmidt/Documents/head/Arbeit/projects/git/gw_ecosystem_services/Fulda_plains/R_git/functions")
source("River_Fulda_floodplain_data_offline.R") #required for parameters variables
source("River_Fulda_floodplain_parameters_offline.R")            

source("River_Fulda_floodplain_prec_plot.R") #function Fulda_prec_plot

source("gtm_sim.R")

source("gtm_MulObj.R")
source("gtm_MulOut.R")
source("gtm_nse.R")


source("model_execution_gtm.R")

source("scatter_plots_gtm.R")
source("scatter_plots_tr_gtm.R")

source("parcoord_gtm.R")
source("boxplot1_gtm.R")

source('plot_convergence_gtm.R')#at this stage not differnet to plot_convergence

#urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_data.R"
#source(urlfiletext)  ##fulda_variables

#urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_parameters.R"
#source(urlfiletext)

########
#functions reactions
#see above urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/model_functions.R"
#source(urlfiletext)
source("model_functions.R")


#need to update
urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/error_measures.R"
source(urlfiletext)
source("error_measures.R")

#   ########
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

#this depends on the two scenario variables
fulda_variables_read_in <- fulda_variables(run, factor_CC_MO, factor_CC_fauna)
names(fulda_variables_read_in) <- c("Fulda_daily_prec", "Fulda_daily_temp_", "chem_ordered_per_date_1978_1981", "chem_ordered_per_date_1978_1981_mean_per_group", "fauna_deep_PerSamplPerTaxonWide_bm_sum", "fauna_deep_PerSamplPerTaxon_bm_mean_per_group", "t_0", "t_max", "DETRITUS_gr1_t0", "DETRITUS_gr2_t0", "DETRITUS_gr3_t0", "DETRITUS_gr4_t0", "BOC_gr1_t0", "BOC_gr2_t0", "BOC_gr3_t0", "BOC_gr4_t0", "MO_het_gr1_t0", "MO_het_gr2_t0", "MO_het_gr3_t0", "MO_het_gr4_t0", "fauna_gr1_t0", "fauna_gr2_t0", "fauna_gr3_t0", "fauna_gr4_t0",  "CC_table_MO", "CC_table_fauna", "Fulda_daily_temp_joh_long") 

list2env(fulda_variables_read_in, globalenv())

chem_ordered_per_date_1978_1981$group <- chem_ordered_per_date_1978_1981$kmeans4gr
chem_ordered_per_date_1978_1981$group_letter <- ifelse(chem_ordered_per_date_1978_1981$group ==  2, "P", ifelse(chem_ordered_per_date_1978_1981$group == 3, "R" , ifelse (chem_ordered_per_date_1978_1981$group == 4, "A", ifelse (chem_ordered_per_date_1978_1981$group == 1, "M", NA))))   
chem_ordered_per_date_1978_1981$group_letter <- factor(chem_ordered_per_date_1978_1981$group_letter , levels = c("R", "M", "P", "A"))

##########
#parameters
##########

#read parameters  from file
parameters_read_in <- read_parameters( run)
names(parameters_read_in) <- c("delta_t", "max_t", "aquifer_depth", "import_MO_het", "scenario_with_1_or_without_0_fauna", "scenario_with_1_or_without_0_MO", "mortalityRate", "import_fauna", "yield_ac", "yield_MO", "K_MO_at_temp", "rMO_BOC_uptake_per_day_at_lab_temperature", "rFauna_MO_uptake_per_day_at_TEMP", "k1", "excretionRate", "TOC_COD_mol_m2_yr_precipitation", "RECHARGE_COD_mol_per_m3_per_day_df", "lab_temp", "K_ac", "growth_model_MO_type", "growth_model_fauna_type", "mortalityFraction_per_degree", "microbe_loss_factor_when_no_fauna") 

list2env(parameters_read_in, globalenv())

##########
#creating temperature scenarios
##########

temperature_scenario <- parvar$temperature_scenario[run]

Fulda_daily_temp_joh_long$TT_TER <- Fulda_daily_temp_joh_long$TT_TER + temperature_scenario #air temperature
names(Fulda_daily_temp_joh_long) <- sub("value" , "gw_temp", names(Fulda_daily_temp_joh_long))
Fulda_daily_temp_joh_long$gw_temp <- Fulda_daily_temp_joh_long$gw_temp + temperature_scenario #daily groundwater temperature extrapolated from air temperature

###################
#preparing in situ data for plotting and error measures and sensitivity analysis

#detritus
DETR_1978_1981_mean_per_group_and_date <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(OS_mol_COD_L)) %>%
  dplyr::group_by(Date, kmeans4gr)%>%
  dplyr::summarise(OS_mol_COD_L = mean(OS_mol_COD_L, na.rm = TRUE)) %>%
  dplyr::mutate(ID = paste(Date, kmeans4gr, sep = "_"))%>%
  dplyr::mutate(x = OS_mol_COD_L)

#BOC
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
#container for results
##########

if(is.na(max_t)){
  t_max = t_max
}else{
  t_max = max_t #set another end date than the one in the Fulda study; max_t is read in from parameter_variables
}

error_measures_data_table <- data.frame(run = NA, group = NA, variable = NA, R2 = NA, MAE = NA, RMSE = NA, MB = NA, NSE = NA, N = NA)

##########
#reading in Fulda precitiation plot - does not depend on model variables, but depends on Fulda_daily_prec in fulda_variables_read_in, and thus, cannot be read in earlier than this
##########

#urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/River_Fulda_floodplain_prec_plot.R"
#source(urlfiletext)

#TODO done above for offline

Fulda_prec_plotted <- Fulda_prec_plot(enddate = t_max)

##########
#over time
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
results2$import_from_detritus <- 0
results2$BOC[1] <- BOC_gr2_t0

results2$MO_het[1] <- MO_het_gr2_t0

results2$fauna <- fauna_gr2_t0


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
results3$import_from_detritus <- 0
results3$BOC[1] <- BOC_gr3_t0

results3$MO_het[1] <- MO_het_gr3_t0

results3$fauna <- fauna_gr3_t0


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
results4$import_from_detritus <- 0
results4$BOC[1] <- BOC_gr4_t0

results4$MO_het[1] <- MO_het_gr4_t0

results4$fauna <- fauna_gr4_t0


results <- rbind(results1, results2, results3, results4)

results$group_letter <- factor(results$group_letter , levels = c("R", "M", "P", "A"))



uniquedatevector <- unique(results$dateRi)
uniquegroupvector <- unique(results$group)
uniquegrouplettervector <- unique(results$group_letter)

#TODO for just the modelling, it is sufficient to do this here. However, for the function and sensitivity modelling, it needs to be done earlier ?!
#chem_ordered_per_date_1978_1981_BOC_joined <- left_join(chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date, results_df, by =  c("Date" = "dateRi", "kmeans4gr" = "group"))
results_joined <- dplyr::left_join(results, chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date, by =  c( "dateRi" = "Date" , "group" = "kmeans4gr"  ))
#TODO 29.11. hang on - this is now superflusous because I have constructed the results file from - no, from meteo - I do need to join the BOC data - or do I do that from the start ?! for SA yes

#sensitivity analysis leaning on https://github.com/SAFEtoolbox/SAFE-R/blob/main/demo/workflow_rsa_hymod.R
#workflow_rsa_hymod.R
# Define inputs:
DistrFun  <- "unif" # Parameter distribution

DistrPar  <- list( 
  #c(0.05, .8), #yield_ac  dann ist K_ac nicht mehr wichtig
  c(0.001, .8), #yield_ac 
  # then yield highly influential c(0.0000001, .00001), #K_ac upper limit was .05
  # c(0.0000001, .05), #K_ac upper limit was .5 # then K_ac most influential, as always
  c(0.00000001, .0001), #K_ac upper limit was .05
  c(0.00000001, .1), #factor_CC_MO
  c(1, 20), #rMO_COD_uptake_per_day_at_lab_temperature
  c(0, 1), #microbe_loss_factor_when_no_fauna
  c(500,2000),#average_precipitation_mm_yr
  c(0.5,1),#recharge_fraction_of_precipitation # was 1 in th efirst 6 runs
  c(0.1, 1),#TOC_mol_m2_yr_precipitation
  c(1,20),#factor_how_many_times_Detritus_compared_to_TOC
  c(.000001, 1),#k1
  c(1,50),#aquifer_depth
  c(.00000001, 1),#K_MO_at_temp
  c(.001,.1),#yield_MO
  c(0.001, 1.1),#rFauna_MO_uptake_per_day_at_TEMP
  c(.0001, .1),#excretionRate
  c(0.000000001, 0.1),#mortalityRate
  c(.01,1),#factor_CC_fauna
  c(.001,0.5)#mortalityFraction_per_degree
) #Parameter ranges (ideally from literature, but partly not available)

x_labels <- c("yield_ac",
              "K_ac", 
              "factor_CC_MO", 
              "rMO_COD_uptake_per_day_at_lab_temperature",
              "microbe_loss_factor_when_no_fauna",
              "average_precipitation_mm_yr",
              "recharge_fraction_of_precipitation",
              "TOC_mol_m2_yr_precipitation",	
              "factor_how_many_times_Detritus_compared_to_TOC",
              "k1",
              "aquifer_depth",
              "K_MO_at_temp",	
              "yield_MO",
              "rFauna_MO_uptake_per_day_at_TEMP",
              "excretionRate",
              "mortalityRate",
              "factor_CC_fauna",
              "mortalityFraction_per_degree"
)

#TODO vector of abbreviated variable names
x_labels <- c("yield_ac",
              "K_ac", 
              "fact_CC_MO", 
              "rMO_C_upt",
              "mic_loss",
              "av_prec_yr",
              "rech_frct_prc",
              "TOC_prec",	
              "fct_Dt_TOC",
              "k1",
              "aquifer_depth",
              "K_MO_temp",	
              "yield_MO",
              "rFau_MO_up",
              "excretionRte",
              "mortRate",
              "fact_CC_fau",
              "mrtFrct_p_dg"
)

g <- 4
for (g in 2:length(unique(results$group))){
  #TODO close this loop - need to write rbind the results into one file ?! or not?s
  CC_group_MO_g <- CC_table_MO$CC[CC_table_MO$group == uniquegroupvector[g]]
  CC_group_fauna_g <- CC_table_fauna$CC[CC_table_fauna$group == uniquegroupvector[g]]
  group_letter_g <- unique(results$group_letter[results$group == uniquegroupvector[g]])
  
  Fulda_daily_temp_joh_long_g <- Fulda_daily_temp_joh_long %>%
    dplyr::filter(group_letter == group_letter_g)
  
  results_g <- results_joined %>%
    dplyr::filter(group_letter == group_letter_g)
  
  myfun <- "gtm_MulObj" # for e.g. RSA
  
  ## Step 3 (sample inputs space)
  
  SampStrategy <- "lhs" # Latin Hypercube
  N <- 3000 # Number of samples
  M <- length(DistrPar) # Number of inputs
  X <- AAT_sampling(SampStrategy, M, DistrFun, DistrPar, N)
  colnames(X) <- x_labels
  
  ## Step 4 (run the model) 
  Y <- model_execution_gtm(myfun, X, dat = results_g)  # size (N,2). sis: X are the parameters
  #sis 17:14 - 17:32 , 11:56- vor 12:30
  colnames(Y) <- c("rmse", "bias")
  #sis code checking - if code does not work, try increasing N first   colnames(Y) <- c("rmse")
  
  ## Step 5a (Regional Sensitivity Analysis with threshold)
  
  # (**) Note: if you want to use input/output samples generated in
  # another programme, them here and save them in two matrix 
  # input : X = (N x M)
  # output: Y = (N x P)
  # [N=number of samples; M=number of inputs; P=number of outputs]
  
  # Visualize input/output samples (this may help finding a reasonable value
  # for the output threshold):
  
  #scatter_plots(X,Y[,1]) + ylab("rmse")
  #TODO sis I have very low values, and thus, RMSE and bias are "low", I need other ylims
  windows();scatter_plots_gtm(X,Y[,1]) + ylab("rmse") #sis based on ggplot, thus shoudl work:
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("scatter_plots_RMSE_group_",g ,".png"))
  
  # 
  # use dev.new() if you want to open the plot in a new window
  
  #dev.new() #does not work for me on visual studio code
  #scatter_plots(X,Y[,2]) + ylab("bias")
  #sis does not show in VS Code, therefore, see below, after on foot code:
  
  windows();scatter_plots_gtm(X,Y[,2]) + ylab("bias")
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("scatter_plots_bias_group_",g ,".png"))
  
  #sis instead, from inside scatter_plots()
  #8.11.25 does not seem necessary anymore, or rather, not even correct; outcommented
  # prnam <- as.character( 1:ncol(X) )
  # ns <- nrow( X )
  #  dat <- data.frame(x = as.vector(X), 
  #                       y = rep(Y, ncol( X )), 
  #                       parnam = factor(rep(prnam, each = ns), levels = prnam))
  # (gplres <- ggplot(data = dat, 
  #                   mapping = aes(x = x, y = y, parnam = parnam)) + 
  #              facet_grid(. ~ parnam, scales = "free") + 
  #              geom_point() + ylab("Output") + xlab("Inputs") + 
  #              theme_bw()+
  #              ylim ( c(-1e-04,1e-04)))
  
  #  windows();plot(gplres) #https://stackoverflow.com/questions/52284345/how-to-show-r-graph-from-visual-studio-code
  
  # Set output threshold:
  rmse_thres <- .000044    #  threshold for the first obj. fun.
  bias_thres <- 0.00009  # behavioural threshold for the second obj. fun.
  
  #g 1
  if (g == 1) {
    rmse_thres <- .000045  #  threshold for the first obj. fun.
    bias_thres <- 0.00045  # behavioural threshold for the second obj. fun.
  }
  #g 2
  if (g == 2) {
    rmse_thres <- .000069
    bias_thres <- 0.00045
  }
  #g 3
  if (g == 3) {
    rmse_thres <- .000075
    bias_thres <- 0.001 
  }
  #g 4
  if (g == 4) {
    rmse_thres <- .00005
    bias_thres <- 0.0006 
  }
  # RSA (find behavioural parameterizations):
  threshold <- c(rmse_thres, bias_thres)
  #for testing threshold <- c(rmse_thres)
  
  rsatr <- RSA_indices_thres(X, Y, threshold) 
  mvd <- rsatr$stat
  idxb <- rsatr$idxb #sis True False - for what ?! for colouring in one of the next plots
  
  # Highlight the behavioural parameterizations in the scatter plots:
  
  #dev.new() #sis does not work, add windows(); in line below
  windows();scatter_plots_tr_gtm(X, Y[,1], prnam = x_labels, idxb) + ylab("rmse")
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("scatter_plots_RMSE_mvd_idxb_group_",g ,".png"))
  
  #dev.new()#sis does not work, add windows(); in line below
  windows();scatter_plots_tr_gtm(X, Y[,2], prnam = x_labels, idxb) + ylab("bias")
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("scatter_plots_bias_mvd_idxb_group_",g ,".png"))
  
  # Plot parameter CDFs:
  #dev.new()#sis does not work, add windows(); in line below
  #RSA_plot_thres(X, idxb, prnam = x_labels, threshold = threshold) #threshold has two values, one for rmse, one for bias
  #sis Error in `levels<-`(`*tmp*`, value = as.character(levels)) : 
  # factor level [2] is duplicated
  #setwd(gw_FuldaEcosystemServices_plots_path)
  #ggsave(paste0("RSA_plot_thres_group_",g ,".png"))
  
  # #sis code of RSA_plot_thres, to be able to find out why this does not work
  # function (X, idxb, prnam = NULL, threshold)  #X 300 x 5; idxb 300
  # {
  #     stopifnot(is.matrix(X), is.numeric(X), is.logical(idxb), 
  #         length(idxb) == nrow(X))
  #     Ng <- 2
  #     N <- nrow(X) #300
  #     M <- ncol(X) # 5
  #     if (!is.null(prnam)) {
  #         stopifnot(length(prnam) == M)
  #     }
  #     else {
  #         prnam <- paste("X", seq(1, M), sep = "") #5
  #     }
  #     idxl <- as.numeric(idxb) #300 long
  #     idx <- idxl + 1                 
  #     Yk <- c(min(Y), threshold, max(Y))#4 long
  #     xx <- lapply(1:M, function(.ii) { #dim 300 x 5 --> 6000
  #         unique(sort(X[, .ii]))
  #     })
  
  #      CDF_interim <- lapply(1:M, function(.ii) lapply(1:Ng, function(.kk) {
  #         .tmp <- ecdf(X[idx == .kk, .ii])(xx[[.ii]])
  #         c(rep(.tmp[1:(length(.tmp) - 1)], each = 2), tail(.tmp,
  #             1))
  #          }))   
  #          #broken down, and then not usable anymore:
  #     CDF <- lapply(1:M, function(.ii) #for each of M = 5 variables
  #                lapply(1:Ng, function(.kk) { #for each of 2 ... i.e., these two lapply make it 10 times the fct.
  #                    .tmp <- 
  #                         ecdf(X[idx == .kk, .ii]) #dim 740 . these are the data that is worked on. nee the fct ?!!!
  #                             #str(ecdf(X[idx == .kk, .ii])) defines a fct of 3 classes.                        
  #                               (xx[[.ii]])   #function argument - this many times will the edcf fct be performed
  #                               #
  #                               c(rep(.tmp[1:(length(.tmp) - 1)], each = 2), tail(.tmp, 1) #this is what is done by the lapply - so now the fct is used
  #                             ) #first the function parts except for the last are used, then the last part - no - first , the N x Nb lines are used except for one, then one
  #     }
  #     ))#CDF_interim dim is : list of 5, each of which list of 2, each of which 599 entries, thus 5990 values
  #     xx <- lapply(xx, function(.inp) c(.inp[1], rep(.inp[-1],
  #         each = 2))) # dim list of 5, each entry 599 long
  #     nux <- sapply(xx, length) #5 entries: each 300 # 5 x 300 = 6000
  #     dat <- data.frame(
  #       x = unlist(lapply(xx, function(.inp) rep(.inp,  Ng))), #5990 entries. take each value in a 5x599 list, repeat each twice, thus 599*10
  #         CDF = unlist(lapply(CDF_interim, function(.inp) unlist(.inp))), #5990 entries.
  #         #parnam = unlist(lapply(1:M, function(.ii) rep(prnam[.ii], each = Ng * nux[.ii]))), #repeat each entry of prnam, i.e. 5 entries, in this case always 300*2 times, because each entry of nux is 300, thus 300 *2 * 5 --> 3000
  #          parnam = unlist(lapply(1:M, function(.ii) rep(prnam[.ii], each = Ng*(Ng * (nux[.ii])-1)))),
  #             #group = unlist(lapply(1:M, function(.ii) rep(round(Yk[2:(Ng + 1)], 2), each = nux[.ii])))
  #             group = unlist(lapply(1:M, function(.ii) rep(round(Yk[2:(Ng + 1)], 2), each = Ng *( (nux[.ii]))-1 )))
  #             )
  #             #fkt immer noch nicht, also die zeilen jeweils einzlen machen und 
  #             dat <- data.frame(cbind(x = x, CDF = CDF, parnam = parnam, group = group))
  #     #does not  work because rounding means that is is 0 everywhere dat$group <- factor(dat$group, levels = round(Yk[2:(Ng + 1)], 2))
  #     dat$parnam <- factor(dat$parnam, levels = prnam)
  #     .pl <- ggplot(data = dat, mapping = aes(x = x, y = CDF, color = group,
  #         parnam = parnam)) + facet_grid(. ~ parnam, scales = "free_x") +
  #         geom_line() + scale_color_manual(values = c("blue", "red")) +
  #         theme_bw() + theme(legend.position = "none")
  #     return(.pl)
  # }
  
  #sis 8.11.25 fkt nicht. fkt, wenn ich die vier einzlen herstelle. aber der plot geht schief, s.o.
  #dat <- data.frame(x = unlist(lapply(xx, function(.inp) rep(.inp,
  #         Ng))), 
  #         CDF = unlist(lapply(CDF, function(.inp) unlist(.inp))),
  #         parnam = unlist(lapply(1:M, function(.ii) rep(prnam[.ii], each = Ng * nux[.ii]))), 
  #        group = unlist(lapply(1:M,  function(.ii) rep(round(Yk[2:(Ng + 1)], 2), each = nux[.ii]))))
  #all elements are 3000 long, except for CDF which is 5990
  
  
  # Check the ranges of behavioural parameterizations by
  # Parallel coordinate plot:
  
  mycol <- idxb
  mycol[idxb == FALSE] <- gray(.7, alpha = .7)
  mycol[idxb == TRUE] <-  gray(0, alpha = .7)
  
  #dev.new()#sis does not work, add windows(); in line below
  windows(); parcoord_gtm(X, col = mycol, plotorder = idxb)
  #setwd(gw_FuldaEcosystemServices_plots_path)
  #ggsave(paste0("Parallel_coordinate_plot_group_",g ,".png"))
  setEPS()
  #postscript(paste0("Parallel_coordinate_plot_group_",g ,"_.pdf"))
  #TODO does not work
  pdf(paste0("Parallel_coordinate_plot_group_",g ,"_.pdf"))
  parcoord_gtm(X, col = mycol, plotorder = idxb)
  dev.off()
  
  
  
  # Plot the sensitivity indices (maximum vertical distance between
  # parameters CDFs):
  
  # border sets the colors of the boxplots, boxwex sets the width of the boxplot, axes = FALSE and add = TRUE allow to draw the boxplot over the parcoord plot.
  
  #dev.new()#sis does not work, add windows(); in line below
  windows(); boxplot1_gtm(mu = mvd, prnam = x_labels) 
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("boxplot_mvd_group_",g ,".png"), width = 8, height = 4)
  
  # Compute sensitivity indices with confidence intervals using bootstrapping
  
  Nboot <- 1000
  rsatr_b <- RSA_indices_thres(X, Y, threshold, Nboot = Nboot) 
  
  mvd <- rsatr_b$stat
  idxb <- rsatr_b$idxb
  
  mvd_lb <- rsatr_b$stat_lb
  mvd_ub <- rsatr_b$stat_ub
  
  # Plot results:
  
  #dev.new()#sis does not work, add windows(); in line below
  #sis this is without alpha - 
  windows(); boxplot1_gtm(mu = mvd, lb = mvd_lb, ub = mvd_ub, prnam = x_labels) 
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("boxplot_mvd_bootstrap_group_",g ,".png"), width = 8, height = 4)
  
  # Repeat computations using an increasing number of samples so to assess
  # convergence:
  NN <- seq(N / 5, N, by = N / 5 )
  mvd <- RSA_convergence_thres(X, Y[,1], NN, threshold = rmse_thres) 
  
  mvd_st <- mvd$stat
  
  # Plot the sensitivity measures (maximum vertical distance between
  # parameters CDFs) as a function of the number of samples:
  
  #dev.new()#sis does not work, add windows(); in line below
  windows(); plot_convergence_gtm(NN, mvd_st, xlab = "no of samples", ylab = "mvd", labels = x_labels)
  #setwd(gw_FuldaEcosystemServices_plots_path)
  #ggsave(paste0("plot_convergence_group_",g ,".png"))
  setEPS()
  #postscript(paste0("plot_convergence_group_",g ,"_.pdf"))
  # does not work
  pdf(paste0("plot_convergence_group_",g ,"_.pdf"))
  plot_convergence_gtm(NN, mvd_st, xlab = "no of samples", ylab = "mvd", labels = x_labels)
  dev.off()
  
  # Repeat convergence analysis using bootstrapping to derive
  # confidence bounds:
  
  Nboot <- 1000
  rsatr_b_conf <- RSA_convergence_thres(X, Y[,1], NN,  threshold = rmse_thres, Nboot = Nboot) 
  #sis 9.11.25 Error in compute_indices(Xi, Yi, threshold, flag) : 
  # Cannot find any output value below the threshold! Try increasing the threshold value
  #sis thus, the rest of 5a will not work - do I care?
  #TODO it does work on 30.11.
  mvd <- rsatr_b_conf$stat
  idxb <- rsatr_b_conf$idxb
  
  mvd_lb <- rsatr_b_conf$stat_lb
  mvd_ub <- rsatr_b_conf$stat_ub
  
  #dev.new()#sis does not work, add windows(); in line below
  #setwd(gw_FuldaEcosystemServices_plots_path)
  #ggsave(paste0("plot_convergence_gtm_bootstrap_group_",g ,"_.png"))
  #TODO is not saved - why not? becase no ggplot ?
  #from https://stackoverflow.com/questions/72115153/how-do-you-save-matplot-in-r-as-eps
  windows();plot_convergence_gtm(NN, mvd, mvd_lb, mvd_ub, xlab = "no of samples", ylab = "mvd", labels = x_labels)
  #setEPS()
  pdf(paste0("plot_convergence_gtm_bootstrap_group_",g ,"_.pdf"))
  plot_convergence_gtm(NN, mvd, mvd_lb, mvd_ub, xlab = "no of samples", ylab = "mvd", labels = x_labels)
  dev.off()
  
  
  
  ## Step 5b (Regional Sensitivity Analysis with groups)
  
  # RSA (find behavioural parameterizations):
  
  rsa_gr <- RSA_indices_groups(X, Y[,1])
  
  mvd_median <- rsa_gr$mvd_median
  mvd_mean <- rsa_gr$mvd_mean
  mvd_max <- rsa_gr$mvd_max
  spread_median <- rsa_gr$spread_median
  spread_mean <- rsa_gr$spread_mean
  spread_max <- rsa_gr$spread_max
  idx <- rsa_gr$idx
  Yk <- rsa_gr$Yk
  
  # Plot parameter CDFs:
  #dev.new()#sis does not work, add windows(); in line below
  windows(); RSA_plot_groups(X, idx, Yk, prnam = x_labels) + ylab("rmse")
  #sis Error in `levels<-`(`*tmp*`, value = as.character(levels)) : 
  #  factor level [2] is duplicated
  #same error as for CDF - well, here is CDF again, thus no surprise
  
  # Compute sensitivity indices with confidence intervals using bootstrapping
  Nboot <- 1000
  ngroup <- 10
  rsa_gr_b <- RSA_indices_groups(X, Y[,1], ngroup, Nboot)
  
  # Statistics across all bootstrap resamples
  mvd_median <- rsa_gr_b$mvd_median
  mvd_mean <- rsa_gr_b$mvd_mean
  mvd_max <- rsa_gr_b$mvd_max
  spread_median <- rsa_gr_b$spread_median
  spread_mean <- rsa_gr_b$spread_mean
  spread_max <- rsa_gr_b$spread_max
  idx <- rsa_gr_b$idx
  Yk <- rsa_gr_b$Yk
  
  # Compute mean and confidence intervals of the sensitivity indices across the
  # bootstrap resamples:
  alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 
  
  mvd_median_m <- colMeans(mvd_median) # median
  mvd_median_lb <-  colQuantiles(mvd_median,probs=alfa/2) # Lower bound
  mvd_median_ub <- colQuantiles(mvd_median,probs=1-alfa/2) # Upper bound
  
  mvd_mean_m <- colMeans(mvd_mean) # mean
  mvd_mean_lb <-  colQuantiles(mvd_mean,probs=alfa/2) # Lower bound
  mvd_mean_ub <- colQuantiles(mvd_mean,probs=1-alfa/2) # Upper bound
  
  mvd_max_m <- colMeans(mvd_max) # max
  mvd_max_lb <-  colQuantiles(mvd_max,probs=alfa/2) # Lower bound
  mvd_max_ub <- colQuantiles(mvd_max,probs=1-alfa/2) # Upper bound
  
  # Plot results:
  
  #dev.new()#sis does not work, add windows(); in line below
  windows(); boxplot1_gtm(mu = mvd_median_m, lb = mvd_median_lb, ub = mvd_median_ub, prnam = x_labels) + ylab("mvd median") 
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("boxplot_mvd_bootstrap_resample_group_",g ,".png"), width = 8, height = 4)
  ##### end regional sensitivity analysis
}#end loop through groups








#### PAWN snesitivity https://github.com/SAFEtoolbox/SAFE-R/blob/main/demo/workflow_PAWN_hymod.R
#minus those steps that are done for RSA
#### Step 1 (load the packages) ####

#### Step 2 (setup the Hymod model) ####

# Load data from SAFEr:
data(LeafCatch)
dat <- LeafCatch[1:365,]

# Define inputs:
#see above

# Define output:
myfun <- "hymod_MulOut"
#sis
myfun <- "gtm_MulOut"

#### Step 3 (sample inputs space) ####
#M, N, X,  see above

#### Step 4 (run the model) ####
#Y_multi_out <- model_execution(myfun, X, dat = dat)  # size (N,2)
#applied to my data
Y_multi_out <- model_execution(myfun, X, dat = results_g)  # size (N,2)
#14:27 - 15:15
colnames(Y_multi_out) <- c("rmse", "bias", "mean", "st dev", "var", "max")

# Visualize input/output samples for maximum BDC
windows(); scatter_plots_gtm(X,Y_multi_out[,6]) + ylab("max")
#sis der geht schief - nix zu sehen, und abstruse Y axis - nd da war das Beispiel !!!

#### Step 5 (apply PAWN) ####

n <- 10 # number of conditioning intervals

# choose output of interest
Y <- Y_multi_out[,6] # max BDC


# Compute and plot conditional and unconditional CDFs:
dev.new()
windows(); pawn_cdf <- pawn_plot_CDF(X, Y, n=10, n_col=3, y_label='output y', labelinput=x_labels)
YF <- pawn_cdf$YF
FU <- pawn_cdf$FU
FC <- pawn_cdf$FC
xc <- pawn_cdf$xc

# Compute and plot KS statistics for each conditioning interval:
KS <- pawn_ks(YF, FU, FC)
dev.new()
windows();KS_all <- pawn_plot_ks(YF, FU, FC, xc, n_col=3, x_labels = x_labels)

# Compute PAWN sensitivity indices:
pawn_ind <- pawn_indices(X, Y, n)

KS_median <- pawn_ind$KS_median
KS_mean <- pawn_ind$KS_mean
KS_max <- pawn_ind$KS_max

# Plot results:
dev.new()
p1 <- boxplot1_gtm(as.vector(KS_median), prnam = x_labels) + ylab("KS (median)") +
  ggtitle("max BDC") + theme(plot.title = element_text(hjust = 0.5))
p2 <- boxplot1_gtm(as.vector(KS_mean), prnam = x_labels) + ylab("KS (mean)") +
  ggtitle("max BDC") + theme(plot.title = element_text(hjust = 0.5))
p3 <- boxplot1_gtm(as.vector(KS_max), prnam = x_labels) + ylab("KS (max)") +
  ggtitle("max BDC") + theme(plot.title = element_text(hjust = 0.5))
windows();grid.arrange(grobs = list(p1, p2, p3), ncol = 3)

# Use bootstrapping to derive confidence bounds:
Nboot <- 1000

# Compute sensitivity indices for Nboot bootstrap resamples
# (Warning: the following line may take some time to run, as the computation of
# CDFs is costly):
pawn_ind <- pawn_indices(X, Y, n, Nboot)
#15:23-15:28
KS_median <- pawn_ind$KS_median
KS_mean <- pawn_ind$KS_mean
KS_max <- pawn_ind$KS_max

# KS_median and KS_mean and KS_max have shape (Nboot, M)
# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

KS_stat <- KS_median
KS_median_m <- colMeans(KS_stat) # mean
KS_median_lb <-  colQuantiles(KS_stat,probs=alfa/2) # Lower bound
KS_median_ub <- colQuantiles(KS_stat,probs=1-alfa/2) # Upper bound

KS_stat <- KS_mean
KS_mean_m <- colMeans(KS_stat) # mean
KS_mean_lb <-  colQuantiles(KS_stat,probs=alfa/2) # Lower bound
KS_mean_ub <- colQuantiles(KS_stat,probs=1-alfa/2) # Upper bound

KS_stat <- KS_max
KS_max_m <- colMeans(KS_stat) # mean
KS_max_lb <- colQuantiles(KS_stat,probs=alfa/2) # Lower bound
KS_max_ub <- colQuantiles(KS_stat,probs=1-alfa/2) # Upper bound

# Plot bootstrapping results:
dev.new()
p1 <- boxplot1_gtm(mu = KS_median_m, lb = KS_median_lb, ub = KS_median_ub, prnam = x_labels) + ylab("KS (median)") +
  theme(plot.title = element_text(hjust = 0.5))
p2 <- boxplot1_gtm(mu = KS_mean_m, lb = KS_mean_lb, ub = KS_mean_ub, prnam = x_labels) + ylab("KS (mean)") +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- boxplot1_gtm(mu = KS_max_m, lb = KS_max_lb, ub = KS_max_ub, prnam = x_labels) + ylab("KS (max)") +
  theme(plot.title = element_text(hjust = 0.5))
windows();grid.arrange(grobs = list(p1, p2, p3), ncol = 3)


# Analyze convergence of sensitivity indices:
NN <- seq(N / 5, N, by = N / 5 )

#( Warning: the following line may take some time to run, as the computation of
# CDFs is costly):
pawn_conv <- pawn_convergence(X, Y, n, NN)
#15:31 - 15:33
KS_median_c <- pawn_conv$KS_median
KS_mean_c <- pawn_conv$KS_mean
KS_max_c <- pawn_conv$KS_max

KS_median_c <- do.call("rbind",KS_median_c)
KS_mean_c <- do.call("rbind",KS_mean_c)
KS_max_c <- do.call("rbind",KS_max_c)

# Plot convergence
dev.new()
windows();par(mfrow=c(3,1))
plot_convergence(NN, KS_median_c, xlab = "no of model executions", ylab = "KS (median)", labels = x_labels, panel.first = grid())
plot_convergence(NN, KS_mean_c, xlab = "no of model executions", ylab = "KS (mean)", labels = x_labels, panel.first = grid())
plot_convergence(NN, KS_max_c, xlab = "no of model executions", ylab = "KS (max)", labels = x_labels, panel.first = grid())

# Analyze convergence using bootstrapping to derive confidence intervals
#( Warning: the following line may take some time to run, as the computation of
# CDFs is costly):
pawn_conv <- pawn_convergence(X, Y, n, NN, Nboot)
KS_median_c <- pawn_conv$KS_median
KS_mean_c <- pawn_conv$KS_mean
KS_max_c <- pawn_conv$KS_max

# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

KS_stat <- KS_median_c
KS_median_c_m <- t(sapply(KS_stat,colMeans)) # mean
KS_median_c_lb <-  t(sapply(KS_stat,colQuantiles,probs=alfa/2)) # Lower bound
KS_median_c_ub <- t(sapply(KS_stat,colQuantiles,probs=1-alfa/2)) # Upper bound

KS_stat <- KS_mean_c
KS_mean_c_m <- t(sapply(KS_stat,colMeans)) # mean
KS_mean_c_lb <-  t(sapply(KS_stat,colQuantiles,probs=alfa/2)) # Lower bound
KS_mean_c_ub <- t(sapply(KS_stat,colQuantiles,probs=1-alfa/2)) # Upper bound

KS_stat <- KS_max_c
KS_max_c_m <- t(sapply(KS_stat,colMeans)) # mean
KS_max_c_lb <-  t(sapply(KS_stat,colQuantiles,probs=alfa/2)) # Lower bound
KS_max_c_ub <- t(sapply(KS_stat,colQuantiles,probs=1-alfa/2)) # Upper bound

# Plot convergence results:
dev.new()
windows();par(mfrow=c(3,1))
plot_convergence(NN, KS_median_c_m, KS_median_c_lb, KS_median_c_ub, xlab = "no of model executions", ylab = "KS (median)", labels = x_labels, panel.first = grid())
plot_convergence(NN, KS_mean_c_m, KS_mean_c_lb, KS_mean_c_ub, xlab = "no of model executions", ylab = "KS (mean)", labels = x_labels, panel.first = grid())
plot_convergence(NN, KS_max_c_m, KS_max_c_lb, KS_max_c_ub, xlab = "no of model executions", ylab = "KS (max)", labels = x_labels, panel.first = grid())


#### Step 6 (identification of influential and non-influential inputs) ####
# This is done by adding an articial 'dummy' input to the list of the model inputs. 
# The sensitivity indices for the dummy parameter estimate the approximation error of the
# sensitivity indices. For reference and more details, see help of the function pawn_indices

# Sensitivity indices using bootstrapping for the model inputs and the dummy input:
# Use bootstrapping to derive confidence bounds:
Nboot <- 1000
# Compute sensitivity indices for Nboot bootstrap resamples. We analyse KS_max
# only (and not KS_median and KS_mean) for screening purposes.
# (Warning: the following line may take some time to run, as the computation of
# CDFs is costly):
pawn_ind <- pawn_indices(X, Y, n, Nboot, dummy = TRUE)
#16:47  - 16:49
KS_max <- pawn_ind$KS_max # KS_max has dim (Nboot, M)
KS_dummy <- pawn_ind$KS_dummy # KS_dummy has dim (Nboot, 1)

# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

KS_stat <- KS_max
KS_max_m <- colMeans(KS_stat) # mean
KS_max_lb <- colQuantiles(KS_stat,probs=alfa/2) # Lower bound
KS_max_ub <- colQuantiles(KS_stat,probs=1-alfa/2) # Upper bound

KS_stat <- KS_dummy
KS_dummy_m <- mean(KS_stat) # mean
KS_dummy_lb <-  quantile(KS_dummy,alfa/2) # Lower bound
KS_dummy_ub <- quantile(KS_dummy,1-alfa/2) # Upper bound

# Combine KS max for all inputs and for dummy to plot
KS_max_d_m <- c(KS_max_m,KS_dummy_m) 
KS_max_d_lb <- c(KS_max_lb,KS_dummy_lb)
KS_max_d_ub <- c(KS_max_ub,KS_dummy_ub)

# Plot bootstrapping results:
dev.new()
windows();boxplot1_dummy(mu = KS_max_d_m, lb = KS_max_d_lb, ub = KS_max_d_ub, prnam = x_labels) + ylab("KS") +
  theme(plot.title = element_text(hjust = 0.5))

# Analyze convergence using bootstrapping to derive confidence intervals
#( Warning: the following line may take some time to run, as the computation of
# CDFs is costly):
NN <- seq(N / 5, N, by = N / 5 )
pawn_conv <- pawn_convergence(X, Y, n, NN, Nboot, dummy = TRUE)
#16:55 - vor 17:32
KS_median_c <- pawn_conv$KS_median
KS_mean_c <- pawn_conv$KS_mean
KS_max_c <- pawn_conv$KS_max
KS_dummy_c <- pawn_conv$KS_dummy

# Calculate statistics across bootstrap resamples (mean, lower and upper bounds of sensitivity indices):
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

KS_stat <- KS_max_c
KS_max_c_m <- t(sapply(KS_stat,colMeans)) # mean
KS_max_c_lb <-  t(sapply(KS_stat,colQuantiles,probs=alfa/2)) # Lower bound
KS_max_c_ub <- t(sapply(KS_stat,colQuantiles,probs=1-alfa/2)) # Upper bound

KS_stat <- KS_dummy_c
KS_dummy_c_m <- sapply(KS_stat,mean) # mean
KS_dummy_c_lb <-  sapply(KS_dummy_c,quantile,alfa/2) # Lower bound
KS_dummy_c_ub <- sapply(KS_dummy_c,quantile,1-alfa/2) # Upper bound

# Combine KS max for all inputs and for dummy to plot
KS_max_d_c_m <- unname(cbind(KS_max_c_m,KS_dummy_c_m))
KS_max_d_c_lb <- unname(cbind(KS_max_c_lb,KS_dummy_c_lb))
KS_max_d_c_ub <- unname(cbind(KS_max_c_ub,KS_dummy_c_ub))

#x_labels_dummy <- c("Sm", "beta", "alfa", "Rs", "Rf", "dummy")
x_labels_dummy <- c(x_labels, "dummy")

# Plot convergence results:
dev.new()
windows();plot_convergence(NN, KS_max_d_c_m, KS_max_d_c_lb, KS_max_d_c_ub, xlab = "no of model executions", ylab = "KS", labels = x_labels_dummy, panel.first = grid())


#### Step 7 (ADVANCED USAGE for Regional-Response Global Sensitivity Analysis) ####
# (Apply PAWN to a sub-region of the output range)

# Compute the PAWN index over a sub-range of the output distribution, for
# instance only output values above a given threshold:

thres = list(30)
Nboot <- 1000

pawn_ind_cond <- pawn_indices(X, Y, n, Nboot, output_condition = above, par = thres)
#17:34 - vor 18:32
KS_median_cond <- pawn_ind_cond$KS_median
KS_mean_cond <- pawn_ind_cond$KS_mean
KS_max_cond <- pawn_ind_cond$KS_max

# KS_median and KS_mean and KS_max have shape (Nboot, M)
# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

KS_stat <- KS_median_cond
KS_median_cond_m <- colMeans(KS_stat) # mean
KS_median_cond_lb <-  colQuantiles(KS_stat,probs=alfa/2) # Lower bound
KS_median_cond_ub <- colQuantiles(KS_stat,probs=1-alfa/2) # Upper bound

KS_stat <- KS_mean_cond
KS_mean_cond_m <- colMeans(KS_stat) # mean
KS_mean_cond_lb <-  colQuantiles(KS_stat,probs=alfa/2) # Lower bound
KS_mean_cond_ub <- colQuantiles(KS_stat,probs=1-alfa/2) # Upper bound

KS_stat <- KS_max_cond
KS_max_cond_m <- colMeans(KS_stat) # mean
KS_max_cond_lb <- colQuantiles(KS_stat,probs=alfa/2) # Lower bound
KS_max_cond_ub <- colQuantiles(KS_stat,probs=1-alfa/2) # Upper bound

dev.new()
p1 <- boxplot1_gtm(mu = KS_median_cond_m, lb = KS_median_cond_lb, ub = KS_median_cond_ub, prnam = x_labels) + ylab("KS (median)") +
  theme(plot.title = element_text(hjust = 0.5))
p2 <- boxplot1_gtm(mu = KS_mean_cond_m, lb = KS_mean_cond_lb, ub = KS_mean_cond_ub, prnam = x_labels) + ylab("KS (mean)") +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- boxplot1_gtm(mu = KS_max_cond_m, lb = KS_max_cond_lb, ub = KS_max_cond_ub, prnam = x_labels) + ylab("KS (max)") +
  theme(plot.title = element_text(hjust = 0.5))
windows();grid.arrange(grobs = list(p1, p2, p3), ncol = 3)
### end PAWN


#EET https://github.com/SAFEtoolbox/SAFE-R/blob/main/demo/workflow_eet_hymod.R
#minus redundant lines 
#
# This script provides an example of application of the Elementary Effects
# Test (EET) or "method of Morris" (Morris, 1991; Saltelli et al., 2008).
#
# The EET is a One-At-the-Time method for global Sensitivity Analysis.
# It computes two indices for each input:
# i) the mean (mi) of the EEs, which measures the total effect of an input
# over the output;
# ii) the standard deviation (sigma) of the EEs, which measures the degree
# of interactions with the other inputs.
# Both sensitivity indices are relative measures, i.e. their value does not
# have any specific meaning per se but it can only be used in pair-wise
# comparison (e.g. if input x(1) has higher mean EEs than input x(3) than
# x(1) is more influential than x(3)).
#
# For an application example in the environmental domain, see for instance
# Nguyen and de Kok (2007).
#
# MODEL AND STUDY AREA
#
# The model under study is the rainfall-runoff model Hymod
# (see help of function hymod_sim.m for more details) 
# applied to the Leaf catchment in Mississipi, USA
# (Sorooshian et al., 1983).
# The inputs subject to SA are the 5 model parameters, and the scalar 
# output for SA is a metric of model performance.
#
# INDEX
#
# Steps:
# 1. Add paths to required directories
# 2. Load data and set-up the HBV model
# 3. Sample inputs space
# 4. Run the model against input samples 
# 5. Compute the elementary effects
#
# REFERENCES
# 
# Morris, M.D. (1991), Factorial sampling plans for preliminary          
# computational experiments, Technometrics, 33(2).
#
# Nguyen, T.G. and de Kok, J.L. (2007). Systematic testing of an integrated
# systems model for coastal zone management using sensitivity and
# uncertainty analyses. Env. Mod. & Soft., 22, 1572-1587. 
#
# Saltelli, A., et al. (2008) Global Sensitivity Analysis, The Primer,
# Wiley.

# This script prepared by Francesca Pianosi and Fanny Sarrazin (for Matlab version)
# Isabella Gollini for the SAFER package,
# University of Bristol, 2014
# mail to: isabella.gollini@bristol.ac.uk

## Step 1 (load the package)

## Step 2 (setup the Hymod model)

# Load data:
data(LeafCatch)
dat <- LeafCatch[1:365,]

# Number of uncertain parameters subject to SA:
#M

# Parameter ranges (from literature):
#DistrPar

# Parameter distributions:
#DistrFun  <- "unif"

# Name of parameters (will be used to costumize plots):
#X_labels <- c("Sm","beta","alfa","Rs","Rf") 
X_labels <- x_labels

# Define output:
myfun <- "hymod_nse"
myfun <- "gtm_nse"


## Step 3 (sample inputs space)

r <- 100 # Number of Elementary Effects
# [notice that the final number of model executions will be equal to
# r * (M + 1)]

# # option 1: use the sampling method originally proposed by Morris (1991):
# L <- 6  # number of levels in the uniform grid
# design_type  <- "trajectory" # (note used here but required later)
# X <- Morris_sampling(r, xmin, xmax, L) # (r * (M + 1), M)

# option 2: Latin Hypercube sampling strategy
#SampStrategy <- "lhs" # Latin Hypercube
design_type <- "radial"
# other options for design type:
# design_type  = "trajectory"
X <- OAT_sampling(r, M, DistrFun, DistrPar, SampStrategy, design_type)

# Step 4 (run the model) 
#Y <- model_execution(myfun, X, dat = dat) # size (r*(M+1),1)
#sis 
Y <- model_execution(myfun, X, dat = results_g) # size (r*(M+1),1)
#18:59 - 19:25
## Step 5 (Computation of the Elementary effects)

# Compute Elementary Effects:
EETind <- EET_indices(r, DistrPar, X, Y, design_type)

EE <- EETind$EE
mi <- EETind$mi
sigma <- EETind$sigma 

# Plot results in the plane (mean(EE),std(EE)):

dev.new()
windows();EET_plot(mi, sigma,  xlab = "Mean of EEs", ylab = "Sd of EEs",  labels = X_labels)

# Use bootstrapping to derive confidence bounds:

Nboot <-100

EETind100 <- EET_indices(r, DistrPar, X, Y, design_type, Nboot)

EE <- EETind100$EE
mi <- EETind100$mi
sigma <- EETind100$sigma
mi_lb <- EETind100$mi_lb
mi_ub <- EETind100$mi_ub
sigma_lb <- EETind100$sigma_lb
sigma_ub <- EETind100$sigma_ub

# Plot bootstrapping results in the plane (mean(EE),std(EE)):
#EET_plot

dev.new()
windows();EET_plot(mi, sigma, mi_lb, mi_ub, sigma_lb, sigma_ub, labels = X_labels)

# Repeat computations using a decreasing number of samples so as to assess
# if convergence was reached within the available dataset:
rr <- seq(r / 5, r, by = r / 5)
EETconv<- EET_convergence(EE, rr)

m_r <- EETconv$m_r

# Plot the sensitivity measure (mean of elementary effects) as a function 
# of model executions:

dev.new()
windows();plot_convergence(rr * (M + 1), m_r, labels = X_labels, xlab = "no of model executions", ylab = "mean of EEs")

# Repeat convergence analysis using bootstrapping:
Nboot <- 100
rr <- seq(r / 5, r, by = r / 5)
EETconv100<- EET_convergence(EE, rr, Nboot)

m_r <- EETconv100$m_r
s_r <- EETconv100$s_r
m_lb_r <- EETconv100$m_lb_r
m_ub_r <- EETconv100$m_ub_r

dev.new()
windows();plot_convergence(rr * (M + 1), m_r, m_lb_r, m_ub_r, xlab = "no of model executions", ylab = "mean of EEs", labels = X_labels)
#END EET





#VBSA SOBOL ach nee, das ist gar nicht sobol proper ?! gibt auch noch nen FAST g SOBOL - jetzt nicht mehr machen

# INDEX
#
# Steps:
# 1. Add paths to required directories
# 2. Load data, set-up the Hymod model and define input ranges
# 3. Compute first-order (main effects) and total-order (total effects)
#    variance-based indices.
# 4: Example of how to repeat computions after adding up new 
#    input/output samples.
# 5. Example of how to compute indices when dealing with multiple outputs. 
#
# REFERENCES
#
# Sorooshian, S., Gupta, V., Fulton, J. (1983). Evaluation of maximum 
# likelihood parameter estimation techniques for conceptual rainfall-runoff
# models: Influence of calibration data variability and length on model 
# credibility. Water Resour. Res., 19, 251-259.

# This script prepared by Francesca Pianosi and Fanny Sarrazin (for Matlab version)
# Isabella Gollini for the SAFER package,
# University of Bristol, 2014
# mail to: isabella.gollini@bristol.ac.uk

## Step 1: load the packages - done abov

## Step 2: setup the model and define input ranges

# Load data:
data(LeafCatch)

dat <- LeafCatch[1:365,]

# Define input distribution and ranges:

M  <- 5 # number of uncertain parameters (Sm beta alfa Rs Rf )
M  <- 18 # sis number of uncertain parameters 
M <- length(DistrPar) # Number of inputs

DistrFun  <- "unif" # Parameter distribution
#DistrPar  <- list( c(0, 400), c(0, 2), c(0, 1), c(0, 0.1), c(0.1, 1)) #Parameter ranges (from literature)
#x_labels <- c("Sm", "beta", "alfa", "Rs", "Rf")

## Step 3: Compute first-order and total-order variance-based indices

#myfun <- "hymod_nse"
myfun <- "gtm_nse"

# Sample parameter space using the resampling strategy proposed by 
# (Saltelli, 2008; for reference and more details, see help of functions
# vbsa_resampling and vbsa_indices) 

SampStrategy <- "lhs"
N <- 3000 # Base sample size.

# Comment: the base sample size N is not the actual number of input 
# samples that will be executed In fact, because of the resampling
# strategy, the total number of model executions to compute the two
# variance-based indices is equal to N*(M+2) 

X <- AAT_sampling(SampStrategy, M, DistrFun, DistrPar, 2 * N)
#sis here, 2*N, not just N, in contrast to above

XABC <- vbsa_resampling(X)

# Run the model and compute selected model output at sampled parameter
# sets:

YA <- model_execution(myfun, XABC$XA, dat = results_g) # size (N,1)
#13:27 - 13:40
YB <- model_execution(myfun, XABC$XB, dat = results_g) # size (N,1)
#13:48 - 14:05
YC <- model_execution(myfun, XABC$XC, dat = results_g) # size (N*M,1)
#14:05 - 

# Compute main (first-order) and total effects:

ind <- vbsa_indices(YA, YB, YC)

Si <- ind[1,]
STi <- ind[2,] 


names(Si) <- x_labels
names(STi) <- x_labels

# Plot results:

# plot main and total separately
dev.new()

p1 <- boxplot1_gtm(Si, prnam = x_labels) + ggtitle("Si")
p2 <- boxplot1_gtm(STi, prnam = x_labels) + ggtitle("STi")
plot_grid(p1, p2, nrow = 2)

# plot both in one plot
dev.new()

windows();boxplot2(Si, STi, leg = c("main effects", "total effects"), labels = x_labels)

# Check the model output distribution (if multi-modal or highly skewed, the
# variance-based approach may not be adequate):
Y <- c(YA, YC)

dev.new()
windows();par(mfrow = c(1, 2))
plot(ecdf(Y), xlab = "NSE", ylab = "CDF", main ="")
plot(density(Y), xlab = "NSE", ylab = "PDF", main ="")

# Compute confidence bounds:
Nboot <- 500
ind500 <- vbsa_indices(YA,YB,YC, Nboot)

Si <- ind500[1,]
Si_lb <- ind500[3,]
Si_ub <- ind500[4,]

STi <- ind500[5,]
STi_lb <- ind500[7,]
STi_ub <- ind500[8,]

dev.new()

# plot main and total separately
p3 <- boxplot1_gtm(mu=Si, lb=Si_lb, ub=Si_ub, prnam = x_labels) + ggtitle("Si")
p4 <- boxplot1_gtm(mu=STi, lb=STi_lb, ub=STi_ub, prnam = x_labels) + ggtitle("STi")
windows();plot_grid(p3, p4, nrow = 2)

# plot both in one plot
dev.new()

windows();boxplot2(Si, STi, Si_lb, Si_ub, STi_lb, STi_ub, leg = c("main effects", "total effects"), labels = x_labels)

# Analyze convergence of sensitivity indices:
NN <- seq(N / 5, N, by = N / 5)

vbsaconv <- vbsa_convergence(c(YA, YB, YC), M, NN)

Sic <- vbsaconv$Si
STic <- vbsaconv$STi

dev.new()
windows();par(mfrow = c(1, 2))
plot_convergence(NN * (M + 2), Sic, xlab = "model evals", ylab = "main effect", labels = x_labels)
plot_convergence(NN * (M + 2), STic, xlab = "model evals", ylab = "total effect", labels = x_labels)

# With confidence bounds:

vbsaconv500 <- vbsa_convergence(c(YA, YB, YC), M, NN, Nboot = Nboot)

Sic <- vbsaconv500$Si
STic <- vbsaconv500$STi

Si_lbc <- vbsaconv500$Si_lb
Si_ubc <- vbsaconv500$Si_ub

STi_lbc <- vbsaconv500$STi_lb
STi_ubc <- vbsaconv500$STi_ub

dev.new()
windows();par(mfrow = c(1, 2))
plot_convergence(NN * (M + 2), Sic, Si_lbc, Si_ubc, xlab = "model evals", ylab = "main effect", labels = x_labels)
plot_convergence(NN * (M + 2), STic, STi_lbc, STi_ubc, xlab = "model evals", ylab = "total effect", labels = x_labels)

## Step 4: Adding up new samples

N2 <- 500 # increase of base sample size
# (that means: N2*(M+2) new samples that will need to be executed)

Xext <- AAT_sampling_extend(X, DistrFun, DistrPar, 2 * (N + N2)) # extended sample 

# (it includes the already executed samples X and the new ones)

Xnew <- Xext[-(1:(2 * N)),] # extract the new input samples that need to be executed

# # Resampling strategy:
XABC2 <- vbsa_resampling(Xnew)

# Execute model against new samples:
#sis das mache ich nicht, weil YC2 ca 8 h dauern wuerde
YA2 <- model_execution(myfun, XABC2$XA, dat = dat) # size (N2,1)
YB2 <- model_execution(myfun, XABC2$XB, dat = dat) # size (N2,1)
YC2 <- model_execution(myfun, XABC2$XC, dat = dat) # size (N2*M,1)


# Put new and old results toghether:
YAn <- c(YA, YA2) # should have length (N+N2)
YBn <- c(YB, YB2) # should have length (N+N2)
YCn <- rbind(matrix(YC, N, M), matrix(YC2, N2, M)) #  should have size (N+N2,M)
YCn <- c(YCn) # should have length ((N+N2)*M)

# Recompute indices:
Nboot <- 1000 

ind1000 <- vbsa_indices(YAn,YBn,YCn, Nboot)

Sin <- ind1000[1,]
Si_lbn <- ind1000[3,]
Si_ubn <- ind1000[4,]

STin <- ind1000[5,]
STi_lbn <- ind1000[7,]
STi_ubn <- ind1000[8,]

dev.new()

par(mfrow = c(1, 2))

boxplot2(Si, STi, Si_lb, Si_ub, STi_lb, STi_ub, main = paste(N*(M+2), "model eval."),  leg = c("main effects", "total effects"), labels = x_labels)

boxplot2(Sin, STin, Si_lbn, Si_ubn, STi_lbn, STi_ubn, main = paste((N+N2)*(M+2), "model eval."), leg = c("main effects", "total effects"), labels = x_labels)

# Step 5: case of multiple outputs 
# (In this example: RMSE and AME)

myfun <- "hymod_MulObj"
myfun <- "gtm_MulObj"
YA <- model_execution(myfun, XABC$XA, dat = results_g) # size (N,P)
#20:33-ca 21:30
YB <- model_execution(myfun, XABC$XB, dat = results_g) # size (N,P)
YC <- model_execution(myfun, XABC$XC, dat = results_g) # size (N*M,P)

# select the j-th model output:
j <- 1 
ind1 <- vbsa_indices(YA[, j], YB[, j],YC[, j])

Si1 <- ind1[1,]
STi1<- ind1[2,]

j <- 2
ind2 <- vbsa_indices(YA[, j], YB[, j],YC[, j])

Si2 <- ind2[1,]
STi2<- ind2[2,]

dev.new()
par(mfrow = c(1, 2))
boxplot2(Si1, STi1, leg = c("main effects", "total effects"), main = "RMSE", labels = x_labels)
boxplot2(Si2, STi2, leg = c("main effects", "total effects"), main = "BIAS", labels = x_labels)

# If you want to add samples in this case:
N2 <- 500 # increase of base sample size (see previous Step)
Xext <- AAT_sampling_extend(X, DistrFun, DistrPar, 2*(N+N2)) # extended sample 
Xnew <- Xext[-(1:(2 * N)),] # extract the new input samples that need to be executed
# Resampling strategy:
XABC2 <- vbsa_resampling(Xnew)
# Execute the model against new samples:
YA2 <- model_execution(myfun, XABC2$XA, dat = dat) # size (N2,2)
YB2 <- model_execution(myfun, XABC2$XB, dat = dat) # size (N2,2)
YC2 <- model_execution(myfun, XABC2$XC, dat = dat) # size (N2*M,2)

# Select the j-th model output:
j <- 1 

# Put new and old results toghether:
YAn <- c(YA[,j], YA2[,j]) # should have length (N+N2)
YBn <- c(YB[,j], YB2[,j]) # should have length (N+N2)
YCn <- rbind(matrix(YC[,j], N, M), matrix(YC2[,j], N2, M)) #  should have size (N+N2,M)
YCn <- c(YCn) # should have length ((N+N2)*M)


ind1n <- vbsa_indices(YAn, YBn, YCn)

Si1n <- ind1n[1,]
STi1n<- ind1n[2,]

dev.new()
par(mfrow = c(1, 2))
boxplot2(Si1, STi1, main = paste(N*(M+2), "model eval."),  leg = c("main effects", "total effects"), labels = x_labels)
boxplot2(Si1n, STi1n, main = paste((N+N2)*(M+2), "model eval."), leg = c("main effects", "total effects"), labels = x_labels)


#end VBSA SOBOHL






i = 2; g = 1 #g =2 --> "P"


results <- gtm()

#ggplot requires the data to be in data frame
results_df <- as.data.frame(results)
maxmodelfauna <-max(results_df$fauna)
#combine measured data with modelled data to calculate model accuracy - for that, join model results to measured values
my.formula <- y ~ x



setwd(gw_FuldaEcosystemServices_results_txt_path) 
write.table(results_df, paste0("results_df_run_",run,".txt"), row.names = FALSE)

#for plotting several variables, make long form of the results data frame
results_df_long <- results_df %>%
  tidyr::pivot_longer(cols = c(BOC, DETRITUS, MO_het, fauna), names_to = "variable") 

write.table(results_df, paste0("results_df_long_run_",run,".txt"), row.names = FALSE)


#read in data - outcomment this block for fresh data !
#in order to produce plots which compare the runs, read the run results from files saved previously
setwd(gw_FuldaEcosystemServices_results_txt_path)

results_df_1 <- read.table( "results_df_run_1.txt", header = TRUE)
results_df_2 <- read.table( "results_df_run_2.txt", header = TRUE)
results_df_3 <- read.table( "results_df_run_3.txt", header = TRUE)
results_df_4 <- read.table( "results_df_run_4.txt", header = TRUE)
results_df_5 <- read.table( "results_df_run_5.txt", header = TRUE)
results_df_6 <- read.table( "results_df_run_6.txt", header = TRUE)

results_df_1$run <- 1
results_df_2$run <- 2
results_df_3$run <- 3
results_df_4$run <- 4
results_df_5$run <- 5
results_df_6$run <- 6

#if using one of the saved ones for producing the  figures below, prepare the respective data, by replacing xx with the number of the run
# results_df <- results_df_xx
# results_df$dateRi <- as.Date(results_df$dateRi)
# results_df$group_letter <- factor(results_df$group_letter , levels = c("R", "M", "P", "A"))
# run <- xx

#e.g. for plotting from previously saved file
run <- 1
results_df <- results_df_1
results_df$dateRi <- as.Date(results_df$dateRi)
results_df$group_letter <- factor(results_df$group_letter , levels = c("R", "M", "P", "A"))
#END block read in data - outcomment for fresh data !


unified_axes <- 1 # 1 = make the same axis for all four subplots , representing the four groups . 0 = axes reflect the groups' minima and maxima




#combine measured data with modelled data to calculate model accuracy - for that, join model results to measured values
DETR_1978_1981_mean_per_group_and_date <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(OS_mol_COD_L)) %>%
  dplyr::group_by(Date, kmeans4gr)%>%
  dplyr::summarise(OS_mol_COD_L = mean(OS_mol_COD_L, na.rm = TRUE)) %>%
  dplyr::mutate(ID = paste(Date, kmeans4gr, sep = "_"))%>%
  dplyr::mutate(x = OS_mol_COD_L)

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

chem_ordered_per_date_1978_1981$group_letter <- factor(chem_ordered_per_date_1978_1981$group_letter, levels = c("R", "M", "P", "A"))

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




Fulda_Detritus_partOrganics_plot <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(OS_mol_COD_L))%>%
  ggplot() +
  geom_point(data = results_df, aes(x = dateRi, y = DETRITUS, colour = as.factor(group_letter)),  pch = 16, size = 0.5, show.legend = FALSE 
  )+
  geom_point(aes(x = Date, y = OS_mol_COD_L, fill = as.factor(group_letter)),  pch = 21 , colour = "black", show.legend = FALSE
  )+
  scale_x_date(limits = c(t_0, t_max) )+
  labs(x = "Date", y = "Detritus\n [mol COD/ L]\n measured [o] and\nmodelled [.]")+ 
  #TODO updATE  scale_fill_okabe_ito()+ 
  #TODO updATE  scale_color_okabe_ito()+
  
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
        axis.text.x = element_text(angle = 45, vjust = 0.4
        )
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

(Fulda_Detritus_partOrganics_plot_trends <- Fulda_Detritus_partOrganics_plot +
    geom_smooth(method = "lm",
                data = results_df, aes(x = dateRi, y = DETRITUS),
                se=TRUE,  formula = my.formula, lwd = 0.3) +
    stat_smooth(method = "lm",
                data = results_df, aes(x = dateRi, y = DETRITUS)
                ,  formula = my.formula, lwd = 0.3)
)



#TODO careful - I replaced results_df by results
max_BOC <- max(chem_ordered_per_date_1978_1981$BOC_mol_COD_L, results$BOC, na.rm =TRUE)
#TODO in offline this max now moved doen

#combine measured data with modelled data to calculate model accuracy - for that, join model results to measured values
chem_ordered_per_date_1978_1981_BOC_joined <- left_join(chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date, results_df, by =  c("Date" = "dateRi", "kmeans4gr" = "group"))

i <- 1 # M
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

max_BOC <- max(chem_ordered_per_date_1978_1981$BOC_mol_COD_L, results_df$BOC, na.rm =TRUE)

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
  labs(x = "Date", y = "BOC\n [mol COD / L]\nmeasured [o] and\nmodelled [.]")+ 
  #TODO scale_fill_okabe_ito()+ 
  #TODO scale_color_okabe_ito()+
  
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
        
        legend.key = element_blank()
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




#combine measured data with modelled data to calculate model accuracy - for that, join model results to measured values
MO_het_1978_1981_mean_per_group_and_date <- chem_ordered_per_date_1978_1981 %>%
  dplyr::filter(!is.na(total_Prok_mol_COD_L)) %>%
  dplyr::group_by(Date, kmeans4gr)%>%
  dplyr::summarise(MO_het_mol_COD_L = mean(total_Prok_mol_COD_L, na.rm = TRUE)) %>%
  dplyr::mutate(ID = paste(Date, kmeans4gr, sep = "_"))%>%
  dplyr::mutate(x = MO_het_mol_COD_L)

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
geomtexttable_MO$y_R2 <- geomtexttable_MO$y-max(results_df$MO_het)*.01
geomtexttable_MO$y_MAE <- geomtexttable_MO$y-max(results_df$MO_het)*.15
geomtexttable_MO$y_RMSE <- geomtexttable_MO$y-max(results_df$MO_het)*.35
geomtexttable_MO$y_NSE <- geomtexttable_MO$y-max(results_df$MO_het)*.55
geomtexttable_MO$y_MB <- geomtexttable_MO$y-max(results_df$MO_het)*.75
geomtexttable_MO$y_N <- geomtexttable_MO$y-max(results_df$MO_het)*.95
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
  
  labs(x = "Date", y = "Microbial dry mass\n[mol COD / L]\nmeasured [o] and\nmodelled [.]")+ 
  
  #TODO update scale_fill_okabe_ito()+ 
  #TODO update scale_color_okabe_ito()+
  
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
        axis.text.x = element_text(angle = 45, vjust = 0.4
        )
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


fauna_deep_PerSamplPerTaxonWide_bm_sum$group <- fauna_deep_PerSamplPerTaxonWide_bm_sum$kmeans4gr

fauna_deep_PerSamplPerTaxonWide_bm_sum$group_letter <- ifelse(fauna_deep_PerSamplPerTaxonWide_bm_sum$group ==  2, "P", ifelse(fauna_deep_PerSamplPerTaxonWide_bm_sum$group == 3, "R" , ifelse (fauna_deep_PerSamplPerTaxonWide_bm_sum$group == 4, "A", ifelse (fauna_deep_PerSamplPerTaxonWide_bm_sum$group == 1, "M", NA))))

fauna_deep_PerSamplPerTaxonWide_bm_sum$group_letter <- factor(fauna_deep_PerSamplPerTaxonWide_bm_sum$group_letter , levels = c("R", "M", "P", "A"))


maxmodelfauna <-max(results_df$fauna)
fauna_deep_PerSamplPerTaxonWide_bm_sum_no_high_biomass  <- fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
  dplyr::filter(bm_mol_COD_perL < 0.00001) #for visualization - explained in text and caption
maxfaunaplot <-max(fauna_deep_PerSamplPerTaxonWide_bm_sum_no_high_biomass$bm_mol_COD_perL)


fauna_deep_PerSamplPerTaxonWide_bm_sum_for_plot <- fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
  dplyr::filter(!is.na(bm_mol_COD_perL))%>%
  dplyr::filter(!is.na(kmeans4gr))




#combine measured data with modelled data to calculate model accuracy - for that, join model results to measured values
fauna_mean_per_group_and_date <- fauna_deep_PerSamplPerTaxonWide_bm_sum %>%
  dplyr::filter(!is.na(bm_mol_COD_perL)) %>%
  dplyr::group_by(dateRi, kmeans4gr)%>%
  dplyr::summarise(bm_mol_COD_perL = mean(bm_mol_COD_perL, na.rm = TRUE)) %>%
  dplyr::mutate(ID = paste(dateRi, kmeans4gr, sep = "_"))%>%
  dplyr::mutate(x = bm_mol_COD_perL)

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
  labs(x = "Date", y = "Fauna dry mass\n[mol COD / L]\nmeasured [o] and\nmodelled [.]")+ 
  
  #TODO update scale_fill_okabe_ito()+ 
  #TODO update scale_color_okabe_ito()+
  
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
        axis.text.x = element_text(angle = 45, vjust = 0.4
        )
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
  #save as png AND as pdf
  ggsave(paste0("model_measured_run_",run,"_with_MO_one_y_scale_w_model_qual.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_run_",run,"_with_MO_one_y_scale_w_model_qual.pdf"), width = 10, height = 9)
}else{
  #save as png AND as pdf
  ggsave(paste0("model_measured_run_",run,"_with_MO_free_y_scale_w_model_qual.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_run_",run,"_with_MO_free_y_scale_w_model_qual.pdf"), width = 10, height = 9)
}


(Fulda_trends <- Fulda_prec_plotted +  Fulda_Detritus_partOrganics_plot_trends  + Fulda_BOC_plot_trends + Fulda_MO_plot_trends + Fulda_fauna_plot_trends + plot_layout(ncol = 1) +
    plot_annotation(tag_levels = "a", tag_suffix = ")"))

setwd(gw_FuldaEcosystemServices_plots_path)
if (unified_axes == 1) {
  ggsave(paste0("model_measured_trend_run_",run,"_with_MO_one_y_scale_w_model_qual.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_trend_run_",run,"_with_MO_one_y_scale_w_model_qual.pdf"), width = 10, height = 9)
}else{
  ggsave(paste0("model_measured_trend_run_",run,"_with_MO_free_y_scale_w_model_qual.png"), width = 10, height = 9)
  ggsave(paste0("model_measured_trend_run_",run,"_with_MO_free_y_scale_w_model_qual.png"), width = 10, height = 9)
}




results_df__for_overview_all <- rbind(results_df_1, results_df_2, results_df_3, results_df_4, results_df_5, results_df_6)


setwd(gw_FuldaEcosystemServices_results_txt_path)

write.table(results_df__for_overview_all, "results_df__for_overview_all_for_barplot_1_2_3_4_5_6__.txt", row.names = FALSE)

results_df__for_overview_all$dateRi <- as.Date(results_df__for_overview_all$dateRi)

results_df__for_overview_all$group_letter <- factor(results_df__for_overview_all$group_letter , levels = c("R", "M", "P", "A"))


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
names(lm_i_list) <- sub("fitted_conc_t_0.1", "fitted_conc_t_0", names(lm_i_list) )
names(lm_i_list) <- sub("fitted_conc_t_max.1342", "fitted_conc_t_max", names(lm_i_list) )

lm_i_list_ <- lm_i_list %>%
  dplyr::mutate(group = ifelse(group_letter==3,"R",ifelse(group_letter==2,"P",ifelse(group_letter==4,"A",ifelse(group_letter==1,"M","z"))))) %>%
  dplyr::select(c("run", "group", "variable", "coefficient_intercept", "coefficient_slope",  "F_value", "p_val", "fitted_diff_over_observation_period", "conc_t_0", "conc_t_max", "fitted_conc_t_0", "fitted_conc_t_max", "group_letter", "max_"))

setwd(gw_FuldaEcosystemServices_results_txt_path)
write.table(lm_i_list_, "results_df__lm_1_2_3_4_5_6__fp.txt", row.names = FALSE)
# lm_i_list<- read.table( "results_df__lm_1_2_3_4_5_6__fp.txt", header = TRUE)

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
    
    labs(x = "Date", y = "BOC [mol COD / L]") +
    theme(panel.background = element_rect(fill = "white",  colour = "black", 
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank()
    )
)

if (unified_axes == 1) {
  
  (plot_trends_BOC <- plot_trends_BOC +
     facet_wrap(.~group_letter, ncol = 4))
  
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_COD <- plot_trends_COD+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_BOC_1_2_3_4_5_6__free_y_scale.pdf"), width = 8, height = 3)
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
          legend.background=element_blank()
    )+
    facet_wrap(.~group_letter, scales="free_y", ncol = 4)
)
setwd(gw_FuldaEcosystemServices_plots_path)
ggsave("plot_trends_MO_1_2_3_4_5_6__.png", width = 8, height = 2.5)
ggsave("plot_trends_MO_1_2_3_4_5_6__.pdf", width = 8, height = 2.5)



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
    
    labs(x = "Date", y = "Fauna [mol COD / L]") +
    theme(panel.background = element_rect(fill = "white",  colour = "black",  linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank()
    )+
    facet_wrap(.~group_letter, scales="free_y", ncol = 4)
)
setwd(gw_FuldaEcosystemServices_plots_path)
ggsave("plot_trends_fauna_1_2_3_4_5_6__.png", width = 8, height = 2.5)
ggsave("plot_trends_fauna_1_2_3_4_5_6__.pdf", width = 8, height = 2.5)

