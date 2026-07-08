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
library(devtools) #TODO what for?!

#install package for color blind-safe plots
# install.packages("ggokabeito")
# #You can alternatively install the development version of ggokabeito from GitHub with:
#   devtools::install_github("malcolmbarrett/ggokabeito")
library(ggokabeito) # #scale_fill_okabe_ito
library(ggplot2)
#install.packages("ggtext") #
library(ggtext)#for exponents in ylab in ggplot, and element_textbox_simple


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
#do once install_github("SAFEtoolbox/SAFE-R")
library(SAFER)

rm(list = ls()) #remove any variables and data created before, to make sure that this scenarios with fresh data

##########
#the scenarios with the different parameters are listed in the excel file "parameters.xlsx"; 1 to 8. Change accordingly
##########
scenario <- 1

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

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/error_measures.R"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/gtm.r"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/gtm_sim.r"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/gtm_MulObj.r"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/gtm_MulOut.r"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/gtm_nse.r"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/model_execution_gtm.r"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/scatter_plots_gtm.r"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/scatter_plots_tr_gtm.R"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/parcoord_gtm.r"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/boxplot1_GTM.r"
source(urlfiletext)

urlfiletext <- "https://raw.github.com/suisch/gtm_River_Fulda_floodplain/main/plot_convergence_gtm.r"
source(urlfiletext)#


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
names(fulda_variables_read_in) <- c("Fulda_daily_prec", "Fulda_daily_temp_", "chem_ordered_per_date_1978_1981", "chem_ordered_per_date_1978_1981_mean_per_group", "fauna_deep_PerSamplPerTaxonWide_bm_sum", "fauna_deep_PerSamplPerTaxon_bm_mean_per_group", "t_0", "t_max", "DETRITUS_gr1_t0", "DETRITUS_gr2_t0", "DETRITUS_gr3_t0", "DETRITUS_gr4_t0", "BOC_gr1_t0", "BOC_gr2_t0", "BOC_gr3_t0", "BOC_gr4_t0", "MO_het_gr1_t0", "MO_het_gr2_t0", "MO_het_gr3_t0", "MO_het_gr4_t0", "fauna_gr1_t0", "fauna_gr2_t0", "fauna_gr3_t0", "fauna_gr4_t0",  "CC_table_MO", "CC_table_fauna", "Fulda_daily_temp_joh_long") 

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

#join BOC measured / observed to the results data frame for the error measures to be calculated between observed and modelled BOC
results_joined <- dplyr::left_join(results, chem_ordered_per_date_1978_1981_mean_BOC_per_group_and_date, by =  c( "dateRi" = "Date" , "group" = "kmeans4gr"  ))

#sensitivity analysis leaning heavily on https://github.com/SAFEtoolbox/SAFE-R/blob/main/demo/workflow_rsa_hymod.R

# Define inputs:
DistrFun  <- "unif" # Parameter distribution

DistrPar  <- list( 
  c(0.001, .8), #yield_ac 
  c(0.00000001, .0001), #K_ac 
  c(0.00000001, .1), #factor_CC_MO
  c(1, 20), #rMO_COD_uptake_per_day_at_lab_temperature
  c(500,2000),#average_precipitation_mm_yr
  c(0.5,1),#recharge_fraction_of_precipitation # was 1 in th efirst 6 scenarios
  c(0.1, 1),#TOC_mol_m2_yr_precipitation
  c(1,20),#factor_how_many_times_Detritus_compared_to_TOC
  c(.0000001, 1),#k1 
  c(1,50),#aquifer_depth #10
  c(.00000001, 1),#K_MO_at_temp
  c(.001,.1),#yield_MO
  c(0.001, .5),#rFauna_MO_uptake_per_day_at_TEMP
  c(.0001, .1),#excretionRate 
  c(0.0001, 0.1),#mortalityRate #15
  c(.01,1),#factor_CC_fauna
  c(.001,0.5)#mortalityFraction_per_degree
) #Parameter ranges (ideally from literature, but mostly not available)


# vector of abbreviated variable names
x_labels <- c(
  "yield_ac",
  "K_ac", 
  "fact_CC_MO", 
  "rMO_C_upt",
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

#loop over the groups for executing the sensitivity analysis. For each group, this might take hours.
for (g in 1:length(unique(results$group))){
  CC_group_MO_g <- CC_table_MO$CC[CC_table_MO$group == uniquegroupvector[g]]
  CC_group_fauna_g <- CC_table_fauna$CC[CC_table_fauna$group == uniquegroupvector[g]]
  group_letter_g <- unique(results$group_letter[results$group == uniquegroupvector[g]])
  
  results_g <- results_joined %>%
    dplyr::filter(group_letter == group_letter_g)
  
  myfun <- "gtm_MulObj" # calls gtm_sim which is the actual time-stepped groundwater trophic model
  
  ## Step 3 (sample inputs space) from github https://github.com/SAFEtoolbox/SAFE-R/blob/main/demo/workflow_rsa_hymod.R
  SampStrategy <- "lhs" # Latin Hypercube
  N <- 3400 # Number of samples; 3400 as 20*17 parameters here; 3000 in https://github.com/SAFEtoolbox/SAFE-R/blob/main/demo/workflow_rsa_hymod.R
  M <- length(DistrPar) # Number of inputs
  X <- AAT_sampling(SampStrategy, M, DistrFun, DistrPar, N)
  colnames(X) <- x_labels
  
  ## Step 4 (run the model) 
  Y <- model_execution_gtm(myfun, X, dat = results_g)  # size (N,2).  X are the parameters
  colnames(Y) <- c("rmse", "bias")
  # if code does not work, try increasing N first   
  
  ## Step 5a (Regional Sensitivity Analysis with threshold)
  
  # (**) Note: if you want to use input/output samples generated in
  # another programme, them here and save them in two matrix 
  # input : X = (N x M)
  # output: Y = (N x P)
  # [N=number of samples; M=number of inputs; P=number of outputs]
  
  # Visualize input/output samples (this may help finding a reasonable value
  # for the output threshold):
  
  scatter_plots(X,Y[,1]) + ylab("rmse")
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("scatter_plots_RMSE_group_",g ,".png"))
  
  # use dev.new() if you want to open the plot in a new window
  #dev.new() #does not always work on visual studio code; #https://stackoverflow.com/questions/52284345/how-to-show-r-graph-from-visual-studio-code. Workaround is to do windows() before a call to a plot
  

  scatter_plots(X,Y[,2]) + ylab("bias")
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("scatter_plots_bias_group_",g ,".png"))
  
  
  # Set output threshold:
  rmse_thres <- .000044    #  threshold for the first obj. fun.
  bias_thres <- 0.00009  # behavioural threshold for the second obj. fun.
  
  #group 1
  if (g == 1) {
    rmse_thres <- .00075  #  threshold for the first obj. fun.
    bias_thres <- 0.00044  # behavioural threshold for the second obj. fun.
  }
  
  #group 2
  if (g == 2) {
    rmse_thres <- .0001
    bias_thres <- 0.00002
  }
  
  #group 3
  if (g == 3) {
    rmse_thres <- .0008
    bias_thres <- 0.00026 
  }
  
  #group 4
  if (g == 4) {
    rmse_thres <- .0001
    bias_thres <- 0.001
  }
  
  # RSA (find behavioural parameterizations):
  threshold <- c(rmse_thres, bias_thres)
  
  rsatr <- RSA_indices_thres(X, Y, threshold) 
  mvd <- rsatr$stat
  idxb <- rsatr$idxb # True False - for colouring in one of the next plots
  
  # Highlight the behavioural parameterizations in the scatter plots:
  
  dev.new() # does not work in visual studio code
  scatter_plots_tr_gtm(X, Y[,1], prnam = x_labels, idxb) + ylab("rmse")
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("scatter_plots_RMSE_mvd_idxb_group_",g ,".png"))
  
  dev.new()# does not work in visual studio code
  scatter_plots_tr_gtm(X, Y[,2], prnam = x_labels, idxb) + ylab("bias")
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("scatter_plots_bias_mvd_idxb_group_",g ,".png"))
  
  # Plot parameter CDFs:
  dev.new()# does not work in visual studio code
  RSA_plot_thres(X, idxb, prnam = x_labels, threshold = threshold) #threshold has two values, one for rmse, one for bias
  #this plot might not work, reasons unknown; therefore outcommented
  #setwd(gw_FuldaEcosystemServices_plots_path)
  #ggsave(paste0("RSA_plot_thres_group_",g ,".png"))
  
  
  
  # Check the ranges of behavioural parameterizations by
  # Parallel coordinate plot:
  
  mycol <- idxb
  mycol[idxb == FALSE] <- gray(.7, alpha = .7)
  mycol[idxb == TRUE] <-  gray(0, alpha = .7)
  
  dev.new()# does not work in visual studio code
  parcoord_gtm(X, col = mycol, plotorder = idxb)
  pdf(paste0("Parallel_coordinate_plot_group_",g ,"_.pdf"))
  parcoord_gtm(X, col = mycol, plotorder = idxb)
  dev.off()
  
  
  
  # Plot the sensitivity indices (maximum vertical distance between
  # parameters CDFs):
  
  dev.new()
  boxplot1_gtm(mu = mvd, prnam = x_labels) 
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
  
  dev.new()
  boxplot1_gtm(mu = mvd, lb = mvd_lb, ub = mvd_ub, prnam = x_labels) 
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("boxplot_mvd_bootstrap_group_",g ,".png"), width = 8, height = 4)
  
  # Repeat computations using an increasing number of samples so to assess
  # convergence:
  NN <- seq(N / 5, N, by = N / 5 )
  mvd <- RSA_convergence_thres(X, Y[,1], NN, threshold = rmse_thres) 
  
  mvd_st <- mvd$stat
  
  # Plot the sensitivity measures (maximum vertical distance between
  # parameters CDFs) as a function of the number of samples:
  
  dev.new()#
  plot_convergence_gtm(NN, mvd_st, xlab = "no of samples", ylab = "mvd", labels = x_labels)
  pdf(paste0("plot_convergence_group_",g ,"_.pdf"))
  plot_convergence_gtm(NN, mvd_st, xlab = "no of samples", ylab = "mvd", labels = x_labels)
  dev.off()
  
  # Repeat convergence analysis using bootstrapping to derive
  # confidence bounds:
  
  Nboot <- 1000
  #if that does not work, and Nboot <- 600 does not work, try e.g. with Nboot <- 3 - that gives at least an impression
  rsatr_b_conf <- RSA_convergence_thres(X, Y[,1], NN,  threshold = rmse_thres, Nboot = Nboot) 
  mvd <- rsatr_b_conf$stat
  idxb <- rsatr_b_conf$idxb
  
  mvd_lb <- rsatr_b_conf$stat_lb
  mvd_ub <- rsatr_b_conf$stat_ub
  
  dev.new()
  setwd(gw_FuldaEcosystemServices_plots_path)
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
  #dev.new()
  #RSA_plot_groups(X, idx, Yk, prnam = x_labels) + ylab("rmse")
  #might not work, -   #same error as for CDF - this IS CDF again, - therefore outcommented
  
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
  mvd_max_ub <- colQuantiles(mvd_max,probs = 1-alfa/2) # Upper bound
  
  # Plot results:
  
  dev.new()
  boxplot1_gtm(mu = mvd_median_m, lb = mvd_median_lb, ub = mvd_median_ub, prnam = x_labels) + ylab("mvd median") 
  setwd(gw_FuldaEcosystemServices_plots_path)
  ggsave(paste0("boxplot_mvd_bootstrap_resample_group_",g ,".png"), width = 8, height = 4)
  ##### end regional sensitivity analysis
}#end loop through groups







# for plotting the results, need to derive the results in a separate data frame
results <- gtm()

#ggplot requires the data to be in data frame
results_df <- as.data.frame(results)


#TODO where needed ?
maxmodelfauna <-max(results_df$fauna)
#combine measured data with modelled data to calculate model accuracy - for that, join model results to measured values
my.formula <- y ~ x


unified_axes <- 1 # 1 = make the same axis for all four subplots , representing the four groups . 0 = axes reflect the groups' minima and maxima
unified_axes_fauna <- 1







########
###  reading in the complete set of scenario runs, for calculating the linear model fits to the GTM model scenarios

setwd(gw_FuldaEcosystemServices_results_txt_path)
results_df__for_overview_all <- read.table("results_df__for_overview_all__1_2_3_4_5_6_7_8_9_10.txt", header = TRUE)

results_df__for_overview_all$dateRi <- as.Date(results_df__for_overview_all$dateRi)

results_df__for_overview_all$group_letter <- factor(results_df__for_overview_all$group_letter , levels = c("R", "M", "P", "A"))

results_df__for_overview_all_long <- results_df__for_overview_all %>%
  dplyr::group_by(dateRi, group, group_letter, scenario) %>%
  tidyr::pivot_longer(cols = c(DETRITUS, BOC, MO_het, fauna), names_to = "variable", values_to = "value")


########
### restrainging only to the six temperature and fauna scenarios
results_df__for_overview_all_1_to_6 = results_df__for_overview_all %>%
  dplyr::filter(scenario %in% c(1:6))



######
#trend plots with the extreme k1 
######
#TODO extreme Scenarios 7 and 8 are the scenarios of the first of the two  most sensitive parameters, i.e. k1, yielding Fig.  Figs. X in SI Sx



#prepare data

#reading in the already existing file with all the GTM model scenarios 
results_df__for_overview_all_1_to_6 <- read.table( "results_df__for_overview_all__1_2_3_4_5_6_7_8_9_10.txt", header = TRUE)%>%
dplyr::filter(scenario %in% 1:6)
results_df__for_overview_all_1_to_6$dateRi <- as.Date(results_df__for_overview_all_1_to_6$dateRi)

results_df__for_overview_all_1_to_6$group_letter <- factor(results_df__for_overview_all_1_to_6$group_letter , levels = c("R", "M", "P", "A"))

my.formula <- y ~ x

######
#trend plots with the extreme k1 which was one of the two most sensitive parameters
######
setwd(gw_FuldaEcosystemServices_results_txt_path)
#as above, read in the GTM model results and smooth the linear model fits on the fly



results_df__for_overview_all_1_to_8 <- read.table("results_df__for_overview_all__1_2_3_4_5_6_7_8_9_10.txt", header = TRUE) %>%
dplyr::filter(scenario %in% c(1:8))

results_df__for_overview_all_1_to_8$dateRi <- as.Date(results_df__for_overview_all_1_to_8$dateRi)

results_df__for_overview_all_1_to_8$group_letter <- factor(results_df__for_overview_all_1_to_8$group_letter , levels = c("R", "M", "P", "A"))

results_df__for_overview_all_1_to_8_long <- results_df__for_overview_all_1_to_8 %>%
  dplyr::group_by(dateRi, group_letter, scenario) %>%
  tidyr::pivot_longer(cols = c(DETRITUS, BOC, MO_het, fauna), names_to = "variable", values_to = "value")


# now the frist 6 in grey because they had been plotted in detail already, and the extreme scenarios in other colours


#colors chosen according to https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
#colours of scenarios 1 to 6 in grey, because they had been shown in detail in previous figures
results_df__for_overview_all_1_to_8$colour_for_plot <- ifelse(results_df__for_overview_all_1_to_8$scenario == 1, "#1a1b1a",ifelse(results_df__for_overview_all_1_to_8$scenario == 3, "#2c2c2c", ifelse(results_df__for_overview_all_1_to_8$scenario == 5, "#353636", ifelse(results_df__for_overview_all_1_to_8$scenario == 2, "#373838", ifelse(results_df__for_overview_all_1_to_8$scenario == 4, "#009E73", ifelse(results_df__for_overview_all_1_to_8$scenario == 6, "#2d2e2e",
   ifelse(results_df__for_overview_all_1_to_8$scenario == 7, "#0072B2", 
   ifelse(results_df__for_overview_all_1_to_8$scenario == 8, "#E69F00", "black"))))))))

results_df__for_overview_all_1_to_8$line <- ifelse(results_df__for_overview_all_1_to_8$scenario == 1, 2,ifelse(results_df__for_overview_all_1_to_8$scenario == 3, 2, ifelse(results_df__for_overview_all_1_to_8$scenario == 5, 2,  ifelse(results_df__for_overview_all_1_to_8$scenario == 2, 2, ifelse(results_df__for_overview_all_1_to_8$scenario == 4, 2, ifelse(results_df__for_overview_all_1_to_8$scenario == 6, 2, ifelse(results_df__for_overview_all_1_to_8$scenario == 7, 1, ifelse(results_df__for_overview_all_1_to_8$scenario == 8, 1, 1))))))))

results_df__for_overview_all_1_to_8$linewidth <- ifelse(results_df__for_overview_all_1_to_8$scenario == 1, .8,ifelse(results_df__for_overview_all_1_to_8$scenario == 3, .8, ifelse(results_df__for_overview_all_1_to_8$scenario == 5, .8, ifelse(results_df__for_overview_all_1_to_8$scenario == 2, .8, ifelse(results_df__for_overview_all_1_to_8$scenario == 4, .8, ifelse(results_df__for_overview_all_1_to_8$scenario == 6, .8, ifelse(results_df__for_overview_all_1_to_8$scenario == 7, 2.1, ifelse(results_df__for_overview_all_1_to_8$scenario == 8, 2.11, 1))))))))

#the two extreme k1 scenarios are plotted on top of the eight scenarios in order to get only their smoothing parameters
df2 = results_df__for_overview_all_1_to_8 %>%
  dplyr::filter(scenario == 7)%>%
  data.frame()

df3 = results_df__for_overview_all_1_to_8 %>%
  dplyr::filter(scenario == 8)%>%
  data.frame()



################## trend plots of the first of the two extreme scenarios of the two most sensitive parameters
#k1

(plot_trends_BOC <- ggplot(data = results_df__for_overview_all_1_to_8)+
    geom_smooth(method = "lm",
                data = results_df__for_overview_all_1_to_8, aes(x = dateRi, y = BOC, group = as.factor(colour_for_plot)#, #lty =  as.factor(line), lwd = as.factor(linewidth) 
                ), 
                se=FALSE,  formula = my.formula, show.legend = FALSE, colour = "grey", lwd = .5
                 )+ 
   
  new_scale_colour() + #requires library(ggnewscale; needed, to be able to plot 6 grey lines and two coloered ones acc to colour_for_plot, correct?
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
#trend plots with the extreme Yac microbial yield 
######
#delete todo
#TODO extreme Scenarios 9 and 10 of the second of the  two  most sensitive parameters, i.e. microbial yield, yielding 
#TODO Fig.  Figs. X in SI Sx

#the code from here requires reading in the results as in the block marked for outcommenting above 
setwd(gw_FuldaEcosystemServices_results_txt_path)
results_df__for_overview_all_1_to_6_9_10 <- read.table("results_df__for_overview_all__1_2_3_4_5_6_7_8_9_10.txt", header = TRUE) %>%
dplyr::filter(scenario %in% c(1:6,9,10))


results_df__for_overview_all_1_to_6_9_10$dateRi <- as.Date(results_df__for_overview_all_1_to_6_9_10$dateRi)

results_df__for_overview_all_1_to_6_9_10$group_letter <- factor(results_df__for_overview_all_1_to_6_9_10$group_letter , levels = c("R", "M", "P", "A"))


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

#does not work results_df__for_overview_all$colour_for_plot <- factor(results_df__for_overview_all$colour_for_plot, level = c("#2f3030" ,"#2f3030" ,"#2f3030" ,"#2f3030" ,"#2f3030" ,"#2f3030" , "#0072B2",   "#E69F00"))

results_df__for_overview_all_1_to_6_9_10$line <- ifelse(results_df__for_overview_all_1_to_6_9_10$run == 1, 2,ifelse(results_df__for_overview_all_1_to_6_9_10$run == 3, 2, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 5, 2,  ifelse(results_df__for_overview_all_1_to_6_9_10$run == 2, 2, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 4, 2, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 6, 2, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 7, 1, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 8, 1, 1))))))))

#does not work results_df__for_overview_all$line <- factor(results_df__for_overview_all$line, level = c(1, 5, 2,  6, 3,   4))

results_df__for_overview_all_1_to_6_9_10$linewidth <- ifelse(results_df__for_overview_all_1_to_6_9_10$run == 1, .8,ifelse(results_df__for_overview_all_1_to_6_9_10$run == 3, .8, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 5, .8, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 2, .8, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 4, .8, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 6, .8, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 7, 1, ifelse(results_df__for_overview_all_1_to_6_9_10$run == 8, 1.1, 1))))))))

#TODO edelete reudnantn line and inewidth
#does not workresults_df__for_overview_all$linewidth <- factor(results_df__for_overview_all$linewidth, level = c(1.2, 1.1, .82, .81, .31, .3))

#the following block not used in the end
#override.col <- c( "#1a1b1a", "#2c2c2c", "#353636", "#373838", "#009E73", "#2d2e2e",  "#0072B2", "#D55E00")
##override.line <- c(2,2,2,2,2,2, 1, 1)
#override.linewidth <- c(.8, .81, .82, .83, .84, .85, 1, 1)

##the following leads to not all line sbeing shown.
##how cna i make a legend that contains less elements than groups?
#override.col <- c( "#009E73", "#0072B2", "#D55E00")
#override.line <- c(1, 2, 3)
#override.linewidth <- c( .8, 1, 1.1)
##and this is wrong sequence

#the two extreme microbial yield scenarios are plotted on top of the eight scenarios in order to get only their smoothing parameters

df2 = results_df__for_overview_all_1_to_6_9_10 %>%
  dplyr::filter(run == 9)%>%
  data.frame()

df3 = results_df__for_overview_all_1_to_6_9_10 %>%
  dplyr::filter(run == 10)%>%
  data.frame()

#TODO replace second df2
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
#was #colour = as.factor(colour_for_plot), 
#lty =  as.factor(line), lwd = as.factor(linewidth)  ), 
 #               se=FALSE,  formula = my.formula )+

                group = as.factor(colour_for_plot)#, #lty =  as.factor(line), lwd = as.factor(linewidth) 
                ), 
                se=FALSE,  formula = my.formula, show.legend = FALSE, colour = "grey", lwd = .5
                 )+ 
   
  new_scale_colour() + ## requires library(ggnewscale)
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

#not needed anymore here ?!
#    scale_colour_manual("Run",values = c("#009E73", "#F0E442", "#0072B2", "#E69F00", "#56B4E9", "#D55E00") ,
#                        labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
#    scale_linetype_manual("Run", values = c(1, 5, 2, 6, 3, 4),
#                          labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
#    scale_linewidth_manual("Run", values = c(1.2, 1.1, .82, .81,  .41,  .4),
#                           labels = c("Reference", "No fauna", "+1.5°C", "No fauna  +1.5°C", "+3°C", "No fauna +3°C"))+
#    
#    guides(colour = guide_legend(override.aes = list(line = override.line, colour = override.col, linewidth = override.linewidth))) +
#    
#    #labs(x = "Date", y = "Fauna [mol COD / L]") +
#    labs(x = "Date", y = "Fauna dry mass [mol COD L<sup>-1</sup>])+ 


    theme(panel.background = element_rect(fill = "white",  colour = "black", 
                                          linetype = "solid" ),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "black"), 
          axis.text.x = element_text(angle = 45, vjust = 0.4),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          #install packages
axis.title.y = element_textbox_simple(width = NULL,
                                              orientation = 'left-rotated')# necessary for the ylab with library ggtext
    )+
    #labs(x = "Date", y = "Fauna [mol COD / L]") +
    labs(x = "Date", y = "Fauna dry mass [mol COD L<sup>-1</sup>]")
 )

setwd(gw_FuldaEcosystemServices_plots_path)
if (unified_axes == 1) {
  
  (plot_trends_fauna <- plot_trends_fauna +
     facet_wrap(.~group_letter, ncol = 4))
  
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_9_10_one_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_9_10_one_y_scale.pdf"), width = 8, height = 3)
  
}else{
  (plot_trends_fauna <- plot_trends_fauna+
     facet_wrap(.~group_letter, scales="free_y", ncol = 4))
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_9_10_free_y_scale.png"), width = 8, height = 3)
  ggsave(paste0("plot_trends_fauna_1_2_3_4_5_6_9_10_free_y_scale.pdf"), width = 8, height = 3)
}
