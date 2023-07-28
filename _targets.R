################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# _targets.R
#
# Script decomposing with all steps of the analysis with target
################################################################################

library("targets")


# Source all functions contained in all files in the R directory
lapply(list.files(here::here("R"),
                  recursive = TRUE, full.names = T),
       source)


list(
  #################### DATA : SAMPLES ##########################################
  # script 01_data_samples.R
  
  ##### 1 - on fish species analysed
  # define and load data on samples of fish
  tar_target(data_fish_file,
             "data/data_fish_YC.xlsx", 
             format = "file"),
  tar_target(data_fish_samples, load_xl(data_fish_file)),
  # define and load results of composition of fish samples
  tar_target(res_compo_fish_file,
             "data/res_compo_fish.xlsx", 
             format = "file"),
  tar_target(res_compo_fish_raw, 
             load_xl(res_compo_fish_file)),
  # clean the file with samples and nutrients 
  tar_target(res_compo_fish, 
             set_up_fish_compo(res_compo_fish_raw)),
  
  ##### 2 - on scats
  # define and load data on samples of scats
  tar_target(data_scat_file,
             "data/data_scats.xlsx", 
             format = "file"),
  tar_target(data_scat_samples, load_xl(data_scat_file)),
  # define and load results of composition of scats 
  tar_target(res_compo_scats_file,
             "data/res_compo_scats.xlsx", 
             format = "file"),
  tar_target(res_compo_scats, load_xl(res_compo_scats_file)),
  
  # summarize data on fish samples 
  tar_target(data_fish_summary, summary_fish_samples(data_fish_samples, 
                                                     res_compo_fish)),
  # summarize data on scat samples 
  tar_target(data_scat_summary, summary_scat_samples(data_scat_samples)),
  
  #################### DATA : DIETS ############################################
  # script 02_diet.R
  
  #####  on diet of Arctocephalus gazella, everywhere
  tar_target(data_diet_file, 
             "data/data_diet_Agazella.xlsx", 
             format = "file"), 
  tar_target(data_diet, load_xl(data_diet_file)), 
  # identify prey species analysed and not analysed  
  # with all studies on Kerguelen
  tar_target(data_diet_sp_Ker, diet_data_Ker(data_diet, 
                                             res_compo_fish)),
  # with only Kerguelen studies with quantitative description of diets 
  tar_target(data_diet_sp_Ker_W, diet_data_Ker_Wpercent(data_diet, 
                                                        res_compo_fish)), 
  # for species not analysed, identify their relative contribution to diets
  tar_target(data_sp_notanalysed_W, 
             diet_data_Ker_sp_notanalysed(data_diet_sp_Ker_W)), 
  # for species analysed, identify their relative contribution to diets
  tar_target(data_sp_analysed_W, 
             diet_data_Ker_sp_analysed(data_diet_sp_Ker_W)),
  # identify how many species were not identified in each of the source for diet 
  tar_target(data_sp_known_sources, 
             summary_diet_data(data_diet_sp_Ker_W)), 
  
  
  # final data set with only quantitative sources and analysed species 
  # with diets set to 100%
  tar_target(diet_data_input, 
             diet_data_for_simulations(data_diet_sp_Ker_W)), 
  
  # see the different specific composition of diets depending on sources/years 
  tar_target(fig_diet_data_sources, 
             fig_quant_diet_data_prey_analysed(diet_data_input)), 
  tar_target(fig_diet_data_sources_fam, 
             fig_quant_diet_data_prey_analysed_fam(diet_data_input)), 
  
  #################### PREPARE DIET INPUT DATA #################################
  # script 03_prepare_diet_input_data.R
  
  # nest per diet 
  tar_target(diet_data_nested, 
             format_data(diet_data_input)),
  
  # add metabolism data 
  tar_target(diet_data_nested_withmeta, 
             add_metabolic_data(diet_data_nested, 
                                nsim = 1000)),
  
  #################### COMPUTE NUTRIENT CONTENT OF DIETS #######################
  # script 04_compute_nut_in_diet.R
  
  # prepare dataset for bootstrap (with categories per Genus/Family/Taxa)
  tar_target(fish_compo_data_ready, 
             prepare_compo_data(res_compo_fish)), 
  
  # bootstrap composition data
  # in dry weight
  tar_target(fish_compo_data_boot_dw, 
             bootstrap_compo_data(fish_compo_data_ready, 
                                  nsim = 1000)),
  # in wet weight
  tar_target(fish_compo_data_boot_ww, 
             compot_data_ww(fish_compo_data_boot_dw)), 
  
  # pool with diet data
  tar_target(diet_compo_data_pooled, 
             compute_nut_in_diet(diet_data_nested_withmeta, 
                                 fish_compo_data_boot_dw)), 
  
  #################### RUN MODEL ###############################################
  # script 05_run_model.R
  
  tar_target(model_output, 
             run_model(diet_compo_data_pooled, 
                       nsim = 1e3)),
  
  #################### SHOW OUTPUT #############################################
  # script 06_see_outputs.R
  
  tar_target(fig_ind_nut_release_per_diet, 
             fig_nut_release_ind_diets(model_output)),
  tar_target(fig_pop_nut_release_per_diet, 
             fig_nut_release_pop_diets(model_output)),
  tar_target(table_pop_nut_release_per_diet, 
             tab_nut_release_pop_diets(model_output))
  
)

