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
  # define and load results of composition of ptey samples
  # results from Kerguelen + Northeast Atlantic for a few taxa
  tar_target(res_compo_prey_file,
             "data/res_compo_prey.xlsx", 
             format = "file"),
  tar_target(res_compo_prey_raw, 
             load_xl(res_compo_prey_file)),
  # clean the file with samples and nutrients 
  tar_target(res_compo_prey, 
             set_up_prey_compo(res_compo_prey_raw)),
  
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
  tar_target(res_compo_scats_raw, load_xl(res_compo_scats_file)),
  # clean the file with samples and nutrients 
  tar_target(res_compo_scats, 
             set_up_scats_compo(res_compo_scats_raw)),
  
  # summarize data on fish samples 
  # tar_target(data_fish_summary, summary_fish_samples(data_fish_samples, 
  #                                                    res_compo_fish_Ker)),
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
                                             res_compo_prey)),
  # with only Kerguelen studies with quantitative description of diets 
  tar_target(data_diet_sp_Ker_W, diet_data_Ker_Wpercent(data_diet_sp_Ker, 
                                                        res_compo_prey)), 
  # for species not analysed, identify their relative contribution to diets
  tar_target(data_sp_notanalysed_W, 
             diet_data_Ker_sp_notanalysed(data_diet_sp_Ker_W)), 
  # for species analysed, identify their relative contribution to diets
  tar_target(data_sp_analysed_W, 
             diet_data_Ker_sp_analysed(data_diet_sp_Ker_W)),
  # identify how many species were not identified in each of the source for diet 
  tar_target(data_sp_known_sources, 
             summary_diet_data(data_diet_sp_Ker_W)), 
  
  
  
  #################### PREPARE DIET INPUT DATA #################################
  # script 03_prepare_diet_input_data.R
  
  # final data set used for calculations with only quantitative sources and analysed species 
  # with diets set to 100%
  tar_target(diet_data_input_fish_Ker, 
             diet_data_for_simulations(data_diet_sp_Ker_W, 
                                       "Kerguelen")), 
  tar_target(diet_data_input_all_prey, 
             diet_data_for_simulations(data_diet_sp_Ker_W, 
                                                 "all")), 
  
  # see the different specific composition of diets depending on sources/years 
  tar_target(fig_diet_data_sources_fish_Ker, 
             fig_quant_diet_data_prey_analysed(diet_data_input_fish_Ker)), 
  tar_target(fig_diet_data_sources_all_prey, 
             fig_quant_diet_data_prey_analysed(diet_data_input_all_prey)), 
  tar_target(fig_diet_data_sources_fam_fish_Ker, 
             fig_quant_diet_data_prey_analysed_fam(diet_data_input_fish_Ker)),
  
  # nest per diet 
  tar_target(diet_data_nested_fish_Ker, 
             format_data(diet_data_input_fish_Ker)),
  tar_target(diet_data_nested_all_prey, 
             format_data(diet_data_input_all_prey)),
  
  # add metabolism data 
  tar_target(diet_data_nested_withmeta_fish_Ker, 
             add_metabolic_data_r_unif(diet_data_nested_fish_Ker, 
                                nsim = 1000)),
  tar_target(diet_data_nested_withmeta_all_prey, 
             add_metabolic_data_r_unif(diet_data_nested_all_prey, 
                                nsim = 1000)),
  
  #################### COMPUTE NUTRIENT CONTENT OF DIETS #######################
  # script 04_compute_nut_in_diet.R
  
  # prepare dataset for bootstrap (with categories per Genus/Family/Taxa)
  tar_target(fish_Ker_compo_data_ready,
             prepare_compo_data_prey(res_compo_prey,
                                     "Kerguelen")),
  tar_target(all_prey_compo_data_ready,
             prepare_compo_data_prey(res_compo_prey,
                                          "all")),
  
  # bootstrap composition data
  # in dry weight
  tar_target(fish_Ker_compo_data_boot_dw,
             bootstrap_compo_data(fish_Ker_compo_data_ready,
                                  nsim = 1000)),
  tar_target(all_prey_compo_data_boot_dw,
             bootstrap_compo_data(all_prey_compo_data_ready,
                                  nsim = 1000)),
  # in wet weight
  tar_target(fish_Ker_compo_data_boot_ww,
             compot_data_ww(fish_Ker_compo_data_boot_dw)),
  tar_target(all_prey_compo_data_boot_ww,
             compot_data_ww(all_prey_compo_data_boot_dw)),

  # pool with diet data
  tar_target(diet_compo_data_pooled_fish_Ker,
             compute_nut_in_diet_fish_Ker(diet_data_nested_withmeta_fish_Ker,
                                           fish_Ker_compo_data_boot_ww)),
  tar_target(diet_compo_data_pooled_all_prey,
             compute_nut_in_diet_all_prey(diet_data_nested_withmeta_all_prey,
                                           all_prey_compo_data_boot_ww)),

  #################### RUN MODEL ###############################################
  # script 05_run_model.R

  tar_target(model_output_fish_Ker,
             run_model(diet_compo_data_pooled_fish_Ker,
                       nsim = 1e3)),
  tar_target(model_output_all_prey,
             run_model(diet_compo_data_pooled_all_prey,
                       nsim = 1e3)),

  #################### SHOW OUTPUT #############################################
  # script 06_see_outputs.R

  tar_target(fig_ind_nut_release_per_diet_fish_Ker,
             fig_nut_release_ind_diets(model_output_fish_Ker)),
  tar_target(fig_pop_nut_release_per_diet_fish_Ker,
             fig_nut_release_pop_diets(model_output_fish_Ker,
                                       "fish_Ker_n1000")),
  tar_target(fig_pop_nut_release_per_diet_all_prey,
             fig_nut_release_pop_diets(model_output_all_prey,
                                       "all_prey_n1000")),
  tar_target(table_pop_nut_release_per_diet_fish_Ker,
             tab_nut_release_pop_diets(model_output_fish_Ker)),


  tar_target(fig_ind_nut_release_rel_norm_fish_Ker,
             fig_nut_release_ind_relative_all_diets(model_output_fish_Ker,
                                          "fish_Ker")),
  tar_target(fig_ind_nut_release_rel_norm_all_prey,
             fig_nut_release_ind_relative_all_diets(model_output_all_prey,
                                       "all_prey")),
  tar_target(fig_scat_compo_rel_norm,
             fig_nut_scat_compo_relative_norm(res_compo_scats)), 
  
  
  ############### TRY FACTICE FIETS ############################################
  tar_target(diet_data_with_factice, 
             add_factice_diets(data_diet_sp_Ker_W)),
  
  # final data set used for calculations with only quantitative sources and analysed species
  # with diets set to 100%
  tar_target(diet_data_input_all_prey_with_factice,
             diet_data_for_simulations(diet_data_with_factice,
                                       "all")),

  # see the different specific composition of diets depending on sources/years
  tar_target(fig_diet_data_sources_all_prey_with_factice,
             fig_quant_diet_data_prey_analysed(diet_data_input_all_prey_with_factice)),

  # nest per diet
  tar_target(diet_data_nested_all_prey_with_factice,
             format_data(diet_data_input_all_prey_with_factice)),

  # add metabolism data
  tar_target(diet_data_nested_withmeta_all_prey_with_factice_runif,
             add_metabolic_data_r_unif(diet_data_nested_all_prey_with_factice,
                                nsim = 1000)),
  tar_target(diet_data_nested_withmeta_all_prey_with_factice_runif_Fe,
             add_metabolic_data_r_unif(diet_data_nested_all_prey_with_factice,
                                nsim = 1000, 
                                # set maximum and minimum release rate for each 
                                # trace nutrient
                                minFe = 0.99, 
                                maxFe = 1, 
                                minZn = 0.6, 
                                maxZn = 0.7, 
                                minSe = 0.6, 
                                maxSe = 0.7, 
                                minCo = 0.8, 
                                maxCo = 0.95)),
  tar_target(diet_data_nested_withmeta_all_prey_with_factice_rnorm,
             add_metabolic_data_r_truncnorm(diet_data_nested_all_prey_with_factice,
                                            nsim = 1000)),
  tar_target(diet_data_nested_withmeta_all_prey_with_factice_rnorm_Fe,
             add_metabolic_data_r_truncnorm(diet_data_nested_all_prey_with_factice,
                                       nsim = 1000, 
                                       # set mean release rate for each 
                                       # trace nutrient
                                       meanFe = 0.98,
                                       meanZn = 0.35,
                                       meanCu = 0.55,
                                       meanSe = 0.35,
                                       meanMn = 0.99,
                                       meanCo = 0.98)),

  # pool with diet data
  tar_target(diet_compo_data_pooled_all_prey_with_factice_runif,
             compute_nut_in_diet_all_prey(diet_data_nested_withmeta_all_prey_with_factice_runif,
                                          all_prey_compo_data_boot_ww)),
  tar_target(diet_compo_data_pooled_all_prey_with_factice_runif_Fe,
             compute_nut_in_diet_all_prey(diet_data_nested_withmeta_all_prey_with_factice_runif_Fe,
                                          all_prey_compo_data_boot_ww)),
  tar_target(diet_compo_data_pooled_all_prey_with_factice_rnorm,
             compute_nut_in_diet_all_prey(diet_data_nested_withmeta_all_prey_with_factice_rnorm,
                                          all_prey_compo_data_boot_ww)),
  tar_target(diet_compo_data_pooled_all_prey_with_factice_rnorm_Fe,
             compute_nut_in_diet_all_prey(diet_data_nested_withmeta_all_prey_with_factice_rnorm_Fe,
                                          all_prey_compo_data_boot_ww)),

  # run_model.R
  tar_target(model_output_all_prey_with_factice_runif,
             run_model(diet_compo_data_pooled_all_prey_with_factice_runif,
                       nsim = 1e3)),
  tar_target(model_output_all_prey_with_factice_runif_Fe,
             run_model(diet_compo_data_pooled_all_prey_with_factice_runif_Fe,
                       nsim = 1e3)),
  tar_target(model_output_all_prey_with_factice_rnorm,
             run_model(diet_compo_data_pooled_all_prey_with_factice_rnorm,
                       nsim = 1e3)),
  tar_target(model_output_all_prey_with_factice_rnorm_Fe,
             run_model(diet_compo_data_pooled_all_prey_with_factice_rnorm_Fe,
                       nsim = 1e3)),
  
  # relative proportion of nutrient consumed
  tar_target(fig_ind_nut_conso_all_diets_all_prey_with_factice_runif,
             fig_nut_conso_ind_relative_all_diets(model_output_all_prey_with_factice_runif,
                                       "all_prey_with_factice_n1000_runif")),
  
  # relative proportion of nutrient released
  tar_target(fig_pop_nut_release_per_diet_all_prey_with_factice_runif,
             fig_nut_release_pop_diets(model_output_all_prey_with_factice_runif,
                                       "all_prey_with_factice_n1000_runif")),
  
  tar_target(fig_ind_nut_release_rel_norm_all_prey_with_factice_runif,
             fig_nut_release_ind_relative_all_diets(model_output_all_prey_with_factice_runif,
                                          "all_prey_with_factice_runif")),
  tar_target(fig_ind_nut_release_rel_norm_all_prey_with_factice_runif_Fe,
             fig_nut_release_ind_relative_all_diets(model_output_all_prey_with_factice_runif_Fe,
                                                    "all_prey_with_factice_runif_Fe")),
  tar_target(fig_ind_nut_release_rel_norm_all_prey_with_factice_rnorm,
             fig_nut_release_ind_relative_all_diets(model_output_all_prey_with_factice_rnorm,
                                                    "all_prey_with_factice_rnorm")),
  tar_target(fig_ind_nut_release_rel_norm_all_prey_with_factice_rnorm_Fe,
             fig_nut_release_ind_relative_all_diets(model_output_all_prey_with_factice_rnorm_Fe,
                                                    "all_prey_with_factice_rnorm_Fe")),
  
  tar_target(fig_ind_nut_release_rel_norm_per_diet_all_prey_with_factice_runif,
             fig_nut_release_ind_relative_per_diet(model_output_all_prey_with_factice_runif,
                                                    "all_prey_with_factice_runif"))
)

