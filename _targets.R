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
  
  # ################ FIRST ANALYSIS : can known diet be used to ##################
  # ##############estimate relative composition of scats observed ? ##############
  # #################### DATA : SAMPLES ##########################################
  # # script 01.1_data_samples.R
  # 
  # ##### 1 - on fish species analysed
  # # define and load data on samples of fish
  # tar_target(data_fish_file,
  #            "data/data_fish_YC.xlsx", 
  #            format = "file"),
  # tar_target(data_fish_samples, load_xl(data_fish_file)),
  # # define and load results of composition of ptey samples
  # # results from Kerguelen + Northeast Atlantic for a few taxa
  # tar_target(res_compo_prey_file,
  #            "data/res_compo_prey.xlsx", 
  #            format = "file"),
  # tar_target(res_compo_prey_raw, 
  #            load_xl(res_compo_prey_file)),
  # # clean the file with samples and nutrients 
  # tar_target(res_compo_prey, 
  #            set_up_prey_compo(res_compo_prey_raw)),
   
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
  tar_target(res_compo_scats_part1,
             set_up_scats_compo_part1(res_compo_scats_raw)),

  # summarize data on fish samples
  # tar_target(data_fish_summary, summary_fish_samples(data_fish_samples,
  #                                                    res_compo_fish_Ker)),
  # summarize data on scat samples
  tar_target(data_scat_summary, summary_scat_samples(data_scat_samples)),

  # #################### DATA : DIETS ############################################
  # # script 01.2_diet.R
  # 
  # #####  on diet of Arctocephalus gazella, everywhere
  # tar_target(data_diet_file, 
  #            "data/data_diet_Agazella.xlsx", 
  #            format = "file"), 
  # tar_target(data_diet, load_xl(data_diet_file)), 
  # # identify prey species analysed and not analysed  
  # # with all studies on Kerguelen
  # tar_target(data_diet_sp_Ker, diet_data_Ker(data_diet, 
  #                                            res_compo_prey)),
  # # with only Kerguelen studies with quantitative description of diets 
  # tar_target(data_diet_sp_Ker_W, diet_data_Ker_Wpercent(data_diet_sp_Ker, 
  #                                                       res_compo_prey)), 
  # # for species not analysed, identify their relative contribution to diets
  # tar_target(data_sp_notanalysed_W, 
  #            diet_data_Ker_sp_notanalysed(data_diet_sp_Ker_W)), 
  # # for species analysed, identify their relative contribution to diets
  # tar_target(data_sp_analysed_W, 
  #            diet_data_Ker_sp_analysed(data_diet_sp_Ker_W)),
  # # identify how many species were not identified in each of the source for diet 
  # tar_target(data_sp_known_sources, 
  #            summary_diet_data(data_diet_sp_Ker_W)), 
  # 
  # 
  # 
  # #################### PREPARE DIET INPUT DATA #################################
  # # script 01.3_prepare_diet_input_data.R
  # 
  # # final data set used for calculations with only quantitative sources and analysed species 
  # # with diets set to 100%
  # tar_target(diet_data_input_fish_Ker, 
  #            diet_data_for_simulations(data_diet_sp_Ker_W, 
  #                                      "Kerguelen")), 
  # tar_target(diet_data_input_all_prey, 
  #            diet_data_for_simulations(data_diet_sp_Ker_W, 
  #                                      "all")), 
  # 
  # # see the different specific composition of diets depending on sources/years 
  # tar_target(fig_diet_data_sources_fish_Ker, 
  #            fig_quant_diet_data_prey_analysed(diet_data_input_fish_Ker)), 
  # tar_target(fig_diet_data_sources_all_prey, 
  #            fig_quant_diet_data_prey_analysed(diet_data_input_all_prey)), 
  # tar_target(fig_diet_data_sources_fam_fish_Ker, 
  #            fig_quant_diet_data_prey_analysed_fam(diet_data_input_fish_Ker)),
  # 
  # # nest per diet 
  # tar_target(diet_data_nested_fish_Ker, 
  #            format_data(diet_data_input_fish_Ker)),
  # tar_target(diet_data_nested_all_prey, 
  #            format_data(diet_data_input_all_prey)),
  # 
  # # add metabolism data 
  # tar_target(diet_data_nested_withmeta_fish_Ker, 
  #            add_metabolic_data_r_unif(diet_data_nested_fish_Ker, 
  #                                      nsim = 1000)),
  # tar_target(diet_data_nested_withmeta_all_prey, 
  #            add_metabolic_data_r_unif(diet_data_nested_all_prey, 
  #                                      nsim = 1000)),
  # 
  # #################### COMPUTE NUTRIENT CONTENT OF DIETS #######################
  # # script 01.4_compute_nut_in_diet.R
  # 
  # # prepare dataset for bootstrap (with categories per Genus/Family/Taxa)
  # tar_target(fish_Ker_compo_data_ready,
  #            prepare_compo_data_prey(res_compo_prey,
  #                                    "Kerguelen")),
  # tar_target(all_prey_compo_data_ready,
  #            prepare_compo_data_prey(res_compo_prey,
  #                                    "all")),
  # 
  # # bootstrap composition data
  # # in dry weight
  # tar_target(fish_Ker_compo_data_boot_dw,
  #            bootstrap_compo_data(fish_Ker_compo_data_ready,
  #                                 nsim = 1000)),
  # tar_target(all_prey_compo_data_boot_dw,
  #            bootstrap_compo_data(all_prey_compo_data_ready,
  #                                 nsim = 1000)),
  # # in wet weight
  # tar_target(fish_Ker_compo_data_boot_ww,
  #            compot_data_ww(fish_Ker_compo_data_boot_dw)),
  # tar_target(all_prey_compo_data_boot_ww,
  #            compot_data_ww(all_prey_compo_data_boot_dw)),
  # 
  # # pool with diet data
  # tar_target(diet_compo_data_pooled_fish_Ker,
  #            compute_nut_in_diet_fish_Ker(diet_data_nested_withmeta_fish_Ker,
  #                                         fish_Ker_compo_data_boot_ww)),
  # tar_target(diet_compo_data_pooled_all_prey,
  #            compute_nut_in_diet_all_prey(diet_data_nested_withmeta_all_prey,
  #                                         all_prey_compo_data_boot_ww)),
  # 
  # #################### RUN MODEL ###############################################
  # # script 01.5_run_model.R
  # 
  # tar_target(model_output_fish_Ker,
  #            run_model(diet_compo_data_pooled_fish_Ker,
  #                      nsim = 1e3)),
  # tar_target(model_output_all_prey,
  #            run_model(diet_compo_data_pooled_all_prey,
  #                      nsim = 1e3)),
  # 
  # #################### SHOW OUTPUT #############################################
  # # script 06_see_outputs.R
  # 
  # # individual relase
  # # absolute release per daily ration, per diet 
  # tar_target(fig_ind_nut_release_abs_per_diet_fish_Ker,
  #            fig_nut_release_ind_abs_per_diet(model_output_fish_Ker, 
  #                                             "fish_Ker")),
  # tar_target(fig_ind_nut_release_abs_per_diet_all_prey,
  #            fig_nut_release_ind_abs_per_diet(model_output_all_prey, 
  #                                             "all_prey")),
  # # absolute release per daily ration, across all diets
  # tar_target(fig_ind_nut_release_abs_all_diets_fish_Ker,
  #            fig_nut_release_ind_abs_all_diets(model_output_fish_Ker, 
  #                                              "fish_Ker")),
  # tar_target(fig_ind_nut_release_abs_all_diets_all_prey,
  #            fig_nut_release_ind_abs_all_diets(model_output_all_prey, 
  #                                              "all_prey")),
  # # relative release per daily ration, per diet
  # tar_target(fig_ind_nut_release_rel_per_diet_fish_Ker,
  #            fig_nut_release_ind_relative_per_diet(model_output_fish_Ker,
  #                                                  "fish_Ker")),
  # tar_target(fig_ind_nut_release_rel_per_diet_all_prey,
  #            fig_nut_release_ind_relative_per_diet(model_output_all_prey,
  #                                                  "all_prey")),
  # # relative release per daily ration, across all diets
  # tar_target(fig_ind_nut_release_rel_all_diets_fish_Ker,
  #            fig_nut_release_ind_relative_all_diets(model_output_fish_Ker,
  #                                                   "fish_Ker")),
  # tar_target(fig_ind_nut_release_rel_all_diets_all_prey,
  #            fig_nut_release_ind_relative_all_diets(model_output_all_prey,
  #                                                   "all_prey")),
  # 
  # # relative proportion of nutrient consumed
  # tar_target(fig_ind_nut_conso_all_diets_all_prey_runif,
  #            fig_nut_conso_ind_relative_all_diets(model_output_all_prey,
  #                                                 "all_prey_n1000_runif")),
  # tar_target(fig_ind_nut_conso_per_diet_all_prey_runif,
  #            fig_nut_conso_ind_relative_per_diet(model_output_all_prey,
  #                                                "all_prey_n1000_runif")),
  # 
  # # release for 1000 individuals
  # tar_target(fig_pop_nut_release_per_diet_fish_Ker,
  #            fig_nut_release_pop_per_diet(model_output_fish_Ker,
  #                                         "fish_Ker_n1000")),
  # tar_target(table_pop_nut_release_per_diet_fish_Ker,
  #            tab_nut_release_pop_per_diet(model_output_fish_Ker)),
  # tar_target(fig_pop_nut_release_per_diet_all_prey,
  #            fig_nut_release_pop_per_diet(model_output_all_prey,
  #                                         "all_prey_n1000")),
  # 
  # 
  # ############### TRY FACTICE FIETS ############################################
  # ############### And different nutrient release rates #########################
  # 
  # ############ 1 - factice diets
  # tar_target(diet_data_with_factice, 
  #            add_factice_diets(data_diet_sp_Ker_W)),
  # 
  # # final data set used for calculations with only quantitative sources and analysed species
  # # with diets set to 100%
  # tar_target(diet_data_input_all_prey_with_factice,
  #            diet_data_for_simulations(diet_data_with_factice,
  #                                      "all")),
  # 
  # # see the different specific composition of diets depending on sources/years
  # tar_target(fig_diet_data_sources_all_prey_with_factice,
  #            fig_quant_diet_data_prey_analysed(diet_data_input_all_prey_with_factice)),
  # 
  # # nest per diet
  # tar_target(diet_data_nested_all_prey_with_factice,
  #            format_data(diet_data_input_all_prey_with_factice)),
  # 
  # # add metabolism data
  # tar_target(diet_data_nested_withmeta_all_prey_with_factice_runif,
  #            add_metabolic_data_r_unif(diet_data_nested_all_prey_with_factice,
  #                                      nsim = 1000)),
  # 
  # # pool with diet data
  # tar_target(diet_compo_data_pooled_all_prey_with_factice_runif,
  #            compute_nut_in_diet_all_prey(diet_data_nested_withmeta_all_prey_with_factice_runif,
  #                                         all_prey_compo_data_boot_ww)),
  # 
  # # run_model.R
  # tar_target(model_output_all_prey_with_factice_runif,
  #            run_model(diet_compo_data_pooled_all_prey_with_factice_runif,
  #                      nsim = 1e3)),
  # 
  # # relative proportion of nutrient consumed
  # tar_target(fig_ind_nut_conso_all_diets_all_prey_with_factice_runif,
  #            fig_nut_conso_ind_relative_all_diets(model_output_all_prey_with_factice_runif,
  #                                                 "all_prey_with_factice_n1000_runif")),
  # tar_target(fig_ind_nut_conso_per_diet_all_prey_with_factice_runif,
  #            fig_nut_conso_ind_relative_per_diet(model_output_all_prey_with_factice_runif,
  #                                                "all_prey_with_factice_n1000_runif")),
  # 
  # # relative proportion of nutrient released for one individual
  # tar_target(fig_ind_nut_release_rel_per_diet_all_prey_with_factice_runif,
  #            fig_nut_release_ind_relative_per_diet(model_output_all_prey_with_factice_runif,
  #                                                  "all_prey_with_factice_runif")),
  # tar_target(fig_ind_nut_release_rel_per_diet_stack_all_prey_with_factice_runif,
  #            fig_nut_release_ind_relative_per_diet_stack(model_output_all_prey_with_factice_runif,
  #                                                        "all_prey_with_factice_runif")),
  # tar_target(fig_ind_nut_release_rel_all_diets_all_prey_with_factice_runif,
  #            fig_nut_release_ind_relative_all_diets(model_output_all_prey_with_factice_runif,
  #                                                   "all_prey_with_factice_runif")),
  # 
  # # nutrient released for a population of 1000 individuals
  # tar_target(fig_pop_nut_release_per_diet_all_prey_with_factice_runif,
  #            fig_nut_release_pop_per_diet(model_output_all_prey_with_factice_runif,
  #                                         "all_prey_with_factice_n1000_runif")),
  # 
  # 
  # 
  # ####### 2 - different nutrient release rates
  # tar_target(diet_data_nested_withmeta_all_prey_with_factice_runif_settings,
  #            add_metabolic_data_r_unif(diet_data_nested_all_prey_with_factice,
  #                                      nsim = 1000, 
  #                                      # set maximum and minimum release rate for each 
  #                                      # trace nutrient
  #                                      minFe = 0.90, 
  #                                      maxFe = 0.95, 
  #                                      minZn = 0.4, 
  #                                      maxZn = 0.6, 
  #                                      minCu = 0.7, 
  #                                      maxCu = 0.9, 
  #                                      minMn = 0.95, 
  #                                      maxMn = 1, 
  #                                      minSe = 0.6, 
  #                                      maxSe = 0.7, 
  #                                      minCo = 0.8, 
  #                                      maxCo = 0.95)),
  # tar_target(diet_data_nested_withmeta_all_prey_with_factice_rnorm,
  #            add_metabolic_data_r_truncnorm(diet_data_nested_all_prey_with_factice,
  #                                           nsim = 1000)),
  # tar_target(diet_data_nested_withmeta_all_prey_with_factice_rnorm_settings,
  #            add_metabolic_data_r_truncnorm(diet_data_nested_all_prey_with_factice,
  #                                           nsim = 1000, 
  #                                           # set mean release rate for each 
  #                                           # trace nutrient
  #                                           meanFe = 0.98, 
  #                                           a_Fe = 0.8, 
  #                                           b_Fe = 0.99,
  #                                           meanZn = 0.5, 
  #                                           a_Zn = 0.3, 
  #                                           b_Zn = 0.6,
  #                                           meanCu = 0.8, 
  #                                           a_Cu = 0.7, 
  #                                           b_Cu = 0.9,
  #                                           meanMn = 0.98, 
  #                                           a_Mn = 0.9, 
  #                                           b_Mn = 1,
  #                                           meanSe = 0.8,
  #                                           a_Se = 0.7, 
  #                                           b_Se = 0.9, 
  #                                           meanCo = 0.8,
  #                                           a_Co = 0.7, 
  #                                           b_Co = 0.9)),
  # 
  # # pool with diet data
  # tar_target(diet_compo_data_pooled_all_prey_with_factice_runif_settings,
  #            compute_nut_in_diet_all_prey(diet_data_nested_withmeta_all_prey_with_factice_runif_settings,
  #                                         all_prey_compo_data_boot_ww)),
  # tar_target(diet_compo_data_pooled_all_prey_with_factice_rnorm,
  #            compute_nut_in_diet_all_prey(diet_data_nested_withmeta_all_prey_with_factice_rnorm,
  #                                         all_prey_compo_data_boot_ww)),
  # tar_target(diet_compo_data_pooled_all_prey_with_factice_rnorm_settings,
  #            compute_nut_in_diet_all_prey(diet_data_nested_withmeta_all_prey_with_factice_rnorm_settings,
  #                                         all_prey_compo_data_boot_ww)),
  # 
  # # run_model.R
  # tar_target(model_output_all_prey_with_factice_runif_settings,
  #            run_model(diet_compo_data_pooled_all_prey_with_factice_runif_settings,
  #                      nsim = 1e3)),
  # tar_target(model_output_all_prey_with_factice_rnorm,
  #            run_model(diet_compo_data_pooled_all_prey_with_factice_rnorm,
  #                      nsim = 1e3)),
  # tar_target(model_output_all_prey_with_factice_rnorm_settings,
  #            run_model(diet_compo_data_pooled_all_prey_with_factice_rnorm_settings,
  #                      nsim = 1e3)),
  # 
  # # relative proportion of nutrient released for one individual
  # tar_target(fig_ind_nut_release_rel_all_prey_with_factice_runif_settings,
  #            fig_nut_release_ind_relative_all_diets(model_output_all_prey_with_factice_runif_settings,
  #                                                   "all_prey_with_factice_runif_settings")),
  # tar_target(fig_ind_nut_release_rel_all_prey_with_factice_rnorm,
  #            fig_nut_release_ind_relative_all_diets(model_output_all_prey_with_factice_rnorm,
  #                                                   "all_prey_with_factice_rnorm")),
  # tar_target(fig_ind_nut_release_rel_all_prey_with_factice_rnorm_settings,
  #            fig_nut_release_ind_relative_all_diets(model_output_all_prey_with_factice_rnorm_settings,
  #                                                   "all_prey_with_factice_rnorm_settings")), 
  # 
  
  
  ################ SECOND ANALYSIS : how much dry matter do  ###################
  ######################## the two colonies release ? ##########################
  #################### DATA : SAMPLES ##########################################
  # script 02.1_prepare_pop_input_data.R
  # define and load data on samples of fish
  tar_target(data_counts_file,
             "data/counts_Ker_2022-2023_compiled.xlsx", 
             format = "file"),
  tar_target(data_counts_raw, load_xl(data_counts_file)),
  tar_target(pop_counts_summary,
             Ker_summarise_count_data(data_counts_raw)),

  tar_target(pop_initial_count_data,
              simulate_count_data(pop_counts_summary)),
  tar_target(list_pop_data_with_indi_data, # now a list with data for the two sites
             pop_data_for_simulations(pop_initial_count_data,
                                      nsim = 10000)),
  # with n = 10,000 causes R to crash because of memeory limit, so we'll split 
  # everything for each site
  tar_target(pop_data_with_indi_data_CN, list_pop_data_with_indi_data$CN),
  tar_target(pop_data_with_indi_data_PS, list_pop_data_with_indi_data$PS),

  # script 02.2_compute_dm_produced.R

  tar_target(output_dm_produced_CN,
             run_dm_estimate(pop_data_with_indi_data_CN)),
  tar_target(output_dm_produced_PS,
             run_dm_estimate(pop_data_with_indi_data_PS)),

  # script 02.3_output_dm_produced.R

  tar_target(barplot_dm_produced_per_site_period,
             dm_per_site_period(output_dm_produced_CN, 
                                output_dm_produced_PS)),
  tar_target(table_test_dm_release_per_site_tot_period,
             test_diff_dm_sites_tot_period(output_dm_produced_CN, 
                                           output_dm_produced_PS)),
  tar_target(table_summary_model_param,
             table_model_param(output_dm_produced_CN, 
                               output_dm_produced_PS)),

  # script 02.4_compute_nut_release.R
  # prepare scat composition dataset
  tar_target(res_compo_scats,
             set_up_scats_compo_part2(res_compo_scats_raw)),
  tar_target(output_dm_produced_with_scat_compo_data_CN,
             add_bootstrap_scat_data(output_dm_produced_CN,
                                     res_compo_scats)),
  tar_target(output_dm_produced_with_scat_compo_data_PS,
             add_bootstrap_scat_data(output_dm_produced_PS,
                                     res_compo_scats)),
  tar_target(output_nut_release_CN,
             compute_nut_release(output_dm_produced_with_scat_compo_data_CN)),
  tar_target(output_nut_release_PS,
             compute_nut_release(output_dm_produced_with_scat_compo_data_PS)),

  # script 02.5_output_nut_release.R

  tar_target(barplot_nut_release_per_site_tot_period,
             nut_per_site_tot_period(output_nut_release_CN, 
                                     output_nut_release_PS)),
  tar_target(table_nut_release_per_site_period,
             table_nut_per_site_sea_land_period(output_nut_release_CN, 
                                                output_nut_release_PS)),
  tar_target(table_test_nut_release_per_site_tot_period,
             test_nut_sites_tot_period(output_nut_release_CN, 
                                       output_nut_release_PS)),

  ################ THIRD ANALYSIS : scenarios of evolution  ####################
  ################# of diets: how would it affect totals ? #####################
  # script 03.1_scats_compo_sites.R
  # nutrient composition of scats as in the dataset
  tar_target(boxplot_scat_compo_in_sites,
             boxplot_compo_scats_site(res_compo_scats)),
  tar_target(boxplot_scat_compo_in_sites_FeZnCu_MnSeCo,
             fig_nut_scat_compo_relative_sites_FeZnCu_MnSeCo(res_compo_scats)),
  tar_target(table_summary_scat_compo_in_sites,
             table_compo_scats_site(res_compo_scats)),
  tar_target(table_test_scat_compo_sites,
             MWtest_scats_compo_sites(res_compo_scats)),

  # relative composition of scats
  tar_target(fig_scat_compo_rel,
             fig_nut_scat_compo_relative(res_compo_scats)),
  tar_target(fig_scat_compo_rel_sites,
             fig_nut_scat_compo_relative_sites(res_compo_scats)),
  # relative composition of scats, comparison with other pinnipeds
  tar_target(fig_scat_compo_rel_compa_pinnipeds,
             fig_nut_scat_compo_relative_comp_pinn(res_compo_scats)),

  # ########### with cluster from chap3
  # # define and load results of composition of scats
  # tar_target(res_compo_scats_clust_file,
  #            "data/clust_attribution_scats_tot_PCs_k2.xlsx",
  #            format = "file"),
  # tar_target(res_compo_clust_scats_raw, load_xl(res_compo_scats_clust_file)),
  # # clean the file with samples and nutrients
  # tar_target(res_compo_clust_scats,
  #            set_up_scats_compo_clust(res_compo_clust_scats_raw)),
  # # see compo relative in the two clusters
  # tar_target(fig_scat_compo_rel_clust,
  #            fig_nut_scat_compo_relative_clust(res_compo_clust_scats)),

  ########################### CLUSTERING #######################################
  ######################## USING ALL NUTRIENTS #################################
  # script 03.2_clustering_scats_sites.R
  ### without using a PCA to reduce dimensions
  tar_target(clust_all_nut_findk_table_sites,
             clust_find_k_table_full_tib(res_compo_scats,
                                    method = "ward.D2",
                                    k_range = c(2:10),
                                    type = "sites")),
  tar_target(clust_all_nut_findk_means_plot_sites,
             means_clust_find_k_val_full_tib(clust_all_nut_findk_table_sites,
                                             type = "sites")),
  tar_target(clust_all_nut_findk_table_all_scats,
             clust_find_k_table_full_tib(res_compo_scats,
                                    method = "ward.D2",
                                    k_range = c(2:10),
                                    type = "all")),
  tar_target(clust_all_nut_findk_means_plot_all_scats,
             means_clust_find_k_val_full_tib(clust_all_nut_findk_table_all_scats,
                                    type = "all")),

  tar_target(clust_all_nut_sites,
             clust_compo_full_tib(res_compo_scats,
                                  k = c(3, 3, 4),
                                  method = "ward.D2",
                                  type = "sites")),
  tar_target(clust_all_nut_all_scats,
             clust_compo_full_tib(res_compo_scats,
                                  k = c(4, 4, 4),
                                  method = "ward.D2",
                                  type = "all")),

  # dendrogram ##### DID NOT FIND A WAY TO MAKE A DENDROGRAM WITH GGPLOT
  # WITH HCLUST OUTPUT OF RobComposition PACKAGE so it's not pretty
  # but still we can identify samples in clusters...
  tar_target(clust_all_nut_sites_dendro,
             clust_compo_dendro_full_tib(clust_all_nut_sites,
                                  res_compo_scats,
                                  type = "sites")),
  tar_target(clust_all_nut_all_scats_dendro,
             clust_compo_dendro_full_tib(clust_all_nut_all_scats,
                                         res_compo_scats,
                                         type = "all")),
  # boxplots
  tar_target(clust_all_nut_sites_boxplot,
             boxplot_compo_clust_full_tib(clust_all_nut_sites,
                                 res_compo_scats,
                                 "sites")),
  tar_target(clust_all_nut_all_scats_boxplot,
             boxplot_compo_clust_full_tib(clust_all_nut_all_scats,
                                 res_compo_scats,
                                 "all")),

  # barplot with relative composition per scat with cluster coloring
  tar_target(clust_all_nut_sites_barplot_rel,
             barplot_nut_scat_compo_relative_clust(clust_all_nut_sites,
                                                   res_compo_scats,
                                                   "sites")),
  tar_target(clust_all_nut_all_scats_barplot_rel,
             barplot_nut_scat_compo_relative_clust(clust_all_nut_all_scats,
                                          res_compo_scats,
                                          "all")),
  # tables
   tar_target(table_stats_clusts_all_nut_sites,
              table_stats_clust_per_site_full_tib(clust_all_nut_sites,
                                         res_compo_scats)),
   # tar_target(table_test_clust_all_nut_sites,
   #            MWtest_clust_k43_full_tib(clust_all_nut_sites,
   #                            res_compo_scats)),
  tar_target(table_test_clust_all_nut_sites,
             MWtest_clust_k33_full_tib(clust_all_nut_sites,
                                       res_compo_scats)),

  # ################ USING PCA FIRST TO REDUCE DIMENSIONS ########################
  # # PCA and clustering, script 03.2_clustering_scats_sites.R
  # 
  # tar_target(list_pca_sites,
  #            pca_coda(res_compo_scats,
  #                     "sites")),
  # tar_target(pca_all_scats,
  #            pca_coda(res_compo_scats,
  #                     "all")),
  # tar_target(biplot_pca_sites,
  #            biplot_pca_coda(list_pca_sites,
  #                            "sites",
  #                            res_compo_scats)),
  # tar_target(biplot_pca_all_scats,
  #            biplot_pca_coda(pca_all_scats,
  #                            "all",
  #                            res_compo_scats)),
  # tar_target(clust_PC_sites,
  #            clust_compo_PCs(list_pca_sites,
  #                            "sites",
  #                            pcomp = c(1, 2),
  #                            k = c(4, 4, 2),
  #                            method = "ward.D2")),
  # tar_target(clust_PC_all_scats,
  #            clust_compo_PCs(pca_all_scats,
  #                            "all",
  #                            pcomp = c(1, 2),
  #                            k = c(3, 3, 4),
  #                            method = "ward.D2")),
  # tar_target(clust_PC_dendro_sites,
  #            clust_dendro_scats(list_pca_sites,
  #                               res_compo_scats,
  #                               type = "sites",
  #                               method = "ward.D2",
  #                               pcomp = c(1, 2),
  #                               k = c(4, 4, 3))),
  # tar_target(clust_PC_dendro_all_scats,
  #            clust_dendro_scats(pca_all_scats,
  #                               res_compo_scats,
  #                               type = "all",
  #                               method = "ward.D2",
  #                               pcomp = c(1, 3),
  #                               k = c(3, 3, 8))),
  # 
  # 
  # tar_target(clust_PC_findk_table_sites,
  #            clust_find_k_table_PCs(list_pca_sites,
  #                                   method = "ward.D2",
  #                                   k_range = c(2:10),
  #                                   type = "sites")),
  # tar_target(clust_PC_findk_means_plot_sites,
  #            means_clust_find_k_val(clust_PC_findk_table_sites,
  #                                   type = "sites")),
  # tar_target(clust_PC_findk_table_all_scats,
  #            clust_find_k_table_PCs(pca_all_scats,
  #                               method = "ward.D2",
  #                               k_range = c(2:10),
  #                               type = "all")),
  # tar_target(clust_PC_findk_means_plot_all_scats,
  #            means_clust_find_k_val(clust_PC_findk_table_all_scats,
  #                                   type = "all")),
  # 
  # tar_target(clust_PC_sites_barplot_per_scat,
  #            barplot_compo_rel_clust_per_scat(clust_PC_sites,
  #                                             res_compo_scats,
  #                                             type = "sites")),
  # tar_target(clust_PC_all_scats_barplot_per_scat,
  #            barplot_compo_rel_clust_per_scat(clust_PC_all_scats,
  #                                             res_compo_scats,
  #                                             type = "all")),
  # tar_target(clust_PC_sites_boxplot,
  #            boxplot_compo_clust(clust_PC_sites,
  #                                res_compo_scats,
  #                                "sites")),
  # tar_target(clust_PC_all_scats_boxplot,
  #            boxplot_compo_clust(clust_PC_all_scats,
  #                                res_compo_scats,
  #                                "all")),
  # 
  # tar_target(table_clust_percent_sites,
  #            table_compo_clust_per_site(clust_PC_sites,
  #                                       res_compo_scats)),
  # tar_target(table_stats_percent_sites,
  #            table_stats_clust_per_site(clust_PC_sites,
  #                                       res_compo_scats)),
  # tar_target(table_test_clust_sites,
  #            MWtest_clust_k4(clust_PC_sites,
  #                            res_compo_scats)),



 ###############################################################################
 ### with scenarios of different ratios of scats of different "types"
 # script 03.4_set_up_nut_release_scenarios.R
 # first CAP NOIR
  tar_target(input_data_with_scenarios_CN_clust1,
             add_bootstrap_scat_data_scenarios(output_nut_release_CN,
                                               res_compo_scats,
                                               clust_all_nut_sites,
                                               site = "Cap Noir",
                                               clust_test = 1)),
 tar_target(input_data_with_scenarios_CN_clust2,
            add_bootstrap_scat_data_scenarios(output_nut_release_CN,
                                              res_compo_scats,
                                              clust_all_nut_sites,
                                              site = "Cap Noir",
                                              clust_test = 2)),
 tar_target(input_data_with_scenarios_CN_clust3,
            add_bootstrap_scat_data_scenarios(output_nut_release_CN,
                                              res_compo_scats,
                                              clust_all_nut_sites,
                                              site = "Cap Noir",
                                              clust_test = 3)),
 # tar_target(input_data_with_scenarios_CN_clust4,
 #            add_bootstrap_scat_data_scenarios(output_nut_release_CN,
 #                                              res_compo_scats,
 #                                              clust_all_nut_sites,
 #                                              site = "Cap Noir",
 #                                              clust_test = 4)),
 # check percentages obtained
 tar_target(percent_cluster_with_scenarios_CN_clust1,
            table_percent_cluster_k33_after_scenarios(input_data_with_scenarios_CN_clust1,
                                              res_compo_scats,
                                              clust_all_nut_sites,
                                              site = "Cap Noir",
                                              clust_test = "1")),
 tar_target(percent_cluster_with_scenarios_CN_clust2,
            table_percent_cluster_k33_after_scenarios(input_data_with_scenarios_CN_clust2,
                                                  res_compo_scats,
                                                  clust_all_nut_sites,
                                                  site = "Cap Noir",
                                                  clust_test = "2")),
 tar_target(percent_cluster_with_scenarios_CN_clust3,
            table_percent_cluster_k33_after_scenarios(input_data_with_scenarios_CN_clust3,
                                                  res_compo_scats,
                                                  clust_all_nut_sites,
                                                  site = "Cap Noir",
                                                  clust_test = "3")),
 # tar_target(percent_cluster_with_scenarios_CN_clust4,
 #            table_percent_cluster_after_scenarios(input_data_with_scenarios_CN_clust4,
 #                                                  res_compo_scats,
 #                                                  clust_all_nut_sites,
 #                                                  site = "Cap Noir",
 #                                                  clust_test = "4")),


 # and POINTE SUZANNE
 tar_target(input_data_with_scenarios_PS_clust1,
            add_bootstrap_scat_data_scenarios(output_nut_release_PS,
                                              res_compo_scats,
                                              clust_all_nut_sites,
                                              site = "Pointe Suzanne",
                                              clust_test = 1)),
 tar_target(input_data_with_scenarios_PS_clust2,
            add_bootstrap_scat_data_scenarios(output_nut_release_PS,
                                              res_compo_scats,
                                              clust_all_nut_sites,
                                              site = "Pointe Suzanne",
                                              clust_test = 2)),
 tar_target(input_data_with_scenarios_PS_clust3,
            add_bootstrap_scat_data_scenarios(output_nut_release_PS,
                                              res_compo_scats,
                                              clust_all_nut_sites,
                                              site = "Pointe Suzanne",
                                              clust_test = 3)),
 # check percentages obtained
 tar_target(percent_cluster_with_scenarios_PS_clust1,
            table_percent_cluster_k33_after_scenarios(input_data_with_scenarios_PS_clust1,
                                                  res_compo_scats,
                                                  clust_all_nut_sites,
                                                  site = "Pointe Suzanne",
                                                  clust_test = "1")),
 tar_target(percent_cluster_with_scenarios_PS_clust2,
            table_percent_cluster_k33_after_scenarios(input_data_with_scenarios_PS_clust2,
                                                  res_compo_scats,
                                                  clust_all_nut_sites,
                                                  site = "Pointe Suzanne",
                                                  clust_test = "2")),
 tar_target(percent_cluster_with_scenarios_PS_clust3,
            table_percent_cluster_k33_after_scenarios(input_data_with_scenarios_PS_clust3,
                                                  res_compo_scats,
                                                  clust_all_nut_sites,
                                                  site = "Pointe Suzanne",
                                                  clust_test = "3"))

 #  # script 03.3_compute_nut_produced_scenarios.R
 # # first CAP NOIR
 #  tar_target(output_nut_release_with_scenarios_CN_clust1,
 #             compute_nut_release_scenarios(input_data_with_scenarios_CN_clust1)),
 #  tar_target(output_nut_release_with_scenarios_CN_clust2,
 #             compute_nut_release_scenarios(input_data_with_scenarios_CN_clust2)),
 #  tar_target(output_nut_release_with_scenarios_CN_clust3,
 #             compute_nut_release_scenarios(input_data_with_scenarios_CN_clust3)),
 # #  tar_target(output_nut_release_with_scenarios_CN_clust4,
 # #             compute_nut_release_scenarios(input_data_with_scenarios_CN_clust4)),
 # 
 # # and POINTE SUZANNE
 # tar_target(output_nut_release_with_scenarios_PS_clust1,
 #            compute_nut_release_scenarios(input_data_with_scenarios_PS_clust1)),
 # tar_target(output_nut_release_with_scenarios_PS_clust2,
 #            compute_nut_release_scenarios(input_data_with_scenarios_PS_clust2)),
 # tar_target(output_nut_release_with_scenarios_PS_clust3,
 #            compute_nut_release_scenarios(input_data_with_scenarios_PS_clust3)),
 # 
 #  # script 03.4_output_nut_release_scenarios.R
 # # first CAP NOIR
 #  tar_target(plot_nut_release_with_scenarios_CN_clust1,
 #             nut_per_site_tot_period_scenarios(output_nut_release_with_scenarios_CN_clust1,
 #                                               clust_test = 1,
 #                                               site = "Cap Noir")),
 # tar_target(plot_nut_release_with_scenarios_CN_clust2,
 #            nut_per_site_tot_period_scenarios(output_nut_release_with_scenarios_CN_clust2,
 #                                              clust_test = 2,
 #                                              site = "Cap Noir")),
 # tar_target(plot_nut_release_with_scenarios_CN_clust3,
 #            nut_per_site_tot_period_scenarios(output_nut_release_with_scenarios_CN_clust3,
 #                                              clust_test = 3,
 #                                              site = "Cap Noir")),
 # # tar_target(plot_nut_release_with_scenarios_CN_clust4,
 # #            nut_per_site_tot_period_scenarios(output_nut_release_with_scenarios_CN_clust4,
 # #                                              site = "Cap Noir",
 # #                                              clust_test = 4)),
 # 
 # # and POINTE SUZANNE
 # tar_target(plot_nut_release_with_scenarios_PS_clust1,
 #            nut_per_site_tot_period_scenarios(output_nut_release_with_scenarios_PS_clust1,
 #                                              clust_test = 1,
 #                                              site = "Pointe Suzanne")),
 # tar_target(plot_nut_release_with_scenarios_PS_clust2,
 #            nut_per_site_tot_period_scenarios(output_nut_release_with_scenarios_PS_clust2,
 #                                              clust_test = 2,
 #                                              site = "Pointe Suzanne")),
 # tar_target(plot_nut_release_with_scenarios_PS_clust3,
 #            nut_per_site_tot_period_scenarios(output_nut_release_with_scenarios_PS_clust3,
 #                                              clust_test = 3,
 #                                              site = "Pointe Suzanne")),
 # 
 # # all together
 #  tar_target(plot_nut_release_with_all_scenarios_CN,
 #             nut_per_site_tot_period_all_scenarios(list(output_nut_release_with_scenarios_CN_clust1,
 #                                                        output_nut_release_with_scenarios_CN_clust2,
 #                                                        output_nut_release_with_scenarios_CN_clust3),
 #                                                   list(percent_cluster_with_scenarios_CN_clust1,
 #                                                        percent_cluster_with_scenarios_CN_clust2,
 #                                                        percent_cluster_with_scenarios_CN_clust3),
 #                                                   site = "Cap Noir")),
 # # tar_target(table_test_nut_release_with_all_scenarios_CN,
 # #            MWtest_test_nut_sites_tot_period_all_scenarios(list(output_nut_release_with_scenarios_CN_clust1,
 # #                                                       output_nut_release_with_scenarios_CN_clust2,
 # #                                                       output_nut_release_with_scenarios_CN_clust3),
 # #                                                  site = "Cap Noir")),
 # # tar_target(table_differences_mean_nut_release_with_all_scenarios_CN,
 # #            percent_diff_nut_sites_tot_period_all_scenarios(list(output_nut_release_with_scenarios_CN_clust1,
 # #                                                                output_nut_release_with_scenarios_CN_clust2,
 # #                                                                output_nut_release_with_scenarios_CN_clust3),
 # #                                                           site = "Cap Noir")),
 # 
 # 
 # # tar_target(plot_nut_release_with_all_scenarios_PS,
 # #            nut_per_site_tot_period_all_scenarios(list(output_nut_release_with_scenarios_PS_clust1,
 # #                                                       output_nut_release_with_scenarios_PS_clust2,
 # #                                                       output_nut_release_with_scenarios_PS_clust3),
 # #                                                  list(percent_cluster_with_scenarios_PS_clust1,
 # #                                                       percent_cluster_with_scenarios_PS_clust2,
 # #                                                       percent_cluster_with_scenarios_PS_clust3),
 # #                                                  site = "Pointe Suzanne")),
 # tar_target(table_differences_mean_nut_release_with_scenarios_clust1_PS,
 #            percent_diff_nut_sites_tot_period_1_scenarios(output_nut_release_with_scenarios_PS_clust1,
 #                                                          clust_test = 1,
 #                                                          site = "Pointe Suzanne")),
 # tar_target(table_differences_mean_nut_release_with_scenarios_clust2_PS,
 #            percent_diff_nut_sites_tot_period_1_scenarios(output_nut_release_with_scenarios_PS_clust2,
 #                                                          clust_test = 2,
 #                                                          site = "Pointe Suzanne")),
 # tar_target(table_differences_mean_nut_release_with_scenarios_clust3_PS,
 #            percent_diff_nut_sites_tot_period_1_scenarios(output_nut_release_with_scenarios_PS_clust3,
 #                                                          clust_test = 3,
 #                                                          site = "Pointe Suzanne")),
 # tar_target(table_differences_mean_nut_release_with_all_scenarios_PS,
 #            compile_percent_diff_nut_sites_tot_period_scenarios(
 #              list(table_differences_mean_nut_release_with_scenarios_clust1_PS,
 #                   table_differences_mean_nut_release_with_scenarios_clust2_PS,
 #                   table_differences_mean_nut_release_with_scenarios_clust3_PS),
 #              site = "Pointe Suzanne")), 
 # tar_target(table_test_nut_release_with_scenarios_clust1_PS,
 #            test_diff_test_nut_sites_tot_period_1_scenarios(output_nut_release_with_scenarios_PS_clust1,
 #                                                         clust_test = 1)), 
 # tar_target(table_test_nut_release_with_scenarios_clust2_PS,
 #            test_diff_test_nut_sites_tot_period_1_scenarios(output_nut_release_with_scenarios_PS_clust2,
 #                                                            clust_test = 2)), 
 # tar_target(table_test_nut_release_with_scenarios_clust3_PS,
 #            test_diff_test_nut_sites_tot_period_1_scenarios(output_nut_release_with_scenarios_PS_clust3,
 #                                                            clust_test = 3)),
 # tar_target(table_test_diff_mean_nut_release_with_all_scenarios_PS,
 #            compile_test_diff_test_nut_sites_tot_period_scenarios(
 #              list(table_test_nut_release_with_scenarios_clust1_PS,
 #                   table_test_nut_release_with_scenarios_clust2_PS,
 #                   table_test_nut_release_with_scenarios_clust3_PS),
 #              site = "Pointe Suzanne"))

  
  
)





