################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 03.4_set_up_nut_release_scenarios.R
#
################################################################################


#'
#'
#'
#'
#
add_bootstrap_scat_data_scenarios <- function(list_output_nut_release_site, 
                                              scat_compo_tib, 
                                              list_res_clust_sites, 
                                              site, # either "Cap Noir" or "Pointe Suzanne"
                                              clust_test 
) {
  
  if (site == "Cap Noir") {
    clust_vec <- list_res_clust_sites$CN$cluster
    
    nb_cluster <- length(unique(clust_vec))
    
    output_tib <- list_output_nut_release_site$CN
    
    scat_compo_tib <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "CN")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      dplyr::mutate(cluster = clust_vec)
    
    
  } else if (site == "Pointe Suzanne") {
    clust_vec <- list_res_clust_sites$PS$cluster
    
    nb_cluster <- length(unique(clust_vec))
    
    output_tib <- list_output_nut_release_site$PS
    
    scat_compo_tib <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "PS")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      dplyr::mutate(cluster = clust_vec)
  }
  
  output_tib |>
    # get rid of unwanted columns to lighten the data
    dplyr::select(-c(Indi_data, 
                     release_dm_pop_tot_daily,
                     release_dm_pop_on_land_daily,
                     release_dm_pop_at_sea_daily,
                     release_dm_pop_tot_period,
                     release_dm_pop_on_land_period,
                     release_dm_pop_at_sea_period,
                     release_nut_ind_daily_all_scats,
                     release_nut_ind_tot_period_all_scats,
                     release_nut_pop_tot_period_all_scats)) |>
    dplyr::mutate(
      scat_boot_scenario100 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 100 % of clust_test in scat dataset in scat dataset
                     dplyr::filter(cluster == clust_test) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE) |>
                     dplyr::select(-cluster)),  
      scat_boot_scenario90 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 90 % of clust_test in scat dataset
                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.9, 
                                                             TRUE ~ 0.1/nb_cluster)) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE, 
                                         weight_by = weight) |> 
                     dplyr::select(-c(cluster, weight))), 
      scat_boot_scenario80 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 80 % of clust_test in scat dataset
                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.8, 
                                                             TRUE ~ 0.2/nb_cluster)) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE, 
                                         weight_by = weight) |> 
                     dplyr::select(-c(cluster, weight))), 
      scat_boot_scenario70 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 70 % of clust_test in scat dataset
                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.7, 
                                                             TRUE ~ 0.3/nb_cluster)) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE, 
                                         weight_by = weight) |> 
                     dplyr::select(-c(cluster, weight))), 
      scat_boot_scenario60 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 60 % of clust_test in scat dataset
                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.6, 
                                                             TRUE ~ 0.4/nb_cluster)) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE, 
                                         weight_by = weight) |> 
                     dplyr::select(-c(cluster, weight))), 
      scat_boot_scenario50 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 50 % of clust_test in scat dataset
                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.5, 
                                                             TRUE ~ 0.5/nb_cluster)) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE, 
                                         weight_by = weight) |> 
                     dplyr::select(-c(cluster, weight))), 
      scat_boot_scenario40 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 40 % of clust_test in scat dataset
                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.4, 
                                                             TRUE ~ 0.6/nb_cluster)) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE, 
                                         weight_by = weight) |> 
                     dplyr::select(-c(cluster, weight))), 
      scat_boot_scenario30 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 30 % of clust_test in scat dataset
                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.3, 
                                                             TRUE ~ 0.7/nb_cluster)) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE, 
                                         weight_by = weight) |> 
                     dplyr::select(-c(cluster, weight))), 
      scat_boot_scenario20 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 20 % of clust_test in scat dataset
                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.2, 
                                                             TRUE ~ 0.8/nb_cluster)) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE, 
                                         weight_by = weight) |> 
                     dplyr::select(-c(cluster, weight))), 
      scat_boot_scenario10 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 10 % of clust_test in scat dataset
                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.1, 
                                                             TRUE ~ 0.9/nb_cluster)) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE, 
                                         weight_by = weight) |> 
                     dplyr::select(-c(cluster, weight))), 
      scat_boot_scenario00 = seq_along(release_dm_ind_daily) |>
        purrr::map(~  scat_compo_tib |>
                     # 0 % of clust_test in scat dataset
                     dplyr::filter(cluster != clust_test) |>
                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                         replace = TRUE) |> 
                     dplyr::select(-cluster))
    )
  
}



#'
#'
#'
#'
#
table_percent_cluster_after_scenarios <- function(input_nut_release_scenarios_tib,
                                                  scat_compo_tib,
                                                  list_res_clust_sites,
                                                  site, # "Cap Noir" or "Pointe Suzanne"
                                                  clust_test
) {
  
  
  if (site == "Cap Noir") {
    clust_vec <- list_res_clust_sites$CN$cluster
    
    unique_clust_vec <- unique(clust_vec)
    
    scat_compo_tib <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "CN")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      dplyr::mutate(cluster = clust_vec)
    
    ############### COMPILE OUTPUT WITH ALL SCENARIOS ##########################
    full_tib <- rbind(
      # with all scats taken on site
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_data_sampled_sites) |>
        tidyr::unnest(scat_data_sampled_sites) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = "All samples from site", 
                      subscenario = NA) |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`)), 
      # with only scat samples from one enriched cluster - 100% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario100) |>
        tidyr::unnest(scat_boot_scenario100) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        dplyr::bind_rows(data.frame(Site = rep(site, 3), 
                                    cluster = c(as.character(unique_clust_vec[unique_clust_vec != clust_test][1]), 
                                                as.character(unique_clust_vec[unique_clust_vec != clust_test][2]), 
                                                as.character(unique_clust_vec[unique_clust_vec != clust_test][3])), 
                                    percent_cluster = rep(0, 3))) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "100%") |>
        dplyr::select(c(Site, scenario, subscenario,  
                        `1`, `2`, `3`, `4`)),
      # with 90% scat samples from one enriched cluster - 90% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario90) |>
        tidyr::unnest(scat_boot_scenario90) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "90%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`)),
      # with 80% scat samples from one enriched cluster - 80% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario80) |>
        tidyr::unnest(scat_boot_scenario80) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "80%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`)),
      # with 70% scat samples from one enriched cluster - 70% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario70) |>
        tidyr::unnest(scat_boot_scenario70) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "70%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`)),
      # with 60% scat samples from one enriched cluster - 60% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario60) |>
        tidyr::unnest(scat_boot_scenario60) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "60%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`)),
      # with 50% scat samples from one enriched cluster - 50% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario50) |>
        tidyr::unnest(scat_boot_scenario50) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "50%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`)),
      # with 40% scat samples from one enriched cluster - 40% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario40) |>
        tidyr::unnest(scat_boot_scenario40) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "40%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`)),
      # with 30% scat samples from one enriched cluster - 30% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario30) |>
        tidyr::unnest(scat_boot_scenario30) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "30%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`)),
      # with 20% scat samples from one enriched cluster - 20% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario20) |>
        tidyr::unnest(scat_boot_scenario20) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "20%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`)),
      # with 10% scat samples from one enriched cluster - 10% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario10) |>
        tidyr::unnest(scat_boot_scenario10) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "10%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`)),
      # with 00% scat samples from one enriched cluster - 00% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario00) |>
        tidyr::unnest(scat_boot_scenario00) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        dplyr::bind_rows(data.frame(Site = site, 
                                    cluster = as.character(unique_clust_vec[unique_clust_vec == clust_test]), 
                                    percent_cluster = 0)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "00%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`, `4`))
    ) 
    
  } else if (site == "Pointe Suzanne") {
    clust_vec <- list_res_clust_sites$PS$cluster
    
    unique_clust_vec <- unique(clust_vec)
    
    scat_compo_tib <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "PS")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      dplyr::mutate(cluster = clust_vec)
    
    ############### COMPILE OUTPUT WITH ALL SCENARIOS ##########################
    full_tib <- rbind(
      # with all scats taken on site
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_data_sampled_sites) |>
        tidyr::unnest(scat_data_sampled_sites) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = "All samples from site", 
                      subscenario = NA) |>
        dplyr::select(c(Site, scenario, subscenario, 
                        `1`, `2`, `3`)), 
      # with only scat samples from one enriched cluster - 100% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario100) |>
        tidyr::unnest(scat_boot_scenario100) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        dplyr::bind_rows(data.frame(Site = rep(site, 2), 
                                    cluster = c(as.character(unique_clust_vec[unique_clust_vec != clust_test][1]), 
                                                as.character(unique_clust_vec[unique_clust_vec != clust_test][2])), 
                                    percent_cluster = rep(0, 2))) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "100%") |>
        dplyr::select(c(Site, scenario, subscenario,  
                        `1`, `2`, `3`)),
      # with 90% scat samples from one enriched cluster - 90% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario90) |>
        tidyr::unnest(scat_boot_scenario90) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "90%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`)),
      # with 80% scat samples from one enriched cluster - 80% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario80) |>
        tidyr::unnest(scat_boot_scenario80) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "80%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`)),
      # with 70% scat samples from one enriched cluster - 70% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario70) |>
        tidyr::unnest(scat_boot_scenario70) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "70%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`)),
      # with 60% scat samples from one enriched cluster - 60% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario60) |>
        tidyr::unnest(scat_boot_scenario60) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "60%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`)),
      # with 50% scat samples from one enriched cluster - 50% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario50) |>
        tidyr::unnest(scat_boot_scenario50) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "50%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`)),
      # with 40% scat samples from one enriched cluster - 40% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario40) |>
        tidyr::unnest(scat_boot_scenario40) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "40%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`)),
      # with 30% scat samples from one enriched cluster - 30% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario30) |>
        tidyr::unnest(scat_boot_scenario30) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "30%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`)),
      # with 20% scat samples from one enriched cluster - 20% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario20) |>
        tidyr::unnest(scat_boot_scenario20) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "20%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`)),
      # with 10% scat samples from one enriched cluster - 10% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario10) |>
        tidyr::unnest(scat_boot_scenario10) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "10%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`)),
      # with 00% scat samples from one enriched cluster - 00% scenario
      input_nut_release_scenarios_tib |>
        dplyr::select(Site,
                      scat_boot_scenario00) |>
        tidyr::unnest(scat_boot_scenario00) |>
        dplyr::left_join(scat_compo_tib, 
                         by = c("Fe", "Zn", "Cu", "Mn", "Se", "Co")) |>
        dplyr::select(c(Site, cluster)) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(nb_samples = sum(Freq)) |>
        dplyr::group_by(Site, cluster) |>
        dplyr::mutate(percent_cluster = round(100*(Freq/nb_samples), 1)) |>
        dplyr::select(c(Site, cluster, percent_cluster)) |>
        dplyr::bind_rows(data.frame(Site = site, 
                                    cluster = as.character(unique_clust_vec[unique_clust_vec == clust_test]), 
                                    percent_cluster = 0)) |>
        tidyr::pivot_wider(names_from = cluster, 
                           values_from = percent_cluster) |>
        dplyr::mutate(scenario = clust_test, 
                      subscenario = "00%") |>
        dplyr::select(c(Site, scenario, subscenario, `1`, `2`, `3`))
    ) 
    
  }
  
  
}

