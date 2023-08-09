################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 03.2_set_up_nut_release_scenarios.R
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
    
    output_tib <- list_output_nut_release_site$CN
    
    scat_compo_tib <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "CN")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      dplyr::mutate(cluster = clust_vec)
    
    
  } else if (site == "Pointe Suzanne") {
    clust_vec <- list_res_clust_sites$PS$cluster
    
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
                                                               TRUE ~ 0.1)) |>
                       dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                           replace = TRUE, 
                                           weight_by = weight) |> 
                       dplyr::select(-c(cluster, weight))), 
      scat_boot_scenario80 = seq_along(release_dm_ind_daily) |>
          purrr::map(~  scat_compo_tib |>
                       # 80 % of clust_test in scat dataset
                       dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.8, 
                                                               TRUE ~ 0.2)) |>
                       dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                           replace = TRUE, 
                                           weight_by = weight) |> 
                       dplyr::select(-c(cluster, weight))), 
        scat_boot_scenario70 = seq_along(release_dm_ind_daily) |>
            purrr::map(~  scat_compo_tib |>
                         # 70 % of clust_test in scat dataset
                         dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.7, 
                                                                 TRUE ~ 0.3)) |>
                         dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                             replace = TRUE, 
                                             weight_by = weight) |> 
                         dplyr::select(-c(cluster, weight))), 
          scat_boot_scenario60 = seq_along(release_dm_ind_daily) |>
              purrr::map(~  scat_compo_tib |>
                           # 60 % of clust_test in scat dataset
                           dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.6, 
                                                                   TRUE ~ 0.4)) |>
                           dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                               replace = TRUE, 
                                               weight_by = weight) |> 
                           dplyr::select(-c(cluster, weight))), 
            scat_boot_scenario50 = seq_along(release_dm_ind_daily) |>
                purrr::map(~  scat_compo_tib |>
                             # 50 % of clust_test in scat dataset
                             dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.5, 
                                                                     TRUE ~ 0.5)) |>
                             dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                 replace = TRUE, 
                                                 weight_by = weight) |> 
                             dplyr::select(-c(cluster, weight))), 
              scat_boot_scenario40 = seq_along(release_dm_ind_daily) |>
                  purrr::map(~  scat_compo_tib |>
                               # 40 % of clust_test in scat dataset
                               dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.4, 
                                                                       TRUE ~ 0.6)) |>
                               dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                   replace = TRUE, 
                                                   weight_by = weight) |> 
                               dplyr::select(-c(cluster, weight))), 
                scat_boot_scenario30 = seq_along(release_dm_ind_daily) |>
                    purrr::map(~  scat_compo_tib |>
                                 # 30 % of clust_test in scat dataset
                                 dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.3, 
                                                                         TRUE ~ 0.7)) |>
                                 dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                     replace = TRUE, 
                                                     weight_by = weight) |> 
                                 dplyr::select(-c(cluster, weight))), 
                  scat_boot_scenario20 = seq_along(release_dm_ind_daily) |>
                      purrr::map(~  scat_compo_tib |>
                                   # 20 % of clust_test in scat dataset
                                   dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.2, 
                                                                           TRUE ~ 0.8)) |>
                                   dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                       replace = TRUE, 
                                                       weight_by = weight) |> 
                                   dplyr::select(-c(cluster, weight))), 
                    scat_boot_scenario10 = seq_along(release_dm_ind_daily) |>
                        purrr::map(~  scat_compo_tib |>
                                     # 10 % of clust_test in scat dataset
                                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test ~ 0.1, 
                                                                             TRUE ~ 0.9)) |>
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
                                       dplyr::select(-cluster)))
  
}
