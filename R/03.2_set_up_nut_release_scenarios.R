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
add_bootstrap_scat_data_scenarios <- function(output_nut_release_site_tib, 
                                              scat_compo_tib, 
                                              res_clust_sites, 
                                              nut # Fe, Zn, Cu...
                                              ) {
  
  clust_vec_CN <- res_clust_sites$CN$cluster
  clust_vec_PS <- res_clust_sites$PS$cluster
  
  scat_compo_tib_CN <- scat_compo_tib |> 
    dplyr::filter(stringr::str_detect(Code_sample, "CN")) |> 
    dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
    dplyr::mutate(cluster = clust_vec_CN)
  
  scat_compo_tib_PS <- scat_compo_tib |> 
    dplyr::filter(stringr::str_detect(Code_sample, "PS")) |> 
    dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
    dplyr::mutate(cluster = clust_vec_PS)
  
  if (nut == "Fe") {
    clust_test_CN <- clust_test_PS <- 2
  } else if (nut == "Cu") {
    clust_test_CN <- clust_test_PS <- 3
  } else if (nut == "Zn") {
    clust_test_CN <- clust_test_PS <- 1
  }
  
  output_nut_release_site_tib |>
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
                     release_nut_pop_tot_period_all_scats )) |>
    dplyr::mutate(
      scat_boot_scenario100 = dplyr::case_when(
        Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
          purrr::map(~  scat_compo_tib_CN |>
                       # 100 % cluster enriched in Fe
                       dplyr::filter(cluster == clust_test_CN) |>
                       dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                           replace = TRUE) |>
                       dplyr::select(-cluster)), 
        Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
          purrr::map(~ scat_compo_tib_PS |>
                       # 100 % cluster enriched in Fe
                       dplyr::filter(cluster == clust_test_PS) |>
                       dplyr::slice_sample(n = purrr::pluck(simu_count, .),
                                           replace = TRUE) |> 
                       dplyr::select(-cluster))), 
      scat_boot_scenario90 = dplyr::case_when(
        Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
          purrr::map(~  scat_compo_tib_CN |>
                       # 90 % cluster enriched in Fe
                       dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_CN ~ 0.9, 
                                                               TRUE ~ 0.1)) |>
                       dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                           replace = TRUE, 
                                           weight_by = weight) |> 
                       dplyr::select(-c(cluster, weight))), 
        Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
          purrr::map(~ scat_compo_tib_PS |>
                       # 90 % cluster enriched in Fe
                       dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_PS ~ 0.9, 
                                                               TRUE ~ 0.1)) |>
                       dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                           replace = TRUE, 
                                           weight_by = weight) |> 
                       dplyr::select(-c(cluster, weight)))), 
      scat_boot_scenario80 = dplyr::case_when(
        Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
          purrr::map(~  scat_compo_tib_CN |>
                       # 80 % cluster enriched in Fe
                       dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_CN ~ 0.8, 
                                                               TRUE ~ 0.2)) |>
                       dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                           replace = TRUE, 
                                           weight_by = weight) |> 
                       dplyr::select(-c(cluster, weight))), 
        Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
          purrr::map(~ scat_compo_tib_PS |>
                       # 80 % cluster enriched in Fe
                       dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_PS ~ 0.8, 
                                                               TRUE ~ 0.2)) |>
                       dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                           replace = TRUE, 
                                           weight_by = weight) |> 
                       dplyr::select(-c(cluster, weight)))), 
        scat_boot_scenario70 = dplyr::case_when(
          Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
            purrr::map(~  scat_compo_tib_CN |>
                         # 70 % cluster enriched in Fe
                         dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_CN ~ 0.7, 
                                                                 TRUE ~ 0.3)) |>
                         dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                             replace = TRUE, 
                                             weight_by = weight) |> 
                         dplyr::select(-c(cluster, weight))), 
          Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
            purrr::map(~ scat_compo_tib_PS |>
                         # 70 % cluster enriched in Fe
                         dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_PS ~ 0.7, 
                                                                 TRUE ~ 0.3)) |>
                         dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                             replace = TRUE, 
                                             weight_by = weight) |> 
                         dplyr::select(-c(cluster, weight)))), 
          scat_boot_scenario60 = dplyr::case_when(
            Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
              purrr::map(~  scat_compo_tib_CN |>
                           # 60 % cluster enriched in Fe
                           dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_CN ~ 0.6, 
                                                                   TRUE ~ 0.4)) |>
                           dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                               replace = TRUE, 
                                               weight_by = weight) |> 
                           dplyr::select(-c(cluster, weight))), 
            Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
              purrr::map(~ scat_compo_tib_PS |>
                           # 60 % cluster enriched in Fe
                           dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_PS ~ 0.6, 
                                                                   TRUE ~ 0.4)) |>
                           dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                               replace = TRUE, 
                                               weight_by = weight) |> 
                           dplyr::select(-c(cluster, weight)))), 
            scat_boot_scenario50 = dplyr::case_when(
              Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
                purrr::map(~  scat_compo_tib_CN |>
                             # 50 % cluster enriched in Fe
                             dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_CN ~ 0.5, 
                                                                     TRUE ~ 0.5)) |>
                             dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                 replace = TRUE, 
                                                 weight_by = weight) |> 
                             dplyr::select(-c(cluster, weight))), 
              Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
                purrr::map(~ scat_compo_tib_PS |>
                             # 50 % cluster enriched in Fe
                             dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_PS ~ 0.5, 
                                                                     TRUE ~ 0.5)) |>
                             dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                 replace = TRUE, 
                                                 weight_by = weight) |> 
                             dplyr::select(-c(cluster, weight)))), 
              scat_boot_scenario40 = dplyr::case_when(
                Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
                  purrr::map(~  scat_compo_tib_CN |>
                               # 40 % cluster enriched in Fe
                               dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_CN ~ 0.4, 
                                                                       TRUE ~ 0.6)) |>
                               dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                   replace = TRUE, 
                                                   weight_by = weight) |> 
                               dplyr::select(-c(cluster, weight))), 
                Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
                  purrr::map(~ scat_compo_tib_PS |>
                               # 40 % cluster enriched in Fe
                               dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_PS ~ 0.4, 
                                                                       TRUE ~ 0.6)) |>
                               dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                   replace = TRUE, 
                                                   weight_by = weight) |> 
                               dplyr::select(-c(cluster, weight)))), 
                scat_boot_scenario30 = dplyr::case_when(
                  Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
                    purrr::map(~  scat_compo_tib_CN |>
                                 # 30 % cluster enriched in Fe
                                 dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_CN ~ 0.3, 
                                                                         TRUE ~ 0.7)) |>
                                 dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                     replace = TRUE, 
                                                     weight_by = weight) |> 
                                 dplyr::select(-c(cluster, weight))), 
                  Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
                    purrr::map(~ scat_compo_tib_PS |>
                                 # 30 % cluster enriched in Fe
                                 dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_PS ~ 0.3, 
                                                                         TRUE ~ 0.7)) |>
                                 dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                     replace = TRUE, 
                                                     weight_by = weight) |> 
                                 dplyr::select(-c(cluster, weight)))), 
                  scat_boot_scenario20 = dplyr::case_when(
                    Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
                      purrr::map(~  scat_compo_tib_CN |>
                                   # 20 % cluster enriched in Fe
                                   dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_CN ~ 0.2, 
                                                                           TRUE ~ 0.8)) |>
                                   dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                       replace = TRUE, 
                                                       weight_by = weight) |> 
                                   dplyr::select(-c(cluster, weight))), 
                    Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
                      purrr::map(~ scat_compo_tib_PS |>
                                   # 20 % cluster enriched in Fe
                                   dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_PS ~ 0.2, 
                                                                           TRUE ~ 0.8)) |>
                                   dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                       replace = TRUE, 
                                                       weight_by = weight) |> 
                                   dplyr::select(-c(cluster, weight)))), 
                    scat_boot_scenario10 = dplyr::case_when(
                      Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
                        purrr::map(~  scat_compo_tib_CN |>
                                     # 10 % cluster enriched in Fe
                                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_CN ~ 0.1, 
                                                                             TRUE ~ 0.9)) |>
                                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                         replace = TRUE, 
                                                         weight_by = weight) |> 
                                     dplyr::select(-c(cluster, weight))), 
                      Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
                        purrr::map(~ scat_compo_tib_PS |>
                                     # 10 % cluster enriched in Fe
                                     dplyr::mutate(weight = dplyr::case_when(cluster == clust_test_PS ~ 0.1, 
                                                                             TRUE ~ 0.9)) |>
                                     dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                         replace = TRUE, 
                                                         weight_by = weight) |> 
                                     dplyr::select(-c(cluster, weight)))), 
                      scat_boot_scenario00 = dplyr::case_when(
                        Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
                          purrr::map(~  scat_compo_tib_CN |>
                                       # 0 % cluster enriched in Fe
                                       dplyr::filter(cluster != clust_test_CN) |>
                                       dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                           replace = TRUE) |> 
                                       dplyr::select(-cluster)), 
                        Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
                          purrr::map(~ scat_compo_tib_PS |>
                                       # 00 % cluster enriched in Fe
                                       dplyr::filter(cluster != clust_test_PS) |>
                                       dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                           replace = TRUE) |> 
                                       dplyr::select(-cluster)))
    )
  
}
