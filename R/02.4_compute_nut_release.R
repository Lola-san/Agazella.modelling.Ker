################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 02.4_compute_nut_release.R
#
################################################################################

#'
#'
#'
#'
#
add_bootstrap_scat_data <- function(output_dm_tib, 
                                 scat_compo_tib) {
  
  scat_compo_tib_CN <- scat_compo_tib |> 
    dplyr::filter(stringr::str_detect(Code_sample, "CN")) |> 
    dplyr::select(Fe, Zn, Cu, Mn, Se, Co)
  
  scat_compo_tib_PS <- scat_compo_tib |> 
    dplyr::filter(stringr::str_detect(Code_sample, "PS")) |> 
    dplyr::select(Fe, Zn, Cu, Mn, Se, Co)
  
  scat_compo_tib <- scat_compo_tib |> 
  dplyr::select(Fe, Zn, Cu, Mn, Se, Co)
  
  
  output_dm_tib |>
    dplyr::mutate(scat_data_sampled_all = seq_along(release_dm_ind_daily) |>
                    purrr::map(~ scat_compo_tib |>
                                 dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                     replace = TRUE)),
                  scat_data_sampled_sites = dplyr::case_when(Site == "Cap Noir" ~ seq_along(release_dm_ind_daily) |>
                                                             purrr::map(~  scat_compo_tib_CN |>
                                                                          dplyr::slice_sample(n = purrr::pluck(simu_count, .), 
                                                                                              replace = TRUE)), 
                                                             Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_daily) |>
                                                                          purrr::map(~ scat_compo_tib_PS |>
                                                                                       dplyr::slice_sample(n = purrr::pluck(simu_count, .),
                                                                                                           replace = TRUE))))
}


#'
#'
#'
#'
#
compute_nut_release <- function(output_dm_with_scat_compo_tib) {
  
  output_dm_with_scat_compo_tib |>
    dplyr::mutate(release_nut_ind_daily_all_scats = seq_along(release_dm_ind_daily) |>
                    purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_data_sampled_all, .)), 
                  release_nut_ind_tot_period_all_scats = seq_along(release_dm_ind_daily) |>
                    purrr::map(~ purrr::pluck(release_nut_ind_daily_all_scats, .) * purrr::pluck(duration_of_stay, ., 1)),
                  release_nut_pop_tot_period_all_scats = seq_along(release_dm_ind_daily) |>
                    purrr::map(~ purrr::pluck(release_nut_ind_tot_period_all_scats, .) |> 
                                 tidyr::pivot_longer(cols = c(Fe:Co), 
                                                     names_to = "Nutrient", 
                                                     values_to = "release_ind_tot_period") |> 
                                 dplyr::group_by(Nutrient) |> 
                                 dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                                 tidyr::pivot_wider(names_from = Nutrient, 
                                                    values_from = pop_tot_release_period) |> 
                                 dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
                  # with scats separated per site
                  release_nut_ind_daily_sites = seq_along(release_dm_ind_daily) |>
                  purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_data_sampled_sites, .)),
                  release_nut_ind_tot_period_sites = seq_along(release_dm_ind_daily) |>
                    purrr::map(~ purrr::pluck(release_nut_ind_daily_sites, .) * purrr::pluck(duration_of_stay, ., 1)),
                  release_nut_pop_tot_period_sites = seq_along(release_dm_ind_daily) |>
                    purrr::map(~ purrr::pluck(release_nut_ind_tot_period_sites, .) |> 
                                 tidyr::pivot_longer(cols = c(Fe:Co), 
                                                     names_to = "Nutrient", 
                                                     values_to = "release_ind_tot_period") |> 
                                 dplyr::group_by(Nutrient) |> 
                                 dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                                 tidyr::pivot_wider(names_from = Nutrient, 
                                                    values_from = pop_tot_release_period) |> 
                                 dplyr::select(Fe, Zn, Cu, Mn, Se, Co))
                  # release_nut_ind_on_land_period_sites = seq_along(release_dm_ind_daily) |>
                  #   purrr::map(~ purrr::pluck(release_nut_ind_tot_period_sites, .) * purrr::pluck(time_on_land, .)), 
                  # release_nut_ind_at_sea_period_sites = seq_along(release_dm_ind_daily) |>
                  #   purrr::map(~ purrr::pluck(release_nut_ind_tot_period_sites, .) * (1 - purrr::pluck(time_on_land, .)))
                  )
}
