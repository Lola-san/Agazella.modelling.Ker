################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 03.3_compute_nut_produced.R
#
################################################################################



############################# COMPUTATION ######################################

compute_nut_release_scenarios <- function(input_tib_with_scenarios) {
  
  input_tib_with_scenarios |>
    dplyr::mutate(# scenario 100%
      release_nut_ind_daily_scenario100 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario100, .)), 
      release_nut_ind_tot_period_scenario100 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario100, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario100 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario100, .) |> 
                     tidyr::pivot_longer(cols = c(Fe:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
      # scenario 90%
      release_nut_ind_daily_scenario90 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario90, .)), 
      release_nut_ind_tot_period_scenario90 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario90, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario90 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario90, .) |> 
                     tidyr::pivot_longer(cols = c(Fe:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
      # scenario 80%
      release_nut_ind_daily_scenario80 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario80, .)), 
      release_nut_ind_tot_period_scenario80 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario80, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario80 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario80, .) |> 
                     tidyr::pivot_longer(cols = c(Fe:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co))
    )

}
