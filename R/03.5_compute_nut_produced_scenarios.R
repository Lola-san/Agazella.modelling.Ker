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
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario100, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
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
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario90, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
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
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario80, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg 
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
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
      # scenario 70%
      release_nut_ind_daily_scenario70 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario70, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg 
      release_nut_ind_tot_period_scenario70 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario70, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario70 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario70, .) |> 
                     tidyr::pivot_longer(cols = c(Fe:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
      # scenario 60%
      release_nut_ind_daily_scenario60 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario60, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
      release_nut_ind_tot_period_scenario60 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario60, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario60 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario60, .) |> 
                     tidyr::pivot_longer(cols = c(Fe:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
      # scenario 50%
      release_nut_ind_daily_scenario50 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario50, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg 
      release_nut_ind_tot_period_scenario50 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario50, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario50 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario50, .) |> 
                     tidyr::pivot_longer(cols = c(Fe:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
      # scenario 40%
      release_nut_ind_daily_scenario40 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario40, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg 
      release_nut_ind_tot_period_scenario40 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario40, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario40 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario40, .) |> 
                     tidyr::pivot_longer(cols = c(Fe:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
      # scenario 30%
      release_nut_ind_daily_scenario30 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario30, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg 
      release_nut_ind_tot_period_scenario30 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario30, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario30 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario30, .) |> 
                     tidyr::pivot_longer(cols = c(Fe:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
      # scenario 20%
      release_nut_ind_daily_scenario20 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario20, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg 
      release_nut_ind_tot_period_scenario20 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario20, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario20 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario20, .) |> 
                     tidyr::pivot_longer(cols = c(Fe:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
      # scenario 10%
      release_nut_ind_daily_scenario10 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario10, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
      release_nut_ind_tot_period_scenario10 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario10, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario10 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario10, .) |> 
                     tidyr::pivot_longer(cols = c(Fe:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(Fe, Zn, Cu, Mn, Se, Co)),
      # scenario 00%
      release_nut_ind_daily_scenario00 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_dm_ind_daily, ., 1) * purrr::pluck(scat_boot_scenario00, .)*1e-6), 
      # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
      release_nut_ind_tot_period_scenario00 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_daily_scenario00, .) * purrr::pluck(duration_of_stay, ., 1)),
      release_nut_pop_tot_period_scenario00 = seq_along(release_dm_ind_daily) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario00, .) |> 
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
