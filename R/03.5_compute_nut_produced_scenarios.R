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
    dplyr::mutate(
      # with all scat from the colony
      release_nut_ind_tot_period_all_scats_sites = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_data_sampled_sites, .)*1e-6),
      release_nut_pop_tot_period_all_scats_sites = seq_along(release_nut_ind_tot_period_all_scats_sites) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_all_scats_sites, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_all_scats_sites,
                     scat_data_sampled_sites)) |>
    dplyr::mutate(
      # scenario 100%
      release_nut_ind_tot_period_scenario100 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario100, .)*1e-6),
      release_nut_pop_tot_period_scenario100 = seq_along(release_nut_ind_tot_period_scenario100) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario100, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario100,
                     scat_boot_scenario100)) |>
    dplyr::mutate(
      # scenario 90%
      release_nut_ind_tot_period_scenario90 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario90, .)*1e-6),
      release_nut_pop_tot_period_scenario90 = seq_along(release_nut_ind_tot_period_scenario90) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario90, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario90,
                     scat_boot_scenario90)) |>
    dplyr::mutate(
      # scenario 80%
      release_nut_ind_tot_period_scenario80 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario80, .)*1e-6),
      release_nut_pop_tot_period_scenario80 = seq_along(release_nut_ind_tot_period_scenario80) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario80, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario80,
                     scat_boot_scenario80)) |>
    dplyr::mutate(
      # scenario 70%
      release_nut_ind_tot_period_scenario70 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario70, .)*1e-6),
      release_nut_pop_tot_period_scenario70 = seq_along(release_nut_ind_tot_period_scenario70) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario70, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario70,
                     scat_boot_scenario70)) |>
    dplyr::mutate(
      # scenario 60%
      release_nut_ind_tot_period_scenario60 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario60, .)*1e-6),
      release_nut_pop_tot_period_scenario60 = seq_along(release_nut_ind_tot_period_scenario60) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario60, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario60,
                     scat_boot_scenario60)) |>
    dplyr::mutate(
      # scenario 50%
      release_nut_ind_tot_period_scenario50 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario50, .)*1e-6),
      release_nut_pop_tot_period_scenario50 = seq_along(release_nut_ind_tot_period_scenario50) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario50, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario50,
                     scat_boot_scenario50)) |>
    dplyr::mutate(
      # scenario 40%
      release_nut_ind_tot_period_scenario40 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario40, .)*1e-6),
      release_nut_pop_tot_period_scenario40 = seq_along(release_nut_ind_tot_period_scenario40) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario40, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario40,
                     scat_boot_scenario40)) |>
    dplyr::mutate(
      # scenario 30%
      release_nut_ind_tot_period_scenario30 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario30, .)*1e-6),
      release_nut_pop_tot_period_scenario30 = seq_along(release_nut_ind_tot_period_scenario30) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario30, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario30,
                     scat_boot_scenario30)) |>
    dplyr::mutate(
      # scenario 20%
      release_nut_ind_tot_period_scenario20 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario20, .)*1e-6),
      release_nut_pop_tot_period_scenario20 = seq_along(release_nut_ind_tot_period_scenario20) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario20, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario20,
                     scat_boot_scenario20)) |>
    dplyr::mutate(
      # scenario 10%
      release_nut_ind_tot_period_scenario10 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario10, .)*1e-6),
      release_nut_pop_tot_period_scenario10 = seq_along(release_nut_ind_tot_period_scenario10) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario10, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario10,
                     scat_boot_scenario10)) |>
    dplyr::mutate(
      # scenario 00%
      release_nut_ind_tot_period_scenario00 = seq_along(release_dm_ind_period_tot) |> 
        # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
        purrr::map(~ purrr::pluck(release_dm_ind_period_tot, ., 1) * purrr::pluck(scat_boot_scenario00, .)*1e-6),
      release_nut_pop_tot_period_scenario00 = seq_along(release_nut_ind_tot_period_scenario00) |>
        purrr::map(~ purrr::pluck(release_nut_ind_tot_period_scenario00, .) |> 
                     tidyr::pivot_longer(cols = c(P:Co), 
                                         names_to = "Nutrient", 
                                         values_to = "release_ind_tot_period") |> 
                     dplyr::group_by(Nutrient) |> 
                     dplyr::summarise(pop_tot_release_period = sum(release_ind_tot_period)) |> 
                     tidyr::pivot_wider(names_from = Nutrient, 
                                        values_from = pop_tot_release_period) |> 
                     dplyr::select(P, Fe, Zn, Cu, Mn, Se, Co))) |>
    dplyr::select(-c(release_nut_ind_tot_period_scenario00,
                     scat_boot_scenario00)) |>
    # now clean up the dataset and keep only what's needed
    dplyr::select(c(Site, 
                    simu_count,
                    release_nut_pop_tot_period_all_scats_sites, 
                    release_nut_pop_tot_period_scenario00,
                    release_nut_pop_tot_period_scenario10, 
                    release_nut_pop_tot_period_scenario20, 
                    release_nut_pop_tot_period_scenario30, 
                    release_nut_pop_tot_period_scenario40, 
                    release_nut_pop_tot_period_scenario50, 
                    release_nut_pop_tot_period_scenario60, 
                    release_nut_pop_tot_period_scenario70, 
                    release_nut_pop_tot_period_scenario80, 
                    release_nut_pop_tot_period_scenario90, 
                    release_nut_pop_tot_period_scenario100))

}
