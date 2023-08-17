################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 02.1_prepare_pop_input_data.R
#
################################################################################


#'
#'
#'
#'
# Colony counts for Cap Noir and Pointe Suzanne 
Ker_summarise_count_data <- function(count_raw_tib) {
  
  # counts are different in the two sites: 
  
  # in Pointe Suzanne, the site was not counted entirely in one day: 
  # two first sectors were counted by the 3 operators on one day (adults + pups)
  # on a another day, other count in sector 3 but only one for adults
  
  # in Cap Noir: one count by a single operator on one day for the entire site
  # and second count by 3 operators on another day
  
  rbind(
    # Cap Noir
    count_raw_tib |>
      dplyr::filter(site == "Cap Noir",
                    species == "A. gazella",
                    status == "pups", 
                    comment == "with beach") |>
      dplyr::rename(Site = site) |>
      dplyr::group_by(Site, operator, date) |>
      dplyr::summarise(min_count = sum(count)), #|>
      # dplyr::select(site, operator, sum_site),
    # Pointe Suzanne
    count_raw_tib |>
      dplyr::filter(site == "Pointe Suzanne", 
                    species == "A. gazella", 
                    status == "pups", 
                    comment == "with beach") |>
      dplyr::rename(Site = site) |>
      dplyr::group_by(Site, operator, date) |>
      dplyr::summarise(min_count = sum(count)) #|>
      # tidyr::pivot_wider(names_from = c(operator, date), 
      #                    values_from = sum_site, 
      #                    names_sep = "_") 
      # dplyr::mutate(LG = `LG_2022-12-31` + `LG_2023-01-04`, 
      #               MC_LG = `MC_2022-12-31` + `LG_2023-01-04`, 
      #               TJDD_LG = `TJDD_2022-12-31` + `LG_2023-01-04`) |>
      # tidyr::pivot_longer(cols = c(LG, MC_LG, TJDD_LG), 
      #                     names_to = "operator",
      #                     values_to = "sum_site") |>
      # dplyr::select(c(site, operator, sum_site))
  ) # |>
    # dplyr::group_by(site) |>
    # dplyr::summarise(min_count = min(sum_site),
    #                  mean_count = round(mean(sum_site), 0),
    #                  max_count = max(sum_site)) |>
    # dplyr::rename(Site = site)
  
   

}

#'
#'
#'
#'
# Colony counts for Cap Noir and Pointe Suzanne 
simulate_count_data <- function(counts_summmarise_pups_tib) {
  
  counts_summmarise_pups_tib |>
    dplyr::mutate(max_count = min_count + 0.2*min_count) |>
        tidyr::nest(initial_counts = c(operator, date, 
                                       min_count, max_count))
  
}

#'
#'
#'
#'
# final pop data with colony count variability, body size variability, 
# metabolic index variability and energy in diet variability
pop_data_for_simulations <- function(pop_count_tib, 
                                     nsim) {
  
  tib_both_sites <- pop_count_tib |>
    dplyr::mutate(pop_count = seq_along(initial_counts) |>
                    purrr::map(~ tibble::tibble(simu_count = round(runif(n = nsim,  
                                                                         min = purrr::pluck(initial_counts, ., "min_count"), 
                                                                         max = purrr::pluck(initial_counts, ., "max_count")), 
                                                                   0)))) |>
    tidyr::unnest(pop_count) |>
    dplyr::mutate(BM = seq_along(initial_counts) |>
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 27, 
                                                                             sd = 0.2*25, 
                                                                             a = 20, 
                                                                             b = 38))),  
                  Beta_sea = seq_along(initial_counts) |> # taken from JDD 2017, Ecol. and Evol. 
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 4.7, 
                                                                             sd = 0.3, 
                                                                             a = 3.5, 
                                                                             b = 6.5))),  
                  Beta_land = seq_along(initial_counts) |> # taken from JDD 2017, Ecol. and Evol. 
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 3.3, 
                                                                             sd = 0.1, 
                                                                             a = 2, 
                                                                             b = 4.5))), 
                  NRJ_diet = seq_along(initial_counts) |> # mean diet energy content from TJDD2015
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 7.75*1e3, # kJ per g to kJ per kg
                                                                             sd = 2.47*1e3, 
                                                                             a = 4*1e3, 
                                                                             b = 13*1e3))), 
                  dm_ration = seq_along(initial_counts) |> # % dry matter in ration
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                       min = 0.15, 
                                       max = 0.25))), 
                  dm_release = seq_along(initial_counts) |> # % dry matter from ration that is released as feces
                    # from Keiver 1994 and Ronald 1984
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                       min = 0.09, 
                                       max = 0.17))),
                  duration_of_stay = seq_along(initial_counts) |> # nb of days spent "on land" (approx four months)
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                                             min = 100, 
                                                             max = 140))),
                  time_on_land = seq_along(initial_counts) |> # % of time spent on land
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                                             min = 0.15, 
                                                             max = 0.35))))
  
  list(CN = tib_both_sites |> 
         dplyr:: filter(Site == "Cap Noir"), 
       PS = tib_both_sites |> 
         dplyr:: filter(Site == "Pointe Suzanne"))
  
}

