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
Ker_col_count_data <- function() {
  
  tibble::tribble(~ Site, ~ min_count, ~ mean_count, ~ max_count, 
                  "Pointe Suzanne", 600, 700, 800,
                  "Cap Noir", 400, 500, 600) |>
        tidyr::nest(initial_counts = c(min_count, mean_count, max_count))
  
}

#'
#'
#'
#'
# final pop data with colony count variability, body size variability, 
# metabolic index variability and energy in diet variability
pop_data_for_simulations <- function(pop_count_tib, 
                                     nsim) {
  
  pop_count_tib |>
    dplyr::mutate(pop_count = seq_along(initial_counts) |>
                    purrr::map(~ tibble::tibble(simu_count = round(runif(n = nsim,  
                                                                         min = purrr::pluck(initial_counts, ., "min_count"), 
                                                                         max = purrr::pluck(initial_counts, ., "max_count")), 
                                                                   0)))) |>
    tidyr::unnest(pop_count) |>
    dplyr::mutate(BM = seq_along(initial_counts) |>
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 25, 
                                                                             sd = 0.2*25, 
                                                                             a = 15, 
                                                                             b = 35))),  
                  Beta = seq_along(initial_counts) |>
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 2.5, 
                                                                             sd = 0.1*2.5, 
                                                                             a = 2, 
                                                                             b = 4))), 
                  NRJ_diet = seq_along(initial_counts) |> # mean diet energy content from TJDD2015
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 7.75*1e3, # kJ per g to kJ per kg
                                                                             sd = 2.47*1e3, 
                                                                             a = 4*1e3, 
                                                                             b = 14*1e3))), 
                  dm_ration = seq_along(initial_counts) |> # % dry matter in ration
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                       min = 0.15, 
                                       max = 0.25))), 
                  dm_release = seq_along(initial_counts) |> # % dry matter from ration that is released as feces
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                       min = 0.10, 
                                       max = 0.20))),
                  duration_of_stay = seq_along(initial_counts) |> # nb of days spent "on land" (approx four months)
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                                             min = 100, 
                                                             max = 140))),
                  time_on_land = seq_along(initial_counts) |> # % of time spent on land
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                                             min = 0.20, 
                                                             max = 0.40))))
  
}

