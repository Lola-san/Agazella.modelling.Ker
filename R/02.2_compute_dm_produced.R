################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 02.2_compute_dm_produced.R
#
################################################################################


# to compute the daily need of an individual (Kleiber equation) of a given species
kleiber <- function(beta, mass, n_sim, 
                    assimil_mean = NULL,  assimil_sd = 0.05,
                    assimil_a, assimil_b,
                    dietQuality
) {
  # should the daily ration be computed?
  if(!is.null(assimil_mean) && !is.null(dietQuality)) {
    a <- truncnorm::rtruncnorm(n = n_sim, 
                               mean = assimil_mean, 
                               sd = assimil_sd, 
                               a = assimil_a, 
                               b = assimil_b) # assimilation 
    
    return(tibble::tibble(ADMR = beta * (293.1*mass^(3/4)),
                          A_rate = a,
                          Ration = beta * (293.1*mass^(3/4))/(a*dietQuality),
                          PercentBM = 293.1*beta/(a*dietQuality*mass^(1/4)))
    )
  }
  else { return(list(ADMR = beta * (293.1*mass^(3/4)))) }
}



############################# COMPUTATION ######################################

run_dm_estimate <- function(input_tib) {
  
  input_tib |> 
    ###### SIMULATE UNCERTAINTY IN MASS, BETA, ABUNDANCE DATA, EXCRETION
    dplyr::mutate(
      ############################ COMPUTE INDIVIDUAL NRJTIC DATA, NEEDS AND CONSUMPTION OF POP ######  
      Indi_data = seq_along(BM) |>
        purrr::map(~ kleiber(beta = purrr::pluck(Beta, ., 1), 
                             mass = purrr::pluck(BM, ., 1), 
                             n_sim = purrr::pluck(simu_count, .), 
                             assimil_mean = 0.85, assimil_sd = 0.05, assimil_a = 0.8, assimil_b = 0.9,
                             dietQuality = purrr::pluck(NRJ_diet, ., 1))), 
      ############################ COMPUTE CONSUMPTION OF FOOD ######
      # Individual consumption of dry matter in mg by ration
      conso_food_dm_ind_daily = seq_along(Indi_data) |> 
        purrr::map(~ purrr::pluck(Indi_data, ., "Ration") * purrr::pluck(dm_ration, .)), 
      # Individual release of dry matter by ration (kg/ration)
      release_dm_ind_daily = seq_along(conso_food_dm_ind_daily) |> 
        purrr::map(~ purrr::pluck(conso_food_dm_ind_daily, .) * purrr::pluck(dm_release, .)), 
      # total population production of dry matter for one ration (in kg/ration) 
      release_dm_pop_tot_daily = seq_along(release_dm_ind_daily) |> 
        purrr::map(~ sum(purrr::pluck(release_dm_ind_daily, .))), 
      # total population production of dry matter for one ration (in kg/ration) on land
      release_dm_pop_on_land_daily = seq_along(release_dm_ind_daily) |> 
        purrr::map(~ sum(purrr::pluck(release_dm_ind_daily, .) * purrr::pluck(time_on_land, . ))), 
      # total population production of dry matter for one ration (in kg/ration) at sea
      release_dm_pop_at_sea_daily = seq_along(release_dm_pop_on_land_daily) |> 
        purrr::map(~ sum(purrr::pluck(release_dm_ind_daily, .) * (1 - purrr::pluck(time_on_land, .)))),
      # Individual release of dry matter during the 4 mths period of stay
      release_dm_ind_period = seq_along(release_dm_ind_daily) |> 
        purrr::map(~ purrr::pluck(release_dm_ind_daily, .) * purrr::pluck(duration_of_stay, .)),
      # total population production of dry matter during the 4 mths period of stay
      release_dm_pop_tot_period = seq_along(release_dm_ind_period) |> 
        purrr::map(~ sum(purrr::pluck(release_dm_ind_period, .))), 
      # total population production of dry matter on land during the 4 mths period of stay
      release_dm_pop_on_land_period = seq_along(release_dm_ind_period) |> 
        purrr::map(~ sum(purrr::pluck(release_dm_ind_period, .) * purrr::pluck(time_on_land, . ))), 
      # total population production of dry matter at sea during the 4 mths period of stay
      release_dm_pop_at_sea_period = seq_along(release_dm_pop_on_land_period) |> 
        purrr::map(~ sum(purrr::pluck(release_dm_ind_period, .) *(1 - purrr::pluck(time_on_land, . ))))
    )  |> 
    dplyr::mutate(release_dm_pop_tot_daily = unlist(release_dm_pop_tot_daily),
                  release_dm_pop_on_land_daily = unlist(release_dm_pop_on_land_daily),
                  release_dm_pop_at_sea_daily = unlist(release_dm_pop_at_sea_daily),
                  release_dm_pop_tot_period = unlist(release_dm_pop_tot_period),
                  release_dm_pop_on_land_period = unlist(release_dm_pop_on_land_period),
                  release_dm_pop_at_sea_period = unlist(release_dm_pop_at_sea_period))
}
