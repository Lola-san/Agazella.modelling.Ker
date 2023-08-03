################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# 05_run_model.R
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

run_model <- function(input_tib, nsim) {
  input_tib |> 
    ###### SIMULATE UNCERTAINTY IN MASS, BETA, ABUNDANCE DATA, EXCRETION
    dplyr::mutate(
      ############################ COMPUTE INDIVIDUAL NRJTIC DATA, NEEDS AND CONSUMPTION OF POP ######  
      Indi_data = seq_along(BM) |>
        purrr::map(~ kleiber(beta = purrr::pluck(Beta, ., 1), 
                             mass = purrr::pluck(BM, ., 1), 
                             n_sim = nsim, 
                             assimil_mean = 0.85, assimil_sd = 0.05, assimil_a = 0.8, assimil_b = 0.9,
                             dietQuality = purrr::pluck(NRJ_diet, ., 1))), 
      conso_nut_ind = seq_along(Indi_data) |> # Individual consumption of nutrient in mg by ration
        purrr::map(~ purrr::pluck(Indi_data, ., "Ration") * purrr::pluck(nut_diet_full, .)), 
      release_nut_ind = seq_along(nut_release_rate) |> # Individual nutrient release by ration (mg/ration)
        purrr::map(~ purrr::pluck(conso_nut_ind, .) * purrr::pluck(nut_release_rate, .))
  ) 
}