################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# 03_prepare_diet_input_data.R
#
################################################################################



#'
#'
#'
#'
# format data with nest per source
format_data <- function(diet_tab_input) {
  diet_tab_input |>
    dplyr::group_by(Location, Site, Source, Year_collection, n) |>
    tidyr::nest(diet = c(Species, Genus, Family, Taxa, W))
  
}



#'
#'
#'
#'
# add Beta, Body Mass, AE and diet energy content as random variables
add_metabolic_data <- function(diet_tab_input_nested, 
                               nsim) {
  diet_tab_input_nested |>
    dplyr::mutate(BM = seq_along(diet) |>
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = nsim, 
                                                             mean = 25, 
                                                             sd = 0.2*25, 
                                                             a = 15, 
                                                             b = 35))), 
                  Beta = seq_along(diet) |>
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = nsim, 
                                                                             mean = 2.5, 
                                                                             sd = 0.1*2.5, 
                                                                             a = 2, 
                                                                             b = 4))), 
                  NRJ_diet = seq_along(diet) |> # mean diet energy content from TJDD2015
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = nsim, 
                                                                             mean = 7.75*1e3, # kJ per g to kJ per kg
                                                                             sd = 2.47*1e3, 
                                                                             a = 4*1e3, 
                                                                             b = 14*1e3))), 
                  nut_release_rate = seq_along(diet) |> 
                    purrr::map(~ tibble::tibble(As = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9), 
                                                Ca = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                Co = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                Cu = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                Fe = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                K = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                Mg = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                Mn = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                Na = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                Ni = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                P = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                Se = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9),
                                                Zn = runif(n = nsim,  
                                                           min = 0.7, 
                                                           max = 0.9)))) 
  
}

