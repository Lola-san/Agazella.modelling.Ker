################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# 06_see_outputs.R
#
################################################################################


#'
#'
#'
#'
#'
# 
fig_nut_release_ind_diets <- function(output_tib) {
  output_tib |>
    tidyr::unnest(release_nut_ind) |>
    tidyr::pivot_longer(cols = c(As:Zn), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                   levels = c("Ca", "P", "Na", "K", "Mg", 
                                              "Fe", "Zn", "Cu", "Mn", "Se",
                                              "As", "Ni","Co")), 
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000")))  |>
    dplyr::group_by(Source, Year_collection, Nutrient) |>
    dplyr::summarize(min = min(individual_release), 
                     `2.5_quant` = quantile(individual_release, probs = c(0.025)), 
                     mean = mean(individual_release), 
                     median = median(individual_release), 
                     `97.5_quant` = quantile(individual_release, probs = c(0.975)), 
                     max = max(individual_release)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Year_collection, y = mean, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Year_collection, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`), 
                            linewidth = 1, 
                            position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y")
  
}


#'
#'
#'
#'
#'
# 
fig_nut_release_pop_diets <- function(output_tib) {
  output_tib |>
    tidyr::unnest(release_nut_ind) |>
    tidyr::pivot_longer(cols = c(As:Zn), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co")), 
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000")))  |>
    dplyr::group_by(Source, Year_collection, Nutrient) |>
    dplyr::summarize(tot_pop_release = sum(individual_release)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Year_collection, y = tot_pop_release, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y")
  
}


#'
#'
#'
#'
#'
# 
tab_nut_release_pop_diets <- function(output_tib) {
  output_tib |>
    tidyr::unnest(release_nut_ind) |>
    tidyr::pivot_longer(cols = c(As:Zn), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Na", "K", "Mg", 
                                               "Fe", "Zn", "Cu", "Mn", "Se",
                                               "As", "Ni","Co")), 
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000")))  |>
    dplyr::group_by(Source, Year_collection, Nutrient) |>
    dplyr::summarize(tot_pop_release = sum(individual_release)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = tot_pop_release)
  
}