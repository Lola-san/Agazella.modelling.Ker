################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# 02_diet.R
#
################################################################################


#'
#'
#'
#'
# select studies on Kerguelen only and 
# identify sp of fish that were analysed
diet_data_Ker <- function(diet_tab, 
                             compo_fish) {
  diet_tab |>
    dplyr::filter(Location  == "Kerguelen Islands") |>
    dplyr::mutate(fish_analysed = dplyr::case_when(Species %in% compo_fish$Species ~ 1, 
                                                   TRUE ~ 0)) |>
    dplyr::arrange(fish_analysed)
}

#'
#'
#'
#'
# select studies on Kerguelen only and with quantitative description of diet 
# with % of weight of each prey species in the diet
# identify sp of fish that were analysed
diet_data_Ker_Wpercent <- function(diet_tab, 
                             compo_fish) {
  diet_tab |>
    dplyr::filter(Location  == "Kerguelen Islands", 
                  # select only studies estimating %W
                  Source %in% c("Cherel et al 1997", 
                                "Lea et al 2002")) |>
    dplyr::mutate(fish_analysed = dplyr::case_when(Species %in% compo_fish$Species ~ 1, 
                                                   TRUE ~ 0)) |>
    dplyr::arrange(fish_analysed)
}


#'
#'
#'
#'
# identify sp of fish not analysed and included in the diets
diet_data_Ker_sp_notanalysed <- function(diet_tab_KerW) {
  diet_tab_KerW |>
    dplyr::filter(fish_analysed == 0) |>
    dplyr::mutate(`%W` = as.numeric(`%W`)) |>
    dplyr::group_by(Species, Genus, Family, Order, Taxa) |>
    dplyr::summarise(min_W = min(`%W`, na.rm = TRUE), 
                     max_W = max(`%W`, na.rm = TRUE))
}


#'
#'
#'
#'
# identify sp of fish analysed and included in the diets
diet_data_Ker_sp_analysed <- function(diet_tab_KerW) {
  diet_tab_KerW |>
    dplyr::filter(fish_analysed == 1) |>
    dplyr::mutate(`%W` = as.numeric(`%W`)) |>
    dplyr::group_by(Species, Genus, Family, Order, Taxa) |>
    dplyr::summarise(min_W = min(`%W`, na.rm = TRUE), 
                     max_W = max(`%W`, na.rm = TRUE))
}


#'
#'
#'
#'
# summarise data on diets for each quantitative study, how many analysed species
# and not analysed
summary_diet_data <- function(diet_tab_KerW) {
  diet_tab_KerW |>
    dplyr::group_by(Source, Year_collection, Site, n) |>
    dplyr::summarize(n_species_prey = dplyr::n_distinct(Species, na.rm = TRUE), 
                     n_species_fish_analysed = sum(fish_analysed), 
                     n_prey_not_analysed = n_species_prey - n_species_fish_analysed)
}


#'
#'
#'
#'
# final data set with only quantitative sources and analysed species 
# with diets set to 100%
diet_data_for_simulations <- function(diet_tab_KerW) {
  diet_tab_KerW |>
    dplyr::filter(fish_analysed == 1 | (is.na(Species) & Genus == "Gymnoscopelus") |
                    (is.na(Species) & is.na(Genus) & Family == "Myctophidae") |
                    (is.na(Species) & is.na(Genus) & Family == "Channichthyidae") |
                    (is.na(Species) & is.na(Genus) & is.na(Family) & Taxa == "Fish")) |>
    dplyr::rename(W = `%W`) |>
    dplyr::mutate(W = as.numeric(W)) |>
    tidyr::drop_na(W) |>
    # drop unwated columns/info
    dplyr::select(-c(Order, `%N`, `%SSFO or %FO`, fish_analysed))
}

#'
#'
#'
#'
# for quantitative study, keep only species analysed, set %W to sum to 100
# and display the different diets obtaines see how similar they are
fig_quant_diet_data_prey_analysed <- function(diet_tab_input) {
  diet_tab_input |>
    dplyr::group_by(Year_collection) |>
    dplyr::mutate(W = 100*(W/sum(W)), 
                  name = dplyr::case_when(is.na(Family) ~ Taxa, 
                                          is.na(Genus) ~ Family, 
                                          is.na(Species) ~ Genus,
                                          !is.na(Species) ~ Species)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = name, 
                                   y = W, 
                                   fill = factor(Year_collection)), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(0.5)) +
    ggplot2::coord_flip()
  
}



#'
#'
#'
#'
# for quantitative study, keep only species analysed, set %W to sum to 100
# and display the different diets obtaines see how similar they are
fig_quant_diet_data_prey_analysed_fam <- function(diet_tab_input) {
  diet_tab_input |>
    dplyr::group_by(Year_collection) |>
    dplyr::mutate(W = 100*(W/sum(W)), 
                  name = dplyr::case_when(is.na(Family) ~ Taxa, 
                                          is.na(Genus) ~ Family, 
                                          is.na(Species) ~ Genus,
                                          !is.na(Species) ~ Species)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Family, 
                                   y = W, 
                                   fill = factor(Year_collection)), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(0.5)) +
    ggplot2::coord_flip()
  
}