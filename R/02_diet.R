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
                          compo_prey) {
  diet_tab |>
    dplyr::filter(Location  == "Kerguelen Islands") |>
    dplyr::mutate(fish_analysed = dplyr::case_when(Species %in% compo_prey$Species ~ 1,
                                                   (is.na(Species) & Genus == "Gymnoscopelus") ~ 1,
                                                   (is.na(Species) & is.na(Genus) & Family == "Myctophidae") ~ 1,
                                                   (is.na(Species) & is.na(Genus) & Family == "Channichthyidae") ~ 1,
                                                   (is.na(Species) & is.na(Genus) & is.na(Family) & Taxa == "Fish") ~ 1,
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
diet_data_Ker_Wpercent <- function(diet_tab_Ker, 
                                   compo_prey) {
  diet_tab_Ker |>
    dplyr::filter(Location  == "Kerguelen Islands", 
                  # select only studies estimating %W
                  Source %in% c("Cherel et al 1997", 
                                "Lea et al 2002"))
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


