################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# 04_compute_nut_in_diet.R
#
################################################################################



#'
#'
#'
#'
# for the composition dataset, add lines with all samples for categories where 
# prey was only identified to level Genus/Family/Taxa
prepare_compo_data_prey <- function(compo_prey, 
                                    location_restriction # "Kerguelen" or "all"
                                    ) {
  
  if (location_restriction == "Kerguelen") {
    compo_prey <- compo_prey |>
      dplyr::filter(Location == "Kerguelen") |>
      dplyr::bind_rows(
        # "Gymnoscopelus spp"
        compo_prey |>
          dplyr::filter(Location == "Kerguelen",
                        Genus == "Gymnoscopelus") |>
          dplyr::mutate(Species = as.character(NA)), 
        # "Myctophidae spp"
        compo_prey |>
          dplyr::filter(Location == "Kerguelen",
                        Family == "Myctophidae") |>
          dplyr::mutate(Species = as.character(NA), 
                        Genus = as.character(NA)), 
        # "Fish spp"
        compo_prey |>
          dplyr::filter(Location == "Kerguelen",
                        Taxa == "Fish") |>
          dplyr::mutate(Species = as.character(NA), 
                        Genus = as.character(NA), 
                        Family = as.character(NA), 
                        Order = as.character(NA)), 
        # "Perciformes spp" (not Perciformes spp in diet data but we don't have
        # neither the species nor the family that is included in the diet)
        compo_prey |>
          dplyr::filter(Location == "Kerguelen",
                        Order == "Perciformes") |>
          dplyr::mutate(Species = as.character(NA), 
                        Genus = as.character(NA), 
                        Family = as.character(NA))) 
  } else if (location_restriction == "all") {
    compo_prey <- compo_prey |>
      # Change Cephalopod/Ommastrephidae/Illex coindetti or Todaropsis eblanae
      # to Cephalopod/Ommastrephidae because we'll use them for the ceph 
      # species we don't have in Kerguelen dataset
      dplyr::mutate(Species = dplyr::case_when(Family == "Ommastrephidae" ~ as.character(NA),
                                               TRUE ~ Species),  
                    Genus = dplyr::case_when(Family == "Ommastrephidae" ~ as.character(NA),
                                             TRUE ~ Genus)) |>
      dplyr::bind_rows(
        # "Gymnoscopelus spp"
        compo_prey |>
          dplyr::filter(Genus == "Gymnoscopelus") |>
          dplyr::mutate(Species = as.character(NA)), 
        # "Myctophidae spp"
        compo_prey |>
          dplyr::filter(Family == "Myctophidae") |>
          dplyr::mutate(Species = as.character(NA), 
                        Genus = as.character(NA)), 
        # "Fish spp"
        compo_prey |>
          dplyr::filter(Taxa == "Fish") |>
          dplyr::mutate(Species = as.character(NA), 
                        Genus = as.character(NA), 
                        Family = as.character(NA), 
                        Order = as.character(NA)), 
        # "Perciformes spp" (not Perciformes spp in diet data but we don't have
        # neither the species nor the family that is included in the diet)
        compo_prey |>
          dplyr::filter(Order == "Perciformes") |>
          dplyr::mutate(Species = as.character(NA), 
                        Genus = as.character(NA), 
                        Family = as.character(NA)) 
        ) 
  }
  
  compo_prey |>
    dplyr::select(c(Code_sample, Species, Genus, Family, Order, Taxa, 
                    Fe, Zn, Cu, Mn, Se, Co))
}


#'
#'
#'
#'
# bootstrapp composition data per Species or group of species 
bootstrap_compo_data <- function(compo_prey, 
                                 nsim) {
  compo_prey |>
    dplyr::group_by(Species, Genus, Family, Order, Taxa) |>
    dplyr::slice_sample(n = nsim, replace = TRUE)  
}


#'
#'
#'
#'
# change dry weight to wet weight concentrations 
compot_data_ww <- function(compo_prey_boot) {
  compo_prey_boot |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # given a water content of 80% for all fish
    dplyr::mutate(concentration_mg_kg_ww = 0.2*concentration_mg_kg_dw) |>
    dplyr::select(-concentration_mg_kg_dw) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = concentration_mg_kg_ww) |>
    tidyr::unnest(c(Fe, Zn, Cu, Mn, Se, Co))
}

#'
#'
#'
#'
# compute nutrient content in diet 
compute_nut_in_diet_fish_Ker <- function(diet_data_input,
                                compo_fish_Ker_boot_ww 
) {
  diet_data_input |>
    dplyr::mutate(nut_diet_sp = seq_along(diet) |>
                    purrr::map(~ purrr::pluck(diet, .) |>
                                 # add column with %W in diet associated to each prey_group, for each pred (ie.line)
                                 dplyr::left_join(y = compo_fish_Ker_boot_ww,
                                                  by = c("Species", "Genus", 
                                                         "Family", 
                                                         "Order", "Taxa")) |>
                                 dplyr::mutate(Fe = Fe*(W/100),
                                               Zn = Zn*(W/100),
                                               Cu = Cu*(W/100),
                                               Mn = Mn*(W/100),
                                               Se = Se*(W/100),
                                               Co = Co*(W/100), 
                                               name = dplyr::case_when(is.na(Order) ~ Taxa,
                                                                       is.na(Family) ~ Order,  
                                                                       is.na(Genus) ~ Family, 
                                                                       is.na(Species) ~ Genus,
                                                                       !is.na(Species) ~ Species)) |>
                                 dplyr::select(c(Fe, Zn, Cu, Mn, Se, Co, name)) |>
                                 tidyr::nest(nut = c("Fe":"Co")) |>
                                 tidyr::pivot_wider(names_from = name,
                                                    values_from = nut)),
                  nut_diet_full = # different number of species depending on diet data
                    dplyr::case_when(# Cherel et. al 1997: 11 species
                      Year_collection == 1994 ~ seq_along(nut_diet_sp) |>
                                       purrr::map(~ purrr::pluck(nut_diet_sp, ., 1, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 2, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 3, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 4, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 5, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 6, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 7, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 8, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 9, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 10, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 11, 1)),
                      # Lea et. al 2002, year collection 1998 : 18 species
                                     Year_collection == 1998 ~ seq_along(nut_diet_sp) |>
                                       purrr::map(~ purrr::pluck(nut_diet_sp, ., 1, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 2, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 3, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 4, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 5, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 6, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 7, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 8, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 9, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 10, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 11, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 12, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 13, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 14, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 15, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 16, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 17, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 18, 1)),
                      # Lea et. al 2002, year collection 1999 : 15 species
                                     Year_collection == 1999 ~ seq_along(nut_diet_sp) |>
                                       purrr::map(~ purrr::pluck(nut_diet_sp, ., 1, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 2, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 3, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 4, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 5, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 6, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 7, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 8, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 9, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 10, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 11, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 12, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 13, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 14, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 15, 1)),
                      # Lea et. al 2002, year collection 2000 : 13 species
                                     Year_collection == 2000 ~ seq_along(nut_diet_sp) |>
                                       purrr::map(~ purrr::pluck(nut_diet_sp, ., 1, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 2, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 3, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 4, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 5, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 6, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 7, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 8, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 9, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 10, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 11, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 12, 1) +
                                                    purrr::pluck(nut_diet_sp, ., 13, 1))) )
}


#'
#'
#'
#'
# compute nutrient content in diet 
compute_nut_in_diet_all_prey <- function(diet_data_input,
                                          compo_prey_boot_ww 
) {
  diet_data_input |>
    dplyr::mutate(nut_diet_sp = seq_along(diet) |>
                    purrr::map(~ purrr::pluck(diet, .) |>
                                 # add column with %W in diet associated to each prey_group, for each pred (ie.line)
                                 dplyr::left_join(y = compo_prey_boot_ww,
                                                  by = c("Species", "Genus", 
                                                         "Family", 
                                                         "Order", "Taxa")) |>
                                 dplyr::mutate(Fe = Fe*(W/100),
                                               Zn = Zn*(W/100),
                                               Cu = Cu*(W/100),
                                               Mn = Mn*(W/100),
                                               Se = Se*(W/100),
                                               Co = Co*(W/100), 
                                               name = dplyr::case_when(is.na(Order) ~ Taxa,
                                                                       is.na(Family) ~ Order, 
                                                                       is.na(Genus) ~ Family, 
                                                                       is.na(Species) ~ Genus,
                                                                       !is.na(Species) ~ Species)) |>
                                 dplyr::select(c(Fe, Zn, Cu, Mn, Se, Co, name)) |>
                                 tidyr::nest(nut = c("Fe":"Co")) |>
                                 tidyr::pivot_wider(names_from = name,
                                                    values_from = nut)),
                  nut_diet_full = # different number of species depending on diet data
                    dplyr::case_when(# Cherel et. al 1997: 11 species
                      Year_collection == 1994 ~ seq_along(nut_diet_sp) |>
                        purrr::map(~ purrr::pluck(nut_diet_sp, ., 1, 1) +
                                     purrr::pluck(nut_diet_sp, ., 2, 1) +
                                     purrr::pluck(nut_diet_sp, ., 3, 1) +
                                     purrr::pluck(nut_diet_sp, ., 4, 1) +
                                     purrr::pluck(nut_diet_sp, ., 5, 1) +
                                     purrr::pluck(nut_diet_sp, ., 6, 1) +
                                     purrr::pluck(nut_diet_sp, ., 7, 1) +
                                     purrr::pluck(nut_diet_sp, ., 8, 1) +
                                     purrr::pluck(nut_diet_sp, ., 9, 1) +
                                     purrr::pluck(nut_diet_sp, ., 10, 1) +
                                     purrr::pluck(nut_diet_sp, ., 11, 1)),
                      # Lea et. al 2002, year collection 1998 : 19 species
                      Year_collection == 1998 ~ seq_along(nut_diet_sp) |>
                        purrr::map(~ purrr::pluck(nut_diet_sp, ., 1, 1) +
                                     purrr::pluck(nut_diet_sp, ., 2, 1) +
                                     purrr::pluck(nut_diet_sp, ., 3, 1) +
                                     purrr::pluck(nut_diet_sp, ., 4, 1) +
                                     purrr::pluck(nut_diet_sp, ., 5, 1) +
                                     purrr::pluck(nut_diet_sp, ., 6, 1) +
                                     purrr::pluck(nut_diet_sp, ., 7, 1) +
                                     purrr::pluck(nut_diet_sp, ., 8, 1) +
                                     purrr::pluck(nut_diet_sp, ., 9, 1) +
                                     purrr::pluck(nut_diet_sp, ., 10, 1) +
                                     purrr::pluck(nut_diet_sp, ., 11, 1) +
                                     purrr::pluck(nut_diet_sp, ., 12, 1) +
                                     purrr::pluck(nut_diet_sp, ., 13, 1) +
                                     purrr::pluck(nut_diet_sp, ., 14, 1) +
                                     purrr::pluck(nut_diet_sp, ., 15, 1) +
                                     purrr::pluck(nut_diet_sp, ., 16, 1) +
                                     purrr::pluck(nut_diet_sp, ., 17, 1) +
                                     purrr::pluck(nut_diet_sp, ., 18, 1) +
                                     purrr::pluck(nut_diet_sp, ., 19, 1)),
                      # Lea et. al 2002, year collection 1999 : 16 species
                      Year_collection == 1999 ~ seq_along(nut_diet_sp) |>
                        purrr::map(~ purrr::pluck(nut_diet_sp, ., 1, 1) +
                                     purrr::pluck(nut_diet_sp, ., 2, 1) +
                                     purrr::pluck(nut_diet_sp, ., 3, 1) +
                                     purrr::pluck(nut_diet_sp, ., 4, 1) +
                                     purrr::pluck(nut_diet_sp, ., 5, 1) +
                                     purrr::pluck(nut_diet_sp, ., 6, 1) +
                                     purrr::pluck(nut_diet_sp, ., 7, 1) +
                                     purrr::pluck(nut_diet_sp, ., 8, 1) +
                                     purrr::pluck(nut_diet_sp, ., 9, 1) +
                                     purrr::pluck(nut_diet_sp, ., 10, 1) +
                                     purrr::pluck(nut_diet_sp, ., 11, 1) +
                                     purrr::pluck(nut_diet_sp, ., 12, 1) +
                                     purrr::pluck(nut_diet_sp, ., 13, 1) +
                                     purrr::pluck(nut_diet_sp, ., 14, 1) +
                                     purrr::pluck(nut_diet_sp, ., 15, 1) +
                                     purrr::pluck(nut_diet_sp, ., 16, 1)),
                      # Lea et. al 2002, year collection 2000 : 14 species
                      Year_collection == 2000 ~ seq_along(nut_diet_sp) |>
                        purrr::map(~ purrr::pluck(nut_diet_sp, ., 1, 1) +
                                     purrr::pluck(nut_diet_sp, ., 2, 1) +
                                     purrr::pluck(nut_diet_sp, ., 3, 1) +
                                     purrr::pluck(nut_diet_sp, ., 4, 1) +
                                     purrr::pluck(nut_diet_sp, ., 5, 1) +
                                     purrr::pluck(nut_diet_sp, ., 6, 1) +
                                     purrr::pluck(nut_diet_sp, ., 7, 1) +
                                     purrr::pluck(nut_diet_sp, ., 8, 1) +
                                     purrr::pluck(nut_diet_sp, ., 9, 1) +
                                     purrr::pluck(nut_diet_sp, ., 10, 1) +
                                     purrr::pluck(nut_diet_sp, ., 11, 1) +
                                     purrr::pluck(nut_diet_sp, ., 12, 1) +
                                     purrr::pluck(nut_diet_sp, ., 13, 1) +
                                     purrr::pluck(nut_diet_sp, ., 14, 1)),
                      # Factice diet #1, year collection 2023: 11 species
                      Year_collection == 2023 ~ seq_along(nut_diet_sp) |>
                        purrr::map(~ purrr::pluck(nut_diet_sp, ., 1, 1) +
                                     purrr::pluck(nut_diet_sp, ., 2, 1) +
                                     purrr::pluck(nut_diet_sp, ., 3, 1) +
                                     purrr::pluck(nut_diet_sp, ., 4, 1) +
                                     purrr::pluck(nut_diet_sp, ., 5, 1) +
                                     purrr::pluck(nut_diet_sp, ., 6, 1) +
                                     purrr::pluck(nut_diet_sp, ., 7, 1) +
                                     purrr::pluck(nut_diet_sp, ., 8, 1) +
                                     purrr::pluck(nut_diet_sp, ., 9, 1) +
                                     purrr::pluck(nut_diet_sp, ., 10, 1) +
                                     purrr::pluck(nut_diet_sp, ., 11, 1)),
                      # Factice diet #1, year collection 2023: 11 species
                      Year_collection == 2024 ~ seq_along(nut_diet_sp) |>
                        purrr::map(~ purrr::pluck(nut_diet_sp, ., 1, 1) +
                                     purrr::pluck(nut_diet_sp, ., 2, 1) +
                                     purrr::pluck(nut_diet_sp, ., 3, 1) +
                                     purrr::pluck(nut_diet_sp, ., 4, 1) +
                                     purrr::pluck(nut_diet_sp, ., 5, 1) +
                                     purrr::pluck(nut_diet_sp, ., 6, 1) +
                                     purrr::pluck(nut_diet_sp, ., 7, 1) +
                                     purrr::pluck(nut_diet_sp, ., 8, 1) +
                                     purrr::pluck(nut_diet_sp, ., 9, 1) +
                                     purrr::pluck(nut_diet_sp, ., 10, 1) +
                                     purrr::pluck(nut_diet_sp, ., 11, 1))) )
}






# 
# # Full_diet <- diets_pred %>%
# #   dplyr::mutate(Nut_PG = seq_along(Diet) %>%
# #                   map(~ nutri_df),
# #                 Diet = seq_along(Diet) %>%
# #                   map(~ pluck(Diet, .) %>%
# #                         pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'),
# #                                      names_to = "Prey_group",
# #                                      values_to = "W"))) %>%
# #   # add column with %W in diet associated to each prey_group, for each pred (ie.line)
# #   dplyr::mutate(Nut_PG = seq_along(Nut_PG) %>%
# #                   map(~ pluck(Nut_PG, .) %>%
# #                         left_join(pluck(Diet, .),
# #                                   by = "Prey_group")
# #                   )
# #   )
# # ##### THE SECOND MAP CAUSES AN UNRESOLVED ERROR
# # # what's in works well independently :
# # Full_diet <- diets_pred %>%
# #   dplyr::mutate(Nut_PG = seq_along(Diet) %>%
# #                   map(~ nutri_df),
# #                 Diet = seq_along(Diet) %>%
# #                   map(~ pluck(Diet, .) %>%
# #                         pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'),
# #                                      names_to = "Prey_group",
# #                                      values_to = "W")))
# # # and
# # pluck(Full_diet, "Nut_PG", 5) %>%
# #                  left_join(pluck(Full_diet, "Diet", 5),
# #                            by = "Prey_group")
# # # and this for all lines of Full_diet/diets_pred
# # # So I can't seem to figure out why it doesn't work in this usual map syntax here
# # # I had to find another solution
# # # which is not very elegant....
# 
# # I found a solution!
# Full_diets <- diets_pred %>%
#   dplyr::mutate(Nut_W = seq_along(Diet) %>%
#                   map(~ pluck(Diet, .) %>%
#                         pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'),
#                                      names_to = "Prey_group",
#                                      values_to = "W") %>%
#                         # add column with %W in diet associated to each prey_group, for each pred (ie.line)
#                         left_join(y = nutri_df,
#                                   by = "Prey_group")
#                   )
#   )
# 
# pluck(Full_diets, "Nut_W", 1) %>%
#   summarise(n = length(unique(NRJ)))
# 
# # # join the nut concentration bootstrapped tables per prey_group to the %W of each prey
# # # group in the diet
# # # the list will contain tibble associated to each predator/line in the final df
# # list_df_Nut_W <- list()
# #
# # for (i in c(1:nrow(diets_pred))) {
# #   list_df_Nut_W <- append(list_df_Nut_W,
# #                           list(nutri_df %>%
# #                                  left_join(pluck(diets_pred, "Diet", i) %>%
# #                                              pivot_longer(cols = c('Large demersal energy-lean fish':'Zooplankton'),
# #                                                           names_to = "Prey_group",
# #                                                           values_to = "W"),
# #                                            by = "Prey_group")))
# # }
# #
# # # define final diets df that will contain all diet info
# # Full_diets <- diets_pred
# # Full_diets$Nut_W <- list_df_Nut_W
# 
# Full_diets <- Full_diets %>%
#   mutate(# compute W*elemental concentration
#     Nut_W = seq_along(Nut_W) %>%
#       map(~ pluck(Nut_W, .) %>%
#             mutate(NRJ = NRJ*(W/100),
#                    N = N*(W/100),
#                    P = P*(W/100),
#                    Fe = Fe*(W/100),
#                    Se = Se*(W/100),
#                    Cu = Cu*(W/100),
#                    Zn = Zn*(W/100),
#                    Mn = Mn*(W/100),
#                    Co = Co*(W/100),
#                    As = As*(W/100)) %>%
#             # change it to get one column per prey_group
#             # and one line, each cell containing a full bootstrap tibble
#             # of elemental concentration, size nsim*nelements
#             select(-c(W)) %>%
#             nest(Nut = c("NRJ":"Zn")) %>%
#             pivot_wider(names_from = Prey_group,
#                         values_from = Nut)),
#     # compute the mean concentration of diet by summing these values across prey_groups
#     Nut_diet = seq_along(Nut_W) %>%
#       map(~ pluck(Nut_W, ., 1, 1) +
#             pluck(Nut_W, ., 2, 1) +
#             pluck(Nut_W, ., 3, 1) +
#             pluck(Nut_W, ., 4, 1) +
#             pluck(Nut_W, ., 5, 1) +
#             pluck(Nut_W, ., 6, 1) +
#             pluck(Nut_W, ., 7, 1) +
#             pluck(Nut_W, ., 8, 1) +
#             pluck(Nut_W, ., 9, 1) +
#             pluck(Nut_W, ., 10, 1) +
#             pluck(Nut_W, ., 11, 1) +
#             pluck(Nut_W, ., 12, 1) +
#             pluck(Nut_W, ., 13, 1) ),
#     # NRJ should be a separated column as it will be used to compute the daily ration
#     NRJ_diet = seq_along(Nut_diet) %>%
#       map(~ as_tibble_col(pluck(Nut_diet, ., "NRJ")*1e3)), # from kJ per g to kJ per kg
#     # delete it from Nut_diet tibbles
#     Nut_diet = seq_along(Nut_diet) %>%
#       map(~ pluck(Nut_diet, .) %>%
#             select(-NRJ))
#   ) %>%
#   select(-c(Nut_W))