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
prepare_compo_data <- function(compo_fish) {
  compo_fish |>
    dplyr::mutate(Genus = stringr::str_split_i(Species, " ", 1),
                  Taxa = "Fish") |>
    dplyr::bind_rows(# "Gymnoscopelus spp"
      compo_fish |>
        dplyr::mutate(Genus = stringr::str_split_i(Species, " ", 1),
                      Taxa = "Fish") |>
        dplyr::filter(Genus == "Gymnoscopelus") |>
        dplyr::mutate(Species = NA), 
      # "Myctophidae spp"
      compo_fish |>
        dplyr::mutate(Genus = stringr::str_split_i(Species, " ", 1),
                      Taxa = "Fish") |>
        dplyr::filter(Family == "Myctophidae") |>
        dplyr::mutate(Species = NA, 
                      Genus = NA), 
      # "Fish spp"
      compo_fish |>
        dplyr::mutate(Genus = stringr::str_split_i(Species, " ", 1),
                      Taxa = "Fish") |>
        dplyr::mutate(Species = NA, 
                      Genus = NA, 
                      Family = NA))  
}

#'
#'
#'
#'
# bootstrapp composition data per Species or group of species 
bootstrap_compo_data <- function(compo_fish, 
                                 nsim) {
  compo_fish |>
    dplyr::group_by(Species, Genus, Family, Taxa) |>
    dplyr::slice_sample(n = nsim, replace = TRUE)  
}


#'
#'
#'
#'
# change dry weight to wet weight concentrations 
compot_data_ww <- function(compo_fish_boot) {
  compo_fish_boot |>
    tidyr::pivot_longer(cols = c(As:Zn), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    # given a water content of 80% for all fish
    dplyr::mutate(concentration_mg_kg_ww = 0.2*concentration_mg_kg_dw) |>
    dplyr::select(-concentration_mg_kg_dw) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = concentration_mg_kg_ww)
}

#'
#'
#'
#'
# compute nutrient content in diet 
compute_nut_in_diet <- function(diet_data_input,
                                compo_fish_boot_ww 
) {
  diet_data_input |>
    dplyr::mutate(nut_diet_sp = seq_along(diet) |>
                    purrr::map(~ purrr::pluck(diet, .) |>
                                 # add column with %W in diet associated to each prey_group, for each pred (ie.line)
                                 dplyr::left_join(y = compo_fish_boot_ww,
                                                  by = c("Species", "Genus", 
                                                         "Family", "Taxa")) |>
                                 dplyr::mutate(As = As*(W/100),
                                               Ca = Ca*(W/100),
                                               Co = Co*(W/100),
                                               Cu = Cu*(W/100),
                                               Fe = Fe*(W/100),
                                               K = K*(W/100),
                                               Mg = Mg*(W/100),
                                               Mn = Mn*(W/100),
                                               Na = Na*(W/100),
                                               Ni = Ni*(W/100),
                                               P = P*(W/100),
                                               Se = Se*(W/100),
                                               Zn = Zn*(W/100), 
                                               name = dplyr::case_when(is.na(Family) ~ Taxa, 
                                                                       is.na(Genus) ~ Family, 
                                                                       is.na(Species) ~ Genus,
                                                                       !is.na(Species) ~ Species)) |>
                                 dplyr::select(-c(Code_sample, W, Species, Genus, Family, Taxa)) |>
                                 tidyr::nest(nut = c("As":"Zn")) |>
                                 tidyr::pivot_wider(names_from = name,
                                                    values_from = nut)),
                  nut_diet_full = # different number of species depending on diet data
                    dplyr::case_when(# Cherel et. al 1997: 10 species
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
                                                    purrr::pluck(nut_diet_sp, ., 10, 1)),
                      # Lea et. al 2002, year collection 1998 : 17 species
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
                                                    purrr::pluck(nut_diet_sp, ., 17, 1)),
                      # Lea et. al 2002, year collection 1999 : 14 species
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
                                                    purrr::pluck(nut_diet_sp, ., 14, 1)),
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