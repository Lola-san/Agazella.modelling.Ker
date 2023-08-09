################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# 01.3_prepare_diet_input_data.R
#
################################################################################


#'
#'
#'
#'
# Add factice diet data to diet dataset
# with diets set to 100%
add_factice_diets <- function(diet_tab_KerW) {
  
  diet_tab_KerW |>
      dplyr::bind_rows(
        tibble::tribble(~ Source, ~ Species, ~ Genus, ~ Family, ~ Order, ~ Taxa, ~ `%W`, ~ fish_analysed,  
                        "Factice diet 1", "Protomyctophum tenisoni", "Protomyctophum", "Myctophidae", "Myctophiformes", "Fish", 5, 1,
                        "Factice diet 1", "Protomyctophum bolini", "Protomyctophum", "Myctophidae", "Myctophiformes", "Fish", 5, 1, 
                        "Factice diet 1", "Protomyctophum choriodon", "Protomyctophum", "Myctophidae", "Myctophiformes", "Fish", 5, 1,
                        "Factice diet 1", "Gymnoscopelus piabilis", "Gymnoscopelus", "Myctophidae", "Myctophiformes", "Fish", 25, 1,
                        "Factice diet 1", "Gymnoscopelus nicholsi", "Gymnoscopelus", "Myctophidae", "Myctophiformes", "Fish", 10, 1,
                        "Factice diet 1", NA, "Gymnoscopelus", "Myctophidae", "Myctophiformes", "Fish", 10, 1,
                        "Factice diet 1", "Electrona subaspera", "Electrona", "Myctophidae", "Myctophiformes", "Fish", 10, 1,
                        "Factice diet 1", NA, NA, "Myctophidae", "Myctophiformes", "Fish", 5, 1,
                        "Factice diet 1", "Champsocephalus gunnari", "Champsocephalus", "Channichthyidae", "Perciformes", "Fish", 5, 1,
                        "Factice diet 1", "Icichthys australis", "Icichthys", "Centrolophidae", "Perciformes", "Fish", 10, 0,
                        "Factice diet 1", "Martialia hyadesi", "Martialia", "Ommastrephidae", "Oegopsida", "Cephalopod", 10, 0, 
                        "Factice diet 2 (ceph+)", "Protomyctophum tenisoni", "Protomyctophum", "Myctophidae", "Myctophiformes", "Fish", 0.1, 1,
                        "Factice diet 2 (ceph+)", "Protomyctophum bolini", "Protomyctophum", "Myctophidae", "Myctophiformes", "Fish", 0.1, 1, 
                        "Factice diet 2 (ceph+)", "Protomyctophum choriodon", "Protomyctophum", "Myctophidae", "Myctophiformes", "Fish", 0.1, 1,
                        "Factice diet 2 (ceph+)", "Gymnoscopelus piabilis", "Gymnoscopelus", "Myctophidae", "Myctophiformes", "Fish", 0.1, 1,
                        "Factice diet 2 (ceph+)", "Gymnoscopelus nicholsi", "Gymnoscopelus", "Myctophidae", "Myctophiformes", "Fish", 0.1, 1,
                        "Factice diet 2 (ceph+)", NA, "Gymnoscopelus", "Myctophidae", "Myctophiformes", "Fish", 0.1, 1,
                        "Factice diet 2 (ceph+)", "Electrona subaspera", "Electrona", "Myctophidae", "Myctophiformes", "Fish", 0.1, 1,
                        "Factice diet 2 (ceph+)", NA, NA, "Myctophidae", "Myctophiformes", "Fish", 0.1, 1,
                        "Factice diet 2 (ceph+)", "Champsocephalus gunnari", "Champsocephalus", "Channichthyidae", "Perciformes", "Fish", 0.1, 1,
                        "Factice diet 2 (ceph+)", "Icichthys australis", "Icichthys", "Centrolophidae", "Perciformes", "Fish", 0.1, 0,
                        "Factice diet 2 (ceph+)", "Martialia hyadesi", "Martialia", "Ommastrephidae", "Oegopsida", "Cephalopod", 99, 0) |>
          dplyr::mutate(`%W` = as.character(`%W`), 
                        `%N` = NA, 
                        `%SSFO or %FO` = NA, 
                        n = NA, 
                        Location = NA, 
                        Year_collection = dplyr::case_when(Source == "Factice diet 1" ~ 2023, 
                                                                  Source == "Factice diet 2 (ceph+)" ~ 2024), 
                        Site = NA 
                        ) |>
          dplyr::select(c(Species, Genus, Family, Order, Taxa, 
                        `%W`, `%N`, `%SSFO or %FO`,
                        Source, n, Location, Year_collection, Site, 
                        fish_analysed)
          )) 
  
}

#'
#'
#'
#'
# final data set with only quantitative sources and analysed species 
# with diets set to 100%
diet_data_for_simulations <- function(diet_tab_KerW, 
                                      location_restriction # "Kerguelen" or "all"
                                      ) {
  if (location_restriction == "Kerguelen") {
    # keep only fish analysed and add enlarged categories Gymnoscopelus spp/ etc
    # plus Icichthys australis and Metelectrona ventralis that we don't have 
    # but we'll take sp from same order/Family
    diet_tab_KerW |>
      dplyr::filter(fish_analysed == 1 | 
                      (Species %in% c("Icichthys australis", 
                                      "Metelectrona ventralis"))) |>
      dplyr::rename(W = `%W`) |>
      dplyr::mutate(W = as.numeric(W), 
                    # change sp and genus to NA for sp of Myctophidae that we 
                    # don't have in the dataset (Metelectrona ventralis)
                    # and sp, genus and family to NA for sp of Perciformes that we 
                    # don't have in the dataset
                    Species = dplyr::case_when(Species == "Metelectrona ventralis" ~ NA,
                                               Species == "Icichthys australis" ~ NA,
                                               TRUE ~ Species), 
                    Genus = dplyr::case_when(Genus == "Metelectrona" ~ NA, 
                                             Genus == "Icichthys" ~ NA,
                                             TRUE ~ Genus), 
                    Family = dplyr::case_when(Family == "Centrolophidae" ~ NA,
                                              TRUE ~ Family)) |>
      tidyr::drop_na(W) |>
      dplyr::group_by(Source, n, Location, Year_collection, Site, 
                      Species, Genus, Family, Order, Taxa) |>
      dplyr::summarise(W = sum(W)) |>
      dplyr::group_by(Year_collection) |>
      dplyr::mutate(W = 100*(W/sum(W))) 
  } else if (location_restriction == "all") {
    # keep fish analysed in the Ker dataset +
    # Ommastrephidae from NEA
    ## plus Icichthys australis and Metelectrona ventralis that we don't have 
    # but we'll take sp from same order/Family
    diet_tab_KerW |>
      dplyr::filter(fish_analysed == 1 | 
                      (Species %in% c("Icichthys australis", 
                                      "Metelectrona ventralis")) |
                      # for NEA
                      (Family == "Ommastrephidae" & Taxa == "Cephalopod")) |>
      dplyr::rename(W = `%W`) |>
      dplyr::mutate(W = as.numeric(W), 
                    # change sp and genus to NA for sp of Ommastrephidae that we 
                    # don't have in the dataset
                    # and sp and genus to NA for sp of Myctophidae that we 
                    # don't have in the dataset (Metelectrona ventralis)
                    # and sp, genus and family to NA for sp of Perciformes that we 
                    # don't have in the dataset (Icichthys australis)
                    Species = dplyr::case_when(Species == "Metelectrona ventralis" ~ NA,
                                               Family == "Ommastrephidae" ~ NA,
                                               Species == "Icichthys australis" ~ NA,
                                               TRUE ~ Species), 
                    Genus = dplyr::case_when(Genus == "Metelectrona" ~ NA,
                                             Family == "Ommastrephidae" ~ NA, 
                                             Genus == "Icichthys" ~ NA,
                                             TRUE ~ Genus), 
                    Family = dplyr::case_when(Family == "Centrolophidae" ~ NA,
                                             TRUE ~ Family)) |>
      tidyr::drop_na(W) |>
      dplyr::group_by(Source, n, Location, Year_collection, Site, 
                      Species, Genus, Family, Order, Taxa) |>
      dplyr::summarise(W = sum(W)) |>
      dplyr::group_by(Year_collection) |>
      dplyr::mutate(W = 100*(W/sum(W))) 
  }
  
  
}


#'
#'
#'
#'
# format data with nest per source
format_data <- function(diet_tab_input) {
  diet_tab_input |>
    dplyr::group_by(Location, Site, Source, Year_collection, n) |>
    tidyr::nest(diet = c(Species, Genus, Family, Order, Taxa, W))
  
}




#'
#'
#'
#'
# add Beta, Body Mass, AE and diet energy content as random variables
# and nutrient release defined by a uniform
add_metabolic_data_r_unif <- function(diet_tab_input_nested, 
                               nsim, 
                               # set maximum and minimum release rate for each 
                               # nutrient
                               minFe = 0.7, 
                               maxFe = 0.9, 
                               minZn = 0.7, 
                               maxZn = 0.9, 
                               minCu = 0.7, 
                               maxCu = 0.9, 
                               minMn = 0.7, 
                               maxMn = 0.9, 
                               minSe = 0.7, 
                               maxSe = 0.9, 
                               minCo = 0.7, 
                               maxCo = 0.9) {
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
                    purrr::map(~ tibble::tibble(Fe = runif(n = nsim,  
                                                           min = minFe, 
                                                           max = maxFe),
                                                Zn = runif(n = nsim,  
                                                           min = minZn, 
                                                           max = maxZn),
                                                Cu = runif(n = nsim,  
                                                           min = minCu, 
                                                           max = maxCu),
                                                Mn = runif(n = nsim,  
                                                           min = minMn, 
                                                           max = maxMn),
                                                Se = runif(n = nsim,   
                                                           min = minSe, 
                                                           max = maxSe),
                                                Co = runif(n = nsim,  
                                                           min = minCo, 
                                                           max = maxCo)))) 
  
}


#'
#'
#'
#'
# add Beta, Body Mass, AE and diet energy content as random variables
# with release rate defined by a truncated normal distribution
add_metabolic_data_r_truncnorm <- function(diet_tab_input_nested, 
                               nsim, 
                               # set mean release rate for each 
                               # trace nutrient
                               meanFe = 0.8, 
                               a_Fe = 0.7, 
                               b_Fe = 0.9,
                               meanZn = 0.8, 
                               a_Zn = 0.7, 
                               b_Zn = 0.9,
                               meanCu = 0.8, 
                               a_Cu = 0.7, 
                               b_Cu = 0.9,
                               meanMn = 0.8, 
                               a_Mn = 0.7, 
                               b_Mn = 0.9,
                               meanSe = 0.8,
                               a_Se = 0.7, 
                               b_Se = 0.9, 
                               meanCo = 0.8,
                               a_Co = 0.7, 
                               b_Co = 0.9) {
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
                    purrr::map(~ tibble::tibble(Fe = truncnorm::rtruncnorm(n = nsim,  
                                                                           mean = meanFe, 
                                                                           sd = 0.2, 
                                                                           a = a_Fe, 
                                                                           b = b_Fe),
                                                Zn = truncnorm::rtruncnorm(n = nsim,  
                                                                           mean = meanZn, 
                                                                           sd = 0.2, 
                                                                           a = a_Zn, 
                                                                           b = b_Zn),
                                                Cu = truncnorm::rtruncnorm(n = nsim,  
                                                                           mean = meanCu, 
                                                                           sd = 0.2, 
                                                                           a = a_Cu, 
                                                                           b = b_Cu),
                                                Mn = truncnorm::rtruncnorm(n = nsim,  
                                                                           mean = meanMn, 
                                                                           sd = 0.2,
                                                                           a = a_Mn, 
                                                                           b = b_Mn),
                                                Se = truncnorm::rtruncnorm(n = nsim,  
                                                                           mean = meanSe, 
                                                                           sd = 0.2, 
                                                                           a = a_Se, 
                                                                           b = b_Se),
                                                Co = truncnorm::rtruncnorm(n = nsim,  
                                                                           mean = meanCo, 
                                                                           sd = 0.2, 
                                                                           a = a_Co, 
                                                                           b = b_Co)))) 
  
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
    dplyr::mutate(name = dplyr::case_when(is.na(Order) ~ Taxa,
                                          is.na(Family) ~ Order, 
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
    dplyr::mutate(name = dplyr::case_when(is.na(Order) ~ Taxa,
                                          is.na(Family) ~ Order,  
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