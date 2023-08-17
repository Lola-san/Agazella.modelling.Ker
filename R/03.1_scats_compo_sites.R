################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 03.1_scats_compo_sites.R
#
################################################################################



################# ABSOLUTE CONCENTRATIONS ######################################
#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per site
boxplot_compo_scats_site <- function(scat_compo_tib) {
  
  mean_median_tib <- scat_compo_tib |>
    tidyr::pivot_longer(cols = c("P":"Co"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", "Cu", 
                                               "Mn", "Se","Co"))) |>
    dplyr::group_by( Nutrient) |>
    dplyr::summarise(mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw))
  
  
  scat_compo_tib |>
    tidyr::pivot_longer(cols = c("P":"Co"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap\nNoir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe\nSuzanne"), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", "Cu", 
                                               "Mn", "Se","Co"))) |>
    dplyr::mutate(site = factor(site, 
                                levels = c("Cap\nNoir", 
                                           "Pointe\nSuzanne"))) |>
    ggplot2::ggplot(ggplot2::aes(x = site, y = concentration_mg_kg_dw, 
                                 fill = site)) +
    ggplot2::geom_violin(ggplot2::aes(color = site),
                         width = 1.4, alpha = 0.5) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = median), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = mean), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::stat_summary(fun.y = mean, geom = "errorbar", 
                          ggplot2::aes(ymax = ..y.., ymin = ..y..),
                          width = .75, linetype = "dashed") +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::scale_color_manual(values = c("#353839", "#AE93BEFF")) +
    ggplot2::scale_fill_manual(values = c("#353839", "#AE93BEFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", 
                        nrow = 2) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/sites/boxplot_scat_conc_sites.jpg",
                  scale = 1,
                  height = 6, width = 10
  )
  
}



#'
#'
#'
#'
#'
# function to produce table with summary of elemental analysis
table_compo_scats_site <- function(scat_compo_tib
) {
  
  table_summary <- scat_compo_tib |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
    dplyr::group_by(Site, Nutrient) |>
    dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                     mean = round(mean(concentration_mg_kg_dw), 3),
                     median = round(median(concentration_mg_kg_dw), 3),
                     sd = round(sd(concentration_mg_kg_dw), 3),
                     cv = round(sd/mean, 3),
                     min = round(min(concentration_mg_kg_dw), 3),
                     max = round(max(concentration_mg_kg_dw), 3)) |>
    tidyr::pivot_longer(cols = c(mean, median, sd, cv, min, max), 
                        names_to = "variable", 
                        values_to = "conc_mg_kg_dw") |>
    tidyr::pivot_wider(names_from = Nutrient,  
                       values_from = conc_mg_kg_dw) |>
    dplyr::select(c(Site, n, variable, 
                    P, Fe, Zn, Cu, Mn, Se, Co)) |>
    dplyr::bind_rows(scat_compo_tib |>
                       tidyr::pivot_longer(cols = c(P:Co), 
                                           names_to = "Nutrient", 
                                           values_to = "concentration_mg_kg_dw") |>
                       dplyr::mutate(Site = "All scats together") |>
                       dplyr::group_by(Site, Nutrient) |>
                       dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                                        mean = round(mean(concentration_mg_kg_dw), 3),
                                        median = round(median(concentration_mg_kg_dw), 3),
                                        sd = round(sd(concentration_mg_kg_dw), 3),
                                        cv = round(sd/mean, 3),
                                        min = round(min(concentration_mg_kg_dw), 3),
                                        max = round(max(concentration_mg_kg_dw), 3)) |>
                       tidyr::pivot_longer(cols = c(mean, median, sd, cv, min, max), 
                                           names_to = "variable", 
                                           values_to = "conc_mg_kg_dw") |>
                       tidyr::pivot_wider(names_from = Nutrient,  
                                          values_from = conc_mg_kg_dw) |>
                       dplyr::select(c(Site, n, variable, 
                                       P, Fe, Zn, Cu, Mn, Se, Co)))
  
  openxlsx::write.xlsx(table_summary, 
                       file = paste0("output/sites/table_summary_sites_scat_compo_data.xlsx"))
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentrations in scats between sites 
MWtest_scats_compo_sites <- function(scat_compo_tib) {
  
  scat_compo_tib <- scat_compo_tib |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne"),
                  Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", "Cu", 
                                               "Mn", "Se", "Co"))) 
  
  nut_vec <- unique(scat_compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- scat_compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = site, 
                         values_from = conc_mg_kg_dw)
    
    CapNo <- na.omit(table$`Cap Noir`)
    PSuz <- na.omit(table$`Pointe Suzanne`)
    
    nut_test <- data.frame(Nutrient = nut,  
                           alpha_MW = wilcox.test(CapNo, PSuz)[[3]])
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,]
  
  df_test <- df_test |>
    dplyr::mutate(significant = dplyr::case_when(alpha_MW <= 0.05 ~ "yes", 
                                                 TRUE ~ "no"))
  
  openxlsx::write.xlsx(df_test, 
                       file = "output/sites/Mann_Whitney_test_scats_sites.xlsx")
  
}


############################ RELATIVE COMPOSITION ##############################


#'
#'
#'
#'
#'
# 
fig_nut_scat_compo_relative <- function(scat_compo_tib) {
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")))  |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarize(mean = mean(conc_relative),
                     `10_quant` = quantile(conc_relative, probs = c(0.1)),
                     `80_quant` = quantile(conc_relative, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::ylab("Relative concentration in one scat") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/scat_compo_rel_all_scats.jpg",
                  scale = 1,
                  height = 6, width = 9
  )
  
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")))  |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarize(mean = mean(conc_relative),
                     `10_quant` = quantile(conc_relative, probs = c(0.1)),
                     `80_quant` = quantile(conc_relative, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::ylab("Relative concentration in one scat") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  
}

#'
#'
#'
#'
#'
# 
fig_nut_scat_compo_relative_comp_pinn <- function(scat_compo_tib) {
  
  tibble::tribble(
    ~ Source, ~ Species, ~ Fe, ~ Fe_sd, ~ Zn, ~ Zn_sd, ~ Cu, ~ Cu_sd, ~ Mn, ~ Mn_sd, ~ Co, ~ Co_sd, 
    "Wing et al. 2017", "New zealand fur seals", 278.8, 57.7, 494.8, 74.0, 696.0, 228.9, 18.7, 4.4, 1.92, 0.55, 
    "Wing et al. 2017", "Hooker s sea lion", 334.0, 38.3, 318.8, 22.5, 38.1, 9.4, 24.0, 1.9, 0.24, 0.06, 
    "Wing et al. 2014", "Hooker s sea lion", 1014, 572, 228.3, 68.9, 6.0, 0.6, 29.6, 3.5, 0.39, 0.39, 
    "Wing et al. 2021", "Weddell seal", 950.8, 148.9, 270.7, 15.7, 17.2, 2.8, 28.7, 2.3, 0.73, 0.06) |>
    dplyr::mutate(sum_mean = Fe + Zn + Cu + Mn + Co) |>
    tidyr::pivot_longer(cols = c(Fe, Zn, Cu, Mn, Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_mean, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |>
    dplyr::select(c(Source, Species, Nutrient, conc_relative)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = conc_relative) |>
    dplyr::bind_rows(scat_compo_tib |>
                       tidyr::pivot_longer(cols = c(Fe:Co), 
                                           names_to = "Nutrient", 
                                           values_to = "conc_mg_kg_dw") |> 
                       dplyr::mutate(Nutrient = factor(Nutrient, 
                                                       levels = c("Fe", "Zn", 
                                                                  "Cu", "Mn", "Se",
                                                                  "Co"))) |> 
                       dplyr::group_by(Nutrient) |>
                       dplyr::summarize(mean = mean(conc_mg_kg_dw)) |>
                       tidyr::pivot_wider(names_from = Nutrient, 
                                          values_from = mean) |>
                       dplyr::mutate(sum_mean = Fe + Zn + Cu + Mn + Co, 
                                     Fe = Fe/sum_mean, 
                                     Zn = Zn/sum_mean, 
                                     Cu = Cu/sum_mean, 
                                     Mn = Mn/sum_mean,
                                     Co = Co/sum_mean,  
                                     Source = "This study", 
                                     Species = "Antarctic fur seals") |> 
                       dplyr::select(c(Source, Species, Fe, Zn, Cu, Mn, Co))) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "ratio_mean") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = ratio_mean, 
                                   fill = Source), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Species) +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "bottom")
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_pinni.jpg",
                  scale = 1,
                  height = 6, width = 9
  )
  
  tibble::tribble(
    ~ Source, ~ Species, ~ Fe, ~ Fe_sd, ~ Zn, ~ Zn_sd, ~ Cu, ~ Cu_sd, ~ Mn, ~ Mn_sd, ~ Co, ~ Co_sd, 
    "Wing et al. 2017", "New zealand fur seals", 278.8, 57.7, 494.8, 74.0, 696.0, 228.9, 18.7, 4.4, 1.92, 0.55, 
    "Wing et al. 2017", "Hooker’s sea lion", 334.0, 38.3, 318.8, 22.5, 38.1, 9.4, 24.0, 1.9, 0.24, 0.06, 
    "Wing et al. 2014", "Hooker’s sea lion", 1014, 572, 228.3, 68.9, 6.0, 0.6, 29.6, 3.5, 0.39, 0.39, 
    "Wing et al. 2021", "Weddell seal", 950.8, 148.9, 270.7, 15.7, 17.2, 2.8, 28.7, 2.3, 0.73, 0.06) |>
    dplyr::mutate(sum_mean = Fe + Zn + Cu + Mn + Co) |>
    tidyr::pivot_longer(cols = c(Fe, Zn, Cu, Mn, Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_mean, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |>
    dplyr::select(c(Source, Species, Nutrient, conc_relative)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = conc_relative) |>
    dplyr::bind_rows(scat_compo_tib |>
                       tidyr::pivot_longer(cols = c(Fe:Co), 
                                           names_to = "Nutrient", 
                                           values_to = "conc_mg_kg_dw") |> 
                       dplyr::mutate(Nutrient = factor(Nutrient, 
                                                       levels = c("Fe", "Zn", 
                                                                  "Cu", "Mn", "Se",
                                                                  "Co"))) |> 
                       dplyr::group_by(Nutrient) |>
                       dplyr::summarize(mean = mean(conc_mg_kg_dw)) |>
                       tidyr::pivot_wider(names_from = Nutrient, 
                                          values_from = mean) |>
                       dplyr::mutate(sum_mean = Fe + Zn + Cu + Mn + Co, 
                                     Fe = Fe/sum_mean, 
                                     Zn = Zn/sum_mean, 
                                     Cu = Cu/sum_mean, 
                                     Mn = Mn/sum_mean,
                                     Co = Co/sum_mean,  
                                     Source = "This study", 
                                     Species = "Antarctic fur seals") |> 
                       dplyr::select(c(Source, Species, Fe, Zn, Cu, Mn, Co))) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "ratio_mean") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = ratio_mean, 
                                   fill = Source), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Species) +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "bottom")
  
  
}



#'
#'
#'
#'
#'
# 
fig_nut_scat_compo_relative_sites <- function(scat_compo_tib) {
  
  # comparison between sites
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::group_by(site, Nutrient) |>
    dplyr::summarise(mean_conc_relative = mean(conc_relative)) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean_conc_relative), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ site) + 
    ggplot2::ylab("Relative proportion\nin scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_sites_Agazella.jpg",
                  scale = 1,
                  height = 3, width = 5
  )
  
  # Cap Noir
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Cap Noir") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative), 
                      fill = "#353839", 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Code_sample) + 
    ggplot2::ggtitle("Cap Noir") +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_CapNoir_Agazella_dodge.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Cap Noir") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Code_sample, y = conc_relative, 
                                   fill = Nutrient), 
                      stat = "identity", 
                      position = ggplot2::position_stack()) +
    ggplot2::scale_fill_manual(values = c("#B4DAE5FF", "#278B9AFF",
                                          "#DE7862FF", "#D8AF39FF",  
                                          "#403369FF", "#5A6F80FF", 
                                          "#E8C4A2FF")) +
    ggplot2::ggtitle("Cap Noir") +
    ggplot2::ylab("Relative fraction") +
    ggplot2::xlab("Scat sample") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
                   axis.text.x = ggplot2::element_blank(), 
                   title = ggplot2::element_text(size = 15, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 14, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 14, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12))
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_CapNoir_Agazella_stack.jpg",
                  scale = 1,
                  height = 4, width = 6
  )
  
  # Pointe Suzanne
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Pointe Suz") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative), 
                      fill = "#AE93BEFF",
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Code_sample) + 
    ggplot2::ggtitle("Pointe Suzanne") +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14),
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_PSuz_Agazella_dodge.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Pointe Suz") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Code_sample, y = conc_relative, 
                                   fill = Nutrient), 
                      stat = "identity", 
                      position = ggplot2::position_stack()) +
    ggplot2::scale_fill_manual(values = c("#B4DAE5FF", "#278B9AFF",
                                          "#DE7862FF", "#D8AF39FF",  
                                          "#403369FF", "#5A6F80FF", 
                                          "#E8C4A2FF")) +
    ggplot2::ggtitle("Pointe Suzanne") +
    ggplot2::ylab("Relative fraction") +
    ggplot2::xlab("Scat sample") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
                   axis.text.x = ggplot2::element_blank(), 
                   title = ggplot2::element_text(size = 15, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 14, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 14, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12))
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_PSuz_Agazella_stack.jpg",
                  scale = 1,
                  height = 4, width = 6
  )
  
  
}


#'
#'
#'
#'
#'
# 
fig_nut_scat_compo_relative_sites_FeZnCu_MnSeCo <- function(scat_compo_tib) {
  
  
  # Cap Noir
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  class_nutrient = factor(dplyr::case_when(Nutrient %in% c("P", "Fe", "Zn", "Cu") ~ "major", 
                                                    Nutrient %in% c("Mn", "Se", "Co") ~ "minor"), 
                                          levels = c("major", "minor")),
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Cap Noir", class_nutrient == "major") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative), 
                      fill = "#353839", 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Code_sample) + 
    ggplot2::ggtitle("Cap Noir") +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_CapNoir_Agazella_FeZnCu.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  class_nutrient = factor(dplyr::case_when(Nutrient %in% c("P", "Fe", "Zn", "Cu") ~ "major", 
                                                           Nutrient %in% c("Mn", "Se", "Co") ~ "minor"), 
                                          levels = c("major", "minor")),
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Cap Noir", class_nutrient == "minor") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative), 
                      fill = "#353839", 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Code_sample) + 
    ggplot2::ggtitle("Cap Noir") +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_CapNoir_Agazella_MnSeCo.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  class_nutrient = factor(dplyr::case_when(Nutrient %in% c("P", "Fe", "Zn", "Cu") ~ "major", 
                                                           Nutrient %in% c("Mn", "Se", "Co") ~ "minor"), 
                                          levels = c("major", "minor")),
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Cap Noir") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative), 
                      fill = "#353839", 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_grid(Code_sample ~ class_nutrient, scales = "free_y") + 
    ggplot2::ggtitle("Cap Noir") +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_CapNoir_Agazella_grid.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  
  
  
  
  # Pointe Suzanne
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  class_nutrient = factor(dplyr::case_when(Nutrient %in% c("P", "Fe", "Zn", "Cu") ~ "major", 
                                                           Nutrient %in% c("Mn", "Se", "Co") ~ "minor"), 
                                          levels = c("major", "minor")),
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Pointe Suz", class_nutrient == "major") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative), 
                      fill = "#AE93BEFF",
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Code_sample) + 
    ggplot2::ggtitle("Pointe Suzanne") +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14),
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_PSuz_Agazella_FeZnCu.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  class_nutrient = factor(dplyr::case_when(Nutrient %in% c("P", "Fe", "Zn", "Cu") ~ "major", 
                                                           Nutrient %in% c("Mn", "Se", "Co") ~ "minor"), 
                                          levels = c("major", "minor")),
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Pointe Suz", class_nutrient == "minor") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative), 
                      fill = "#AE93BEFF",  
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Code_sample) + 
    ggplot2::ggtitle("Pointe Suzanne") +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14),
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/scat_compo_rel_comp_PSuz_Agazella_MnSeCo.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  
}



#'
#'
#'
#'
#'
# 
fig_nut_scat_compo_relative_clust <- function(scat_compo_tib_clust_chap3) {
  
  
  scat_compo_tib_clust_chap3 |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Ni + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::group_by(cluster, Nutrient) |>
    dplyr::summarise(mean_conc_relative = mean(conc_relative),
                     `10_quant` = quantile(conc_relative, probs = c(0.1)),
                     `80_quant` = quantile(conc_relative, probs = c(0.8))) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", "Cu", "Mn", "Se",
                                               "Ni", "Co"))) |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean_conc_relative, 
                                   fill = cluster), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::facet_wrap(~ cluster) + 
    ggplot2::ylab("Relative proportion\nin scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/scat_compo_rel_comp_clust_Agazella.jpg",
                  scale = 1,
                  height = 3, width = 5
  )

  
}





