################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 03.4_output_nut_release_scenarios.R
#
################################################################################

#'
#'
#'
#'
#
nut_per_site_tot_period_scenarios <- function(output_nut_release_scenarios_tib,
                                              clust_test, # 1, 2, 3, 4
                                              site # "Cap Noir" or "Pointe Suzanne"
) {
  
  ############### COMPILE OUTPUT WITH ALL SCENARIOS ##########################
  full_tib <- rbind(
    # with all scats taken on site
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_all_scats_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_all_scats_sites) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = "All scat samples from colony") |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with only scat samples from one enriched cluster - 100% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("100% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("90% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("80% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("70% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("60% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("50% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("40% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("30% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("20% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("10% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg)),
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(P:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("P", "Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("0% of scat samples from cluster ", clust_test)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_kg),
                       mean = mean(tot_pop_release_period_kg), 
                       `10_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.1)),
                       `80_quant` = quantile(tot_pop_release_period_kg,
                                             probs = c(0.8)),
                       max = max(tot_pop_release_period_kg))
  ) |>
    dplyr::mutate(scat_compo = factor(scat_compo,
                                      levels = c("All scat samples from colony",
                                                 paste0("100% of scat samples from cluster ", clust_test),
                                                 paste0("90% of scat samples from cluster ", clust_test),
                                                 paste0("80% of scat samples from cluster ", clust_test),
                                                 paste0("70% of scat samples from cluster ", clust_test),
                                                 paste0("60% of scat samples from cluster ", clust_test),
                                                 paste0("50% of scat samples from cluster ", clust_test),
                                                 paste0("40% of scat samples from cluster ", clust_test),
                                                 paste0("30% of scat samples from cluster ", clust_test),
                                                 paste0("20% of scat samples from cluster ", clust_test),
                                                 paste0("10% of scat samples from cluster ", clust_test),
                                                 paste0("0% of scat samples from cluster ", clust_test)
                                      )))
  
  #################### BARPLOT ######################################
  plot_title <- unique(output_nut_release_scenarios_tib$Site)
  
  full_tib |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = scat_compo,
                                   y = mean,
                                   fill = scat_compo),
                      stat = "identity",
                      position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = scat_compo,
                                         ymin = `10_quant`,
                                         ymax = `80_quant`),
                            color = "black",
                            position = ggplot2::position_dodge2(width = 1)) +
    ggplot2::scale_fill_manual(values = c("#D8AF39FF", "#58A449FF",
                                          "#AE93BEFF", "#B4DAE5FF",
                                          "#E75B64FF", "#1D2645FF",
                                          "#3E6248FF", "#278B9AFF",
                                          "#4C413FFF", "#F0D77BFF",
                                          "#44A57CFF", "#E8C4A2FF"
    )) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::ylab("Total nutrient released\nin feces during breeding\nand moulting period (in kg)") +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(size = 16,
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   title = ggplot2::element_text(size = 17,
                                                 face = "bold"),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15))
  ggplot2::ggsave(paste0("output/sites/barplot_nut_release_tot_pop_sites_scenarios_",
                         site, "_clust", clust_test, ".jpg"),
                  scale = 1,
                  height = 6, width = 10
  )
  
  #################### SCATTERPLOT ######################################
  
  full_tib |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = scat_compo,
                                     y = mean,
                                     color = scat_compo),
                        stat = "identity",
                        position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = scat_compo,
                                         ymin = `10_quant`,
                                         ymax = `80_quant`,
                                         color = scat_compo),
                            position = ggplot2::position_dodge2(width = 1)) +
    ggplot2::scale_color_manual(values = c("#D8AF39FF", "#58A449FF",
                                           "#AE93BEFF", "#B4DAE5FF",
                                           "#E75B64FF", "#1D2645FF",
                                           "#3E6248FF", "#278B9AFF",
                                           "#4C413FFF", "#F0D77BFF",
                                           "#44A57CFF", "#E8C4A2FF"
    )) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::ylab("Total nutrient released\nin feces during breeding\nand moulting period (in kg)") +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(size = 16,
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   title = ggplot2::element_text(size = 17,
                                                 face = "bold"),
                   legend.position = "right",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15))
  ggplot2::ggsave(paste0("output/sites/scatterplot_nut_release_tot_pop_sites_scenarios__",
                         site, "_clust", clust_test, ".jpg"),
                  scale = 1,
                  height = 5, width = 12
  )
  
  
}



#'
#'
#'
#'
#
nut_per_site_tot_period_all_scenarios <- function(list_output_scenarios,
                                                  list_percent_clust_scenarios,
                                                  site # "Cap Noir" or "Pointe Suzanne"
) {
  
  nb_test <- length(list_output_scenarios)
  
  df_compiled_tests <- data.frame(Site = NA, 
                                  scenario = NA,
                                  sub_scenario = NA,
                                  Nutrient = NA, 
                                  min = NA, 
                                  mean = NA) |>
    dplyr::mutate(`10_quant` = NA,
                  `80_quant` = NA,
                  max = NA)
  
  for (i in c(1:nb_test)) {
    # first extract real % of cluster i in dataset used for computation
    vec_percent_i <- list_percent_clust_scenarios[[i]] |> 
      dplyr::filter(scenario == i) |> 
      dplyr::ungroup() |> 
      dplyr::pull(c(as.character(i)))
    
    
    tib_all_scenar_test_i <- rbind(
      # with only scat samples from one enriched cluster - 100% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario100) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[1]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg)),
      # with 90% scat samples from one enriched cluster - 90% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario90) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[2]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg)),
      # with 80% scat samples from one enriched cluster - 80% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario80) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[3]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg)),
      # with 70% scat samples from one enriched cluster - 70% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario70) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[4]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg)),
      # with 60% scat samples from one enriched cluster - 60% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario60) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[5]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg)),
      # with 50% scat samples from one enriched cluster - 50% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario50) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[6]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg)),
      # with 40% scat samples from one enriched cluster - 40% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario40) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[7]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg)),
      # with 30% scat samples from one enriched cluster - 30% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario30) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[8]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg)),
      # with 20% scat samples from one enriched cluster - 20% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario20) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[9]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg)),
      # with 10% scat samples from one enriched cluster - 10% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario10) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[10]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg)),
      # with 00% scat samples from one enriched cluster - 00% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario00) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
        tidyr::pivot_longer(cols = c(P:Co),
                            names_to = "Nutrient",
                            values_to = "tot_pop_release_period_kg") |>
        dplyr::mutate(Nutrient = factor(Nutrient,
                                        levels = c("P", "Fe", "Zn",
                                                   "Cu", "Mn", "Se",
                                                   "Co")),
                      sub_scenario = vec_percent_i[11]) |>
        dplyr::group_by(Site, sub_scenario, Nutrient) |>
        dplyr::summarise(min = min(tot_pop_release_period_kg),
                         mean = mean(tot_pop_release_period_kg), 
                         `10_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.1)),
                         `80_quant` = quantile(tot_pop_release_period_kg,
                                               probs = c(0.8)),
                         max = max(tot_pop_release_period_kg))
    ) |>
      dplyr::mutate(scenario = paste0("changing % of scats from cluster ", i)) |>
      dplyr::select(c(Site, scenario, sub_scenario, Nutrient, 
                      min, mean, `10_quant`, `80_quant`, max))
    
    # compile with the rest 
    df_compiled_tests <- rbind(df_compiled_tests, tib_all_scenar_test_i)
    
  }
  
  # delete first line of NA 
  df_compiled_tests <- df_compiled_tests[-1, ]
  
  # table with stats for the baseline values
  mean_conc_table_with_all_scats <- list_output_scenarios[[2]] |>
    dplyr::select(Site, 
                  release_nut_pop_tot_period_all_scats_sites) |>
    dplyr::filter(Site == site) |>
    tidyr::unnest(release_nut_pop_tot_period_all_scats_sites) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(mean_release_est = mean(tot_pop_release_period_kg), 
                     median_release_est = median(tot_pop_release_period_kg)) |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) 
  
  df_compiled_tests |>
    dplyr::mutate(sub_scenario = as.numeric(sub_scenario), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  y_lim = dplyr::case_when(Nutrient == "P" ~ 1100,
                                           Nutrient == "Fe" ~ 65, 
                                           Nutrient == "Zn" ~ 4.2, 
                                           Nutrient == "Cu" ~ 3.2, 
                                           Nutrient == "Mn" ~ 1.7, 
                                           Nutrient == "Se" ~ 0.7, 
                                           Nutrient == "Co" ~ 0.06)) |>
    ggplot2::ggplot(ggplot2::aes(x = sub_scenario,
                                 y = mean,
                                 color = scenario)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
    ggplot2::geom_linerange(ggplot2::aes(x = sub_scenario,
                                         ymin = `10_quant`,
                                         ymax = `80_quant`,
                                         color = scenario)) +
    ggplot2::scale_color_manual(values = c("#44A57CFF",
                                           "#1D2645FF",
                                           "#D8AF39FF", 
                                           "#AE93BEFF"
    )) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::geom_hline(data = mean_conc_table_with_all_scats, 
                        ggplot2::aes(yintercept = mean_release_est),
                        linetype = "dashed", 
                        linewidth = 1.5,
                        color = "darkred") +
    ggplot2::ylab("Total nutrient released\nin scats during breeding\nand moulting period (in kg)") +
    ggplot2::xlab("% of varying-scat cluster tested in simulation dataset") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::ggtitle(site) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   axis.title.x = ggplot2::element_text(size = 16,
                                                        face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 16,
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   title = ggplot2::element_text(size = 17,
                                                 face = "bold"),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 15))
  ggplot2::ggsave(paste0("output/sites/scatterplot_nut_release_tot_pop_all_scenarios_", 
                         site, ".jpg"),
                  scale = 1,
                  height = 7, width = 10
  )
  
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different habitats 
test_diff_test_nut_sites_tot_period_1_scenarios <- function(output_scenarios,
                                                         clust_test # cluster tested with scenarios
) {
  
  output_scenarios_light <- output_scenarios |> 
    dplyr::select(c(Site, 
                    release_nut_pop_tot_period_all_scats_sites, 
                    release_nut_pop_tot_period_scenario00,
                    release_nut_pop_tot_period_scenario10, 
                    release_nut_pop_tot_period_scenario20, 
                    release_nut_pop_tot_period_scenario30, 
                    release_nut_pop_tot_period_scenario40, 
                    release_nut_pop_tot_period_scenario50, 
                    release_nut_pop_tot_period_scenario60, 
                    release_nut_pop_tot_period_scenario70, 
                    release_nut_pop_tot_period_scenario80, 
                    release_nut_pop_tot_period_scenario90, 
                    release_nut_pop_tot_period_scenario100))
  

    # with 00% scat samples from one enriched cluster - 00% scenario
    table_test_clust_test <- 
      # all samples from site
      output_scenarios_light |>
      dplyr::select(Site, 
                    release_nut_pop_tot_period_all_scats_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_all_scats_sites) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(subscenario = "all scat samples from colony") |>
      dplyr::bind_rows(
        # with 00% scat samples from one enriched cluster - 00% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario00) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "0%"),
        # with 10% scat samples from one enriched cluster - 10% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario10) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "10%"),
        # with 20% scat samples from one enriched cluster - 20% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario20) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "20%"),
        # with 30% scat samples from one enriched cluster - 30% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario30) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "30%"),
        # with 40% scat samples from one enriched cluster - 40% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario40) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "40%"),
        # with 50% scat samples from one enriched cluster - 50% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario50) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "50%"),
        # with 60% scat samples from one enriched cluster - 60% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario60) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "60%"),
        # with 70% scat samples from one enriched cluster - 70% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario70) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "70%"),
        # with 80% scat samples from one enriched cluster - 80% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario80) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "80%"),
        # with 90% scat samples from one enriched cluster - 90% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario90) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "90%"),
        # with 100% scat samples from one enriched cluster - 100% scenario
        output_scenarios_light |>
          dplyr::select(Site,
                        release_nut_pop_tot_period_scenario100) |>
          tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(subscenario = "100%"),
        ) |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      tidyr::pivot_wider(names_from = subscenario,
                         values_from = tot_pop_release_period_kg,
                         values_fn = list) |>
      tidyr::unnest(cols = c(`all scat samples from colony`, `0%`, `10%`, `20%`,
                             `30%`, `40%`, `50%`, `60%`, `70%`, `80%`,
                             `90%`, `100%`)) |>
      dplyr::mutate(t_obs_0 = dplyr::case_when(`all scat samples from colony` > `0%` ~ 1, TRUE ~ 0), 
                    t_obs_10 = dplyr::case_when(`all scat samples from colony` > `10%` ~ 1, TRUE ~ 0),
                    t_obs_20 = dplyr::case_when(`all scat samples from colony` > `20%` ~ 1, TRUE ~ 0),
                    t_obs_30 = dplyr::case_when(`all scat samples from colony` > `30%` ~ 1, TRUE ~ 0),
                    t_obs_40 = dplyr::case_when(`all scat samples from colony` > `40%` ~ 1, TRUE ~ 0),
                    t_obs_50 = dplyr::case_when(`all scat samples from colony` > `50%` ~ 1, TRUE ~ 0),
                    t_obs_60 = dplyr::case_when(`all scat samples from colony` > `60%` ~ 1, TRUE ~ 0),
                    t_obs_70 = dplyr::case_when(`all scat samples from colony` > `70%` ~ 1, TRUE ~ 0),
                    t_obs_80 = dplyr::case_when(`all scat samples from colony` > `80%` ~ 1, TRUE ~ 0),
                    t_obs_90 = dplyr::case_when(`all scat samples from colony` > `90%` ~ 1, TRUE ~ 0),
                    t_obs_100 = dplyr::case_when(`all scat samples from colony` > `100%` ~ 1, TRUE ~ 0)) |>
      dplyr::group_by(Site, Nutrient) |>
      dplyr::summarise(test_obs_0 = mean(t_obs_0), 
                       test_obs_10 = mean(t_obs_10), 
                       test_obs_20 = mean(t_obs_20), 
                       test_obs_30 = mean(t_obs_30), 
                       test_obs_40 = mean(t_obs_40), 
                       test_obs_50 = mean(t_obs_50), 
                       test_obs_60 = mean(t_obs_60), 
                       test_obs_70 = mean(t_obs_70), 
                       test_obs_80 = mean(t_obs_80), 
                       test_obs_90 = mean(t_obs_90), 
                       test_obs_100 = mean(t_obs_100)) |>
      tidyr::pivot_longer(cols = c(test_obs_0:test_obs_100), 
                          names_to = "subscenario", 
                          values_to = "test_obs_scenario") |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = test_obs_scenario) |>
      dplyr::mutate(scenario = clust_test) |>
      dplyr::select(c(Site, scenario, subscenario, P:Co))
      

}

#'
#'
#'
#'
#'
# function to calculate percentage of difference of all scenarios compared to 
# estimate made with the full observed dataset
compile_test_diff_test_nut_sites_tot_period_scenarios <- function(
    list_test_diff_test_nut_sites_tot_period_scenarios,
    site # "Cap Noir" or "Pointe Suzanne"
) {
  
  # in the case where the number of cluster per site changed
  # this function should be set to detect the number of cluster and adapt the 
  # binding of tables
  # for now, we'll just set it with 3 clusters
  
  table_full <- rbind(list_test_diff_test_nut_sites_tot_period_scenarios[[1]], 
                      list_test_diff_test_nut_sites_tot_period_scenarios[[2]],
                      list_test_diff_test_nut_sites_tot_period_scenarios[[3]])
  
  openxlsx::write.xlsx(table_full,
                       file = paste0("output/sites/percent_differences_with_all_scat_est_all_scenarios_",
                                     site, ".xlsx"))
  
}





#'
#'
#'
#'
#'
# function to calculate percentage of difference of all scenarios compared to 
# estimate made with the full observed dataset
percent_diff_nut_sites_tot_period_1_scenarios <- function(output_scenarios,
                                                          clust_test, # cluster tested with scenarios
                                                          site # "Cap Noir" or "Pointe Suzanne"
) {
  
  output_scenarios_light <- output_scenarios |> 
    dplyr::select(c(Site, 
                    release_nut_pop_tot_period_all_scats_sites, 
                    release_nut_pop_tot_period_scenario00,
                    release_nut_pop_tot_period_scenario10, 
                    release_nut_pop_tot_period_scenario20, 
                    release_nut_pop_tot_period_scenario30, 
                    release_nut_pop_tot_period_scenario40, 
                    release_nut_pop_tot_period_scenario50, 
                    release_nut_pop_tot_period_scenario60, 
                    release_nut_pop_tot_period_scenario70, 
                    release_nut_pop_tot_period_scenario80, 
                    release_nut_pop_tot_period_scenario90, 
                    release_nut_pop_tot_period_scenario100))
  
  
  
  # tibble with mean of the estimate made with all the (observed) scat samples
  # and their respective ratios
  df_mean <- output_scenarios_light |> # could be any number should always 
    # be the same (with variability due to bootstrapp)
    dplyr::select(Site, 
                  release_nut_pop_tot_period_all_scats_sites) |>
    dplyr::filter(Site == site) |>
    tidyr::unnest(release_nut_pop_tot_period_all_scats_sites) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |>
    dplyr::group_by(Site, Nutrient) |>
    dplyr::summarise(mean_all_scats_kg = mean(tot_pop_release_period_kg))
  
  
  df_compiled_mean_subscenarios <- data.frame(Site = NA, 
                                              scenario = NA,
                                              subscenario = NA,
                                              Nutrient = NA, 
                                              mean_subscenario_kg = NA)
  
  
  # with 00% scat samples from one enriched cluster - 00% scenario
  table_test_clust_test_0 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario00) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "0%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  # with 10% scat samples from one enriched cluster - 10% scenario
  table_test_clust_test_10 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario10) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "10%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  
  # with 20% scat samples from one enriched cluster - 20% scenario
  table_test_clust_test_20 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario20) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "20%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  
  # with 30% scat samples from one enriched cluster - 30% scenario
  table_test_clust_test_30 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario30) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "30%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  
  # with 40% scat samples from one enriched cluster - 40% scenario
  table_test_clust_test_40 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario40) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |>
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "40%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  
  # with 50% scat samples from one enriched cluster - 50% scenario
  table_test_clust_test_50 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario50) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "50%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  
  # with 60% scat samples from one enriched cluster - 60% scenario
  table_test_clust_test_60 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario60) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "60%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  
  # with 70% scat samples from one enriched cluster - 70% scenario
  table_test_clust_test_70 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario70) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "70%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  
  # with 80% scat samples from one enriched cluster - 80% scenario
  table_test_clust_test_80 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario80) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "80%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  
  # with 90% scat samples from one enriched cluster - 90% scenario
  table_test_clust_test_90 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario90) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "90%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  # with 100% scat samples from one enriched cluster - 100% scenario
  table_test_clust_test_100 <- output_scenarios_light |>
    dplyr::select(Site,
                  release_nut_pop_tot_period_scenario100) |>
    tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(scenario = clust_test, 
                  subscenario = "100%") |>
    dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
    dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
  
  table_mean_nut_test_clust_test <- rbind(
    table_test_clust_test_0,
    table_test_clust_test_10,
    table_test_clust_test_20,
    table_test_clust_test_30,
    table_test_clust_test_40,
    table_test_clust_test_50,
    table_test_clust_test_60,
    table_test_clust_test_70,
    table_test_clust_test_80,
    table_test_clust_test_90,
    table_test_clust_test_100
  ) 

  # join with mean for all scats 
  table_mean_nut_test_clust_test |>
    dplyr::left_join(df_mean, by = c("Site", "Nutrient")) |>
    dplyr::mutate(Nutrient = factor(Nutrient,
                                    levels = c("P", "Fe", "Zn",
                                               "Cu", "Mn", "Se",
                                               "Co")),
                  diff_mean_with_all_scats = mean_subscenario_kg - mean_all_scats_kg,
                  percent_of_mean_with_all_scats = round(100*(diff_mean_with_all_scats/mean_all_scats_kg), 0)) |>
    dplyr::select(Site, scenario, subscenario, Nutrient, percent_of_mean_with_all_scats) |>
    dplyr::group_by(scenario, Nutrient) |>
    dplyr::summarise(mean = round(mean(abs(percent_of_mean_with_all_scats)), 0), 
                     min = min(percent_of_mean_with_all_scats),
                     max = max(percent_of_mean_with_all_scats)) |>
    tidyr::pivot_longer(cols = c(mean, min, max), 
                        names_to = "Summarising variables",
                        values_to = "Percent value") |>
    dplyr::mutate(`Summarising variables` = dplyr::case_when(
      `Summarising variables` == "mean" ~ "Mean of absolute % of difference", 
      TRUE ~ `Summarising variables`)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = `Percent value`)
  

}


#'
#'
#'
#'
#'
# function to calculate percentage of difference of all scenarios compared to 
# estimate made with the full observed dataset
compile_percent_diff_nut_sites_tot_period_scenarios <- function(
    list_percent_diff_nut_sites_tot_period_scenarios,
    site # "Cap Noir" or "Pointe Suzanne"
) {
  
  # in the case where the number of cluster per site changed
  # this function should be set to detect the number of cluster and adapt the 
  # binding of tables
  # for now, we'll just set it with 3 clusters
  
  table_full <- rbind(list_percent_diff_nut_sites_tot_period_scenarios[[1]], 
                      list_percent_diff_nut_sites_tot_period_scenarios[[2]],
                      list_percent_diff_nut_sites_tot_period_scenarios[[3]])
 
  openxlsx::write.xlsx(table_full,
                       file = paste0("output/sites/percent_differences_with_all_scat_est_all_scenarios_",
                                     site, ".xlsx"))
  
}



# function with objects too heavy to run with 10,000 simulations, was decomposed
#'
#'
#'
#'
#'
# function to calculate percentage of difference of all scenarios compared to 
# estimate made with the full observed dataset
percent_diff_nut_sites_tot_period_all_scenarios <- function(list_output_scenarios,
                                                            site # "Cap Noir" or "Pointe Suzanne"
) {
  
  nb_test <- length(list_output_scenarios)
  
  list_light <- list()
  
  for (i in c(1:nb_test)) {
    # delete daa not needed to enlighten the objects 
    # otherwise causing memory crash...
    list_output_scenarios_i_light <- list_output_scenarios[[i]] |> 
      dplyr::select(c(Site, 
                      release_nut_pop_tot_period_all_scats_sites, 
                      release_nut_pop_tot_period_scenario00,
                      release_nut_pop_tot_period_scenario10, 
                      release_nut_pop_tot_period_scenario20, 
                      release_nut_pop_tot_period_scenario30, 
                      release_nut_pop_tot_period_scenario40, 
                      release_nut_pop_tot_period_scenario50, 
                      release_nut_pop_tot_period_scenario60, 
                      release_nut_pop_tot_period_scenario70, 
                      release_nut_pop_tot_period_scenario80, 
                      release_nut_pop_tot_period_scenario90, 
                      release_nut_pop_tot_period_scenario100))
    
    list_light <- append(list_light, list_output_scenarios_i_light)
  }
  
  
  # tibble with mean of the estimate made with all the (observed) scat samples
  # and their respective ratios
  df_mean <- list_light[[1]] |> # could be any number should always 
    # be the same (with variability due to bootstrapp)
    dplyr::select(c(Site, 
                  release_nut_pop_tot_period_all_scats_sites)) |>
    dplyr::filter(Site == site) |>
    tidyr::unnest(release_nut_pop_tot_period_all_scats_sites) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |>
    dplyr::group_by(Site, Nutrient) |>
    dplyr::summarise(mean_all_scats_kg = mean(tot_pop_release_period_kg))
  
  
  df_compiled_mean_subscenarios <- data.frame(Site = NA, 
                                              scenario = NA,
                                              subscenario = NA,
                                              Nutrient = NA, 
                                              mean_subscenario_kg = NA)
  
  for (i in c(1:nb_test)) {
    
    # with 00% scat samples from one enriched cluster - 00% scenario
    table_test_i_0 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario00)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(scenario = i, 
                    subscenario = "0%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    # with 10% scat samples from one enriched cluster - 10% scenario
    table_test_i_10 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario10)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(scenario = i, 
                    subscenario = "10%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    
    # with 20% scat samples from one enriched cluster - 20% scenario
    table_test_i_20 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario20)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(scenario = i, 
                    subscenario = "20%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    
    # with 30% scat samples from one enriched cluster - 30% scenario
    table_test_i_30 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario30)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(scenario = i, 
                    subscenario = "30%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    
    # with 40% scat samples from one enriched cluster - 40% scenario
    table_test_i_40 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario40)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |>
      dplyr::mutate(scenario = i, 
                    subscenario = "40%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    
    # with 50% scat samples from one enriched cluster - 50% scenario
    table_test_i_50 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario50)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(scenario = i, 
                    subscenario = "50%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    
    # with 60% scat samples from one enriched cluster - 60% scenario
    table_test_i_60 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario60)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(scenario = i, 
                    subscenario = "60%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    
    # with 70% scat samples from one enriched cluster - 70% scenario
    table_test_i_70 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario70)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(scenario = i, 
                    subscenario = "70%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    
    # with 80% scat samples from one enriched cluster - 80% scenario
    table_test_i_80 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario80)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(scenario = i, 
                    subscenario = "80%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    
    # with 90% scat samples from one enriched cluster - 90% scenario
    table_test_i_90 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario90)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(scenario = i, 
                    subscenario = "90%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    # with 100% scat samples from one enriched cluster - 100% scenario
    table_test_i_100 <- list_light[[i]] |>
      dplyr::select(c(Site,
                    release_nut_pop_tot_period_scenario100)) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(scenario = i, 
                    subscenario = "100%") |>
      dplyr::group_by(Site, scenario, subscenario, Nutrient) |>
      dplyr::summarise(mean_subscenario_kg = mean(tot_pop_release_period_kg))
    
    table_mean_nut_test_i <- rbind(
      table_test_i_0,
      table_test_i_10,
      table_test_i_20,
      table_test_i_30,
      table_test_i_40,
      table_test_i_50,
      table_test_i_60,
      table_test_i_70,
      table_test_i_80,
      table_test_i_90,
      table_test_i_100
    ) 
    
    # compile with the rest 
    df_compiled_mean_subscenarios <- rbind(df_compiled_mean_subscenarios, 
                                           table_mean_nut_test_i)
    
  }
  
  # delete first line of NA 
  df_compiled_mean_subscenarios <- df_compiled_mean_subscenarios[-1, ]
  
  # join with mean for all scats 
  table <- df_compiled_mean_subscenarios |>
    dplyr::left_join(df_mean, by = c("Site", "Nutrient")) |>
    dplyr::mutate(Nutrient = factor(Nutrient,
                                    levels = c("P", "Fe", "Zn",
                                               "Cu", "Mn", "Se",
                                               "Co")),
                  diff_mean_with_all_scats = mean_subscenario_kg - mean_all_scats_kg,
                  percent_of_mean_with_all_scats = round(100*(diff_mean_with_all_scats/mean_all_scats_kg), 0)) |>
    dplyr::select(c(Site, scenario, subscenario, Nutrient, percent_of_mean_with_all_scats)) |>
    dplyr::group_by(scenario, Nutrient) |>
    dplyr::summarise(mean = round(mean(abs(percent_of_mean_with_all_scats)), 0), 
                     min = min(percent_of_mean_with_all_scats),
                     max = max(percent_of_mean_with_all_scats)) |>
    tidyr::pivot_longer(cols = c(mean, min, max), 
                        names_to = "Summarising variables",
                        values_to = "Percent value") |>
    dplyr::mutate(`Summarising variables` = dplyr::case_when(`Summarising variables` == "mean" ~ "Mean of absolute % of difference", 
                                                             TRUE ~ `Summarising variables`)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = `Percent value`)
  
  
  
  openxlsx::write.xlsx(table,
                       file = paste0("output/sites/percent_differences_with_all_scat_est_all_scenarios_",
                                     site, ".xlsx"))
  
}


# same, was impossible to run without memory crash, so was decomposed
#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different habitats 
MWtest_test_nut_sites_tot_period_all_scenarios <- function(list_output_scenarios,
                                                           site # "Cap Noir" or "Pointe Suzanne"
) {
  
  nb_test <- length(list_output_scenarios)
  
  df_compiled_tests <- data.frame(Site = NA, 
                                  scenario = NA,
                                  subscenario = NA,
                                  P = NA,
                                  Fe = NA, 
                                  Zn = NA, 
                                  Cu = NA, 
                                  Mn = NA, 
                                  Se = NA, 
                                  Co = NA)
  
  for (i in c(1:nb_test)) {
    table_test_nut_test_i <- rbind(
      # all samples from site
      list_output_scenarios[[i]] |>
        dplyr::select(Site, 
                      release_nut_pop_tot_period_all_scats_sites) |>
        tidyr::unnest(release_nut_pop_tot_period_all_scats_sites) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "all scat samples from colony"), 
      # with 00% scat samples from one enriched cluster - 00% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario00) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "0%"), 
      # with 10% scat samples from one enriched cluster - 10% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario10) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "10%"), 
      # with 20% scat samples from one enriched cluster - 20% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario20) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "20%"), 
      # with 30% scat samples from one enriched cluster - 30% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario30) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "30%"), 
      # with 40% scat samples from one enriched cluster - 40% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario40) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "40%"), 
      # with 50% scat samples from one enriched cluster - 50% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario50) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "50%"), 
      # with 60% scat samples from one enriched cluster - 60% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario60) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "60%"), 
      # with 70% scat samples from one enriched cluster - 70% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario70) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "70%"), 
      # with 80% scat samples from one enriched cluster - 80% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario80) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "80%"), 
      # with 90% scat samples from one enriched cluster - 90% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario90) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "90%"), 
      # with 100% scat samples from one enriched cluster - 100% scenario
      list_output_scenarios[[i]] |>
        dplyr::select(Site,
                      release_nut_pop_tot_period_scenario100) |>
        tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
        tidyr::pivot_longer(cols = c(P:Co), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::mutate(scenario = "100%")
    )  |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      tidyr::pivot_wider(names_from = scenario,
                         values_from = tot_pop_release_period_kg,
                         values_fn = list) |>
      tidyr::unnest(cols = c(`all scat samples from colony`, `0%`, `10%`, `20%`,
                             `30%`, `40%`, `50%`, `60%`, `70%`, `80%`,
                             `90%`, `100%`)) |>
      dplyr::mutate(t_obs_0 = dplyr::case_when(`all scat samples from colony` > `0%` ~ 1, TRUE ~ 0), 
                    t_obs_10 = dplyr::case_when(`all scat samples from colony` > `10%` ~ 1, TRUE ~ 0),
                    t_obs_20 = dplyr::case_when(`all scat samples from colony` > `20%` ~ 1, TRUE ~ 0),
                    t_obs_30 = dplyr::case_when(`all scat samples from colony` > `30%` ~ 1, TRUE ~ 0),
                    t_obs_40 = dplyr::case_when(`all scat samples from colony` > `40%` ~ 1, TRUE ~ 0),
                    t_obs_50 = dplyr::case_when(`all scat samples from colony` > `50%` ~ 1, TRUE ~ 0),
                    t_obs_60 = dplyr::case_when(`all scat samples from colony` > `60%` ~ 1, TRUE ~ 0),
                    t_obs_70 = dplyr::case_when(`all scat samples from colony` > `70%` ~ 1, TRUE ~ 0),
                    t_obs_80 = dplyr::case_when(`all scat samples from colony` > `80%` ~ 1, TRUE ~ 0),
                    t_obs_90 = dplyr::case_when(`all scat samples from colony` > `90%` ~ 1, TRUE ~ 0),
                    t_obs_100 = dplyr::case_when(`all scat samples from colony` > `100%` ~ 1, TRUE ~ 0)) |>
      dplyr::group_by(Site, Nutrient) |>
      dplyr::summarise(test_obs_0 = mean(t_obs_0), 
                       test_obs_10 = mean(t_obs_10), 
                       test_obs_20 = mean(t_obs_20), 
                       test_obs_30 = mean(t_obs_30), 
                       test_obs_40 = mean(t_obs_40), 
                       test_obs_50 = mean(t_obs_50), 
                       test_obs_60 = mean(t_obs_60), 
                       test_obs_70 = mean(t_obs_70), 
                       test_obs_80 = mean(t_obs_80), 
                       test_obs_90 = mean(t_obs_90), 
                       test_obs_100 = mean(t_obs_100)) |>
      tidyr::pivot_longer(cols = c(test_obs_0:test_obs_100), 
                          names_to = "subscenario", 
                          values_to = "test_obs_scenario") |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = test_obs_scenario) |>
      dplyr::mutate(scenario = i) |>
      dplyr::select(c(Site, scenario, subscenario, P:Co))
    
    
    # compile with the rest 
    df_compiled_tests <- rbind(df_compiled_tests, table_test_nut_test_i)
    
  }
  
  # delete first line of NA 
  df_compiled_tests <- df_compiled_tests[-1, ]
  
  openxlsx::write.xlsx(df_compiled_tests, 
                       file = paste0("output/sites/test_differences_nut_all_scenarios_", 
                                     site, ".xlsx"))
  
}