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
                                              nut # Fe, Zn, Cu, ...
) {
  
  full_tib <- rbind(
    # with all scats taken on site
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = "All scat samples from colony") |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with only scat samples from one enriched cluster - 100% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("100% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("90% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("80% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("70% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("60% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("50% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("40% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("30% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("20% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("10% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_release_scenarios_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    scat_compo = paste0("0% of scat samples enriched in ", nut)) |>
      dplyr::group_by(Site, scat_compo, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6)
  ) |>
    dplyr::mutate(scat_compo = factor(scat_compo,
                                      levels = c("All scat samples from colony",
                                                 paste0("100% of scat samples enriched in ", nut),
                                                 paste0("90% of scat samples enriched in ", nut),
                                                 paste0("80% of scat samples enriched in ", nut),
                                                 paste0("70% of scat samples enriched in ", nut),
                                                 paste0("60% of scat samples enriched in ", nut),
                                                 paste0("50% of scat samples enriched in ", nut),
                                                 paste0("40% of scat samples enriched in ", nut),
                                                 paste0("30% of scat samples enriched in ", nut),
                                                 paste0("20% of scat samples enriched in ", nut),
                                                 paste0("10% of scat samples enriched in ", nut),
                                                 paste0("0% of scat samples enriched in ", nut)
                                      )))
  
  #################### CAP NOIR BARPLOT ######################################
  
  full_tib |>
    dplyr::filter(Site == "Cap Noir") |>
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
    ggplot2::ggtitle("Cap Noir") +
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
  ggplot2::ggsave(paste0("output/sites/barplot_nut_release_tot_pop_sites_scenarios_CN_",
                         nut, ".jpg"),
                  scale = 1,
                  height = 6, width = 10
  )
  
  
  
  full_tib |>
    dplyr::filter(Site == "Cap Noir") |>
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
    ggplot2::ggtitle("Cap Noir") +
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
  ggplot2::ggsave(paste0("output/sites/scatterplot_nut_release_tot_pop_sites_scenarios_CN_",
                         nut, ".jpg"),
                  scale = 1,
                  height = 5, width = 12
  )
  
  
  #################### POINTE SUZANNE BARPLOT ##################################
  
  full_tib |>
    dplyr::filter(Site == "Pointe Suzanne") |>
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
                                          "#44A57CFF", "#E8C4A2FF")) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::ylab("Total nutrient released\nin feces during breeding\nand moulting period (in kg)") +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::ggtitle("Pointe Suzanne") +
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
  ggplot2::ggsave(paste0("output/sites/barplot_nut_release_tot_pop_sites_scenarios_PS_",
                         nut, ".jpg"),
                  scale = 1,
                  height = 6, width = 10
  )
  
  
  full_tib |>
    dplyr::filter(Site == "Pointe Suzanne") |>
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
    ggplot2::ggtitle("Pointe Suzanne") +
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
  ggplot2::ggsave(paste0("output/sites/scatterplot_nut_release_tot_pop_sites_scenarios_PS_",
                         nut, ".jpg"),
                  scale = 1,
                  height = 5, width = 12
  )
  
  
  
}



#'
#'
#'
#'
#
nut_per_site_tot_period_all_scenarios <- function(output_nut_release_scenario1_tib,
                                                  output_nut_release_scenario2_tib,
                                                  output_nut_release_scenario3_tib,
                                                  output_nut_release_scenario4_tib
) {
  
  full_tib1 <- rbind(
    # with all scats taken on site
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "All scat samples from colony") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with only scat samples from one enriched cluster - 100% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "100") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "90") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "80") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "70") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "60") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "50") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "40") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "30") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "20") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "10") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_release_scenario1_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "0") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6)
  ) |>
    dplyr::mutate(sub_scenario = factor(sub_scenario,
                                        levels = c("All scat samples from colony",
                                                   "100",
                                                   "90",
                                                   "80",
                                                   "70",
                                                   "60",
                                                   "50",
                                                   "40",
                                                   "30",
                                                   "20",
                                                   "10",
                                                   "0")),
                  scenario = dplyr::case_when(sub_scenario == "All scat samples from colony" ~ sub_scenario, 
                                              TRUE ~ "changing % of cluster 1 type scat samples"))
  
  
  full_tib2 <- rbind(
    # with all scats taken on site: already in full_tib1
    # with only scat samples from one enriched cluster - 100% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "100") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "90") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "80") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "70") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "60") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "50") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "40") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "30") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "20") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "10") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_release_scenario2_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "0") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6)
  ) |>
    dplyr::mutate(sub_scenario = factor(sub_scenario,
                                        levels = c("100",
                                                   "90",
                                                   "80",
                                                   "70",
                                                   "60",
                                                   "50",
                                                   "40",
                                                   "30",
                                                   "20",
                                                   "10",
                                                   "0")),
                  scenario = "changing % of cluster 2 type scat samples")
  
  
  full_tib3 <- rbind(
    # with all scats taken on site: already in full_tib1
    # with only scat samples from one enriched cluster - 100% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "100") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "90") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "80") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "70") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "60") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "50") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "40") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "30") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "20") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "10") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_release_scenario3_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "0") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6)) |>
    dplyr::mutate(sub_scenario = factor(sub_scenario,
                                        levels = c("100",
                                                   "90",
                                                   "80",
                                                   "70",
                                                   "60",
                                                   "50",
                                                   "40",
                                                   "30",
                                                   "20",
                                                   "10",
                                                   "0")),
                  scenario = "changing % of cluster 3 type scat samples")
  
  
  full_tib4 <- rbind(
    # with all scats taken on site: already in full_tib1
    # with only scat samples from one enriched cluster - 100% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "100") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "90") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "80") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "70") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "60") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "50") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "40") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "30") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "20") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "10") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6),
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_release_scenario4_tib |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co),
                          names_to = "Nutrient",
                          values_to = "tot_pop_release_period_mg") |>
      dplyr::mutate(Nutrient = factor(Nutrient,
                                      levels = c("Fe", "Zn",
                                                 "Cu", "Mn", "Se",
                                                 "Co")),
                    sub_scenario = "0") |>
      dplyr::group_by(Site, sub_scenario, Nutrient) |>
      dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6,
                       mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg
                       `10_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.1))*1e-6,
                       `80_quant` = quantile(tot_pop_release_period_mg,
                                             probs = c(0.8))*1e-6,
                       max = max(tot_pop_release_period_mg)*1e-6)) |>
    dplyr::mutate(sub_scenario = factor(sub_scenario,
                                        levels = c("100",
                                                   "90",
                                                   "80",
                                                   "70",
                                                   "60",
                                                   "50",
                                                   "40",
                                                   "30",
                                                   "20",
                                                   "10",
                                                   "0")),
                  scenario = "changing % of cluster 4 type scat samples")
  
  ######################## CAP NOIR ############################################
  rbind(full_tib1 |>
          dplyr::filter(Site == "Cap Noir") |>
          dplyr::mutate(sub_scenario = dplyr::case_when(sub_scenario == "All scat samples from colony" ~ "19", 
                                                        TRUE ~ sub_scenario)), 
        full_tib2 |>
          dplyr::filter(Site == "Cap Noir") |>
          dplyr::mutate(sub_scenario = dplyr::case_when(sub_scenario == "All scat samples from colony" ~ "33", 
                                                        TRUE ~ sub_scenario)), 
        full_tib3 |>
          dplyr::filter(Site == "Cap Noir") |>
          dplyr::mutate(sub_scenario = dplyr::case_when(sub_scenario == "All scat samples from colony" ~ "33", 
                                                        TRUE ~ sub_scenario)), 
        full_tib4 |>
          dplyr::filter(Site == "Cap Noir") |>
          dplyr::mutate(sub_scenario = dplyr::case_when(sub_scenario == "All scat samples from colony" ~ "15", 
                                                        TRUE ~ sub_scenario))
  ) |>
    dplyr::mutate(sub_scenario = as.numeric(sub_scenario)) |>
    ggplot2::ggplot(ggplot2::aes(x = sub_scenario,
                                 y = mean,
                                 color = scenario)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(x = sub_scenario,
                                         ymin = `10_quant`,
                                         ymax = `80_quant`,
                                         color = scenario)) +
    ggplot2::scale_color_manual(values = c("#E75B64FF", "#44A57CFF",
                                           "#1D2645FF", "#D8AF39FF",
                                           "#AE93BEFF"
    )) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::ylab("Total nutrient released\nin feces during breeding\nand moulting period (in kg)") +
    ggplot2::xlab("% of cluster in scat samples bootstrapped") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::ggtitle("Cap Noir") +
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
  ggplot2::ggsave("output/sites/scatterplot_nut_release_tot_pop_sites_all_scenarios_CN.jpg",
                  scale = 1,
                  height = 7, width = 10
  )
  
  
  ######################## POINTE SUZANNE ######################################
  rbind(full_tib1 |>
          dplyr::filter(Site == "Pointe Suzanne") |>
          dplyr::mutate(sub_scenario = dplyr::case_when(sub_scenario == "All scat samples from colony" ~ "32", 
                                                        TRUE ~ sub_scenario)), 
        full_tib2 |>
          dplyr::filter(Site == "Pointe Suzanne") |>
          dplyr::mutate(sub_scenario = dplyr::case_when(sub_scenario == "All scat samples from colony" ~ "26", 
                                                        TRUE ~ sub_scenario)), 
        full_tib3 |>
          dplyr::filter(Site == "Pointe Suzanne") |>
          dplyr::mutate(sub_scenario = dplyr::case_when(sub_scenario == "All scat samples from colony" ~ "19", 
                                                        TRUE ~ sub_scenario)), 
        full_tib4 |>
          dplyr::filter(Site == "Pointe Suzanne") |>
          dplyr::mutate(sub_scenario = dplyr::case_when(sub_scenario == "All scat samples from colony" ~ "23", 
                                                        TRUE ~ sub_scenario))
  ) |>
    dplyr::mutate(sub_scenario = as.numeric(sub_scenario)) |>
    ggplot2::ggplot(ggplot2::aes(x = sub_scenario,
                                 y = mean,
                                 color = scenario)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(x = sub_scenario,
                                         ymin = `10_quant`,
                                         ymax = `80_quant`,
                                         color = scenario)) +
    ggplot2::scale_color_manual(values = c("#E75B64FF", "#44A57CFF",
                                           "#1D2645FF", "#D8AF39FF",
                                           "#AE93BEFF"
    )) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::ylab("Total nutrient released\nin feces during breeding\nand moulting period (in kg)") +
    ggplot2::xlab("% of cluster in scat samples bootstrapped") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::ggtitle("Pointe Suzanne") +
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
  ggplot2::ggsave("output/sites/scatterplot_nut_release_tot_pop_sites_all_scenarios_PS.jpg",
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
MWtest_test_nut_sites_tot_period_all_scenarios <- function(output_nut_release_scenario1_tib,
                                             output_nut_release_scenario2_tib,
                                             output_nut_release_scenario3_tib,
                                             output_nut_release_scenario4_tib) {
  
  
  ################ CAP NOIR ####################################################
  output_nut_sc1_CN <- output_nut_release_scenario1_tib |>
    dplyr::filter(Site == "Cap Noir")
  output_nut_sc2_CN <- output_nut_release_scenario2_tib |>
    dplyr::filter(Site == "Cap Noir")
  output_nut_sc3_CN <- output_nut_release_scenario3_tib |>
    dplyr::filter(Site == "Cap Noir")
  output_nut_sc4_CN <- output_nut_release_scenario4_tib |>
    dplyr::filter(Site == "Cap Noir")
  
  table_test_nut_sc1_CN <- rbind(
    # all samples from site
    output_nut_sc1_CN |>
    dplyr::select(Site, 
                  release_nut_pop_tot_period_sites) |>
    tidyr::unnest(release_nut_pop_tot_period_sites) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_mg") |> 
    dplyr::mutate(scenario = "all scat samples from colony"), 
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "0%"), 
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "10%"), 
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "20%"), 
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "30%"), 
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "40%"), 
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "50%"), 
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "60%"), 
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "70%"), 
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "80%"), 
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "90%"), 
    # with 100% scat samples from one enriched cluster - 100% scenario
    output_nut_sc1_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "100%")
    )  |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    tidyr::pivot_wider(names_from = scenario,
                       values_from = tot_pop_release_period_mg,
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
    dplyr::group_by(Nutrient) |>
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
    dplyr::mutate(scenario = 1,
                  site = "Cap Noir") |>
    dplyr::select(c(site, scenario, subscenario, Fe:Co))
  
  
  table_test_nut_sc2_CN <- rbind(
    # all samples from site
    output_nut_sc2_CN |>
      dplyr::select(Site, 
                    release_nut_pop_tot_period_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "all scat samples from colony"), 
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "0%"), 
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "10%"), 
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "20%"), 
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "30%"), 
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "40%"), 
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "50%"), 
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "60%"), 
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "70%"), 
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "80%"), 
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "90%"), 
    # with 100% scat samples from one enriched cluster - 100% scenario
    output_nut_sc2_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "100%")
  )  |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    tidyr::pivot_wider(names_from = scenario,
                       values_from = tot_pop_release_period_mg,
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
    dplyr::group_by(Nutrient) |>
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
    dplyr::mutate(scenario = 2,
                  site = "Cap Noir") |>
    dplyr::select(c(site, scenario, subscenario, Fe:Co))
  
  
  table_test_nut_sc3_CN <- rbind(
    # all samples from site
    output_nut_sc3_CN |>
      dplyr::select(Site, 
                    release_nut_pop_tot_period_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "all scat samples from colony"), 
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "0%"), 
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "10%"), 
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "20%"), 
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "30%"), 
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "40%"), 
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "50%"), 
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "60%"), 
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "70%"), 
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "80%"), 
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "90%"), 
    # with 100% scat samples from one enriched cluster - 100% scenario
    output_nut_sc3_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "100%")
  )  |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    tidyr::pivot_wider(names_from = scenario,
                       values_from = tot_pop_release_period_mg,
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
    dplyr::group_by(Nutrient) |>
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
    dplyr::mutate(scenario = 3,
                  site = "Cap Noir") |>
    dplyr::select(c(site, scenario, subscenario, Fe:Co))
  
  
  table_test_nut_sc4_CN <- rbind(
    # all samples from site
    output_nut_sc4_CN |>
      dplyr::select(Site, 
                    release_nut_pop_tot_period_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "all scat samples from colony"), 
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "0%"), 
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "10%"), 
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "20%"), 
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "30%"), 
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "40%"), 
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "50%"), 
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "60%"), 
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "70%"), 
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "80%"), 
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "90%"), 
    # with 100% scat samples from one enriched cluster - 100% scenario
    output_nut_sc4_CN |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "100%")
  )  |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    tidyr::pivot_wider(names_from = scenario,
                       values_from = tot_pop_release_period_mg,
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
    dplyr::group_by(Nutrient) |>
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
    dplyr::mutate(scenario = 4,
                  site = "Cap Noir") |>
    dplyr::select(c(site, scenario, subscenario, Fe:Co))
  
  
  openxlsx::write.xlsx(rbind(table_test_nut_sc1_CN,
                             table_test_nut_sc2_CN,
                             table_test_nut_sc3_CN,
                             table_test_nut_sc4_CN), 
                       file = paste0("output/sites/test_differences_nut_scenarios_CN.xlsx"))
  
  
  ########################### POINTE SUZANNE ###################################
  
  output_nut_sc1_PS <- output_nut_release_scenario1_tib |>
    dplyr::filter(Site == "Pointe Suzanne")
  output_nut_sc2_PS <- output_nut_release_scenario2_tib |>
    dplyr::filter(Site == "Pointe Suzanne")
  output_nut_sc3_PS <- output_nut_release_scenario3_tib |>
    dplyr::filter(Site == "Pointe Suzanne")
  output_nut_sc4_PS <- output_nut_release_scenario4_tib |>
    dplyr::filter(Site == "Pointe Suzanne")
  

  table_test_nut_sc1_PS <- rbind(
    # all samples from site
    output_nut_sc1_PS |>
      dplyr::select(Site, 
                    release_nut_pop_tot_period_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "all scat samples from colony"), 
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "0%"), 
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "10%"), 
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "20%"), 
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "30%"), 
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "40%"), 
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "50%"), 
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "60%"), 
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "70%"), 
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "80%"), 
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "90%"), 
    # with 100% scat samples from one enriched cluster - 100% scenario
    output_nut_sc1_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "100%")
  )  |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    tidyr::pivot_wider(names_from = scenario,
                       values_from = tot_pop_release_period_mg,
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
    dplyr::group_by(Nutrient) |>
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
    dplyr::mutate(scenario = 1,
                  site = "Pointe Suzanne") |>
    dplyr::select(c(site, scenario, subscenario, Fe:Co))
  
  
  table_test_nut_sc2_PS <- rbind(
    # all samples from site
    output_nut_sc2_PS |>
      dplyr::select(Site, 
                    release_nut_pop_tot_period_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "all scat samples from colony"), 
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "0%"), 
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "10%"), 
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "20%"), 
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "30%"), 
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "40%"), 
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "50%"), 
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "60%"), 
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "70%"), 
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "80%"), 
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "90%"), 
    # with 100% scat samples from one enriched cluster - 100% scenario
    output_nut_sc2_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "100%")
  )  |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    tidyr::pivot_wider(names_from = scenario,
                       values_from = tot_pop_release_period_mg,
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
    dplyr::group_by(Nutrient) |>
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
    dplyr::mutate(scenario = 2,
                  site = "Pointe Suzanne") |>
    dplyr::select(c(site, scenario, subscenario, Fe:Co))
  
  
  table_test_nut_sc3_PS <- rbind(
    # all samples from site
    output_nut_sc3_PS |>
      dplyr::select(Site, 
                    release_nut_pop_tot_period_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "all scat samples from colony"), 
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "0%"), 
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "10%"), 
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "20%"), 
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "30%"), 
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "40%"), 
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "50%"), 
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "60%"), 
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "70%"), 
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "80%"), 
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "90%"), 
    # with 100% scat samples from one enriched cluster - 100% scenario
    output_nut_sc3_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "100%")
  )  |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    tidyr::pivot_wider(names_from = scenario,
                       values_from = tot_pop_release_period_mg,
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
    dplyr::group_by(Nutrient) |>
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
    dplyr::mutate(scenario = 3,
                  site = "Pointe Suzanne") |>
    dplyr::select(c(site, scenario, subscenario, Fe:Co))
  
  
  table_test_nut_sc4_PS <- rbind(
    # all samples from site
    output_nut_sc4_PS |>
      dplyr::select(Site, 
                    release_nut_pop_tot_period_sites) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "all scat samples from colony"), 
    # with 00% scat samples from one enriched cluster - 00% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario00) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario00) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "0%"), 
    # with 10% scat samples from one enriched cluster - 10% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario10) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario10) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "10%"), 
    # with 20% scat samples from one enriched cluster - 20% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario20) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario20) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "20%"), 
    # with 30% scat samples from one enriched cluster - 30% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario30) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario30) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "30%"), 
    # with 40% scat samples from one enriched cluster - 40% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario40) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario40) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "40%"), 
    # with 50% scat samples from one enriched cluster - 50% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario50) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario50) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "50%"), 
    # with 60% scat samples from one enriched cluster - 60% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario60) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario60) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "60%"), 
    # with 70% scat samples from one enriched cluster - 70% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario70) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario70) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "70%"), 
    # with 80% scat samples from one enriched cluster - 80% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario80) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario80) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "80%"), 
    # with 90% scat samples from one enriched cluster - 90% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario90) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario90) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "90%"), 
    # with 100% scat samples from one enriched cluster - 100% scenario
    output_nut_sc4_PS |>
      dplyr::select(Site,
                    release_nut_pop_tot_period_scenario100) |>
      tidyr::unnest(release_nut_pop_tot_period_scenario100) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_mg") |> 
      dplyr::mutate(scenario = "100%")
  )  |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    tidyr::pivot_wider(names_from = scenario,
                       values_from = tot_pop_release_period_mg,
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
    dplyr::group_by(Nutrient) |>
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
    dplyr::mutate(scenario = 4,
                  site = "Pointe Suzanne") |>
    dplyr::select(c(site, scenario, subscenario, Fe:Co))
  
  
  openxlsx::write.xlsx(rbind(table_test_nut_sc1_PS,
                             table_test_nut_sc2_PS,
                             table_test_nut_sc3_PS,
                             table_test_nut_sc4_PS), 
                       file = paste0("output/sites/test_differences_nut_scenarios_PS.xlsx"))
  
}
