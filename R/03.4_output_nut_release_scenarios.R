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
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
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
  
  
  
}
