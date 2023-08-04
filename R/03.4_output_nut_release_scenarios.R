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
nut_per_site_tot_period_scenarios <- function(output_nut_release_tib) {
  
  rbind(output_nut_release_tib |>
          dplyr::select(Site, 
                        release_nut_pop_tot_period_sites) |>
          tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
          tidyr::pivot_longer(cols = c(Fe:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_mg") |> 
          dplyr::mutate(Nutrient = factor(Nutrient, 
                                          levels = c("Fe", "Zn", 
                                                     "Cu", "Mn", "Se",
                                                     "Co")), 
                        scat_compo = "All scat\nsamples mixed") |>
          dplyr::group_by(Site, scat_compo, Nutrient) |>
          dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6, 
                           mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg 
                           `10_quant` = quantile(tot_pop_release_period_mg, 
                                                 probs = c(0.1))*1e-6,
                           `80_quant` = quantile(tot_pop_release_period_mg, 
                                                 probs = c(0.8))*1e-6, 
                           max = max(tot_pop_release_period_mg)*1e-6), 
        output_nut_release_tib |>
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
                        scat_compo = "Scat samples\nseparated\nper site") |>
          dplyr::group_by(Site, scat_compo, Nutrient) |>
          dplyr::summarise(min = min(tot_pop_release_period_mg)*1e-6, 
                           mean = mean(tot_pop_release_period_mg)*1e-6, # from mg to kg 
                           `10_quant` = quantile(tot_pop_release_period_mg, 
                                                 probs = c(0.1))*1e-6,
                           `80_quant` = quantile(tot_pop_release_period_mg, 
                                                 probs = c(0.8))*1e-6, 
                           max = max(tot_pop_release_period_mg)*1e-6)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, 
                                   y = mean, 
                                   fill = Site), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`), 
                            color = "black",
                            position = ggplot2::position_dodge2(width = 1)) +
    ggplot2::scale_fill_manual(values = c("#D8AF39FF",
                                          "#58A449FF",
                                          "#AE93BEFF",
                                          "#B4DAE5FF",
                                          "#E75B64FF",
                                          "#1D2645FF")) +
    ggplot2::facet_wrap(~ scat_compo) +
    ggplot2::ylab("Total nutrient released\nin feces during breeding\nand moulting period (in kg)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/barplot_nut_release_tot_pop_sites.jpg",
                  scale = 1,
                  height = 4, width = 10
  )
}
