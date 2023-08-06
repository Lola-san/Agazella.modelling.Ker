################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 02.5_output_nut_release.R
#
################################################################################

#'
#'
#'
#'
#
nut_per_site_tot_period <- function(output_nut_release_tib) {
  
  rbind(output_nut_release_tib |>
          dplyr::select(Site, 
                        release_nut_pop_tot_period_all_scats) |>
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




#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different habitats 
MWtest_test_nut_sites_tot_period <- function(output_nut_release_tib) {
  
  table_test_scat_per_site <- output_nut_release_tib |>
    dplyr::select(Site, 
                  release_nut_pop_tot_period_sites) |>
    tidyr::unnest(release_nut_pop_tot_period_sites) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_mg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |>
    dplyr::select(Site, Nutrient, tot_pop_release_period_mg) |>
    tidyr::pivot_wider(names_from = Site,
                       values_from = tot_pop_release_period_mg,
                       values_fn = list) |>
    tidyr::unnest(cols = c(`Cap Noir`, `Pointe Suzanne`)) |>
    dplyr::mutate(t_PS_CN = dplyr::case_when(`Pointe Suzanne` > `Cap Noir` ~ 1,
                                             TRUE ~ 0)) |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(test_nut_sites = mean(t_PS_CN)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = test_nut_sites)
  
  openxlsx::write.xlsx(table_test_scat_per_site, 
                       file = paste0("output/sites/test_differences_sites_nut_scat_per_site.xlsx"))
  
  
  table_test_all_scats <- output_nut_release_tib |>
    dplyr::select(Site, 
                  release_nut_pop_tot_period_all_scats) |>
    tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_mg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |>
    dplyr::select(Site, Nutrient, tot_pop_release_period_mg) |>
    tidyr::pivot_wider(names_from = Site,
                       values_from = tot_pop_release_period_mg,
                       values_fn = list) |>
    tidyr::unnest(cols = c(`Cap Noir`, `Pointe Suzanne`)) |>
    dplyr::mutate(t_PS_CN = dplyr::case_when(`Pointe Suzanne` > `Cap Noir` ~ 1,
                                             TRUE ~ 0)) |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(test_nut_sites = mean(t_PS_CN)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = test_nut_sites)
  
  openxlsx::write.xlsx(table_test_all_scats, 
                       file = paste0("output/sites/test_differences_sites_nut_all_scats.xlsx"))
  
}
