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
nut_per_site_tot_period <- function(output_nut_release_CN, 
                                    output_nut_release_PS) {
  
  # first with all scats mixed 
  rbind(output_nut_release_CN |>
          dplyr::select(c(Site, 
                        release_nut_pop_tot_period_all_scats)), 
        output_nut_release_PS |>
          dplyr::select(c(Site, 
                        release_nut_pop_tot_period_all_scats))) |>
    tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  scat_compo = "All scat\nsamples mixed", 
                  Site = factor(dplyr::case_when(Site == "Cap Noir" ~ "Cap\nNoir", 
                                                 Site == "Pointe Suzanne" ~ "Pointe\nSuzanne"), 
                                levels = c("Cap\nNoir", "Pointe\nSuzanne"))) |>
    dplyr::group_by(Site, scat_compo, Nutrient) |>
    dplyr::summarise(min = min(tot_pop_release_period_kg), 
                     mean = mean(tot_pop_release_period_kg), 
                     `2.5_quant` = quantile(tot_pop_release_period_kg, 
                                           probs = c(0.025)),
                     `97.5_quant` = quantile(tot_pop_release_period_kg, 
                                           probs = c(0.975)), 
                     max = max(tot_pop_release_period_kg)) |>
    dplyr::mutate(y_lim = dplyr::case_when(Nutrient == "P" ~ 500,
                                           Nutrient == "Fe" ~ 35, 
                                           Nutrient == "Zn" ~ 3.9, 
                                           Nutrient == "Cu" ~ 1.9, 
                                           Nutrient == "Mn" ~ 0.9, 
                                           Nutrient == "Se" ~ 0.3, 
                                           Nutrient == "Co" ~ 0.03)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Site, 
                                   y = mean, 
                                   fill = Site), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Site, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`), 
                            color = "darkgrey",
                            position = ggplot2::position_dodge2(width = 1)) +
    ggplot2::scale_fill_manual(values = c("#353839",
                                          "#AE93BEFF")) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
    ggplot2::ggtitle("With all scat samples mixed") +
    ggplot2::ylab("Total nutrient released\nin feces during breeding\nand moulting period (in kg)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/barplot_nut_release_tot_pop_sites_all_scats_mixed.jpg",
                  scale = 1,
                  height = 5, width = 8
  )
  
  # then with scat samples separated per site 
  rbind(output_nut_release_CN |>
          dplyr::select(c(Site, 
                          release_nut_pop_tot_period_sites)), 
        output_nut_release_PS |>
          dplyr::select(c(Site, 
                          release_nut_pop_tot_period_sites))) |>
    tidyr::unnest(release_nut_pop_tot_period_sites) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("P", "Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  scat_compo = "Scat samples\nseparated\nper site", 
                  Site = factor(dplyr::case_when(Site == "Cap Noir" ~ "Cap\nNoir", 
                                                 Site == "Pointe Suzanne" ~ "Pointe\nSuzanne"), 
                                levels = c("Cap\nNoir", "Pointe\nSuzanne"))) |>
    dplyr::group_by(Site, scat_compo, Nutrient) |>
    dplyr::summarise(min = min(tot_pop_release_period_kg), 
                     mean = mean(tot_pop_release_period_kg), 
                     `2.5_quant` = quantile(tot_pop_release_period_kg, 
                                           probs = c(0.025)),
                     `97.5_quant` = quantile(tot_pop_release_period_kg, 
                                           probs = c(0.975)), 
                     max = max(tot_pop_release_period_kg)) |>
    dplyr::mutate(y_lim = dplyr::case_when(Nutrient == "P" ~ 500,
                                           Nutrient == "Fe" ~ 35, 
                                           Nutrient == "Zn" ~ 3.9, 
                                           Nutrient == "Cu" ~ 1.9, 
                                           Nutrient == "Mn" ~ 0.9, 
                                           Nutrient == "Se" ~ 0.3, 
                                           Nutrient == "Co" ~ 0.03)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Site, 
                                   y = mean, 
                                   fill = Site), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Site, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`), 
                            color = "darkgrey",
                            position = ggplot2::position_dodge2(width = 1)) +
    ggplot2::scale_fill_manual(values = c("#353839",
                                          "#AE93BEFF")) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
    ggplot2::ggtitle("With scat samples separated per site") +
    ggplot2::ylab("Total nutrient released\nin feces during breeding\nand moulting period (in kg)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   title = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/barplot_nut_release_tot_pop_sites_scats_per_site.jpg",
                  scale = 1,
                  height = 5, width = 8
  )
}



#'
#'
#'
#'
#
table_nut_per_site_sea_land_period <- function(output_nut_release_CN, 
                                               output_nut_release_PS) {
  
  table_summary <- rbind(
    # first with all scat samples mixed
    rbind(output_nut_release_CN |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_land_all_scats), 
          output_nut_release_PS |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_land_all_scats)) |>
      tidyr::unnest(release_nut_pop_tot_period_land_all_scats) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "on_land_pop_release_period_kg") |> 
      dplyr::group_by(Site, Nutrient) |>
      dplyr::summarise(mean_on_land = round(mean(on_land_pop_release_period_kg),4), 
                       low2_5quant_on_land = round(quantile(on_land_pop_release_period_kg, 0.025), 4), 
                       high97_5quant_on_land = round(quantile(on_land_pop_release_period_kg, 0.975), 4), 
                       min_on_land = round(min(on_land_pop_release_period_kg), 4),
                       max_on_land = round(max(on_land_pop_release_period_kg), 4)) |>
      tidyr::pivot_longer(cols = c(mean_on_land:max_on_land), 
                          names_to = "variable", 
                          values_to = "estimates") |>
      dplyr::bind_rows(
        rbind(output_nut_release_CN |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sea_all_scats), 
              output_nut_release_PS |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sea_all_scats)) |>
          tidyr::unnest(release_nut_pop_tot_period_sea_all_scats) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "at_sea_pop_release_period_kg") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_at_sea = round(mean(at_sea_pop_release_period_kg),4),
                           low2_5quant_at_sea = round(quantile(at_sea_pop_release_period_kg, 0.025), 4), 
                           high97_5quant_at_sea = round(quantile(at_sea_pop_release_period_kg, 0.975), 4),  
                           min_at_sea = round(min(at_sea_pop_release_period_kg), 4),
                           max_at_sea = round(max(at_sea_pop_release_period_kg), 4)) |>
          tidyr::pivot_longer(cols = c(mean_at_sea:max_at_sea), 
                              names_to = "variable", 
                              values_to = "estimates")) |>
      dplyr::bind_rows(
        rbind(output_nut_release_CN |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_all_scats), 
              output_nut_release_PS |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_all_scats)) |>
          tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "release_nut_pop_tot_period_sites") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_tot = round(mean(release_nut_pop_tot_period_sites),4), 
                           low2_5quant_tot = round(quantile(release_nut_pop_tot_period_sites, 0.025), 4), 
                           high97_5quant_tot = round(quantile(release_nut_pop_tot_period_sites, 0.975), 4),  
                           min_tot = round(min(release_nut_pop_tot_period_sites), 4),
                           max_tot = round(max(release_nut_pop_tot_period_sites), 4)) |>
          tidyr::pivot_longer(cols = c(mean_tot:max_tot), 
                              names_to = "variable", 
                              values_to = "estimates")) |>
      tidyr::pivot_wider(names_from = variable, 
                         values_from = estimates) |>
      dplyr::mutate(on_land_percent = round(100*mean_on_land/mean_tot, 0), 
                    at_sea_percent = round(100*mean_at_sea/mean_tot, 0)) |>
      tidyr::pivot_longer(cols = c(mean_on_land:at_sea_percent), 
                          names_to = "level_variable", 
                          values_to = "estimate_kg") |>
      dplyr::mutate(level = dplyr::case_when(stringr::str_detect(level_variable, "tot") ~ "total", 
                                             stringr::str_detect(level_variable, "on_land") ~ "on land",
                                             stringr::str_detect(level_variable, "at_sea") ~ "at sea"), 
                    variable = dplyr::case_when(stringr::str_detect(level_variable, "mean") ~ "mean", 
                                                stringr::str_detect(level_variable, "low2_5") ~ "2.5% quantile",
                                                stringr::str_detect(level_variable, "high97_5") ~ "97.5% quantile",
                                                stringr::str_detect(level_variable, "min") ~ "min",
                                                stringr::str_detect(level_variable, "max") ~ "max",
                                                stringr::str_detect(level_variable, "percent") ~ "percent"), 
                    scat_compo = "All scat samples mixed", 
                    Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = estimate_kg) |>
      dplyr::select(c(scat_compo, Site, level, variable,  
                      P, Fe, Zn, Cu, Mn, Se, Co)),
    # then with scat samples separated per site 
    rbind(output_nut_release_CN |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_land_sites), 
          output_nut_release_PS |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_land_sites)) |>
      tidyr::unnest(release_nut_pop_tot_period_land_sites) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "on_land_pop_release_period_kg") |> 
      dplyr::group_by(Site, Nutrient) |>
      dplyr::summarise(mean_on_land = round(mean(on_land_pop_release_period_kg),4), 
                       low2_5quant_on_land = round(quantile(on_land_pop_release_period_kg, 0.025), 4), 
                       high97_5quant_on_land = round(quantile(on_land_pop_release_period_kg, 0.975), 4),  
                       min_on_land = round(min(on_land_pop_release_period_kg), 4),
                       max_on_land = round(max(on_land_pop_release_period_kg), 4)) |>
      tidyr::pivot_longer(cols = c(mean_on_land:max_on_land), 
                          names_to = "variable", 
                          values_to = "estimates") |>
      dplyr::bind_rows(
        rbind(output_nut_release_CN |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sea_sites), 
              output_nut_release_PS |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sea_sites)) |>
          tidyr::unnest(release_nut_pop_tot_period_sea_sites) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "at_sea_pop_release_period_kg") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_at_sea = round(mean(at_sea_pop_release_period_kg),4), 
                           low2_5quant_at_sea = round(quantile(at_sea_pop_release_period_kg, 0.025), 4), 
                           high97_5quant_at_sea = round(quantile(at_sea_pop_release_period_kg, 0.975), 4),  
                           min_at_sea = round(min(at_sea_pop_release_period_kg), 4),
                           max_at_sea = round(max(at_sea_pop_release_period_kg), 4)) |>
          tidyr::pivot_longer(cols = c(mean_at_sea:max_at_sea), 
                              names_to = "variable", 
                              values_to = "estimates")) |>
      dplyr::bind_rows(
        rbind(output_nut_release_CN |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sites), 
              output_nut_release_PS |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sites)) |>
          tidyr::unnest(release_nut_pop_tot_period_sites) |>
          tidyr::pivot_longer(cols = c(P:Co), 
                              names_to = "Nutrient", 
                              values_to = "release_nut_pop_tot_period_sites") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_tot = round(mean(release_nut_pop_tot_period_sites),4), 
                           low2_5quant_tot = round(quantile(release_nut_pop_tot_period_sites, 0.025), 4), 
                           high97_5quant_tot = round(quantile(release_nut_pop_tot_period_sites, 0.975), 4), 
                           min_tot = round(min(release_nut_pop_tot_period_sites), 4),
                           max_tot = round(max(release_nut_pop_tot_period_sites), 4)) |>
          tidyr::pivot_longer(cols = c(mean_tot:max_tot), 
                              names_to = "variable", 
                              values_to = "estimates")) |>
      tidyr::pivot_wider(names_from = variable, 
                         values_from = estimates) |>
      dplyr::mutate(on_land_percent = round(100*mean_on_land/mean_tot, 0), 
                    at_sea_percent = round(100*mean_at_sea/mean_tot, 0)) |>
      tidyr::pivot_longer(cols = c(mean_on_land:at_sea_percent), 
                          names_to = "level_variable", 
                          values_to = "estimate_kg") |>
      dplyr::mutate(level = dplyr::case_when(stringr::str_detect(level_variable, "tot") ~ "total", 
                                             stringr::str_detect(level_variable, "on_land") ~ "on land",
                                             stringr::str_detect(level_variable, "at_sea") ~ "at sea"), 
                    variable = dplyr::case_when(stringr::str_detect(level_variable, "mean") ~ "mean", 
                                                stringr::str_detect(level_variable, "low2_5") ~ "2.5% quantile",
                                                stringr::str_detect(level_variable, "high97_5") ~ "97.5% quantile",
                                                stringr::str_detect(level_variable, "min") ~ "min",
                                                stringr::str_detect(level_variable, "max") ~ "max",
                                                stringr::str_detect(level_variable, "percent") ~ "percent"), 
                    scat_compo = "Scats separated between colonies", 
                    Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = estimate_kg) |>
      dplyr::select(c(scat_compo, Site, level, variable,  
                      P, Fe, Zn, Cu, Mn, Se, Co)))
  
  
  openxlsx::write.xlsx(table_summary, 
                       file = paste0("output/sites/table_summary_sites_nut_total_estimates.xlsx"))
  
}



#'
#'
#'
#'
#'
#
test_nut_sites_tot_period <- function(output_nut_release_CN, 
                                      output_nut_release_PS) {
  
  table_test <- rbind(
    # first with scats separated per site
    rbind(output_nut_release_CN |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_sites), 
          output_nut_release_PS |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_sites)) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |>
      dplyr::select(Site, Nutrient, tot_pop_release_period_kg) |>
      tidyr::pivot_wider(names_from = Site,
                         values_from = tot_pop_release_period_kg,
                         values_fn = list) |>
      tidyr::unnest(cols = c(`Cap Noir`, `Pointe Suzanne`)) |>
      dplyr::mutate(t_PS_CN = dplyr::case_when(`Pointe Suzanne` > `Cap Noir` ~ 1,
                                               TRUE ~ 0), 
                    scat_compo = "Scats separated per site") |>
      dplyr::group_by(scat_compo, Nutrient) |>
      dplyr::summarise(test_nut_sites = round(mean(t_PS_CN), 5)) |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = test_nut_sites),
    # then with all scats mixed 
    table_test_all_scats <- rbind(output_nut_release_CN |>
                                    dplyr::select(Site, 
                                                  release_nut_pop_tot_period_all_scats), 
                                  output_nut_release_PS |>
                                    dplyr::select(Site, 
                                                  release_nut_pop_tot_period_all_scats)) |>
      tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |>
      dplyr::select(Site, Nutrient, tot_pop_release_period_kg) |>
      tidyr::pivot_wider(names_from = Site,
                         values_from = tot_pop_release_period_kg,
                         values_fn = list) |>
      tidyr::unnest(cols = c(`Cap Noir`, `Pointe Suzanne`)) |>
      dplyr::mutate(t_PS_CN = dplyr::case_when(`Pointe Suzanne` > `Cap Noir` ~ 1,
                                               TRUE ~ 0), 
                    scat_compo = "All scats mixed") |>
      dplyr::group_by(scat_compo, Nutrient) |>
      dplyr::summarise(test_nut_sites = round(mean(t_PS_CN), 5)) |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = test_nut_sites)
  )
  
  openxlsx::write.xlsx(table_test, 
                       file = paste0("output/sites/test_differences_sites_with_and_without_all_scats.xlsx"))
  
}

