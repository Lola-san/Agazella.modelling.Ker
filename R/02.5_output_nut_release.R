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
nut_per_site_tot_period <- function(list_output_nut_release) {
  
  rbind(# first with all scat samples mixed
    rbind(list_output_nut_release$CN, list_output_nut_release$PS) |>
          dplyr::select(Site, 
                        release_nut_pop_tot_period_all_scats) |>
          tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
          tidyr::pivot_longer(cols = c(Fe:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(Nutrient = factor(Nutrient, 
                                          levels = c("Fe", "Zn", 
                                                     "Cu", "Mn", "Se",
                                                     "Co")), 
                        scat_compo = "All scat\nsamples mixed") |>
          dplyr::group_by(Site, scat_compo, Nutrient) |>
          dplyr::summarise(min = min(tot_pop_release_period_kg), 
                           mean = mean(tot_pop_release_period_kg), 
                           `10_quant` = quantile(tot_pop_release_period_kg, 
                                                 probs = c(0.1)),
                           `80_quant` = quantile(tot_pop_release_period_kg, 
                                                 probs = c(0.8)), 
                           max = max(tot_pop_release_period_kg)),
    # then with scat samples separated per site 
        rbind(list_output_nut_release$CN, list_output_nut_release$PS) |>
          dplyr::select(Site, 
                        release_nut_pop_tot_period_sites) |>
          tidyr::unnest(release_nut_pop_tot_period_sites) |>
          tidyr::pivot_longer(cols = c(Fe:Co), 
                              names_to = "Nutrient", 
                              values_to = "tot_pop_release_period_kg") |> 
          dplyr::mutate(Nutrient = factor(Nutrient, 
                                          levels = c("Fe", "Zn", 
                                                     "Cu", "Mn", "Se",
                                                     "Co")), 
                        scat_compo = "Scat samples\nseparated\nper site") |>
          dplyr::group_by(Site, scat_compo, Nutrient) |>
          dplyr::summarise(min = min(tot_pop_release_period_kg), 
                           mean = mean(tot_pop_release_period_kg), 
                           `10_quant` = quantile(tot_pop_release_period_kg, 
                                                 probs = c(0.1)),
                           `80_quant` = quantile(tot_pop_release_period_kg, 
                                                 probs = c(0.8)), 
                           max = max(tot_pop_release_period_kg))) |>
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
#
table_nut_per_site_sea_land_period <- function(list_output_nut_release) {
  
  table_summary <- rbind(
    # first with all scat samples mixed
    rbind(list_output_nut_release$CN, list_output_nut_release$PS) |>
                         dplyr::select(Site, 
                                       release_nut_pop_on_land_period_all_scats) |>
                         tidyr::unnest(release_nut_pop_on_land_period_all_scats) |>
                         tidyr::pivot_longer(cols = c(Fe:Co), 
                                             names_to = "Nutrient", 
                                             values_to = "on_land_pop_release_period_kg") |> 
      dplyr::group_by(Site, Nutrient) |>
      dplyr::summarise(mean_on_land = round(mean(on_land_pop_release_period_kg),4), 
                       min_on_land = round(min(on_land_pop_release_period_kg), 4),
                       max_on_land = round(max(on_land_pop_release_period_kg), 4)) |>
      tidyr::pivot_longer(cols = c(mean_on_land, min_on_land, max_on_land), 
                          names_to = "variable", 
                          values_to = "estimates") |>
      dplyr::bind_rows(
        rbind(list_output_nut_release$CN, list_output_nut_release$PS) |>
          dplyr::select(Site, 
                        release_nut_pop_at_sea_period_all_scats) |>
          tidyr::unnest(release_nut_pop_at_sea_period_all_scats) |>
          tidyr::pivot_longer(cols = c(Fe:Co), 
                              names_to = "Nutrient", 
                              values_to = "at_sea_pop_release_period_kg") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_at_sea = round(mean(at_sea_pop_release_period_kg),4), 
                           min_at_sea = round(min(at_sea_pop_release_period_kg), 4),
                           max_at_sea = round(max(at_sea_pop_release_period_kg), 4)) |>
          tidyr::pivot_longer(cols = c(mean_at_sea, min_at_sea, max_at_sea), 
                              names_to = "variable", 
                              values_to = "estimates")) |>
      dplyr::bind_rows(
        rbind(list_output_nut_release$CN, list_output_nut_release$PS) |>
          dplyr::select(Site, 
                        release_nut_pop_tot_period_all_scats) |>
          tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
          tidyr::pivot_longer(cols = c(Fe:Co), 
                              names_to = "Nutrient", 
                              values_to = "release_nut_pop_tot_period_sites") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_tot = round(mean(release_nut_pop_tot_period_sites),4), 
                           min_tot = round(min(release_nut_pop_tot_period_sites), 4),
                           max_tot = round(max(release_nut_pop_tot_period_sites), 4)) |>
          tidyr::pivot_longer(cols = c(mean_tot, min_tot, max_tot), 
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
                                             stringr::str_detect(level_variable, "min") ~ "min",
                                             stringr::str_detect(level_variable, "max") ~ "max",
                                             stringr::str_detect(level_variable, "percent") ~ "percent"), 
                    scat_compo = "All scat samples mixed", 
                    Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = estimate_kg) |>
      dplyr::select(c(scat_compo, Site, level, variable,  
                    Fe, Zn, Cu, Mn, Se, Co)),
    # then with scat samples separated per site 
    rbind(list_output_nut_release$CN, list_output_nut_release$PS) |>
      dplyr::select(Site, 
                    release_nut_pop_on_land_period_sites) |>
      tidyr::unnest(release_nut_pop_on_land_period_sites) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "on_land_pop_release_period_kg") |> 
      dplyr::group_by(Site, Nutrient) |>
      dplyr::summarise(mean_on_land = round(mean(on_land_pop_release_period_kg),4), 
                       min_on_land = round(min(on_land_pop_release_period_kg), 4),
                       max_on_land = round(max(on_land_pop_release_period_kg), 4)) |>
      tidyr::pivot_longer(cols = c(mean_on_land, min_on_land, max_on_land), 
                          names_to = "variable", 
                          values_to = "estimates") |>
      dplyr::bind_rows(
        rbind(list_output_nut_release$CN, list_output_nut_release$PS) |>
          dplyr::select(Site, 
                        release_nut_pop_at_sea_period_sites) |>
          tidyr::unnest(release_nut_pop_at_sea_period_sites) |>
          tidyr::pivot_longer(cols = c(Fe:Co), 
                              names_to = "Nutrient", 
                              values_to = "at_sea_pop_release_period_kg") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_at_sea = round(mean(at_sea_pop_release_period_kg),4), 
                           min_at_sea = round(min(at_sea_pop_release_period_kg), 4),
                           max_at_sea = round(max(at_sea_pop_release_period_kg), 4)) |>
          tidyr::pivot_longer(cols = c(mean_at_sea, min_at_sea, max_at_sea), 
                              names_to = "variable", 
                              values_to = "estimates")) |>
      dplyr::bind_rows(
        rbind(list_output_nut_release$CN, list_output_nut_release$PS) |>
          dplyr::select(Site, 
                        release_nut_pop_tot_period_sites) |>
          tidyr::unnest(release_nut_pop_tot_period_sites) |>
          tidyr::pivot_longer(cols = c(Fe:Co), 
                              names_to = "Nutrient", 
                              values_to = "release_nut_pop_tot_period_sites") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_tot = round(mean(release_nut_pop_tot_period_sites),4), 
                           min_tot = round(min(release_nut_pop_tot_period_sites), 4),
                           max_tot = round(max(release_nut_pop_tot_period_sites), 4)) |>
          tidyr::pivot_longer(cols = c(mean_tot, min_tot, max_tot), 
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
                                                stringr::str_detect(level_variable, "min") ~ "min",
                                                stringr::str_detect(level_variable, "max") ~ "max",
                                                stringr::str_detect(level_variable, "percent") ~ "percent"), 
                    scat_compo = "Scats separated between colonies", 
                    Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = estimate_kg) |>
      dplyr::select(c(scat_compo, Site, level, variable,  
                    Fe, Zn, Cu, Mn, Se, Co)))
  
  
  openxlsx::write.xlsx(table_summary, 
                       file = paste0("output/sites/table_summary_sites_nut_total_estimates.xlsx"))
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different habitats 
MWtest_test_nut_sites_tot_period <- function(list_output_nut_release) {
  
  table_test_scat_per_site <- rbind(list_output_nut_release$CN, 
                                    list_output_nut_release$PS) |>
    dplyr::select(Site, 
                  release_nut_pop_tot_period_sites) |>
    tidyr::unnest(release_nut_pop_tot_period_sites) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |>
    dplyr::select(Site, Nutrient, tot_pop_release_period_kg) |>
    tidyr::pivot_wider(names_from = Site,
                       values_from = tot_pop_release_period_kg,
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
  
  
  table_test_all_scats <- rbind(list_output_nut_release$CN, 
                                list_output_nut_release$PS) |>
    dplyr::select(Site, 
                  release_nut_pop_tot_period_all_scats) |>
    tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |>
    dplyr::select(Site, Nutrient, tot_pop_release_period_kg) |>
    tidyr::pivot_wider(names_from = Site,
                       values_from = tot_pop_release_period_kg,
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


#'
#'
#'
#'
#'
#' function to generate supplementary material table with all parameters summary values
table_model_param <- function(list_output_nut_release) {
  
  options(scipen = 999)
  
  table_summary_model_param <- list_output_nut_release$CN  |>
    dplyr::summarize(min = round(min(simu_count), 0),
                     `2.5_quant` = round(quantile(simu_count, probs = c(0.025)), 0),
                     mean = round(mean(simu_count), 0),
                     median = round(median(simu_count), 0),
                     `97.5_quant` = round(quantile(simu_count, probs = c(0.975)), 0),
                     max = round(max(simu_count), 0)) |>
    dplyr::mutate(Site = "Cap Noir", 
                  Parameter = "Abundance on colony") |>
    # next parameters
    dplyr::bind_rows(list_output_nut_release$PS  |>
                       dplyr::summarize(min = round(min(simu_count), 0),
                                        `2.5_quant` = round(quantile(simu_count, probs = c(0.025)), 0),
                                        mean = round(mean(simu_count), 0),
                                        median = round(median(simu_count), 0),
                                        `97.5_quant` = round(quantile(simu_count, probs = c(0.975)), 0),
                                        max = round(max(simu_count), 0)) |>
                       dplyr::mutate(Site = "Pointe Suzanne", 
                                     Parameter = "Abundance on colony"),
                     rbind(list_output_nut_release$CN, 
                           list_output_nut_release$PS) |>
                       tidyr::unnest(BM) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "Body mass (kg)"),
                     rbind(list_output_nut_release$CN, 
                           list_output_nut_release$PS) |>
                       tidyr::unnest(Beta) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "Beta"),
                     rbind(list_output_nut_release$CN, 
                           list_output_nut_release$PS) |>
                       tidyr::unnest(NRJ_diet) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "mean energy content of diet (kJ/kg)"),
                     rbind(list_output_nut_release$CN, 
                           list_output_nut_release$PS) |>
                       tidyr::unnest(dm_ration) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "% of dry matter in ration"),
                     rbind(list_output_nut_release$CN, 
                           list_output_nut_release$PS) |>
                       tidyr::unnest(dm_release) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "dry matter release rate"),
                     rbind(list_output_nut_release$CN, 
                           list_output_nut_release$PS) |>
                       tidyr::unnest(duration_of_stay) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "Duration of stay (days)"),
                     rbind(list_output_nut_release$CN, 
                           list_output_nut_release$PS) |>
                       tidyr::unnest(time_on_land) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "% of time spent on land"),
                     rbind(list_output_nut_release$CN, 
                           list_output_nut_release$PS) |>
                       tidyr::unnest(Indi_data) |>
                       tidyr::pivot_longer(cols = c(ADMR:`PercentBM`),
                                           names_to = "Parameter",
                                           values_to = "value") |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = dplyr::case_when(Parameter == "A_rate" ~ "Assimilation rate",
                                                                  Parameter == "PercentBM" ~ "% of body mass (daily ration)",
                                                                  Parameter == "Ration" ~ "Daily ration (kg)",
                                                                  Parameter == "ADMR" ~ "Average Daily Metabolic Rate (kJ)")) |>
                       dplyr::group_by(Site, Parameter) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2))
    )
  
  openxlsx::write.xlsx(table_summary_model_param,
                         file = "output/sites/table_summary_model_parameters.xlsx")
 
  
}