################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 02.3_compute_dm_produced.R
#
################################################################################

#'
#'
#'
#'
#
dm_per_site_period <- function(output_dm_tib) {
  
  output_dm_tib |>
    dplyr::group_by(Site) |>
    tidyr::pivot_longer(cols = c(release_dm_pop_tot_period, 
                                 release_dm_pop_on_land_period, 
                                 release_dm_pop_at_sea_period), 
                        names_to = "Location", 
                        values_to = "dm_released_kg") |>
    dplyr::mutate(Location = factor(
      dplyr::case_when(Location == "release_dm_pop_tot_period" ~ "Total (at sea & on land)", 
                       Location == "release_dm_pop_on_land_period" ~ "On land",
                       Location == "release_dm_pop_at_sea_period" ~ "At sea"), 
      levels = c("On land", "At sea", "Total (at sea & on land)")),
      Site = factor(
        dplyr::case_when(Site == "Cap Noir" ~ "Cap\nNoir", 
                         Site == "Pointe Suzanne" ~ "Pointe\nSuzanne"), 
        levels = c("Cap\nNoir", "Pointe\nSuzanne"))) |>
    dplyr::group_by(Site, Location) |>
    dplyr::summarise(min = min(dm_released_kg), 
                     mean = mean(dm_released_kg),
                     `10_quant` = quantile(dm_released_kg, 
                                           probs = c(0.1)),
                     `80_quant` = quantile(dm_released_kg, 
                                           probs = c(0.8)), 
                     max = max(dm_released_kg)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Site, y = mean, 
                                   fill = Site), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Site, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF")) +
    ggplot2::facet_wrap(~ Location, scales = "free_y") +
    ggplot2::ylab("Fecal dry matter released during\nbreeding and moulting period") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/barplot_dm_release_tot_pop_sites.jpg",
                  scale = 1,
                  height = 4, width = 9
  )
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different habitats 
MWtest_test_dm_sites_tot_period <- function(output_dm_tib) {
  
  table_test <- output_dm_tib |>
    dplyr::select(Site, release_dm_pop_tot_period) |>
    dplyr::group_by(Site) |>
    tidyr::unnest(release_dm_pop_tot_period) |>
    dplyr::select(Site, release_dm_pop_tot_period) |>
    tidyr::pivot_wider(names_from = Site,
                       values_from = release_dm_pop_tot_period,
                       values_fn = list) |>
    tidyr::unnest(cols = c(`Cap Noir`, `Pointe Suzanne`)) |>
    dplyr::mutate(t_PS_CN = dplyr::case_when(`Pointe Suzanne` > `Cap Noir` ~ 1,
                                                 TRUE ~ 0)
    ) |>
    dplyr::summarise(test_dm_sites = mean(t_PS_CN))
  
  openxlsx::write.xlsx(table_test, 
                       file = paste0("output/sites/test_differences_sites_dm.xlsx"))
  
}
