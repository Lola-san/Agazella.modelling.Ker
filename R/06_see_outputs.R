################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# 06_see_outputs.R
#
################################################################################


#'
#'
#'
#'
#'
# 
fig_nut_release_ind_diets <- function(output_tib) {
  output_tib |>
    tidyr::unnest(release_nut_ind) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                   levels = c("Fe", "Zn", 
                                              "Cu", "Mn", "Se",
                                              "Co")), 
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000")))  |>
    dplyr::group_by(Source, Year_collection, Nutrient) |>
    dplyr::summarize(min = min(individual_release), 
                     `2.5_quant` = quantile(individual_release, probs = c(0.025)), 
                     mean = mean(individual_release), 
                     median = median(individual_release), 
                     `97.5_quant` = quantile(individual_release, probs = c(0.975)), 
                     max = max(individual_release)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Year_collection, y = mean, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Year_collection, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`), 
                            linewidth = 1, 
                            position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y")
  
}


#'
#'
#'
#'
#'
# 
fig_nut_release_pop_diets <- function(output_tib, 
                                      file_name) {
  output_tib |>
    tidyr::unnest(release_nut_ind) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000", 
                                                      "2023")))  |>
    dplyr::group_by(Source, Year_collection, Nutrient) |>
    dplyr::summarize(tot_pop_release = sum(individual_release)*1e-3 # from mg to g
                     ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Year_collection, y = tot_pop_release, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF", 
                                          "#B4DAE5FF")) +
    ggplot2::ggtitle("For a population of 1000 individuals") +
    ggplot2::ylab("Nutrient release for one daily ration (in g)") +
    ggplot2::xlab("Year of diet assessment") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/barplot_pop_release_", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 9
  )
  
  
  output_tib |>
    tidyr::unnest(release_nut_ind) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000")))  |>
    dplyr::group_by(Source, Year_collection, Nutrient) |>
    dplyr::summarize(tot_pop_release = sum(individual_release)*1e-3) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Year_collection, y = tot_pop_release, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF")) +
    ggplot2::ylab("For a population of 1000 individuals)") +
    ggplot2::ylab("Nutrient release for\none daily ration (in g)") +
    ggplot2::xlab("Year of diet assessment") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
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
fig_nut_release_ind_relative_all_diets <- function(output_tib, 
                                      file_name) {
  output_tib |>
    dplyr::select(-c(conso_nut_ind, Indi_data, 
                     nut_release_rate, nut_diet_sp, 
                     nut_diet_full)) |>
    tidyr::unnest(release_nut_ind) |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  ind_release_relative = individual_release/sum_nut)  |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(ind_release_relative_norm = (ind_release_relative - min(ind_release_relative))/
                    (max(ind_release_relative) - min(ind_release_relative))) |>
    dplyr::summarize(mean = mean(ind_release_relative_norm),
                     `10_quant` = quantile(ind_release_relative_norm, probs = c(0.1)),
                     `80_quant` = quantile(ind_release_relative_norm, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                    ymin = `10_quant`, 
                                    ymax = `80_quant`)) +
    ggplot2::ylab("Relative proportion of nutrient released for one daily ration") +
    ggplot2::xlab("Nutrient") +
    ggplot2::ylim(c(0,1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/barplot_ind_release_rel_norm_", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 9
  )
  
  
  output_tib |>
    dplyr::select(-c(conso_nut_ind, Indi_data, 
                     nut_release_rate, nut_diet_sp, 
                     nut_diet_full)) |>
    tidyr::unnest(release_nut_ind) |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  ind_release_relative = individual_release/sum_nut)  |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(ind_release_relative_norm = (ind_release_relative - min(ind_release_relative))/
                    (max(ind_release_relative) - min(ind_release_relative))) |>
    dplyr::summarize(mean = mean(ind_release_relative_norm),
                     `10_quant` = quantile(ind_release_relative_norm, probs = c(0.1)),
                     `80_quant` = quantile(ind_release_relative_norm, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::ylab("Relative proportion of nutrient released for one daily ration") +
    ggplot2::xlab("Nutrient") +
    ggplot2::ylim(c(0,1)) +
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
fig_nut_release_ind_relative_per_diet <- function(output_tib, 
                                                   file_name) {
  output_tib |>
    dplyr::select(-c(conso_nut_ind, Indi_data, 
                     nut_release_rate, nut_diet_sp, 
                     nut_diet_full)) |>
    tidyr::unnest(release_nut_ind) |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000", 
                                                      "2023")), 
                  ind_release_relative = individual_release/sum_nut)  |>
    dplyr::group_by(Year_collection, Nutrient) |>
    dplyr::mutate(ind_release_relative_norm = (ind_release_relative - min(ind_release_relative))/
                    (max(ind_release_relative) - min(ind_release_relative))) |>
    dplyr::summarize(mean = mean(ind_release_relative_norm),
                     `10_quant` = quantile(ind_release_relative_norm, probs = c(0.1)),
                     `80_quant` = quantile(ind_release_relative_norm, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Year_collection, y = mean, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Year_collection, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF", 
                                          "#B4DAE5FF")) +
    ggplot2::facet_wrap(~ Nutrient) +
    ggplot2::ylab("Relative proportion of nutrient released for one daily ration") +
    ggplot2::xlab("Year of diet assessment") +
    ggplot2::ylim(c(0,1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/barplot_ind_release_rel_norm_per_diet_", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 10
  )
  
  
  output_tib |>
    dplyr::select(-c(conso_nut_ind, Indi_data, 
                     nut_release_rate, nut_diet_sp, 
                     nut_diet_full)) |>
    tidyr::unnest(release_nut_ind) |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  ind_release_relative = individual_release/sum_nut)  |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(ind_release_relative_norm = (ind_release_relative - min(ind_release_relative))/
                    (max(ind_release_relative) - min(ind_release_relative))) |>
    dplyr::summarize(mean = mean(ind_release_relative_norm),
                     `10_quant` = quantile(ind_release_relative_norm, probs = c(0.1)),
                     `80_quant` = quantile(ind_release_relative_norm, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::ylab("Relative proportion of nutrient released for one daily ration") +
    ggplot2::xlab("Nutrient") +
    ggplot2::ylim(c(0,1)) +
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
fig_nut_conso_ind_relative_all_diets <- function(output_tib, 
                                                   file_name) {
  output_tib |>
    dplyr::select(-c(release_nut_ind, Indi_data, 
                     nut_release_rate, nut_diet_sp, 
                     nut_diet_full)) |>
    tidyr::unnest(conso_nut_ind) |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_conso") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  ind_conso_relative = individual_conso/sum_nut)  |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(ind_conso_relative_norm = (ind_conso_relative - min(ind_conso_relative))/
                    (max(ind_conso_relative) - min(ind_conso_relative))) |>
    dplyr::summarize(mean = mean(ind_conso_relative_norm),
                     `10_quant` = quantile(ind_conso_relative_norm, probs = c(0.1)),
                     `80_quant` = quantile(ind_conso_relative_norm, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::ylab("Relative proportion of nutrient in daily ration") +
    ggplot2::xlab("Nutrient") +
    ggplot2::ylim(c(0,1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/barplot_ind_conso_rel_norm_", 
                         file_name, ".jpg"),
                  scale = 1,
                  height = 6, width = 9
  )
  
  
  output_tib |>
    dplyr::select(-c(release_nut_ind, Indi_data, 
                     nut_release_rate, nut_diet_sp, 
                     nut_diet_full)) |>
    tidyr::unnest(conso_nut_ind) |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_conso") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  ind_conso_relative = individual_conso/sum_nut)  |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(ind_conso_relative_norm = (ind_conso_relative - min(ind_conso_relative))/
                    (max(ind_conso_relative) - min(ind_conso_relative))) |>
    dplyr::summarize(mean = mean(ind_conso_relative_norm),
                     `10_quant` = quantile(ind_conso_relative_norm, probs = c(0.1)),
                     `80_quant` = quantile(ind_conso_relative_norm, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::ylab("Relative proportion of nutrient in daily ration") +
    ggplot2::xlab("Nutrient") +
    ggplot2::ylim(c(0,1)) +
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
fig_nut_scat_compo_relative_norm <- function(scat_compo_tib) {
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  conc_relative = conc_mg_kg_dw/sum_nut)  |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_relative_norm = (conc_relative - min(conc_relative))/
                    (max(conc_relative) - min(conc_relative))) |>
    dplyr::summarize(mean = mean(conc_relative_norm),
                     `10_quant` = quantile(conc_relative_norm, probs = c(0.1)),
                     `80_quant` = quantile(conc_relative_norm, probs = c(0.8))
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
  ggplot2::ggsave("output/scat_compo_rel_norm.jpg",
                  scale = 1,
                  height = 6, width = 9
  )
  
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  conc_relative = conc_mg_kg_dw/sum_nut)  |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(conc_relative_norm = (conc_relative - min(conc_relative))/
                    (max(conc_relative) - min(conc_relative))) |>
    dplyr::summarize(mean = mean(conc_relative_norm),
                     `10_quant` = quantile(conc_relative_norm, probs = c(0.1)),
                     `80_quant` = quantile(conc_relative_norm, probs = c(0.8))
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
tab_nut_release_pop_diets <- function(output_tib) {
  output_tib |>
    tidyr::unnest(release_nut_ind) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000")))  |>
    dplyr::group_by(Source, Year_collection, Nutrient) |>
    dplyr::summarize(tot_pop_release = sum(individual_release)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = tot_pop_release)
  
}