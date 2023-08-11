################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# 06_see_outputs.R
#
################################################################################



################################ INDIVIDUAL LEVEL ##############################

# ABSOLUTE RELEASE FOR ONE RATION 
#'
#'
#'
#'
#'
# 
fig_nut_release_ind_abs_per_diet <- function(output_tib, 
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
                                                      "2023", "2024")))  |>
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
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF", 
                                          "#B4DAE5FF", "#58A449FF")) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
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
  ggplot2::ggsave(paste0("output/barplot_ind_release_abs_per_diet_", 
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
                                                      "1999", "2000", 
                                                      "2023", "2024")))  |>
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
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF", 
                                          "#B4DAE5FF", "#58A449FF")) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
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
  
}


#'
#'
#'
#'
#'
# 
fig_nut_release_ind_abs_all_diets <- function(output_tib, 
                                         file_name) {
  output_tib |>
    tidyr::unnest(release_nut_ind) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "individual_release") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")))  |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarize(min = min(individual_release), 
                     `2.5_quant` = quantile(individual_release, probs = c(0.025)), 
                     mean = mean(individual_release), 
                     median = median(individual_release), 
                     `97.5_quant` = quantile(individual_release, probs = c(0.975)), 
                     max = max(individual_release)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`), 
                            linewidth = 1, 
                            position = ggplot2::position_dodge(1)) +
    ggplot2::ylab("Nutrient release for one daily ration (in g)") +
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
  ggplot2::ggsave(paste0("output/barplot_ind_release_abs_all_diets_", 
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
                                               "Co")))  |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarize(min = min(individual_release), 
                     `2.5_quant` = quantile(individual_release, probs = c(0.025)), 
                     mean = mean(individual_release), 
                     median = median(individual_release), 
                     `97.5_quant` = quantile(individual_release, probs = c(0.975)), 
                     max = max(individual_release)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`), 
                            linewidth = 1, 
                            position = ggplot2::position_dodge(1)) +
    ggplot2::ylab("Nutrient release for one daily ration (in g)") +
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


# RELATIVE RELEASE FOR ONE RATION 

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
                                                      "2023", "2024")), 
                  ind_release_relative = individual_release/sum_nut)  |>
    dplyr::group_by(Year_collection, Nutrient) |>
    dplyr::summarize(mean = mean(ind_release_relative),
                     `10_quant` = quantile(ind_release_relative, probs = c(0.1)),
                     `80_quant` = quantile(ind_release_relative, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF", 
                                          "#B4DAE5FF", "#58A449FF")) +
    ggplot2::facet_wrap(~ Year_collection) +
    ggplot2::ylim(c(0, 0.7)) +
    ggplot2::ylab("Relative proportion of nutrient\nreleased for one daily ration") +
    ggplot2::xlab("Year of diet assessment") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave(paste0("output/barplot_ind_release_rel_per_diet_", 
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
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000", 
                                                      "2023", "2024")), 
                  ind_release_relative = individual_release/sum_nut)  |>
    dplyr::group_by(Year_collection, Nutrient) |>
    dplyr::summarize(mean = mean(ind_release_relative),
                     `10_quant` = quantile(ind_release_relative, probs = c(0.1)),
                     `80_quant` = quantile(ind_release_relative, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF", 
                                          "#B4DAE5FF", "#58A449FF")) +
    ggplot2::facet_wrap(~ Year_collection) +
    ggplot2::ylim(c(0, 0.7)) +
    ggplot2::ylab("Relative proportion of nutrient\nreleased for one daily ration") +
    ggplot2::xlab("Year of diet assessment") +
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
fig_nut_release_ind_relative_per_diet_stack <- function(output_tib, 
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
                                                      "2023", "2024")), 
                  ind_release_relative = individual_release/sum_nut)  |>
    dplyr::group_by(Year_collection, Nutrient) |>
    dplyr::summarize(mean = mean(ind_release_relative),
                     `10_quant` = quantile(ind_release_relative, probs = c(0.1)),
                     `80_quant` = quantile(ind_release_relative, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Year_collection, y = mean, 
                                   fill = Nutrient), 
                      stat = "identity", 
                      position = ggplot2::position_stack()) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF", 
                                          "#B4DAE5FF", "#58A449FF")) +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::ylab("Relative proportion of nutrient\nreleased for one daily ration") +
    ggplot2::xlab("Year of diet assessment") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "right")
  ggplot2::ggsave(paste0("output/barplot_ind_release_rel_per_diet_stack_", 
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
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000", 
                                                      "2023", "2024")), 
                  ind_release_relative = individual_release/sum_nut)  |>
    dplyr::group_by(Year_collection, Nutrient) |>
    dplyr::summarize(mean = mean(ind_release_relative),
                     `10_quant` = quantile(ind_release_relative, probs = c(0.1)),
                     `80_quant` = quantile(ind_release_relative, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF", 
                                          "#B4DAE5FF", "#58A449FF")) +
    ggplot2::facet_wrap(~ Year_collection) +
    ggplot2::ylim(c(0, 0.7)) +
    ggplot2::ylab("Relative proportion of nutrient\nreleased for one daily ration") +
    ggplot2::xlab("Year of diet assessment") +
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
    dplyr::summarize(mean = mean(ind_release_relative),
                     `10_quant` = quantile(ind_release_relative, probs = c(0.1)),
                     `80_quant` = quantile(ind_release_relative, probs = c(0.8))
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
  ggplot2::ggsave(paste0("output/barplot_ind_release_rel_all_diets_", 
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
    dplyr::summarize(mean = mean(ind_release_relative),
                     `10_quant` = quantile(ind_release_relative, probs = c(0.1)),
                     `80_quant` = quantile(ind_release_relative, probs = c(0.8))
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

# RELATIVE CONSUMPTION IN ONE RATION 


#'
#'
#'
#'
#'
# 
fig_nut_conso_ind_relative_per_diet <- function(output_tib, 
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
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000", 
                                                      "2023", "2024")),
                  ind_conso_relative = individual_conso/sum_nut)  |>
    dplyr::group_by(Year_collection, Nutrient) |>
    dplyr::summarize(mean = mean(ind_conso_relative),
                     `10_quant` = quantile(ind_conso_relative, probs = c(0.1)),
                     `80_quant` = quantile(ind_conso_relative, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF", 
                                          "#B4DAE5FF", "#58A449FF")) +
    ggplot2::facet_wrap(~ Year_collection) +
    ggplot2::ylim(c(0, 0.7)) +
    ggplot2::ylab("Relative proportion of nutrient in daily ration") +
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
  ggplot2::ggsave(paste0("output/barplot_ind_conso_rel_per_diet_", 
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
                  Year_collection = factor(Year_collection, 
                                           levels = c("1994", "1998", 
                                                      "1999", "2000", 
                                                      "2023", "2024")),
                  ind_conso_relative = individual_conso/sum_nut)  |>
    dplyr::group_by(Year_collection, Nutrient) |>
    dplyr::summarize(mean = mean(ind_conso_relative),
                     `10_quant` = quantile(ind_conso_relative, probs = c(0.1)),
                     `80_quant` = quantile(ind_conso_relative, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean, 
                                   fill = Year_collection), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF", 
                                          "#D8AF39FF", "#403369FF", 
                                          "#B4DAE5FF", "#58A449FF")) +
    ggplot2::facet_wrap(~ Year_collection) +
    ggplot2::ylim(c(0, 0.7)) +
    ggplot2::ylab("Relative proportion of nutrient in daily ration") +
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
    dplyr::summarize(mean = mean(ind_conso_relative),
                     `10_quant` = quantile(ind_conso_relative, probs = c(0.1)),
                     `80_quant` = quantile(ind_conso_relative, probs = c(0.8))
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
  ggplot2::ggsave(paste0("output/barplot_ind_conso_rel_all_diets_", 
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
    dplyr::summarize(mean = mean(ind_conso_relative),
                     `10_quant` = quantile(ind_conso_relative, probs = c(0.1)),
                     `80_quant` = quantile(ind_conso_relative, probs = c(0.8))
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




################################ POPULATION LEVEL ##############################

#'
#'
#'
#'
#'
# 
fig_nut_release_pop_per_diet <- function(output_tib, 
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
                                                      "2023", "2024")))  |>
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
                                          "#B4DAE5FF", "#58A449FF")) +
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
                                                      "1999", "2000", 
                                                      "2023", "2024")))  |>
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
                                          "#B4DAE5FF", "#58A449FF")) +
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
  
}





#'
#'
#'
#'
#'
# 
tab_nut_release_pop_per_diet <- function(output_tib) {
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
                                                      "2023", "2024")))  |>
    dplyr::group_by(Source, Year_collection, Nutrient) |>
    dplyr::summarize(tot_pop_release = sum(individual_release)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = tot_pop_release)
  
}







