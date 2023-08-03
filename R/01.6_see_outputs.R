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



################################ SCAT RELATIVE COMPOSITION #####################


#'
#'
#'
#'
#'
# 
fig_nut_scat_compo_relative <- function(scat_compo_tib) {
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")))  |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarize(mean = mean(conc_relative),
                     `10_quant` = quantile(conc_relative, probs = c(0.1)),
                     `80_quant` = quantile(conc_relative, probs = c(0.8))
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
  ggplot2::ggsave("output/scat_compo_rel.jpg",
                  scale = 1,
                  height = 6, width = 9
  )
  
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")))  |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarize(mean = mean(conc_relative),
                     `10_quant` = quantile(conc_relative, probs = c(0.1)),
                     `80_quant` = quantile(conc_relative, probs = c(0.8))
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
fig_nut_scat_compo_relative_comp_pinn <- function(scat_compo_tib) {
  
  tibble::tribble(
    ~ Source, ~ Species, ~ Fe, ~ Fe_sd, ~ Zn, ~ Zn_sd, ~ Cu, ~ Cu_sd, ~ Mn, ~ Mn_sd, ~ Co, ~ Co_sd, 
    "Wing et al. 2017", "New zealand fur seals", 278.8, 57.7, 494.8, 74.0, 696.0, 228.9, 18.7, 4.4, 1.92, 0.55, 
    "Wing et al. 2017", "Hooker s sea lion", 334.0, 38.3, 318.8, 22.5, 38.1, 9.4, 24.0, 1.9, 0.24, 0.06, 
    "Wing et al. 2014", "Hooker s sea lion", 1014, 572, 228.3, 68.9, 6.0, 0.6, 29.6, 3.5, 0.39, 0.39, 
    "Wing et al. 2021", "Weddell seal", 950.8, 148.9, 270.7, 15.7, 17.2, 2.8, 28.7, 2.3, 0.73, 0.06) |>
    dplyr::mutate(sum_mean = Fe + Zn + Cu + Mn + Co) |>
    tidyr::pivot_longer(cols = c(Fe, Zn, Cu, Mn, Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_mean, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |>
    dplyr::select(c(Source, Species, Nutrient, conc_relative)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = conc_relative) |>
    dplyr::bind_rows(scat_compo_tib |>
                       tidyr::pivot_longer(cols = c(Fe:Co), 
                                           names_to = "Nutrient", 
                                           values_to = "conc_mg_kg_dw") |> 
                       dplyr::mutate(Nutrient = factor(Nutrient, 
                                                       levels = c("Fe", "Zn", 
                                                                  "Cu", "Mn", "Se",
                                                                  "Co"))) |> 
                       dplyr::group_by(Nutrient) |>
                       dplyr::summarize(mean = mean(conc_mg_kg_dw)) |>
                       tidyr::pivot_wider(names_from = Nutrient, 
                                          values_from = mean) |>
                       dplyr::mutate(sum_mean = Fe + Zn + Cu + Mn + Co, 
                                     Fe = Fe/sum_mean, 
                                     Zn = Zn/sum_mean, 
                                     Cu = Cu/sum_mean, 
                                     Mn = Mn/sum_mean,
                                     Co = Co/sum_mean,  
                                     Source = "This study", 
                                     Species = "Antarctic fur seals") |> 
                       dplyr::select(c(Source, Species, Fe, Zn, Cu, Mn, Co))) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "ratio_mean") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = ratio_mean, 
                                   fill = Source), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Species) +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "bottom")
  ggplot2::ggsave("output/scat_compo_rel_comp_pinni.jpg",
                  scale = 1,
                  height = 6, width = 9
  )
  
  tibble::tribble(
    ~ Source, ~ Species, ~ Fe, ~ Fe_sd, ~ Zn, ~ Zn_sd, ~ Cu, ~ Cu_sd, ~ Mn, ~ Mn_sd, ~ Co, ~ Co_sd, 
    "Wing et al. 2017", "New zealand fur seals", 278.8, 57.7, 494.8, 74.0, 696.0, 228.9, 18.7, 4.4, 1.92, 0.55, 
    "Wing et al. 2017", "Hooker’s sea lion", 334.0, 38.3, 318.8, 22.5, 38.1, 9.4, 24.0, 1.9, 0.24, 0.06, 
    "Wing et al. 2014", "Hooker’s sea lion", 1014, 572, 228.3, 68.9, 6.0, 0.6, 29.6, 3.5, 0.39, 0.39, 
    "Wing et al. 2021", "Weddell seal", 950.8, 148.9, 270.7, 15.7, 17.2, 2.8, 28.7, 2.3, 0.73, 0.06) |>
    dplyr::mutate(sum_mean = Fe + Zn + Cu + Mn + Co) |>
    tidyr::pivot_longer(cols = c(Fe, Zn, Cu, Mn, Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_mean, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |>
    dplyr::select(c(Source, Species, Nutrient, conc_relative)) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = conc_relative) |>
    dplyr::bind_rows(scat_compo_tib |>
                       tidyr::pivot_longer(cols = c(Fe:Co), 
                                           names_to = "Nutrient", 
                                           values_to = "conc_mg_kg_dw") |> 
                       dplyr::mutate(Nutrient = factor(Nutrient, 
                                                       levels = c("Fe", "Zn", 
                                                                  "Cu", "Mn", "Se",
                                                                  "Co"))) |> 
                       dplyr::group_by(Nutrient) |>
                       dplyr::summarize(mean = mean(conc_mg_kg_dw)) |>
                       tidyr::pivot_wider(names_from = Nutrient, 
                                          values_from = mean) |>
                       dplyr::mutate(sum_mean = Fe + Zn + Cu + Mn + Co, 
                                     Fe = Fe/sum_mean, 
                                     Zn = Zn/sum_mean, 
                                     Cu = Cu/sum_mean, 
                                     Mn = Mn/sum_mean,
                                     Co = Co/sum_mean,  
                                     Source = "This study", 
                                     Species = "Antarctic fur seals") |> 
                       dplyr::select(c(Source, Species, Fe, Zn, Cu, Mn, Co))) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "ratio_mean") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = ratio_mean, 
                                   fill = Source), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Species) +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "bottom")
  
  
}



#'
#'
#'
#'
#'
# 
fig_nut_scat_compo_relative_sites <- function(scat_compo_tib) {
  
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::group_by(site, Nutrient) |>
    dplyr::summarise(mean_conc_relative = mean(conc_relative)) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co"))) |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean_conc_relative), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ site) + 
    ggplot2::ylab("Relative proportion\nin scats") +
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
  ggplot2::ggsave("output/scat_compo_rel_comp_sites_Agazella.jpg",
                  scale = 1,
                  height = 3, width = 5
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
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Cap Noir") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Code_sample) + 
    ggplot2::ylab("Relative proportion in scats") +
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
  ggplot2::ggsave("output/scat_compo_rel_comp_CapNoir_Agazella.jpg",
                  scale = 1,
                  height = 6, width = 12
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
                  conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Pointe Suz") |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Code_sample) + 
    ggplot2::ylab("Relative proportion in scats") +
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
  ggplot2::ggsave("output/scat_compo_rel_comp_PSuz_Agazella.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  
}


#'
#'
#'
#'
#'
# 
fig_nut_scat_compo_relative_clust <- function(scat_compo_tib_clust_chap3) {
  
  
  scat_compo_tib_clust_chap3 |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Ni + Co) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::group_by(cluster, Nutrient) |>
    dplyr::summarise(mean_conc_relative = mean(conc_relative),
                     `10_quant` = quantile(conc_relative, probs = c(0.1)),
                     `80_quant` = quantile(conc_relative, probs = c(0.8))) |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", "Cu", "Mn", "Se",
                                               "Ni", "Co"))) |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean_conc_relative, 
                                   fill = cluster), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::facet_wrap(~ cluster) + 
    ggplot2::ylab("Relative proportion\nin scats") +
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
  ggplot2::ggsave("output/scat_compo_rel_comp_clust_Agazella.jpg",
                  scale = 1,
                  height = 3, width = 5
  )

  
  
}




