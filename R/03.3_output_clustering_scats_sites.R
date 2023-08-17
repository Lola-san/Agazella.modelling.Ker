################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 03.2_output_clustering_scats_sites.R
#
################################################################################


################ 1 - USING THE FULL COMPOSITIONAL DATA #########################
# as in contrast to using Principal components estimated by PCA 
# to reduce dimensions before conducting the clustering

#'
#'
#'
#'
# function to plot dendrogram for fish and scats based on PC results of robust PCA
clust_compo_dendro_full_tib <- function(clust_full_tib_output,
                                        scat_compo_tib,
                                        type # "sites" if one/site or "all" if all together
) {
  
  if (type == "sites") {
    #################################### CAP NOIR ##############################
    scat_compo_tib_CN <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "CN"))
    
    clust_output_CN <- clust_full_tib_output$CN
    
    # dendrogram
    tree.data_CN <- clust_output_CN$dtree
    
    # change labels to sample code 
    tree.data_CN$labels <- scat_compo_tib_CN$Code_sample
    
    # JPEG device
    jpeg("output/sites/clustering with all nutrients/dendrogram_all_nut_CN.jpg", quality = 85)
    
    plot(tree.data_CN)
    
    # Close device
    dev.off()
    
    ############################## POINTE SUZANNE ##############################
    scat_compo_tib_PS <- scat_compo_tib |>
      dplyr::filter(stringr::str_detect(Code_sample, "PS"))
    
    clust_output_PS <- clust_full_tib_output$PS
    
    # dendrogram
    tree.data_PS <- clust_output_PS$dtree
    
    # change labels to sample code and add colour grouping
    tree.data_PS$labels <- scat_compo_tib_PS$Code_sample
    
    # JPEG device
    jpeg("output/sites/clustering with all nutrients/dendrogram_all_nut_PS.jpg", quality = 85)
    
    plot(tree.data_PS)
    
    # Close device
    dev.off()
    
    
  } else if (type == "all") {
    
    # dendrogram
    tree.data <- clust_full_tib_output$dtree
    
    # change labels to sample code and add colour grouping
    tree.data$labels <- scat_compo_tib$Code_sample
    
    # JPEG device
    jpeg("output/sites/clustering with all nutrients/dendrogram_all_nut_all_scats.jpg", quality = 85)
    
    plot(tree.data)
    
    # Close device
    dev.off()
  }
  
  
}



#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
boxplot_compo_clust_full_tib <- function(clust_full_tib_output,
                                         scat_compo_tib, 
                                         type # "sites" if one/site or "all" if all together
) {
  
  if (type == "sites") {
    
    ############################### CAP NOIR #####################################
    # assign each sample to its cluster
    clust_vec_CN <- clust_full_tib_output$CN$cluster
    
    mean_conc_table_CN <- scat_compo_tib |>
      dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                            stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
      dplyr::filter(site == "Cap Noir") |>
      dplyr::mutate(cluster = clust_vec_CN) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      dplyr::group_by(Nutrient) |>
      dplyr::summarise(mean_conc = mean(conc_mg_kg_dw), 
                       median_conc = median(conc_mg_kg_dw))
    
    scat_compo_tib |>
      dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                            stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
      dplyr::filter(site == "Cap Noir") |>
      dplyr::mutate(cluster = clust_vec_CN) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co")), 
                    y_lim = dplyr::case_when(Nutrient == "P" ~ 142000,
                                             Nutrient == "Fe" ~ 18000, 
                                             Nutrient == "Zn" ~ 1175, 
                                             Nutrient == "Cu" ~ 850, 
                                             Nutrient == "Mn" ~ 420, 
                                             Nutrient == "Se" ~ 135, 
                                             Nutrient == "Co" ~ 14)) |> 
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                         fill = factor(cluster)), 
                            position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Nutrient,
                          nrow = 2, scales = "free_y") + 
      ggplot2::geom_hline(data = mean_conc_table_CN, 
                          ggplot2::aes(yintercept = mean_conc), 
                          linetype = "dashed",
                          color = "darkred") +
      ggplot2::geom_hline(data = mean_conc_table_CN,
                          ggplot2::aes(yintercept = median_conc), 
                          color = "darkred") +
      ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
      ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                            "2" = "#1D2645FF",
                                            "3" = "#D8AF39FF", 
                                            "4" = "#AE93BEFF")) +
      ggplot2::ggtitle("Cap Noir") +
      ggplot2::ylab("Absolute concentration in scats\n(mg per kg dry weight)") +
      ggplot2::xlab("Cluster") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     strip.text.x = ggplot2::element_text(size = 15),
                     legend.position = "none")
    ggplot2::ggsave("output/sites/clustering with all nutrients/clust_scat_compo_abs_all_nut_CapNoir.jpg",
                    scale = 1,
                    height = 5, width = 9
    )
    
    
    ######################### POINTE SUZANNE #####################################
    
    clust_vec_PS <- clust_full_tib_output$PS$cluster
    
    mean_conc_table_PS <- scat_compo_tib |>
      dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                            stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
      dplyr::filter(site == "Pointe Suz") |>
      dplyr::mutate(cluster = clust_vec_PS) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      dplyr::group_by(Nutrient) |>
      dplyr::summarise(mean_conc = mean(conc_mg_kg_dw), 
                       median_conc = median(conc_mg_kg_dw))
    
    
    scat_compo_tib |>
      dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                            stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
      dplyr::filter(site == "Pointe Suz") |>
      dplyr::mutate(cluster = clust_vec_PS) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co")), 
                    y_lim = dplyr::case_when(Nutrient == "P" ~ 142000,
                                             Nutrient == "Fe" ~ 18000, 
                                            Nutrient == "Zn" ~ 1175, 
                                            Nutrient == "Cu" ~ 850, 
                                            Nutrient == "Mn" ~ 420, 
                                            Nutrient == "Se" ~ 135, 
                                            Nutrient == "Co" ~ 14)) |> 
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                         fill = factor(cluster)), 
                            position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Nutrient, 
                          nrow = 2, scales = "free_y") + 
      ggplot2::geom_hline(data = mean_conc_table_PS, 
                          ggplot2::aes(yintercept = mean_conc),
                          linetype = "dashed", 
                          color = "darkred") +
      ggplot2::geom_hline(data = mean_conc_table_PS,
                          ggplot2::aes(yintercept = median_conc), 
                          color = "darkred") +
      ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
      ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                            "2" = "#1D2645FF",
                                            "3" = "#D8AF39FF", 
                                            "4" = "#AE93BEFF")) +
      ggplot2::ggtitle("Pointe Suzanne") +
      ggplot2::ylab("Absolute concentration in scats\n(mg per kg dry weight)") +
      ggplot2::xlab("Cluster") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     strip.text.x = ggplot2::element_text(size = 15),
                     legend.position = "none")
    ggplot2::ggsave("output/sites/clustering with all nutrients/clust_scat_compo_abs_all_nut_PSuzanne.jpg",
                    scale = 1,
                    height = 5, width = 9
    )
    
  } else if (type == "all") {
    
    # assign each sample to its cluster
    clust_vec <- clust_full_tib_output$cluster
    
    mean_conc_table <- scat_compo_tib |>
      dplyr::mutate(cluster = clust_vec) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      dplyr::group_by(Nutrient) |>
      dplyr::summarise(mean_conc = mean(conc_mg_kg_dw), 
                       median_conc = median(conc_mg_kg_dw))
    
    scat_compo_tib |>
      dplyr::mutate(cluster = clust_vec) |>
      tidyr::pivot_longer(cols = c(P:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("P", "Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                         fill = factor(cluster)), 
                            position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Nutrient, scales = "free_y") + 
      ggplot2::geom_hline(data = mean_conc_table, 
                          ggplot2::aes(yintercept = mean_conc), 
                          color = "darkred") +
      ggplot2::geom_hline(data = mean_conc_table,
                          ggplot2::aes(yintercept = median_conc),
                          linetype = "dashed", 
                          color = "darkred") +
      ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                            "2" = "#1D2645FF",
                                            "3" = "#D8AF39FF", 
                                            "4" = "#AE93BEFF")) +
      ggplot2::ggtitle("All scats") +
      ggplot2::ylab("Absolute concentration in scats (mg per kg dry weight)") +
      ggplot2::xlab("Cluster") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     strip.text.x = ggplot2::element_text(size = 15),
                     legend.position = "none")
    ggplot2::ggsave("output/sites/clustering with all nutrients/clust_scat_compo_abs_all_nut_all_scats.jpg",
                    scale = 1,
                    height = 8, width = 12
    )
    
  }
  
  
}


#'
#'
#'
#'
#'
# 
barplot_nut_scat_compo_relative_clust <- function(clust_full_tib_output,
                                                  scat_compo_tib,  
                                                  type # "sites" if one/site or "all" if all together
                                                  ) {
  
  if (type == "sites") {
  # Cap Noir
  clust_vec_CN <- clust_full_tib_output$CN$cluster

  scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Cap Noir") |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co, 
                  cluster = clust_vec_CN) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  conc_relative = conc_mg_kg_dw/sum_nut) |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative, 
                                   fill = factor(cluster)), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Code_sample) + 
    ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                          "2" = "#1D2645FF",
                                          "3" = "#D8AF39FF", 
                                          "4" = "#AE93BEFF")) +
    ggplot2::ggtitle("Cap Noir") +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/clustering with all nutrients/scat_compo_rel_comp__with clusters_CapNoir_Agazella.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  # Pointe Suzanne
  clust_vec_PS <- clust_full_tib_output$PS$cluster
  
  scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Pointe Suz") |>
    dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co, 
                  cluster = clust_vec_PS) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Fe", "Zn", 
                                               "Cu", "Mn", "Se",
                                               "Co")), 
                  conc_relative = conc_mg_kg_dw/sum_nut) |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative, 
                                   fill = factor(cluster)), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Code_sample) + 
    ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                          "2" = "#1D2645FF",
                                          "3" = "#D8AF39FF", 
                                          "4" = "#AE93BEFF")) +
    ggplot2::ggtitle("Pointe Suzanne") +
    ggplot2::ylab("Relative proportion in scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14),
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "none")
  ggplot2::ggsave("output/sites/clustering with all nutrients/scat_compo_rel_comp_with_clusters_PSuz_Agazella.jpg",
                  scale = 1,
                  height = 6, width = 12
  )
  
  
  } else if (type == "all") {
    clust_vec_all <- clust_full_tib_output$cluster
    
    scat_compo_tib |>
      dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co, 
                  cluster = clust_vec_all) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co")), 
                    conc_relative = conc_mg_kg_dw/sum_nut
                    ) |> 
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative, 
                                     fill = factor(cluster)),
                        stat = "identity", 
                        position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Code_sample) + 
      ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                            "2" = "#1D2645FF",
                                            "3" = "#D8AF39FF", 
                                            "4" = "#AE93BEFF")) +
      ggplot2::ylab("Relative proportion\nin scats") +
      ggplot2::xlab("Nutrient") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                     axis.text.y = ggplot2::element_text(size = 14),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     strip.text.x = ggplot2::element_blank(),
                     legend.position = "none")
    ggplot2::ggsave("output/sites/clustering with all nutrients/scat_compo_rel_with_clusters_all_scats.jpg",
                    scale = 1,
                    height = 6, width = 12
    )
    
    
  }
  
  
}


#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
table_stats_clust_per_site_full_tib <- function(list_res_clust_sites_full_tib,
                                                scat_compo_tib
) {
  
  clust_vec_CN <- list_res_clust_sites_full_tib$CN$cluster
  clust_vec_PS <- list_res_clust_sites_full_tib$PS$cluster
  
  table <- rbind(scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Cap Noir") |>
                   dplyr::mutate(cluster = clust_vec_CN, 
                                 ntot = dplyr::n_distinct(Code_sample)) |>
                   tidyr::pivot_longer(cols = c(P:Co), 
                                       names_to = "Nutrient", 
                                       values_to = "conc_mg_kg_dw") |>
                   dplyr::mutate(Nutrient = factor(Nutrient, 
                                                   levels = c("P", "Fe", "Zn", "Cu",
                                                              "Mn", "Se", "Co"))) |>
                   dplyr::group_by(site, cluster, Nutrient) |>
                   dplyr::summarise(mean = round(mean(conc_mg_kg_dw), 3), 
                                    median = round(median(conc_mg_kg_dw), 3),
                                    sd = round(sd(conc_mg_kg_dw), 3),
                                    n = dplyr::n_distinct(Code_sample), 
                                    clust_ratio = round(100*(n/ntot), 1)) |>
                   dplyr::distinct() |>
                   tidyr::pivot_wider(names_from = Nutrient, 
                                      values_from = c(mean, median,  sd), 
                                      names_sep = "_", 
                                      names_sort = TRUE), 
                 scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Pointe Suzanne") |>
                   dplyr::mutate(cluster = clust_vec_PS, 
                                 ntot = dplyr::n_distinct(Code_sample)) |>
                   tidyr::pivot_longer(cols = c(P:Co), 
                                       names_to = "Nutrient", 
                                       values_to = "conc_mg_kg_dw") |>
                   dplyr::mutate(Nutrient = factor(Nutrient, 
                                                 levels = c("P", "Fe", "Zn", "Cu",
                                                            "Mn", "Se", "Co"))) |>
                   dplyr::group_by(site, cluster, Nutrient) |>
                   dplyr::summarise(mean = round(mean(conc_mg_kg_dw), 3), 
                                    median = round(median(conc_mg_kg_dw), 3),
                                    sd = round(sd(conc_mg_kg_dw), 3),
                                    n = dplyr::n_distinct(Code_sample), 
                                    clust_ratio = round(100*(n/ntot), 1)) |>
                   dplyr::distinct() |>
                   tidyr::pivot_wider(names_from = Nutrient, 
                                      values_from = c(mean, median,  sd), 
                                      names_sep = "_", 
                                      names_sort = TRUE))
  
  openxlsx::write.xlsx(table, 
                       file = "output/sites/clustering with all nutrients/clust_all_nut_compo_sites.xlsx")
  
  table 
  
}





#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different clusters 
MWtest_clust_k43_full_tib <- function(list_res_clust_full_tib_sites,
                                      scat_compo_tib) {
  
  # assign each sample to its cluster
  clust_vec_CN <- list_res_clust_full_tib_sites$CN$cluster
  clust_vec_PS <- list_res_clust_full_tib_sites$PS$cluster
  
  scat_compo_tib_CN <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Cap Noir") |>
    dplyr::mutate(cluster = clust_vec_CN) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  scat_compo_tib_PS <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Pointe Suzanne") |>
    dplyr::mutate(cluster = clust_vec_PS) |>
    tidyr::pivot_longer(cols = c(P:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  
  nut_vec <- unique(scat_compo_tib_CN$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table_CN <- scat_compo_tib_CN |>
      dplyr::filter(Nutrient == nut)
    
    table_PS <- scat_compo_tib_PS |>
      dplyr::filter(Nutrient == nut)
    
    table_CN$cluster <- factor(table_CN$cluster)
    table_PS$cluster <- factor(table_PS$cluster)
    
    table_CN <- table_CN |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_CN <- na.omit(table_CN$`1`)
    clust2_CN <- na.omit(table_CN$`2`)
    clust3_CN <- na.omit(table_CN$`3`)
    clust4_CN <- na.omit(table_CN$`4`)
    
    table_PS <- table_PS |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_PS <- na.omit(table_PS$`1`)
    clust2_PS <- na.omit(table_PS$`2`)
    clust3_PS <- na.omit(table_PS$`3`)
    
    nut_test <- rbind(data.frame(Site = "Cap Noir",
                                 Nutrient = rep(nut, 6), 
                                 Cluster_comp_1 = c("1", "1", "1",
                                                    "2", "2", 
                                                    "3"), 
                                 Cluster_comp_2 = c("2", "3", "4",
                                                    "3", "4", 
                                                    "4"), 
                                 alpha_MW = c(wilcox.test(clust1_CN, clust2_CN)[[3]],
                                              wilcox.test(clust1_CN, clust3_CN)[[3]],
                                              wilcox.test(clust1_CN, clust4_CN)[[3]],
                                              
                                              wilcox.test(clust2_CN, clust3_CN)[[3]],
                                              wilcox.test(clust2_CN, clust4_CN)[[3]],
                                              
                                              wilcox.test(clust3_CN, clust4_CN)[[3]])), 
                      data.frame(Site = "Pointe Suzanne",
                                 Nutrient = rep(nut, 3), 
                                 Cluster_comp_1 = c("1", "1", 
                                                    "2"), 
                                 Cluster_comp_2 = c("2", "3", 
                                                    "3"), 
                                 alpha_MW = c(wilcox.test(clust1_PS, clust2_PS)[[3]],
                                              wilcox.test(clust1_PS, clust3_PS)[[3]],
                                              
                                              wilcox.test(clust2_PS, clust3_PS)[[3]])))
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Site = NA, 
                        Nutrient = NA, 
                        Cluster_comp_1 = NA,
                        Cluster_comp_2 = NA,
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,] |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = alpha_MW)
  
  
  openxlsx::write.xlsx(df_test, 
                       file = "output/sites/clustering with all nutrients/Mann_Whitney_test_clust_all_nut_compo_sites.xlsx")
  
  
}


################ 2 - STARTING WITH A PCA TO REDUCE DIMENSIONS ##################
########################## BEFORE CLUSTERING ###################################


#'
#'
#'
#'
# function to plot dendrogram for scats based on PC results of robust PCA
clust_dendro_scats <- function(res_pca, 
                               scat_compo_tib, 
                               type, # "sites" if one/site or "all" if all together 
                               method, 
                               pcomp = c(1, 2),
                               k = c(2, 2, 2)  # first CN then PS, then "all"
) {
  
  if (type == "sites") {
    res_pca_CN <- res_pca$CN
    res_pca_PS <- res_pca$PS
    
    ########################### CAP NOIR #########################################
    scat_compo_tib_CN <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "CN"))
    
    # extract the data i.e coordinates of individuals on the PCs
    data.act_CN <- as.data.frame(res_pca_CN$scores[, pcomp])
    
    # define distance matrix
    d_CN <- dist(data.act_CN)
    
    # perform clustering
    tree_CN <- stats::hclust(d_CN, method = method)
    
    # dendrogram
    dendro.dat_CN <- ggdendro::dendro_data(tree_CN, 
                                           type = "rectangle")
    
    # cut the tree in k clusters and save output in a df
    clust_output_CN <- data.frame(cluster = stats::cutree(tree = tree_CN, k = k[1]))
    
    # change labels to species name and add colour grouping
    dendro.labels_CN <- dendro.dat_CN$labels |>
      dplyr::mutate(label = scat_compo_tib_CN$Code_sample[tree_CN$order], 
                    cluster = factor(clust_output_CN$cluster[tree_CN$order])) 
    
    # plot dendrogram
    ggplot2::ggplot(dendro.dat_CN$segments) + 
      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend))+
      ggplot2::geom_text(data = dendro.labels_CN, 
                         ggplot2::aes(x, y, 
                                      label = label, 
                                      colour = cluster),
                         hjust = 1, size = 4) +
      ggplot2::scale_color_manual(values = c("1" = "#44A57CFF",
                                             "2" = "#1D2645FF",
                                             "3" = "#D8AF39FF", 
                                             "4" = "#AE93BEFF")) +
      ggplot2::coord_flip() +
      ggplot2::ylim(-2, 25) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Cluster",
                                                     override.aes = list(shape = 12)))  +
      ggplot2::theme_classic()
    
    # save plot 
    ggplot2::ggsave("output/sites/clustering with PCs/dendrogram_clust_PCs_CN.jpg",
                    scale = 1,
                    height = 7, width = 8)
    
    
    ########################### POINTE SUZANNE #################################
    scat_compo_tib_PS <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "PS"))
    
    # extract the data i.e coordinates of individuals on the PCs
    data.act_PS <- as.data.frame(res_pca_PS$scores[, pcomp])
    
    # define distance matrix
    d_PS <- dist(data.act_PS)
    
    # perform clustering
    tree_PS <- stats::hclust(d_PS, method = method)
    
    # dendrogram
    dendro.dat_PS <- ggdendro::dendro_data(tree_PS, 
                                           type = "rectangle")
    
    # cut the tree in k clusters and save output in a df
    clust_output_PS <- data.frame(cluster = stats::cutree(tree = tree_PS, k = k[2]))
    
    # change labels to species name and add colour grouping
    dendro.labels_PS <- dendro.dat_PS$labels |>
      dplyr::mutate(label = scat_compo_tib_PS$Code_sample[tree_PS$order], 
                    cluster = factor(clust_output_PS$cluster[tree_PS$order])) 
    
    # plot dendrogram
    ggplot2::ggplot(dendro.dat_PS$segments) + 
      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend))+
      ggplot2::geom_text(data = dendro.labels_PS, 
                         ggplot2::aes(x, y, 
                                      label = label, 
                                      colour = cluster),
                         hjust = 1, size = 4) +
      ggplot2::scale_color_manual(values = c("1" = "#44A57CFF",
                                             "2" = "#1D2645FF",
                                             "3" = "#D8AF39FF", 
                                             "4" = "#AE93BEFF")) +
      ggplot2::coord_flip() +
      ggplot2::ylim(-2, 25) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Cluster",
                                                     override.aes = list(shape = 12)))  +
      ggplot2::theme_classic()
    
    # save plot 
    ggplot2::ggsave("output/sites/clustering with PCs/dendrogram_clust_PCs_PS.jpg",
                    scale = 1,
                    height = 7, width = 8)
    
  } else if (type == "all") {
    
    # extract the data i.e coordinates of individuals on the PCs
    data.act <- as.data.frame(res_pca$scores[, pcomp])
    
    # define distance matrix
    d <- dist(data.act)
    
    # perform clustering
    tree <- stats::hclust(d, method = method)
    
    # dendrogram
    dendro.dat <- ggdendro::dendro_data(tree, 
                                        type = "rectangle")
    
    # cut the tree in k clusters and save output in a df
    clust_output <- data.frame(cluster = stats::cutree(tree = tree, k = k[3]))
    
    # change labels to species name and add colour grouping
    dendro.labels <- dendro.dat$labels |>
      dplyr::mutate(label = scat_compo_tib$Code_sample[tree$order], 
                    cluster = factor(clust_output$cluster[tree$order])) 
    
    # plot dendrogram
    ggplot2::ggplot(dendro.dat$segments) + 
      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend))+
      ggplot2::geom_text(data = dendro.labels, 
                         ggplot2::aes(x, y, 
                                      label = label, 
                                      colour = cluster),
                         hjust = 1, size = 4) +
      ggplot2::scale_color_manual(values = c("1" = "#44A57CFF",
                                             "2" = "#1D2645FF",
                                             "3" = "#D8AF39FF", 
                                             "4" = "#AE93BEFF", 
                                             "5" = "#5A6F80FF",
                                             "6" = "#E75B64FF",
                                             "7" = "#B4DAE5FF", 
                                             "8" = "#E8C4A2FF")) +
      ggplot2::coord_flip() +
      ggplot2::ylim(-2, 25) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Cluster",
                                                     override.aes = list(shape = 12)))  +
      ggplot2::theme_classic()
    
    # save plot 
    ggplot2::ggsave("output/sites/clustering with PCs/dendrogram_clust_PCs_all_scats.jpg",
                    scale = 1,
                    height = 7, width = 8)
  }
  
  
  
}

#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
barplot_compo_rel_clust_per_scat <- function(res_clust,
                                             scat_compo_tib, 
                                             type # "sites" if one/site or "all" if all together 
) {
  
  if (type == "sites") {
    # assign each sample to its cluster
    clust_vec_CN <- res_clust$CN$cluster
    clust_vec_PS <- res_clust$PS$cluster
    
    
    ############################### CAP NOIR #####################################
    scat_compo_tib |>
      dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co, 
                    site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                            stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
      dplyr::filter(site == "Cap Noir") |>
      dplyr::mutate(cluster = clust_vec_CN) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co")), 
                    conc_relative = conc_mg_kg_dw/sum_nut, 
                    Code_sample = factor(Code_sample, 
                                         levels = c(
                                           # clust 1
                                           "CN20", "CN18", "CN14", "CN11", "CN01",
                                           # clust 2
                                           "CN17", "CN07", "CN19", "CN08", "CN25", 
                                           "CN05", "CN04", "CN06", "CN03",
                                           # clust 3
                                           "CN27", "CN26", "CN23", "CN29", "CN16", 
                                           "CN22", "CN15", "CN13", "CN09",  
                                           # clust 4
                                           "CN24", "CN21", "CN10", "CN28"
                                         ))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative, 
                                     fill = factor(cluster)), 
                        stat = "identity", 
                        position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Code_sample) + 
      ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                            "2" = "#1D2645FF",
                                            "3" = "#D8AF39FF", 
                                            "4" = "#AE93BEFF")) +
      ggplot2::ggtitle("Cap Noir") +
      ggplot2::ylab("Relative proportion in scats") +
      ggplot2::xlab("Nutrient") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     strip.text.x = ggplot2::element_text(size = 15),
                     legend.position = "none")
    ggplot2::ggsave("output/sites/clustering with PCs/clust_PCs_scat_compo_rel_CapNoir.jpg",
                    scale = 1,
                    height = 8, width = 12
    )
    
    ######################### POINTE SUZANNE #####################################
    scat_compo_tib |>
      dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co, 
                    site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                            stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
      dplyr::filter(site == "Pointe Suz") |>
      dplyr::mutate(cluster = clust_vec_PS) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co")), 
                    conc_relative = conc_mg_kg_dw/sum_nut, 
                    Code_sample = factor(Code_sample, 
                                         levels = c(
                                           # clust 1
                                           "PS20", "PS01", "PS18", "PS17", 
                                           "PS07", "PS28", "PS12", "PS08", 
                                           "PS21", "PS25",
                                           # clust 2
                                           "PS29", "PS02", "PS23", "PS09", 
                                           "PS05", "PS04", "PS06", "PS27", 
                                           # clust 3
                                           "PS26", "PS10", "PS24", "PS22", 
                                           "PS11", "PS03", 
                                           # clust 4
                                           "PS19", "PS15", "PS31", "PS16", 
                                           "PS14", "PS30", "PS13"
                                         ))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative, 
                                     fill = factor(cluster)), 
                        stat = "identity", 
                        position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Code_sample) + 
      ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                            "2" = "#1D2645FF",
                                            "3" = "#D8AF39FF", 
                                            "4" = "#AE93BEFF")) +
      ggplot2::ggtitle("Pointe Suzanne") +
      ggplot2::ylab("Relative proportion in scats") +
      ggplot2::xlab("Nutrient") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     strip.text.x = ggplot2::element_text(size = 15),
                     legend.position = "none")
    ggplot2::ggsave("output/sites/clustering with PCs/clust_PCs_scat_compo_rel_PSuzanne.jpg",
                    scale = 1,
                    height = 8, width = 12
    )
  } else if (type == "all") {
    
    # assign each sample to its cluster
    clust_vec <- res_clust$cluster
    
    scat_compo_tib |>
      dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co, 
                    cluster = clust_vec) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co")), 
                    conc_relative = conc_mg_kg_dw/sum_nut, 
                    Code_sample = factor(Code_sample, 
                                         levels = c(
                                           # clust 1
                                           "PS28", "PS07", "PS17", "PS12", "PS08", 
                                           "PS21", "PS25", "CN01",
                                           # clust 2
                                           "PS29", "PS02", "PS05", "PS27", "PS19",
                                           "PS15", "CN04", "PS23", "PS09", "PS04", 
                                           "CN22", "CN15", "PS31", "CN08", "PS14",
                                           "CN05", "PS13", "PS16", "CN25", "CN17", 
                                           "CN07", "PS30", "CN19", "CN06", "CN03", 
                                           "PS06",
                                           # clust 3
                                           "PS20", "CN16", "PS03", "PS01", "CN14",
                                           "PS24", "CN18", "PS22", "CN20", "CN11",
                                           "PS11", "PS10", "CN27", "CN26", "CN23", 
                                           "PS18", "PS26", "CN29", "CN13", "CN09",
                                           # clust 4
                                           "CN24", "CN21", "CN10", "CN28"
                                         ))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative, 
                                     fill = factor(cluster)), 
                        stat = "identity", 
                        position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Code_sample) + 
      ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                            "2" = "#1D2645FF",
                                            "3" = "#D8AF39FF", 
                                            "4" = "#AE93BEFF")) +
      ggplot2::ggtitle("All scats") +
      ggplot2::ylab("Relative proportion in scats") +
      ggplot2::xlab("Nutrient") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     strip.text.x = ggplot2::element_text(size = 15),
                     legend.position = "none")
    ggplot2::ggsave("output/sites/clustering with PCs/clust_PCs_scat_compo_rel_all_scats.jpg",
                    scale = 1,
                    height = 9, width = 14
    )
  }
  
  
}


#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
boxplot_compo_clust <- function(res_clust,
                                scat_compo_tib, 
                                type # "sites" if one/site or "all" if all together
) {
  
  if (type == "sites") {
    
    ############################### CAP NOIR #####################################
    # assign each sample to its cluster
    clust_vec_CN <- res_clust$CN$cluster
    
    mean_conc_table_CN <- scat_compo_tib |>
      dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                            stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
      dplyr::filter(site == "Cap Noir") |>
      dplyr::mutate(cluster = clust_vec_CN) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      dplyr::group_by(Nutrient) |>
      dplyr::summarise(mean_conc = mean(conc_mg_kg_dw), 
                       median_conc = median(conc_mg_kg_dw))
    
    scat_compo_tib |>
      dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                            stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
      dplyr::filter(site == "Cap Noir") |>
      dplyr::mutate(cluster = clust_vec_CN) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                         fill = factor(cluster)), 
                            position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Nutrient, scales = "free_y") + 
      ggplot2::geom_hline(data = mean_conc_table_CN, 
                          ggplot2::aes(yintercept = mean_conc), 
                          color = "darkred") +
      ggplot2::geom_hline(data = mean_conc_table_CN,
                          ggplot2::aes(yintercept = median_conc),
                          linetype = "dashed", 
                          color = "darkred") +
      ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                            "2" = "#1D2645FF",
                                            "3" = "#D8AF39FF", 
                                            "4" = "#AE93BEFF")) +
      ggplot2::ggtitle("Cap Noir") +
      ggplot2::ylab("Absolute concentration in scats (mg per kg dry weight)") +
      ggplot2::xlab("Cluster") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     strip.text.x = ggplot2::element_text(size = 15),
                     legend.position = "none")
    ggplot2::ggsave("output/sites/clustering with PCs/clust_PCs_scat_compo_abs_CapNoir.jpg",
                    scale = 1,
                    height = 8, width = 12
    )
    
    
    ######################### POINTE SUZANNE #####################################
    
    clust_vec_PS <- res_clust$PS$cluster
    
    mean_conc_table_PS <- scat_compo_tib |>
      dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                            stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
      dplyr::filter(site == "Pointe Suz") |>
      dplyr::mutate(cluster = clust_vec_PS) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      dplyr::group_by(Nutrient) |>
      dplyr::summarise(mean_conc = mean(conc_mg_kg_dw), 
                       median_conc = median(conc_mg_kg_dw))
    
    
    scat_compo_tib |>
      dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                            stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
      dplyr::filter(site == "Pointe Suz") |>
      dplyr::mutate(cluster = clust_vec_PS) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                         fill = factor(cluster)), 
                            position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Nutrient, scales = "free_y") + 
      ggplot2::geom_hline(data = mean_conc_table_PS, 
                          ggplot2::aes(yintercept = mean_conc), 
                          color = "darkred") +
      ggplot2::geom_hline(data = mean_conc_table_PS,
                          ggplot2::aes(yintercept = median_conc),
                          linetype = "dashed", 
                          color = "darkred") +
      ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                            "2" = "#1D2645FF",
                                            "3" = "#D8AF39FF", 
                                            "4" = "#AE93BEFF")) +
      ggplot2::ggtitle("Pointe Suzanne") +
      ggplot2::ylab("Absolute concentration in scats (mg per kg dry weight)") +
      ggplot2::xlab("Cluster") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     strip.text.x = ggplot2::element_text(size = 15),
                     legend.position = "none")
    ggplot2::ggsave("output/sites/clustering with PCs/clust_PCs_scat_compo_abs_PSuzanne.jpg",
                    scale = 1,
                    height = 8, width = 12
    )
    
  } else if (type == "all") {
    
    # assign each sample to its cluster
    clust_vec <- res_clust$cluster
    
    mean_conc_table <- scat_compo_tib |>
      dplyr::mutate(cluster = clust_vec) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      dplyr::group_by(Nutrient) |>
      dplyr::summarise(mean_conc = mean(conc_mg_kg_dw), 
                       median_conc = median(conc_mg_kg_dw))
    
    scat_compo_tib |>
      dplyr::mutate(cluster = clust_vec) |>
      tidyr::pivot_longer(cols = c(Fe:Co), 
                          names_to = "Nutrient", 
                          values_to = "conc_mg_kg_dw") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Fe", "Zn", 
                                                 "Cu", "Mn", "Se",
                                                 "Co"))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                         fill = factor(cluster)), 
                            position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Nutrient, scales = "free_y") + 
      ggplot2::geom_hline(data = mean_conc_table, 
                          ggplot2::aes(yintercept = mean_conc), 
                          color = "darkred") +
      ggplot2::geom_hline(data = mean_conc_table,
                          ggplot2::aes(yintercept = median_conc),
                          linetype = "dashed", 
                          color = "darkred") +
      ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                            "2" = "#1D2645FF",
                                            "3" = "#D8AF39FF", 
                                            "4" = "#AE93BEFF")) +
      ggplot2::ggtitle("All scats") +
      ggplot2::ylab("Absolute concentration in scats (mg per kg dry weight)") +
      ggplot2::xlab("Cluster") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     strip.text.x = ggplot2::element_text(size = 15),
                     legend.position = "none")
    ggplot2::ggsave("output/sites/clustering with PCs/clust_PCs_scat_compo_abs_all_scats.jpg",
                    scale = 1,
                    height = 8, width = 12
    )
    
  }
  
  
}



#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
table_compo_clust_per_site <- function(list_res_clust_sites,
                                       scat_compo_tib
) {
  
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  table <- rbind(scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Cap Noir") |>
                   dplyr::mutate(cluster = clust_vec_CN, 
                                 ntot = dplyr::n_distinct(Code_sample)) |>
                   dplyr::group_by(site, cluster) |>
                   dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                                    clust_ratio = 100*(n/ntot)) |>
                   dplyr::distinct(), 
                 scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Pointe Suzanne") |>
                   dplyr::mutate(cluster = clust_vec_PS, 
                                 ntot = dplyr::n_distinct(Code_sample)) |>
                   dplyr::group_by(site, cluster) |>
                   dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                                    clust_ratio = 100*(n/ntot)) |>
                   dplyr::distinct())
  
  openxlsx::write.xlsx(table, 
                       file = "output/sites/clustering with PCs/clust_PCs_percent_sites.xlsx")
  
  table 
  
}


#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
table_stats_clust_per_site <- function(list_res_clust_sites,
                                       scat_compo_tib
) {
  
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  table <- rbind(scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Cap Noir") |>
                   dplyr::mutate(cluster = clust_vec_CN) |>
                   tidyr::pivot_longer(cols = c(Fe:Co), 
                                       names_to = "Nutrient", 
                                       values_to = "conc_mg_kg_dw") |>
                   dplyr::mutate(Nutrient = factor(Nutrient, 
                                                   levels = c("Fe", "Zn", "Cu",
                                                              "Mn", "Se", "Co"))) |>
                   dplyr::group_by(site, cluster, Nutrient) |>
                   dplyr::summarise(median = round(median(conc_mg_kg_dw), 3),
                                    mean = round(mean(conc_mg_kg_dw), 3), 
                                    sd = round(sd(conc_mg_kg_dw), 3)) |>
                   tidyr::pivot_wider(names_from = Nutrient, 
                                      values_from = c(median, mean, sd), 
                                      names_sep = "_", 
                                      names_sort = TRUE), 
                 scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Pointe Suzanne") |>
                   dplyr::mutate(cluster = clust_vec_PS) |>
                   tidyr::pivot_longer(cols = c(Fe:Co), 
                                       names_to = "Nutrient", 
                                       values_to = "conc_mg_kg_dw") |>
                   dplyr::mutate(Nutrient = factor(Nutrient, 
                                                   levels = c("Fe", "Zn", "Cu",
                                                              "Mn", "Se", "Co"))) |>
                   dplyr::group_by(site, cluster, Nutrient) |>
                   dplyr::summarise(median = round(median(conc_mg_kg_dw), 3),
                                    mean = round(mean(conc_mg_kg_dw), 3), 
                                    sd = round(sd(conc_mg_kg_dw), 3)) |>
                   tidyr::pivot_wider(names_from = Nutrient, 
                                      values_from = c(median, mean, sd), 
                                      names_sep = "_", 
                                      names_sort = TRUE))
  
  openxlsx::write.xlsx(table, 
                       file = "output/sites/clustering with PCs/clust_PCs_compo_sites.xlsx")
  
  table 
  
}


#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different clusters 
MWtest_clust_k4 <- function(list_res_clust_sites,
                            scat_compo_tib) {
  
  # assign each sample to its cluster
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  scat_compo_tib_CN <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Cap Noir") |>
    dplyr::mutate(cluster = clust_vec_CN) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  scat_compo_tib_PS <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Pointe Suzanne") |>
    dplyr::mutate(cluster = clust_vec_PS) |>
    tidyr::pivot_longer(cols = c(Fe:Co), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  
  nut_vec <- unique(scat_compo_tib_CN$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table_CN <- scat_compo_tib_CN |>
      dplyr::filter(Nutrient == nut)
    
    table_PS <- scat_compo_tib_PS |>
      dplyr::filter(Nutrient == nut)
    
    table_CN$cluster <- factor(table_CN$cluster)
    table_PS$cluster <- factor(table_PS$cluster)
    
    table_CN <- table_CN |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_CN <- na.omit(table_CN$`1`)
    clust2_CN <- na.omit(table_CN$`2`)
    clust3_CN <- na.omit(table_CN$`3`)
    clust4_CN <- na.omit(table_CN$`4`)
    
    table_PS <- table_PS |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_PS <- na.omit(table_PS$`1`)
    clust2_PS <- na.omit(table_PS$`2`)
    clust3_PS <- na.omit(table_PS$`3`)
    clust4_PS <- na.omit(table_PS$`4`)
    
    nut_test <- rbind(data.frame(Site = "Cap Noir",
                                 Nutrient = rep(nut, 6), 
                                 Cluster_comp_1 = c("1", "1", "1",
                                                    "2", "2", 
                                                    "3"), 
                                 Cluster_comp_2 = c("2", "3", "4",
                                                    "3", "4", 
                                                    "4"), 
                                 alpha_MW = c(wilcox.test(clust1_CN, clust2_CN)[[3]],
                                              wilcox.test(clust1_CN, clust3_CN)[[3]],
                                              wilcox.test(clust1_CN, clust4_CN)[[3]],
                                              
                                              wilcox.test(clust2_CN, clust3_CN)[[3]],
                                              wilcox.test(clust2_CN, clust4_CN)[[3]],
                                              
                                              wilcox.test(clust3_CN, clust4_CN)[[3]])), 
                      data.frame(Site = "Pointe Suzanne",
                                 Nutrient = rep(nut, 6), 
                                 Cluster_comp_1 = c("1", "1", "1",
                                                    "2", "2", 
                                                    "3"), 
                                 Cluster_comp_2 = c("2", "3", "4",
                                                    "3", "4", 
                                                    "4"), 
                                 alpha_MW = c(wilcox.test(clust1_PS, clust2_PS)[[3]],
                                              wilcox.test(clust1_PS, clust3_PS)[[3]],
                                              wilcox.test(clust1_PS, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust2_PS, clust3_PS)[[3]],
                                              wilcox.test(clust2_PS, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust3_PS, clust4_PS)[[3]])))
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Site = NA, 
                        Nutrient = NA, 
                        Cluster_comp_1 = NA,
                        Cluster_comp_2 = NA,
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,] |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = alpha_MW)
  
  
  openxlsx::write.xlsx(df_test, 
                       file = "output/sites/clustering with PCs/Mann_Whitney_test_clust_PCs_compo_sites.xlsx")
  
  
}



