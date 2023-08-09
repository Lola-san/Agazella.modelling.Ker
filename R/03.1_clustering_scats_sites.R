################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 03.1_clustering_scats_sites.R
#
################################################################################





################ 1 - USING THE FULL COMPOSITIONAL DATA #########################
# as in contrast to using Principal components estimated by PCA 
# to reduce dimensions before conducting the clustering

#'
#'
#'
#'
# function to perform clustering directly on compositional dataset
# using the full dataset 
clust_compo_full_tib <- function(scat_compo_tib, 
                                 k, # should be vector of length 3 
                                 scale = "robust", # other option is "classical"
                                 method, 
                                 type # "sites" if one/site or "all" if all together
) {
  
  if (type == "sites") {
    data.act_CN <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "CN")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      as.data.frame()
    
    data.act_PS <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "PS")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      as.data.frame()
    
    ## robust estimation (default):
    res.clust.rob_CN <- robCompositions::clustCoDa(data.act_CN,
                                                   k = k[1], 
                                                   scale = scale, 
                                                   method = method)
    res.clust.rob_PS <- robCompositions::clustCoDa(data.act_PS,
                                                   k = k[2], 
                                                   scale = scale, 
                                                   method = method)
    
    list(CN = res.clust.rob_CN, PS = res.clust.rob_PS)
    
  } else if (type == "all") {
    data.act <- scat_compo_tib |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      as.data.frame()
    
    ## robust estimation (default):
    res.clust.rob <- robCompositions::clustCoDa(data.act,
                                                k = k[3], 
                                                scale = scale, 
                                                method = method)
    
    res.clust.rob
  }
  
  
  
  
}


#'
#'
#'
#'
# function to perform clustering with different cluster nb and plot 
# different validating values of the outputs
clust_find_k_table_full_tib <- function(scat_compo_tib,
                                        k_range = c(2:10),
                                        scale = "robust", # other option is "classical"
                                        method, 
                                        type # either "sites" or "all"
) {
  
  if (type == "sites") {

    ########################### CAP NOIR #########################################
    data.act_CN <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "CN")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      as.data.frame()
    
    # define distance matrix
    d_CN <- dist(data.act_CN)
    
    # perform clustering
    tree_CN <- stats::hclust(d_CN, method = method)
    
    list_outputs_CN <- list()

    for (i in k_range) {
      
      # # cut the tree in k clusters 
      # clust_output <- data.frame(cluster = cutree(tree = tree_CN, k = i))
      # 
      # # compute validity measures
      # clust_stats <- fpc::cluster.stats(as.dist(d_CN), clust_output$cluster)
      # 
      # # and save them
      # ki_df <- data.frame(k = clust_stats$cluster.number, 
      #                     method = method, 
      #                     size = clust_stats$cluster.size,
      #                     separation = round(clust_stats$separation, 3),
      #                     average.distance = round(clust_stats$average.distance, 3), 
      #                     median.distance = round(clust_stats$median.distance, 3),
      #                     avg.silwidth = round(as.data.frame(clust_stats$clus.avg.silwidths)[,1], 
      #                                          3), 
      #                     average.toother = round(clust_stats$average.toother, 3), 
      #                     min.clust.size = clust_stats$min.cluster.size)
      
      ## robust estimation (default):
      res.clust.rob_CN <- robCompositions::clustCoDa(data.act_CN,
                                                     k = i,
                                                     scale = scale,
                                                     method = method)

      # and save them
      ki_df <- data.frame(k = res.clust.rob_CN$k,
                          method = res.clust.rob_CN$method,
                          size = as.data.frame(res.clust.rob_CN$size)$Freq,
                          separation = round(res.clust.rob_CN$separation, 3),
                          average.distance = round(res.clust.rob_CN$average.distance, 3),
                          median.distance = round(res.clust.rob_CN$median.distance, 3),
                          avg.silwidth = round(res.clust.rob_CN$silwidths, 3),
                          average.toother = round(res.clust.rob_CN$average.toother, 3),
                          min.clust.size = min(as.data.frame(res.clust.rob_CN$size)$Freq))
      
      list_outputs_CN <- append(list_outputs_CN, list(ki_df))
    } 
    
    df0_CN <- data.frame(k = NA, 
                         method = NA,
                         size = NA,
                         separation = NA,
                         average.distance = NA, 
                         median.distance = NA,
                         avg.silwidth = NA, 
                         average.toother = NA, 
                         min.clust.size = NA)
    
    for (i in 1:length(k_range)) {
      df0_CN <- rbind(df0_CN, list_outputs_CN[[i]])
    }
    
    # delete first line of NAs
    df.to.plot_CN <- df0_CN[-1,]
    
    openxlsx::write.xlsx(df.to.plot_CN, 
                         file = "output/sites/clustering with all nutrients/clust_all_nut_findk_validity_measures_CN.xlsx")
    
    
    ########################### POINTE SUZANNE ###################################
    data.act_PS <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "PS")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      as.data.frame()
    
    list_outputs_PS <- list()
    
    for (i in k_range) {
      ## robust estimation (default):
      res.clust.rob_PS <- robCompositions::clustCoDa(data.act_PS,
                                                     k = i, 
                                                     scale = scale, 
                                                     method = method)
      
      # and save them
      ki_df <- data.frame(k = res.clust.rob_PS$k, 
                          method = res.clust.rob_PS$method, 
                          size = as.data.frame(res.clust.rob_PS$size)$Freq,
                          separation = round(res.clust.rob_PS$separation, 3),
                          average.distance = round(res.clust.rob_PS$average.distance, 3), 
                          median.distance = round(res.clust.rob_PS$median.distance, 3),
                          avg.silwidth = round(res.clust.rob_PS$silwidths, 3), 
                          average.toother = round(res.clust.rob_PS$average.toother, 3), 
                          min.clust.size = min(as.data.frame(res.clust.rob_PS$size)$Freq))
      
      list_outputs_PS <- append(list_outputs_PS, list(ki_df))
    } 
    
    df0_PS <- data.frame(k = NA, 
                         method = NA,
                         size = NA,
                         separation = NA,
                         average.distance = NA, 
                         median.distance = NA,
                         avg.silwidth = NA, 
                         average.toother = NA, 
                         min.clust.size = NA)
    
    for (i in 1:length(k_range)) {
      df0_PS <- rbind(df0_PS, list_outputs_PS[[i]])
    }
    
    # delete first line of NAs
    df.to.plot_PS <- df0_PS[-1,]
    
    openxlsx::write.xlsx(df.to.plot_PS, 
                         file = "output/sites/clustering with all nutrients/clust_all_nut_findk_validity_measures_PS.xlsx")
    
    list(CN = df.to.plot_CN, PS = df.to.plot_PS)
    
  } else if (type == "all") {
    data.act <- scat_compo_tib |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      as.data.frame()
    
    list_outputs <- list()
    
    for (i in k_range) {
      ## robust estimation (default):
      res.clust.rob <- robCompositions::clustCoDa(data.act,
                                                     k = i, 
                                                     scale = scale, 
                                                     method = method)
      
      # and save them
      ki_df <- data.frame(k = res.clust.rob$k, 
                          method = res.clust.rob$method, 
                          size = as.data.frame(res.clust.rob$size)$Freq,
                          separation = round(res.clust.rob$separation, 3),
                          average.distance = round(res.clust.rob$average.distance, 3), 
                          median.distance = round(res.clust.rob$median.distance, 3),
                          avg.silwidth = round(res.clust.rob$silwidths, 3), 
                          average.toother = round(res.clust.rob$average.toother, 3), 
                          min.clust.size = min(as.data.frame(res.clust.rob$size)$Freq))
      
      list_outputs <- append(list_outputs, list(ki_df))
      
    }
    
    df0 <- data.frame(k = NA, 
                      method = NA,
                      size = NA,
                      separation = NA,
                      average.distance = NA, 
                      median.distance = NA,
                      avg.silwidth = NA, 
                      average.toother = NA, 
                      min.clust.size = NA)
    
    for (i in 1:length(k_range)) {
      df0 <- rbind(df0, list_outputs[[i]])
    }
    
    # delete first line of NAs
    df.to.plot <- df0[-1,]
    
    openxlsx::write.xlsx(df.to.plot, 
                         file = "output/sites/clustering with all nutrients/clust_all_nut_findk_validity_measures_all_scats.xlsx")
    
    df.to.plot
    
    
  } 
  
  
  
}




#'
#'
#'
#'
# function to show means of validating values of the outputs
# for different numbers of clusters
means_clust_find_k_val_full_tib <- function(find_k_output_full_tib, 
                                   type # "sites" or "all"
) {
  
  
  
  if (type == "sites") {
    # set color palette 
    
    diff <- length(unique(find_k_output_full_tib$CN$k)) - 7
    
    possible_col <- c("#CD4F38FF", "#3D4F7DFF", 
                      "#657060FF", "#EAD890FF") 
    
    pal <- c(ghibli::ghibli_palettes$YesterdayMedium, 
             possible_col[1:diff])
    
    ############################### CAP NOIR ###################################
    find_k_output_CN <- find_k_output_full_tib$CN
    
    find_k_output_CN |>
      dplyr::mutate(k = as.factor(k)) |>
      tidyr::pivot_longer(cols = c("separation":"min.clust.size"), 
                          names_to = "validity.variable", 
                          values_to = "value") |>
      dplyr::group_by(k, validity.variable) |>
      dplyr::summarize(mean = mean(value)) |>
      ggplot2::ggplot(ggplot2::aes(x = k, y = mean, color = k)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~validity.variable, scale = "free") +
      ggplot2::scale_color_manual(values = pal) +
      ggplot2::ggtitle("Cap Noir") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.text.x = ggplot2::element_text(size = 15),
                     axis.text.y = ggplot2::element_text(size = 15),
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     strip.text.x = ggplot2::element_text(size = 15),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     legend.position = "none")
    # save plot 
    ggplot2::ggsave("output/sites/clustering with all nutrients/findk_validity_measures_means_all_nut_CN.jpg",
                    scale = 1,
                    height = 6, width = 8)
    
    ########################### POINTE SUZANNE #################################
    
    find_k_output_PS <- find_k_output_full_tib$PS
    
    find_k_output_PS |>
      dplyr::mutate(k = as.factor(k)) |>
      tidyr::pivot_longer(cols = c("separation":"min.clust.size"), 
                          names_to = "validity.variable", 
                          values_to = "value") |>
      dplyr::group_by(k, validity.variable) |>
      dplyr::summarize(mean = mean(value)) |>
      ggplot2::ggplot(ggplot2::aes(x = k, y = mean, color = k)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~validity.variable, scale = "free") +
      ggplot2::scale_color_manual(values = pal) +
      ggplot2::ggtitle("Pointe Suzanne") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.text.x = ggplot2::element_text(size = 15),
                     axis.text.y = ggplot2::element_text(size = 15),
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     strip.text.x = ggplot2::element_text(size = 15),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     legend.position = "none")
    # save plot 
    ggplot2::ggsave("output/sites/clustering with all nutrients/findk_validity_measures_means_all_nut_PS.jpg",
                    scale = 1,
                    height = 6, width = 8)
    
  } else if (type == "all") {
    # set color palette 
    
    diff <- length(unique(find_k_output_full_tib$k)) - 7
    
    possible_col <- c("#CD4F38FF", "#3D4F7DFF", 
                      "#657060FF", "#EAD890FF") 
    
    pal <- c(ghibli::ghibli_palettes$YesterdayMedium, 
             possible_col[1:diff])
    
    find_k_output_full_tib |>
      dplyr::mutate(k = as.factor(k)) |>
      tidyr::pivot_longer(cols = c("separation":"min.clust.size"), 
                          names_to = "validity.variable", 
                          values_to = "value") |>
      dplyr::group_by(k, validity.variable) |>
      dplyr::summarize(mean = mean(value)) |>
      ggplot2::ggplot(ggplot2::aes(x = k, y = mean, color = k)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~validity.variable, scale = "free") +
      ggplot2::scale_color_manual(values = pal) +
      ggplot2::ggtitle("All scats") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.text.x = ggplot2::element_text(size = 15),
                     axis.text.y = ggplot2::element_text(size = 15),
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     strip.text.x = ggplot2::element_text(size = 15),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     legend.position = "none")
    # save plot 
    ggplot2::ggsave("output/sites/clustering with all nutrients/findk_validity_measures_means_all_nut_all_scats.jpg",
                    scale = 1,
                    height = 6, width = 8)
  }
  
  
  
}




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
    ggplot2::ggsave("output/sites/clustering with all nutrients/clust_scat_compo_abs_all_nut_CapNoir.jpg",
                    scale = 1,
                    height = 8, width = 12
    )
    
    
    ######################### POINTE SUZANNE #####################################
    
    clust_vec_PS <- clust_full_tib_output$PS$cluster
    
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
    ggplot2::ggsave("output/sites/clustering with all nutrients/clust_scat_compo_abs_all_nut_PSuzanne.jpg",
                    scale = 1,
                    height = 8, width = 12
    )
    
  } else if (type == "all") {
    
    # assign each sample to its cluster
    clust_vec <- clust_full_tib_output$cluster
    
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
                   tidyr::pivot_longer(cols = c(Fe:Co), 
                                       names_to = "Nutrient", 
                                       values_to = "conc_mg_kg_dw") |>
                   dplyr::group_by(site, cluster, Nutrient) |>
                   dplyr::summarise(mean = round(mean(conc_mg_kg_dw), 3), 
                                    sd = round(sd(conc_mg_kg_dw), 3),
                                    n = dplyr::n_distinct(Code_sample), 
                                    clust_ratio = round(100*(n/ntot), 1)) |>
                   dplyr::distinct() |>
                   tidyr::pivot_wider(names_from = Nutrient, 
                                      values_from = c(mean, sd), 
                                      names_sep = "_"), 
                 scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Pointe Suzanne") |>
                   dplyr::mutate(cluster = clust_vec_PS, 
                                 ntot = dplyr::n_distinct(Code_sample)) |>
                   tidyr::pivot_longer(cols = c(Fe:Co), 
                                       names_to = "Nutrient", 
                                       values_to = "conc_mg_kg_dw") |>
                   dplyr::group_by(site, cluster, Nutrient) |>
                   dplyr::summarise(mean = round(mean(conc_mg_kg_dw), 3), 
                                    sd = round(sd(conc_mg_kg_dw), 3),
                                    n = dplyr::n_distinct(Code_sample), 
                                    clust_ratio = round(100*(n/ntot), 1)) |>
                   dplyr::distinct() |>
                   tidyr::pivot_wider(names_from = Nutrient, 
                                      values_from = c(mean, sd), 
                                      names_sep = "_"))
  
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
#'
# function to perform Principal Component Analysis using robust method 
# for composition data with package robCompositions
pca_coda <- function(scat_compo_tib, 
                     type # "sites" if one/site or "all" if all together
) {
  
  if (type == "sites") {
    data.act_CN <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "CN")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      as.data.frame()
    
    data.act_PS <- scat_compo_tib |> 
      dplyr::filter(stringr::str_detect(Code_sample, "PS")) |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      as.data.frame()
    
    ## robust estimation (default):
    res.rob_CN <- robCompositions::pcaCoDa(data.act_CN)
    res.rob_PS <- robCompositions::pcaCoDa(data.act_PS)
    
    list(CN = res.rob_CN, PS = res.rob_PS)
    
  } else if (type == "all") {
    data.act <- scat_compo_tib |> 
      dplyr::select(Fe, Zn, Cu, Mn, Se, Co) |>
      as.data.frame()
    
    ## robust estimation (default):
    res.rob <- robCompositions::pcaCoDa(data.act)

    res.rob
  }
  
}



#'
#'
#'
#'
#'
# function to create biplot for PCA coda (robust or non-robust) output 
biplot_pca_coda <- function(res_pca, 
                            type, # "sites" if one/site or "all" if all together
                            scat_compo_tib,
                            pcomp = c(1:2), # choices of the PC to plot (2 in total), 
                            # 1 and 2 by default
                            groups, # either "species", "family"
                            # if on fish data 
                            # or "site" "HPI" if on scat data 
                            circle = FALSE, # weither to draw correlation circle or not 
                            circle.prob = 0.69, # not sure yet why this value by default
                            var.add.scaling = 2, # constant to multiply coordinates
                            # of variables by so that they show on a similar scale as 
                            # that of observations # 2 seems to fit ok but could be changed 
                            ellipse = FALSE, # logical weither to draw ellipse around groups
                            # of points or not 
                            ellipse.prob = 0.68 # size of the ellipse in Normal probability
                            # not sure yet why this value by default
) {
  
  # function constructed based on ggbiplot function on github 
  # https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r
  # and was adapted to work for an pca CoDa object
  
  ###### biplot settings
  # common practice as explained in ?biplot() : 
  # variables are scaled by lambda^scale and observations are scaled by
  # lambda^(1-scale) where lambda are singular values as computed by PCA
  # i.e d below
  scale <- 0
  obs.scale <- 1 - scale
  var.scale <- scale
  
  if (type == "sites") {
    res_pca_CN <- res_pca$CN
    res_pca_PS <- res_pca$PS

    
    ################### CAP NOIR FIRST ###########################################
    ##### recover the single value decomposition SVD
    nobs.factor.CN <- sqrt(nrow(res_pca_CN$scores) - 1) # not sure what this is 
    # and what is it for
    
    # standard deviation of the PCs #lambda in ?biplot()
    d.CN <- sqrt(res_pca_CN$eigenvalues)
    
    u.CN <- sweep(res_pca_CN$scores, 2, 1 / (d.CN * nobs.factor.CN), FUN = '*')
    v.CN <- res_pca_CN$loadings
    
    
    #####
    # compute scores 
    # ie coordinates of individuals (observations) on each principal component (PC)
    # pcomp <- pmin(pcomp, ncol(u)) # not sure what is the purpose of this
    df.u.CN <- as.data.frame(sweep(u.CN[,pcomp], 2, d.CN[pcomp]^obs.scale, FUN='*'))
    # scale observations by lambda^(1-scale)
    
    # compute directions 
    # ie coordinates of the variables ie loadings * sdev of PCs
    v.CN <- sweep(v.CN, 2, d.CN^var.scale, FUN='*')
    df.v.CN <- as.data.frame(v.CN[, pcomp])
    
    names(df.u.CN) <- c('xvar', 'yvar')
    names(df.v.CN) <- names(df.u.CN)
    
    df.u.CN <- df.u.CN * nobs.factor.CN # so we are back to the original scores - res_pca$scores
    # ie the coordinates of the individuals on the PCs
    
    # Scale the radius of the correlation circle so that it corresponds to 
    # a data ellipse for the standardized PC scores (as done with ggbiplot)
    r.CN <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u.CN^2))^(1/4) 
    
    # scale directions
    # v^2 = cos2 = quality of representation of variables on each PC 
    v.scale.CN <- rowSums(v.CN^2)
    df.v.CN <- r.CN * df.v.CN / sqrt(max(v.scale.CN))
    # multiply then by another constant to get arrows on the same scale as observations 
    # coordinates
    # as mentioned in 
    # https://stats.stackexchange.com/questions/141085/positioning-the-arrows-on-a-pca-biplot
    # "it might be necessary to scale arrows by some arbitrary constant factor so 
    # that both arrows and data points appear roughly on the same scale "
    df.v.CN <- var.add.scaling * df.v.CN
    
    # scale scores 
    # as done by 
    # https://stackoverflow.com/questions/18039313/pca-scaling-with-ggbiplot
    # with r <- 1 
    # r.scale=sqrt(max(df.u[,1]^2+df.u[,2]^2))
    # df.u=.99*df.u/r.scale
    # this version was set aside as we are more interested in comparing individuals 
    # and not structuring variables, so we went for an additional scaling 
    # of variables coordinates instead - see above
    
    # Append the proportion of explained variance to the axis labels
    u.axis.labs <- paste('PC', pcomp,' (clr - robust)', sep='')   
    # add explained variance
    u.axis.labs.CN <- paste(u.axis.labs, 
                            sprintf('(%0.1f%% explained var.)', 
                                    100 * res_pca_CN$eigenvalues[pcomp]/sum(res_pca_CN$eigenvalues)))
    
    
    # Score Labels (labels of the observations)
    df.u.CN$labels <- (scat_compo_tib |> 
                         dplyr::filter(stringr::str_detect(Code_sample, "CN")))$Code_sample
    
    # Variable Names
    df.v.CN$varname <- rownames(v.CN)
    
    # Variables for text label placement
    varname.adjust <- 1.2 # adjustment factor the placement of the variable names, >= 1 means farther from the arrow
    var.axes <- TRUE # draw arrow for the variable
    df.v.CN$angle <- with(df.v.CN, (180/pi) * atan(yvar / xvar))
    df.v.CN$hjust = with(df.v.CN, (1 - varname.adjust * sign(xvar)) / 2)
    
    
    ############## draw biplot
    # Base plot
    ggplot2::ggplot(data = df.u.CN, ggplot2::aes(x = xvar, y = yvar)) + 
      ggplot2::xlab(u.axis.labs.CN[1]) + 
      ggplot2::ylab(u.axis.labs.CN[2]) + 
      ggplot2::coord_equal() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Cap Noir") +
      # Draw directions
      ggplot2::geom_segment(data = df.v.CN,
                            ggplot2::aes(x = 0, y = 0, xend = xvar, yend = yvar),
                            arrow = ggplot2::arrow(length = ggplot2::unit(1/2, 
                                                                          'picas')), 
                            color = 'darkred') +
      # Draw either labels or points
      ggplot2::geom_point(ggplot2::aes(), 
                          size = 1.5,
                          alpha = 1 # alpha transparency value for the points (0 = transparent, 1 = opaque)
      ) + 
      ggplot2::geom_text(data = df.u.CN, 
                         ggplot2::aes(label = labels, x = xvar + 0.06, y = yvar + 0.06
                         ), 
                         color = 'black', size = 5) +
      # Label the variable axes
      ggplot2::geom_text(data = df.v.CN, 
                         ggplot2::aes(label = varname, x = xvar, y = yvar, 
                                      angle = angle, hjust = hjust), 
                         color = 'darkred', size = 5) +
      #ggplot2::scale_color_manual(values = color_scale) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     legend.position = "bottom",
                     legend.title = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     legend.text = ggplot2::element_text(size = 15))
    
     # save plot 
    ggplot2::ggsave("output/sites/clustering with PCs/PCA_biplot_sites_CN.jpg",
                    scale = 1,
                    height = 8, width = 10
    )
    
    ######################### POINTE SUZANNE #####################################
    
    ##### recover the single value decomposition SVD
    nobs.factor.PS <- sqrt(nrow(res_pca_PS$scores) - 1) # not sure what this is 
    # and what is it for
    
    # standard deviation of the PCs #lambda in ?biplot()
    d.PS <- sqrt(res_pca_PS$eigenvalues)
    
    u.PS <- sweep(res_pca_PS$scores, 2, 1 / (d.PS * nobs.factor.PS), FUN = '*')
    v.PS <- res_pca_PS$loadings
    
    
    #####
    # compute scores 
    # ie coordinates of individuals (observations) on each principal component (PC)
    # pcomp <- pmin(pcomp, ncol(u)) # not sure what is the purpose of this
    df.u.PS <- as.data.frame(sweep(u.PS[,pcomp], 2, d.PS[pcomp]^obs.scale, FUN='*'))
    # scale observations by lambda^(1-scale)
    
    # compute directions 
    # ie coordinates of the variables ie loadings * sdev of PCs
    v.PS <- sweep(v.PS, 2, d.PS^var.scale, FUN='*')
    df.v.PS <- as.data.frame(v.PS[, pcomp])
    
    names(df.u.PS) <- c('xvar', 'yvar')
    names(df.v.PS) <- names(df.u.PS)
    
    df.u.PS <- df.u.PS * nobs.factor.PS # so we are back to the original scores - res_pca$scores
    # ie the coordinates of the individuals on the PCs
    
    # Scale the radius of the correlation circle so that it corresponds to 
    # a data ellipse for the standardized PC scores (as done with ggbiplot)
    r.PS <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u.PS^2))^(1/4) 
    
    # scale directions
    # v^2 = cos2 = quality of representation of variables on each PC 
    v.scale.PS <- rowSums(v.PS^2)
    df.v.PS <- r.PS * df.v.PS / sqrt(max(v.scale.PS))
    # multiply then by another constant to get arrows on the same scale as observations 
    # coordinates
    # as mentioned in 
    # https://stats.stackexchange.com/questions/141085/positioning-the-arrows-on-a-pca-biplot
    # "it might be necessary to scale arrows by some arbitrary constant factor so 
    # that both arrows and data points appear roughly on the same scale "
    df.v.PS <- var.add.scaling * df.v.PS
    
    # scale scores 
    # as done by 
    # https://stackoverflow.com/questions/18039313/pca-scaling-with-ggbiplot
    # with r <- 1 
    # r.scale=sqrt(max(df.u[,1]^2+df.u[,2]^2))
    # df.u=.99*df.u/r.scale
    # this version was set aside as we are more interested in comparing individuals 
    # and not structuring variables, so we went for an additional scaling 
    # of variables coordinates instead - see above
    
    # Append the proportion of explained variance to the axis labels
    u.axis.labs <- paste('PC', pcomp,' (clr - robust)', sep='')   
    # add explained variance
    u.axis.labs.PS <- paste(u.axis.labs, 
                            sprintf('(%0.1f%% explained var.)', 
                                    100 * res_pca_PS$eigenvalues[pcomp]/sum(res_pca_PS$eigenvalues)))
    
    
    # Score Labels (labels of the observations)
    df.u.PS$labels <- (scat_compo_tib |> 
                         dplyr::filter(stringr::str_detect(Code_sample, "PS")))$Code_sample

    # Variable Names
    df.v.PS$varname <- rownames(v.PS)
    
    # Variables for text label placement
    varname.adjust <- 1.2 # adjustment factor the placement of the variable names, >= 1 means farther from the arrow
    var.axes <- TRUE # draw arrow for the variable
    df.v.PS$angle <- with(df.v.PS, (180/pi) * atan(yvar / xvar))
    df.v.PS$hjust = with(df.v.PS, (1 - varname.adjust * sign(xvar)) / 2)
    
    
    ############## draw biplot
    # Base plot
    ggplot2::ggplot(data = df.u.PS, ggplot2::aes(x = xvar, y = yvar)) + 
      ggplot2::xlab(u.axis.labs.PS[1]) + 
      ggplot2::ylab(u.axis.labs.PS[2]) + 
      ggplot2::coord_equal() +
      ggplot2::ggtitle("Pointe Suzanne") +
      ggplot2::theme_bw() +
      # Draw directions
      ggplot2::geom_segment(data = df.v.PS,
                            ggplot2::aes(x = 0, y = 0, xend = xvar, yend = yvar),
                            arrow = ggplot2::arrow(length = ggplot2::unit(1/2, 
                                                                          'picas')), 
                            color = 'darkred') +
      # Draw either labels or points
      ggplot2::geom_point(ggplot2::aes(), 
                          size = 1.5,
                          alpha = 1 # alpha transparency value for the points (0 = transparent, 1 = opaque)
      ) + 
      ggplot2::geom_text(data = df.u.PS, 
                         ggplot2::aes(label = labels, x = xvar + 0.06, y = yvar + 0.06
                         ), 
                         color = 'black', size = 5) +
      # Label the variable axes
      ggplot2::geom_text(data = df.v.PS, 
                         ggplot2::aes(label = varname, x = xvar, y = yvar, 
                                      angle = angle, hjust = hjust), 
                         color = 'darkred', size = 5) +
      #ggplot2::scale_color_manual(values = color_scale) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     legend.position = "bottom",
                     legend.title = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     legend.text = ggplot2::element_text(size = 15))
    
    # save plot 
    ggplot2::ggsave("output/sites/clustering with PCs/PCA_biplot_sites_PS.jpg",
                    scale = 1,
                    height = 8, width = 10
    )
  } else if (type == "all") {
    
    ##### recover the single value decomposition SVD
    nobs.factor <- sqrt(nrow(res_pca$scores) - 1) # not sure what this is 
    # and what is it for
    
    # standard deviation of the PCs #lambda in ?biplot()
    d <- sqrt(res_pca$eigenvalues)
    
    u <- sweep(res_pca$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- res_pca$loadings
    
    
    #####
    # compute scores 
    # ie coordinates of individuals (observations) on each principal component (PC)
    # pcomp <- pmin(pcomp, ncol(u)) # not sure what is the purpose of this
    df.u <- as.data.frame(sweep(u[,pcomp], 2, d[pcomp]^obs.scale, FUN='*'))
    # scale observations by lambda^(1-scale)
    
    # compute directions 
    # ie coordinates of the variables ie loadings * sdev of PCs
    v <- sweep(v, 2, d^var.scale, FUN='*')
    df.v <- as.data.frame(v[, pcomp])
    
    names(df.u) <- c('xvar', 'yvar')
    names(df.v) <- names(df.u)
    
    df.u <- df.u * nobs.factor # so we are back to the original scores - res_pca$scores
    # ie the coordinates of the individuals on the PCs
    
    # Scale the radius of the correlation circle so that it corresponds to 
    # a data ellipse for the standardized PC scores (as done with ggbiplot)
    r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4) 
    
    # scale directions
    # v^2 = cos2 = quality of representation of variables on each PC 
    v.scale <- rowSums(v^2)
    df.v <- r * df.v / sqrt(max(v.scale))
    # multiply then by another constant to get arrows on the same scale as observations 
    # coordinates
    # as mentioned in 
    # https://stats.stackexchange.com/questions/141085/positioning-the-arrows-on-a-pca-biplot
    # "it might be necessary to scale arrows by some arbitrary constant factor so 
    # that both arrows and data points appear roughly on the same scale "
    df.v <- var.add.scaling * df.v
    
    # scale scores 
    # as done by 
    # https://stackoverflow.com/questions/18039313/pca-scaling-with-ggbiplot
    # with r <- 1 
    # r.scale=sqrt(max(df.u[,1]^2+df.u[,2]^2))
    # df.u=.99*df.u/r.scale
    # this version was set aside as we are more interested in comparing individuals 
    # and not structuring variables, so we went for an additional scaling 
    # of variables coordinates instead - see above
    
    # Append the proportion of explained variance to the axis labels
    u.axis.labs <- paste('PC', pcomp,' (clr - robust)', sep='')   
    # add explained variance
    u.axis.labs <- paste(u.axis.labs, 
                            sprintf('(%0.1f%% explained var.)', 
                                    100 * res_pca$eigenvalues[pcomp]/sum(res_pca$eigenvalues)))
    
    
    # Score Labels (labels of the observations)
    df.u$labels <- scat_compo_tib$Code_sample
    df.u$groups <- (scat_compo_tib |>
                      dplyr::mutate(site = dplyr::case_when(
                        stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                        stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")))$site
    
    # Variable Names
    df.v$varname <- rownames(v)
    
    # Variables for text label placement
    varname.adjust <- 1.2 # adjustment factor the placement of the variable names, >= 1 means farther from the arrow
    var.axes <- TRUE # draw arrow for the variable
    df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
    df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
    
    
    ############## draw biplot
    # Base plot
    ggplot2::ggplot(data = df.u, ggplot2::aes(x = xvar, y = yvar)) + 
      ggplot2::xlab(u.axis.labs[1]) + 
      ggplot2::ylab(u.axis.labs[2]) + 
      ggplot2::coord_equal() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("All scats") +
      # Draw directions
      ggplot2::geom_segment(data = df.v,
                            ggplot2::aes(x = 0, y = 0, xend = xvar, yend = yvar),
                            arrow = ggplot2::arrow(length = ggplot2::unit(1/2, 
                                                                          'picas')), 
                            color = 'darkred') +
      # Draw either labels or points
      ggplot2::geom_point(ggplot2::aes(color = groups), 
                          size = 1.5,
                          alpha = 1 # alpha transparency value for the points (0 = transparent, 1 = opaque)
      ) + 
      ggplot2::geom_text(data = df.u, 
                         ggplot2::aes(label = labels, 
                                      x = xvar + 0.08, 
                                      y = yvar + 0.08, 
                                      color = groups), 
                         size = 5) +
      ggplot2::scale_color_manual(values = c("#D8AF39FF", "#58A449FF")) +
      # Label the variable axes
      ggplot2::geom_text(data = df.v, 
                         ggplot2::aes(label = varname, x = xvar, y = yvar, 
                                      angle = angle, hjust = hjust), 
                         color = 'darkred', size = 5) +
      #ggplot2::scale_color_manual(values = color_scale) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                     axis.text.y = ggplot2::element_text(size = 15), 
                     axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"), 
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     legend.position = "bottom",
                     legend.title = ggplot2::element_text(size = 16, 
                                                          face = "bold"),
                     legend.text = ggplot2::element_text(size = 15))
    
    # save plot 
    ggplot2::ggsave("output/sites/clustering with PCs/PCA_biplot_all_scats.jpg",
                    scale = 1,
                    height = 8, width = 10
    )
  }
  
  
  
  
}



#'
#'
#'
#'
# function to perform clustering
# using Principal components estimated by PCA
# with hclust algorithm
clust_compo_PCs <- function(res_pca, 
                            type, # "sites" if one/site or "all" if all together 
                            pcomp = c(1:2),
                            k = c(2, 2, 2), # first CN then PS, then "all"
                            method 
) {
  
  if (type == "sites") {
    res_pca_CN <- res_pca$CN
    res_pca_PS <- res_pca$PS
    
    ########################### CAP NOIR #########################################
    # extract the data i.e coordinates of individuals on the PCs
    data.act_CN <- as.data.frame(res_pca_CN$scores[, pcomp])
    
    # define distance matrix
    d_CN <- dist(data.act_CN)
    
    # perform clustering
    tree_CN <- stats::hclust(d_CN, method = method)
    
    # cut the tree in k clusters and save output in a df
    clust_output_CN <- data.frame(cluster = cutree(tree = tree_CN, k = k[1]))
    
    # compute validity measures and save them
    clust_stats_CN <- fpc::cluster.stats(as.dist(d_CN), clust_output_CN$cluster)
    
    clust.val_CN <- data.frame(k = clust_stats_CN$cluster.number, 
                               method = method, 
                               size = clust_stats_CN$cluster.size,
                               separation = round(clust_stats_CN$separation, 3),
                               average.distance = round(clust_stats_CN$average.distance, 3), 
                               median.distance = round(clust_stats_CN$median.distance, 3),
                               avg.silwidth = round(as.data.frame(clust_stats_CN$clus.avg.silwidths)[,1], 3), 
                               average.toother = round(clust_stats_CN$average.toother, 3), 
                               min.clust.size = clust_stats_CN$min.cluster.size) 
    
    openxlsx::write.xlsx(clust.val_CN, 
                         file = "output/sites/clustering with PCs/clust_PCs_validity_measures_CN.xlsx")
    
    
    ########################### POINTE SUZANNE ###################################
    # extract the data i.e coordinates of individuals on the PCs
    data.act_PS <- as.data.frame(res_pca_PS$scores[, pcomp])
    
    # define distance matrix
    d_PS <- dist(data.act_PS)
    
    # perform clustering
    tree_PS <- stats::hclust(d_PS, method = method)
    
    # cut the tree in k clusters and save output in a df
    clust_output_PS <- data.frame(cluster = cutree(tree = tree_PS, k = k[2]))
    
    # compute validity measures and save them
    clust_stats_PS <- fpc::cluster.stats(as.dist(d_PS), clust_output_PS$cluster)
    
    clust.val_PS <- data.frame(k = clust_stats_PS$cluster.number, 
                               method = method, 
                               size = clust_stats_PS$cluster.size,
                               separation = round(clust_stats_PS$separation, 3),
                               average.distance = round(clust_stats_PS$average.distance, 3), 
                               median.distance = round(clust_stats_PS$median.distance, 3),
                               avg.silwidth = round(as.data.frame(clust_stats_PS$clus.avg.silwidths)[,1], 3), 
                               average.toother = round(clust_stats_PS$average.toother, 3), 
                               min.clust.size = clust_stats_PS$min.cluster.size) 
    
    openxlsx::write.xlsx(clust.val_PS, 
                         file = "output/sites/clustering with PCs/clust_PCs_validity_measures_PS.xlsx")
    
    list(CN = clust_output_CN, PS = clust_output_PS)
    
    } else if (type == "all") {

    # extract the data i.e coordinates of individuals on the PCs
    data.act <- as.data.frame(res_pca$scores[, pcomp])
    
    # define distance matrix
    d <- dist(data.act)
    
    # perform clustering
    tree <- stats::hclust(d, method = method)
    
    # cut the tree in k clusters and save output in a df
    clust_output <- data.frame(cluster = cutree(tree = tree, k = k[3]))
    
    # compute validity measures and save them
    clust_stats <- fpc::cluster.stats(as.dist(d), clust_output$cluster)
    
    clust.val <- data.frame(k = clust_stats$cluster.number, 
                               method = method, 
                               size = clust_stats$cluster.size,
                               separation = round(clust_stats$separation, 3),
                               average.distance = round(clust_stats$average.distance, 3), 
                               median.distance = round(clust_stats$median.distance, 3),
                               avg.silwidth = round(as.data.frame(clust_stats$clus.avg.silwidths)[,1], 3), 
                               average.toother = round(clust_stats$average.toother, 3), 
                               min.clust.size = clust_stats$min.cluster.size) 
    
    openxlsx::write.xlsx(clust.val, 
                         file = "output/sites/clustering with PCs/clust_PCs_validity_measures_all_scats.xlsx")
    
    clust_output
  }
  
}



#'
#'
#'
#'
# function to perform clustering with different cluster nb and plot 
# different validating values of the outputs
clust_find_k_table_PCs <- function(res_pca,
                                   k_range = c(2:10),
                                   method, 
                                   type # either "sites" or "all"
) {
  
  if (type == "sites") {
    pcomp <- c(1:2)
    
    
    res_pca_CN <- res_pca$CN
    res_pca_PS <- res_pca$PS
    
    ########################### CAP NOIR #########################################
    # extract the data i.e coordinates of individuals on the PCs
    data.act_CN <- as.data.frame(res_pca_CN$scores[, pcomp])
    
    # define distance matrix
    d_CN <- dist(data.act_CN)
    
    # perform clustering
    tree_CN <- stats::hclust(d_CN, method = method)
    
    list_outputs_CN <- list()
    
    for (i in k_range) {
      # cut the tree in k clusters 
      clust_output <- data.frame(cluster = cutree(tree = tree_CN, k = i))
      
      # compute validity measures
      clust_stats <- fpc::cluster.stats(as.dist(d_CN), clust_output$cluster)
      
      # and save them
      ki_df <- data.frame(k = clust_stats$cluster.number, 
                          method = method, 
                          size = clust_stats$cluster.size,
                          separation = round(clust_stats$separation, 3),
                          average.distance = round(clust_stats$average.distance, 3), 
                          median.distance = round(clust_stats$median.distance, 3),
                          avg.silwidth = round(as.data.frame(clust_stats$clus.avg.silwidths)[,1], 
                                               3), 
                          average.toother = round(clust_stats$average.toother, 3), 
                          min.clust.size = clust_stats$min.cluster.size)
      
      list_outputs_CN <- append(list_outputs_CN, list(ki_df))
    } 
    
    df0_CN <- data.frame(k = NA, 
                      method = NA,
                      size = NA,
                      separation = NA,
                      average.distance = NA, 
                      median.distance = NA,
                      avg.silwidth = NA, 
                      average.toother = NA, 
                      min.clust.size = NA)
    
    for (i in 1:length(k_range)) {
      df0_CN <- rbind(df0_CN, list_outputs_CN[[i]])
    }
    
    # delete first line of NAs
    df.to.plot_CN <- df0_CN[-1,]
    
    openxlsx::write.xlsx(df.to.plot_CN, 
                         file = "output/sites/clustering with PCs/clust_PCs_findk_validity_measures_CN.xlsx")
    
    
    ########################### POINTE SUZANNE ###################################
    # extract the data i.e coordinates of individuals on the PCs
    data.act_PS <- as.data.frame(res_pca_PS$scores[, pcomp])
    
    # define distance matrix
    d_PS <- dist(data.act_PS)
    
    # perform clustering
    tree_PS <- stats::hclust(d_PS, method = method)
    
    list_outputs_PS <- list()
    
    for (i in k_range) {
      # cut the tree in k clusters 
      clust_output <- data.frame(cluster = cutree(tree = tree_PS, k = i))
      
      # compute validity measures
      clust_stats <- fpc::cluster.stats(as.dist(d_PS), clust_output$cluster)
      
      # and save them
      ki_df <- data.frame(k = clust_stats$cluster.number, 
                          method = method, 
                          size = clust_stats$cluster.size,
                          separation = round(clust_stats$separation, 3),
                          average.distance = round(clust_stats$average.distance, 3), 
                          median.distance = round(clust_stats$median.distance, 3),
                          avg.silwidth = round(as.data.frame(clust_stats$clus.avg.silwidths)[,1], 
                                               3), 
                          average.toother = round(clust_stats$average.toother, 3), 
                          min.clust.size = clust_stats$min.cluster.size)
      
      list_outputs_PS <- append(list_outputs_PS, list(ki_df))
    } 
    
    df0_PS <- data.frame(k = NA, 
                         method = NA,
                         size = NA,
                         separation = NA,
                         average.distance = NA, 
                         median.distance = NA,
                         avg.silwidth = NA, 
                         average.toother = NA, 
                         min.clust.size = NA)
    
    for (i in 1:length(k_range)) {
      df0_PS <- rbind(df0_PS, list_outputs_PS[[i]])
    }
    
    # delete first line of NAs
    df.to.plot_PS <- df0_PS[-1,]
    
    openxlsx::write.xlsx(df.to.plot_PS, 
                         file = "output/sites/clustering with PCs/clust_PCs_findk_validity_measures_PS.xlsx")
    
    list(CN = df.to.plot_CN, PS = df.to.plot_PS)
    
  } else if (type == "all") {
    
    pcomp <- c(1:3)
    
    
    # extract the data i.e coordinates of individuals on the PCs
    data.act <- as.data.frame(res_pca$scores[, pcomp])
    
    # define distance matrix
    d <- dist(data.act)
    
    # perform clustering
    tree <- stats::hclust(d, method = method)
    
    list_outputs <- list()
    
    for (i in k_range) {
      # cut the tree in k clusters 
      clust_output <- data.frame(cluster = cutree(tree = tree, k = i))
      
      # compute validity measures
      clust_stats <- fpc::cluster.stats(as.dist(d), clust_output$cluster)
      
      # and save them
      ki_df <- data.frame(k = clust_stats$cluster.number, 
                          method = method, 
                          size = clust_stats$cluster.size,
                          separation = round(clust_stats$separation, 3),
                          average.distance = round(clust_stats$average.distance, 3), 
                          median.distance = round(clust_stats$median.distance, 3),
                          avg.silwidth = round(as.data.frame(clust_stats$clus.avg.silwidths)[,1], 
                                               3), 
                          average.toother = round(clust_stats$average.toother, 3), 
                          min.clust.size = clust_stats$min.cluster.size)
      
      list_outputs <- append(list_outputs, list(ki_df))
      
    }
    
    df0 <- data.frame(k = NA, 
                      method = NA,
                      size = NA,
                      separation = NA,
                      average.distance = NA, 
                      median.distance = NA,
                      avg.silwidth = NA, 
                      average.toother = NA, 
                      min.clust.size = NA)
    
    for (i in 1:length(k_range)) {
      df0 <- rbind(df0, list_outputs[[i]])
    }
    
    # delete first line of NAs
    df.to.plot <- df0[-1,]
    
    openxlsx::write.xlsx(df.to.plot, 
                         file = "output/sites/clustering with PCs/clust_PCs_findk_validity_measures_all_scats.xlsx")
    
    df.to.plot
    
    
  } 
  
  
  
}




#'
#'
#'
#'
# function to show means of validating values of the outputs
# for different numbers of clusters
means_clust_find_k_val <- function(find_k_output, 
                                   type # "sites" or "all"
                                   ) {
  
 
  
  if (type == "sites") {
    # set color palette 
    
    diff <- length(unique(find_k_output$CN$k)) - 7
    
    possible_col <- c("#CD4F38FF", "#3D4F7DFF", 
                      "#657060FF", "#EAD890FF") 
    
    pal <- c(ghibli::ghibli_palettes$YesterdayMedium, 
             possible_col[1:diff])
    
    ############################### CAP NOIR ###################################
    find_k_output_CN <- find_k_output$CN
    
    find_k_output_CN |>
      dplyr::mutate(k = as.factor(k)) |>
      tidyr::pivot_longer(cols = c("separation":"min.clust.size"), 
                          names_to = "validity.variable", 
                          values_to = "value") |>
      dplyr::group_by(k, validity.variable) |>
      dplyr::summarize(mean = mean(value)) |>
      ggplot2::ggplot(ggplot2::aes(x = k, y = mean, color = k)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~validity.variable, scale = "free") +
      ggplot2::scale_color_manual(values = pal) +
      ggplot2::ggtitle("Cap Noir") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.text.x = ggplot2::element_text(size = 15),
                     axis.text.y = ggplot2::element_text(size = 15),
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     strip.text.x = ggplot2::element_text(size = 15),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     legend.position = "none")
    # save plot 
    ggplot2::ggsave("output/sites/clustering with PCs/findk_validity_measures_PCs_means_CN.jpg",
                    scale = 1,
                    height = 6, width = 8)
    
    ########################### POINTE SUZANNE #################################
    
    find_k_output_PS <- find_k_output$PS
    
    find_k_output_PS |>
      dplyr::mutate(k = as.factor(k)) |>
      tidyr::pivot_longer(cols = c("separation":"min.clust.size"), 
                          names_to = "validity.variable", 
                          values_to = "value") |>
      dplyr::group_by(k, validity.variable) |>
      dplyr::summarize(mean = mean(value)) |>
      ggplot2::ggplot(ggplot2::aes(x = k, y = mean, color = k)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~validity.variable, scale = "free") +
      ggplot2::scale_color_manual(values = pal) +
      ggplot2::ggtitle("Pointe Suzanne") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.text.x = ggplot2::element_text(size = 15),
                     axis.text.y = ggplot2::element_text(size = 15),
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     strip.text.x = ggplot2::element_text(size = 15),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     legend.position = "none")
    # save plot 
    ggplot2::ggsave("output/sites/clustering with PCs/findk_validity_measures_PCs_means_PS.jpg",
                    scale = 1,
                    height = 6, width = 8)
    
  } else if (type == "all") {
    # set color palette 
    
    diff <- length(unique(find_k_output$k)) - 7
    
    possible_col <- c("#CD4F38FF", "#3D4F7DFF", 
                      "#657060FF", "#EAD890FF") 
    
    pal <- c(ghibli::ghibli_palettes$YesterdayMedium, 
             possible_col[1:diff])
    
    find_k_output |>
      dplyr::mutate(k = as.factor(k)) |>
      tidyr::pivot_longer(cols = c("separation":"min.clust.size"), 
                          names_to = "validity.variable", 
                          values_to = "value") |>
      dplyr::group_by(k, validity.variable) |>
      dplyr::summarize(mean = mean(value)) |>
      ggplot2::ggplot(ggplot2::aes(x = k, y = mean, color = k)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~validity.variable, scale = "free") +
      ggplot2::scale_color_manual(values = pal) +
      ggplot2::ggtitle("All scats") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     axis.text.x = ggplot2::element_text(size = 15),
                     axis.text.y = ggplot2::element_text(size = 15),
                     axis.title.y = ggplot2::element_text(size = 16, 
                                                          face = "bold"), 
                     strip.text.x = ggplot2::element_text(size = 15),
                     title = ggplot2::element_text(size = 17, 
                                                   face = "bold"),
                     legend.position = "none")
    # save plot 
    ggplot2::ggsave("output/sites/clustering with PCs/findk_validity_measures_PCs_means_all_scats.jpg",
                    scale = 1,
                    height = 6, width = 8)
  }

  
  
}





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
                   dplyr::group_by(site, cluster, Nutrient) |>
                   dplyr::summarise(mean = round(mean(conc_mg_kg_dw), 3), 
                                    sd = round(sd(conc_mg_kg_dw), 3)) |>
                   tidyr::pivot_wider(names_from = Nutrient, 
                                      values_from = c(mean, sd), 
                                      names_sep = "_"), 
                 scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Pointe Suzanne") |>
                   dplyr::mutate(cluster = clust_vec_PS) |>
                   tidyr::pivot_longer(cols = c(Fe:Co), 
                                       names_to = "Nutrient", 
                                       values_to = "conc_mg_kg_dw") |>
                   dplyr::group_by(site, cluster, Nutrient) |>
                   dplyr::summarise(mean = round(mean(conc_mg_kg_dw), 3), 
                                    sd = round(sd(conc_mg_kg_dw), 3)) |>
                   tidyr::pivot_wider(names_from = Nutrient, 
                                      values_from = c(mean, sd), 
                                      names_sep = "_"))
  
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
