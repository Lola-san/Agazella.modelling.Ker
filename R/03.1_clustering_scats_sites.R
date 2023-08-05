################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 03.1_clustering_scats_sites.R
#
################################################################################


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
    ggplot2::ggsave("output/sites/PCA_biplot_sites_CN.jpg",
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
    ggplot2::ggsave("output/sites/PCA_biplot_sites_PS.jpg",
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
    ggplot2::ggsave("output/sites/PCA_biplot_all_scats.jpg",
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
                         file = "output/sites/clust_PCs_validity_measures_CN.xlsx")
    
    
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
                         file = "output/sites/clust_PCs_validity_measures_PS.xlsx")
    
    list(CN = clust_output_CN, PS = clust_output_PS)
    
    } else if (type == "all") {

    ########################### CAP NOIR #########################################
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
                         file = "output/sites/clust_PCs_validity_measures_all_scats.xlsx")
    
    clust_output
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
      ggplot2::scale_color_manual(values = c("1" = "#D8AF39FF",
                                             "2" = "#AE93BEFF",
                                             "3" = "#3D4F7DFF", 
                                             "4" = "#B4DAE5FF")) +
      ggplot2::coord_flip() +
      ggplot2::ylim(-2, 25) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Cluster",
                                                     override.aes = list(shape = 12)))  +
      ggplot2::theme_classic()
    
    # save plot 
    ggplot2::ggsave("output/sites/dendrogram_clust_CN.jpg",
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
      ggplot2::scale_color_manual(values = c("1" = "#D8AF39FF",
                                             "2" = "#AE93BEFF",
                                             "3" = "#3D4F7DFF", 
                                             "4" = "#B4DAE5FF")) +
      ggplot2::coord_flip() +
      ggplot2::ylim(-2, 25) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Cluster",
                                                     override.aes = list(shape = 12)))  +
      ggplot2::theme_classic()
    
    # save plot 
    ggplot2::ggsave("output/sites/dendrogram_clust_PS.jpg",
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
      ggplot2::scale_color_manual(values = c("1" = "#D8AF39FF",
                                             "2" = "#AE93BEFF",
                                             "3" = "#3D4F7DFF", 
                                             "4" = "#B4DAE5FF")) +
      ggplot2::coord_flip() +
      ggplot2::ylim(-2, 25) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Cluster",
                                                     override.aes = list(shape = 12)))  +
      ggplot2::theme_classic()
    
    # save plot 
    ggplot2::ggsave("output/sites/dendrogram_clust_all_scats.jpg",
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
                                           "CN27", "CN26", "CN23", "CN29", "CN16", 
                                           "CN22", "CN15", "CN13", "CN09", "CN20", 
                                           "CN18", "CN14", "CN11", "CN01", 
                                           # clust 2
                                           "CN17", "CN07", "CN19", "CN08", "CN25", 
                                           "CN05", "CN04", "CN06", "CN03",
                                           # clust 3
                                           "CN24", "CN21", "CN10", "CN28"
                                         ))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative, 
                                     fill = factor(cluster)), 
                        stat = "identity", 
                        position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Code_sample) + 
      ggplot2::scale_fill_manual(values = c("1" = "#D8AF39FF",
                                            "2" = "#AE93BEFF",
                                            "3" = "#3D4F7DFF", 
                                            "4" = "#B4DAE5FF")) +
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
    ggplot2::ggsave("output/sites/clust_scat_compo_rel_CapNoir.jpg",
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
                                           "PS07", "PS28", "PS12", "PS08", "PS21",
                                           "PS25",
                                           # clust 2
                                           "PS29", "PS02", "PS23", "PS09", "PS05",
                                           "PS04", "PS06", "PS27", "PS19", "PS15", 
                                           "PS31", "PS16", "PS14", "PS30", "PS13",
                                           # clust 3
                                           "PS26", "PS10", "PS24", 
                                           "PS22", "PS11", "PS03" 
                                         ))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = conc_relative, 
                                     fill = factor(cluster)), 
                        stat = "identity", 
                        position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Code_sample) + 
      ggplot2::scale_fill_manual(values = c("1" = "#D8AF39FF",
                                            "2" = "#AE93BEFF",
                                            "3" = "#3D4F7DFF", 
                                            "4" = "#B4DAE5FF")) +
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
    ggplot2::ggsave("output/sites/clust_scat_compo_rel_PSuzanne.jpg",
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
      ggplot2::scale_fill_manual(values = c("1" = "#D8AF39FF",
                                            "2" = "#AE93BEFF",
                                            "3" = "#3D4F7DFF", 
                                            "4" = "#B4DAE5FF")) +
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
    ggplot2::ggsave("output/sites/clust_scat_compo_rel_all_scats.jpg",
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
                                                 "Co"))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                         fill = factor(cluster)), 
                            position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Nutrient, scales = "free_y") + 
      ggplot2::scale_fill_manual(values = c("1" = "#D8AF39FF",
                                            "2" = "#AE93BEFF",
                                            "3" = "#3D4F7DFF", 
                                            "4" = "#B4DAE5FF")) +
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
    ggplot2::ggsave("output/sites/clust_scat_compo_abs_CapNoir.jpg",
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
                                                 "Co"))) |> 
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                         fill = factor(cluster)), 
                            position = ggplot2::position_dodge(1)) +
      ggplot2::facet_wrap(~ Nutrient, scales = "free_y") + 
      ggplot2::scale_fill_manual(values = c("1" = "#D8AF39FF",
                                            "2" = "#AE93BEFF",
                                            "3" = "#3D4F7DFF", 
                                            "4" = "#B4DAE5FF")) +
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
    ggplot2::ggsave("output/sites/clust_scat_compo_abs_PSuzanne.jpg",
                    scale = 1,
                    height = 8, width = 12
    )
    
  } else if (type == "all") {
    
    # assign each sample to its cluster
    clust_vec <- res_clust$cluster
    
    ############################### CAP NOIR #####################################
    scat_compo_tib |>
      dplyr::mutate(sum_nut = Fe + Zn + Cu + Mn + Se + Co) |>
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
      ggplot2::scale_fill_manual(values = c("1" = "#D8AF39FF",
                                            "2" = "#AE93BEFF",
                                            "3" = "#3D4F7DFF", 
                                            "4" = "#B4DAE5FF")) +
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
    ggplot2::ggsave("output/sites/clust_scat_compo_abs_all_scats.jpg",
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
                       file = "output/sites/clust_percent_sites.xlsx")

  table 
  
}
