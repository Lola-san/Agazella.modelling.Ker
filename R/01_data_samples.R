################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# 01_diet_data.R
#
################################################################################


#' 
#' 
# load excel files  
load_xl <- function(pathxl) {
  readxl::read_xlsx(pathxl)
}

#'
#'
#'
#'
# clean file and summarise data on samples
summary_fish_samples <- function(fish_tab, 
                                 compo_results_fish) {
  table <- fish_tab |>
    dplyr::filter(Code_new_format %in% compo_results_fish$Code_sample) |>
    # add data on habitat as given on Fishbase
    dplyr::mutate(Habitat = dplyr::case_when(Species %in% c("Gobionotothen acuta", 
                                                            "Channichthys rhinoceratus",
                                                            "Dissostichus eleginoides",
                                                            "Mancopsetta mancopsetta",
                                                            "Electrona antarctica") ~ "Demersal", 
                                             Species %in% c("Lepidonotothen squamifrons", 
                                                            "Champsocephalus gunnari",
                                                            "Lindbergichthys mizops",
                                                            "Muraenolepis sp",
                                                            "Gymnoscopelus piabilis",
                                                            "Gymnoscopelus bolini") ~ "Benthopelagic", 
                                             Species %in% c("Bathydraco antarcticus",
                                                            "Macrourus carinatus",
                                                            "Paradiplospinus gracilis",
                                                            "Echiodon cryomargarites") ~ "Bathydemersal", 
                                             Species %in% c("Krefftichthys anderssoni",
                                                            "Melanostigma gelatinosum",
                                                            "Bathylagus tenuis",
                                                            "Luciosudis normani", 
                                                            "Gymnoscopelus braueri", 
                                                            "Gymnoscopelus fraseri", 
                                                            "Gymnoscopelus nicholsi", 
                                                            "Electrona subaspera",
                                                            "Poromitra crassiceps", 
                                                            "Nansenia antarctica",
                                                            "Electrona carlsbergi", 
                                                            "Protomyctophum andriashevi",
                                                            "Protomyctophum bolini", 
                                                            "Protomyctophum choriodon",
                                                            "Stomias sp",
                                                            "Idiacanthus atlanticus",
                                                            "Arctozenus risso",
                                                            "Notolepis coatsi", 
                                                            "Protomyctophum tenisoni") ~ "Bathypelagic")) |>
    dplyr::group_by(Family, Species, Habitat) |>
    dplyr::mutate(SL_cm = as.integer(SL_cm)) |> # generates warnings because
    # of samples with approximate length (*XX) as they were damaged
    dplyr::summarize(n = dplyr::n_distinct(Code_new_format), 
                     length_mean = mean(SL_cm, na.rm = TRUE), 
                     length_min = min(SL_cm, na.rm = TRUE), 
                     length_max = max(SL_cm, na.rm = TRUE), 
                     length_sd = sd(SL_cm, na.rm = TRUE),  
                     length_cv = sd(SL_cm, na.rm = TRUE)/mean(SL_cm, na.rm = TRUE),
                     H20_mean = mean(Water_percent), 
                     H20_min = min(Water_percent), 
                     H20_max = max(Water_percent), 
                     H20_sd = sd(Water_percent), 
                     H20_cv = H20_sd/H20_mean)
  
  openxlsx::write.xlsx(table, 
                       file = "output/summary_fish_samples_sp.xlsx")
}


#'
#'
#'
#'
# select only nutrient of interest, get rid of technical outliers/contaminated
# samples and set statistical outliers to higher quantiles values
set_up_fish_compo <- function(compo_results_fish) {
  
  # for statistical outliers
  # calculate quantiles 
  all_quant <- compo_results_fish |>
    # exclude unwanted samples 
    dplyr::filter(!(Code_sample %in% c("2010PII_MACRCAR_CHA103_MC07",
                                       "2005_GYMNFRA_GF11",
                                       "2005_STOMSP_SS09",
                                       "2005_NOTOCOA_NC03",
                                       "2005_PARAGRA_PG06"))) |>
    # get rid of unneeded nutrient
    dplyr::select(c(Code_sample, Fe, Zn, Ni)) |> 
    tidyr::pivot_longer(cols = c("Fe":"Ni"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(high_quant = quantile(concentration_mg_g_dw, 0.975)) |> 
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = high_quant)
  
  quant_zn <- all_quant$Zn
  quant_ni <- all_quant$Ni
  quant_fe <- all_quant$Fe
  
  
  # set up the table 
  compo_results_fish |> 
    # technical outliers/possibly contaminated samples
    dplyr::filter(!(Code_sample %in% c("2005_PROTAND_PA03",
                                       "2010PII_ARCTRIS_CHA94_AR01"))) |>
    # get rid of unwanted nutrients (+ with only trace)
    dplyr::select(-c(Mo, V, Ag, Cr, Pb, Cd, Sr)) |>
    # statistical outliers
    dplyr::mutate(Fe = dplyr::case_when(Code_sample %in% c("2005_STOMSP_SS09",
                                                           "2005_NOTOCOA_NC03",
                                                           "2005_PARAGRA_PG06") ~ quant_fe, 
                                        TRUE ~ Fe),
                  Ni = dplyr::case_when(Code_sample == "2010PII_MACRCAR_CHA103_MC07" ~ quant_ni,
                                        TRUE ~ Ni), 
                  Zn = dplyr::case_when(Code_sample == "2005_GYMNFRA_GF11" ~ quant_zn,
                                        TRUE ~ Zn))
  
}


#'
#'
#'
#'
#'
# clean file and summarise data on samples
summary_scat_samples <- function(scat_tab) {
  
  table <- scat_tab |>
    dplyr::group_by(site, date_collecte) |>
    dplyr::mutate(HPI_0 = dplyr::case_when(index_hard_parts == 0 ~ 1,
                                           TRUE ~ 0),
                  HPI_1 = dplyr::case_when(index_hard_parts == 1 ~ 1,
                                           TRUE ~ 0),
                  HPI_2 = dplyr::case_when(index_hard_parts == 2 ~ 1,
                                           TRUE ~ 0), 
                  HPI_3 = dplyr::case_when(index_hard_parts == 3 ~ 1,
                                           TRUE ~ 0)) |>
    dplyr::summarize(n = dplyr::n_distinct(Code_sample), 
                     wweight_mean = mean(ww), 
                     wweight_min = min(ww), 
                     wweight_max = max(ww), 
                     H20_mean = mean(water_percent), 
                     H20_min = min(water_percent), 
                     H20_max= max(water_percent), 
                     percent_HPI0 = 100*(sum(HPI_0)/n), 
                     percent_HPI1 = 100*(sum(HPI_1)/n), 
                     percent_HPI2 = 100*(sum(HPI_2)/n), 
                     percent_HPI3 = 100*(sum(HPI_3)/n))
  
  openxlsx::write.xlsx(table, 
                       file = "output/summary_scat_samples_sp.xlsx")
}


