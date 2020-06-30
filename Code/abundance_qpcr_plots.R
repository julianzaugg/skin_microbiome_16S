# Script to generate the mean relative abundance and bacterial load figure for publication

# invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)



common_theme <- theme(
  panel.border = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  strip.background = element_rect(fill = "white", colour = "white", size = 1),
  strip.text = element_text(size = 6),
  legend.key=element_blank(),
  legend.direction="vertical",
  legend.background = element_rect(colour ="white", size = .3),
  legend.text.align = 0,
  legend.title = element_text(size=10, face="bold"),
  legend.title.align = 0.5,
  legend.margin = margin(c(2,2,2,2)),
  legend.key.height= unit(.3,"cm"),
  legend.key.width = unit(.3,"cm"),
  legend.text = element_text(size = 8),
  axis.line = element_line(colour = "black", size = 0.5),
  axis.text = element_text(size = 6, colour = "black"),
  axis.title = element_text(size = 7,face = "bold"),
  complete = F,
  plot.title = element_text(size = 8))

# ------------------------------------------------------------------------------------------
# Various colour palettes
my_colour_palette_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_palette_12_soft <-c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621", "#c75a93", "#7c7731")
my_colour_palette_35_distinct <- c("#00b57e","#5866ff","#d30051","#fc2fdd","#bbe6b5","#4c5817","#ff8e19","#384c9f","#ff7bc1","#5feff6","#a3d700","#d462ff","#0170a9","#ff9967","#ddc1ff","#01ed7d","#9600c1","#00bb16","#f29aff","#e09b8c","#e04500","#774d00","#b88400","#0097db","#a5008b","#84f37b","#00a993","#9f1f2d","#515485","#ff5585","#006b24","#014ad3","#ff6e56","#dce25e","#9c1f4a")
# ------------------------------------------------------------------------------------------
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
source("Code/helper_functions.R")

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC and SCC lesions. 
metadata.df <- metadata.df[!metadata.df$Sample_type == "negative",]

# forearm_PL_swab_ids <- c("1382","1384", "1471","1562","1600","1648","1650")
#forearm_swab_ids <- c("1383","1385","1470","1561","1599","1647","1649")
forearm_swab_ids_IC = c("1358","1359","1492","1493")
forearm_swab_ids_IS <- c("1383","1385","1470","1561","1599","1647","1649","1382","1384", "1471","1562","1600","1648","1650")
# forearm_indices <- c("R1383_J1425", "SA6550_J1427", "R1368_J1477", "R1460_J1425", "R1498_J1425", "SB4909_J1426", "SB4911_J1426")
# R1498_J1425  R1460_J1425  R1368_J1477  SA6550_J1427 SB4909_J1426
# SB4911_J1426  R1498_J1425 R1383_J1425

# Load filtered/processed abundance data with metadata
genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/genus_counts_abundances_and_metadata.csv")

genus_data.df <- genus_data.df[genus_data.df$Sample %in% rownames(metadata.df),]

temp <- genus_data.df[genus_data.df$Swab_ID %in% forearm_swab_ids_IS,]
forearm_swab_ids_IS %in% genus_data.df$Swab_ID
forearm_swab_ids_IC %in% genus_data.df$Swab_ID

# ------------------------------
# FIXME, want to show the following
# HS PDS AK SCC_PL(forearm) SCC(forearm) SCC_PL SCC

# genus_data.df[genus_data.df$Swab_ID %in% c(forearm_swab_ids, forearm_swab_ids_IC),]

# Set levels
genus_data.df$Sample_type <- factor(genus_data.df$Sample_type, levels = c("HS", "PDS", "AK", "SCC_PL","SCC"))

# Create taxonomy label
genus_data.df$taxonomy_label <- with(genus_data.df, paste0(Domain,";", Class,";", Genus))

immunosuppressed_genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed")
immunocompetent_genus_data.df <- subset(genus_data.df, Cohort == "immunocompetent")
immunosuppressed_forearm_genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" & Swab_ID %in% forearm_swab_ids_IS)
immunocompetent_forearm_genus_data.df <- subset(genus_data.df, Cohort == "immunocompetent" & Swab_ID %in% forearm_swab_ids_IC)


immunosuppressed_genus_data.df$Sample_type <- factor(immunosuppressed_genus_data.df$Sample_type, levels = rev(c("HS","PDS", "AK", "SCC_PL", "SCC")))
immunocompetent_genus_data.df$Sample_type <- factor(immunocompetent_genus_data.df$Sample_type, levels = rev(c("PDS", "AK", "SCC_PL", "SCC")))
immunosuppressed_forearm_genus_data.df$Sample_type <- factor(immunosuppressed_forearm_genus_data.df$Sample_type, levels = rev(c("SCC_PL", "SCC")))
immunocompetent_forearm_genus_data.df$Sample_type <- factor(immunocompetent_forearm_genus_data.df$Sample_type, levels = rev(c("SCC_PL", "SCC")))

# Generate full genus summary for each sample type
immunosuppressed_genus_summary.df <- generate_taxa_summary(immunosuppressed_genus_data.df,
                                                           taxa_column = "taxonomy_label", 
                                                           group_by_columns = c("Sample_type"))
immunocompetent_genus_summary.df <- generate_taxa_summary(immunocompetent_genus_data.df,
                                                          taxa_column = "taxonomy_label", 
                                                          group_by_columns = c("Sample_type"))

immunosuppressed_forearm_genus_summary.df <- generate_taxa_summary(immunosuppressed_forearm_genus_data.df,
                                                                   taxa_column = "taxonomy_label", 
                                                                   group_by_columns = c("Sample_type"))

immunocompetent_forearm_genus_summary.df <- generate_taxa_summary(immunocompetent_forearm_genus_data.df,
                                                                   taxa_column = "taxonomy_label", 
                                                                   group_by_columns = c("Sample_type"))
unique(immunosuppressed_forearm_genus_summary.df$Sample_type)
unique(immunocompetent_forearm_genus_summary.df$Sample_type)

# Identify the top genus for each lesion type
immunosuppressed_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_genus_summary.df, 
                                                                 grouping_variables = c("Sample_type"),
                                                                 abundance_column = "Mean_relative_abundance",
                                                                 my_top_n = 5)

immunocompetent_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunocompetent_genus_summary.df, 
                                                                grouping_variables = c("Sample_type"),
                                                                abundance_column = "Mean_relative_abundance",
                                                                my_top_n = 5)

immunosuppressed_top_forearm_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_forearm_genus_summary.df, 
                                                                         grouping_variables = c("Sample_type"),
                                                                         abundance_column = "Mean_relative_abundance",
                                                                         my_top_n = 5)
immunocompetent_top_forearm_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunocompetent_forearm_genus_summary.df, 
                                                                         grouping_variables = c("Sample_type"),
                                                                         abundance_column = "Mean_relative_abundance",
                                                                         my_top_n = 5)

# Create a unique list of the top taxa across all the cohorts / groups
both_cohorts_lesions_top_genus <- unique(c(immunosuppressed_top_genus_summary.df$taxonomy_label,
                                           immunocompetent_top_genus_summary.df$taxonomy_label,
                                           immunosuppressed_top_forearm_genus_summary.df$taxonomy_label,
                                           immunocompetent_top_forearm_genus_summary.df$taxonomy_label))

# both_cohorts_lesions_top_genus <- unique(c(immunosuppressed_top_genus_summary.df$taxonomy_label, 
                                           # immunocompetent_top_genus_summary.df$taxonomy_label))
# union(union(union(immunosuppressed_top_genus_summary.df$taxonomy_label,
#             immunocompetent_top_genus_summary.df$taxonomy_label),
#       immunosuppressed_top_forearm_genus_summary.df$taxonomy_label),
#     immunocompetent_top_forearm_genus_summary.df$taxonomy_label)

# Create palette based on unique set
both_cohorts_genus_palette <- setNames(my_colour_palette_35_distinct[1:length(both_cohorts_lesions_top_genus)], both_cohorts_lesions_top_genus)
both_cohorts_genus_palette["Other"] <- "grey"

# 
# immunosuppressed_top_genus_summary_limited.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_genus_summary.df,
#                                                                  grouping_variables = c("Sample_type"),
#                                                                  abundance_column = "Mean_relative_abundance",
#                                                                  my_top_n = 5)
# 
# immunocompetent_top_genus_summary_limited.df <- filter_summary_to_top_n(taxa_summary = immunocompetent_genus_summary.df, 
#                                                                 grouping_variables = c("Sample_type"),
#                                                                 abundance_column = "Mean_relative_abundance",
#                                                                 my_top_n = 5)

# Take the full table and re-label any taxa not in the top to "Other"
# immunosuppressed_genus_summary.df[!immunosuppressed_genus_summary.df$taxonomy_label %in% immunosuppressed_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# immunocompetent_genus_summary.df[!immunocompetent_genus_summary.df$taxonomy_label %in% immunocompetent_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# immunosuppressed_forearm_genus_summary.df[!immunosuppressed_forearm_genus_summary.df$taxonomy_label %in% immunosuppressed_top_forearm_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"

immunosuppressed_genus_summary.df[!immunosuppressed_genus_summary.df$taxonomy_label %in% both_cohorts_lesions_top_genus,]$taxonomy_label <- "Other"
immunocompetent_genus_summary.df[!immunocompetent_genus_summary.df$taxonomy_label %in% both_cohorts_lesions_top_genus,]$taxonomy_label <- "Other"
immunosuppressed_forearm_genus_summary.df[!immunosuppressed_forearm_genus_summary.df$taxonomy_label %in% both_cohorts_lesions_top_genus,]$taxonomy_label <- "Other"
immunocompetent_forearm_genus_summary.df[!immunocompetent_forearm_genus_summary.df$taxonomy_label %in% both_cohorts_lesions_top_genus,]$taxonomy_label <- "Other"

# Normalise the Mean_relative_abundance values within each sample type
sum(immunosuppressed_genus_summary.df$Mean_relative_abundance)
sum(immunosuppressed_genus_summary.df$Normalised_mean_relative_abundance)

immunosuppressed_genus_summary.df <- 
  immunosuppressed_genus_summary.df %>% 
  dplyr::group_by(Sample_type) %>% 
  dplyr::mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% 
  as.data.frame()

immunocompetent_genus_summary.df <- 
  immunocompetent_genus_summary.df %>% 
  dplyr::group_by(Sample_type) %>% 
  dplyr::mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% 
  as.data.frame()

immunosuppressed_forearm_genus_summary.df <-
  immunosuppressed_forearm_genus_summary.df %>%
  dplyr::group_by(Sample_type) %>%
  dplyr::mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>%
  as.data.frame()

immunocompetent_forearm_genus_summary.df <-
  immunocompetent_forearm_genus_summary.df %>%
  dplyr::group_by(Sample_type) %>%
  dplyr::mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>%
  as.data.frame()


immunosuppressed_genus_summary.df[immunosuppressed_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunocompetent_genus_summary.df[immunocompetent_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunosuppressed_forearm_genus_summary.df[immunosuppressed_forearm_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunocompetent_forearm_genus_summary.df[immunocompetent_forearm_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA

# Need a single entry for the Other group
# TODO - check abundance values, non-zero etc
immunosuppressed_genus_summary.df <-
  immunosuppressed_genus_summary.df %>% 
  group_by(Sample_type, taxonomy_label) %>% 
  dplyr::summarise(Mean_relative_abundance = max(Mean_relative_abundance), 
                   Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% 
  as.data.frame()

immunocompetent_genus_summary.df <- 
  immunocompetent_genus_summary.df %>% 
  group_by(Sample_type, taxonomy_label) %>% 
  dplyr::summarise(Mean_relative_abundance = max(Mean_relative_abundance), 
                   Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% 
  as.data.frame()

immunosuppressed_forearm_genus_summary.df <-
  immunosuppressed_forearm_genus_summary.df %>%
  group_by(Sample_type, taxonomy_label) %>%
  dplyr::summarise(Mean_relative_abundance = max(Mean_relative_abundance),
                   Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>%
  as.data.frame()

immunocompetent_forearm_genus_summary.df <-
  immunocompetent_forearm_genus_summary.df %>%
  group_by(Sample_type, taxonomy_label) %>%
  dplyr::summarise(Mean_relative_abundance = max(Mean_relative_abundance),
                   Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>%
  as.data.frame()


immunosuppressed_genus_summary.df[immunosuppressed_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunocompetent_genus_summary.df[immunocompetent_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunosuppressed_forearm_genus_summary.df[immunosuppressed_forearm_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunocompetent_forearm_genus_summary.df[immunocompetent_forearm_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA

# Calculate the mean qPCR totals for each cohort + lesion type
# FIXME? Metadata contains values only for samples passing QC. Would be interesting to use all the values.
# FIXME need values for forearm scc_pl and scc samples as well
Mean_qpcr_totals.df <-
  metadata.df %>% 
  group_by(Cohort, Sample_type) %>% 
  dplyr::summarise(N_samples = n_distinct(Index), #!!!
                   # Mean_Staph_spp_qPCR = mean(Staph_spp_qPCR, na.rm = T),
                   # Mean_S_aureus_qPCR = mean(S_aureus_qPCR, na.rm = T),
                   Mean_qPCR_16S = mean(qPCR_16S, na.rm = T)
  ) %>% 
  as.data.frame()

# metadata.df[metadata.df$Swab_ID %in% forearm_swab_ids_IC,]$Index

Mean_qpcr_totals_forearm.df <-
  metadata.df %>% 
  filter(Swab_ID %in% c(forearm_swab_ids_IS,forearm_swab_ids_IC)) %>%
  # filter(Swab_ID %in% forearm_swab_ids_IC) %>%
  group_by(Cohort, Sample_type) %>% 
  dplyr::summarise(N_samples = n_distinct(Index), #!!!
                   # Mean_Staph_spp_qPCR = mean(Staph_spp_qPCR, na.rm = T),
                   # Mean_S_aureus_qPCR = mean(S_aureus_qPCR, na.rm = T),
                   Mean_qPCR_16S = mean(qPCR_16S, na.rm = T)
  ) %>% 
  as.data.frame()

immunosuppressed_genus_summary.df$Cohort <- "immunosuppressed"
immunocompetent_genus_summary.df$Cohort <- "immunocompetent"
immunosuppressed_forearm_genus_summary.df$Cohort <- "immunosuppressed"
immunocompetent_forearm_genus_summary.df$Cohort <- "immunocompetent"

# immunosuppressed_genus_summary.df$Mean_relative_abundance_staph_spp_proportional <- NA
# immunosuppressed_genus_summary.df$Mean_relative_abundance_saureus_proportional <- NA

# immunocompetent_genus_summary.df$Mean_relative_abundance_staph_spp_proportional <- NA
# immunocompetent_genus_summary.df$Mean_relative_abundance_saureus_proportional <- NA
# immunosuppressed_forearm_genus_summary.df$Mean_relative_abundance_staph_spp_proportional <- NA
# immunosuppressed_forearm_genus_summary.df$Mean_relative_abundance_saureus_proportional <- NA

immunosuppressed_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- NA
immunocompetent_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- NA
immunosuppressed_forearm_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- NA
immunocompetent_forearm_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- NA

immunosuppressed_genus_summary.df <- left_join(immunosuppressed_genus_summary.df, Mean_qpcr_totals.df, by = c("Cohort" = "Cohort", "Sample_type" = "Sample_type"))
immunocompetent_genus_summary.df <- left_join(immunocompetent_genus_summary.df, Mean_qpcr_totals.df, by = c("Cohort" = "Cohort", "Sample_type" = "Sample_type"))
immunosuppressed_forearm_genus_summary.df <- left_join(immunosuppressed_forearm_genus_summary.df, Mean_qpcr_totals_forearm.df, by = c("Cohort" = "Cohort", "Sample_type" = "Sample_type"))
immunocompetent_forearm_genus_summary.df <- left_join(immunocompetent_forearm_genus_summary.df, Mean_qpcr_totals_forearm.df, by = c("Cohort" = "Cohort", "Sample_type" = "Sample_type"))

# Calculate abundance value proportional to qPCR totals
# immunosuppressed_genus_summary.df$Mean_relative_abundance_staph_spp_proportional <- 
#   with(immunosuppressed_genus_summary.df, Normalised_mean_relative_abundance * Mean_Staph_spp_qPCR)
# immunosuppressed_genus_summary.df$Mean_relative_abundance_saureus_proportional <- 
#   with(immunosuppressed_genus_summary.df, Normalised_mean_relative_abundance * Mean_S_aureus_qPCR)
# 
# immunocompetent_genus_summary.df$Mean_relative_abundance_staph_spp_proportional <- 
#   with(immunocompetent_genus_summary.df, Normalised_mean_relative_abundance * Mean_Staph_spp_qPCR)
# immunocompetent_genus_summary.df$Mean_relative_abundance_saureus_proportional <- 
#   with(immunocompetent_genus_summary.df, Normalised_mean_relative_abundance * Mean_S_aureus_qPCR)

immunosuppressed_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- 
  with(immunosuppressed_genus_summary.df, Normalised_mean_relative_abundance * Mean_qPCR_16S)
immunocompetent_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- 
  with(immunocompetent_genus_summary.df, Normalised_mean_relative_abundance * Mean_qPCR_16S)

immunosuppressed_forearm_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- 
  with(immunosuppressed_forearm_genus_summary.df, Normalised_mean_relative_abundance * Mean_qPCR_16S)
immunocompetent_forearm_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- 
  with(immunocompetent_forearm_genus_summary.df, Normalised_mean_relative_abundance * Mean_qPCR_16S)


# Order taxonomy by the abundance. This is only approximate.
# The Other group should be last as it is the largest. This is not enforced however, so just be aware of it
immunosuppressed_genus_summary.df <- immunosuppressed_genus_summary.df %>% group_by(Sample_type) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(unique(immunosuppressed_genus_summary.df$taxonomy_label)[unique(immunosuppressed_genus_summary.df$taxonomy_label) != "Other"], "Other")
immunosuppressed_genus_summary.df$taxonomy_label <- factor(immunosuppressed_genus_summary.df$taxonomy_label, levels = my_levels)
immunosuppressed_genus_summary.df$value_label <- as.character(lapply(immunosuppressed_genus_summary.df$Normalised_mean_relative_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), "")))

immunocompetent_genus_summary.df <- immunocompetent_genus_summary.df %>% group_by(Sample_type) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(unique(immunocompetent_genus_summary.df$taxonomy_label)[unique(immunocompetent_genus_summary.df$taxonomy_label) != "Other"], "Other")
immunocompetent_genus_summary.df$taxonomy_label <- factor(immunocompetent_genus_summary.df$taxonomy_label, levels = my_levels)
immunocompetent_genus_summary.df$value_label <- as.character(lapply(immunocompetent_genus_summary.df$Normalised_mean_relative_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), "")))

immunosuppressed_forearm_genus_summary.df <- immunosuppressed_forearm_genus_summary.df %>% group_by(Sample_type) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(unique(immunosuppressed_forearm_genus_summary.df$taxonomy_label)[unique(immunosuppressed_forearm_genus_summary.df$taxonomy_label) != "Other"], "Other")
immunosuppressed_forearm_genus_summary.df$taxonomy_label <- factor(immunosuppressed_forearm_genus_summary.df$taxonomy_label, levels = my_levels)
immunosuppressed_forearm_genus_summary.df$value_label <- as.character(lapply(immunosuppressed_forearm_genus_summary.df$Normalised_mean_relative_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), "")))

immunocompetent_forearm_genus_summary.df <- immunocompetent_forearm_genus_summary.df %>% group_by(Sample_type) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(unique(immunocompetent_forearm_genus_summary.df$taxonomy_label)[unique(immunocompetent_forearm_genus_summary.df$taxonomy_label) != "Other"], "Other")
immunocompetent_forearm_genus_summary.df$taxonomy_label <- factor(immunocompetent_forearm_genus_summary.df$taxonomy_label, levels = my_levels)
immunocompetent_forearm_genus_summary.df$value_label <- as.character(lapply(immunocompetent_forearm_genus_summary.df$Normalised_mean_relative_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), "")))



immunosuppressed_genus_summary.df$Sample_type <- factor(immunosuppressed_genus_summary.df$Sample_type, levels = rev(c("HS", "PDS", "AK", "SCC_PL","SCC")))
immunocompetent_genus_summary.df$Sample_type <- factor(immunocompetent_genus_summary.df$Sample_type, levels = rev(c("PDS", "AK", "SCC_PL","SCC")))
immunosuppressed_forearm_genus_summary.df$Sample_type <- factor(immunosuppressed_forearm_genus_summary.df$Sample_type, levels = rev(c("SCC_PL","SCC")))
immunocompetent_forearm_genus_summary.df$Sample_type <- factor(immunocompetent_forearm_genus_summary.df$Sample_type, levels = rev(c("SCC_PL","SCC")))

# TODO - now combine the forearm with the full dataset, requires new Sample_Type_label
immunosuppressed_forearm_genus_summary.df$Forearm <- "Forearm"
immunosuppressed_genus_summary.df$Forearm <- "Forearm"
immunosuppressed_genus_summary.df[immunosuppressed_genus_summary.df$Sample_type %in% c("SCC_PL", "SCC"),]$Forearm <- "All body sites"
immunosuppressed_genus_summary_combined_forearm.df <- rbind(immunosuppressed_genus_summary.df, immunosuppressed_forearm_genus_summary.df)

immunocompetent_forearm_genus_summary.df$Forearm <- "Forearm"
immunocompetent_genus_summary.df$Forearm <- "Forearm"
immunocompetent_genus_summary.df[immunocompetent_genus_summary.df$Sample_type %in% c("SCC_PL", "SCC"),]$Forearm <- "All body sites"
immunocompetent_genus_summary_combined_forearm.df <- rbind(immunocompetent_genus_summary.df, immunocompetent_forearm_genus_summary.df)

immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label <- 
  with(immunosuppressed_genus_summary_combined_forearm.df, paste0(Sample_type, " (", Forearm, ")", " n=",N_samples))

# immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label <-
#   factor(immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label,
#          levels = rev(c("HS (Forearm)", "PDS (Forearm)", "AK (Forearm)",
#                     "SCC_PL (Forearm)", "SCC (Forearm)",
#                     "SCC_PL (All body sites)", "SCC (All body sites)")))

immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label  <- as.character(immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label)

immunocompetent_genus_summary_combined_forearm.df$Sample_type_label <- 
  with(immunocompetent_genus_summary_combined_forearm.df, paste0(Sample_type, " (", Forearm, ")", " n=", N_samples))

# immunocompetent_genus_summary_combined_forearm.df$Sample_type_label <- 
#   factor(immunocompetent_genus_summary_combined_forearm.df$Sample_type_label,
#          levels = rev(c("PDS (Forearm)", "AK (Forearm)", 
#                         "SCC_PL (Forearm)", "SCC (Forearm)", 
#                         "SCC_PL (All body sites)", "SCC (All body sites)")))
immunocompetent_genus_summary_combined_forearm.df$Sample_type_label  <- as.character(immunocompetent_genus_summary_combined_forearm.df$Sample_type_label)


# FIXME order by forearm then sample_type


# ------------------------------------------------------------------------------------
# Now plot

# Immunosuppressed
immunosuppressed_just_legend_plot <- ggplot(immunosuppressed_genus_summary_combined_forearm.df, aes(x = Sample_type_label, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, name = "Taxonomy", 
                    guide = guide_legend(title.position = "top",nrow= 3)) +
  xlab("Sample site") +
  ylab("Normalised mean relative abundance (%)") +
  common_theme

immunosuppressed_abundance_plot <- ggplot(immunosuppressed_genus_summary_combined_forearm.df, aes(x = Sample_type_label, y = Normalised_mean_relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  # geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  ylab("Normalised mean relative abundance (%)") +
  common_theme

immunosuppressed_qpcr_16S_abundance_plot <- 
  ggplot(immunosuppressed_genus_summary_combined_forearm.df,
         aes(x = Sample_type_label,
             y = Mean_relative_abundance_qpcr_16S_proportional,
             fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette,guide = F) +
  scale_y_continuous(breaks = seq(0,1800, by = 100), limits=c(0,1800)) +
  # xlab("Sample site") +
  xlab("") +
  ylab("Normalised mean relative abundance scaled by scaled by absolute microbial load") +

  common_theme +
  theme(axis.text.y = element_blank())

# Extract the legend
my_legend_taxa <- cowplot::get_legend(immunosuppressed_just_legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 4),
                                          legend.title = element_text(size =5, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)
# now add the title
title <- ggdraw() + 
  draw_label(
    "immunosuppressed",
    fontface = 'bold',
    size = 8,
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

# Make a grid of plots with the list of plots for both cohorts
grid_plot <- plot_grid(plotlist = list(immunosuppressed_abundance_plot, NULL, immunosuppressed_qpcr_16S_abundance_plot),
                       ncol = 3,nrow=1, rel_widths = c(1.2,-.05,1),align = "hv")
grid_plot <- plot_grid(title, grid_plot, my_legend_taxa, rel_heights = c(.1,1,0.4), ncol = 1, nrow=3)
grid_plot
ggsave(filename = "Result_figures/abundance_analysis_plots/immunosuppressed_sample_type_relative_abundance_and_16S_qPCR.pdf", 
       plot = grid_plot, width = 30, 
       height = 6, units = "cm")
ggsave(filename = "Result_figures/abundance_analysis_plots/immunosuppressed_sample_type_relative_abundance_and_16S_qPCR.svg", 
       plot = grid_plot, width = 30, height = 5, units = "cm",device = "svg")


# Immunocompetent
immunocompetent_just_legend_plot <- ggplot(immunocompetent_genus_summary_combined_forearm.df, aes(x = Sample_type_label, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, 
                    name = "Taxonomy", 
                    guide = guide_legend(title.position = "top",nrow= 3)) +
  xlab("Sample site") +
  ylab("Normalised mean relative abundance (%)") +
  common_theme

immunocompetent_abundance_plot <- ggplot(immunocompetent_genus_summary_combined_forearm.df, aes(x = Sample_type_label, y = Normalised_mean_relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  # geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  ylab("Normalised mean relative abundance (%)") +
  common_theme

immunocompetent_qpcr_16S_abundance_plot <- 
  ggplot(immunocompetent_genus_summary_combined_forearm.df,
         aes(x = Sample_type_label,
             y = Mean_relative_abundance_qpcr_16S_proportional,
             fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette,guide = F) +
  # scale_y_continuous(breaks = seq(0,4500, by = 250), limits=c(0,4550)) +
  # xlab("Sample site") +
  xlab("") +
  ylab("Normalised mean relative abundance scaled by absolute microbial load") +
  
  common_theme +
  theme(axis.text.y = element_blank())

# Extract the legend
my_legend_taxa <- cowplot::get_legend(immunocompetent_just_legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 4),
                                          legend.title = element_text(size=5, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)

# now add the title
title <- ggdraw() + 
  draw_label(
    "Immunocompetent",
    fontface = 'bold',
    size = 8
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

# Make a grid of plots with the list of plots for both cohorts
grid_plot <- plot_grid(plotlist = list(immunocompetent_abundance_plot, NULL, immunocompetent_qpcr_16S_abundance_plot),
                       ncol = 3,nrow=1, rel_widths = c(1.2,-.05,1),align = "hv")
grid_plot <- plot_grid(title,grid_plot, my_legend_taxa, rel_heights = c(0.1,1,0.4), ncol = 1, nrow=3)

ggsave(filename = "Result_figures/abundance_analysis_plots/immunocompetent_sample_type_relative_abundance_and_16S_qPCR.pdf", 
       plot = grid_plot, width = 30, height = 5, units = "cm")
ggsave(filename = "Result_figures/abundance_analysis_plots/immunocompetent_sample_type_relative_abundance_and_16S_qPCR.svg", 
       plot = grid_plot, width = 30, height = 5, units = "cm",device = "svg")


# mean relative abundance for forearm?

