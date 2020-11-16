# Script to generate the mean relative abundance and bacterial load figure for publication

# invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()

library(reshape2)
library(ggplot2)
library(cowplot)
library(dplyr)
library(scales)

common_theme <- theme(
  panel.border = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black", size = 0.5),
  panel.background = element_blank(),
  strip.background = element_rect(fill = "white", colour = "white", size = 1),
  legend.key=element_blank(),
  legend.direction="vertical",
  legend.background = element_rect(colour ="white", size = .3),
  legend.text.align = 0,
  legend.title = element_text(size=10, face="bold"),
  legend.title.align = 0.5,
  legend.margin = margin(c(2,2,2,2)),
  legend.key.height=unit(.4,"cm"),
  legend.text = element_text(size = 8),
  axis.text = element_text(size = 9, colour = "black"),
  axis.title = element_text(size = 10,face = "bold"),
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

# otu_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/OTU_counts_abundances_and_metadata.csv", header = T)
# otu_taxonomy_map.df[grepl("Psychromonas", otu_taxonomy_map.df$taxonomy_species),]$taxonomy_species
# temp <- otu_taxonomy_map.df[grepl("Psychromonas", otu_taxonomy_map.df$Genus),][,c("OTU.ID", "RepSeq")]
# temp2 <- otu_data.df[otu_data.df$OTU.ID %in% as.character(temp$OTU.ID),]
# temp2 <- temp2[,c("Sample","Patient", "Sample_type","Relative_abundance", "OTU.ID")]
# temp2$Relative_abundance <- temp2$Relative_abundance  * 100
# temp2 <- left_join(temp2, temp, by = "OTU.ID")
# temp2 <- temp2[order(temp2$Relative_abundance, decreasing = T),]
# unique(temp2$OTU.ID)
# write.csv(x = temp2, file = "Result_tables/other/Psychromonas_ASV_abundances.csv", quote=F, row.names = F)

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)
metadata_consolidated.df <- read.csv("Result_tables/other/metadata_consolidated.csv", sep = ",", header = T)
metadata_consolidated.df <- subset(metadata_consolidated.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")
metadata_consolidated.df <- metadata_consolidated.df[,c("Index","Sample_type","Swab_ID", "Cohort","Sample_type_colour","qPCR_16S")]

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index
rownames(metadata_consolidated.df) <- metadata_consolidated.df$Index

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC and SCC lesions. 
metadata.df <- metadata.df[!metadata.df$Sample_type == "negative",]
metadata_consolidated.df <- metadata_consolidated.df[! metadata_consolidated.df$Sample_type == "negative",]

# forearm_PL_swab_ids <- c("1382","1384", "1471","1562","1600","1648","1650")
#forearm_swab_ids <- c("1383","1385","1470","1561","1599","1647","1649")
# 1492 1493 are MS0010 are not present
forearm_swab_ids_IC = c("522","523","564","565","678","679","740","741", "1172", "1200","1201","1322","1323",
"1358","1359","1492","1493")

# "678","679" are 'odd'/outliers exclude 
# forearm_swab_ids_IC = c("522","523","564","565","740","741", "1172", "1200","1201","1322","1323",
#                         "1358","1359","1492","1493")
forearm_swab_ids_IS <- c("1382","1383","1384", "1385","1470","1471","1561","1562",
                         "1599","1600","1649", "1650")

# forearm_indices <- c("R1383_J1425", "SA6550_J1427", "R1368_J1477", "R1460_J1425", "R1498_J1425", "SB4909_J1426", "SB4911_J1426")
# R1498_J1425  R1460_J1425  R1368_J1477  SA6550_J1427 SB4909_J1426
# SB4911_J1426  R1498_J1425 R1383_J1425

# Load filtered/processed abundance data with metadata
genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/genus_counts_abundances_and_metadata.csv")

genus_data.df <- genus_data.df[genus_data.df$Sample %in% rownames(metadata.df),]

temp <- genus_data.df[genus_data.df$Swab_ID %in% forearm_swab_ids_IS,]
# forearm_swab_ids_IS[!forearm_swab_ids_IS %in% genus_data.df$Swab_ID]
# forearm_swab_ids_IC[!forearm_swab_ids_IC %in% genus_data.df$Swab_ID]

# ------------------------------
# Set levels
genus_data.df$Sample_type <- factor(genus_data.df$Sample_type, levels = c("NS", "PDS", "AK", "SCC_PL","SCC"))

# Create taxonomy label
genus_data.df$taxonomy_label <- with(genus_data.df, paste0(Domain,";", Class,";", Genus))
genus_data.df$taxonomy_label <- gsub("[a-z]__", "", genus_data.df$taxonomy_label)

# Create cohort specific datasets
immunosuppressed_genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed")
immunocompetent_genus_data.df <- subset(genus_data.df, Cohort == "immunocompetent")
immunosuppressed_forearm_genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" & Swab_ID %in% forearm_swab_ids_IS)
immunocompetent_forearm_genus_data.df <- subset(genus_data.df, Cohort == "immunocompetent" & Swab_ID %in% forearm_swab_ids_IC)

# Exclude swabs "678","679" from abundance calculations as they are weird samples
# immunocompetent_genus_data.df <- subset(immunocompetent_genus_data.df,  !Swab_ID %in% c("678","679"))
# immunocompetent_forearm_genus_data.df <- subset(immunocompetent_forearm_genus_data.df, ! Swab_ID %in% c("678","679"))

# Set factor levels for sample type
immunosuppressed_genus_data.df$Sample_type <- factor(immunosuppressed_genus_data.df$Sample_type, levels = rev(c("NS","PDS", "AK", "SCC_PL", "SCC")))
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

# Create palette based on unique set
publication_palette_10 <- c("#d35238","#6ab74d","#8562cc","#c3ab41","#688bcd","#c07b44","#4bb193","#c361aa","#6d8038","#c9566e")
# publication_palette_23 <- c("#7860cd","#458532","#d6434d","#398762","#dd8066","#ce93dd","#cf5628","#a4ba68","#bf4bb3","#6281c6","#e285a5","#61c897","#d33f7c","#6a6f2d","#975d2a","#afbb30","#a24753","#d8aa6c","#62c151","#9a8c2e","#904e86","#dd9c32","#51bcd6")
# both_cohorts_genus_palette <- setNames(my_colour_palette_35_distinct[1:length(both_cohorts_lesions_top_genus)], both_cohorts_lesions_top_genus)
both_cohorts_genus_palette <- setNames(publication_palette_10[1:length(both_cohorts_lesions_top_genus)], both_cohorts_lesions_top_genus)
# both_cohorts_genus_palette <- setNames(publication_palette_23[1:length(both_cohorts_lesions_top_genus)], both_cohorts_lesions_top_genus)

# Set other colour to grey
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

# Re-label any taxa not in the top taxa (for the SAME cohort/groups) to "Other" for each summary table
# immunosuppressed_genus_summary.df[!immunosuppressed_genus_summary.df$taxonomy_label %in% immunosuppressed_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# immunocompetent_genus_summary.df[!immunocompetent_genus_summary.df$taxonomy_label %in% immunocompetent_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# immunosuppressed_forearm_genus_summary.df[!immunosuppressed_forearm_genus_summary.df$taxonomy_label %in% immunosuppressed_top_forearm_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"

# Re-label any taxa not in the top taxa (across ALL cohorts/groups) to "Other" for each summary table
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
sum(immunosuppressed_genus_summary.df$Mean_relative_abundance)
sum(immunosuppressed_genus_summary.df$Normalised_mean_relative_abundance)

immunosuppressed_genus_summary.df[immunosuppressed_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunocompetent_genus_summary.df[immunocompetent_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunosuppressed_forearm_genus_summary.df[immunosuppressed_forearm_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunocompetent_forearm_genus_summary.df[immunocompetent_forearm_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA

# Need a single entry for the Other group
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
Mean_qpcr_totals.df <-
  metadata.df %>% 
  group_by(Cohort, Sample_type) %>% 
  dplyr::summarise(N_samples = n_distinct(Index), #!!!
                   # Mean_Staph_spp_qPCR = mean(Staph_spp_qPCR, na.rm = T),
                   # Mean_S_aureus_qPCR = mean(S_aureus_qPCR, na.rm = T),
                   Mean_qPCR_16S = mean(qPCR_16S, na.rm = T)
  ) %>% 
  as.data.frame()

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


immunosuppressed_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- NA
immunocompetent_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- NA
immunosuppressed_forearm_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- NA
immunocompetent_forearm_genus_summary.df$Mean_relative_abundance_qpcr_16S_proportional <- NA

immunosuppressed_genus_summary.df <- left_join(immunosuppressed_genus_summary.df, Mean_qpcr_totals.df, by = c("Cohort" = "Cohort", "Sample_type" = "Sample_type"))
immunocompetent_genus_summary.df <- left_join(immunocompetent_genus_summary.df, Mean_qpcr_totals.df, by = c("Cohort" = "Cohort", "Sample_type" = "Sample_type"))
immunosuppressed_forearm_genus_summary.df <- left_join(immunosuppressed_forearm_genus_summary.df, Mean_qpcr_totals_forearm.df, by = c("Cohort" = "Cohort", "Sample_type" = "Sample_type"))
immunocompetent_forearm_genus_summary.df <- left_join(immunocompetent_forearm_genus_summary.df, Mean_qpcr_totals_forearm.df, by = c("Cohort" = "Cohort", "Sample_type" = "Sample_type"))

# Calculate abundance value proportional to qPCR totals
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

# Set levels for sample type
immunosuppressed_genus_summary.df$Sample_type <- factor(immunosuppressed_genus_summary.df$Sample_type, levels = rev(c("NS", "PDS", "AK", "SCC_PL","SCC")))
immunocompetent_genus_summary.df$Sample_type <- factor(immunocompetent_genus_summary.df$Sample_type, levels = rev(c("PDS", "AK", "SCC_PL","SCC")))
immunosuppressed_forearm_genus_summary.df$Sample_type <- factor(immunosuppressed_forearm_genus_summary.df$Sample_type, levels = rev(c("SCC_PL","SCC")))
immunocompetent_forearm_genus_summary.df$Sample_type <- factor(immunocompetent_forearm_genus_summary.df$Sample_type, levels = rev(c("SCC_PL","SCC")))

# Combine the forearm with the full dataset and set forearm status
immunosuppressed_forearm_genus_summary.df$Location <- "Forearm"
immunosuppressed_genus_summary.df$Location <- "Forearm"
immunosuppressed_genus_summary.df[immunosuppressed_genus_summary.df$Sample_type %in% c("SCC_PL", "SCC"),]$Location <- "All body sites"
immunosuppressed_genus_summary_combined_forearm.df <- rbind(immunosuppressed_genus_summary.df, immunosuppressed_forearm_genus_summary.df)

immunocompetent_forearm_genus_summary.df$Location <- "Forearm"
immunocompetent_genus_summary.df$Location <- "Forearm"
immunocompetent_genus_summary.df[immunocompetent_genus_summary.df$Sample_type %in% c("SCC_PL", "SCC"),]$Location <- "All body sites"
immunocompetent_genus_summary_combined_forearm.df <- rbind(immunocompetent_genus_summary.df, immunocompetent_forearm_genus_summary.df)

# Create sample_type_label
immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label <- 
  with(immunosuppressed_genus_summary_combined_forearm.df, paste0(Sample_type, "\n", Location, "\nn=", N_samples))

immunocompetent_genus_summary_combined_forearm.df$Sample_type_label <- 
  with(immunocompetent_genus_summary_combined_forearm.df, paste0(Sample_type, "\n", Location, "\nn=", N_samples))


# immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label  <- as.character(immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label)
# Set the order or appearance for the Sample_type_label
immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label <- 
  factor(immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label, 
         levels = rev(unique(immunosuppressed_genus_summary_combined_forearm.df[with(immunosuppressed_genus_summary_combined_forearm.df, order(Location, Sample_type)),]$Sample_type_label)))

immunocompetent_genus_summary_combined_forearm.df$Sample_type_label <- 
  factor(immunocompetent_genus_summary_combined_forearm.df$Sample_type_label, 
         levels = rev(unique(immunocompetent_genus_summary_combined_forearm.df[with(immunocompetent_genus_summary_combined_forearm.df, order(Location, Sample_type)),]$Sample_type_label)))

levels(immunosuppressed_genus_summary_combined_forearm.df$Sample_type_label)
levels(immunocompetent_genus_summary_combined_forearm.df$Sample_type_label)

# -----------------------------------------------------------------------------
# -------- format metadata for scatter plots -------
metadata_consolidated.df <- metadata_consolidated.df[metadata_consolidated.df$qPCR_16S != 0,]

metadata_consolidated_forearm.df <- metadata_consolidated.df %>% 
  filter(Swab_ID %in% c(forearm_swab_ids_IS,forearm_swab_ids_IC) | Sample_type %in% c("NS", "PDS","AK"))
metadata_consolidated_all_sites.df <- metadata_consolidated.df %>% 
  filter(Sample_type %in% c("SCC_PL", "SCC"))

metadata_consolidated_forearm.df$Location <- "Forearm"
metadata_consolidated_all_sites.df$Location <- "All body sites"

# metadata_consolidated_forearm.df$Index

metadata_consolidated_forearm.df <- 
  metadata_consolidated_forearm.df %>% 
  dplyr::group_by(Cohort, Sample_type) %>%
  dplyr::mutate(N_samples = n_distinct(Index)) %>%
  as.data.frame()

metadata_consolidated_all_sites.df <- 
  metadata_consolidated_all_sites.df %>% 
  dplyr::group_by(Cohort, Sample_type) %>%
  dplyr::mutate(N_samples = n_distinct(Index)) %>%
  as.data.frame()
unique(metadata_consolidated_forearm.df[,c("Cohort","Sample_type", "N_samples")])
unique(metadata_consolidated_all_sites.df[,c("Cohort","Sample_type", "N_samples")])

metadata_consolidated_forearm.df$Sample_type_label <- 
  with(metadata_consolidated_forearm.df, paste0(Sample_type, "\n", Location, "\nn=", N_samples))
metadata_consolidated_all_sites.df$Sample_type_label <- 
  with(metadata_consolidated_all_sites.df, paste0(Sample_type, "\n", Location, "\nn=", N_samples))

metadata_consolidated_forearm.df$Sample_type <- factor(metadata_consolidated_forearm.df$Sample_type, 
                                                       levels = rev(c("NS", "PDS", "AK", "SCC_PL","SCC")))
metadata_consolidated_all_sites.df$Sample_type <- factor(metadata_consolidated_all_sites.df$Sample_type, 
                                                       levels = rev(c("SCC_PL","SCC")))

metadata_consolidated_forearm.df$Sample_type_label <- 
  factor(metadata_consolidated_forearm.df$Sample_type_label, 
         levels = rev(unique(metadata_consolidated_forearm.df[with(metadata_consolidated_forearm.df, order(Location, Sample_type)),]$Sample_type_label)))

metadata_consolidated_all_sites.df$Sample_type_label <- 
  factor(metadata_consolidated_all_sites.df$Sample_type_label, 
         levels = rev(unique(metadata_consolidated_all_sites.df[with(metadata_consolidated_all_sites.df, order(Location, Sample_type)),]$Sample_type_label)))

metadata_consolidated_combined.df <- rbind(metadata_consolidated_forearm.df, metadata_consolidated_all_sites.df)
metadata_consolidated_combined.df$Sample_type <- factor(metadata_consolidated_combined.df$Sample_type, 
                                                       levels = c("NS", "PDS", "AK", "SCC_PL","SCC"))

# c(25,24,23,22,21)

variable_shapes <- setNames(c(25,24,23,22,21), as.character(unique(sort(metadata_consolidated_combined.df$Sample_type))))

# metadata_consolidated_combined.df <- metadata_consolidated_combined.df[metadata_consolidated_combined.df$qPCR_16S != 0,]

# Stats
immunosuppressed_qPCR_dunn <- dunnTest(x = qPCR_16S~Sample_type_label, 
                                       data = subset(metadata_consolidated_combined.df, Cohort == "immunosuppressed"), 
                                       method = "bh", alpha = 0.05)$res

immunocompetent_qPCR_dunn <-  dunnTest(x = qPCR_16S~Sample_type_label, 
                                       data = subset(metadata_consolidated_combined.df, Cohort == "immunocompetent"), 
                                       method = "bh", alpha = 0.05)$res
immunosuppressed_qPCR_dunn$Cohort <- "immunosuppressed"
immunocompetent_qPCR_dunn$Cohort <- "immunocompetent"
dunn <- rbind(immunosuppressed_qPCR_dunn,immunocompetent_qPCR_dunn)
dunn$Comparison <- gsub("\\n", " ", dunn$Comparison)
dunn <- separate(dunn, Comparison, into = c("Group_1", "Group_2"), sep = " - ")[,c("Cohort","Group_1","Group_2","P.unadj","P.adj")]
dunn$P_value_label <- as.character(lapply(dunn[,"P.adj"], function(x) ifelse(x <= 0.0001, "****",
                                                                             ifelse(x <= 0.001, "***", 
                                                                                 ifelse(x <= 0.01, "**", 
                                                                                        ifelse(x <= 0.05, "*", "ns"))))))
dunn <- dunn[dunn$P_value_label != "ns",]
write.csv(x = dunn, file = "Result_tables/stats_various/qPCR_sampletype_dunn.csv", quote = F, row.names = F)

metadata_consolidated_combined.df %>% 
  dplyr::group_by(Cohort, Sample_type,Sample_type_label) %>%
  tally()

mean_qpcr_values.df <- metadata_consolidated_combined.df %>%
  dplyr::group_by(Cohort, Sample_type,Sample_type_label) %>%
  dplyr::summarise(Mean_qPCR_16S = mean(qPCR_16S,na.rm = T)) %>%
  as.data.frame()


# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# Now plot

all_combined <- rbind(immunosuppressed_genus_summary_combined_forearm.df, immunocompetent_genus_summary_combined_forearm.df)
dim(all_combined)
all_combined <- unique(left_join(all_combined, metadata.df[,c("Sample_type","Sample_type_colour")], by = "Sample_type"))
dim(all_combined)
temp <- all_combined
temp$Sample_type_label <- gsub("\\n", " ", temp$Sample_type_label)
write.csv(x = temp %>% select(-Sample_type_colour) %>% as.data.frame(), 
          file = "Result_tables/abundance_analysis_tables/genus_abundances_combined_publication.csv", 
          quote = F,
          row.names = F)

all_combined %>% 
  dplyr::group_by(Cohort, Sample_type,Sample_type_label) %>%
  tally()

# Compare SCC and SCC_PL from all body sites to forearm, to determine change in abundance
temp <- subset(temp, Sample_type %in% c("SCC", "SCC_PL") & taxonomy_label != "Other")
temp$Cohort_sampletype_taxonomy_label <- with(temp, paste0(Cohort, "__", Sample_type, "__", taxonomy_label))
for (tax in unique(temp$Cohort_sampletype_taxonomy_label)){
  f_abundance <- round(subset(temp, Cohort_sampletype_taxonomy_label == tax & Location == "Forearm")$Mean_relative_abundance*100,3)
  abs_abundance <- round(subset(temp, Cohort_sampletype_taxonomy_label == tax & Location == "All body sites")$Mean_relative_abundance*100,3)
  print(paste(tax, f_abundance, abs_abundance, f_abundance - abs_abundance))
}
# ------ Version using scaled abundances

# undo order-by-abundance and just use all_combined order
immunosuppressed_genus_summary_combined_forearm.df$taxonomy_label <- 
  factor(immunosuppressed_genus_summary_combined_forearm.df$taxonomy_label, levels = levels(all_combined$taxonomy_label))
immunocompetent_genus_summary_combined_forearm.df$taxonomy_label <- 
  factor(immunocompetent_genus_summary_combined_forearm.df$taxonomy_label, levels = levels(all_combined$taxonomy_label))

just_legend_plot <- ggplot(all_combined, 
                           aes(x = Sample_type_label, 
                               y = Normalised_mean_relative_abundance, 
                               fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .1) +
  # coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, name = "Taxonomy", 
                    guide = guide_legend(title.position = "top",nrow= 3)) +
  xlab("Sample site") +
  ylab("Normalised mean relative abundance (%)") +
  common_theme +
  # theme(plot.title = element_text(hjust = .5, face = "bold"))
  theme(plot.title = element_text(hjust = .5, face = "bold"),
      axis.text.y = element_blank(),
      axis.title.x = element_blank())


immunosuppressed_abundance_plot <- 
  ggplot(immunosuppressed_genus_summary_combined_forearm.df, 
         aes(x = Sample_type_label, 
             y = Normalised_mean_relative_abundance*100, 
             fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .1) +
  # geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") +
  scale_fill_manual(values = both_cohorts_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10), expand = c(0, 0)) +
  # scale_y_continuous(breaks = c(seq(0,100, by = 10),1000),limits =c(0,100.01), expand = c(0, 0)) +
  # coord_cartesian(ylim = c(0,100.01)) +
  xlab("Sample type") +
  ylab("Normalised mean relative abundance (%)") +
  # ylab(expression(bold(atop("Normalised mean relative abundance (%)","",sep ="\n")))) +
  common_theme +
  theme(plot.title = element_text(hjust = .5, face = "bold"),
      axis.text.x = element_blank(),
      # axis.title.y = element_text(lineheight = 2),
      axis.title.x = element_blank())
immunosuppressed_abundance_plot

immunocompetent_abundance_plot <- 
  ggplot(immunocompetent_genus_summary_combined_forearm.df,
         aes(x = Sample_type_label, 
             y = Normalised_mean_relative_abundance*100,
             fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .1) +
  # geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") +
  scale_fill_manual(values = both_cohorts_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10), expand = c(0, 0)) +
  xlab("Sample type") +
  # ylab("Normalised mean relative abundance (%)") +
  common_theme +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold"))

immunosuppressed_scaled_abundance_plot <- 
  ggplot(immunosuppressed_genus_summary_combined_forearm.df,
         aes(x = Sample_type_label,
             y = Mean_relative_abundance_qpcr_16S_proportional,
             fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .1) +
  scale_fill_manual(values = both_cohorts_genus_palette,guide = F) +
  scale_y_continuous(breaks = seq(0,5000, by = 500), limits=c(0,4700), expand = c(0, 0)) +
  xlab("Sample type") +
  # ylab("Normalised mean relative abundance x absolute microbial load") +
  # 
  # ylab(expression("Normalised mean relative abundance"~''%*%''~"absolute microbial load")) +
  # ylab(expression(bold(paste("Normalised mean relative abundance "%*%'',"absolute microbial load",sep ="\n")))) +
  ylab(expression(bold(atop("Normalised mean relative abundance (%)"%*%'',"mean absolute microbial load",sep ="\n")))) +
  # ylab(expression(bold(atop("Normalised mean relative abundance "~''%*%'',
  #                      absolute~microbial~load)))) +
  common_theme
  # theme(plot.title = element_text(hjust = .5, face = "bold"))
  # theme(axis.text.y = element_blank())
immunosuppressed_scaled_abundance_plot

immunocompetent_scaled_abundance_plot <- 
  ggplot(immunocompetent_genus_summary_combined_forearm.df,
         aes(x = Sample_type_label,
             y = Mean_relative_abundance_qpcr_16S_proportional,
             fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .1) +
  scale_fill_manual(values = both_cohorts_genus_palette,guide = F) +
  scale_y_continuous(breaks = seq(0,5000, by = 500), limits=c(0,4700), expand = c(0, 0)) +
  xlab("Sample type") +
  # ylab("Normalised mean relative abundance scaled by scaled by absolute microbial load") +
  common_theme +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold"))
        # axis.text.x = element_text(angle = 45))

# Extract the legend
my_legend_taxa <- cowplot::get_legend(just_legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 7),
                                          legend.title = element_text(size = 8, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)
# now add the title
title <- ggdraw() + 
  draw_label(
    "",
    fontface = 'bold',
    size = 8
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

sample_type_colours <- unique(all_combined[,c("Sample_type", "Sample_type_colour")])
sample_type_colours <- setNames(as.character(sample_type_colours[,"Sample_type_colour"]), 
                                sample_type_colours[,"Sample_type"])
# length(sample_type_colours)
# length(names(sample_type_colours))
# length(unique(names(sample_type_colours)))
# ggplot(immunosuppressed_genus_summary_combined_forearm.df,
#        aes(x = Sample_type_label,
#            y = Mean_relative_abundance_qpcr_16S_proportional,
#            fill = taxonomy_label)) +
#   geom_bar(stat = "identity", colour = "black", lwd = .1) +
#   scale_fill_manual(values = both_cohorts_genus_palette,guide = F) +
#   # scale_y_continuous(breaks = seq(0,5000, by = 500), limits=c(0,4700), expand = c(0, 0)) +
#   xlab("Sample type")  +
#   annotate(geom='point', x = seq(1,7,1),
#            y = -50.00, size=10,stroke= 1, shape = 22, fill = "red")

# Make a grid of plots with the list of plots for both cohorts

# axis.title.y = element_text(margin = unit(c(0, 0, 0, 0), "cm"))), #t r b l
grid_plot_immunosuppressed <- plot_grid(plotlist = list(immunosuppressed_abundance_plot+
                                                          labs(title = "Organ transplant recipient") +
                                                          theme(plot.margin = unit(c(5.5,5.5,5.5,24),"pt"),
                                                                axis.text.y = element_text(size = 7),
                                                                axis.title.y = element_text(size = 9),
                                                                plot.title = element_text(size = 12)),
                                                        immunosuppressed_scaled_abundance_plot +
                                                          theme(axis.text.x = element_text(size = 7),
                                                                axis.text.y = element_text(size = 7),
                                                                axis.title = element_text(size = 9))
                                                        ),
                                        ncol = 1, rel_widths = c(1,1),scale = c(1,1),axis= "r")
grid_plot_immunocompetent <- plot_grid(plotlist = list(immunocompetent_abundance_plot +
                                                         labs(title = "Immunocompetent") +
                                                         theme(plot.title = element_text(size = 12)), 
                                                       immunocompetent_scaled_abundance_plot +
                                                         theme(axis.text.x = element_text(size = 7),
                                                               axis.title.x = element_text(size = 9))),
                                        ncol = 1, rel_widths = c(1,1))

grid_plot <- plot_grid(grid_plot_immunosuppressed, 
                       grid_plot_immunocompetent,
                       rel_heights = c(1,1),rel_widths = c(1,.8),scale = c(1,1), ncol = 2, nrow=1)

grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.1), ncol = 1, nrow=2)


# Add (a) (b) figure labels
text_par <- grid::gpar(col = "black", fontsize = 16, 
                       fontface = "bold", lineheight = 0.9, alpha = 1)
text.grob_a <- grid::textGrob("(a)", x = grid::unit(0.5, "npc"), 
                              y = grid::unit(0.5, "npc"), hjust = 0.5, vjust = 0.5, 
                              rot = 0, gp = text_par)
text.grob_b <- grid::textGrob("(b)", x = grid::unit(0.5, "npc"), 
                            y = grid::unit(0.5, "npc"), hjust = 0.5, vjust = 0.5, 
                            rot = 0, gp = text_par)
# text_bracket_forearm <- grid::textGrob("[", x = grid::unit(0.5, "npc"), 
#                                        y = grid::unit(0.5, "npc"), hjust = 0.5, vjust = 0.5, 
#                                        rot = -90, gp = text_par)

rect_grob <- grid.rect(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
          width = unit(1, "npc"), height = unit(1, "npc"),
          just = "centre", hjust = NULL, vjust = NULL,
          default.units = "npc", name = NULL,
          gp=gpar(col = sample_type_colours["NS"],fill = NA,stroke = 2,alpha= 1,lwd =2), draw = TRUE, vp = NULL)

# grid.draw(grid_plot + theme(axis.text = element_blank()))
# temp <- ggplotGrob(grid_plot)

# library(grid)
# library(gridExtra)

# annotation_custom(text.grob_a, xmin = 0.02, xmax = 0.02, ymin = .97, ymax=.97)
# grid_plot <- grid_plot + theme(panel.background = element_blank())
# grid.draw(grobTree(rectGrob(gp=gpar(fill="grey80", lwd=0)), 
                   # ggplotGrob(grid_plot)))

grid_plot <- grid_plot +
  annotation_custom(text.grob_a, xmin = 0.02, xmax = 0.02, ymin = .97, ymax=.97) +
  annotation_custom(text.grob_b, xmin = 0.02, xmax = 0.02, ymin = .53, ymax=.53) 
  
  # annotation_custom(rect_grob, xmin = 0.09, xmax = 0.14, ymin = .11, ymax=.144)
  
ggsave(filename = "Result_figures/abundance_analysis_plots/IS_IC_sample_type_relative_abundance_and_rRNA_qPCR.pdf", 
       plot = grid_plot, width = 26, 
       height = 25, units = "cm")
ggsave(filename = "Result_figures/abundance_analysis_plots/IS_IC_sample_type_relative_abundance_and_rRNA_qPCR.svg", 
       plot = grid_plot, width = 26, height = 25, units = "cm",device = "svg")
ggsave(plot =my_legend_taxa,"Result_figures/abundance_analysis_plots/taxa.svg", 
       width = 27, units = "cm", device = "svg")


# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# ------ Version using qPCR scatter plots

immunosuppressed_qpcr_plot <- 
  ggplot(subset(metadata_consolidated_combined.df, Cohort == "immunosuppressed"), 
         aes(x = Sample_type_label,
             y = qPCR_16S,
             # colour = Sample_type_colour,
             fill = Sample_type_colour,
             shape = Sample_type)) +
  geom_errorbar(data = subset(mean_qpcr_values.df, Cohort == "immunosuppressed")[,c("Sample_type_label", "Mean_qPCR_16S")],
                aes(ymax = Mean_qPCR_16S, ymin = Mean_qPCR_16S,x = Sample_type_label),inherit.aes = F,
                width = .4, lwd =.6, linetype = "solid", colour = "black") +
  geom_jitter(show.legend = F,size = 1.5,stroke = .3, alpha = 1,position = position_jitter(width = .15)) +
  scale_y_log10(limits=c(10^-1, 10^5),breaks=10^(-1:5),labels = trans_format('log10',math_format(10^.x))) +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_shape_manual(values = variable_shapes) +
  # labs(title = "Organ transplant recipient") +
  xlab("Sample type") +
  ylab("SSU rRNA equivalents per sampling area") +
  common_theme +
  theme(axis.text.x = element_text(angle = 0, vjust = 1,hjust = .5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 6))


immunocompetent_qpcr_plot <- 
  ggplot(subset(metadata_consolidated_combined.df, Cohort == "immunocompetent"), 
         aes(x = Sample_type_label,
             y = qPCR_16S,
             # colour = Sample_type_colour,
             fill = Sample_type_colour,
             shape = Sample_type)) +
  geom_errorbar(data = subset(mean_qpcr_values.df, Cohort == "immunocompetent")[,c("Sample_type_label", "Mean_qPCR_16S")],
                aes(ymax = Mean_qPCR_16S, ymin = Mean_qPCR_16S,x = Sample_type_label),inherit.aes = F,
                width = .4, lwd =.6, linetype = "solid", colour = "black") +
  geom_jitter(show.legend = F,size = 1.5,stroke = .3, alpha = 1,position = position_jitter(width = .15)) +
  scale_y_log10(limits=c(10^-1, 10^5),breaks=10^(-1:5),labels = trans_format('log10',math_format(10^.x))) +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_shape_manual(values = variable_shapes) +
  # labs(title = "Immunocompetent") +
  xlab("Sample type") +
  ylab("SSU rRNA equivalents per sampling area") +
  common_theme +
  theme(axis.text.x = element_text(angle = 0, vjust = 1,hjust = .5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 6))


grid_plot_immunosuppressed <- plot_grid(plotlist = list(immunosuppressed_abundance_plot+
                                                          labs(title = "Organ transplant recipient") +
                                                          theme(plot.margin = unit(c(5.5,5.5,5.5,14),"pt"),
                                                                axis.text.x = element_text(size = 7),
                                                                axis.text.y = element_text(size = 7),
                                                                axis.title.y = element_text(size = 9),
                                                                plot.title = element_text(size = 12)),
                                                        immunosuppressed_qpcr_plot +
                                                          theme(plot.margin = unit(c(5.5,5.5,5.5,12),"pt"),
                                                                axis.text.x = element_text(size = 7),
                                                                axis.text.y = element_text(size = 7),
                                                                axis.title = element_text(size = 9))
                                                        ),
                                        ncol = 1, rel_widths = c(1,1),scale = c(1,1),axis= "r")
grid_plot_immunosuppressed

grid_plot_immunocompetent <- plot_grid(plotlist = list(immunocompetent_abundance_plot +
                                                         labs(title = "Immunocompetent") +
                                                         theme(plot.title = element_text(size = 12),
                                                               axis.text.x = element_text(size = 7)), 
                                                       immunocompetent_qpcr_plot +
                                                         theme(axis.text.x = element_text(size = 7),
                                                               axis.text.y = element_blank(),
                                                               axis.title.x = element_text(size = 9),
                                                               axis.title.y = element_blank())),
                                       ncol = 1, rel_widths = c(1,1))
grid_plot_immunocompetent
grid_plot <- plot_grid(grid_plot_immunosuppressed, 
                       grid_plot_immunocompetent,
                       rel_heights = c(1,1),rel_widths = c(1,.8),scale = c(1,1), ncol = 2, nrow=1)

grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.1), ncol = 1, nrow=2)
grid_plot


# Add (a) (b) figure labels
text_par <- grid::gpar(col = "black", fontsize = 16, 
                       fontface = "bold", lineheight = 0.9, alpha = 1)
text.grob_a <- grid::textGrob("(a)", x = grid::unit(0.5, "npc"), 
                              y = grid::unit(0.5, "npc"), hjust = 0.5, vjust = 0.5, 
                              rot = 0, gp = text_par)
text.grob_b <- grid::textGrob("(b)", x = grid::unit(0.5, "npc"), 
                              y = grid::unit(0.5, "npc"), hjust = 0.5, vjust = 0.5, 
                              rot = 0, gp = text_par)

grid_plot <- grid_plot +
  annotation_custom(text.grob_a, xmin = 0.02, xmax = 0.02, ymin = .98, ymax=.98) +
  annotation_custom(text.grob_b, xmin = 0.02, xmax = 0.02, ymin = .54, ymax=.54) 

# annotation_custom(rect_grob, xmin = 0.09, xmax = 0.14, ymin = .11, ymax=.144)

ggsave(filename = "Result_figures/abundance_analysis_plots/IS_IC_sample_type_relative_abundance_and_rRNA_qPCR_scatter.pdf", 
       plot = grid_plot, width = 26, 
       height = 25, units = "cm")
ggsave(filename = "Result_figures/abundance_analysis_plots/IS_IC_sample_type_relative_abundance_and_rRNA_qPCR_scatter.svg", 
       plot = grid_plot, width = 26, height = 25, units = "cm",device = "svg")
ggsave(plot =my_legend_taxa,"Result_figures/abundance_analysis_plots/taxa.svg", 
       width = 27, units = "cm", device = "svg")

