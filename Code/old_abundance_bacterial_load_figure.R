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

forearm_swab_ids <- c("1383","1385","1470","1561","1599","1647","1649")
forearm_indices <- c("R1383_J1425", "SA6550_J1427", "R1368_J1477", "R1460_J1425", "R1498_J1425", "SB4909_J1426", "SB4911_J1426")
# R1498_J1425  R1460_J1425  R1368_J1477  SA6550_J1427 SB4909_J1426
# SB4911_J1426  R1498_J1425 R1383_J1425

# Load filtered/processed abundance data with metadata
genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/genus_counts_abundances_and_metadata.csv")

genus_data.df <- genus_data.df[genus_data.df$Sample %in% rownames(metadata.df),]

# ------------------------------

# Set levels
genus_data.df$Sample_type <- factor(genus_data.df$Sample_type, levels = c("HS", "PDS", "AK", "SCC_PL","SCC"))

# Create taxonomy label
# Domain, family, genus
# if family is Unassigned, use last assignment
# genus_data.df$taxonomy_label <- as.character(genus_data.df$taxonomy_genus)
genus_data.df$taxonomy_label <- with(genus_data.df, paste0(Domain,";", Class,";", Genus))


# Only snapshot and immunosuppressed samples
# genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" | Snapshot_sample_1 == "yes")
# genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" | Snapshot_sample_2 == "yes")
# genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" | Snapshot_sample_3 == "yes")
# genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" | Snapshot_sample_4 == "yes")
# genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")
# genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" | Snapshot_sample_6 == "yes")
# genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" | Snapshot_sample_7 == "yes")
# genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" | Snapshot_sample_8 == "yes")

immunosuppressed_genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed")
immunosuppressed_forearm_genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" & Sample %in% forearm_indices)
immunocompetent_genus_data.df <- subset(genus_data.df, Cohort == "immunocompetent")
# write.csv(immunocompetent_genus_data.df, file = "Result_tables/temp.csv", quote = F, row.names = F)

immunosuppressed_genus_data.df$Sample_type <- factor(immunosuppressed_genus_data.df$Sample_type, levels = rev(c("HS","PDS", "AK", "SCC_PL", "SCC")))
immunocompetent_genus_data.df$Sample_type <- factor(immunocompetent_genus_data.df$Sample_type, levels = rev(c("PDS", "AK", "SCC_PL", "SCC")))
immunosuppressed_forearm_genus_data.df$Sample_type <- factor(immunosuppressed_forearm_genus_data.df$Sample_type, levels = rev(c("HS","PDS", "AK", "SCC_PL", "SCC")))

# ----------------------------------------------------------------------------------------
# Snapshot analysis

# sn1_genus_data.df <- subset(genus_data.df, Snapshot_sample_1 == "yes")
# sn2_genus_data.df <- subset(genus_data.df, Snapshot_sample_2 == "yes")
# sn3_genus_data.df <- subset(genus_data.df, Snapshot_sample_3 == "yes")
# sn4_genus_data.df <- subset(genus_data.df, Snapshot_sample_4 == "yes")
# sn5_genus_data.df <- subset(genus_data.df, Snapshot_sample_5 == "yes")
# sn6_genus_data.df <- subset(genus_data.df, Snapshot_sample_6 == "yes")
# sn7_genus_data.df <- subset(genus_data.df, Snapshot_sample_7 == "yes")
# sn8_genus_data.df <- subset(genus_data.df, Snapshot_sample_8 == "yes")
# 
# immunocompetent_summary.df <- immunocompetent_genus_data.df %>% group_by(Sample_type) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn1_summary.df <- sn1_genus_data.df %>% group_by(Sample_type) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn2_summary.df <- sn2_genus_data.df %>% group_by(Sample_type) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn3_summary.df <- sn3_genus_data.df %>% group_by(Sample_type) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn4_summary.df <- sn4_genus_data.df %>% group_by(Sample_type) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn5_summary.df <- sn5_genus_data.df %>% group_by(Sample_type) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn6_summary.df <- sn6_genus_data.df %>% group_by(Sample_type) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn7_summary.df <- sn7_genus_data.df %>% group_by(Sample_type) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn8_summary.df <- sn8_genus_data.df %>% group_by(Sample_type) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# 
# immunocompetent_summary.df$Snapshot <- "Full_cohort"
# sn1_summary.df$Snapshot <- "Snapshot_1"
# sn2_summary.df$Snapshot <- "Snapshot_2"
# sn3_summary.df$Snapshot <- "Snapshot_3"
# sn4_summary.df$Snapshot <- "Snapshot_4"
# sn5_summary.df$Snapshot <- "Snapshot_5"
# sn6_summary.df$Snapshot <- "Snapshot_6"
# sn7_summary.df$Snapshot <- "Snapshot_7"
# sn8_summary.df$Snapshot <- "Snapshot_8"
# combined_snapshot_summary.df <- rbind(immunocompetent_summary.df, sn1_summary.df,sn2_summary.df,sn3_summary.df,sn4_summary.df,sn5_summary.df,sn6_summary.df,sn7_summary.df,sn8_summary.df)
# write.csv(x = combined_snapshot_summary.df, file = "Result_tables/abundance_analysis_tables/snapshots_summary.csv", row.names = F)
# 
# 
# immunocompetent_genus_summary.df <- generate_taxa_summary(immunocompetent_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sample_type"))
# immunocompetent_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunocompetent_genus_summary.df, grouping_variables = c("Sample_type"),abundance_column = "Mean_relative_abundance",my_top_n = 9)
# immunocompetent_genus_summary.df[!immunocompetent_genus_summary.df$taxonomy_label %in% immunocompetent_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# immunocompetent_genus_summary.df <- immunocompetent_genus_summary.df %>% group_by(Sample_type) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
# immunocompetent_genus_summary.df <- immunocompetent_genus_summary.df %>% group_by(Sample_type, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn1_genus_summary.df <- generate_taxa_summary(sn1_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sample_type"))
# sn1_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn1_genus_summary.df, grouping_variables = c("Sample_type"),abundance_column = "Mean_relative_abundance",my_top_n = 9)
# sn1_genus_summary.df[!sn1_genus_summary.df$taxonomy_label %in% sn1_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn1_genus_summary.df <- sn1_genus_summary.df %>% group_by(Sample_type) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
# sn1_genus_summary.df <- sn1_genus_summary.df %>% group_by(Sample_type, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn2_genus_summary.df <- generate_taxa_summary(sn2_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sample_type"))
# sn2_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn2_genus_summary.df, grouping_variables = c("Sample_type"),abundance_column = "Mean_relative_abundance",my_top_n = 9)
# sn2_genus_summary.df[!sn2_genus_summary.df$taxonomy_label %in% sn2_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn2_genus_summary.df <- sn2_genus_summary.df %>% group_by(Sample_type) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
# sn2_genus_summary.df <- sn2_genus_summary.df %>% group_by(Sample_type, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn3_genus_summary.df <- generate_taxa_summary(sn3_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sample_type"))
# sn3_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn3_genus_summary.df, grouping_variables = c("Sample_type"),abundance_column = "Mean_relative_abundance",my_top_n = 9)
# sn3_genus_summary.df[!sn3_genus_summary.df$taxonomy_label %in% sn3_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn3_genus_summary.df <- sn3_genus_summary.df %>% group_by(Sample_type) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
# sn3_genus_summary.df <- sn3_genus_summary.df %>% group_by(Sample_type, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn4_genus_summary.df <- generate_taxa_summary(sn4_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sample_type"))
# sn4_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn4_genus_summary.df, grouping_variables = c("Sample_type"),abundance_column = "Mean_relative_abundance",my_top_n = 9)
# sn4_genus_summary.df[!sn4_genus_summary.df$taxonomy_label %in% sn4_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn4_genus_summary.df <- sn4_genus_summary.df %>% group_by(Sample_type) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
# sn4_genus_summary.df <- sn4_genus_summary.df %>% group_by(Sample_type, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn5_genus_summary.df <- generate_taxa_summary(sn5_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sample_type"))
# sn5_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn5_genus_summary.df, grouping_variables = c("Sample_type"),abundance_column = "Mean_relative_abundance",my_top_n = 9)
# sn5_genus_summary.df[!sn5_genus_summary.df$taxonomy_label %in% sn5_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn5_genus_summary.df <- sn5_genus_summary.df %>% group_by(Sample_type) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
# sn5_genus_summary.df <- sn5_genus_summary.df %>% group_by(Sample_type, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn6_genus_summary.df <- generate_taxa_summary(sn6_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sample_type"))
# sn6_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn6_genus_summary.df, grouping_variables = c("Sample_type"),abundance_column = "Mean_relative_abundance",my_top_n = 9)
# sn6_genus_summary.df[!sn6_genus_summary.df$taxonomy_label %in% sn6_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn6_genus_summary.df <- sn6_genus_summary.df %>% group_by(Sample_type) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
# sn6_genus_summary.df <- sn6_genus_summary.df %>% group_by(Sample_type, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn7_genus_summary.df <- generate_taxa_summary(sn7_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sample_type"))
# sn7_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn7_genus_summary.df, grouping_variables = c("Sample_type"),abundance_column = "Mean_relative_abundance",my_top_n = 9)
# sn7_genus_summary.df[!sn7_genus_summary.df$taxonomy_label %in% sn7_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn7_genus_summary.df <- sn7_genus_summary.df %>% group_by(Sample_type) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
# sn7_genus_summary.df <- sn7_genus_summary.df %>% group_by(Sample_type, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn8_genus_summary.df <- generate_taxa_summary(sn8_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sample_type"))
# sn8_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn8_genus_summary.df, grouping_variables = c("Sample_type"),abundance_column = "Mean_relative_abundance",my_top_n = 9)
# sn8_genus_summary.df[!sn8_genus_summary.df$taxonomy_label %in% sn8_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn8_genus_summary.df <- sn8_genus_summary.df %>% group_by(Sample_type) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
# sn8_genus_summary.df <- sn8_genus_summary.df %>% group_by(Sample_type, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# immunocompetent_genus_summary.df$Snapshot <- "Full_cohort"
# sn1_genus_summary.df$Snapshot <- "Snapshot_1"
# sn2_genus_summary.df$Snapshot <- "Snapshot_2"
# sn3_genus_summary.df$Snapshot <- "Snapshot_3"
# sn4_genus_summary.df$Snapshot <- "Snapshot_4"
# sn5_genus_summary.df$Snapshot <- "Snapshot_5"
# sn6_genus_summary.df$Snapshot <- "Snapshot_6"
# sn7_genus_summary.df$Snapshot <- "Snapshot_7"
# sn8_genus_summary.df$Snapshot <- "Snapshot_8"
# combined_snapshot.df <- rbind(immunocompetent_genus_summary.df, sn1_genus_summary.df,sn2_genus_summary.df,sn3_genus_summary.df,sn4_genus_summary.df,sn5_genus_summary.df,sn6_genus_summary.df,sn7_genus_summary.df,sn8_genus_summary.df)
# combined_snapshot.df$Normalised_mean_relative_abundance <- round(combined_snapshot.df$Normalised_mean_relative_abundance*100 ,2)
# write.csv(x = combined_snapshot.df, file = "Result_tables/abundance_analysis_tables/snapshots_top_taxa_summary.csv", row.names = F)
# ------------------------------------------------

# Remove samples that do not have qPCR results
# !is.na(immunosuppressed_genus_data.df$S_aureus_Geq_per_ul_x5) | !is.na(immunosuppressed_genus_data.df$S_aureus_Geq_per_ul_x5)

immunosuppressed_genus_data.df <- immunosuppressed_genus_data.df[!is.na(immunosuppressed_genus_data.df$S_aureus_Geq_per_ul_x5) | 
                                 !is.na(immunosuppressed_genus_data.df$S_aureus_Geq_per_ul_x5),]
immunocompetent_genus_data.df <- immunocompetent_genus_data.df[!is.na(immunocompetent_genus_data.df$S_aureus_Geq_per_ul_x5) | 
                                                                   !is.na(immunocompetent_genus_data.df$S_aureus_Geq_per_ul_x5),]

# immunosuppressed_genus_data.df <- immunosuppressed_genus_data.df[!is.na(immunosuppressed_genus_data.df$Bacterial_load_CFU),]
# immunocompetent_genus_data.df <- immunocompetent_genus_data.df[!is.na(immunocompetent_genus_data.df$Bacterial_load_CFU),]

# Remove factorisation as we will re-assign later
# immunosuppressed_genus_data.df$taxonomy_label <- as.character(immunosuppressed_genus_data.df$taxonomy_label)

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

# Identify the top genus for each lesion type
immunosuppressed_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_genus_summary.df, 
                                                                  grouping_variables = c("Sample_type"),
                                                                  abundance_column = "Mean_relative_abundance",
                                                                  my_top_n = 9)

immunocompetent_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunocompetent_genus_summary.df, 
                                                                grouping_variables = c("Sample_type"),
                                                                abundance_column = "Mean_relative_abundance",
                                                                my_top_n = 9)

immunosuppressed_top_forearm_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_forearm_genus_summary.df, 
                                                                     grouping_variables = c("Sample_type"),
                                                                     abundance_column = "Mean_relative_abundance",
                                                                     my_top_n = 9)

# Create a unique list of the top taxa across all the cohorts / groups
both_cohorts_lesions_top_genus <- unique(c(immunosuppressed_top_genus_summary.df$taxonomy_label, 
                                           immunocompetent_top_genus_summary.df$taxonomy_label,
                                           immunosuppressed_top_forearm_genus_summary.df$taxonomy_label))
# union(union(immunosuppressed_top_genus_summary.df$taxonomy_label, 
#             immunocompetent_top_genus_summary.df$taxonomy_label),
#       immunosuppressed_top_forearm_genus_summary.df$taxonomy_label)

# Create palette based on unique set
both_cohorts_genus_palette <- setNames(my_colour_palette_35_distinct[1:length(both_cohorts_lesions_top_genus)], both_cohorts_lesions_top_genus)
both_cohorts_genus_palette["Other"] <- "grey"


# Take the full table and re-label any taxa not in the top to "Other"
# immunosuppressed_genus_data.df[!immunosuppressed_genus_data.df$taxonomy_label %in% immunosuppressed_genus_data.df$taxonomy_label,]$taxonomy_label <- "Other"
# immunocompetent_genus_data.df[!immunocompetent_genus_data.df$taxonomy_label %in% immunocompetent_genus_data.df$taxonomy_label,]$taxonomy_label <- "Other"
# immunosuppressed_forearm_genus_data.df[!immunosuppressed_forearm_genus_data.df$taxonomy_label %in% immunosuppressed_forearm_genus_data.df$taxonomy_label,]$taxonomy_label <- "Other"

# Take the full table and re-label any taxa not in the top to "Other"
immunosuppressed_genus_summary.df[!immunosuppressed_genus_summary.df$taxonomy_label %in% immunosuppressed_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
immunocompetent_genus_summary.df[!immunocompetent_genus_summary.df$taxonomy_label %in% immunocompetent_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
immunosuppressed_forearm_genus_summary.df[!immunosuppressed_forearm_genus_summary.df$taxonomy_label %in% immunosuppressed_top_forearm_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"

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

# immunosuppressed_genus_summary.df %>% group_by(Sample_type,taxonomy_label) %>% dplyr::summarise(Mean_relative_abundance = sum(Mean_relative_abundance)) %>% filter(grepl("g__Staph", taxonomy_label))
# immunosuppressed_genus_summary.df %>% group_by(Sample_type,taxonomy_label) %>% dplyr::summarise(Summed_relative_abundance = sum(Summed_relative_abundance)) %>% filter(grepl("g__Staph", taxonomy_label))

sum(immunosuppressed_genus_summary.df$Mean_relative_abundance)
sum(immunosuppressed_genus_summary.df$Normalised_mean_relative_abundance)

# Need a single entry for the Other group
# TODO - check abundance values, non-zero etc
# immunosuppressed_genus_summary.df %>% 
  # group_by(Sample_type, taxonomy_label) %>% 

immunosuppressed_genus_summary.df <-
  immunosuppressed_genus_summary.df %>% 
  group_by(Sample_type, taxonomy_label) %>% 
  dplyr::summarise(Mean_relative_abundance = mean(Mean_relative_abundance), 
                   Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% 
  as.data.frame()


immunocompetent_genus_summary.df <- 
  immunocompetent_genus_summary.df %>% 
  group_by(Sample_type, taxonomy_label) %>% 
  dplyr::summarise(Mean_relative_abundance = mean(Mean_relative_abundance), 
                   Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% 
  as.data.frame()

immunosuppressed_forearm_genus_summary.df <- 
  immunosuppressed_forearm_genus_summary.df %>% 
  group_by(Sample_type, taxonomy_label) %>% 
  dplyr::summarise(Mean_relative_abundance = max(Mean_relative_abundance), 
                   Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% 
  as.data.frame()
sum(immunosuppressed_genus_summary.df$Mean_relative_abundance)
sum(immunosuppressed_genus_summary.df$Normalised_mean_relative_abundance)

immunosuppressed_genus_summary.df[immunosuppressed_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunocompetent_genus_summary.df[immunocompetent_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA
immunosuppressed_forearm_genus_summary.df[immunosuppressed_forearm_genus_summary.df$taxonomy_label == "Other","Mean_relative_abundance"] <- NA

# Calculate the mean qPCR totals for each lesion type
immunosuppressed_mean_qpcr_totals.df <-
  immunosuppressed_genus_data.df %>% 
  group_by(Sample_type) %>% 
  dplyr::summarise(N_samples = n_distinct(Sample), #!!!
                   Mean_Staph_spp_Geq_per_ul_x5 = mean(Staph_spp_Geq_per_ul_x5, na.rm = T),
                   Mean_S_aureus_Geq_per_ul_x5 = mean(S_aureus_Geq_per_ul_x5, na.rm = T)
                   ) %>% 
  as.data.frame()

# immunosuppressed_sf_mean_bacterial_loads.df <-
#   immunosuppressed_genus_data.df %>% 
#   group_by(Sample_type) %>% 
#   dplyr::summarise(Mean_bacterial_load = mean(Bacterial_load_CFU, na.rm = T)) %>% 
#   as.data.frame()

rownames(immunosuppressed_sf_mean_bacterial_loads.df) <- immunosuppressed_sf_mean_bacterial_loads.df$Sample_type
# immunocompetent_sf_mean_bacterial_loads.df <- immunocompetent_genus_data.df %>% group_by(Sample_type) %>% dplyr::summarise(Mean_bacterial_load = mean(Bacterial_load_CFU)) %>% as.data.frame()
# rownames(immunocompetent_sf_mean_bacterial_loads.df) <- immunocompetent_sf_mean_bacterial_loads.df$Sample_type
immunosuppressed_forearm_sf_mean_bacterial_loads.df <- immunosuppressed_forearm_genus_data.df %>% group_by(Sample_type) %>% dplyr::summarise(Mean_bacterial_load = mean(Bacterial_load_CFU)) %>% as.data.frame()
rownames(immunosuppressed_forearm_sf_mean_bacterial_loads.df) <- immunosuppressed_forearm_sf_mean_bacterial_loads.df$Sample_type

# Calculate abundance value proportional to bacterial load
immunosuppressed_genus_summary.df$Mean_relative_abundance_BL_proportional <-
  apply(immunosuppressed_genus_summary.df, 1, function(x) as.numeric(x["Normalised_mean_relative_abundance"]) * immunosuppressed_sf_mean_bacterial_loads.df[x["Sample_type"],"Mean_bacterial_load"])

immunocompetent_genus_summary.df$Mean_relative_abundance_BL_proportional <- NA
#   apply(immunocompetent_genus_summary.df, 1, function(x) as.numeric(x["Mean_relative_abundance"]) * immunocompetent_sf_mean_bacterial_loads.df[x["Sample_type"],"Mean_bacterial_load"])

immunosuppressed_forearm_genus_summary.df$Mean_relative_abundance_BL_proportional <-
  apply(immunosuppressed_forearm_genus_summary.df, 1, function(x) as.numeric(x["Normalised_mean_relative_abundance"]) * immunosuppressed_forearm_sf_mean_bacterial_loads.df[x["Sample_type"],"Mean_bacterial_load"])

# Get unique list of taxa, ensure "Other" is first
# most_abundant_taxa <- sort(unique(immunosuppressed_genus_summary.df$taxonomy_label))
# most_abundant_taxa <- c("Other", most_abundant_taxa[!grepl("Other", most_abundant_taxa)])


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


# Combine the cohorts summaries
immunosuppressed_genus_summary.df$Cohort <- "immunosuppressed"
immunocompetent_genus_summary.df$Cohort <- "immunocompetent"
immunosuppressed_forearm_genus_summary.df$Cohort <- "immunosuppressed_forearm"

both_cohorts_taxa_summary.df <- rbind(immunosuppressed_genus_summary.df,immunocompetent_genus_summary.df)
both_cohorts_taxa_summary.df <- both_cohorts_taxa_summary.df %>% group_by(Sample_type) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(as.character(unique(both_cohorts_taxa_summary.df$taxonomy_label))[as.character(unique(both_cohorts_taxa_summary.df$taxonomy_label)) != "Other"], "Other")
both_cohorts_taxa_summary.df$taxonomy_label <- factor(both_cohorts_taxa_summary.df$taxonomy_label, levels = my_levels)

write.csv(both_cohorts_taxa_summary.df[with(both_cohorts_taxa_summary.df, order(Cohort, Sample_type, Normalised_mean_relative_abundance)),],
          file = "Result_tables/abundance_analysis_tables/both_cohorts_sampletype_final_refined_normalised_mean_relative_abundance_top_genera.csv",
          row.names = F,
          quote = F)


# ------------------------------------------------------------------------------------
# Now plot

# Immunosuppressed
immunosuppressed_just_legend_plot <- ggplot(immunosuppressed_genus_summary.df, aes(x = Sample_type, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 5)) +
  xlab("Sample site") +
  ylab("Mean relative abundance") +
  common_theme


# bars <- map(unique(dats$id)
#             , ~geom_bar(stat = "identity", position = "stack"
#                         , data = dats %>% filter(id == .x)))
# dats %>% 
#   ggplot(aes(x = id, y = value, fill = reorder(filling,-ordering))) + 
#   bars +
#   guides(fill=guide_legend("ordering"))

# bars <- map(unique(immunosuppressed_genus_summary.df$Sample_type), ~geom_bar(stat = "identity", position = "stack", data = immunosuppressed_genus_summary.df %>% filter(Sample_type == .x)))

immunosuppressed_abundance_plot <- ggplot(immunosuppressed_genus_summary.df, aes(x = Sample_type, y = Normalised_mean_relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  ylab("Mean relative abundance (%)") +
  common_theme

immunosuppressed_BL_abundance_plot <- ggplot(immunosuppressed_genus_summary.df, aes(x = Sample_type, y = Mean_relative_abundance_BL_proportional, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette,guide = F) +
  scale_y_continuous(breaks = seq(0,30000, by = 5000), limits=c(0,30001)) +
  # xlab("Sample site") +
  xlab("") +
  ylab("Normalised mean relative abundance scaled by mean bacterial load (CFU)") +
  
  common_theme +
  theme(axis.text.y = element_blank())


# Extract the legend
my_legend_taxa <- cowplot::get_legend(immunosuppressed_just_legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 6),
                                          legend.title = element_text(size=7, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)

# Make a grid of plots with the list of plots for both cohorts
grid_plot <- plot_grid(plotlist = list(immunosuppressed_abundance_plot, NULL, immunosuppressed_BL_abundance_plot),ncol = 3,nrow=1, rel_widths = c(1,-.12,1),align = "hv")
grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.4), ncol = 1, nrow=2)
ggsave(filename = "Result_figures/abundance_analysis_plots/immunosuppressed_sampletype_relative_abundance_and_bacterial_load.pdf", plot = grid_plot, width = 30, height = 8, units = "cm")

# ----------------------------------------
# Immunocompentent
immunocompentent_just_legend_plot <- ggplot(immunocompetent_genus_summary.df, aes(x = Sample_type, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 5)) +
  xlab("Sample site") +
  ylab("Mean relative abundance") +
  common_theme 

immunocompentent_abundance_plot <- ggplot(immunocompetent_genus_summary.df, aes(x = Sample_type, y = Normalised_mean_relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  ylab("Mean relative abundance (%)") +
  common_theme

immunocompentent_BL_abundance_plot <- ggplot(immunocompetent_genus_summary.df, aes(x = Sample_type, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette,guide = F) +
  scale_y_continuous(breaks = seq(0,30000, by = 5000), limits=c(0,30001)) +
  # xlab("Sample site") +
  xlab("") +
  ylab("Normalised mean relative abundance scaled by mean bacterial load (CFU)") +
  
  common_theme +
  theme(axis.text.y = element_blank())


# Extract the legend
my_legend_taxa <- cowplot::get_legend(immunocompentent_just_legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 6),
                                          legend.title = element_text(size=7, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)

# Make a grid of plots with the list of plots for both cohorts
# grid_plot <- plot_grid(plotlist = list(abundance_plot, NULL, BL_abundance_plot),ncol = 3,nrow=1, rel_widths = c(1,-.12,1),align = "hv")
grid_plot <- plot_grid(plotlist = list(immunocompentent_abundance_plot, NULL, NULL),ncol = 3,nrow=1, rel_widths = c(1,-.12,1),align = "hv")
grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.4), ncol = 1, nrow=2)
ggsave(filename = "Result_figures/abundance_analysis_plots/immunocompetent_sampletype_relative_abundance_and_bacterial_load.pdf", plot = grid_plot, width = 30, height = 7, units = "cm")


# Combined plot
# Extract the legend
combined_just_legend_plot <- ggplot(both_cohorts_taxa_summary.df, aes(x = Sample_type, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 6)) +
  xlab("Sample site") +
  ylab("Mean relative abundance") +
  common_theme
# + guides(fill = guide_legend(override.aes = list(size=2)))

my_legend_taxa <- cowplot::get_legend(combined_just_legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 5),
                                          legend.title = element_text(size=5, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          legend.key.width=unit(.2, "cm"),
                                          legend.key.height=unit(.1, "cm")
                                          # plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)
suppressed_title <- ggdraw() + 
  draw_label(
    "Immunosuppressed",
    fontface = 'bold',
    size = 8,
    x = .5,
  ) 

competent_title <- ggdraw() + 
  draw_label(
    "Immunocompetent",
    fontface = 'bold',
    size = 8,
    x = .5,
  )


grid_plot <- plot_grid(plotlist = list(immunosuppressed_abundance_plot + ylab(""),NULL,immunosuppressed_BL_abundance_plot+ylab("")), ncol = 3, nrow =1, rel_widths = c(1,-.05,1),align = "hv")
grid_plot <- plot_grid(suppressed_title, grid_plot,ncol = 1,rel_heights = c(0.1, 1))
grid_plot2 <- plot_grid(plotlist = list(immunocompentent_abundance_plot,NULL,immunocompentent_BL_abundance_plot), ncol = 3, nrow =1, rel_widths = c(1,-.05,1),align = "hv")
grid_plot2 <- plot_grid(competent_title, grid_plot2,ncol = 1,rel_heights = c(0.1, 1))

grid_plot <- plot_grid(plotlist = list(grid_plot, NULL, grid_plot2, my_legend_taxa),rel_heights = c(1, -.15, 1,.5), ncol = 1, nrow =4)
grid_plot
ggsave(filename = "Result_figures/abundance_analysis_plots/both_cohorts_sampletype_relative_abundance_and_bacterial_load.pdf", plot = grid_plot, width = 30, height = 12, units = "cm")

# 
# 
# grid_plot <- plot_grid(plotlist = list(immunosuppressed_abundance_plot + ylab(""),NULL,immunosuppressed_BL_abundance_plot+ylab(""),
#                           NULL, NULL,NULL,
#                           immunocompentent_abundance_plot,NULL, immunocompentent_BL_abundance_plot), ncol = 3, nrow=3, rel_widths = c(1,-0.1,1), rel_heights = c(1,-0.15,0.9), align = "hv")
# 
# grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.3), ncol = 1, nrow=2)
# # grid_plot
# ggsave(filename = "Result_figures/abundance_analysis_plots/combined_sampletype_relative_abundance_and_bacterial_load.pdf", plot = grid_plot, width = 30, height = 12, units = "cm")

# ---------------------------------------------------------------------------------
# Now create figure for just forearm samples

immunosuppressed_forearm_just_legend_plot <- ggplot(immunosuppressed_forearm_genus_summary.df, aes(x = Sample_type, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 2)) +
  xlab("Sample site") +
  ylab("Mean relative abundance") +
  common_theme 

immunosuppressed_forearm_abundance_plot <- ggplot(immunosuppressed_forearm_genus_summary.df, aes(x = Sample_type, y = Normalised_mean_relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  # geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  ylab("Mean relative abundance (%)") +
  common_theme

immunosuppressed_forearm_BL_abundance_plot <- ggplot(immunosuppressed_forearm_genus_summary.df, aes(x = Sample_type, y = Mean_relative_abundance_BL_proportional, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette,guide = F) +
  scale_y_continuous(breaks = seq(0,30000, by = 5000), limits=c(0,30101)) +
  # xlab("Sample site") +
  xlab("") +
  ylab("Mean relative abundance scaled by mean bacterial load (CFU)") +
  
  common_theme +
  theme(axis.text.y = element_blank())


# Extract the legend
my_legend_taxa <- cowplot::get_legend(immunosuppressed_forearm_just_legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 6),
                                          legend.title = element_text(size=7, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)

# Make a grid of plots with the list of plots for both cohorts
grid_plot <- plot_grid(plotlist = list(immunosuppressed_forearm_abundance_plot, NULL, immunosuppressed_forearm_BL_abundance_plot),
                       ncol = 3,nrow=1, rel_widths = c(1,-.08,1),align = "hv")
mytitle <- ggdraw() + 
  draw_label(
    "Immunosuppressed, forearm SCC",
    fontface = 'bold',
    size = 8,
    x = .5,
  ) 
grid_plot <- plot_grid(
  mytitle, grid_plot,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.2, 1)
)

grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.4), ncol = 1, nrow=2)
ggsave(filename = "Result_figures/abundance_analysis_plots/immunosuppressed_forearm_sampletype_relative_abundance_and_bacterial_load.pdf", plot = grid_plot, width = 30, height = 4, units = "cm")


