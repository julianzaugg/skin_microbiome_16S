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

# ------------------------------------------------------------------------------------------
### Calculate the (min, max, mean, median, stdev, #samples) abundances of each taxa at each taxa level
generate_taxa_summary <- function(mydata, taxa_column, group_by_columns){
  # select_columns <- c(taxa_column, group_by_columns, "Sample", "Patient", "Read_count", "Read_count_rarefied", "Relative_abundance", "Relative_abundance_rarefied", "Bacterial_load_CFU")
  select_columns <- c(taxa_column, group_by_columns, "Sample", "Patient", "Read_count", "Read_count_rarefied", "Relative_abundance", "Relative_abundance_rarefied")
  total_samples <- length(unique(mydata$Sample))
  total_patients <- length(unique(mydata$Patient))
  taxa_group_summary <- 
    mydata %>%
    # dplyr::filter(retained = "yes") %>% # keep only those samples that were retained
    dplyr::select_(.dots = select_columns) %>%
    dplyr::group_by_(.dots = c(taxa_column, group_by_columns)) %>%
    dplyr::mutate(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% # number of unique samples/index
    dplyr::group_by_(.dots = c(group_by_columns)) %>%
    dplyr::mutate(N_total_samples_in_group = n_distinct(Sample),
                  N_total_patients_in_group = n_distinct(Patient))  %>%
    dplyr::group_by_(.dots = c(group_by_columns, taxa_column)) %>%
    dplyr::select(-Sample, -Patient) %>%
    dplyr::summarise(N_samples = max(N_samples),
                     N_total_samples_in_group = max(N_total_samples_in_group),
                     N_patients = max(N_patients),
                     N_total_patients_in_group = max(N_total_patients_in_group),
                     Percent_group_samples = round((max(N_samples) / max(N_total_samples_in_group))*100, 2),
                     Percent_total_samples = round((max(N_samples) / total_samples)*100, 2),
                     Percent_group_patients = round((max(N_patients) / max(N_total_patients_in_group))*100, 2),
                     Percent_total_patients = round((max(N_patients) / total_patients)*100, 2),
                     Mean_read_count = round(mean(Read_count), 2),
                     Median_read_count = median(Read_count),
                     Min_read_count = min(Read_count),
                     Max_read_count = max(Read_count),
                     Summed_read_count = sum(Read_count),
                     
                     Mean_read_count_rarefied = round(mean(Read_count_rarefied),2),
                     Median_read_count_rarefied = median(Read_count_rarefied),
                     Min_read_count_rarefied = min(Read_count_rarefied),
                     Max_read_count_rarefied = max(Read_count_rarefied),
                     Summed_read_count_rarefied = sum(Read_count_rarefied),
                     
                     Mean_relative_abundance = round(mean(Relative_abundance), 5),
                     Median_relative_abundance = round(median(Relative_abundance), 5),
                     Min_relative_abundance = round(min(Relative_abundance),5),
                     Max_relative_abundance = round(max(Relative_abundance),5),
                     Summed_relative_abundance = round(sum(Relative_abundance),5),
                     
                     Mean_relative_abundance_rarefied = round(mean(Relative_abundance_rarefied), 5),
                     Median_relative_abundance_rarefied = round(median(Relative_abundance_rarefied), 5),
                     Min_relative_abundance_rarefied = round(min(Relative_abundance_rarefied), 5),
                     Max_relative_abundance_rarefied = round(max(Relative_abundance_rarefied), 5),
                     Summed_relative_abundance_rarefied = round(sum(Relative_abundance_rarefied),5),
                     
                     # Mean_bacterial_load = round(mean(Bacterial_load_CFU), 5),
                     # Median_bacterial_load = round(median(Bacterial_load_CFU), 5),
                     # Min_bacterial_load = round(min(Bacterial_load_CFU), 5),
                     # Max_bacterial_load = round(max(Bacterial_load_CFU), 5),
                     # Summed_bacterial_load = round(sum(Bacterial_load_CFU),5)
    ) %>%
    as.data.frame()
  return(taxa_group_summary)
}

filter_summary_to_top_n <- function(taxa_summary, grouping_variables, abundance_column, my_top_n = 10){
  # Get the top N taxa as described in a provided taxa summary table.
  out <- 
    taxa_summary %>%
    dplyr::group_by_(.dots = c(grouping_variables)) %>%
    dplyr::arrange(dplyr::desc(get(abundance_column))) %>%
    dplyr::top_n(my_top_n, get(abundance_column)) %>% 
    # dplyr::arrange_(.dots = c(grouping_variables),abundance_column) %>%
    dplyr::arrange_(.dots = c(grouping_variables)) %>%
    as.data.frame()
  return(out)
}


# ------------------------------------------------------------------------------------------
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC and SCC lesions. 
# Remove samples for different lesion types (nasal,scar,scar_PL,KA,KA_PL,VV,VV_PL,SF,SF_PL,other,other_PL) from metadata and otu table
# metadata.df <- metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC", "LC"),]
metadata.df <- metadata.df[!metadata.df$Sampletype == "negative",]

forearm_swab_ids <- c("1383","1385","1470","1561","1599","1647","1649")
forearm_indices <- c("R1383_J1425", "SA6550_J1427", "R1368_J1477", "R1460_J1425", "R1498_J1425", "SB4909_J1426", "SB4911_J1426")
# R1498_J1425  R1460_J1425  R1368_J1477  SA6550_J1427 SB4909_J1426
# SB4911_J1426  R1498_J1425 R1383_J1425
# subset(subset(metadata.df, Project != "immunocompromised")[,c("Sampletype", "Sampletype_final")], Sampletype == "C")

# Load filtered/processed abundance data with metadata
genus_data.df <- read.csv("Result_tables/other/genus_counts_abundances_and_metadata.csv")

# unique(subset(genus_data.df, Project == "immunocompetent" & Sampletype_final == "LC")$Sample)
# length(unique(subset(genus_data.df, Project == "immunocompetent" & Sampletype_final == "LC")$Sample))

genus_data.df <- genus_data.df[genus_data.df$Sample %in% rownames(metadata.df),]
# length(unique(subset(genus_data.df, Project == "immunocompetent" & Sampletype_final == "LC")$Sample))

# ------------------------------
# TEMP
# Change Sampletype_final to different labels
# genus_data.df$Sampletype_final <- as.character(genus_data.df$Sampletype_final)
# genus_data.df[genus_data.df$Project == "immunocompetent",][genus_data.df[genus_data.df$Project == "immunocompetent",]$Sampletype %in% c("LC", "AK_PL"),]$Sampletype_final <- "C_P"
# genus_data.df[genus_data.df$Project == "immunocompetent",][genus_data.df[genus_data.df$Project == "immunocompetent",]$Sampletype %in% c("AK"),]$Sampletype_final <- "C_P"
# genus_data.df[genus_data.df$Project == "immunocompetent",][genus_data.df[genus_data.df$Project == "immunocompetent",]$Sampletype %in% c("LC", "AK_PL"),]$Sampletype_final <- "C_P"
# genus_data.df[genus_data.df$Project == "immunocompetent",][genus_data.df[genus_data.df$Project == "immunocompetent",]$Sampletype %in% c("LC", "AK_PL"),]$Sampletype_final <- "C_P"

# C_P (C1-3 and AK_PL) – P indicating photo-damaged; as Nancy said they are not direct AK controls this may be the more appropriate description
# AK (AK)
# SCC_PL (SCC_PL and IEC_PL)
# SCC (SCC and IEC)


# ------------------------------
# Set levels
# genus_data.df$Sampletype_pooled <- factor(genus_data.df$Sampletype_pooled, levels = c("LC", "AK","SCC"))
# genus_data.df$Sampletype_compromised_refined <- factor(genus_data.df$Sampletype_compromised_refined, levels = c("C","LC", "AK","SCC"))
# genus_data.df$Sampletype_final <- factor(genus_data.df$Sampletype_final, levels = c("C","LC", "AK","SCC"))
genus_data.df$Sampletype_final <- factor(genus_data.df$Sampletype_final, levels = c("SCC","AK","LC", "C"))
# genus_data.df$Sampletype_final_refined <- factor(genus_data.df$Sampletype_final_refined, levels = c("C", "C_P", "AK", "SCC_PL", "SCC"))

# Create taxonomy label
# Domain, family, genus
# if family is Unassigned, use last assignment
# genus_data.df$taxonomy_label <- as.character(genus_data.df$taxonomy_genus)
genus_data.df$taxonomy_label <- with(genus_data.df, paste0(Domain,";", Class,";", Genus))
# temp <- "d__Bacteria;p__Planctomycetes;c__vadinHA49;Unassigned;Unassigned;Unassigned"
# temp <- "d__Bacteria;p__Actinobacteria;c__Acidimicrobiia;o__uncultured;f__uncultured bacterium;g__uncultured bacterium" 
# gsub("^.*([a-z]__.*?);Unassigned.*","\\1", temp)
# genus_data.df$taxonomy_label <- gsub(".*(f__.*)", "\\1",genus_data.df$Family_Genus)

# Only snapshot and immunocompromised samples
genus_data.df <- subset(genus_data.df, Project == "immunocompromised" | Snapshot_sample_1 == "yes")
# genus_data.df <- subset(genus_data.df, Project == "immunocompromised" | Snapshot_sample_2 == "yes")
# genus_data.df <- subset(genus_data.df, Project == "immunocompromised" | Snapshot_sample_3 == "yes")
# genus_data.df <- subset(genus_data.df, Project == "immunocompromised" | Snapshot_sample_4 == "yes")
# genus_data.df <- subset(genus_data.df, Project == "immunocompromised" | Snapshot_sample_5 == "yes")
# genus_data.df <- subset(genus_data.df, Project == "immunocompromised" | Snapshot_sample_6 == "yes")
# genus_data.df <- subset(genus_data.df, Project == "immunocompromised" | Snapshot_sample_7 == "yes")
# genus_data.df <- subset(genus_data.df, Project == "immunocompromised" | Snapshot_sample_8 == "yes")

immunocompromised_genus_data.df <- subset(genus_data.df, Project == "immunocompromised")
immunocompromised_forearm_genus_data.df <- subset(genus_data.df, Project == "immunocompromised" & Sample %in% forearm_indices)
# immunocompetent_genus_data.df <- subset(genus_data.df, Project == "immunocompetent" & Snapshot_sample_1 == "yes")
immunocompetent_genus_data.df <- subset(genus_data.df, Project == "immunocompetent")

immunocompromised_genus_data.df$Sampletype_final_refined <- factor(immunocompromised_genus_data.df$Sampletype_final_refined, levels = rev(c("C","C_P", "AK", "SCC_PL", "SCC")))
immunocompetent_genus_data.df$Sampletype_final_refined <- factor(immunocompetent_genus_data.df$Sampletype_final_refined, levels = rev(c("C_P", "AK", "SCC_PL", "SCC")))
immunocompromised_forearm_genus_data.df$Sampletype_final_refined <- factor(immunocompromised_forearm_genus_data.df$Sampletype_final_refined, levels = rev(c("C","C_P", "AK", "SCC_PL", "SCC")))

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
# immunocompetent_summary.df <- immunocompetent_genus_data.df %>% group_by(Sampletype_final_refined) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn1_summary.df <- sn1_genus_data.df %>% group_by(Sampletype_final_refined) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn2_summary.df <- sn2_genus_data.df %>% group_by(Sampletype_final_refined) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn3_summary.df <- sn3_genus_data.df %>% group_by(Sampletype_final_refined) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn4_summary.df <- sn4_genus_data.df %>% group_by(Sampletype_final_refined) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn5_summary.df <- sn5_genus_data.df %>% group_by(Sampletype_final_refined) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn6_summary.df <- sn6_genus_data.df %>% group_by(Sampletype_final_refined) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn7_summary.df <- sn7_genus_data.df %>% group_by(Sampletype_final_refined) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
# sn8_summary.df <- sn8_genus_data.df %>% group_by(Sampletype_final_refined) %>% summarise(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% as.data.frame()
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
# immunocompetent_genus_summary.df <- generate_taxa_summary(immunocompetent_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sampletype_final_refined"))
# immunocompetent_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunocompetent_genus_summary.df, grouping_variables = c("Sampletype_final_refined"),abundance_column = "Mean_relative_abundance_rarefied",my_top_n = 9)
# immunocompetent_genus_summary.df[!immunocompetent_genus_summary.df$taxonomy_label %in% immunocompetent_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# immunocompetent_genus_summary.df <- immunocompetent_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
# immunocompetent_genus_summary.df <- immunocompetent_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn1_genus_summary.df <- generate_taxa_summary(sn1_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sampletype_final_refined"))
# sn1_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn1_genus_summary.df, grouping_variables = c("Sampletype_final_refined"),abundance_column = "Mean_relative_abundance_rarefied",my_top_n = 9)
# sn1_genus_summary.df[!sn1_genus_summary.df$taxonomy_label %in% sn1_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn1_genus_summary.df <- sn1_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
# sn1_genus_summary.df <- sn1_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn2_genus_summary.df <- generate_taxa_summary(sn2_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sampletype_final_refined"))
# sn2_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn2_genus_summary.df, grouping_variables = c("Sampletype_final_refined"),abundance_column = "Mean_relative_abundance_rarefied",my_top_n = 9)
# sn2_genus_summary.df[!sn2_genus_summary.df$taxonomy_label %in% sn2_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn2_genus_summary.df <- sn2_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
# sn2_genus_summary.df <- sn2_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn3_genus_summary.df <- generate_taxa_summary(sn3_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sampletype_final_refined"))
# sn3_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn3_genus_summary.df, grouping_variables = c("Sampletype_final_refined"),abundance_column = "Mean_relative_abundance_rarefied",my_top_n = 9)
# sn3_genus_summary.df[!sn3_genus_summary.df$taxonomy_label %in% sn3_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn3_genus_summary.df <- sn3_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
# sn3_genus_summary.df <- sn3_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn4_genus_summary.df <- generate_taxa_summary(sn4_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sampletype_final_refined"))
# sn4_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn4_genus_summary.df, grouping_variables = c("Sampletype_final_refined"),abundance_column = "Mean_relative_abundance_rarefied",my_top_n = 9)
# sn4_genus_summary.df[!sn4_genus_summary.df$taxonomy_label %in% sn4_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn4_genus_summary.df <- sn4_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
# sn4_genus_summary.df <- sn4_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn5_genus_summary.df <- generate_taxa_summary(sn5_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sampletype_final_refined"))
# sn5_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn5_genus_summary.df, grouping_variables = c("Sampletype_final_refined"),abundance_column = "Mean_relative_abundance_rarefied",my_top_n = 9)
# sn5_genus_summary.df[!sn5_genus_summary.df$taxonomy_label %in% sn5_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn5_genus_summary.df <- sn5_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
# sn5_genus_summary.df <- sn5_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn6_genus_summary.df <- generate_taxa_summary(sn6_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sampletype_final_refined"))
# sn6_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn6_genus_summary.df, grouping_variables = c("Sampletype_final_refined"),abundance_column = "Mean_relative_abundance_rarefied",my_top_n = 9)
# sn6_genus_summary.df[!sn6_genus_summary.df$taxonomy_label %in% sn6_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn6_genus_summary.df <- sn6_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
# sn6_genus_summary.df <- sn6_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn7_genus_summary.df <- generate_taxa_summary(sn7_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sampletype_final_refined"))
# sn7_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn7_genus_summary.df, grouping_variables = c("Sampletype_final_refined"),abundance_column = "Mean_relative_abundance_rarefied",my_top_n = 9)
# sn7_genus_summary.df[!sn7_genus_summary.df$taxonomy_label %in% sn7_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn7_genus_summary.df <- sn7_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
# sn7_genus_summary.df <- sn7_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
# 
# sn8_genus_summary.df <- generate_taxa_summary(sn8_genus_data.df,taxa_column = "taxonomy_label", group_by_columns = c("Sampletype_final_refined"))
# sn8_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = sn8_genus_summary.df, grouping_variables = c("Sampletype_final_refined"),abundance_column = "Mean_relative_abundance_rarefied",my_top_n = 9)
# sn8_genus_summary.df[!sn8_genus_summary.df$taxonomy_label %in% sn8_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# sn8_genus_summary.df <- sn8_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
# sn8_genus_summary.df <- sn8_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
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

# Remove samples that do not have a bacterial load CFU value
immunocompromised_genus_data.df <- immunocompromised_genus_data.df[!is.na(immunocompromised_genus_data.df$Bacterial_load_CFU),]
# immunocompetent_genus_data.df <- immunocompetent_genus_data.df[!is.na(immunocompetent_genus_data.df$Bacterial_load_CFU),]

# Remove factorisation as we will re-assign later
# immunocompromised_genus_data.df$taxonomy_label <- as.character(immunocompromised_genus_data.df$taxonomy_label)

# Generate full genus summary for each lesion type
immunocompromised_genus_summary.df <- generate_taxa_summary(immunocompromised_genus_data.df,
                                                            taxa_column = "taxonomy_label", 
                                                            group_by_columns = c("Sampletype_final_refined"))
immunocompetent_genus_summary.df <- generate_taxa_summary(immunocompetent_genus_data.df,
                                                          taxa_column = "taxonomy_label", 
                                                          group_by_columns = c("Sampletype_final_refined"))
immunocompromised_forearm_genus_summary.df <- generate_taxa_summary(immunocompromised_forearm_genus_data.df,
                                                                     taxa_column = "taxonomy_label", 
                                                                     group_by_columns = c("Sampletype_final_refined"))

# Identify the top genus for each lesion type
immunocompromised_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunocompromised_genus_summary.df, 
                                                                  grouping_variables = c("Sampletype_final_refined"),
                                                                  abundance_column = "Mean_relative_abundance_rarefied",
                                                                  my_top_n = 9)

immunocompetent_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunocompetent_genus_summary.df, 
                                                                grouping_variables = c("Sampletype_final_refined"),
                                                                abundance_column = "Mean_relative_abundance_rarefied",
                                                                my_top_n = 9)
immunocompromised_top_forearm_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunocompromised_forearm_genus_summary.df, 
                                                                     grouping_variables = c("Sampletype_final_refined"),
                                                                     abundance_column = "Mean_relative_abundance_rarefied",
                                                                     my_top_n = 9)

# immunocompromised_top_forearm_genus_summary.df$taxonomy_label %in% both_cohorts_lesions_top_genus
# both_cohorts_lesions_top_genus <- unique(c(immunocompromised_top_genus_summary.df$taxonomy_label, immunocompetent_top_genus_summary.df$taxonomy_label))
both_cohorts_lesions_top_genus <- unique(c(immunocompromised_top_genus_summary.df$taxonomy_label, 
                                           immunocompetent_top_genus_summary.df$taxonomy_label,
                                           immunocompromised_top_forearm_genus_summary.df$taxonomy_label))


# Create palette based on unique set
both_cohorts_genus_palette <- setNames(my_colour_palette_30_distinct[1:length(both_cohorts_lesions_top_genus)], both_cohorts_lesions_top_genus)
both_cohorts_genus_palette["Other"] <- "grey"


# Take the full table and re-label any taxa not in the top to "Other"
immunocompromised_genus_summary.df[!immunocompromised_genus_summary.df$taxonomy_label %in% immunocompromised_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
immunocompetent_genus_summary.df[!immunocompetent_genus_summary.df$taxonomy_label %in% immunocompetent_top_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
immunocompromised_forearm_genus_summary.df[!immunocompromised_forearm_genus_summary.df$taxonomy_label %in% immunocompromised_top_forearm_genus_summary.df$taxonomy_label,]$taxonomy_label <- "Other"

# immunocompromised_genus_summary.df[!immunocompromised_genus_summary.df$taxonomy_label %in% both_cohorts_lesions_top_genus,]$taxonomy_label <- "Other"
# immunocompetent_genus_summary.df[!immunocompetent_genus_summary.df$taxonomy_label %in% both_cohorts_lesions_top_genus,]$taxonomy_label <- "Other"
# immunocompromised_forearm_genus_summary.df[!immunocompromised_forearm_genus_summary.df$taxonomy_label %in% both_cohorts_lesions_top_genus,]$taxonomy_label <- "Other"


# Normalise the Mean_relative_abundance_rarefied values within each lesion
immunocompromised_genus_summary.df <- immunocompromised_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
immunocompetent_genus_summary.df <- immunocompetent_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
immunocompromised_forearm_genus_summary.df<- immunocompromised_forearm_genus_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()

# immunocompromised_genus_summary.df %>% group_by(Sampletype_final_refined,taxonomy_label) %>% dplyr::summarise(Mean_relative_abundance_rarefied = sum(Mean_relative_abundance_rarefied)) %>% filter(grepl("g__Staph", taxonomy_label))
# immunocompromised_genus_summary.df %>% group_by(Sampletype_final_refined,taxonomy_label) %>% dplyr::summarise(Summed_relative_abundance_rarefied = sum(Summed_relative_abundance_rarefied)) %>% filter(grepl("g__Staph", taxonomy_label))

# Need a single entry for the Other group
immunocompromised_genus_summary.df <- immunocompromised_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
immunocompetent_genus_summary.df <- immunocompetent_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()
immunocompromised_forearm_genus_summary.df <- immunocompromised_forearm_genus_summary.df %>% group_by(Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% as.data.frame()

# Calculate the mean bacterial loads for each lesion type
immunocompromised_sf_mean_bacterial_loads.df <- immunocompromised_genus_data.df %>% group_by(Sampletype_final_refined) %>% dplyr::summarise(Mean_bacterial_load = mean(Bacterial_load_CFU, na.rm = T)) %>% as.data.frame()

rownames(immunocompromised_sf_mean_bacterial_loads.df) <- immunocompromised_sf_mean_bacterial_loads.df$Sampletype_final_refined
# immunocompetent_sf_mean_bacterial_loads.df <- immunocompetent_genus_data.df %>% group_by(Sampletype_final_refined) %>% dplyr::summarise(Mean_bacterial_load = mean(Bacterial_load_CFU)) %>% as.data.frame()
# rownames(immunocompetent_sf_mean_bacterial_loads.df) <- immunocompetent_sf_mean_bacterial_loads.df$Sampletype_final_refined
immunocompromised_forearm_sf_mean_bacterial_loads.df <- immunocompromised_forearm_genus_data.df %>% group_by(Sampletype_final_refined) %>% dplyr::summarise(Mean_bacterial_load = mean(Bacterial_load_CFU)) %>% as.data.frame()
rownames(immunocompromised_forearm_sf_mean_bacterial_loads.df) <- immunocompromised_forearm_sf_mean_bacterial_loads.df$Sampletype_final_refined

# Calculate abundance value proportional to bacterial load
immunocompromised_genus_summary.df$Mean_relative_abundance_rarefied_BL_proportional <-
  apply(immunocompromised_genus_summary.df, 1, function(x) as.numeric(x["Normalised_mean_relative_abundance"]) * immunocompromised_sf_mean_bacterial_loads.df[x["Sampletype_final_refined"],"Mean_bacterial_load"])

immunocompetent_genus_summary.df$Mean_relative_abundance_rarefied_BL_proportional <- NA
#   apply(immunocompetent_genus_summary.df, 1, function(x) as.numeric(x["Mean_relative_abundance_rarefied"]) * immunocompetent_sf_mean_bacterial_loads.df[x["Sampletype_final_refined"],"Mean_bacterial_load"])

immunocompromised_forearm_genus_summary.df$Mean_relative_abundance_rarefied_BL_proportional <-
  apply(immunocompromised_forearm_genus_summary.df, 1, function(x) as.numeric(x["Normalised_mean_relative_abundance"]) * immunocompromised_forearm_sf_mean_bacterial_loads.df[x["Sampletype_final_refined"],"Mean_bacterial_load"])

# Get unique list of taxa, ensure "Other" is first
# most_abundant_taxa <- sort(unique(immunocompromised_genus_summary.df$taxonomy_label))
# most_abundant_taxa <- c("Other", most_abundant_taxa[!grepl("Other", most_abundant_taxa)])


# Order taxonomy by the abundance. This is only approximate.
# The Other group should be last as it is the largest. This is not enforced however, so just be aware of it
immunocompromised_genus_summary.df <- immunocompromised_genus_summary.df %>% group_by(Sampletype_final_refined) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(unique(immunocompromised_genus_summary.df$taxonomy_label)[unique(immunocompromised_genus_summary.df$taxonomy_label) != "Other"], "Other")
immunocompromised_genus_summary.df$taxonomy_label <- factor(immunocompromised_genus_summary.df$taxonomy_label, levels = my_levels)
immunocompromised_genus_summary.df$value_label <- lapply(immunocompromised_genus_summary.df$Normalised_mean_relative_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), ""))

immunocompetent_genus_summary.df <- immunocompetent_genus_summary.df %>% group_by(Sampletype_final_refined) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(unique(immunocompetent_genus_summary.df$taxonomy_label)[unique(immunocompetent_genus_summary.df$taxonomy_label) != "Other"], "Other")
immunocompetent_genus_summary.df$taxonomy_label <- factor(immunocompetent_genus_summary.df$taxonomy_label, levels = my_levels)
immunocompetent_genus_summary.df$value_label <- lapply(immunocompetent_genus_summary.df$Normalised_mean_relative_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), ""))

immunocompromised_forearm_genus_summary.df <- immunocompromised_forearm_genus_summary.df %>% group_by(Sampletype_final_refined) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(unique(immunocompromised_forearm_genus_summary.df$taxonomy_label)[unique(immunocompromised_forearm_genus_summary.df$taxonomy_label) != "Other"], "Other")
immunocompromised_forearm_genus_summary.df$taxonomy_label <- factor(immunocompromised_forearm_genus_summary.df$taxonomy_label, levels = my_levels)
immunocompromised_forearm_genus_summary.df$value_label <- lapply(immunocompromised_forearm_genus_summary.df$Normalised_mean_relative_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), ""))


# Combine the cohorts summaries
immunocompromised_genus_summary.df$Project <- "immunocompromised"
immunocompetent_genus_summary.df$Project <- "immunocompetent"
immunocompromised_forearm_genus_summary.df$Project <- "immunocompromised"

both_cohorts_taxa_summary.df <- rbind(immunocompromised_genus_summary.df,immunocompetent_genus_summary.df)
both_cohorts_taxa_summary.df <- both_cohorts_taxa_summary.df %>% group_by(Sampletype_final_refined) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(as.character(unique(both_cohorts_taxa_summary.df$taxonomy_label))[as.character(unique(both_cohorts_taxa_summary.df$taxonomy_label)) != "Other"], "Other")
both_cohorts_taxa_summary.df$taxonomy_label <- factor(both_cohorts_taxa_summary.df$taxonomy_label, levels = my_levels)



# ------------------------------------------------------------------------------------
# Now plot

# Immunocompromised
immunocompromised_just_legend_plot <- ggplot(immunocompromised_genus_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
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

# bars <- map(unique(immunocompromised_genus_summary.df$Sampletype_final_refined), ~geom_bar(stat = "identity", position = "stack", data = immunocompromised_genus_summary.df %>% filter(Sampletype_final_refined == .x)))

immunocompromised_abundance_plot <- ggplot(immunocompromised_genus_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  # geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  ylab("Mean relative abundance (%)") +
  common_theme

immunocompromised_BL_abundance_plot <- ggplot(immunocompromised_genus_summary.df, aes(x = Sampletype_final_refined, y = Mean_relative_abundance_rarefied_BL_proportional, fill = taxonomy_label)) +
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
my_legend_taxa <- cowplot::get_legend(immunocompromised_just_legend_plot + 
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
grid_plot <- plot_grid(plotlist = list(immunocompromised_abundance_plot, NULL, immunocompromised_BL_abundance_plot),ncol = 3,nrow=1, rel_widths = c(1,-.12,1),align = "hv")
grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.4), ncol = 1, nrow=2)
ggsave(filename = "Result_figures/abundance_analysis_plots/immunocompromised_sampletype_relative_abundaunce_and_bacterial_load.pdf", plot = grid_plot, width = 30, height = 8, units = "cm")

# ----------------------------------------
# Immunocompentent
immunocompentent_just_legend_plot <- ggplot(immunocompetent_genus_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 5)) +
  xlab("Sample site") +
  ylab("Mean relative abundance") +
  common_theme 

immunocompentent_abundance_plot <- ggplot(immunocompetent_genus_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  # geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  ylab("Mean relative abundance (%)") +
  common_theme

immunocompentent_BL_abundance_plot <- ggplot(immunocompetent_genus_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
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
ggsave(filename = "Result_figures/abundance_analysis_plots/immunocompetent_sampletype_relative_abundaunce_and_bacterial_load.pdf", plot = grid_plot, width = 30, height = 7, units = "cm")


# Combined plot
# Extract the legend
combined_just_legend_plot <- ggplot(both_cohorts_taxa_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 5, )) +
  xlab("Sample site") +
  ylab("Mean relative abundance") +
  common_theme 

my_legend_taxa <- cowplot::get_legend(combined_just_legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 5),
                                          legend.title = element_text(size=6, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          # plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)
compromised_title <- ggdraw() + 
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


grid_plot <- plot_grid(plotlist = list(immunocompromised_abundance_plot + ylab(""),NULL,immunocompromised_BL_abundance_plot+ylab("")), ncol = 3, nrow =1, rel_widths = c(1,-.05,1),align = "hv")
grid_plot <- plot_grid(compromised_title, grid_plot,ncol = 1,rel_heights = c(0.1, 1))
grid_plot2 <- plot_grid(plotlist = list(immunocompentent_abundance_plot,NULL,immunocompentent_BL_abundance_plot), ncol = 3, nrow =1, rel_widths = c(1,-.05,1),align = "hv")
grid_plot2 <- plot_grid(competent_title, grid_plot2,ncol = 1,rel_heights = c(0.1, 1))

grid_plot <- plot_grid(plotlist = list(grid_plot, NULL, grid_plot2, my_legend_taxa),rel_heights = c(1, -.15, 1,.4), ncol = 1, nrow =4)
grid_plot
ggsave(filename = "Result_figures/abundance_analysis_plots/both_cohorts_sampletype_relative_abundaunce_and_bacterial_load.pdf", plot = grid_plot, width = 30, height = 12, units = "cm")

# 
# 
# grid_plot <- plot_grid(plotlist = list(immunocompromised_abundance_plot + ylab(""),NULL,immunocompromised_BL_abundance_plot+ylab(""),
#                           NULL, NULL,NULL,
#                           immunocompentent_abundance_plot,NULL, immunocompentent_BL_abundance_plot), ncol = 3, nrow=3, rel_widths = c(1,-0.1,1), rel_heights = c(1,-0.15,0.9), align = "hv")
# 
# grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.3), ncol = 1, nrow=2)
# # grid_plot
# ggsave(filename = "Result_figures/abundance_analysis_plots/combined_sampletype_relative_abundaunce_and_bacterial_load.pdf", plot = grid_plot, width = 30, height = 12, units = "cm")

# ---------------------------------------------------------------------------------
# Now create figure for just forearm samples

immunocompromised_forearm_just_legend_plot <- ggplot(immunocompromised_forearm_genus_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 2)) +
  xlab("Sample site") +
  ylab("Mean relative abundance") +
  common_theme 

immunocompromised_forearm_abundance_plot <- ggplot(immunocompromised_forearm_genus_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  # geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  ylab("Mean relative abundance (%)") +
  common_theme

immunocompromised_forearm_BL_abundance_plot <- ggplot(immunocompromised_forearm_genus_summary.df, aes(x = Sampletype_final_refined, y = Mean_relative_abundance_rarefied_BL_proportional, fill = taxonomy_label)) +
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
my_legend_taxa <- cowplot::get_legend(immunocompromised_forearm_just_legend_plot + 
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
grid_plot <- plot_grid(plotlist = list(immunocompromised_forearm_abundance_plot, NULL, immunocompromised_forearm_BL_abundance_plot),
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
ggsave(filename = "Result_figures/abundance_analysis_plots/immunocompromised_forearm_sampletype_relative_abundaunce_and_bacterial_load.pdf", plot = grid_plot, width = 30, height = 4, units = "cm")


