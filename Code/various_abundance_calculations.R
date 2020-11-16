# Calculate the abundances for different groupings
# Some basic plotting of abundance data
# Significance tests comparing abundances

# TODO - add function for multiple group signficance tests (Kruskal + dunn)

library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)
# install.packages("VennDiagram")
# library(VennDiagram)

common_theme <- theme(
  panel.border = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black", size = 0.5),
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
  legend.key.height=unit(.4,"cm"),
  legend.text = element_text(size = 8),
  axis.text = element_text(size = 9, colour = "black"),
  axis.title = element_text(size = 10,face = "bold"),
  complete = F,
  plot.title = element_text(size = 8))

# ------------------------------------------------------------------------------------------
# Various colour palettes
my_colour_palette <- c("#8dd3c7","#ffffb3","#bebada","#fb8072", "#80b1d3", "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5", "#cc0000")
# From http://tools.medialab.sciences-po.fr/iwanthue/
my_colour_palette_20 <- c("#66bd79","#a35bcf","#5bb643","#d14ea6","#a2b239","#5c6bcc","#dc892e","#5e93cd","#d64737","#49b6a8","#dc3c6e","#4f7e3c","#bd8cd5","#caab55","#914c88","#867230","#df82a2","#a65429","#ab4a5a","#e0896a")
my_colour_palette_20_distinct <- c("#0057b4","#7fff56","#d600bc","#d8d500","#e76eff","#019932","#9f8fff","#ffc730","#007fac","#a20019","#06fefd","#ff6782","#00774c","#e0c8ff","#717a00","#4b2952","#e2ed7d","#46321e","#ffbd76","#ffb4c6")
my_colour_palette_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_palette_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
my_colour_palette_15 <- c("#77b642","#7166d9","#cfa240","#b351bb","#4fac7f","#d44891","#79843a","#c68ad4","#d15a2c","#5ba7d9","#ce4355","#6570ba","#b67249","#9b4a6f","#df8398")
my_colour_palette_32_distinct <- c("#ea7e00","#ca0074","#d1c69b","#474007","#bb00ad","#9c80ff","#be3300","#542e72","#00b9f5","#09436b","#8b0036","#9ac8e6","#ff1059","#959eff","#154a11","#0290f4","#ff7762","#7dbf00","#ff8194","#834c00","#006e73","#f9bb5d","#d6c943","#017229","#00d3a8","#732427","#36e191","#6a8200","#efb3ea","#3227bb","#ff90e1","#e92a12")
# lesion_palette_7 <- c("#8558d6","#6ee268","#d247ad","#c9d743","#d7453e","#59a237","#d78f2a")
# patient_palette_45 <- c("#d64530","#585fb1","#795d97","#9e4773","#3f6921","#71692c","#a2b93c","#d571cc","#9b3e97","#33947a","#98ad66","#448a4e","#869ae0","#5ce7af","#e085a3","#dfdc87","#d19be2","#5cb735","#e38269","#3db6c0","#50b565","#50902c","#a98a2c","#dde84a","#db3d76","#5fe485","#7c8329","#b3e791","#6fe965","#5ebce9","#3c86c1","#2a6a45","#65b688","#6651d1","#af4ed3","#df872f","#56e4db","#737cea","#ac464b","#dd37b5","#995b2b","#daac6f","#92e2be","#a2e24b","#e0be3a")
my_colour_palette_10_distinct <- c("#8eec45","#0265e8","#f6a800","#bf6549","#486900","#c655a0","#00d1b6","#ff4431","#aeb85c","#7e7fc8")
my_colour_palette_10_soft <- c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621")

my_colour_palette_12_soft <-c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621", "#c75a93", "#7c7731")

# ------------------------------------------------------------------------------------------
species_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  paste(phylostring[3], phylostring[5], phylostring[7], sep = ";")
                }))
}

genus_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  paste(phylostring[3], phylostring[5], phylostring[6], sep = ";")
                }))
}

# ------------------------------------------------------------------------------------------
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
source("Code/helper_functions.R")

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)
# temp <- read.table("Result_tables/relative_abundance_tables/Genus_relative_abundances.csv", sep =",", header = T)
# temp <- melt(temp, variable.name = "Index", value.name = "Relative_abundance")
# temp <- left_join(temp, metadata.df, by = "Index")
# temp$Genus <- unlist(lapply(as.character(temp$taxonomy_genus), function(x) unlist(strsplit(x,";"))[6]))
# temp2 <- subset(temp, Genus %in% c("g__Staphylococcus", "g__Paracoccus", "g__Cutibacterium", "g__Malassezia", "Micrococcus"))
# length(temp2[temp2$Relative_abundance == 0,]$Index)
# summary(temp2[temp2$Relative_abundance == 0,]$Sample_type)
# temp2 <- subset(temp, Genus %in% c("g__Paracoccus"))
# length(temp2[temp2$Relative_abundance == 0,]$Index)
# summary(temp2[temp2$Relative_abundance == 0,]$Sample_type)
# temp2 <- subset(temp, Genus %in% c("g__Cutibacterium"))
# length(temp2[temp2$Relative_abundance == 0,]$Index)
# summary(temp2[temp2$Relative_abundance == 0,]$Sample_type)
# temp2 <- subset(temp, Genus %in% c("g__Malassezia"))
# length(temp2[temp2$Relative_abundance == 0,]$Index)
# summary(temp2[temp2$Relative_abundance == 0,]$Sample_type)
# temp2 <- subset(temp, Genus %in% c("Micrococcus"))
# length(temp2[temp2$Relative_abundance == 0,]$Index)
# summary(temp2[temp2$Relative_abundance == 0,]$Sample_type)



# temp <- read.table(pipe("pbpaste"), sep = "\t", header = T)



# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# Factorise discrete columns
metadata.df$Patient <- factor(metadata.df$Patient)
metadata.df$Sample_type <- factor(metadata.df$Sample_type, levels = c("NS", "PDS", "AK", "SCC_PL", "SCC"))
metadata.df$Cohort <- factor(metadata.df$Cohort)
metadata.df$Length_of_immunosuppression_group_1 <- factor(metadata.df$Length_of_immunosuppression_group_1)
metadata.df$Length_of_immunosuppression_group_2 <- factor(metadata.df$Length_of_immunosuppression_group_2)

# Load filtered/processed abundance data with metadata
# otu_data.df <- read.csv("Result_tables/other/OTU_counts_abundances_and_metadata.csv")
species_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Specie_counts_abundances_and_metadata.csv")
genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Genus_counts_abundances_and_metadata.csv")
family_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Family_counts_abundances_and_metadata.csv")
# order_data.df <- read.csv("Result_tables/other/order_counts_abundances_and_metadata.csv")
# class_data.df <- read.csv("Result_tables/other/class_counts_abundances_and_metadata.csv")
# phylum_data.df <- read.csv("Result_tables/other/phylum_counts_abundances_and_metadata.csv")

# Load abundances
genus_rel.m <- as.matrix(read.csv("Result_tables/relative_abundance_tables/Genus_relative_abundances.csv",row.names = 1))

# First, filter out non-snapshot samples from immunocompetent (this may have been done in main processing script)
species_data.df <- subset(species_data.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")
genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")
family_data.df <- subset(family_data.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")

# Set levels
# species_data.df$Sample_type <- factor(species_data.df$Sample_type, levels = c("C", "C_P", "AK","SCC_PL","SCC"))
# genus_data.df$Sample_type <- factor(genus_data.df$Sample_type, levels = c("C", "C_P", "AK","SCC_PL","SCC"))
# family_data.df$Sample_type <- factor(family_data.df$Sample_type, levels = c("C", "C_P", "AK","SCC_PL","SCC"))

species_data.df$Sample_type <- factor(species_data.df$Sample_type, levels = c("NS", "PDS", "AK", "SCC_PL", "SCC"))
genus_data.df$Sample_type <- factor(genus_data.df$Sample_type, levels = c("NS", "PDS", "AK", "SCC_PL", "SCC"))
family_data.df$Sample_type <- factor(family_data.df$Sample_type, levels = c("NS", "PDS", "AK", "SCC_PL", "SCC"))

# Create cohort specific data sets
immunosuppressed_metadata.df <- metadata.df[metadata.df$Cohort == "immunosuppressed",]
immunocompetent_metadata.df <- metadata.df[metadata.df$Cohort == "immunocompetent",]

immunosuppressed_genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed")
immunosuppressed_species_data.df <- subset(species_data.df, Cohort == "immunosuppressed")
immunocompetent_genus_data.df <- subset(genus_data.df, Cohort == "immunocompetent")
# --------------------------------------------------------------------------------------------
# FUNCTIONS

# Calculate the significance values for taxa between groups
calculate_taxa_significances <- function(mydata, variable_column, value_column, taxonomy_column){
  internal_data.df <- mydata
  results.df <- data.frame("Taxa" = character(),
                           "Variable" = character(),
                           "Group_1" = character(),
                           "Group_2" = character(),
                           "MannW_pvalue" = character()
                           # "KrusW_pvalue" = character()
                           )
  group_combinations <- combn(as.character(unique(internal_data.df[,variable_column])), 2)
  for (taxa in unique(internal_data.df[,taxonomy_column])){ # For each taxa in the taxonomy column
    for (i in 1:ncol(group_combinations)) { # For each group combination
      group_1 <- group_combinations[1,i]
      group_2 <- group_combinations[2,i]
      group_1_meta <- subset(internal_data.df, get(variable_column) == group_1 & get(taxonomy_column) == taxa)
      group_2_meta <- subset(internal_data.df, get(variable_column) == group_2 & get(taxonomy_column) == taxa)
      if (any(c(nrow(group_1_meta) < 2, nrow(group_2_meta) < 2))){
        next
      }
      # Mann-Whitney test
      wilcox_test <- wilcox.test(group_1_meta[,value_column], group_2_meta[,value_column], exact = F)
    
      # Kruskal-Wallis
      # kruskal_test <- kruskal.test(get(metric)~get(variable_column), data = subset(mydata, get(variable_column) %in% c(group_1, group_2)))

      results.df <- rbind(results.df, data.frame("Taxonomy" = taxa,
                                                 "Variable" = variable_column,
                                                 "Group_1" = group_1, 
                                                 "Group_2" = group_2, 
                                                 "MannW_pvalue" = round(wilcox_test$p.value,6)
                                                 # "KrusW_pvalue" = round(kruskal_test$p.value,6)
      ))
    }
  }
  
  results.df$MannW_padj <- round(p.adjust(results.df$MannW_pvalue,method = "BH"),6)
  # results.df$KrusW_padj <- round(p.adjust(results.df$KrusW_pvalue,method = "BH"),6)
  results.df
}

# Calculate the significance values for taxa between multiple groups
calculate_taxa_significances_multiple <- function(mydata, variable_column, value_column, taxonomy_column){
  # FIXME only using non-zero values!!
  results.df <- data.frame("Taxonomy" = character(),
                           "Variable" = character(),
                           "Group_1" = character(),
                           "Group_2" = character(),
                           "Dunn_pvalue" = character(),
                           "Dunn_padj" = character(),
                           "KrusW_pvalue" = character()
  )
  for (taxa in unique(mydata[,taxonomy_column])){ # For each taxa in the taxonomy column
    taxa_data <- subset(mydata, get(taxonomy_column) == taxa)
    n_groups = length(as.character(unique(taxa_data[,variable_column])))
    if (any(is.na(taxa_data[,variable_column]))){
      return()
    }
    if (n_groups > 2){
      kw <- kruskal.test(get(value_column)~get(variable_column), data = taxa_data)
      dunn <- dunnTest(x = get(value_column)~get(variable_column), data = taxa_data, method = "bh", alpha = 0.05)
      dunn <- separate(dunn$res, Comparison, into = c("Group_1", "Group_2"), sep = " - ")[,c("Group_1","Group_2","P.unadj","P.adj")]
      names(dunn) <- c("Group_1","Group_2","Dunn_pvalue","Dunn_padj")
      dunn$Taxonomy <- taxa
      dunn$KrusW_pvalue <- kw$p.value
      dunn$Variable <- variable_column
      results.df <- rbind(results.df, dunn)
    }
  }
  results.df[,c("Taxonomy", "Variable", "Group_1","Group_2", "Dunn_pvalue", "Dunn_padj", "KrusW_pvalue")]
}

# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# Cohort

# Summarise the data
# ********************************
# TESTING
# head(genus_data.df)
# genus_data.df %>% group_by(Cohort) %>% tally(n_distinct(Sample))
# temp <- read.csv("Result_tables/relative_abundance_tables/Genus_relative_abundances.csv", header = T)
# IC_samples <- as.character(subset(metadata.df, Cohort  == "immunocompetent")$Index)
# temp <- df2matrix(temp[,c("taxonomy_genus", IC_samples)])
# apply(temp[grepl("g__Cutibac", rownames(temp)),,drop =F],1,mean)
# 
# cohort_genus_summary.df <- genus_data.df %>% 
#   select(Cohort, Sample, taxonomy_genus, Relative_abundance) %>% 
#   group_by(Cohort) %>% 
#   dplyr::mutate(Total_N_Samples = n_distinct(Sample)) %>% # number of unique samples/index
#   group_by(Cohort, taxonomy_genus) %>% 
#   dplyr::summarise(Total_N_Samples = max(Total_N_Samples), 
#                    Mean_relative_abundance = sum(Relative_abundance)/max(Total_N_Samples),
#                    Mean_relative_abundance2 = mean(Relative_abundance),
#                    Sum_relative_abundance = sum(Relative_abundance)) %>% 
#   as.data.frame()
# ********************************
# Just values for samples
sample_genus_summary.df <- generate_taxa_summary(genus_data.df,
                                                 taxa_column = "taxonomy_genus", 
                                                 group_by_columns = c("Sample", "Cohort", "Sample_type", "Patient"))

# Get top taxa per group
sample_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = sample_genus_summary.df, 
                                                       grouping_variables = c("Sample"), 
                                                       abundance_column = "Mean_relative_abundance", 
                                                       my_top_n = 20)
sample_genus_summary.df <- sample_genus_summary.df[,c("Sample", "Cohort", "Sample_type", "Patient", "taxonomy_genus", "Mean_relative_abundance")]
sample_genus_summary_top.df <- sample_genus_summary_top.df[,c("Sample", "Cohort", "Sample_type", "Patient", "taxonomy_genus", "Mean_relative_abundance")]

# genus_data.df[,c("Sample", "Cohort", "Sample_type", "Patient", "taxonomy_genus", "Relative_abundance")]
write.csv(genus_data.df[,c("Sample", "Cohort", "Sample_type", "Patient", "taxonomy_genus", "Relative_abundance")], 
          "Result_tables/abundance_analysis_tables/sample_genus_abundances.csv", row.names = F, quote = F)

write.csv(sample_genus_summary_top.df, 
          "Result_tables/abundance_analysis_tables/sample_top_20_genus.csv", row.names = F, quote = F)


# ********************************
cohort_genus_summary.df <- generate_taxa_summary(genus_data.df,
                                                  taxa_column = "taxonomy_genus", 
                                                  group_by_columns = c("Cohort"))

# Get top taxa per group
cohort_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = cohort_genus_summary.df, 
                                                        grouping_variables = c("Cohort"), 
                                                        abundance_column = "Mean_relative_abundance", 
                                                        my_top_n = 20)

# Filter to taxa with a mean abundance above the threshold
cohort_genus_summary_filtered.df <- subset(cohort_genus_summary.df, Mean_relative_abundance > 0.005)
genus_data_filtered_top_abundant_cohort.df <- subset(genus_data.df, taxonomy_genus %in% cohort_genus_summary_top.df$taxonomy_genus)
genus_data_filtered_cohort.df <- subset(genus_data.df, taxonomy_genus %in% cohort_genus_summary_filtered.df$taxonomy_genus)

# Calculate significances for the top taxa.
# Note, this is ONLY based on the top taxa. P-values will likely differ to those calculated on a less filtered subset of the data (or all). 
# You will see examples in the literature where the statistics are only calculated on the top. This is fine, just be aware of it.
cohort_genus_summary_top_significances.df <- calculate_taxa_significances(mydata = genus_data_filtered_top_abundant_cohort.df,
                                                                          variable_column = "Cohort",
                                                                          value_column = "Relative_abundance",
                                                                          taxonomy_column = "taxonomy_genus")

# Calculate significances for the taxa with abundances above threshold
cohort_genus_summary_all_significances.df <- calculate_taxa_significances(mydata = genus_data_filtered_cohort.df,
                                                                          variable_column = "Cohort",
                                                                          value_column = "Relative_abundance",
                                                                          taxonomy_column = "taxonomy_genus")
# cohort_genus_summary_top_significances.df[grepl("g__Staphylococcus", cohort_genus_summary_top_significances.df$Taxonomy),]
# cohort_genus_summary_all_significances.df[grepl("g__Staphylococcus", cohort_genus_summary_all_significances.df$Taxonomy),]

cohort_genus_summary_top_significances_filtered.df <- cohort_genus_summary_top_significances.df[which(cohort_genus_summary_top_significances.df$MannW_pvalue <= 0.05 | cohort_genus_summary_top_significances.df$MannW_padj <= 0.05),]
cohort_genus_summary_all_significances_filtered.df <- cohort_genus_summary_all_significances.df[which(cohort_genus_summary_all_significances.df$MannW_pvalue <= 0.05 | cohort_genus_summary_all_significances.df$MannW_padj <= 0.05),]
write.csv(cohort_genus_summary_all_significances_filtered.df,"Result_tables/abundance_analysis_tables/cohort__genus_dunn.csv", row.names = F)

source("Code/helper_functions.R")
# Make individual boxplots for the top taxa
# for (taxa in as.character(unique(cohort_genus_summary_top.df$taxonomy_genus))){
#   # ...but limit to those taxa that are also significant
#   significances_subset <- subset(cohort_genus_summary_top_significances_filtered.df, Taxonomy == taxa)
#   # significances_subset <- subset(cohort_genus_summary_all_significances_filtered.df, Taxonomy == taxa)
#   if (dim(significances_subset)[1] == 0){
#     print(paste0(taxa, " not significant"))
#     next()
#   }
#   taxa_title <- genus_relabeller_function(taxa)
#   data_subset <- subset(genus_data.df, taxonomy_genus == taxa)
#   myplot <- generate_significance_boxplots(mydata.df = data_subset,
#                                  variable_column = "Cohort",
#                                  value_column = "Relative_abundance",
#                                  variable_colours_available = T,
#                                  significances.df = significances_subset,
#                                  p_value_column = "MannW_padj",
#                                  sig_threshold = 0.05) +
#     ggtitle(taxa_title) +
#     ylab("Relative abundance") + scale_y_continuous( breaks = seq(0,1,.1))
#   
#   print(taxa_title)
#   # print(gsub(";", ";", taxa_title))
#   ggsave(filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Cohort/genus/",taxa_title,".pdf"),
#          device = "pdf",
#          plot = myplot,
#          width = 5,
#          height = 5)
# }

# ---------------------------------------------------------------------------------------------
# Sample type

sample_type_genus_summary.df <- generate_taxa_summary(genus_data.df,
                                                      taxa_column = "taxonomy_genus", 
                                                      group_by_columns = c("Sample_type"))

sample_type_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = sample_type_genus_summary.df, 
                                                            grouping_variables = c("Sample_type"), 
                                                            abundance_column = "Mean_relative_abundance", 
                                                            my_top_n = 20)

sample_type_genus_summary_filtered.df <- subset(sample_type_genus_summary.df, Mean_relative_abundance > 0.005)
genus_data_filtered_top_abundant_sample_type.df <- subset(genus_data.df, taxonomy_genus %in% sample_type_genus_summary_top.df$taxonomy_genus)
genus_data_filtered_sample_type.df <- subset(genus_data.df, taxonomy_genus %in% sample_type_genus_summary_filtered.df$taxonomy_genus)
# 
# sample_type_genus_summary_top_significances.df <- calculate_taxa_significances(mydata = genus_data_filtered_top_abundant_sample_type.df,
#                                                                                variable_column = "Sample_type",
#                                                                                value_column = "Relative_abundance",
#                                                                                taxonomy_column = "taxonomy_genus")

sample_type_genus_summary_top_significances.df <- calculate_taxa_significances_multiple(mydata = genus_data_filtered_top_abundant_sample_type.df,
                                                                                   variable_column = "Sample_type",
                                                                                   value_column = "Relative_abundance",
                                                                                   taxonomy_column = "taxonomy_genus")

# sample_type_genus_summary_all_significances.df <- calculate_taxa_significances(mydata = genus_data_filtered_sample_type.df,
#                                                                                variable_column = "Sample_type",
#                                                                                value_column = "Relative_abundance",
#                                                                                taxonomy_column = "taxonomy_genus")
sample_type_genus_summary_all_significances.df <- calculate_taxa_significances_multiple(mydata = genus_data_filtered_sample_type.df,
                                                                                        variable_column = "Sample_type",
                                                                                        value_column = "Relative_abundance",
                                                                                        taxonomy_column = "taxonomy_genus")

# sample_type_genus_summary_top_significances_filtered.df <- sample_type_genus_summary_top_significances.df[which(sample_type_genus_summary_top_significances.df$MannW_pvalue <= 0.05 | sample_type_genus_summary_top_significances.df$MannW_padj <= 0.05),]
# sample_type_genus_summary_all_significances_filtered.df <- sample_type_genus_summary_all_significances.df[which(sample_type_genus_summary_all_significances.df$MannW_pvalue <= 0.05 | sample_type_genus_summary_all_significances.df$MannW_padj <= 0.05),]
# write.csv(sample_type_genus_summary_all_significances_filtered.df,"Result_tables/abundance_analysis_tables/sample_type__genus_wilcox.csv", row.names = F)
sample_type_genus_summary_top_significances_filtered.df <- sample_type_genus_summary_top_significances.df[which((sample_type_genus_summary_top_significances.df$Dunn_pvalue <= 0.05 | 
                                                       sample_type_genus_summary_top_significances.df$Dunn_padj <= 0.05) &
                                                       sample_type_genus_summary_top_significances.df$KrusW_pvalue <= 0.05),]

sample_type_genus_summary_all_significances_filtered.df <- sample_type_genus_summary_all_significances.df[which((sample_type_genus_summary_all_significances.df$Dunn_pvalue <= 0.05 | 
                                                                                                                   sample_type_genus_summary_all_significances.df$Dunn_padj <= 0.05) &
                                                                                                                  sample_type_genus_summary_all_significances.df$KrusW_pvalue <= 0.05),]


write.csv(sample_type_genus_summary_top_significances_filtered.df,"Result_tables/abundance_analysis_tables/sample_type__top_genus_dunn.csv", row.names = F)
write.csv(sample_type_genus_summary_all_significances_filtered.df,"Result_tables/abundance_analysis_tables/sample_type__genus_dunn.csv", row.names = F)


# for (taxa in as.character(unique(sample_type_genus_summary_top.df$taxonomy_genus))){
#   significances_subset <- subset(sample_type_genus_summary_top_significances_filtered.df, Taxonomy == taxa)
#   # significances_subset <- subset(sample_type_genus_summary_all_significances_filtered.df, Taxonomy == taxa)
#   if (dim(significances_subset)[1] == 0){
#     print(paste0(taxa, " not significant"))
#     next()
#   }
#   taxa_title <- genus_relabeller_function(taxa)
#   data_subset <- subset(genus_data.df, taxonomy_genus == taxa)
#   myplot <- generate_significance_boxplots(mydata.df = data_subset,
#                                            variable_column = "Sample_type",
#                                            value_column = "Relative_abundance",
#                                            variable_colours_available = T,
#                                            significances.df = significances_subset,
#                                            p_value_column = "Dunn_padj",
#                                            sig_threshold = 0.05) +
#     ggtitle(taxa_title) +
#     ylab("Relative abundance") + scale_y_continuous( breaks = seq(0,1,.1))
#   
#   print(taxa_title)
#   
#   ggsave(filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Sample_type/genus/",taxa_title,".pdf"),
#          device = "pdf",
#          plot = myplot,
#          width = 5,
#          height = 5)
# }

# ---------------------------------------------------------------------------------------------
# Sample type, immunosuppressed
immunosuppressed_sample_type_species_summary.df <- generate_taxa_summary(immunosuppressed_species_data.df,
                                                        taxa_column = "taxonomy_species", 
                                                        group_by_columns = c("Sample_type"))

immunosuppressed_sample_type_species_summary_top.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_sample_type_species_summary.df, 
                                                                             grouping_variables = c("Sample_type"), 
                                                                             abundance_column = "Mean_relative_abundance", 
                                                                             my_top_n = 10)
# Create summary of taxa abundances within each group
immunosuppressed_sample_type_genus_summary.df <- generate_taxa_summary(immunosuppressed_genus_data.df,
                                                      taxa_column = "taxonomy_genus", 
                                                      group_by_columns = c("Sample_type"))

# Filter summary to top taxa by mean relative abundance
immunosuppressed_sample_type_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_sample_type_genus_summary.df, 
                                                            grouping_variables = c("Sample_type"), 
                                                            abundance_column = "Mean_relative_abundance", 
                                                            my_top_n = 20)

# Filter main sample data to top taxa
immunosuppressed_species_data_filtered_top_abundant_sample_type.df <- subset(immunosuppressed_species_data.df, taxonomy_species %in% immunosuppressed_sample_type_species_summary_top.df$taxonomy_species)
immunosuppressed_genus_data_filtered_top_abundant_sample_type.df <- subset(immunosuppressed_genus_data.df, taxonomy_genus %in% immunosuppressed_sample_type_genus_summary_top.df$taxonomy_genus)

# Also create summary data limited to taxa with greater than 0.05% abundance
immunosuppressed_sample_type_genus_summary_filtered.df <- subset(immunosuppressed_sample_type_genus_summary.df, Mean_relative_abundance > 0.005)
# And create a version of the main sample data to this subset of taxa
immunosuppressed_genus_data_filtered_sample_type.df <- subset(immunosuppressed_genus_data.df, taxonomy_genus %in% immunosuppressed_sample_type_genus_summary_filtered.df$taxonomy_genus)


# Calculate the significances of each taxa across all groups
immunosuppressed_sample_type_species_summary_top_significances.df <- calculate_taxa_significances_multiple(mydata = immunosuppressed_species_data_filtered_top_abundant_sample_type.df,
                                                                                                         variable_column = "Sample_type",
                                                                                                         value_column = "Relative_abundance",
                                                                                                         taxonomy_column = "taxonomy_species")


immunosuppressed_sample_type_genus_summary_top_significances.df <- calculate_taxa_significances_multiple(mydata = immunosuppressed_genus_data_filtered_top_abundant_sample_type.df,
                                                                                        variable_column = "Sample_type",
                                                                                        value_column = "Relative_abundance",
                                                                                        taxonomy_column = "taxonomy_genus")

# immunosuppressed_sample_type_genus_summary_all_significances.df <- calculate_taxa_significances(mydata = immunosuppressed_genus_data_filtered_sample_type.df,
#                                                                                variable_column = "Sample_type",
#                                                                                value_column = "Relative_abundance",
#                                                                                taxonomy_column = "taxonomy_genus")

immunosuppressed_sample_type_genus_summary_all_significances.df <- calculate_taxa_significances_multiple(mydata = immunosuppressed_genus_data_filtered_sample_type.df,
                                                                                                         variable_column = "Sample_type",
                                                                                                         value_column = "Relative_abundance",
                                                                                                         taxonomy_column = "taxonomy_genus")

# Filter the significance results to only those of interest
# immunosuppressed_sample_type_genus_summary_top_significances_filtered.df <- immunosuppressed_sample_type_genus_summary_top_significances.df[which(immunosuppressed_sample_type_genus_summary_top_significances.df$MannW_pvalue <= 0.05 | immunosuppressed_sample_type_genus_summary_top_significances.df$MannW_padj <= 0.05),]
# immunosuppressed_sample_type_genus_summary_all_significances_filtered.df <- immunosuppressed_sample_type_genus_summary_all_significances.df[which(immunosuppressed_sample_type_genus_summary_all_significances.df$MannW_pvalue <= 0.05 | immunosuppressed_sample_type_genus_summary_all_significances.df$MannW_padj <= 0.05),]
# write.csv(immunosuppressed_sample_type_genus_summary_all_significances_filtered.df,"Result_tables/abundance_analysis_tables/immunosuppressed_sample_type__genus_wilcox.csv", row.names = F)

immunosuppressed_sample_type_species_summary_top_significances_filtered.df <- immunosuppressed_sample_type_species_summary_top_significances.df[which(immunosuppressed_sample_type_species_summary_top_significances.df$Dunn_pvalue <= 0.05 | immunosuppressed_sample_type_species_summary_top_significances.df$Dunn_padj <= 0.05),]

immunosuppressed_sample_type_genus_summary_top_significances_filtered.df <- immunosuppressed_sample_type_genus_summary_top_significances.df[which((immunosuppressed_sample_type_genus_summary_top_significances.df$Dunn_pvalue <= 0.05 | 
                                                                                                                                                     immunosuppressed_sample_type_genus_summary_top_significances.df$Dunn_padj <= 0.05) &
                                                                                                                                                    immunosuppressed_sample_type_genus_summary_top_significances.df$KrusW_pvalue <= 0.05),]

immunosuppressed_sample_type_genus_summary_all_significances_filtered.df <- immunosuppressed_sample_type_genus_summary_all_significances.df[which((immunosuppressed_sample_type_genus_summary_all_significances.df$Dunn_pvalue <= 0.05 | 
                                                                                                                                                     immunosuppressed_sample_type_genus_summary_all_significances.df$Dunn_padj <= 0.05) &
                                                                                                                                                    immunosuppressed_sample_type_genus_summary_all_significances.df$KrusW_pvalue <= 0.05),]

write.csv(immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,"Result_tables/abundance_analysis_tables/immunosuppressed_sample_type__top_genus_dunn.csv", row.names = F)
write.csv(immunosuppressed_sample_type_genus_summary_all_significances_filtered.df,"Result_tables/abundance_analysis_tables/immunosuppressed_sample_type__genus_dunn.csv", row.names = F)

# for (taxa in as.character(unique(immunosuppressed_sample_type_genus_summary_top.df$taxonomy_genus))){
#   significances_subset <- subset(immunosuppressed_sample_type_genus_summary_top_significances_filtered.df, Taxonomy == taxa)
#   # significances_subset <- subset(immunosuppressed_sample_type_genus_summary_all_significances_filtered.df, Taxonomy == taxa)
#   if (dim(significances_subset)[1] == 0){
#     print(paste0(taxa, " not significant"))
#     next()
#   }
#   print(paste0("processing ", taxa))
#   taxa_title <- genus_relabeller_function(taxa)
#   data_subset <- subset(immunosuppressed_genus_data.df, taxonomy_genus == taxa)
#   myplot <- generate_significance_boxplots(mydata.df = data_subset,
#                                            variable_column = "Sample_type",
#                                            value_column = "Relative_abundance",
#                                            variable_colours_available = T,
#                                            significances.df = significances_subset,
#                                            p_value_column = "Dunn_padj",
#                                            # p_value_column = "MannW_pvalue",
#                                            sig_threshold = 0.05) +
#     labs(title = "Immunosuppressed",subtitle = taxa_title) + theme(plot.title = element_text(size = 8),plot.subtitle = element_text(size = 5,hjust = .5))+
#     ylab("Relative abundance") + scale_y_continuous( breaks = seq(0,1,.1), limits = c(0,1.4))
#   
#   ggsave(filename = paste0("Result_figures/abundance_analysis_plots/boxplots/immunosuppressed/Sample_type/genus/",taxa_title,".pdf"),
#          device = "pdf",
#          plot = myplot,
#          width = 5,
#          height = 5)
# }
# ---------------------------------------------------------------------------------------------
# Sample type, immunocompetent


immunocompetent_sample_type_genus_summary.df <- generate_taxa_summary(immunocompetent_genus_data.df,
                                                                      taxa_column = "taxonomy_genus", 
                                                                      group_by_columns = c("Sample_type"))

immunocompetent_sample_type_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = immunocompetent_sample_type_genus_summary.df, 
                                                                            grouping_variables = c("Sample_type"), 
                                                                            abundance_column = "Mean_relative_abundance", 
                                                                            my_top_n = 20)

immunocompetent_genus_data_filtered_top_abundant_sample_type.df <- subset(immunocompetent_genus_data.df, taxonomy_genus %in% immunocompetent_sample_type_genus_summary_top.df$taxonomy_genus)
immunocompetent_sample_type_genus_summary_filtered.df <- subset(immunocompetent_sample_type_genus_summary.df, Mean_relative_abundance > 0.005)
immunocompetent_genus_data_filtered_sample_type.df <- subset(immunocompetent_genus_data.df, taxonomy_genus %in% immunocompetent_sample_type_genus_summary_filtered.df$taxonomy_genus)

# immunocompetent_sample_type_genus_summary_top_significances.df <- calculate_taxa_significances(mydata = immunocompetent_genus_data_filtered_top_abundant_sample_type.df,
#                                                                                                variable_column = "Sample_type",
#                                                                                                value_column = "Relative_abundance",
#                                                                                                taxonomy_column = "taxonomy_genus")

immunocompetent_sample_type_genus_summary_top_significances.df <- calculate_taxa_significances_multiple(mydata = immunocompetent_genus_data_filtered_top_abundant_sample_type.df,
                                                                                                        variable_column = "Sample_type",
                                                                                                        value_column = "Relative_abundance",
                                                                                                        taxonomy_column = "taxonomy_genus")

# immunocompetent_sample_type_genus_summary_all_significances.df <- calculate_taxa_significances(mydata = immunocompetent_genus_data_filtered_sample_type.df,
#                                                                                                variable_column = "Sample_type",
#                                                                                                value_column = "Relative_abundance",
#                                                                                                taxonomy_column = "taxonomy_genus")

immunocompetent_sample_type_genus_summary_all_significances.df <- calculate_taxa_significances_multiple(mydata = immunocompetent_genus_data_filtered_sample_type.df,
                                                                                                         variable_column = "Sample_type",
                                                                                                         value_column = "Relative_abundance",
                                                                                                         taxonomy_column = "taxonomy_genus")

# immunocompetent_sample_type_genus_summary_top_significances_filtered.df <- immunocompetent_sample_type_genus_summary_top_significances.df[which(immunocompetent_sample_type_genus_summary_top_significances.df$MannW_pvalue <= 0.05 | immunocompetent_sample_type_genus_summary_top_significances.df$MannW_padj <= 0.05),]
# immunocompetent_sample_type_genus_summary_all_significances_filtered.df <- immunocompetent_sample_type_genus_summary_all_significances.df[which(immunocompetent_sample_type_genus_summary_all_significances.df$MannW_pvalue <= 0.05 | immunocompetent_sample_type_genus_summary_all_significances.df$MannW_padj <= 0.05),]
# write.csv(immunocompetent_sample_type_genus_summary_all_significances_filtered.df,"Result_tables/abundance_analysis_tables/immunocompetent_sample_type__genus_wilcox.csv", row.names = F)
immunocompetent_sample_type_genus_summary_top_significances_filtered.df <- immunocompetent_sample_type_genus_summary_top_significances.df[which((immunocompetent_sample_type_genus_summary_top_significances.df$Dunn_pvalue <= 0.05 | 
                                                                                                                                                   immunocompetent_sample_type_genus_summary_top_significances.df$Dunn_padj <= 0.05) &
                                                                                                                                                  immunocompetent_sample_type_genus_summary_top_significances.df$KrusW_pvalue <= 0.05),]

immunocompetent_sample_type_genus_summary_all_significances_filtered.df <- immunocompetent_sample_type_genus_summary_all_significances.df[which((immunocompetent_sample_type_genus_summary_all_significances.df$Dunn_pvalue <= 0.05 | 
                                                                                                                                                   immunocompetent_sample_type_genus_summary_all_significances.df$Dunn_padj <= 0.05) &
                                                                                                                                                  immunocompetent_sample_type_genus_summary_all_significances.df$KrusW_pvalue <= 0.05),]

write.csv(immunocompetent_sample_type_genus_summary_top_significances_filtered.df,"Result_tables/abundance_analysis_tables/immunocompetent_sample_type__top_genus_dunn.csv", row.names = F)
write.csv(immunocompetent_sample_type_genus_summary_all_significances_filtered.df,"Result_tables/abundance_analysis_tables/immunocompetent_sample_type__genus_dunn.csv", row.names = F)


# for (taxa in as.character(unique(immunocompetent_sample_type_genus_summary_top.df$taxonomy_genus))){
#   significances_subset <- subset(immunocompetent_sample_type_genus_summary_top_significances_filtered.df, Taxonomy == taxa)
#   # significances_subset <- subset(immunocompetent_sample_type_genus_summary_all_significances_filtered.df, Taxonomy == taxa)
#   if (dim(significances_subset)[1] == 0){
#     print(paste0(taxa, " not significant"))
#     next()
#   }
#   taxa_title <- genus_relabeller_function(taxa)
#   data_subset <- subset(immunocompetent_genus_data.df, taxonomy_genus == taxa)
#   myplot <- generate_significance_boxplots(mydata.df = data_subset,
#                                            variable_column = "Sample_type",
#                                            value_column = "Relative_abundance",
#                                            variable_colours_available = T,
#                                            significances.df = significances_subset,
#                                            p_value_column = "Dunn_padj",
#                                            sig_threshold = 0.05) +
#     labs(title = "Immunocompetent",subtitle = taxa_title) + theme(plot.title = element_text(size = 8),plot.subtitle = element_text(size = 5,hjust = .5))+
#     ylab("Relative abundance") + scale_y_continuous( breaks = seq(0,1,.1), limits = c(0,1.4))
#   
#   print(taxa_title)
#   
#   ggsave(filename = paste0("Result_figures/abundance_analysis_plots/boxplots/immunocompetent/Sample_type/genus/",taxa_title,".pdf"),
#          device = "pdf",
#          plot = myplot,
#          width = 5,
#          height = 5)
# }

# ---------------------------------------------------------------------------------------------
# Sample type, each patient

patient_sample_type_genus_summary.df <- generate_taxa_summary(genus_data.df,
                                                              taxa_column = "taxonomy_genus", 
                                                              group_by_columns = c("Patient","Sample_type"))

patient_sample_type_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = patient_sample_type_genus_summary.df, 
                                                                    grouping_variables = c("Sample_type"), 
                                                                    abundance_column = "Mean_relative_abundance", 
                                                                    my_top_n = 20)
write.csv(patient_sample_type_genus_summary_top.df[c("Patient", "Sample_type", "taxonomy_genus", "Mean_relative_abundance", "N_samples")], 
          "Result_tables/abundance_analysis_tables/patient_sample_type_top_20_genus.csv", row.names = F, quote = F)

patient_sample_type_genus_data_filtered_top_abundant.df <- subset(genus_data.df, taxonomy_genus %in% patient_sample_type_genus_summary_top.df$taxonomy_genus)
patient_sample_type_genus_summary_filtered.df <- subset(patient_sample_type_genus_summary.df, Mean_relative_abundance > 0.005)
patient_sample_type_genus_data_filtered.df <- subset(genus_data.df, taxonomy_genus %in% patient_sample_type_genus_summary_filtered.df$taxonomy_genus)

patient_sample_type_genus_summary_top_significances.df <- calculate_taxa_significances_multiple(mydata = patient_sample_type_genus_data_filtered_top_abundant.df,
                                                                                                variable_column = "Sample_type",
                                                                                                value_column = "Relative_abundance",
                                                                                                taxonomy_column = "taxonomy_genus")

patient_sample_type_genus_summary_all_significances.df <- calculate_taxa_significances_multiple(mydata = patient_sample_type_genus_data_filtered.df,
                                                                                                variable_column = "Sample_type",
                                                                                                value_column = "Relative_abundance",
                                                                                                taxonomy_column = "taxonomy_genus")

patient_sample_type_genus_summary_top_significances_filtered.df <- patient_sample_type_genus_summary_top_significances.df[which((patient_sample_type_genus_summary_top_significances.df$Dunn_pvalue <= 0.05 | 
                                                                                                                                   patient_sample_type_genus_summary_top_significances.df$Dunn_padj <= 0.05) &
                                                                                                                                  patient_sample_type_genus_summary_top_significances.df$KrusW_pvalue <= 0.05),]

patient_sample_type_genus_summary_all_significances_filtered.df <- patient_sample_type_genus_summary_all_significances.df[which((patient_sample_type_genus_summary_all_significances.df$Dunn_pvalue <= 0.05 | 
                                                                                                                                   patient_sample_type_genus_summary_all_significances.df$Dunn_padj <= 0.05) &
                                                                                                                                  patient_sample_type_genus_summary_all_significances.df$KrusW_pvalue <= 0.05),]

write.csv(patient_sample_type_genus_summary_top_significances_filtered.df,"Result_tables/abundance_analysis_tables/patient_sample_type__top_genus_dunn.csv", row.names = F)
write.csv(patient_sample_type_genus_summary_all_significances_filtered.df,"Result_tables/abundance_analysis_tables/patient_sample_type__genus_dunn.csv", row.names = F)

# ---------------------------------------------------------------------------------------------
# Between cohorts, same sample type

# First calculate top genera for each cohort
# Create summary of taxa abundances within each group
immunosuppressed_sample_type_genus_summary.df <- generate_taxa_summary(immunosuppressed_genus_data.df,
                                                                       taxa_column = "taxonomy_genus", 
                                                                       group_by_columns = c("Sample_type"))
immunosuppressed_sample_type_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_sample_type_genus_summary.df, 
                                                                             grouping_variables = c("Sample_type"), 
                                                                             abundance_column = "Mean_relative_abundance", 
                                                                             my_top_n = 20)

immunocompetent_sample_type_genus_summary.df <- generate_taxa_summary(immunocompetent_genus_data.df,
                                                                      taxa_column = "taxonomy_genus", 
                                                                      group_by_columns = c("Sample_type"))

immunocompetent_sample_type_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = immunocompetent_sample_type_genus_summary.df, 
                                                                            grouping_variables = c("Sample_type"), 
                                                                            abundance_column = "Mean_relative_abundance", 
                                                                            my_top_n = 20)
both_cohorts_top_genus <- base::union(immunosuppressed_sample_type_genus_summary_top.df$taxonomy_genus, immunocompetent_sample_type_genus_summary_top.df$taxonomy_genus)

both_cohorts_sample_type_genus_data_filtered_top_abundant.df <- subset(genus_data.df, taxonomy_genus %in% both_cohorts_top_genus)

both_cohorts_sample_type_genus_data_filtered_top_abundant.df$Cohort_Sample_type <- with(both_cohorts_sample_type_genus_data_filtered_top_abundant.df, 
                                                                                        paste0(Cohort, "_", Sample_type))

both_cohorts_sample_type_genus_summary_top_significances.df <- calculate_taxa_significances_multiple(mydata = both_cohorts_sample_type_genus_data_filtered_top_abundant.df,
                                                                                                variable_column = "Cohort_Sample_type",
                                                                                                value_column = "Relative_abundance",
                                                                                                taxonomy_column = "taxonomy_genus")

both_cohorts_sample_type_genus_summary_top_significances.df$Group_1_sample_type <- gsub("(immunocompetent|immunosuppressed)_", "",both_cohorts_sample_type_genus_summary_top_significances.df$Group_1)
both_cohorts_sample_type_genus_summary_top_significances.df$Group_2_sample_type <- gsub("(immunocompetent|immunosuppressed)_", "",both_cohorts_sample_type_genus_summary_top_significances.df$Group_2)
both_cohorts_sample_type_genus_summary_top_significances.df <- both_cohorts_sample_type_genus_summary_top_significances.df[with(both_cohorts_sample_type_genus_summary_top_significances.df, Group_1_sample_type == Group_2_sample_type),]
both_cohorts_sample_type_genus_summary_top_significances.df$Group_1_sample_type <- NULL
both_cohorts_sample_type_genus_summary_top_significances.df$Group_2_sample_type <- NULL

both_cohorts_sample_type_genus_summary_top_significances_filtered.df <- both_cohorts_sample_type_genus_summary_top_significances.df[which((both_cohorts_sample_type_genus_summary_top_significances.df$Dunn_pvalue <= 0.05 | 
                                                                                                                                             both_cohorts_sample_type_genus_summary_top_significances.df$Dunn_padj <= 0.05) &
                                                                                                                                            both_cohorts_sample_type_genus_summary_top_significances.df$KrusW_pvalue <= 0.05),]

write.csv(both_cohorts_sample_type_genus_summary_top_significances_filtered.df,"Result_tables/abundance_analysis_tables/both_cohorts_sample_type__top_genus_dunn.csv", row.names = F)


cohort_type_genus_summary.df <- generate_taxa_summary(genus_data.df,
                                                      taxa_column = "taxonomy_genus", 
                                                      group_by_columns = c("Cohort", "Sample_type"))
cohort_sample_type_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = cohort_type_genus_summary.df, 
                                                            grouping_variables = c("Cohort", "Sample_type"), 
                                                            abundance_column = "Mean_relative_abundance", 
                                                            my_top_n = 20)
# cohort_sample_type_genus_summary_top.df$taxonomy_genus %in% both_cohorts_top_genus

write.csv(cohort_sample_type_genus_summary_top.df[c("Cohort", "Sample_type", "taxonomy_genus", "Mean_relative_abundance", "N_samples")], 
          "Result_tables/abundance_analysis_tables/cohort_sample_type_top_20_genus.csv", row.names = F, quote = F)

# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# Publication figures
# significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
# significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
# taxonomy_level = "taxonomy_genus",
# sample_data.df = genus_data.df,

# genus_relabeller_function2 <- function(my_labels){
#   unlist(lapply(my_labels, 
#                 function(x) {
#                   phylostring <- unlist(strsplit(x, split = ";"))
#                   paste(phylostring[6], sep = ";")
#                 }))
# }
# 
# immunosuppressed_genus_summary.df <- generate_taxa_summary(immunosuppressed_genus_data.df,
#                                                            taxa_column = "taxonomy_genus", 
#                                                            group_by_columns = c("Sample_type"))
# immunosuppressed_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_genus_summary.df, 
#                                                                  grouping_variables = c("Sample_type"),
#                                                                  abundance_column = "Mean_relative_abundance",
#                                                                  my_top_n = 5)
# 
# temp <- subset(immunosuppressed_genus_data.df, taxonomy_genus %in% immunosuppressed_top_genus_summary.df$taxonomy_genus)
# temp$taxa_label <- genus_relabeller_function2(as.character(temp$taxonomy_genus))
# ggplot(temp, aes(x =taxa_label, y = Relative_abundance,fill = Sample_type)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(show.legend = F,
#               size = .4, 
#               alpha = 1,
#               position = position_jitterdodge()) +
#   # scale_fill_manual(values = variable_colours, name = variable_column) +
#   common_theme +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6))
# 

# Based on those taxa that we are interested in, e.g.:
unique(unique(subset(immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,Dunn_padj <= 0.05 & KrusW_pvalue <= 0.05)$Taxonomy),
unique(subset(immunocompetent_sample_type_genus_summary_top_significances_filtered.df,Dunn_padj <= 0.05 & KrusW_pvalue <= 0.05)$Taxonomy))

source("Code/helper_functions.R")
make_publication_plot <- function(taxa,
                                  sample_data.df,
                                  significances_IS.df,
                                  significances_IC.df,
                                  taxonomy_level,
                                  taxa_title = NULL,
                                  relabeller_function = NULL,
                                  p_value_column = "Dunn_padj",
                                  value_column = "Relative_abundance",
                                  ymin_break = 0, ymax_break = 100, ybreak = 10,ymax_limit = 140, ...){
  # FIXME - allow providing significance table and abundance table
  significances_subset_immunosuppressed.df <- significances_IS.df[grepl(taxa,significances_IS.df$Taxonomy),]
  significances_subset_immunocompetent.df <- significances_IC.df[grepl(taxa,significances_IC.df$Taxonomy),]
  
  # significances_subset_immunosuppressed <- immunosuppressed_sample_type_genus_summary_top_significances_filtered.df[grepl(taxa,immunosuppressed_sample_type_genus_summary_top_significances_filtered.df$Taxonomy),]
  # significances_subset_immunocompetent <- immunocompetent_sample_type_genus_summary_top_significances_filtered.df[grepl(taxa,immunocompetent_sample_type_genus_summary_top_significances_filtered.df$Taxonomy),]
  # significances_subset_immunosuppressed <- immunosuppressed_sample_type_genus_summary_all_significances.df[grepl(taxa,immunosuppressed_sample_type_genus_summary_all_significances.df$Taxonomy),]
  # significances_subset_immunocompetent <- immunocompetent_sample_type_genus_summary_all_significances.df[grepl(taxa,immunocompetent_sample_type_genus_summary_all_significances.df$Taxonomy),]

  data_subset_immunosuppressed.df <- subset(sample_data.df, Cohort == "immunosuppressed")
  data_subset_immunosuppressed.df <- data_subset_immunosuppressed.df[grepl(taxa,data_subset_immunosuppressed.df[,taxonomy_level]),]
  
  data_subset_immunocompetent.df <- subset(sample_data.df, Cohort == "immunocompetent")
  data_subset_immunocompetent.df <- data_subset_immunocompetent.df[grepl(taxa,data_subset_immunocompetent.df[,taxonomy_level]),]
  
  data_subset_immunosuppressed.df$Relative_abundance <- data_subset_immunosuppressed.df$Relative_abundance * 100
  data_subset_immunocompetent.df$Relative_abundance <- data_subset_immunocompetent.df$Relative_abundance * 100
  
  if (is.null(taxa_title)){
    if (!is.null(relabeller_function)){
      taxa_title <- relabeller_function(as.character(unique(data_subset_immunosuppressed.df[,taxonomy_level])[[1]]))
    } else{
      taxa_title <- unique(data_subset_immunosuppressed.df[,taxonomy_level])[[1]]  
    }
  }
  
  IS_plot <-  generate_significance_boxplots(mydata.df = data_subset_immunosuppressed.df,
                                             variable_column = "Sample_type",
                                             value_column = value_column,
                                             variable_colours_available = T,
                                             significances.df = significances_subset_immunosuppressed.df,
                                             p_value_column = p_value_column,
                                             sig_threshold = 0.05, ...) +
    labs(title = "Immunosuppressed") +
    xlab("Sample type") + 
    ylab(gsub("_", " ", value_column)) + 
    scale_y_continuous(breaks = seq(ymin_break,ymax_break,ybreak), limits = c(0,ymax_limit)) +
    theme(plot.title = element_text(size = 8),
          plot.subtitle = element_text(size = 5,hjust = .5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.title = element_text(size = 8))
  
  IC_plot <- generate_significance_boxplots(mydata.df = data_subset_immunocompetent.df,
                                            variable_column = "Sample_type",
                                            value_column = value_column,
                                            variable_colours_available = T,
                                            significances.df = significances_subset_immunocompetent.df,
                                            p_value_column = p_value_column,
                                            sig_threshold = 0.05,...) +
    labs(title = "Immunocompetent") +
    xlab("Sample type") + 
    ylab("") +
    # scale_y_continuous( breaks = seq(0,1,.1), limits = c(0,1.4)) +
    scale_y_continuous(breaks = seq(ymin_break,ymax_break,ybreak), limits = c(0,ymax_limit)) +
    theme(plot.title = element_text(size = 8),
          plot.subtitle = element_text(size = 5,hjust = .5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.title = element_text(size = 8))
  
  title <- ggdraw() + 
    draw_label(
      taxa_title,
      fontface = 'bold',
      x = .5,
      hjust = 0.5,
      size = 6
    ) 
  grid_plot <- plot_grid(plotlist = list(IS_plot, NULL, IC_plot),ncol = 3,nrow=1, rel_widths = c(1,-.01,1),align = "hv")
  plot_grid(title, grid_plot,ncol = 1, rel_heights = c(0.1, 1))
  
}
# stat_summary(fun = mean, geom = "errorbar",
#              colour ="black",
#              linetype ="dotted",
#              size = .5,
#              width = .5,
#              aes(ymax = ..y.., ymin = ..y..))

# ---------------------------------------------
# ---------- Compare within cohort ------------
# ---------------------------------------------
# g__Rothia
myplot <- make_publication_plot(taxa = "g__Rothia",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 60,ybreak = 1,ymax_limit = 3,sig_vjust = 0.5)
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Rothia.pdf",width = 15,height = 12,units = "cm")
# ---------------------------------------------
# g__Staphylococcus
myplot <- make_publication_plot(taxa = "g__Staphylococcus",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                sig_vjust = 0.5,use_mean = T)

ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Staphylococcus.pdf",width = 15,height = 12,units = "cm")

myplot <- make_publication_plot(taxa = "g__Staphylococcus",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                value_column = "Read_count_logged",
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 20,ybreak = 1,ymax_limit = 7,sig_line_starting_scale = 1.1,sig_tip_length = .0005,sig_vjust = 0.5)

myplot
# ---------------------------------------------
# g__Malassezia 
myplot <- make_publication_plot(taxa = "g__Malassezia",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 60,ybreak = 10,ymax_limit = 65)
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Malassezia.pdf",width = 15,height = 12,units = "cm")
# ---------------------------------------------
# g__Aspergillus
myplot <- make_publication_plot(taxa = "g__Aspergillus",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 20,ybreak = .5,ymax_limit = 2,sig_line_starting_scale = 1.1,sig_tip_length = .0005)
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Aspergillus.pdf",width = 15,height = 12,units = "cm")
# ---------------------------------------------
# g__Pseudomonas
myplot <- make_publication_plot(taxa = "g__Pseudomonas",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 40,ybreak = 5,ymax_limit = 40, sig_line_starting_scale = 1.05,sig_line_scaling_percentage = 0.2)
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Pseudomonas.pdf",width = 15,height = 12,units = "cm")
# ---------------------------------------------
# g__Acinetobacter
myplot <- make_publication_plot(taxa = "g__Acinetobacter",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 40,ybreak = 5,ymax_limit = 30, sig_line_starting_scale = 1.05)
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Acinetobacter.pdf",width = 15,height = 12,units = "cm")
# ---------------------------------------------
# g__Anaerococcus
myplot <- make_publication_plot(taxa = "g__Anaerococcus",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 40,ybreak = 5,ymax_limit = 30, sig_line_starting_scale = 1.05,sig_vjust = 0.5)
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Anaerococcus.pdf",width = 15,height = 12,units = "cm")

# myplot <- make_publication_plot(taxa = "g__Anaerococcus",
#                                 significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
#                                 significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
#                                 taxonomy_level = "taxonomy_genus",
#                                 sample_data.df = genus_data.df,
#                                 relabeller_function = genus_relabeller_function,
#                                 value_column = "Read_count_logged",
#                                 ymin_break = 0, ymax_break = 40,ybreak = 1,ymax_limit = 5, sig_line_starting_scale = 1.05,sig_vjust = 0.5)
# myplot
# ---------------------------------------------
# g__Brevundimonas
myplot <- make_publication_plot(taxa = "g__Brevundimonas",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 40,ybreak = 1,ymax_limit = 12, sig_line_starting_scale = 1.05)
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Brevundimonas.pdf",width = 15,height = 12,units = "cm")
# ---------------------------------------------
# g__Paracoccus
myplot <- make_publication_plot(taxa = "g__Paracoccus",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 40,ybreak = 10,ymax_limit = 40, sig_line_starting_scale = 1.05, sig_line_scaling_percentage = 0.1,sig_vjust = 0.5)
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Paracoccus.pdf",width = 15,height = 12,units = "cm")
# ---------------------------------------------
# g__Cutibacterium  (formerly Propionibacterium)
myplot <- make_publication_plot(taxa = "g__Cutibacterium",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 100,ybreak = 10,ymax_limit = 120, sig_line_starting_scale = 1.05)
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Cutibacterium.pdf",width = 15,height = 12,units = "cm")
# ---------------------------------------------
# g__Anaerococcus
myplot <- make_publication_plot(taxa = "g__Anaerococcus",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 100,ybreak = 5,ymax_limit = 25, sig_line_starting_scale = 1.15)
myplot
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Anaerococcus.pdf",width = 15,height = 12,units = "cm")

myplot <- make_publication_plot(taxa = "g__Anaerococcus",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                ymin_break = 0, ymax_break = 100,ybreak = 5,ymax_limit = 25, sig_line_starting_scale = 1.15)

# ---------------------------------------------
# g__Micrococcus 
mytitle <- genus_relabeller_function(as.character(unique(genus_data.df[grepl("g__Micrococcus", genus_data.df$taxonomy_genus),]$taxonomy_genus)[[1]]))
myplot <- make_publication_plot(taxa = "g__Micrococcus",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                relabeller_function = genus_relabeller_function,
                                sample_data.df = genus_data.df,
                                ymin_break = 0, ymax_break = 100,ybreak = 10,ymax_limit = 40, sig_line_starting_scale = 1.05,
                                use_mean = F) 
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Micrococcus.pdf",width = 15,height = 12,units = "cm")

# ---------------------------------------------
# g__Corynebacterium
mytitle <- genus_relabeller_function(as.character(unique(genus_data.df[grepl("g__Corynebacterium", genus_data.df$taxonomy_genus),]$taxonomy_genus)[[1]]))
myplot <- make_publication_plot(taxa = "g__Corynebacterium",
                                significances_IS.df = immunosuppressed_sample_type_genus_summary_top_significances_filtered.df,
                                significances_IC.df = immunocompetent_sample_type_genus_summary_top_significances_filtered.df,
                                taxonomy_level = "taxonomy_genus",
                                relabeller_function = genus_relabeller_function,
                                sample_data.df = genus_data.df,
                                ymin_break = 0, ymax_break = 100,ybreak = 10,ymax_limit = 100, sig_line_starting_scale = 1.05)
ggsave(plot = myplot, filename = "Result_figures/abundance_analysis_plots/boxplots/g__Corynebacterium.pdf",width = 15,height = 12,units = "cm")



# ---------------------------------------------
# ---------- Compare between cohorts ----------
# ---------------------------------------------



# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# Heatmap, top # genera per sample_type in each cohort

# ------- immunosuppressed

# Summarise
immunosuppressed_sample_type_genus_summary.df <- generate_taxa_summary(immunosuppressed_genus_data.df,
                                                                       taxa_column = "taxonomy_genus", 
                                                                       group_by_columns = c("Sample_type"))
# Get top taxa
immunosuppressed_sample_type_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_sample_type_genus_summary.df, 
                                                                             grouping_variables = c("Sample_type"), 
                                                                             abundance_column = "Mean_relative_abundance", 
                                                                             my_top_n = 10)

# Make matrix
heatmap.m <- genus_rel.m[as.character(unique(immunosuppressed_sample_type_genus_summary_top.df$taxonomy_genus)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% immunosuppressed_metadata.df$Index]

myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = immunosuppressed_metadata.df, 
                     variables = c("Sample_type"),
                     my_row_labels = data.frame(rownames(heatmap.m), genus_relabeller_function(rownames(heatmap.m))),
                     column_title = "Sample",
                     row_title = "Genus",
                     plot_height = 4,
                     plot_width = 25,
                     cluster_columns = T,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("#08306B","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     filename = paste0("Result_figures/abundance_analysis_plots/sample_type_immunosuppressed_top_10_genus_heatmap.pdf"))

# Summarise
immunocompetent_sample_type_genus_summary.df <- generate_taxa_summary(immunocompetent_genus_data.df,
                                                                       taxa_column = "taxonomy_genus", 
                                                                       group_by_columns = c("Sample_type"))
# Get top taxa
immunocompetent_sample_type_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = immunocompetent_sample_type_genus_summary.df, 
                                                                             grouping_variables = c("Sample_type"), 
                                                                             abundance_column = "Mean_relative_abundance", 
                                                                             my_top_n = 10)

# Make matrix
heatmap.m <- genus_rel.m[as.character(unique(immunocompetent_sample_type_genus_summary_top.df$taxonomy_genus)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% immunocompetent_metadata.df$Index]

myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = immunocompetent_metadata.df, 
                     variables = c("Sample_type"),
                     my_row_labels = data.frame(rownames(heatmap.m), genus_relabeller_function(rownames(heatmap.m))),
                     column_title = "Sample",
                     row_title = "Genus",
                     plot_height = 3,
                     plot_width = 25,
                     cluster_columns = T,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("#08306B","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     filename = paste0("Result_figures/abundance_analysis_plots/sample_type_immunocompetent_top_10_genus_heatmap.pdf"))





# ---------------------------------------------------------------------------------------------
# Heatmap, genera across both cohorts that were in the top 20 and were signfiicantly differentally abundant according to dunn
temp <- genus_data.df[genus_data.df$taxonomy_genus %in% as.character(unique(immunosuppressed_sample_type_genus_summary_top_significances_filtered.df$Taxonomy, immunosuppressed_sample_type_genus_summary_top_significances_filtered.df$Taxonomy)),]
temp <- dcast(temp, taxonomy_genus~Sample, value.var = "Relative_abundance",fill = 0)
temp <- df2matrix(temp)
temp <- temp * 100
# temp_meta <- metadata.df[colnames(temp),]
# Heatmap(temp)
# metadata.df$Sample_type
source("Code/helper_functions.R")
myhm <- make_heatmap(temp,
             mymetadata = metadata.df,
             variables = c("Sample_type", "Cohort"),
             column_title = "Sample",
             row_title = "Genus",
             plot_height = 5,
             plot_width = 60,
             cluster_columns = F,
             cluster_rows = T,
             column_title_size = 10,
             row_title_size = 10,
             annotation_name_size = 6,
             show_cell_values = F,
             # my_annotation_palette = my_colour_palette_15,
             # my_palette = c("#08306B","#FFD92F","#67001F"),
             legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
             my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
             discrete_legend = T,
             legend_title = "Relative abundance %",
             palette_choice = 'bluered',
             row_dend_width = unit(3, "cm"),
             simple_anno_size = unit(.25, "cm"),
             show_top_annotation = T,
             filename = paste0("Result_figures/test.pdf"))
myhm$heatmap
# draw(myhm$heatmap, annotation_legend_list = c(myhm$legend), merge_legends = T)

temp[temp > 0] <- log(temp[temp > 0],10)
make_heatmap(temp,
             mymetadata = metadata.df,
             discrete_legend = T,
             plot_height = 5,
             plot_width = 60,
             my_breaks = seq(-3,3,.5),
             legend_title = "Relative abundance %",
             my_palette = c("darkblue", "white","red"),
             palette_choice = 'dark_bluered',
             filename = paste0("Result_figures/test2.pdf"))

