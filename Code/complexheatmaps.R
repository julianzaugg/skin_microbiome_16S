#************************************
# Build heatmaps at OTU/ASV and varying taxonomy levels
#************************************
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
# library(vegan)
library(reshape2)
# library(gplots)
# library(pheatmap)
library(grid)

source("Code/helper_functions.R")


# --------------------
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")

# library(devtools)
# install_github("jokergoo/ComplexHeatmap")

library(ComplexHeatmap) # Make Complex Heatmaps
# --------------------
# install.packages("circlize")
library(circlize)  # circular visualization in R




####################################
# Define various colour palettes
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
patient_palette_270 <- c("#456c00","#77bbff","#75fb3f","#273300","#f5006f","#ac008b","#125700","#ffef77","#00278e","#3d005e","#d84100","#015686","#01289f","#ff8c4c","#0070b5","#8015cd","#feffd6","#02d8aa","#019cf4","#4f2f00","#bbffe9","#c52900","#1b002c","#a3ac00","#5d9200","#f29fff","#231500","#934cfc","#988a00","#002cbb","#ffeb36","#ffa758","#f1f9ff","#000045","#b4004b","#602900","#390048","#e6c400","#00ce3c","#ff7bd0","#8cff56","#e60051","#b89aff","#00474b","#d5fbff","#ff79c2","#1d0016","#00635d","#ff8e33","#992300","#ff6e91","#ffa081","#534a00","#61002d","#ffe1c1","#8c0072","#00405d","#89ffc6","#607500","#64ff6f","#002e52","#9b97ff","#b1ccff","#02c5cd","#5dffba","#beff45","#00112b","#b8ff74","#7f0027","#0074cd","#005c6f","#3f00af","#dd7900","#cced00","#77ffd6","#ffc5b5","#99ffb1","#01ea72","#f0ff88","#007f63","#abff9d","#391200","#003a74","#114b00","#0a0100","#ff5fff","#ffccfb","#00d6b7","#c7ff93","#1efa5a","#005437","#f6af00","#a60024","#ffb7e6","#ea0043","#c7ffbc","#72ab00","#789300","#585500","#c3ff14","#00f398","#ab4a00","#9b7600","#85e5ff","#006235","#130020","#006825","#ff735c","#007a7f","#02a3a4","#4856ff","#bf52ff","#00edbc","#a31bd6","#009642","#e93bee","#e400ae","#ffbdd2","#00cfc7","#f1ffaa","#009b7a","#dd00c9","#ff697d","#004a14","#ff72ac","#ff3d1f","#fffaa3","#5d0400","#027ba4","#01c774","#002655","#00941f","#0a32d7","#82acff","#ff8af3","#ff4165","#001104","#ffd6f2","#efebff","#aebc00","#3e0030","#c5abff","#00402e","#ff4bae","#0275f1","#be89ff","#ffd899","#00c765","#01b208","#97ffd4","#7e9fff","#00fde1","#0050c9","#ff8eb5","#c800cd","#005173","#ff2b95","#76ff7a","#ea0074","#001d70","#009856","#f100a8","#ba6b00","#0293df","#00462d","#ff6862","#f6ff65","#02bbda","#2c2200","#01a876","#e35a00","#e3000f","#ff819e","#5a0039","#a558ff","#e2ffb2","#784800","#016def","#b400a2","#00143c","#00212d","#403d00","#ff75fe","#975300","#166c00","#260008","#917fff","#ff8d89","#01bf7a","#ffa6bf","#800086","#90a100","#cce4ff","#dad800","#52c900","#46a700","#0c0039","#0b0052","#79009d","#003c85","#bb0034","#59e7ff","#af0064","#64001e","#c0007e","#000897","#bd8400","#2b007f","#318400","#31f1ff","#7c8600","#807300","#ffc072","#6f005f","#770040","#e62c00","#2e0019","#005599","#6535e1","#5b0099","#006bd5","#0142a1","#baaf00","#00ab2d","#ffcc40","#edffec","#ef0031","#153100","#abe9ff","#6bbd00","#e5ff4e","#ffdb43","#ffa5ef","#01c4f3","#ffbd8f","#84d100","#bbff84","#9fcdff","#7b0059","#ffe897","#ff8711","#ffa869","#febaff","#20003a","#94002b","#5387ff","#756dff","#fff494","#a5c1ff","#e0ffcf","#002417","#530076","#ff8459","#ffe4ec","#00b650","#0119b7","#c963ff","#a2ff64","#9c6800","#03b6f8","#00a0c2","#00240b","#6297ff","#bd0010","#fff7af","#7d2d00","#cf7aff","#af5600","#322c00","#500028")
my_colour_palette_10_distinct <- c("#8eec45","#0265e8","#f6a800","#bf6549","#486900","#c655a0","#00d1b6","#ff4431","#aeb85c","#7e7fc8")
my_colour_palette_10_soft <- c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621")
####################################
# Function
log_matrix <- function(mymat){
  out <- log(mymat, 10)
  out[is.infinite(out)] <- 0
  return(out)
}


gm_mean = function(x, na.rm=TRUE){
  # The geometric mean, with some error-protection bits.
  exp(sum(log(x[x > 0 & !is.na(x)]), na.rm=na.rm) / length(x))
}

# Center log ratio transform
clr = function(x, base=2){
  x <- log((x / gm_mean(x)), base)
  x[!is.finite(x) | is.na(x)] <- 0.0
  return(x)
}


filter_heatmap_matrix <- function(myheatmap, row_max = 0, prevalence = 0){
  internal_heatmap <- myheatmap
  internal_heatmap <- internal_heatmap[which(apply(internal_heatmap, 1, max) >= row_max), ]
  # keep only OTUs/taxa that are in more than this fraction of samples
  filter_fraction <- prevalence
  entry_prevalences <- apply(internal_heatmap, 1, function(x) {length(which(x > 0))})/dim(internal_heatmap)[2]
  entries_from_prevalences <- names(entry_prevalences)[entry_prevalences > filter_fraction]
  entries_from_prevalences <- entries_from_prevalences[!is.na(entries_from_prevalences)]
  return(internal_heatmap[entries_from_prevalences,])
}

####################################

setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
source("Code/helper_functions.R")

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)


# Factorise discrete columns. Set order.
metadata.df$Patient <- factor(metadata.df$Patient)
metadata.df$Lesion_type_refined <- factor(metadata.df$Lesion_type_refined, levels = c("C", "C_P", "AK", "SCC_PL","SCC"))
metadata.df$Cohort <- factor(metadata.df$Cohort)
metadata.df$Gender <- factor(metadata.df$Gender)

# Load level specific data
genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Genus_counts_abundances_and_metadata.csv",header = T)
family_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Family_counts_abundances_and_metadata.csv",header = T)

# Load count matrices and covert to log space
# otu_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/OTU_counts.csv", sep = ",", header = T, row.names = 1)))
# genus_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Genus_counts.csv", sep = ",", header = T, row.names = 1)))
# family_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Family_counts.csv", sep = ",", header = T, row.names = 1)))
# order_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Order_counts.csv", sep = ",", header = T, row.names = 1)))
# class_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Class_counts.csv", sep = ",", header = T, row.names = 1)))
# phylum_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Phylum_counts.csv", sep = ",", header = T, row.names = 1)))

# Load count matrices and apply CLR transformation
# otu_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/OTU_counts.csv", sep = ",", header = T, row.names = 1)))
# species_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Specie_counts.csv", sep = ",", header = T, row.names = 1)))
# genus_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Genus_counts.csv", sep = ",", header = T, row.names = 1)))
# family_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Family_counts.csv", sep = ",", header = T, row.names = 1)))
# order_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Order_counts.csv", sep = ",", header = T, row.names = 1)))
# class_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Class_counts.csv", sep = ",", header = T, row.names = 1)))
# phylum_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Phylum_counts.csv", sep = ",", header = T, row.names = 1)))

# Load relative abundance matrices
otu_rel.m <- as.matrix(read.table(file = "Result_tables/relative_abundance_tables/OTU_relative_abundances.csv", sep = ",", header = T, row.names = 1))
genus_rel.m <- as.matrix(read.table(file = "Result_tables/relative_abundance_tables/Genus_relative_abundances.csv", sep = ",", header = T, row.names = 1))
family_rel.m <- as.matrix(read.table(file = "Result_tables/relative_abundance_tables/Family_relative_abundances.csv", sep = ",", header = T, row.names = 1))
# 
# # Cleanup column (sample) names - log matrices
# # colnames(otu_log.m) <- gsub("_J.*", "", colnames(otu_log.m))
# colnames(genus_log.m) <- gsub("_J.*", "", colnames(genus_log.m))
# colnames(family_log.m) <- gsub("_J.*", "", colnames(family_log.m))
# colnames(order_log.m) <- gsub("_J.*", "", colnames(order_log.m))
# colnames(class_log.m) <- gsub("_J.*", "", colnames(class_log.m))
# colnames(phylum_log.m) <- gsub("_J.*", "", colnames(phylum_log.m))
# 
# # Cleanup column (sample) names - CLR matrices
# # colnames(otu_clr.m) <- gsub("_J.*", "", colnames(otu_clr.m))
# # colnames(species_clr.m) <- gsub("_J.*", "", colnames(species_clr.m))
# colnames(genus_clr.m) <- gsub("_J.*", "", colnames(genus_clr.m))
# colnames(family_clr.m) <- gsub("_J.*", "", colnames(family_clr.m))
# colnames(order_clr.m) <- gsub("_J.*", "", colnames(order_clr.m))
# colnames(class_clr.m) <- gsub("_J.*", "", colnames(class_clr.m))
# colnames(phylum_clr.m) <- gsub("_J.*", "", colnames(phylum_clr.m))
# 
# # Cleanup column (sample) names - Relative abundance matrices
# colnames(otu_rel.m) <- gsub("_J.*", "", colnames(otu_rel.m))
# colnames(genus_rel.m) <- gsub("_J.*", "", colnames(genus_rel.m))
# colnames(family_rel.m) <- gsub("_J.*", "", colnames(family_rel.m))

# and correct metadata
# rownames(metadata.df) <- gsub("_J.*", "", rownames(metadata.df))
# metadata.df$Index <- gsub("_J.*", "", metadata.df$Index)

metadata.df <- subset(metadata.df, Project == "immunosuppressed" | Snapshot_sample_5 == "yes")

# Remove samples that are not in the metadata.
# otu_log.m <- otu_log.m[,colnames(otu_log.m) %in% metadata.df$Index]
# genus_log.m <- genus_log.m[,colnames(genus_log.m) %in% metadata.df$Index]
# family_log.m <- family_log.m[,colnames(family_log.m) %in% metadata.df$Index]
# order_log.m <- order_log.m[,colnames(order_log.m) %in% metadata.df$Index]
# class_log.m <- class_log.m[,colnames(class_log.m) %in% metadata.df$Index]
# phylum_log.m <- phylum_log.m[,colnames(phylum_log.m) %in% metadata.df$Index]

# Remove samples that are not in the metadata.
# otu_clr.m <- otu_clr.m[,colnames(otu_clr.m) %in% metadata.df$Index]
# species_clr.m <- species_clr.m[,colnames(species_clr.m) %in% metadata.df$Index]
# genus_clr.m <- genus_clr.m[,colnames(genus_clr.m) %in% metadata.df$Index]
# family_clr.m <- family_clr.m[,colnames(family_clr.m) %in% metadata.df$Index]
# order_clr.m <- order_clr.m[,colnames(order_clr.m) %in% metadata.df$Index]
# class_clr.m <- class_clr.m[,colnames(class_clr.m) %in% metadata.df$Index]
# phylum_clr.m <- phylum_clr.m[,colnames(phylum_clr.m) %in% metadata.df$Index]

# Remove samples that are not in the metadata.
otu_rel.m <- otu_rel.m[,colnames(otu_rel.m) %in% metadata.df$Index,drop=F]
genus_rel.m <- genus_rel.m[,colnames(genus_rel.m) %in% metadata.df$Index,drop=F]
family_rel.m <- family_rel.m[,colnames(family_rel.m) %in% metadata.df$Index,drop=F]


# If there are negative values in the CLR matrices, assign them a value of zero
# otu_clr.m[which(otu_clr.m < 0)] <- 0
# species_clr.m[which(species_clr.m < 0)] <- 0
# genus_clr.m[which(genus_clr.m < 0)] <- 0
# family_clr.m[which(family_clr.m < 0)] <- 0
# order_clr.m[which(order_clr.m < 0)] <- 0
# class_clr.m[which(class_clr.m < 0)] <- 0
# phylum_clr.m[which(phylum_clr.m < 0)] <- 0




# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# Family level, filtered to top taxa by mean abundance

# Generate taxonomy summary
family_taxa_summary.df <- generate_taxa_summary(mydata = family_data.df,taxa_column = "taxonomy_family",
                                               group_by_columns = c("Cohort", "Lesion_type_refined"))

# Get top taxa by mean abundance
family_taxa_summary_filtered.df <- filter_summary_to_top_n(taxa_summary = family_taxa_summary.df, 
                                                          grouping_variables = c("Cohort", "Lesion_type_refined"),
                                                          abundance_column = "Mean_relative_abundance",
                                                          my_top_n = 20)

# Create heatmap matrix
heatmap.m <- family_rel.m[rownames(family_rel.m) %in% family_taxa_summary_filtered.df$taxonomy_family,]

# Make heatmap
source("Code/helper_functions.R")
make_heatmap(heatmap.m*100, 
             # mymetadata = subset(metadata.df, Lesion_type_refined %in% c("SCC_PL", "SCC")),
             mymetadata = metadata.df,
             filename = paste0("Result_figures/heatmaps/family_top_20_by_mean_relative_abundance_heatmap.pdf"),
             variables = c("Lesion_type_refined","Cohort"),
             column_title = "Sample",
             row_title = "Genus",
             plot_height = 8,
             plot_width = 60,
             cluster_columns = F,
             cluster_rows = T,
             column_title_size = 10,
             row_title_size = 10,
             annotation_name_size = 6,
             show_cell_values = F,
             # my_annotation_palette = my_colour_palette_15,
             legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
             my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
             # legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,1,.1))*100),
             # my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1))*100,
             discrete_legend = T,
             legend_title = "Mean relative abundance %",
             palette_choice = 'purple',
             row_dend_width = unit(3, "cm"),
             simple_anno_size = unit(.25, "cm"),
             show_top_annotation = T
)


# --------------------------------------------------------------------------------------------------------------------------------
# Genus level, filtered to top taxa by mean abundance

# Generate taxonomy summary
genus_taxa_summary.df <- generate_taxa_summary(mydata = genus_data.df,taxa_column = "taxonomy_genus",
                                               group_by_columns = c("Cohort", "Lesion_type_refined"))

# Get top taxa by mean abundance
genus_taxa_summary_filtered.df <- filter_summary_to_top_n(taxa_summary = genus_taxa_summary.df, 
                                                          grouping_variables = c("Cohort", "Lesion_type_refined"),
                                                          abundance_column = "Mean_relative_abundance",
                                                          my_top_n = 20)

# Create heatmap matrix
heatmap.m <- genus_rel.m[rownames(genus_rel.m) %in% genus_taxa_summary_filtered.df$taxonomy_genus,]

# Make heatmap
make_heatmap(heatmap.m*100, 
             # mymetadata = subset(metadata.df, Lesion_type_refined %in% c("SCC_PL", "SCC")),
             mymetadata = metadata.df,
             filename = paste0("Result_figures/heatmaps/genus_top_20_by_mean_relative_abundance_heatmap.pdf"),
             variables = c("Lesion_type_refined","Cohort"),
             column_title = "Sample",
             row_title = "Genus",
             plot_height = 10,
             plot_width = 60,
             cluster_columns = F,
             cluster_rows = T,
             column_title_size = 10,
             row_title_size = 10,
             annotation_name_size = 6,
             show_cell_values = F,
             # my_annotation_palette = my_colour_palette_15,
             legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
             my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
             # legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,1,.1))*100),
             # my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1))*100,
             discrete_legend = T,
             legend_title = "Mean relative abundance %",
             palette_choice = 'purple',
             row_dend_width = unit(3, "cm"),
             simple_anno_size = unit(.25, "cm"),
             show_top_annotation =F
)

# --------------------------------------------------------------------------------------------------------------------------------
# Genus level, filtered to top taxa by mean abundance
# Collapsed to cohort and lesion type and displaying the mean abundance

# Generate taxonomy summary for ...
genus_taxa_summary.df <- generate_taxa_summary(mydata = genus_data.df,
                                               taxa_column = "taxonomy_genus",
                                               group_by_columns = c("Cohort", "Lesion_type_refined"))

# Get top taxa by mean abundance
genus_taxa_summary_filtered.df <- filter_summary_to_top_n(taxa_summary = genus_taxa_summary.df, 
                                                                         grouping_variables = c("Cohort", "Lesion_type_refined"),
                                                                         abundance_column = "Mean_relative_abundance",
                                                                         my_top_n = 20)

# Generate matrix for heatmap
genus_taxa_summary.df$Cohort_Lesion_type_refined <- with(genus_taxa_summary.df, paste0(Cohort, "_", Lesion_type_refined))
heatmap.m <- genus_taxa_summary.df[c("Cohort_Lesion_type_refined", "taxonomy_genus","Mean_relative_abundance")]
heatmap.m <- heatmap.m[heatmap.m$taxonomy_genus %in% genus_taxa_summary_filtered.df$taxonomy_genus,]
heatmap.m <- heatmap.m %>% spread(Cohort_Lesion_type_refined, Mean_relative_abundance,fill = 0)
heatmap.m <- df2matrix(heatmap.m)

heatmap_metadata.df <- metadata.df
heatmap_metadata.df$Cohort_Lesion_type_refined <- with(heatmap_metadata.df, paste0(Cohort, "_", Lesion_type_refined))
heatmap_metadata.df <- unique(heatmap_metadata.df[,c("Cohort_Lesion_type_refined", "Cohort", "Cohort_colour", "Lesion_type_refined", "Lesion_type_refined_colour")])
rownames(heatmap_metadata.df) <- heatmap_metadata.df$Cohort_Lesion_type_refined
source("Code/helper_functions.R")
make_heatmap(heatmap.m*100, 
             mymetadata = heatmap_metadata.df,
             filename = paste0("Result_figures/heatmaps/Cohort_Lesion_type_refined_genus_top_20_mean_relative_abundance_heatmap.pdf"),
             variables = c("Cohort_Lesion_type_refined"),
             column_title = "Cohort_Lesion_type_refined",
             row_title = "Genus",
             plot_height = 15,
             plot_width = 10,
             cluster_columns = F,
             cluster_rows = T,
             column_title_size = 10,
             row_title_size = 10,
             annotation_name_size = 6,
             show_cell_values = F,
             # my_annotation_palette = my_colour_palette_15,
             legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
             my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
             # legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,1,.1))*100),
             # my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1))*100,
             discrete_legend = T,
             legend_title = "Mean relative abundance %",
             palette_choice = 'purple',
             row_dend_width = unit(3, "cm"),
             simple_anno_size = unit(.25, "cm"),
             show_top_annotation =F
)
# --------------------------------------------------------------------------------------------------------------------------------










# ------------------------------------------------------------------------------------

# heatmap_family_rel.m <- filter_heatmap_matrix(family_rel.m, row_max = 0.05, prevalence = 0.1)
# heatmap_genus_rel.m <- filter_heatmap_matrix(genus_rel.m, row_max = 0.05, prevalence = 0.1)
# heatmap_otu_rel.m <- filter_heatmap_matrix(otu_rel.m, row_max = 0.05, prevalence = 0.2)
# 
# new_row_names <- unlist(lapply(rownames(heatmap_otu_rel.m), function(x) {paste0(x, "; ", otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,]$Genus)}))
# row_labels.df <- data.frame("Row_label" = rownames(heatmap_otu_rel.m), "Row_label_new" = new_row_names)
# 
# # ------------------------------------
# # family, both cohorts
# make_heatmap(heatmap_family_rel.m, 
#              metadata.df,
#              filename = paste0("Result_figures/heatmaps/family_relative_abundance_both_cohorts_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 100,
#              cluster_columns = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# # family, immunosuppressed
# # discrete_variables <- c("Patient","Sampletype_final","Number_of_meds","Fitzpatrick_skin_type","Patient_group")
# discrete_variables <- c("Patient","Sampletype_final")
# make_heatmap(heatmap_family_rel.m, 
#              subset(metadata.df, Project == "immunosuppressed"),
#              filename = paste0("Result_figures/heatmaps/family_relative_abundance_immunosuppressed_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 25,
#              cluster_columns = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'purple'
# )
# # discrete_variables <- c("Sampletype_pooled")
# make_heatmap(heatmap_family_rel.m, 
#              subset(metadata.df, Project == "immunosuppressed"),
#              filename = paste0("Result_figures/heatmaps/family_relative_abundance_immunosuppressed.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 25,
#              cluster_columns = F,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'purple'
# )
# 
# 
# 
# # family, immunocompetent
# discrete_variables <- c("Patient","Sampletype_final")
# make_heatmap(heatmap_family_rel.m, 
#              subset(metadata.df, Project == "immunocompetent"),
#              filename = paste0("Result_figures/heatmaps/family_relative_abundance_immunocompetent_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 20,
#              cluster_columns = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'purple'
# )
# make_heatmap(heatmap_family_rel.m, 
#              subset(metadata.df, Project == "immunocompetent"),
#              filename = paste0("Result_figures/heatmaps/family_relative_abundance_immunocompetent.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 20,
#              cluster_columns = F,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'purple'
# )
# 
# 
# 
# 
# 
# # ------------------------------------
# # OTU, both cohorts
# make_heatmap(heatmap_otu_rel.m, 
#              metadata.df,
#              my_row_labels = row_labels.df,
#              filename = paste0("Result_figures/heatmaps/otu_relative_abundance_both_cohorts.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 100,
#              cluster_columns = F,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# make_heatmap(heatmap_otu_rel.m, 
#              metadata.df,
#              my_row_labels = row_labels.df,
#              filename = paste0("Result_figures/heatmaps/otu_relative_abundance_both_cohorts_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 100,
#              cluster_columns = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# # ------------------------------------
# # OTU separate cohorts
# discrete_variables <- c("Patient","Sampletype","Sampletype_pooled")
# # Competent
# make_heatmap(heatmap_otu_rel.m, 
#              subset(metadata.df, Project == "immunocompetent"),
#              my_row_labels = row_labels.df,
#              filename = paste0("Result_figures/heatmaps/otu_relative_abundance_immunocompetent.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 100,
#              cluster_columns = F,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# make_heatmap(heatmap_otu_rel.m, 
#              subset(metadata.df, Project == "immunocompetent"),
#              my_row_labels = row_labels.df,
#              filename = paste0("Result_figures/heatmaps/otu_relative_abundance_immunocompetent_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 100,
#              cluster_columns = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# 
# # Compromised
# make_heatmap(heatmap_otu_rel.m, 
#              subset(metadata.df, Project == "immunosuppressed"),
#              my_row_labels = row_labels.df,
#              filename = paste0("Result_figures/heatmaps/otu_relative_abundance_immunosuppressed.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 25,
#              cluster_columns = F,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# make_heatmap(heatmap_otu_rel.m, 
#              subset(metadata.df, Project == "immunosuppressed"),
#              my_row_labels = row_labels.df,
#              filename = paste0("Result_figures/heatmaps/otu_relative_abundance_immunosuppressed_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 25,
#              cluster_columns = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# 
# 
# # ------------------------------------
# # ------------------------------------
# # Genus, both cohorts
# make_heatmap(heatmap_genus_rel.m, 
#              metadata.df,
#              filename = paste0("Result_figures/heatmaps/genus_relative_abundance_both_cohorts.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 100,
#              cluster_columns = F,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# make_heatmap(heatmap_genus_rel.m, 
#              metadata.df,
#              filename = paste0("Result_figures/heatmaps/genus_relative_abundance_both_cohorts_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 100,
#              cluster_columns = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# # ------------------------------------
# # Genus separate cohorts
# discrete_variables <- c("Patient","Sampletype","Sampletype_pooled")
# # Competent
# make_heatmap(heatmap_genus_rel.m, 
#              subset(metadata.df, Project == "immunocompetent"),
#              filename = paste0("Result_figures/heatmaps/genus_relative_abundance_immunocompetent.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 100,
#              cluster_columns = F,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# make_heatmap(heatmap_genus_rel.m, 
#              subset(metadata.df, Project == "immunocompetent"),
#              filename = paste0("Result_figures/heatmaps/genus_relative_abundance_immunocompetent_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 100,
#              cluster_columns = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# 
# # Compromised
# make_heatmap(heatmap_genus_rel.m, 
#              subset(metadata.df, Project == "immunosuppressed"),
#              filename = paste0("Result_figures/heatmaps/genus_relative_abundance_immunosuppressed.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 25,
#              cluster_columns = F,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# make_heatmap(heatmap_genus_rel.m, 
#              subset(metadata.df, Project == "immunosuppressed"),
#              filename = paste0("Result_figures/heatmaps/genus_relative_abundance_immunosuppressed_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 10,
#              plot_width = 25,
#              cluster_columns = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# 
# 
# # ---------------------------------------------------------------------------------
# # Heatmaps for differentially abundant OTUs
# # Obviously assumes these results have been generated
# 
# 
# patient_deseq.df <- read.csv("Result_tables/DESeq_results/by_patient/patient__Sampletype_pooled_combined.csv", sep =",", header = T)
# lesion_cohort_deseq.df <- read.csv("Result_tables/DESeq_results/by_lesion_cohort/lesion_cohort_combined.csv", sep =",", header = T)
# 
# patient_deseq_heatmap_otu_rel.m <- otu_rel.m[unique(patient_deseq.df$OTU),]
# 
# new_row_names <- unlist(lapply(rownames(patient_deseq_heatmap_otu_rel.m), function(x) {paste0(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,]$Genus,"; ", x)}))
# row_labels.df <- data.frame("Row_label" = rownames(patient_deseq_heatmap_otu_rel.m), "Row_label_new" = new_row_names)
# 
# discrete_variables <- c("Patient","Sampletype","Sampletype_pooled", "Project")
# 
# make_heatmap(patient_deseq_heatmap_otu_rel.m, 
#              metadata.df,
#              my_row_labels = row_labels.df,
#              filename = paste0("Result_figures/heatmaps/patient_deseq_relative_abundance_both_cohorts_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 15,
#              plot_width = 80,
#              cluster_columns = T,
#              cluster_rows = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# 
# make_heatmap(patient_deseq_heatmap_otu_rel.m, 
#              metadata.df,
#              my_row_labels = row_labels.df,
#              filename = paste0("Result_figures/heatmaps/patient_deseq_relative_both_cohorts_abundance.pdf"),
#              variables = discrete_variables,
#              plot_height = 15,
#              plot_width = 80,
#              cluster_columns = F,
#              cluster_rows = F,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# 
# discrete_variables <- c("Patient","Sampletype","Sampletype_pooled")
# # Immunosuppressed
# patient_deseq_heatmap_otu_rel_suppressed.m <- otu_rel.m[patient_deseq.df[grepl("MST", patient_deseq.df$Variable),]$OTU,]
# patient_deseq_heatmap_otu_rel_competent.m <- otu_rel.m[patient_deseq.df[!grepl("MST", patient_deseq.df$Variable),]$OTU,]
# 
# new_row_names_suppressed <- unlist(lapply(rownames(patient_deseq_heatmap_otu_rel_suppressed.m), function(x) {paste0(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,]$Genus,"; ", x)}))
# row_labels_suppressed.df <- data.frame("Row_label" = rownames(patient_deseq_heatmap_otu_rel_suppressed.m), "Row_label_new" = new_row_names_suppressed)
# new_row_names_competent <- unlist(lapply(rownames(patient_deseq_heatmap_otu_rel_competent.m), function(x) {paste0(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,]$Genus,"; ", x)}))
# row_labels_competent.df <- data.frame("Row_label" = rownames(patient_deseq_heatmap_otu_rel_competent.m), "Row_label_new" = new_row_names_competent)
# 
# 
# make_heatmap(patient_deseq_heatmap_otu_rel_suppressed.m, 
#              subset(metadata.df, Project == "immunosuppressed"),
#              my_row_labels = row_labels_suppressed.df,
#              filename = paste0("Result_figures/heatmaps/patient_immunosuppressed_deseq_relative_abundance_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 15,
#              plot_width = 25,
#              cluster_columns = T,
#              cluster_rows = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# 
# make_heatmap(patient_deseq_heatmap_otu_rel_competent.m, 
#              subset(metadata.df, Project == "immunocompetent"),
#              my_row_labels = row_labels_suppressed.df,
#              filename = paste0("Result_figures/heatmaps/patient_immunocompetent_deseq_relative_abundance_clustered.pdf"),
#              variables = discrete_variables,
#              plot_height = 8,
#              plot_width = 80,
#              cluster_columns = F,
#              cluster_rows = T,
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,1,.1)),
#              # my_breaks = c(0, .1, .4),
#              legend_title = "Relative abundance %",
#              palette_choice = 'blue'
# )
# 
# 
# 
# # ---------------------------------------------------------------------------
# # Make heatmaps for top taxa, using mean abundance
# df2matrix <- function(mydataframe){
#   mymatrix <- mydataframe
#   rownames(mymatrix) <- mydataframe[,1]
#   mymatrix[,1] <- NULL
#   mymatrix <- as.matrix(mymatrix)
#   mymatrix
# }
# # Get the top 10 taxa per project and just show those in the plot
# genus_data.df <- read.csv("Result_tables/other/Genus_counts_abundances_and_metadata.csv",header = T)
# genus_data.df <- subset(genus_data.df, Sampletype_final_refined != "negative")
# immunocompetent_genus_data.df <- subset(genus_data.df, Project == "immunocompetent" & Snapshot_sample_5 == "yes")
# immunosuppressed_genus_data.df <- subset(genus_data.df, Project == "immunosuppressed")
# 
# immunosuppressed_genus_taxa_summary.df <- generate_taxa_summary(mydata = immunosuppressed_genus_data.df,
#                                                taxa_column = "taxonomy_genus",
#                                                group_by_columns = c("Project", "Sampletype_final_refined"))
# 
# immunosuppressed_genus_taxa_summary_filtered.df <- filter_summary_to_top_n(taxa_summary = immunosuppressed_genus_taxa_summary.df, 
#                                                           grouping_variables = c("Sampletype_final_refined"),
#                                                           abundance_column = "Mean_relative_abundance",
#                                                           my_top_n = 9)
# 
# temp <- immunosuppressed_genus_taxa_summary.df[c("Sampletype_final_refined", "taxonomy_genus","Mean_relative_abundance")]
# temp <- temp[temp$taxonomy_genus %in% immunosuppressed_genus_taxa_summary_filtered.df$taxonomy_genus,]
# temp <- temp %>% spread(Sampletype_final_refined, Mean_relative_abundance,fill = 0)
# temp <- df2matrix(temp)
# 
# 
# temp_metadata.df <- unique(metadata.df[,c("Sampletype_final_refined", "Sampletype_final_refined_colour"), drop = F])
# rownames(temp_metadata.df) <- temp_metadata.df$Sampletype_final_refined
# temp_metadata.df$Sampletype_final_refined <- factor(temp_metadata.df$Sampletype_final_refined, levels = c("C", "C_P", "AK", "SCC_PL", "SCC"))
# 
# make_heatmap(temp*100, 
#              mymetadata = temp_metadata.df,
#              filename = paste0("Result_figures/test.pdf"),
#              variables = c("Sampletype_final_refined"),
#              column_title = "",
#              annotation_name_size = 0,
#              plot_height = 5,
#              plot_width = 8,
#              cluster_columns = T,
#              cluster_rows = T,
#              column_title_size = 10,
#              row_title_size = 10,
#              legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, "> 60"),
#              my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
#              legend_title = "Mean relative abundance %",
#              palette_choice = 'purple'
# )
