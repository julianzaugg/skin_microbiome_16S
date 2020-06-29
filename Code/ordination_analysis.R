detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()

library(vegan)
library(ggplot2)
library(ggfortify)



############################################################
# Various colour colour_palettes
my_colour_palette <- c("#8dd3c7","#ffffb3","#bebada","#fb8072", "#80b1d3", "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5", "#cc0000")
# From http://tools.medialab.sciences-po.fr/iwanthue/
my_colour_palette_20 <- c("#66bd79","#a35bcf","#5bb643","#d14ea6","#a2b239","#5c6bcc","#dc892e","#5e93cd","#d64737","#49b6a8","#dc3c6e","#4f7e3c","#bd8cd5","#caab55","#914c88","#867230","#df82a2","#a65429","#ab4a5a","#e0896a")
my_colour_palette_20_distinct <- c("#0057b4","#7fff56","#d600bc","#d8d500","#e76eff","#019932","#9f8fff","#ffc730","#007fac","#a20019","#06fefd","#ff6782","#00774c","#e0c8ff","#717a00","#4b2952","#e2ed7d","#46321e","#ffbd76","#ffb4c6")
my_colour_palette_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_palette_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
my_colour_palette_15 <- c("#77b642","#7166d9","#cfa240","#b351bb","#4fac7f","#d44891","#79843a","#c68ad4","#d15a2c","#5ba7d9","#ce4355","#6570ba","#b67249","#9b4a6f","#df8398")
my_colour_palette_32_distinct <- c("#ea7e00","#ca0074","#d1c69b","#474007","#bb00ad","#9c80ff","#be3300","#542e72","#00b9f5","#09436b","#8b0036","#9ac8e6","#ff1059","#959eff","#154a11","#0290f4","#ff7762","#7dbf00","#ff8194","#834c00","#006e73","#f9bb5d","#d6c943","#017229","#00d3a8","#732427","#36e191","#6a8200","#efb3ea","#3227bb","#ff90e1","#e92a12")
lesion_colour_palette_7 <- c("#8558d6","#6ee268","#d247ad","#c9d743","#d7453e","#59a237","#d78f2a")
patient_colour_palette_45 <- c("#d64530","#585fb1","#795d97","#9e4773","#3f6921","#71692c","#a2b93c","#d571cc","#9b3e97","#33947a","#98ad66","#448a4e","#869ae0","#5ce7af","#e085a3","#dfdc87","#d19be2","#5cb735","#e38269","#3db6c0","#50b565","#50902c","#a98a2c","#dde84a","#db3d76","#5fe485","#7c8329","#b3e791","#6fe965","#5ebce9","#3c86c1","#2a6a45","#65b688","#6651d1","#af4ed3","#df872f","#56e4db","#737cea","#ac464b","#dd37b5","#995b2b","#daac6f","#92e2be","#a2e24b","#e0be3a")
my_colour_palette_10_distinct <- c("#8eec45","#0265e8","#f6a800","#bf6549","#486900","#c655a0","#00d1b6","#ff4431","#aeb85c","#7e7fc8")
######################## Functions #########################

# For each rowname (OTU), get the corresponding taxonomy_species
# Assumes "OTU.ID" and "taxonomy_species" columns in the provided map dataframe
assign_taxonomy_to_otu <- function(otutable, taxon_map){
  taxonomies <- c()
  for (otuid in rownames(otutable)){
    taxonomies <- c(taxonomies, as.character(taxon_map[taxon_map$OTU.ID == otuid,]$taxonomy_species))
  }
  return(taxonomies)
}

# Function that takes the metadata and a list of variables (column names) and returns those samples (rownames) with NA entries
get_samples_missing_data <- function(my_metadata, variables){
  samples_missing_data <- c()
  for (name in variables) {
    samples_missing_data <- c(samples_missing_data, rownames(my_metadata[is.na(my_metadata[[name]]),]))
  }
  return(unique(samples_missing_data))
}
############################################################
# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")
source("Code/helper_functions.R")

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)
rownames(metadata.df) <- metadata.df$Index

discrete_variables <- c("Sample_type","Gender","Patient", "Cohort", "Length_of_immunosuppression_group_1", "Length_of_immunosuppression_group_2")

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC and SCC lesions. 
# Remove samples for different Sample types (negative, nasal,scar,scar_PL,KA,KA_PL,VV,VV_PL,SF,SF_PL,other,other_PL) from metadata and otu table
# metadata.df <- metadata.df[metadata.df$Sample_type %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC", "LC", "NLC"),]

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the counts
otu.m <- as.matrix(read.csv("Result_tables/count_tables/OTU_counts.csv", header =T, row.names = 1))
genus.m <-  as.matrix(read.csv("Result_tables/count_tables/Genus_counts.csv", header =T, row.names = 1))
# genus.m <-  as.matrix(read.csv("Result_tables/count_tables/Genus_counts.csv", header =T, row.names = 1))
dim(metadata.df)
dim(genus.m)
dim(otu.m)
dim(subset(metadata.df, Cohort == "immunocompetent"))
dim(subset(metadata.df, Cohort == "immunosuppressed"))

genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Genus_counts_abundances_and_metadata.csv",header = T)
length(unique(genus_data.df$Sample))

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
# samples_removed <- metadata.df$Index[!metadata.df$Index %in% names(otu_rare.df)]
# metadata.df <- metadata.df[! metadata.df$Index %in% samples_removed,]

# Factorise discrete columns
metadata.df$Patient <- factor(metadata.df$Patient)
metadata.df$Sample_type_original <- factor(metadata.df$Sample_type_original)
# metadata.df$Lesion_type_refined <- factor(metadata.df$Lesion_type_refined, levels = c("C", "C_P", "AK", "SCC_PL", "SCC"))
metadata.df$Sample_type <- factor(metadata.df$Sample_type, levels = c("HS", "PDS", "AK", "SCC_PL", "SCC"))
metadata.df$Cohort <- factor(metadata.df$Cohort)
metadata.df$Gender <- factor(metadata.df$Gender)


# Need to factorise the colour columns as well
colour_columns <- names(metadata.df)[grepl("colour", names(metadata.df))]
metadata.df[colour_columns] <- lapply(metadata.df[colour_columns], factor)

# Filter to just immunosuppressed or snapshot samples
metadata.df <- subset(metadata.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# Remove samples from the OTU table that are not in the filtered metadata
otu.m <- otu.m[,as.character(metadata.df$Index)]
genus.m <- genus.m[,as.character(metadata.df$Index)]

# Order the matrices and metadata to be the same order
metadata.df <- metadata.df[order(rownames(metadata.df)),]
otu.m <- otu.m[,order(rownames(metadata.df))]
genus.m <- genus.m[,order(rownames(metadata.df))]

# CLR transform the otu matrix.
otu_clr.m <- clr(otu.m)
genus_clr.m <- clr(genus.m)


# ------------------------------------------------------------------------------------------------------------------------
# Testing
# temp_metadata <- subset(metadata.df, Sample_type != "negative")
# temp_data <- otu_rare_filtered.m[,rownames(temp_metadata)]
# temp <- capscale(t(otu_rare_filtered.m)~1, data = temp_metadata, distance = "bray")
# 
# generate_pca(temp, mymetadata = temp_metadata,
#              plot_height = 5, plot_width =5,
#              legend_x = -6, legend_y = 4,
#              point_size = .7, point_line_thickness = .3,point_alpha =.7,
#              legend_title = "Sample type",
#              plot_title = "Both cohorts, all Sample types",
#              limits = c(-5,5,-5,5),
#              plot_spiders = F,
#              plot_ellipses = F,
#              use_shapes = T,
#              ellipse_border_width = .5,
#              label_ellipse = F, ellipse_label_size = .5,
#              colour_palette = my_colour_palette_206_distinct,
#              variable_to_plot = "Sample_type", legend_cols = 1,
#              variable_colours_available = T,
#              filename = paste0("Result_figures/ordination_plots/both_cohorts_Sample_type_bray.pdf"))

# metadata_sampletype.df <- subset(metadata.df, Sample_type == "AK")
# metadata_sampletype.df <- metadata_sampletype.df[order(rownames(metadata_sampletype.df)),]
# otu_rare_sampletype.m <- otu_rare_filtered.m[,colnames(otu_rare_filtered.m) %in% rownames(metadata_sampletype.df)]
# m.pca_sampletype <- capscale(t(otu_rare_filtered.m)~1, data = metadata_sampletype.df, distance = "bray")
# 
# generate_pca(m.pca_sampletype, mymetadata = metadata_sampletype.df,
#              plot_height = 5, plot_width =5,
#              legend_x = -7, legend_y = 6,
#              point_size = .7, point_line_thickness = .3,point_alpha =.7,
#              legend_title = "Cohort",
#              include_legend = T,
#              plot_title = paste0("Sample_type : AK"),
#              # limits = c(-5,5,-5,5),
#              plot_hulls = F,
#              plot_spiders = F,
#              plot_ellipses = F,
#              use_shapes = T,
#              ellipse_border_width = .5,
#              label_ellipse = F, ellipse_label_size = .5,
#              colour_palette = my_colour_palette_206_distinct,
#              variable_to_plot = "Project", legend_cols = 1,
#              variable_colours_available = T,
#              filename = paste0("Result_figures/ordination_plots/AK_cohort_bray.pdf"))

# ------------------------------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------
# Ordination analysis

otu_relabeller_function <- function(my_labels){
  taxonomy_strings <- unlist(lapply(my_labels, function(x) {
    as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,]$taxonomy_genus)
  }
  ))
  unlist(lapply(taxonomy_strings, function(x) {
    phylostring <- unlist(strsplit(x, split = ";"))
    paste(phylostring[3], phylostring[6], sep = ";")
  }))
}
# otu_relabeller_function(rownames(otu.m))

genus_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  # paste(phylostring[2],phylostring[3], phylostring[6], sep = ";")
                  paste(phylostring[3], phylostring[6], sep = ";")
                }))
}

# ----------------
# Generate PCA plots. If CLR transformed values with euclidean distances, these will be the same as
# the values calculated from betadisper...maybe not important
# temp <- betadiver(t(otu_clr.m),method = "e")

# Generate ordination objects

# All samples
otu_pca <- rda(t(otu_clr.m), data = metadata.df)
genus_pca <- rda(t(genus_clr.m), data = metadata.df)

# Immunocompetent, all sample types
immunocompetent_samples <- as.character(metadata.df$Index[metadata.df$Cohort == "immunocompetent"])
immunocompetent_otu_pca <- rda(t(otu_clr.m[,immunocompetent_samples]))
immunocompetent_genus_pca <- rda(t(genus_clr.m[,immunocompetent_samples]))

immunocompetent_samples_2 <- as.character(metadata.df[which(!metadata.df$Patient %in% c("MS003", "MS012","MS013","MS014") & metadata.df$Cohort == "immunocompetent"),]$Index)
immunocompetent_genus_pca2 <- rda(t(genus_clr.m[,immunocompetent_samples_2]))

# Immunosuppressed, all sample types
immunosuppressed_samples <- as.character(metadata.df$Index[metadata.df$Cohort == "immunosuppressed"])
immunosuppressed_otu_pca <- rda(t(otu_clr.m[,immunosuppressed_samples]))
immunosuppressed_genus_pca <- rda(t(genus_clr.m[,immunosuppressed_samples]))

temp <- calculate_PC_abundance_correlations(genus_pca, mydata.df = genus_data.df,taxa_column = "taxonomy_genus",variables = discrete_variables)
# temp[order(temp$PC1_contribution_percentage,decreasing = T),]
# ------------------------------------------------------------------------------------
# All samples, both cohorts

# Sample_type
generate_pca(genus_pca, mymetadata = metadata.df,
             plot_height = 5, plot_width = 5,
             legend_x = -4, legend_y = 3,
             # legend_x = -2, legend_y = 2,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Sample type",
             legend_cex = .5,
             plot_title = "Both cohorts, all sample types",
             limits = c(-4,5,-6,3),
             plot_spiders = F,
             plot_ellipses = F,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = T,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = my_colour_palette_15,
             variable_to_plot = "Sample_type", legend_cols = 1,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/genus/both_cohorts_sample_type.pdf"))
             

# Patient
generate_pca(genus_pca, mymetadata = metadata.df,
             plot_height = 5, plot_width = 5,
             legend_x = -6, legend_y = 3,
             # legend_x = -2, legend_y = 2,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Patient",
             legend_cex = .5,
             plot_title = "Both cohorts, all Sample types",
             limits = c(-6,5,-6,3),
             plot_spiders = F,
             plot_ellipses = F,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = T,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Patient", 
             legend_cols = 2,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/genus/both_cohorts_Patient.pdf"))

# Cohort
generate_pca(genus_pca, mymetadata = metadata.df,
             plot_height = 5, plot_width = 5,
             legend_x = -4, legend_y = 3,
             # legend_x = -2, legend_y = 2,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Cohort",
             legend_cex = .5,
             plot_title = "Both cohorts, all Sample types",
             limits = c(-4,5,-6,3),
             plot_spiders = F,
             plot_ellipses = F,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = T,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Cohort", legend_cols = 1,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/genus/both_cohorts_cohort.pdf"))


# ------------------------------------------------------------------------------------
# All samples, immunocompetent

# Sample_type
generate_pca(immunocompetent_genus_pca, mymetadata = subset(metadata.df, Cohort == "immunocompetent"),
             plot_height = 5, plot_width = 5,
             legend_x = -6, legend_y = 5,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Sample type",
             legend_cex = .5,
             plot_title = "immunocompetent cohort, all Sample types",
             limits = c(-6,3,-4,5),
             plot_spiders = F,
             plot_ellipses = F,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = T,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Sample_type", legend_cols = 1,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/genus/immunocompetent_sample_type.pdf"))


# Patient ***
generate_pca(immunocompetent_genus_pca, mymetadata = subset(metadata.df, Cohort == "immunocompetent"),
             plot_height = 5, plot_width = 5,
             legend_x = -6, legend_y = 5,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Patient",
             legend_cex = .5,
             plot_title = "immunocompetent cohort, all Sample types",
             limits = c(-6,3,-4,6),
             plot_spiders = F,
             plot_ellipses = T,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = F,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Patient",
             legend_cols = 2,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/genus/immunocompetent_patient.pdf"))


generate_pca(immunocompetent_otu_pca, mymetadata = subset(metadata.df, Cohort == "immunocompetent"),
             plot_height = 5, plot_width = 5,
             legend_x = -6, legend_y = 5,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Patient",
             legend_cex = .5,
             plot_title = "immunocompetent cohort, all Sample types",
             limits = c(-7,7,-2,8),
             plot_spiders = F,
             plot_ellipses = T,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = F,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Patient",
             legend_cols = 2,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/otu/immunocompetent_patient.pdf"))

generate_pca(immunocompetent_otu_pca, mymetadata = subset(metadata.df, Cohort == "immunocompetent"),
             plot_height = 5, plot_width = 5,
             legend_x = -6, legend_y = 5,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Patient",
             legend_cex = .5,
             plot_title = "immunocompetent cohort, all Sample types",
             limits = c(-6,3,-4,5),
             plot_spiders = F,
             plot_ellipses = F,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = T,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Patient",
             legend_cols = 2,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/otu/immunocompetent_patient.pdf"))

# temp <- prcomp(t(otu_clr.m[,immunocompetent_samples]), center = T)
# autoplot(temp)

generate_pca(immunocompetent_genus_pca2, mymetadata = metadata.df[immunocompetent_samples_2,],
             plot_height = 5, plot_width = 5,
             legend_x = -6, legend_y = 5,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.7,
             legend_title = "Patient",
             legend_cex = .5,
             plot_title = "immunocompetent cohort, all Sample types",
             # limits = c(-6,3,-4,5),
             plot_spiders = F,
             plot_ellipses = F,
             plot_hulls = T,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = T,
             label_ellipse = T, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Patient",
             legend_cols = 2,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/genus/immunocompetent_patient2.pdf"))

# ---------------------------------------------------------------------------------------------------------
# Immunosuppressed, all sample types
# Sample_type
generate_pca(immunosuppressed_genus_pca, mymetadata = metadata.df[immunosuppressed_samples,],
             plot_height = 5, plot_width = 5,
             legend_x = -5, legend_y = 8,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Sample type",
             legend_cex = .5,
             plot_title = "immunosuppressed cohort, all Sample types",
             limits = c(-5,5,-5,8),
             plot_spiders = F,
             plot_ellipses = F,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = T,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Sample_type", legend_cols = 1,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/genus/immunosuppressed_sample_type.pdf"))


# Patient ***
generate_pca(immunosuppressed_genus_pca, mymetadata = metadata.df[immunosuppressed_samples,],
             plot_height = 5, plot_width = 5,
             legend_x = -5, legend_y = 8,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Patient",
             legend_cex = .5,
             plot_title = "immunosuppressed cohort, all Sample types",
             limits = c(-4,5,-5,8),
             plot_spiders = F,
             plot_ellipses = T,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = F,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Patient",
             legend_cols = 2,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/genus/immunosuppressed_patient.pdf"))

generate_pca(immunosuppressed_otu_pca, mymetadata = metadata.df[immunosuppressed_samples,],
             plot_height = 5, plot_width = 5,
             legend_x = -5, legend_y = 8,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Patient",
             legend_cex = .5,
             plot_title = "immunosuppressed cohort, all Sample types",
             limits = c(-4,7,-8,8),
             plot_spiders = F,
             plot_ellipses = T,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = F,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Patient",
             legend_cols = 2,
             variable_colours_available = T,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 3,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .2, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = otu_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/otu/immunosuppressed_patient.pdf"))


# Gender
source("Code/helper_functions.R")
generate_pca(immunosuppressed_genus_pca, mymetadata = metadata.df[immunosuppressed_samples,],
             plot_height = 5, plot_width = 5,
             legend_x = -5, legend_y = 8,
             point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
             legend_title = "Gender",
             legend_cex = .5,
             plot_title = "immunosuppressed cohort, all Sample types",
             limits = c(-5,5,-5,8),
             plot_spiders = F,
             plot_ellipses = F,
             plot_hulls = F,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = T,
             label_ellipse = F, ellipse_label_size = .3,
             colour_palette = patient_colour_palette_45,
             variable_to_plot = "Gender",
             legend_cols = 2,
             variable_colours_available = F,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
             label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
             specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
             filename = paste0("Result_figures/ordination_plots/genus/immunosuppressed_gender.pdf"))

# ---------------------------------------------------------------------------------------------------------

# Each Sample type, color by cohort and patient
for (sample_type in unique(metadata.df$Sample_type)){
  metadata_sample_type.df <- subset(metadata.df, Sample_type == sample_type)
  metadata_sample_type.df <- metadata_sample_type.df[order(rownames(metadata_sample_type.df)),]
  genus_clr_sample_type.m <- genus_clr.m[,colnames(genus_clr.m) %in% rownames(metadata_sample_type.df)]
  genus_pca_sample_type <- rda(t(genus_clr_sample_type.m), data = metadata_sample_type.df)
  generate_pca(genus_pca_sample_type, mymetadata = metadata_sample_type.df,
               plot_height = 5, plot_width = 5,
               legend_x = -5, legend_y = 8,
               point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
               legend_title = "Cohort",
               legend_cex = .5,
               plot_title = paste0("Sample type : ", sample_type),
               limits = c(-10,10,-10,10),
               plot_spiders = F,
               plot_ellipses = F,
               plot_hulls = F,
               use_shapes = T,
               ellipse_border_width = .5,
               include_legend = T,
               label_ellipse = F, ellipse_label_size = .3,
               colour_palette = patient_colour_palette_45,
               variable_to_plot = "Cohort",
               legend_cols = 2,
               variable_colours_available = T,
               num_top_species = 3,
               plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
               label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
               specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
               filename = paste0("Result_figures/ordination_plots/genus/",sample_type,"_cohort.pdf"))
  
  
  generate_pca(genus_pca_sample_type, mymetadata = metadata_sample_type.df,
               plot_height = 5, plot_width = 5,
               legend_x = -5, legend_y = 8,
               point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
               legend_title = "Patient",
               legend_cex = .5,
               plot_title = paste0("Sample type : ", sample_type),
               limits = c(-10,10,-10,10),
               plot_spiders = F,
               plot_ellipses = F,
               plot_hulls = F,
               use_shapes = T,
               ellipse_border_width = .5,
               include_legend = T,
               label_ellipse = F, ellipse_label_size = .3,
               colour_palette = patient_colour_palette_45,
               variable_to_plot = "Patient",
               legend_cols = 2,
               variable_colours_available = T,
               num_top_species = 3,
               plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
               label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
               specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
               filename = paste0("Result_figures/ordination_plots/genus/", sample_type, "_patient.pdf"))
}

# Each Sample type within cohort, color by patient
for (cohort in unique(metadata.df$Cohort)){
  for (sample_type in unique(metadata.df$Sample_type)){
    metadata_sample_type.df <- subset(metadata.df, Cohort == cohort & Sample_type == sample_type)
    metadata_sample_type.df <- metadata_sample_type.df[order(rownames(metadata_sample_type.df)),]
    genus_clr_sample_type.m <- genus_clr.m[,colnames(genus_clr.m) %in% rownames(metadata_sample_type.df)]
    if (dim(genus_clr_sample_type.m)[2] == 0){
      next
    }
    genus_pca_sample_type <- rda(t(genus_clr_sample_type.m), data = metadata_sample_type.df)
    generate_pca(genus_pca_sample_type, mymetadata = metadata_sample_type.df,
                 plot_height = 5, plot_width = 5,
                 legend_x = -5, legend_y = 8,
                 point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
                 legend_title = "Patient",
                 legend_cex = .5,
                 plot_title = paste0("Sample type : ", sample_type),
                 limits = c(-10,10,-10,10),
                 plot_spiders = F,
                 plot_ellipses = F,
                 plot_hulls = F,
                 use_shapes = T,
                 ellipse_border_width = .5,
                 include_legend = T,
                 label_ellipse = F, ellipse_label_size = .3,
                 colour_palette = patient_colour_palette_45,
                 variable_to_plot = "Patient",
                 legend_cols = 2,
                 variable_colours_available = T,
                 num_top_species = 3,
                 plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
                 label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
                 specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0,
                 filename = paste0("Result_figures/ordination_plots/genus/",cohort, "_",sample_type,"_patient.pdf"))
  }
}


# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# Publication figures

# par(mfrow = c(2,2),
#     oma = c(2,2,0,0),
#     mar = c(1,4,0,1))
make_publication_plot <- function(){
  source("Code/helper_functions.R")
  graphics.off()
  # svg(filename = "Result_figures/ordination_plots/sample_type_patient_genus_PCA_for_publication.svg",width = 8,height = 7)
  pdf(file = "Result_figures/ordination_plots/sample_type_patient_genus_PCA_for_publication.pdf",width = 8,height = 7)
  # par(mfrow = c(2,2))
  # par(mfrow = c(2,2),
  #     mar = c(1,10,5,1))
  par(mfrow = c(2,2),
      oma = c(1,4,0,0))
  # plot.new()
  par(mar = c(1,4,3,1))
  # par(bg = rgb(61, 55, 72, maxColorValue = 255))
  generate_pca(immunosuppressed_genus_pca, mymetadata = metadata.df[immunosuppressed_samples,],
               plot_height = 5, plot_width = 5,
               legend_x = -5, legend_y = 8,
               point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
               legend_title = "Sample type",
               legend_cex = .5,
               plot_title = "",
               limits = c(-5,4,-5,8),
               plot_spiders = F,
               plot_ellipses = F,
               plot_hulls = F,
               use_shapes = T,
               show_x_label = F,
               show_y_label = T,
               plot_x_ticks = T,
               plot_y_ticks = T,
               plot_x_tick_labels = F,
               plot_y_tick_labels = T,
               ellipse_border_width = .5,
               include_legend = T,
               label_ellipse = F, ellipse_label_size = .3,
               colour_palette = patient_colour_palette_45,
               variable_to_plot = "Sample_type", legend_cols = 1,
               variable_colours_available = T,
               num_top_species = 3,
               plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
               label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
               specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0)
  title(outer=F,adj=.5,main="Immunosuppressed",cex.main=4,col.main="black",font.main=2,line=1, cex.main =1.5, family = "serif")
  # title(outer=F,ylab = "By Sample type",font.lab=2, line = 5,cex.lab = 2, family = "serif")
  title(outer=T,adj=.77,ylab = "By sample type",font.lab=2, line = 2,cex.lab = 1.5, family = "serif")

  par(mar = c(1,4,3,1))
  generate_pca(immunocompetent_genus_pca, mymetadata = subset(metadata.df, Cohort == "immunocompetent"),
               plot_height = 5, plot_width = 5,
               legend_x = -4, legend_y = 4,
               point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
               legend_title = "Sample type",
               legend_cex = .5,
               plot_title = "",
               limits = c(-4,7,-6,4),
               plot_spiders = F,
               plot_ellipses = F,
               plot_hulls = F,
               use_shapes = T,
               show_x_label = F,
               show_y_label = T,
               plot_x_ticks = T,
               plot_y_ticks = T,
               plot_x_tick_labels = F,
               plot_y_tick_labels = T,
               ellipse_border_width = .5,
               include_legend = T,
               label_ellipse = F, ellipse_label_size = .3,
               colour_palette = patient_colour_palette_45,
               variable_to_plot = "Sample_type", legend_cols = 1,
               variable_colours_available = T,
               num_top_species = 3,
               plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
               label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
               specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0)
  title(outer=F,adj=.5,main="Immunocompetent",cex.main=4,col.main="black",font.main=2,line=1, cex.main =1.5, family = "serif")
  
  par(mar = c(4,4,0,1))
  generate_pca(immunosuppressed_genus_pca, mymetadata = metadata.df[immunosuppressed_samples,],
               plot_height = 5, plot_width = 5,
               legend_x = -5, legend_y = 8,
               point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
               legend_title = "Patient",
               legend_cex = .5,
               plot_title = "",
               limits = c(-4,4,-5,8),
               plot_spiders = F,
               plot_ellipses = T,
               plot_hulls = F,
               use_shapes = T,
               show_x_label = T,
               show_y_label = T,
               plot_x_ticks = T,
               plot_y_ticks = T,
               plot_x_tick_labels = T,
               plot_y_tick_labels = T,
               ellipse_border_width = .5,
               include_legend = F,
               label_ellipse = F, ellipse_label_size = .3,
               colour_palette = patient_colour_palette_45,
               variable_to_plot = "Patient",
               legend_cols = 2,
               variable_colours_available = T,
               num_top_species = 3,
               plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
               label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
               specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0)
  # title(outer=F,ylab = "By patient",font.lab=2, line = 5,cex.lab = 2, family = "serif")
  title(outer=T,adj = 0.26, ylab = "By patient",font.lab=2, line = 2,cex.lab = 1.5, family = "serif")
  
  par(mar = c(4,4,0,1))
  generate_pca(immunocompetent_genus_pca, mymetadata = subset(metadata.df, Cohort == "immunocompetent"),
               plot_height = 5, plot_width = 5,
               legend_x = -4, legend_y = 4,
               point_size = .7, point_line_thickness = 0.3,point_alpha =.9,
               legend_title = "Patient",
               legend_cex = .5,
               plot_title = "",
               limits = c(-4,7,-6,4),
               plot_spiders = F,
               plot_ellipses = T,
               plot_hulls = F,
               use_shapes = T,
               show_x_label = T,
               show_y_label = T,
               plot_x_ticks = T,
               plot_y_ticks = T,
               plot_x_tick_labels = T,
               plot_y_tick_labels = T,
               ellipse_border_width = .5,
               include_legend = F,
               label_ellipse = F, ellipse_label_size = .3,
               colour_palette = patient_colour_palette_45,
               variable_to_plot = "Patient",
               legend_cols = 2,
               variable_colours_available = T,
               num_top_species = 3,
               plot_arrows = F,arrow_alpha = .7, arrow_colour = "grey20",arrow_scalar = 2,arrow_thickness = .7,
               label_arrows = T, arrow_label_size = .5, arrow_label_colour = "black", arrow_label_font_type = 1,
               specie_labeller_function = genus_relabeller_function,arrow_label_offset = 0)
  # title(ylab = "test",outer = T)
  
  # title(main = "",outer = T,ylab = "Test")
  dev.off()
}
make_publication_plot()
# 
# include <- T
# plot(0,
#      type='n',
#      # x = 0, y=0,
#      xlim = c(0,1),
#      ylim = c(0,1),
#      xaxt = ifelse(include, "s","n"), xlab = "test")
# axis(labels = T, side = 3)

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# PERMANOVA tests whether distance differ between groups.

# Permutational Multivariate Analysis of Variance (PERMANOVA) can be used to 
# determine if the structure of the microbial communities is significantly different between
# environmental variables. This is done using the adonis function from Vegan with a distance metric, e.g. Bray-Curtis, and a
# specified number of permutations, e.g. 10,000.
# The analysis measures the degree each environmental variable affects the community composition and indicates 
# the significance of that effect on beta diversity (described by p-values and R2 values). 
# The R2 value corresponds to the proportion of variability observed in the dissimilarity.


# Genus, clr euclidean
print("Centred-log ratio transformed counts - Euclidean distance")

otu_permanova_results <- data.frame()
genus_permanova_results <- data.frame()

otu_interaction_patient_permanova_results <- data.frame()
genus_interaction_patient_permanova_results <- data.frame()

otu_within_cohort_permanova_results <- data.frame()
genus_within_cohort_permanova_results <- data.frame()

otu_interaction_patient_within_cohort_permanova_results <- data.frame()
genus_interaction_patient_within_cohort_permanova_results <- data.frame()

# run_permanova_custom(my_metadata = metadata.df,
#                      my_formula = as.formula(paste0("t(genus_clr_subset.m)~Patient+Cohort+Sample_type+Patient:Sample_type + Cohort:Sample_type")),
#                      my_method = "euclidean",label = "CLR",permutations = 999)

# Compare groups for each variable
for (myvar in discrete_variables){
  print(myvar)
  metadata_subset.df <- metadata.df[!is.na(metadata.df[,myvar]),]
  
  otu_clr_subset.m <- otu_clr.m[,rownames(metadata_subset.df)]
  genus_clr_subset.m <- genus_clr.m[,rownames(metadata_subset.df)]
  number_of_samples <- nrow(metadata_subset.df)
  
  temp <- run_permanova_custom(my_metadata = metadata_subset.df,
                               my_formula = as.formula(paste0("t(otu_clr_subset.m)~", myvar)),
                               my_method = "euclidean",label = "CLR",permutations = 999)
  temp$Number_of_samples <- number_of_samples
  otu_permanova_results <- rbind(otu_permanova_results,temp)
  
  temp <- run_permanova_custom(my_metadata = metadata_subset.df, 
                               my_formula = as.formula(paste0("t(genus_clr_subset.m)~", myvar)),
                               my_method = "euclidean",label = "CLR",permutations = 999)
  temp$Number_of_samples <- number_of_samples
  genus_permanova_results <- rbind(genus_permanova_results, temp)
  
  # Include an interaction with Patient in the model
  if (myvar != "Patient"){
    temp <- run_permanova_custom(my_metadata = metadata_subset.df, 
                                 my_formula = as.formula(paste0("t(otu_clr_subset.m)~Patient+", myvar, "+Patient:",myvar)),
                                 my_method = "euclidean",label = "CLR",permutations = 999)
    temp$Number_of_samples <- number_of_samples
    otu_interaction_patient_permanova_results <- rbind(otu_interaction_patient_permanova_results, temp)
    
    temp <- run_permanova_custom(my_metadata = metadata_subset.df, 
                                 my_formula = as.formula(paste0("t(genus_clr_subset.m)~Patient+", myvar, "+Patient:",myvar)),
                                 my_method = "euclidean",label = "CLR",permutations = 999)
    temp$Number_of_samples <- number_of_samples
    genus_interaction_patient_permanova_results <- rbind(genus_interaction_patient_permanova_results, temp)
  }
  # Also compare the groups after accounting for the cohort
  if (myvar == "Cohort") {next}
  for (cohort in unique(metadata.df$Cohort)){
    print(paste0("Processing cohort: ", cohort))
    metadata_subset.df <- metadata.df[!is.na(metadata.df[,myvar]),]
    metadata_subset.df <- subset(metadata_subset.df, Cohort == cohort)
    if (length(unique(metadata_subset.df[,myvar])) < 2){
      next
    }

    otu_clr_subset.m <- otu_clr.m[,rownames(metadata_subset.df)]
    genus_clr_subset.m <- genus_clr.m[,rownames(metadata_subset.df)]
    
    number_of_samples <- nrow(metadata_subset.df)
    
    # Include an interaction with Patient in the model
    if (myvar != "Patient"){
      temp <- run_permanova_custom(my_metadata = metadata_subset.df, 
                                   my_formula = as.formula(paste0("t(otu_clr_subset.m)~Patient+", myvar, "+Patient:",myvar)),
                                   my_method = "euclidean",label = "CLR",permutations = 999)
      temp$Cohort <- cohort
      temp$Number_of_samples <- number_of_samples
      otu_interaction_patient_within_cohort_permanova_results <- rbind(otu_interaction_patient_within_cohort_permanova_results, temp)
      
      temp <- run_permanova_custom(my_metadata = metadata_subset.df, 
                                   my_formula = as.formula(paste0("t(genus_clr_subset.m)~Patient+", myvar, "+Patient:",myvar)),
                                   my_method = "euclidean",label = "CLR",permutations = 999)
      temp$Cohort <- cohort
      temp$Number_of_samples <- number_of_samples
      genus_interaction_patient_within_cohort_permanova_results <- rbind(genus_interaction_patient_within_cohort_permanova_results, temp)
    }
    
    temp <- run_permanova_custom(my_metadata = metadata_subset.df, 
                                 my_formula = as.formula(paste0("t(otu_clr_subset.m)~", myvar)),
                                 my_method = "euclidean",label = "CLR",permutations = 999)
    temp$Cohort <- cohort
    temp$Number_of_samples <- number_of_samples
    otu_within_cohort_permanova_results <- rbind(otu_within_cohort_permanova_results, temp)
    
    temp <- run_permanova_custom(my_metadata = metadata_subset.df, 
                                 my_formula = as.formula(paste0("t(genus_clr_subset.m)~", myvar)),
                                 my_method = "euclidean",label = "CLR",permutations = 999)
    temp$Cohort <- cohort
    temp$Number_of_samples <- number_of_samples
    genus_within_cohort_permanova_results <- rbind(genus_within_cohort_permanova_results,temp)
    
  }
}
# genus_within_cohort_permanova_results

write.csv(otu_permanova_results, file = "Result_tables/stats_various/otu_PERMANOVA.csv", row.names = F, quote = F)
write.csv(genus_permanova_results, file = "Result_tables/stats_various/genus_PERMANOVA.csv", row.names = F, quote = F)

write.csv(otu_within_cohort_permanova_results, file = "Result_tables/stats_various/otu_within_cohort_PERMANOVA.csv", row.names = F, quote = F)
write.csv(genus_within_cohort_permanova_results, file = "Result_tables/stats_various/genus_within_cohort_PERMANOVA.csv", row.names = F, quote = F)

write.csv(otu_interaction_patient_permanova_results, file = "Result_tables/stats_various/otu_interaction_patient_PERMANOVA.csv", row.names = F, quote = F)
write.csv(genus_interaction_patient_permanova_results, file = "Result_tables/stats_various/genus_interaction_patient_PERMANOVA.csv", row.names = F, quote = F)

write.csv(otu_interaction_patient_within_cohort_permanova_results, file = "Result_tables/stats_various/otu_interaction_patient_within_cohort_PERMANOVA.csv", row.names = F, quote = F)
write.csv(genus_interaction_patient_within_cohort_permanova_results, file = "Result_tables/stats_various/genus_interaction_patient_within_cohort_PERMANOVA.csv", row.names = F, quote = F)

# ---------------------------------------------
# PERMDISP (betadisper)
# See: https://www.nicholas-ollberding.com/post/introduction-to-the-statistical-analysis-of-microbiome-data-in-r/
# "Test the homogeneity of within-group multivariate dispersions on the basis of any resemblance measure."
# temp_meta.df <- metadata.df
# temp_meta.df$Sample_type_Cohort <- with(temp_meta.df, paste0(Sample_type, "__", Cohort))
temp <- with(metadata.df, betadisper(vegdist(t(genus_clr.m), method = "euclidean"), group = Sample_type))
plot(temp, main = "Ordination Centroids and Dispersion Labeled: Aitchison Distance", sub = "")
boxplot(temp, main = "", xlab = "")
vegan::permutest(temp, permutations = 999, parallel = 2)

# ord_unifrac <- ordinate(t(genus_clr.m),method = "PCoA", distance = "wunifrac")
# ord_unifrac_un <- phyloseq::ordinate(t(genus_clr.m), method = "PCoA", distance = "unifrac")   
# temp <- run_permdisp_custom(metadata.df, 
#                     my_data = genus_clr.m,
#                     my_group = "Sample_type",
#                     my_method = "euclidean",
#                     permutations = 999, label = NULL)

otu_permdisp_results <- data.frame()
genus_permdisp_results <- data.frame()

otu_within_cohort_permdisp_results <- data.frame()
genus_within_cohort_permdisp_results <- data.frame()


for (myvar in discrete_variables){
  print(myvar)
  metadata_subset.df <- metadata.df[!is.na(metadata.df[,myvar]),]
  
  otu_clr_subset.m <- otu_clr.m[,rownames(metadata_subset.df)]
  genus_clr_subset.m <- genus_clr.m[,rownames(metadata_subset.df)]
  
  number_of_samples <- nrow(metadata_subset.df)
  
  temp <- run_permdisp_custom(my_metadata = metadata_subset.df, 
                              my_data = otu_clr_subset.m,
                              my_group = myvar,
                              my_method = "euclidean",
                              permutations = 999,
                              label = "CLR")
  temp$Number_of_samples <- number_of_samples
  otu_permdisp_results <- rbind(otu_permdisp_results, temp)
  
  temp <- run_permdisp_custom(my_metadata = metadata_subset.df, 
                              my_data = genus_clr_subset.m,
                              my_group = myvar,
                              my_method = "euclidean",
                              permutations = 999,
                              label = "CLR")
  temp$Number_of_samples <- number_of_samples
  genus_permdisp_results <- rbind(genus_permdisp_results, temp)
  
  if (myvar == "Cohort") {next}
  for (cohort in unique(metadata.df$Cohort)){
    print(paste0("Processing cohort: ", cohort))
    metadata_subset.df <- metadata.df[!is.na(metadata.df[,myvar]),]
    metadata_subset.df <- subset(metadata_subset.df, Cohort == cohort)
    if (length(unique(metadata_subset.df[,myvar])) < 2){
      next
    }
    number_of_samples <- nrow(metadata_subset.df)
    
    otu_clr_subset.m <- otu_clr.m[,rownames(metadata_subset.df)]
    genus_clr_subset.m <- genus_clr.m[,rownames(metadata_subset.df)]
    
    temp <- run_permdisp_custom(my_metadata = metadata_subset.df, 
                                my_data = otu_clr_subset.m,
                                my_group = myvar,
                                my_method = "euclidean",
                                permutations = 999,
                                label = "CLR")
    temp$Cohort <- cohort
    temp$Number_of_samples <- number_of_samples
    otu_within_cohort_permdisp_results <- rbind(otu_within_cohort_permdisp_results, temp)
    
    temp <- run_permdisp_custom(my_metadata = metadata_subset.df, 
                                my_data = genus_clr_subset.m,
                                my_group = myvar,
                                my_method = "euclidean",
                                permutations = 999,
                                label = "CLR")
    temp$Cohort <- cohort
    temp$Number_of_samples <- number_of_samples
    genus_within_cohort_permdisp_results <- rbind(genus_within_cohort_permdisp_results, temp)
  }
}

write.csv(otu_permdisp_results, file = "Result_tables/stats_various/otu_PERMDISP.csv", row.names = F, quote = F)
write.csv(genus_permdisp_results, file = "Result_tables/stats_various/genus_PERMDISP.csv", row.names = F, quote = F)

write.csv(otu_within_cohort_permdisp_results, file = "Result_tables/stats_various/otu_within_cohort_PERMDISP.csv", row.names = F, quote = F)
write.csv(genus_within_cohort_permdisp_results, file = "Result_tables/stats_various/genus_within_cohort_PERMDISP.csv", row.names = F, quote = F)


# ---------------------------------------------
# Takes awhile to calculate
# ANOSIM tests whether distances between groups are greater than within groups.
# "Nonparametric procedure for testing the hypothesis of no difference between two or more groups of entities
# based on permutation test of among- and within-group similarities"
# R = 1 when all pairs of samples within groups are more similar than to any pair of samples from different groups
# R = 0 expected value under the null model that among-and within- group dissimilarities are the same on average.

# "If you have very different group sizes, you may consider analysis of similarities (ANOSIM) instead of PERMANOVA. 
# This test does not assume equal group variances."

otu_anosim_results <- data.frame()
genus_anosim_results <- data.frame()

otu_within_cohort_anosim_results <- data.frame()
genus_within_cohort_anosim_results <- data.frame()

for (myvar in discrete_variables){
  print(myvar)
  metadata_subset.df <- metadata.df[!is.na(metadata.df[,myvar]),]
  
  otu_clr_subset.m <- otu_clr.m[,rownames(metadata_subset.df)]
  genus_clr_subset.m <- genus_clr.m[,rownames(metadata_subset.df)]
  
  number_of_samples <- nrow(metadata_subset.df)
  
  temp <- run_anosim_custom(my_metadata = metadata_subset.df, 
                            my_data = otu_clr_subset.m,
                            my_group = myvar,
                            my_method = "euclidean",
                            permutations = 999,
                            label = "CLR")
  temp$Number_of_samples <- number_of_samples
  otu_anosim_results <- rbind(otu_anosim_results, temp)
  
  temp <- run_anosim_custom(my_metadata = metadata_subset.df, 
                            my_data = genus_clr_subset.m,
                            my_group = myvar,
                            my_method = "euclidean",
                            permutations = 999,
                            label = "CLR")
  temp$Number_of_samples <- number_of_samples
  genus_anosim_results <- rbind(genus_anosim_results, temp)
  
  if (myvar == "Cohort") {next}
  for (cohort in unique(metadata.df$Cohort)){
    print(paste0("Processing cohort: ", cohort))
    
    metadata_subset.df <- metadata.df[!is.na(metadata.df[,myvar]),]
    metadata_subset.df <- subset(metadata_subset.df, Cohort == cohort)
    if (length(unique(metadata_subset.df[,myvar])) < 2){
      next
    }
    otu_clr_subset.m <- otu_clr.m[,rownames(metadata_subset.df)]
    genus_clr_subset.m <- genus_clr.m[,rownames(metadata_subset.df)]
    
    number_of_samples <- nrow(metadata_subset.df)
    
    temp <- run_anosim_custom(my_metadata = metadata_subset.df, 
                              my_data = otu_clr_subset.m,
                              my_group = myvar,
                              my_method = "euclidean",
                              permutations = 999,
                              label = "CLR")
    temp$Cohort <- cohort
    temp$Number_of_samples <- number_of_samples
    otu_within_cohort_anosim_results <- rbind(otu_within_cohort_anosim_results, temp)
    
    temp <- run_anosim_custom(my_metadata = metadata_subset.df, 
                              my_data = genus_clr_subset.m,
                              my_group = myvar,
                              my_method = "euclidean",
                              permutations = 999,
                              label = "CLR")
    temp$Cohort <- cohort
    temp$Number_of_samples <- number_of_samples
    genus_within_cohort_anosim_results <- rbind(genus_within_cohort_anosim_results, temp)
  }
}

write.csv(otu_anosim_results, file = "Result_tables/stats_various/otu_ANOSIM.csv", row.names = F, quote = F)
write.csv(genus_anosim_results, file = "Result_tables/stats_various/genus_ANOSIM.csv", row.names = F, quote = F)

write.csv(otu_within_cohort_anosim_results, file = "Result_tables/stats_various/otu_within_cohort_ANOSIM.csv", row.names = F, quote = F)
write.csv(genus_within_cohort_anosim_results, file = "Result_tables/stats_various/genus_within_cohort_ANOSIM.csv", row.names = F, quote = F)

