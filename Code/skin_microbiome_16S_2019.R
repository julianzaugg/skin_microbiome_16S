#************************************
# This script is the main data preparation script. Data is formatted for use elsewhere
# and relative abundances calculated at different taxonomy levels.
#
# NOTE - Although the below script refers to each representative sequence as an OTU, in reality
# these sequences are what you would call an amplicon sequence variant (ASV).
# For more information, see : https://www.nature.com/articles/ismej2017119
#************************************

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()

# Uncomment and run to install the libraries that might be needed 
# source("http://bioconductor.org/biocLite.R")
# biocLite("DESeq2")
# install.packages("ggplot2")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("RColorBrewer")
# install.packages("vegan")
# install.packages("reshape2")
# install.packages("gplots")

library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(vegan)
library(reshape2)
library(gplots)

####################################
# Define various colour palettes
my_colour_palette <- c("#8dd3c7","#ffffb3","#bebada","#fb8072", "#80b1d3", "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5", "#cc0000")
# From http://tools.medialab.sciences-po.fr/iwanthue/
my_colour_palette_20 <- c("#66bd79","#a35bcf","#5bb643","#d14ea6","#a2b239","#5c6bcc","#dc892e","#5e93cd","#d64737","#49b6a8","#dc3c6e","#4f7e3c","#bd8cd5","#caab55","#914c88","#867230","#df82a2","#a65429","#ab4a5a","#e0896a")
my_colour_palette_20_distinct <- c("#0057b4","#7fff56","#d600bc","#d8d500","#e76eff","#019932","#9f8fff","#ffc730","#007fac","#a20019","#06fefd","#ff6782","#00774c","#e0c8ff","#717a00","#4b2952","#e2ed7d","#46321e","#ffbd76","#ffb4c6")
my_colour_palette_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_palette_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
my_colour_palette_15 <- c("#77b642","#7166d9","#cfa240","#b351bb","#4fac7f","#d44891","#79843a","#c68ad4","#d15a2c","#5ba7d9","#ce4355","#6570ba","#b67249","#9b4a6f","#df8398")
my_colour_palette_32_distinct <- c("#ea7e00","#ca0074","#d1c69b","#474007","#bb00ad","#9c80ff","#be3300","#542e72","#00b9f5","#09436b","#8b0036","#9ac8e6","#ff1059","#959eff","#154a11","#0290f4","#ff7762","#7dbf00","#ff8194","#834c00","#006e73","#f9bb5d","#d6c943","#017229","#00d3a8","#732427","#36e191","#6a8200","#efb3ea","#3227bb","#ff90e1","#e92a12")

# Patient
patient_palette_45 <- c("#d64530","#585fb1","#795d97","#9e4773","#3f6921","#71692c","#a2b93c","#d571cc","#9b3e97","#33947a","#98ad66","#448a4e","#869ae0","#5ce7af","#e085a3","#dfdc87","#d19be2","#5cb735","#e38269","#3db6c0","#50b565","#50902c","#a98a2c","#dde84a","#db3d76","#5fe485","#7c8329","#b3e791","#6fe965","#5ebce9","#3c86c1","#2a6a45","#65b688","#6651d1","#af4ed3","#df872f","#56e4db","#737cea","#ac464b","#dd37b5","#995b2b","#daac6f","#92e2be","#a2e24b","#e0be3a")
# Lesion / Sampletype
lesion_palette_10 <- c("#d4a33e","#5ca876","#687fc9","#ce5944","#51b2d0","#9b62c8","#d14a8e","#79b041","#bc759a","#9c7f45")
# Cohort / Project
#335fa5 - blue
#c12a2a - red
project_palette_2 <- c("#335fa5", "#c12a2a")

####################################

matrix2df <- function(mymatrix, column_name){
  out <- as.data.frame(mymatrix)
  out_names <- colnames(out)
  out$placeholder <- rownames(out)
  rownames(out) <- NULL
  names(out)[length(names(out))] <- column_name
  out <- out[,c(column_name, out_names)]
  return(out)
}
####################################

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



################################################################################################
# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")

###############################################################
# Create result directories if they are missing
dir.create(file.path(".", "Result_figures"), showWarnings = FALSE)
dir.create(file.path(".", "Result_tables"), showWarnings = FALSE)
dir.create(file.path(".", "Result_objects"), showWarnings = FALSE)

dir.create(file.path("./Result_figures", "abundance_analysis_plots"), showWarnings = FALSE)
dir.create(file.path("./Result_figures/abundance_analysis_plots/boxplots"), showWarnings = FALSE, recursive = T)
dir.create(file.path("./Result_figures/abundance_analysis_plots/boxplots/Project_sampletype_final_refined"), showWarnings = FALSE, recursive = T)
dir.create(file.path("./Result_figures/abundance_analysis_plots/boxplots/Project"), showWarnings = FALSE, recursive = T)
# dir.create(file.path("./Result_figures/abundance_analysis_plots/boxplots/Project_sampletype_final"), showWarnings = FALSE, recursive = T)
dir.create(file.path("./Result_figures/abundance_analysis_plots/boxplots/Project_sampletype_final_refined"), showWarnings = FALSE, recursive = T)

dir.create(file.path("./Result_figures", "DESeq_plots"), showWarnings = FALSE)
dir.create(file.path("./Result_figures/DESeq_plots/boxplots"), showWarnings = FALSE, recursive = T)
dir.create(file.path("./Result_figures/DESeq_plots/boxplots/sampletype_final_refined_genus"), showWarnings = FALSE, recursive = T)
dir.create(file.path("./Result_figures/DESeq_plots/boxplots/sampletype_final_refined_otu"), showWarnings = FALSE, recursive = T)

dir.create(file.path("./Result_figures", "diversity_analysis"), showWarnings = FALSE)
dir.create(file.path("./Result_figures", "exploratory_analysis"), showWarnings = FALSE)
dir.create(file.path("./Result_figures", "heatmaps"), showWarnings = FALSE)
dir.create(file.path("./Result_figures", "mixomics"), showWarnings = FALSE)
dir.create(file.path("./Result_figures", "ordination_plots"), showWarnings = FALSE)
dir.create(file.path("./Result_figures", "contaminant_analysis"), showWarnings = FALSE)

dir.create(file.path("./Result_tables", "abundance_analysis_tables"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "count_tables"), showWarnings = FALSE)

dir.create(file.path("./Result_tables", "DESeq_results"), showWarnings = FALSE)
dir.create(file.path("./Result_tables/DESeq_results/by_patient"), showWarnings = FALSE, recursive = T)
dir.create(file.path("./Result_tables/DESeq_results/by_lesion_cohort"), showWarnings = FALSE, recursive = T)

dir.create(file.path("./Result_tables", "diversity_analysis"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "mixomics"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "other"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "relative_abundance_tables"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "stats_various"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "contaminant_analysis"), showWarnings = FALSE)

###############################################################
# Load the and process metadata

metadata.df <- read.table("data/metadata_immunocompromised_competent.tsv", header = T, sep = "\t")


# ---------------------------------------------------------------------------
# temp <- read.table("data/snapshot_temp.tsv", sep = "\t", header = T)
# temp$Snapshot <- as.character(temp$Snapshot)
# # unlist(lapply(temp$Swab.ID[!temp$Swab.ID %in% metadata.df$Swab_ID], function(x) paste(x, grep(x,metadata.df$Swab_ID, value = T,invert = F))[1]))
# # temp[temp$Swab.ID == 1198,]
# temp <- spread(temp,"Snapshot","Snapshot")
# temp[c("SN1","SN2","SN3","SN4")][!is.na(temp[c("SN1","SN2","SN3","SN4")])] <- "yes"
# dim(metadata.df)
# metadata.df <- merge(metadata.df, temp, by.x = "Swab_ID", by.y = "Swab.ID",all.x = T)
# # metadata.df <- merge(metadata.df, temp, by.x = "Swab_ID", by.y = "Swab.ID")
# dim(metadata.df)
# write.csv(metadata.df, file = "data/metadata_immunocompromised_competent_temp.csv",quote = F, row.names = F)
# metadata.df[metadata.df$LIMS.ID %in% c("S8752","S8878","S8899","S8901","SA5412","S8753","S8879","S8900","S8902","SA5413","SA5478"),]
# sort(as.numeric(as.character(temp$Swab.ID[!unlist(lapply(temp$Swab.ID, function(x) any(grepl(x, metadata.df$Swab_ID))))])))
# ---------------------------------------------------------------------------

# Make empty cells NA
metadata.df[metadata.df == ''] <- NA

# Change SwabCo to negative as they are the same
levels(metadata.df$Sampletype)[match("SwabCo",levels(metadata.df$Sampletype))] <- "negative"

# Make the index the rowname
rownames(metadata.df) <- metadata.df$Index

# We are only interested in a samples with the following lesion types. Filter the metadata to these samples.
metadata_unfiltered.df <- metadata.df
metadata.df <- metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC", "negative", "NLC"),]

# Change NLC to LC.
# metadata.df$Sampletype <- as.character(metadata.df$Sampletype)
# metadata.df$Sampletype[metadata.df$Sampletype == "NLC"] <- "LC"

# Create new pooled lesion types variables
pool_1 <- c("C","AK_PL","IEC_PL","SCC_PL", "LC", "NLC")
pool_2 <- c("AK","IEC")
pool_3 <- c("AK_PL","IEC_PL","SCC_PL")
pool_4 <- c("SCC", "IEC")

# If AK, make AK; SCC and IEC is SCC and everything else is NLC
# metadata.df$Sampletype_pooled <- factor(as.character(lapply(metadata.df$Sampletype, function(x) ifelse(x %in% pool_1, "LC", ifelse(x %in% pool_4, "SCC", ifelse(x == "negative", "negative","AK"))))))

# Create Sampletype_final variable. This is a less refined version than that created in the next section. May not be used for publication.
# Use the refined immunocompromised sampletypes and for the remaining use the Sampletype_pooled

# metadata.df$Sampletype_final <- as.character(metadata.df$Sampletype_compromised_refined)
# metadata.df[is.na(metadata.df$Sampletype_final),]$Sampletype_final <- as.character(metadata.df[is.na(metadata.df$Sampletype_final),]$Sampletype_pooled)
# metadata.df$Sampletype_final <- factor(metadata.df$Sampletype_final)
metadata.df$Sampletype_final <- NA

metadata.df[metadata.df$Project == "immunocompetent" & metadata.df$Sampletype %in% c("LC", "NLC", "SCC_PL", "IEC_PL"),]$Sampletype_final <- "LC"
metadata.df[metadata.df$Project == "immunocompetent" & metadata.df$Sampletype %in% c("AK"),]$Sampletype_final <- "AK"
metadata.df[metadata.df$Project == "immunocompetent" & metadata.df$Sampletype %in% c("SCC", "IEC"),]$Sampletype_final <- "SCC"

metadata.df[metadata.df$Project == "immunocompromised" & metadata.df$Sampletype_compromised_refined %in% c("C"),]$Sampletype_final <- "C"
metadata.df[metadata.df$Project == "immunocompromised" & metadata.df$Sampletype_compromised_refined %in% c("LC"),]$Sampletype_final <- "LC"
metadata.df[metadata.df$Project == "immunocompromised" & metadata.df$Sampletype_compromised_refined %in% c("AK"),]$Sampletype_final <- "AK"
metadata.df[metadata.df$Project == "immunocompromised" & metadata.df$Sampletype_compromised_refined %in% c("SCC"),]$Sampletype_final <- "SCC"
metadata.df[metadata.df$Sampletype %in% c("negative"),]$Sampletype_final <- "negative"
metadata.df$Sampletype_final <- factor(metadata.df$Sampletype_final)
# subset(metadata.df, Project == "immunocompromised")[,c("Sampletype", "Sampletype_compromised_refined", "Sampletype_final")]

# Create Sampletype_final_refined variable. This will likely be used for publication.
# For the immunocompetent cohort:
# C_P (C1-3 and AK_PL) – P indicating photo-damaged; as Nancy said they are not direct AK controls this may be the more appropriate description
# AK (AK)
# SCC_PL (SCC_PL and IEC_PL)
# SCC (SCC and IEC)

# For the immunocompromised cohort:
# C (swabs from proper control patients as indicated before)
# C_P (all C and AK_PL from remaining patients)
# AK (AK)
# SCC_PL (SCC_PL and IEC_PL)
# SCC (SCC and IEC)
metadata.df$Sampletype_final_refined <- NA
metadata.df[metadata.df$Project == "immunocompetent" & metadata.df$Sampletype %in% c("LC", "NLC"),]$Sampletype_final_refined <- "C_P"
metadata.df[metadata.df$Project == "immunocompetent" & metadata.df$Sampletype %in% c("AK"),]$Sampletype_final_refined <- "AK"
metadata.df[metadata.df$Project == "immunocompetent" & metadata.df$Sampletype %in% c("SCC_PL", "IEC_PL"),]$Sampletype_final_refined <- "SCC_PL"
metadata.df[metadata.df$Project == "immunocompetent" & metadata.df$Sampletype %in% c("SCC", "IEC"),]$Sampletype_final_refined <- "SCC"

metadata.df[metadata.df$Project == "immunocompromised" & metadata.df$Sampletype %in% c("C", "AK_PL"),]$Sampletype_final_refined <- "C_P"
metadata.df[metadata.df$Project == "immunocompromised" & metadata.df$Sampletype_compromised_refined %in% c("C"),]$Sampletype_final_refined <- "C"
metadata.df[metadata.df$Project == "immunocompromised" & metadata.df$Sampletype %in% c("AK"),]$Sampletype_final_refined <- "AK"
metadata.df[metadata.df$Project == "immunocompromised" & metadata.df$Sampletype %in% c("SCC_PL", "IEC_PL"),]$Sampletype_final_refined <- "SCC_PL"
metadata.df[metadata.df$Project == "immunocompromised" & metadata.df$Sampletype %in% c("SCC", "IEC"),]$Sampletype_final_refined <- "SCC"
metadata.df[metadata.df$Sampletype %in% c("negative"),]$Sampletype_final_refined <- "negative"
metadata.df$Sampletype_final_refined <- factor(metadata.df$Sampletype_final_refined)

# ------------------------------------
# Assign grouping to samples based on whether a patient has a SCC. Or if not, an AK/IEC. Or if not, control.
# metadata.df$Patient_grouping <- NA
# 
# # samples/patients that have at least one NLC
# NLC_patients <- as.character(metadata.df[metadata.df$Sampletype_pooled == "NLC",]$Patient)
# # samples/patients that have at least one AK
# AK_patients <- as.character(metadata.df[metadata.df$Sampletype_pooled == "AK",]$Patient)
# # samples/patients that have at least one SCC
# SCC_patients <- as.character(metadata.df[metadata.df$Sampletype_pooled == "SCC",]$Patient)
# 
# # AK patients not in SCC
# AK_patients <- AK_patients[!AK_patients %in% SCC_patients]
# # NLC patient not in AK or SCC
# NLC_patients <- NLC_patients[!NLC_patients %in% as.character(AK_patients, SCC_patients)]
# 
# # Assign group, starting with NLC, then AK then SCC
# metadata.df[metadata.df$Patient %in% NLC_patients,"Patient_grouping"] <- "Normal"
# metadata.df[metadata.df$Patient %in% AK_patients,"Patient_grouping"] <- "Has_AK"
# metadata.df[metadata.df$Patient %in% SCC_patients,"Patient_grouping"] <- "Has_SCC"

# ------------------------------------

# Remove unnessary columns / variables from metadata
names(metadata.df)
metadata.df <- metadata.df %>% select(-Sampletype_2, 
                       -Sampletype_compromised_refined, 
                       -Patient_group,
                       -Patient_group_other,
                       -Patient_ID_number,
                       -Age,
                       -Gender,
                       -Fitzpatrick_skin_type,
                       -Swab_category_other,
                       -Swab_photo,
                       -Number_of_meds,
                       -Bacterial_load_category)
names(metadata.df)
##############################
# Load and process the OTU table
project_otu_table.df <- read.csv("data/acepipe_immunocompromised_competent/features_statistics.csv")

# Fix name of first column
names(project_otu_table.df)[1] <- "OTU.ID"

# Fix names of r and b samples
# grep("R([1-4]_)", names(project_otu_table.df),value = T)
names(project_otu_table.df) <- gsub("R([1-4]_)","r\\1", names(project_otu_table.df))

# grep("B(_)", names(project_otu_table.df),value = T)
names(project_otu_table.df) <- gsub("B(_)","b\\1", names(project_otu_table.df))

#Fix the _J607 samples where read files did not have the r4 included. These are described in the metadata
samples_to_fix <- as.character(metadata.df[!metadata.df$Internal_name == "",]$Internal_name)
for (s2f in samples_to_fix){
  pattern <- paste("(",s2f,")", "(_J607)", sep ="")
  names(project_otu_table.df) <- gsub(pattern, "\\1r4\\2", names(project_otu_table.df))
}

# Remove the Internal_name column
metadata.df <- metadata.df %>% select(-Internal_name)

# Get the sample ids from the OTU table
sample_ids_original <- grep("R[0-9].*|S[AB][0-9].*|S[0-9].*", names(project_otu_table.df), value = T)
sample_ids <- grep("R[0-9].*|S[AB][0-9].*|S[0-9].*", names(project_otu_table.df), value = T)

print(paste0("There are ", length(sample_ids_original), " samples in the data"))

# ------------------------------------------------------------------------------------------
# The immunocompetent study from 2018 has repeat samples that should be combined.
# For example, S4284_J220 + S4284b_J220 = S4284_J220
# Go through the samples in the project table and combine as necessary (sum and remove b sample)

# First get the b samples S4284b_J220
repeat_samples.v <- grep("b_", sample_ids, value = T)

# The base form of the repeat samples, e.g. S4284
bases_repeat_samples.v <- gsub("_.*","",repeat_samples.v)

# The plate control samples
plate_control_samples.v <- grep("r[1-4]_", sample_ids, value = T)

# Original samples
original_samples.v <- sample_ids[!sample_ids %in% c("OTU.ID",plate_control_samples.v,repeat_samples.v)]
bases_original_samples.v <- gsub("_.*","",original_samples.v)

temp <- project_otu_table.df
# project_otu_table.df <- temp
# Add the counts for 'b' samples to their 'original' counterpart
for (sample in original_samples.v){
  sample_base <- gsub("_.*","",sample)
  if  (paste0(sample_base, 'b') %in% bases_repeat_samples.v){
    matching_repeat_sample <- grep(sample_base, repeat_samples.v, value = T)
    # print(paste(sample_base, sample, matching_repeat_sample))
    sum_base_prior <- sum(project_otu_table.df[,sample])
    sum_repeat <- sum(project_otu_table.df[,matching_repeat_sample])
    project_otu_table.df[,sample] <- project_otu_table.df[,sample] + project_otu_table.df[,matching_repeat_sample]
    sum_base_post <- sum(project_otu_table.df[,sample])
    # print(paste(sample_base, sample, matching_repeat_sample, sum_base_prior, sum_repeat, sum_base_post))
  }
}

# And now remove all the repeat 'b' samples from the project table
project_otu_table.df <- project_otu_table.df[,!colnames(project_otu_table.df) %in% repeat_samples.v]

# Reassign the sample_ids 
n_samples_before <- length(sample_ids)
sample_ids <- grep("R[0-9].*|S[AB][0-9].*|S[0-9].*", names(project_otu_table.df), value = T)
# sample_ids_original <- grep("R[0-9].*|S[AB][0-9].*|S[0-9].*", names(project_otu_table.df), value = T)

paste0("There are ", length(sample_ids), " samples in the data after consolidating immunocompetent samples. This is a loss of ", n_samples_before, "-", length(sample_ids)," = ", n_samples_before - length(sample_ids), " samples")

# ------------------------------------------------------------------------------------------
# Results from the ACE amplicon pipeline `should' contain at least one observation/count in every row, however just to be sure
# remove any rows containing all zeros. To do this, simply keep any row where there is any value not equal to zero.
# project_otu_table.df[sample_ids] will return all columns with names matching the sample ids
# The command below will take each row (MARGIN = 1) for the sample columns and check if any value is not zero.
project_otu_table.df <- project_otu_table.df[apply(project_otu_table.df[sample_ids], MARGIN = 1, function(z) any(z!=0)),]

# Split the Taxon column into Domain, Phylum...Species
project_otu_table.df <- separate(project_otu_table.df, "Taxon", into = c("Domain", "Phylum", "Class", "Order", "Family","Genus", "Species"), remove =F, sep = ";")

# Splitting taxa strings that are not specified at certain taxa levels will produce NA entries at those levels. 
# NA entries should be changed to "Unassigned"
project_otu_table.df[is.na(project_otu_table.df)] <- "Unassigned"

# Replace D_# at beginning of taxon rank string to corresponding taxa label, e.g. D_0 = d (domain) D_1 = p (phylum), etc.
# This doesn't work (well) for non-bacterial entries, which have more than the standard 7 levels
project_otu_table.df$Domain <- as.character(lapply(project_otu_table.df$Domain, FUN = function(x) gsub("D_0", "d", x)))
project_otu_table.df$Phylum <- as.character(lapply(project_otu_table.df$Phylum, FUN = function(x) gsub("D_1", "p", x)))
project_otu_table.df$Class <- as.character(lapply(project_otu_table.df$Class, FUN = function(x) gsub("D_2", "c", x)))
project_otu_table.df$Order <- as.character(lapply(project_otu_table.df$Order, FUN = function(x) gsub("D_3", "o", x)))
project_otu_table.df$Family <- as.character(lapply(project_otu_table.df$Family, FUN = function(x) gsub("D_4", "f", x)))
project_otu_table.df$Genus <- as.character(lapply(project_otu_table.df$Genus, FUN = function(x) gsub("D_5", "g", x)))
project_otu_table.df$Species <- as.character(lapply(project_otu_table.df$Species, FUN = function(x) gsub("D_6", "s", x)))

# Recreate the full taxonomy string with the 'prettier' taxa labels
project_otu_table.df$taxonomy_species <- with(project_otu_table.df, paste(Domain, Phylum, Class, Order, Family, Genus, Species, sep =";"))

# Also create a taxonomy string up to the genus level, since species are very rarely characterised at the Specie level in amplicon data
project_otu_table.df$taxonomy_genus <- with(project_otu_table.df, paste(Domain, Phylum, Class, Order, Family, Genus, sep =";"))

# And just for easier plotting later, create taxonomy strings for phylum, class, order and family levels
project_otu_table.df$taxonomy_family <- with(project_otu_table.df, paste(Domain, Phylum, Class, Order, Family, sep =";"))
project_otu_table.df$taxonomy_order <- with(project_otu_table.df, paste(Domain, Phylum, Class, Order, sep =";"))
project_otu_table.df$taxonomy_class <- with(project_otu_table.df, paste(Domain, Phylum, Class, sep =";"))
project_otu_table.df$taxonomy_phylum <- with(project_otu_table.df, paste(Domain, Phylum, sep =";"))

# Store a version of the unfiltered project table
project_otu_table_unfiltered.df <- project_otu_table.df

# ------------------------------------------------------------------------------------------
## Now ensure the metadata and the OTU table matches

# Sanity check whether all the samples are in the metadata, this should return nothing. 
# Otherwise fix the sample names in the metadata or simply remove the offending samples.
# Possibly have filtered out these samples at this point based on their sampletype
missing_samples.v <- sample_ids[!sample_ids %in% metadata.df$Index]
if ( length(missing_samples.v) == 0) {
  print("No samples missing")
} else {
  print("Samples missing from OTU table")
}
# summary(metadata.df[metadata.df$Index %in% missing_samples.v,]$Sampletype)
# summary(metadata_unfiltered.df[metadata_unfiltered.df$Index  %in% missing_samples.v,]$Sampletype)
# metadata_unfiltered.df[missing_samples.v,]

# Also the reverse, are there samples in the metadata missing from the data (possibly filtered earlier)
missing_samples_metadata.v <- as.character(metadata.df$Index[!metadata.df$Index %in% sample_ids])
# missing_samples_metadata.v <- as.character(metadata.df$Index[!metadata.df$Index %in% sample_ids_original])

if ( length(missing_samples_metadata.v) == 0) {
  print("No samples missing")
} else {
  print("Samples missing from metadata")
}
# summary(missing_samples_metadata.v %in% repeat_samples.v) # You will see that most, if not all, are repeat samples that have already been added to the base sample

# Remove samples from the project table that are not in the metadata
# dim(project_otu_table.df)
project_otu_table.df <- project_otu_table.df[, !colnames(project_otu_table.df) %in% missing_samples.v]
# dim(project_otu_table.df)

# Remove samples in the metadata that are not in the project table, e.g. repeat samples that have been merged at this point
# dim(metadata.df)
metadata.df <- metadata.df[!metadata.df$Index %in% missing_samples_metadata.v,]
# dim(metadata.df)

# Remove MS patient samples that were sequenced along with the immunocompromised samples
MS_resequenced_samples <- metadata.df[grep("Sequenced during immunocompromised batch", metadata.df$Note),]$Index
metadata.df <- metadata.df[!metadata.df$Index %in% MS_resequenced_samples,]
project_otu_table.df <- project_otu_table.df[,!names(project_otu_table.df) %in% MS_resequenced_samples]

# Remove the Note column
metadata.df <- metadata.df %>% select(-Note)

dim(metadata.df)
dim(project_otu_table.df)
# subset(metadata.df, Snapshot_sample == "yes")
# ------------------------------------------------
# ------------------------------------------------
# Assign unique colours for each discrete state, e.g. Sampletype
# For Sampletype (NLC and C have different colours!!!)
sampletype_values <- sort(factor(as.character(unique(metadata.df$Sampletype)), levels = sort(unique(as.character(metadata.df$Sampletype)))))
sampletype_colours <- setNames(lesion_palette_10[1:length(sampletype_values)], sampletype_values)

all_sample_colours <- as.character(lapply(as.character(metadata.df$Sampletype), function(x) sampletype_colours[x]))
metadata.df$Sampletype_colour <- all_sample_colours

# For Sampletype_pooled. Same Sampletype colours defined above
# all_sample_colours <- as.character(lapply(as.character(metadata.df$Sampletype_pooled), function(x) sampletype_colours[x]))
# metadata.df$Sampletype_pooled_colour <- all_sample_colours

# For Sampletype_final Same Sampletype colours defined above
sampletype_final_values <- sort(factor(as.character(unique(metadata.df$Sampletype_final)), levels = sort(unique(as.character(metadata.df$Sampletype_final)))))
sampletype_final_colours <- setNames(lesion_palette_10[1:length(sampletype_final_values)], sampletype_final_values)
all_sample_colours <- as.character(lapply(as.character(metadata.df$Sampletype_final), function(x) sampletype_final_colours[x]))
metadata.df$Sampletype_final_colour <- all_sample_colours


# Sampletype_final_refined
sampletype_final_refined_values <- sort(factor(as.character(unique(metadata.df$Sampletype_final_refined)), levels = sort(unique(as.character(metadata.df$Sampletype_final_refined)))))
sampletype_final_refined_colours <- setNames(lesion_palette_10[1:length(sampletype_final_refined_values)], sampletype_final_refined_values)
all_sample_colours <- as.character(lapply(as.character(metadata.df$Sampletype_final_refined), function(x) sampletype_final_refined_colours[x]))
metadata.df$Sampletype_final_refined_colour <- all_sample_colours

# For Sampletype_pooled_IEC_sep Same Sampletype colours defined above
# all_sample_colours <- as.character(lapply(as.character(metadata.df$Sampletype_pooled_IEC_sep), function(x) sampletype_colours[x]))
# metadata.df$Sampletype_pooled_IEC_sep_colour <- all_sample_colours


# For Sampletype_compromised_refined
# Sampletype_compromised_refined_values <- factor(as.character(unique(metadata.df$Sampletype_compromised_refined)))
# Sampletype_compromised_refined_values <- Sampletype_compromised_refined_values[!is.na(Sampletype_compromised_refined_values)]
# Sampletype_compromised_refined_colours <- setNames(lesion_palette_10[1:length(Sampletype_compromised_refined_values)], Sampletype_compromised_refined_values)
# Sampletype_compromised_refined_colours["SCC"] <- sampletype_colours["SCC"]
# Sampletype_compromised_refined_colours["C"] <- sampletype_colours["C"]
# Sampletype_compromised_refined_colours["LC"] <- sampletype_colours["LC"]
# Sampletype_compromised_refined_colours["AK"] <- sampletype_colours["AK"]
# all_Sampletype_compromised_refined_colours <- as.character(lapply(as.character(metadata.df$Sampletype_compromised_refined), function(x) Sampletype_compromised_refined_colours[x]))
# metadata.df$Sampletype_compromised_refined_colour <- all_Sampletype_compromised_refined_colours

# For Cohort (Project)
project_values <- factor(as.character(unique(metadata.df$Project)))
project_colours <- setNames(project_palette_2[1:length(project_values)], project_values)
all_project_colours <- as.character(lapply(as.character(metadata.df$Project), function(x) project_colours[x]))
metadata.df$Project_colour <- all_project_colours

# For Patient
patient_values <- as.character(unique(metadata.df$Patient))
patient_values <- patient_values[!is.na(patient_values)]
patient_colours <- setNames(patient_palette_45[1:length(patient_values)], patient_values)
all_patient_colours <- as.character(lapply(as.character(metadata.df$Patient), function(x) patient_colours[x]))
# all_patient_colours <- as.character(lapply(as.character(metadata.df$Patient), function(x) ifelse(is.na(x),"black", patient_colours[x])))
metadata.df$Patient_colour <- all_patient_colours

# For Patient group
# patient_group_values <- as.character(unique(metadata.df$Patient_group))
# patient_group_values <- patient_group_values[!is.na(patient_group_values)]
# patient_group_colours <- setNames(lesion_palette_10[1:length(patient_group_values)], patient_group_values)
# # Manually set to match sampletype (since the groupings are similar)
# patient_group_colours["SCC"] <- sampletype_colours["SCC"]
# patient_group_colours["Control"] <- sampletype_colours["LC"]
# patient_group_colours["AK"] <- sampletype_colours["AK"]
# all_patient_group_colours <- as.character(lapply(as.character(metadata.df$Patient_group), function(x) patient_group_colours[x]))
# metadata.df$Patient_group_colour <- all_patient_group_colours

# For Gender
# gender_values <- as.character(unique(metadata.df$Gender))
# gender_values <- gender_values[!is.na(gender_values)]
# gender_colours <- setNames(lesion_palette_10[1:length(gender_values)], gender_values)
# all_gender_colours <- as.character(lapply(as.character(metadata.df$Gender), function(x) gender_colours[x]))
# metadata.df$Gender_colour <- all_gender_colours

# For Number_of_meds
# n_meds_values <- as.character(unique(metadata.df$Number_of_meds))
# n_meds_values <- n_meds_values[!is.na(n_meds_values)]
# n_meds_colours <- setNames(lesion_palette_10[1:length(n_meds_values)], n_meds_values)
# all_n_meds_colours <- as.character(lapply(as.character(metadata.df$Number_of_meds), function(x) n_meds_colours[x]))
# metadata.df$Number_of_meds_colour <- all_n_meds_colours

# For Fitzpatrick_skin_type
# fst_values <- as.character(unique(metadata.df$Fitzpatrick_skin_type))
# fst_values <- fst_values[!is.na(fst_values)]
# fst_colours <- setNames(lesion_palette_10[1:length(fst_values)], fst_values)
# all_fst_colours <- as.character(lapply(as.character(metadata.df$Fitzpatrick_skin_type), function(x) fst_colours[x]))
# metadata.df$Fitzpatrick_skin_type_colour <- all_fst_colours


# ------------------------------------------------
# ------------------------------------------------

# Reassign the sample ids 
sample_ids <- grep("R[0-9].*|S[AB][0-9].*|S[0-9].*", names(project_otu_table.df), value = T)

# Write the top unassigned entries to file for later analysis
top_unassigned.df <- head(project_otu_table.df[project_otu_table.df$Taxon == "Unassigned",], n = 100)
write.csv(top_unassigned.df, file = "Result_tables/other/unassigned_entries.csv", row.names = F)

# ------------------------------------------------
# ----------- Remove unwanted lineages -----------

# Remove OTUs that are Unassigned
# project_otu_table.df <- project_otu_table.df[project_otu_table.df$Taxon != "Unassigned",]

# Discard anything not Bacterial or Fungal
project_otu_table.df <- project_otu_table.df[grepl("D_0__Bacteria|D_3__Fungi", project_otu_table.df$Taxon),]

# Discard Chloroplast entries
# dim(project_otu_table.df)
# D_3__Chloroplast distribution? Arachis?
# project_otu_table.df <- project_otu_table.df[!grepl("D_3__Chloroplast", project_otu_table.df$Taxon),]

# dim(project_otu_table.df)
# unique(grep("D_3__Chloroplast", project_otu_table.df$Taxon, value = T))
# "c__Oxyphotobacteria;o__Chloroplast"

# Discard anything not Bacterial or Fungal or Unassigned
# project_otu_table.df <- project_otu_table.df[grepl("D_0__Bacteria|D_3__Fungi|^Unassigned$", project_otu_table.df$Taxon),]

# Discard anything not Bacterial
# project_otu_table.df <- project_otu_table.df[grepl("D_0__Bacteria", project_otu_table.df$Taxon),]
# ------------------------------------------------

# Remove old Taxon column
project_otu_table.df$Taxon <- NULL
project_otu_table_unfiltered.df$Taxon <- NULL

# Store the OTUs and corresponding taxonomy information in a separate dataframe
otu_taxonomy_map.df <- project_otu_table.df[c("OTU.ID",
                                        "Domain", 
                                        "Phylum", 
                                        "Class", 
                                        "Order", 
                                        "Family",
                                        "Genus",
                                        "Species",
                                        "taxonomy_species", 
                                        "taxonomy_genus",
                                        "taxonomy_family",
                                        "taxonomy_order",
                                        "taxonomy_class",
                                        "taxonomy_phylum",
                                        "RepSeq")]

# Can save this table for later use if required
write.table(otu_taxonomy_map.df, file = "Result_tables/other/otu_taxonomy_map.csv", sep = ",", quote = F, row.names = F)

# Also save the unfiltered table, to avoid processing the original data table again 
write.table(project_otu_table_unfiltered.df, file = "Result_tables/other/project_otu_table_unfiltered.csv", sep = ",", quote = F, row.names = F)

# ---------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------
# Now we can generate the tables that we will need for different analyses at both the OTU and various taxa levels

# -----------------------------
# -----------OTU LEVEL---------
# Dataframe containing the counts for each OTU, for each sample
otu.df <- project_otu_table.df[c("OTU.ID", sample_ids)]
otu_unfiltered.df <- project_otu_table_unfiltered.df[c("OTU.ID", sample_ids)]

## Create a matrix version for ease of processing
# For filtered OTU matrix
otu.m <- otu.df
rownames(otu.m) <- otu.m$OTU.ID
otu.m$OTU.ID <- NULL
otu.m <- as.matrix(otu.m)

# And unfiltered OTU matrix
otu_unfiltered.m <- otu_unfiltered.df
rownames(otu_unfiltered.m) <- otu_unfiltered.m$OTU.ID
otu_unfiltered.m$OTU.ID <- NULL
otu_unfiltered.m <- as.matrix(otu_unfiltered.m)

# Create relative abundance matrix from counts matrix
otu_rel.m <- t(t(otu.m)/ colSums(otu.m))
otu_unfiltered_rel.m <- t(t(otu_unfiltered.m) / colSums(otu_unfiltered.m))

# Change nans to 0. Occurs when a sample has no hits at this point.
otu_rel.m[is.nan(otu_rel.m)] <- 0
otu_unfiltered_rel.m[is.nan(otu_unfiltered_rel.m)] <- 0

# summary(colSums(otu.m) < 5000)
# summary(colSums(otu.m) < 4000)
# colSums(otu_unfiltered.m)["SB4909_J1426"]
# colSums(otu_unfiltered.m)["SB4911_J1426"]
# 1647 SB4909_J1426 REMOVED
# 1649 SB4911_J1426 REMOVED

############################################################
# Filter those OTUs that are low abundance in all samples

# Generally, low abundance OTUs are removed.
# Check how many OTUs would be removed if we filtered any whose abundance is less than 0.05% (0.0005) in all samples
filter_percentage = 0.0005
otu_rel_low_abundance_otus.m <- otu_rel.m[apply(otu_rel.m[,sample_ids],1,function(z) all(z < filter_percentage)),]


# TODO - Collect low abundance OTUs that will be filtered out at this stage
# FIXME write.table(as.data.frame(otu_rel_low_abundance_otus.m), file = "Result_tables/count_tables/low_abundance_OTUs.csv", sep = ",", quote = F, col.names = T, row.names = F)
percent_removed_before_taxa_filtered <- round(dim(otu_rel_low_abundance_otus.m)[1]/length(rownames(otu_unfiltered.m)) * 100,2)
percent_removed_after_taxa_filtered <- round(dim(otu_rel_low_abundance_otus.m)[1]/length(rownames(otu.m)) * 100,2)

# percent_removed_before_taxa_filtered
# percent_removed_after_taxa_filtered

print(paste("There are a total of", dim(otu_rel.m)[1], "OTUs before filtering"))
print(paste("A total of", dim(otu_rel_low_abundance_otus.m)[1], 
            "OTUs will be filtered at a threshold of", 
            filter_percentage * 100, "percent"))

# If you are happy with the filtering threshold, apply it.
otu_rel.m <- otu_rel.m[apply(otu_rel.m[,sample_ids],1,function(z) any(z>=filter_percentage)),]

# Re-normalise the matrix after filtering
otu_rel.m <- t(t(otu_rel.m) / colSums(otu_rel.m))

# Change nans to 0. Occurs when a sample has no hits at this point.
otu_rel.m[is.nan(otu_rel.m)] <- 0

# Also remove low abundance OTUs from the original OTU count matrix
otu.m  <- otu.m[rownames(otu_rel.m),]

# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------
#                     REMOVE CONTAMINANTS

# Note that in the code for the 2018 paper, samples were first capped to a maximum read depth (rrarefy, not true rarefy) 
# before calculating contaminants presence/absence.
# e.g. otu_rare_count.m <- t(rrarefy(x = t(otu_count_raw.m), sample = 2000))
# Large differences in read depth between samples will make it harder to determine the true-positive contaminants from false-positive.
otu_rare_count.m <- t(rrarefy(x = t(otu.m), sample=30000)) # Ensure this value is the same as used in the 'rarefy' section

# Calculate the relative abundance on this rarefied table. Can use this to calculate the mean relative abundance of each OTU in different groups.
# In theory, a contaminant will make up a larger percent of the abundance in the negative control.
otu_rare_rel.m <- t(t(otu_rare_count.m) / colSums(otu_rare_count.m))

# Change nans to 0. Occurs when a sample has no hits at this point.
otu_rare_rel.m[is.nan(otu_rare_rel.m)] <- 0

# First get the sample ids for various lesion type groups
# TODO - Do we want to actually use negative samples rather than the control samples? I think so!
negative_sample_ids <- as.character(metadata.df[metadata.df$Sampletype == "negative",]$Index)
not_negative_sample_ids <- as.character(metadata.df[metadata.df$Sampletype != "negative",]$Index)
# not_negative_or_control_sample_ids <- as.character(metadata.df[!metadata.df$Sampletype %in% c("negative", "C"),]$Index)
# not_negative_or_control_sample_ids <- as.character(metadata.df[!metadata.df$Sampletype %in% c("negative", "C","LC", "NLC"),]$Index)
# pooled_not_negative_sample_ids <- as.character(metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC"),]$Index)
# pooled_not_negative_sample_ids <- as.character(metadata.df[metadata.df$Sampletype %in% c("SCC","C","NLC","AK_PL","IEC_PL","SCC_PL","AK","IEC"),]$Index)

# TODO - Determine whether to include controls when comparing against negative samples
contaminating_otus_from_mean_abundances <- rownames(otu_rare_rel.m[rowMeans(otu_rare_rel.m[,not_negative_sample_ids]) < rowMeans(otu_rare_rel.m[,negative_sample_ids]),])

# ------------------------------------------------------------------
# Approach to identifying contaminants used in 2018 paper. 
# Based on the prevalence/frequency/abundance among sample type, rather than the mean relative abundance.
# This will calculate what percentage of samples in each group an OTU is present in
otu_negative_sample_prevalences <- apply(otu_rare_count.m[,negative_sample_ids], 1, function(x) {length(which(x > 0))}) /length(negative_sample_ids)
otu_not_negative_sample_prevalences <- apply(otu_rare_count.m[,not_negative_sample_ids], 1, function(x) {length(which(x > 0))}) /length(not_negative_sample_ids)

contaminating_otus_from_prevalences <- names(otu_not_negative_sample_prevalences[otu_not_negative_sample_prevalences < otu_negative_sample_prevalences])

print(paste("There are", length(contaminating_otus_from_mean_abundances), "contaminating OTUs based on mean abundances"))
print(paste("There are", length(contaminating_otus_from_prevalences), "contaminating OTUs based on prevalences"))

# Number of features shared by both approaches
# length(contaminating_otus_from_mean_abundances[contaminating_otus_from_mean_abundances %in% contaminating_otus_from_prevalences])
# length(contaminating_otus_from_prevalences[contaminating_otus_from_prevalences %in% contaminating_otus_from_mean_abundances])

# Plot the OTU detection fraction in negative controls vs pooled samples
presences.df <- data.frame(row.names = rownames(otu_rare_count.m))
presences.df$not_negative <- melt(otu_not_negative_sample_prevalences)$value
presences.df$negative <- melt(otu_negative_sample_prevalences)$value
presences.df[,'contaminant'] <- "No"
presences.df[contaminating_otus_from_prevalences, 'contaminant'] <- "Yes" 
myplot <- ggplot(presences.df, aes(x =negative*100, y= not_negative*100)) + 
  geom_point(aes(color=contaminant)) +
  # geom_point(aes(color=contaminant, shape = contaminant)) +
  #scale_shape_manual(values = c(1,4)) +
  xlab("Percent of swab negative control samples feature detected in") +
  ylab("Percent of non-negative samples feature detected in") +
  # scale_x_continuous(breaks = seq(0,1,.25), limits = c(0,1.01)) +
  # scale_y_continuous(breaks = seq(0,1,.25), limits = c(0,1.01)) +
  scale_x_continuous(breaks = seq(0,100,25), limits = c(0,101)) +
  scale_y_continuous(breaks = seq(0,100,25), limits = c(0,101)) +
  common_theme
ggsave(plot = myplot, filename = "./Result_figures/contaminant_analysis/contaminant_otus_scatter_plot.pdf", width=6, height=6)

# ------------------------------------------------------------------
# Profile / summarise the samples containing contaminants

# ---- General ---- 
# How many / what samples are the contaminants in?
# How abundant are the contaminants in each sample?
# What samples types are the contaminants in?
# What is the break down per cohort?

# Get rows from OTU matrix corresponding to contaminants
contaminanted_samples.df <- as.data.frame(otu.m[contaminating_otus_from_prevalences,])
contaminanted_samples_rel.df <- as.data.frame(otu_rel.m[contaminating_otus_from_prevalences,])

# Profile only samples that have any contamination
contaminanted_samples.df <- contaminanted_samples.df[,colSums(contaminanted_samples.df) > 0]
contaminanted_samples_rel.df <- contaminanted_samples_rel.df[,colnames(contaminanted_samples.df)]

number_of_contaminated_samples <- length(names(contaminanted_samples.df))
number_of_contaminating_features <- length(contaminating_otus_from_prevalences)
paste0("There are ", number_of_contaminated_samples, " samples with at least one contaminant")
paste0("There are ", number_of_contaminating_features, " contaminanting features")

# Get the abundance of contaminants in each sample
contaminant_summary.df <- melt(sort(round(colSums(contaminanted_samples_rel.df)*100,2), decreasing = T))
names(contaminant_summary.df) <- "Contaminant_abundance"
contaminant_summary.df <- matrix2df(contaminant_summary.df,column_name = "Sample")
rownames(contaminant_summary.df) <- contaminant_summary.df$Sample

# Get the number of contaminant features in each sample
temp <- otu_rel.m[contaminating_otus_from_prevalences,names(contaminanted_samples.df)]
temp[temp > 0] <- 1
temp <- melt(colSums(temp))
contaminant_summary.df$Number_of_contaminant_features <- temp[rownames(contaminant_summary.df),]

# Get the sampletype, project and patient metadata for each sample
contaminant_summary.df$Sampletype_final_refined <- metadata.df[rownames(contaminant_summary.df),]$Sampletype_final_refined
contaminant_summary.df$Project <- metadata.df[rownames(contaminant_summary.df),]$Project
contaminant_summary.df$Patient <- metadata.df[rownames(contaminant_summary.df),]$Patient
write.csv(x = contaminant_summary.df, file = "Result_tables/contaminant_analysis/samples_with_contaminants_summary.csv", row.names = F)

# ggplot(contaminant_summary.df, aes(x = Sample, y = Contaminant_abundance, fill = Sampletype_final_refined)) + geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(~Project, scales = "free_x") +
#   common_theme +
#   theme(axis.text.x = element_text(angle = 90))
# summary(factor(subset(metadata.df, Project == "immunocompromised")$Sampletype))
# summary(subset(metadata_unfiltered.df, Project == "immunocompromised")$Sampletype)
# summary(factor(subset(metadata.df, Project == "immunocompetent")$Sampletype))
# summary(subset(metadata_unfiltered.df, Project == "immunocompetent")$Sampletype)


# ---- Specific ---- 
# We want a profile of the contaminating features (taxa).

# OTU taxonomy Max_abundance Mean_abundance
max_abundances.df <- matrix2df(melt(round(apply(contaminanted_samples_rel.df, 1, max) *100,3)), "OTU.ID")
names(max_abundances.df)[2] <- "Max_abundance"
min_abundances.df <- matrix2df(melt(round(apply(contaminanted_samples_rel.df, 1, min) *100,3)), "OTU.ID")
names(min_abundances.df)[2] <- "Min_abundance"
mean_abundances.df <- matrix2df(melt(round(apply(contaminanted_samples_rel.df, 1, mean) *100,3)), "OTU.ID")
names(mean_abundances.df)[2] <- "Mean_abundance"
median_abundances.df <- matrix2df(melt(round(apply(contaminanted_samples_rel.df, 1, median) *100,3)), "OTU.ID")
names(median_abundances.df)[2] <- "Median_abundance"
temp <- contaminanted_samples_rel.df
temp[temp > 0] <- 1
colnames(temp)
in_n_immunocompromised_samples <- matrix2df(melt(apply(temp[,names(temp) %in% rownames(metadata.df[metadata.df$Project == "immunocompromised",])], 1, sum)), "OTU.ID")
in_n_immunocompetent_samples <- matrix2df(melt(apply(temp[,names(temp) %in% rownames(metadata.df[metadata.df$Project == "immunocompetent",])], 1, sum)), "OTU.ID")
in_n_samples.df <- matrix2df(melt(apply(temp, 1, sum)), "OTU.ID")
names(in_n_samples.df)[2] <- "In_N_samples"
names(in_n_immunocompromised_samples)[2] <- "In_N_immunocompromised_samples"
names(in_n_immunocompetent_samples)[2] <- "In_N_immunocompetent_samples"


contaminant_profile.df <- otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID %in% contaminating_otus_from_prevalences,c("OTU.ID", "taxonomy_genus")]
# cbind(temp, in_n_samples, max_abundances)
contaminant_profile.df <- merge(contaminant_profile.df, in_n_samples.df, by.x = "OTU.ID", by.y = "OTU.ID")
contaminant_profile.df <- merge(contaminant_profile.df, in_n_immunocompetent_samples, by.x = "OTU.ID", by.y = "OTU.ID")
contaminant_profile.df <- merge(contaminant_profile.df, in_n_immunocompromised_samples, by.x = "OTU.ID", by.y = "OTU.ID")
contaminant_profile.df <- merge(contaminant_profile.df, max_abundances.df, by.x = "OTU.ID", by.y = "OTU.ID")
contaminant_profile.df <- merge(contaminant_profile.df, min_abundances.df, by.x = "OTU.ID", by.y = "OTU.ID")
contaminant_profile.df <- merge(contaminant_profile.df, mean_abundances.df, by.x = "OTU.ID", by.y = "OTU.ID")
contaminant_profile.df <- merge(contaminant_profile.df, median_abundances.df, by.x = "OTU.ID", by.y = "OTU.ID")

write.csv(x = contaminant_profile.df, file = "Result_tables/contaminant_analysis/contaminants_profile.csv", row.names = F)

# ------------------------------------------------------------------

# Filter the contaminating otus calculated from prevalences (or from mean abundances)
# otu_rel.m <- otu_rel.m[!rownames(otu_rel.m) %in% contaminating_otus_from_mean_abundances,]
otu_rel.m <- otu_rel.m[!rownames(otu_rel.m) %in% contaminating_otus_from_prevalences,]

# Re-normalise the matrix after filtering
otu_rel.m <- t(t(otu_rel.m) / colSums(otu_rel.m))

# Change nans to 0. Occurs when a sample has no hits at this point.
otu_rel.m[is.nan(otu_rel.m)] <- 0

# Now go back to OTU counts matrix and remove contaminate OTUs
otu.m <- otu.m[rownames(otu_rel.m),]

# Remove those samples with less than 5000 reads - used in previous paper
# TODO - write to file those samples that are removed
# colSums(otu.m) < 5000

otu_prior_to_removing_low_read_count_samples.m <- otu.m
otu_prior_to_removing_low_read_count_samples_rel.m <- otu_rel.m
dim(otu.m)
# otu.m <- otu.m[,colSums(otu.m) >= 5000]
otu.m <- otu.m[,colSums(otu.m) >= 4000]
dim(otu.m)
dim(otu.m[apply(otu.m, 1, max) == 0,])
# The might be many rows whos maximum is 0 at this point. Remove them.
otu.m <- otu.m[apply(otu.m, 1, max) != 0,]

# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------
#         Rarefying

# Note - For samples with a read count lower then the sample=# parameter, 
# the rrarefy function in vegan will return the this sample with its existing read count.
# Sample with counts higher than the sample parameter will be rarefied as normal and their counts will be capped at the sample parameter value

# Normally samples with OTU counts lower than our desired threshold need to be removed if we want to rarefy.
# Here we are simply using rrarefy to cap the read depth so the fold difference between the highest and lowest is not extreme.

# Looking at rarefaction curve and the counts for each sample can be a good
# way to determine a good minimum/maximum count threshold for samples

# Counts for each sample
column_sums <- colSums(otu.m)
column_sums.df <- melt(column_sums[order(column_sums)])
column_sums.df$sample <- rownames(column_sums.df)
rownames(column_sums.df) <- NULL
column_sums.df <- column_sums.df[c("sample", "value")]
column_sums.df$sample <- factor(column_sums.df$sample, levels = column_sums.df$sample)

myplot <- ggplot(column_sums.df, aes(x = sample, y = value)) + 
  geom_histogram(stat = "identity") +
  geom_hline(yintercept = 30000, color = 'red')+
  # geom_hline(yintercept = 20000, color = 'red')+
  geom_hline(yintercept = mean(column_sums.df$value), color = 'blue')+
  geom_hline(yintercept = median(column_sums.df$value), color = 'purple')+
  scale_y_continuous(breaks = seq(0,max(column_sums.df$value), 4000)) +
  xlab("Sample") +
  ylab("Read count") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90,vjust = .5, size = 3))
ggsave(plot = myplot, filename = "./Result_figures/exploratory_analysis/sample_read_depth_distribution.pdf", width=100, height=15, units = "cm")

myplot <- ggplot(column_sums.df, aes(x = value)) + 
  xlab("Read count") +
  ylab("Number of samples") +
  scale_x_continuous(breaks = seq(4000,max(column_sums.df$value), 4000)) +
  scale_y_continuous(breaks = seq(0,length(column_sums.df$value), 5)) +
  geom_histogram(stat = "bin", bins = 60, colour = "black",fill = "grey") +
  geom_vline(xintercept = 30000, color = 'red')+
  common_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))
ggsave(plot = myplot, filename = "./Result_figures/exploratory_analysis/sample_read_depth_distribution_2.pdf", width=20, height=20, units = "cm")

# summary(column_sums.df$value)
# mean(column_sums.df$value)
# median(column_sums.df$value)
#row_sums <- #rowSums(otu.m) # counts for OTUs

# Rarefaction curve
# rarecurve(t(otu.m[,colSums(otu.m) > 1]),step = 500, label = F,xlim = c(0,30000),sample = 30000)
# temp <- t(rrarefy(t(otu.m[,colSums(otu.m) >= 5000]), 5000))
# dim(temp)
# summary(metadata.df[colnames(temp),]$Sampletype) - summary(metadata.df[colnames(otu_rare_count.m),]$Sampletype)
# Generate a rarefied OTU count matrix
# In the paper from 2018, a rarefaction maximum of 20,000 was used
otu_rare_count.m <- t(rrarefy(x = t(otu.m), sample=30000))
# otu_rare_count.m <- t(rrarefy(x = t(otu.m), sample=20000))

# Compare before and after
# colSums(otu.m)[1:5]
# colSums(otu_rare_count.m)[1:5]
# head(melt(rowSums(otu.m[,1:5])))
# head(melt(rowSums(otu_rare_count.m[,1:5])))

# TODO - number of and fraction of OTUs that are removed from rarefying

# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

# And re-calculate the abundances after filtering
otu_rel.m <- t(t(otu.m)/ colSums(otu.m))
otu_rel.m[is.nan(otu_rel.m)] <- 0
otu_rel_rare.m <- t(t(otu_rare_count.m) /colSums(otu_rare_count.m))
otu_rel_rare.m[is.nan(otu_rel_rare.m)] <- 0
otu_rel.m[1:4,1:4]
otu_rel_rare.m[1:4,1:4]

# Reassign sample IDs
sample_ids <- colnames(otu_rel.m)

## (Re)build dataframes

# Un-rarefied
otu_rel.df <- data.frame("OTU.ID" = rownames(otu_rel.m))
otu_rel.df <- cbind(otu_rel.df, otu_rel.m[,colnames(otu_rel.m)])
rownames(otu_rel.df) <- c()

otu.df <- data.frame("OTU.ID" = rownames(otu.m))
otu.df <- cbind(otu.df, otu.m[,colnames(otu.m)])
rownames(otu.df) <- c()

# Rarified
otu_rel_rare.df <- data.frame("OTU.ID" = rownames(otu_rel_rare.m))
otu_rel_rare.df <- cbind(otu_rel_rare.df, otu_rel_rare.m[,colnames(otu_rel_rare.m)])
rownames(otu_rel_rare.df) <- c()

otu_rare.df <- data.frame("OTU.ID" = rownames(otu_rare_count.m))
otu_rare.df <- cbind(otu_rare.df, otu_rare_count.m[,colnames(otu_rare_count.m)])
rownames(otu_rare.df) <- c()


# Write the final otu counts and abundances to file
write.table(otu.df, file = "Result_tables/count_tables/OTU_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_rel.df, file = "Result_tables/relative_abundance_tables/OTU_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_rare.df, file = "Result_tables/count_tables/OTU_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_rel_rare.df, file = "Result_tables/relative_abundance_tables/OTU_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

# NOTE - otu.df, otu.m, otu_rel.df and otu_rel.m are the final, filtered OTU count / relative abundance dataframes and matrices. These can be used
# elsewhere for a variety of analyses at the OTU level, or, as is shown below, used to calculate abundances at different taxa levels

# Sample removed
samples_retained <- colnames(otu.df)[2:length(colnames(otu.df))]
samples_lost <- metadata.df$Index[!metadata.df$Index %in% samples_retained]
print(paste(length(samples_retained), "samples retained"))
print(paste(length(samples_lost), "samples lost"))

# Label those samples from the metadata.df that are not in the OTU table. A sample that has been filtered out
# will generally not be of interest for the study, though we may wish to have the metadata at hand
metadata.df$Sample_retained <- "no"
metadata.df[metadata.df$Index %in% samples_retained,]$Sample_retained <- "yes"
write.table(metadata.df[metadata.df$Sample_retained == "yes",], file = "Result_tables/other/processed_metadata.csv", sep = ",", quote = F, row.names = F)
write.table(metadata.df[metadata.df$Sample_retained == "no",], file = "Result_tables/other/metadata_samples_removed.csv", sep = ",", quote = F, row.names = F)


# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------
#                                                           METADATA AND READ SUMMARIES
# Generate summaries of the metadata

# Number of MST patients
n_MST_patients <- length(unique(metadata.df$Patient[grepl("MST", metadata.df$Patient)]))
# Number of MS patients
n_MS_patients <- length(unique(metadata.df$Patient[!grepl("MST", metadata.df$Patient)]))
# Number of MST samples
n_MST_samples <- length(metadata.df[grepl("MST", metadata.df$Patient),]$Index)
# Number of MS samples
n_MS_samples <- length(metadata.df[!grepl("MST", metadata.df$Patient),]$Index)

# Number of samples for each Sampletype
per_sampletype <- metadata.df %>% 
  group_by(Sampletype) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count))*100,2)) %>%
  arrange(desc(Percent)) %>%
  as.data.frame
# per_sampletype <- per_sampletype[order(per_sampletype$Percent),]per_sampletype$Sampletype <- factor(per_sampletype$Sampletype, levels = per_sampletype$Sampletype)

per_sampletype_final_refined <- metadata.df %>% 
  group_by(Sampletype_final_refined) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count))*100,2)) %>%
  arrange(desc(Percent)) %>%
  as.data.frame

# Number of samples for each Sampletype for each patient
per_patient_sampletype <- metadata.df %>% 
  group_by(Patient,Sampletype) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count))*100,2)) %>%
  arrange(Patient, desc(Percent)) %>%
  as.data.frame

# Number of samples for each Sampletype_final_refined for each patient
per_patient_sampletype_final_refined <- metadata.df %>% 
  group_by(Patient,Sampletype_final_refined) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count))*100,2)) %>%
  arrange(Patient, desc(Percent)) %>%
  as.data.frame

# Number of samples for each Sampletype for each cohort
per_cohort_sampletype <- metadata.df %>% 
  group_by(Project,Sampletype) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count))*100,2)) %>%
  arrange(Project, desc(Percent)) %>%
  as.data.frame

# Number of samples for each Sampletype_final_refined for each cohort
per_cohort_sampletype_final <- metadata.df %>% 
  group_by(Project,Sampletype_final) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count))*100,2)) %>%
  arrange(Project, desc(Percent)) %>%
  as.data.frame

# Number of samples for each Sampletype_final_refined for each cohort
per_cohort_sampletype_final_refined <- metadata.df %>% 
  group_by(Project,Sampletype_final_refined) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(Percent = round((Count/sum(Count))*100,2)) %>%
  arrange(Project, desc(Percent)) %>%
  as.data.frame


write.csv(per_cohort_sampletype,"Result_tables/other/metadata_cohort_sampletype_counts.csv", row.names = F)
write.csv(per_cohort_sampletype_final,"Result_tables/other/metadata_cohort_sampletype_final_counts.csv", row.names = F)
write.csv(per_cohort_sampletype_final_refined,"Result_tables/other/metadata_cohort_sampletype_final_refined_counts.csv", row.names = F)

# Number of samples and patients for each Number_of_meds
# number_of_meds_summary.df <- 
#   metadata.df %>% 
#   filter(Project == "immunocompromised") %>%
#   group_by(Number_of_meds) %>%
#   dplyr::summarise(N_patients = n_distinct(Patient), N_samples = n_distinct(Index)) %>%
#   as.data.frame()

# write.csv(number_of_meds_summary.df,"Result_tables/other/number_of_meds_summary.csv", row.names = F)

# Number of samples and patients for each Fitzpatrick_skin_type
# fitzpatrick_skin_type_summary.df <- 
#   metadata.df %>% 
#   filter(Project == "immunocompromised") %>%
#   group_by(Fitzpatrick_skin_type) %>%
#   dplyr::summarise(N_patients = n_distinct(Patient), N_samples = n_distinct(Index)) %>%
#   as.data.frame()

# write.csv(fitzpatrick_skin_type_summary.df,"Result_tables/other/Fitzpatrick_skin_type_summary.csv", row.names = F)

# Number of samples and patients for each Patient_group
# patient_group_summary.df <-
#   metadata.df %>% 
#   filter(Project == "immunocompromised") %>%
#   group_by(Patient_group) %>%
#   dplyr::summarise(N_patients = n_distinct(Patient), N_samples = n_distinct(Index)) %>%
#   as.data.frame()

# write.csv(patient_group_summary.df,"Result_tables/other/patient_group_summary.csv", row.names = F)

# ------------------------------------
#           Reads stats

# Rarefied counts                                  = otu_rare_count.m 
# Rarefied relative abundances                     = otu_rare_rel.m
# Relative abundances                              = otu_rel.m
# Counts (filtered)                                = otu.m
# Unfiltered counts                                = otu_unfiltered.m
# Unfiltered relative abundances                   = otu_unfiltered_rel.m
# Counts (with low abundance samples)              = otu_prior_to_removing_low_read_count_samples.m
# Relative abundances (with low abundance samples) = otu_prior_to_removing_low_read_count_samples_rel.m


# Only focus on those samples that have passed QC.
samples_passing_QC <- colnames(otu.m)
samples_in_unfiltered <- colnames(otu_unfiltered.m)
# stats.df <- data.frame(Sample = samples_passing_QC)
stats.df <- data.frame(Sample = samples_in_unfiltered)
rownames(stats.df) <- stats.df$Sample

stats.df$Sample_retained <- "no"
stats.df[samples_passing_QC,]$Sample_retained <- "yes"
samples_not_retained <- rownames(stats.df[stats.df$Sample_retained == "no",])

# stats.df$Patient <- metadata.df[samples_passing_QC,"Patient"]
# stats.df$Project <- metadata.df[samples_passing_QC,"Project"]
# stats.df$Sampletype <- metadata.df[samples_passing_QC,"Sampletype"]
# stats.df$Patient_group <- metadata.df[samples_passing_QC,"Patient_group"]

stats.df$Patient <- metadata.df[samples_in_unfiltered,"Patient"]
stats.df$Project <- metadata.df[samples_in_unfiltered,"Project"]
stats.df$Sampletype <- metadata.df[samples_in_unfiltered,"Sampletype"]
stats.df$Sampletype_final <- metadata.df[samples_in_unfiltered,"Sampletype_final"]
stats.df$Sampletype_final_refined <- metadata.df[samples_in_unfiltered,"Sampletype_final_refined"]
# stats.df$Patient_group <- metadata.df[samples_in_unfiltered,"Patient_group"]

# stats.df[,"Original_read_counts"] <- colSums(otu_unfiltered.m[,samples_passing_QC]) # Read counts prior to filtering out taxonomy / contaminants / low abundance features
stats.df[,"Original_read_counts"] <- colSums(otu_unfiltered.m[,samples_in_unfiltered]) # Read counts prior to filtering out taxonomy / contaminants / low abundance features

stats.df[samples_passing_QC,"Filtered_read_counts"] <- colSums(otu.m[,samples_passing_QC])  # Read counts after filtering (contaminants, low read depth samples and low abundance features removed)
stats.df[samples_not_retained,"Filtered_read_counts"] <- colSums(otu_prior_to_removing_low_read_count_samples.m[,rownames(stats.df[samples_not_retained,])])
stats.df[samples_passing_QC,"Filtered_rarefied_read_counts"] <- colSums(otu_rare_count.m[,samples_passing_QC]) # "" and rarefied

# Reads removed from filtering
stats.df[,"Reads_removed_from_filtering"] <- stats.df[,"Original_read_counts"] - stats.df[,"Filtered_read_counts"]
stats.df[,"Proportion_reads_removed_from_filtering"] <- stats.df[,"Reads_removed_from_filtering"] / stats.df[,"Original_read_counts"]

# Reads removed from filtering and rarefaction
stats.df[,"Reads_removed_from_filtering_rarefied"] <- stats.df[,"Original_read_counts"] - stats.df[,"Filtered_rarefied_read_counts"]
stats.df[,"Proportion_reads_removed_from_filtering_rarefied"] <- stats.df[,"Reads_removed_from_filtering_rarefied"] / stats.df[,"Original_read_counts"]

# ---------------------------------------------
# Read counts and proportions original, unprocessed data. Summing each domain + unassigned should equal one

# Proportion of reads that are mammal (generally human)
# stats.df[,"Mammal_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("Mammal", project_otu_table_unfiltered.df$taxonomy_species),samples_passing_QC])
stats.df[,"Mammal_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("Mammal", project_otu_table_unfiltered.df$taxonomy_species),samples_in_unfiltered])
stats.df[,"Mammal_proportion_original"] <- stats.df[,"Mammal_read_count_original"] / stats.df[,"Original_read_counts"]

# Proportion of reads that are fungal
# stats.df[,"Fungal_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("Fungi", project_otu_table_unfiltered.df$taxonomy_species),samples_passing_QC])
stats.df[,"Fungal_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("Fungi", project_otu_table_unfiltered.df$taxonomy_species),samples_in_unfiltered])
stats.df[,"Fungal_proportion_original"] <- stats.df[,"Fungal_read_count_original"] / stats.df[,"Original_read_counts"]

# Proportion of reads that are fungal
# stats.df[,"Bacterial_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("d__Bacteria", project_otu_table_unfiltered.df$Domain),samples_passing_QC])
stats.df[,"Bacterial_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("d__Bacteria", project_otu_table_unfiltered.df$Domain),samples_in_unfiltered])
stats.df[,"Bacterial_proportion_original"] <- stats.df[,"Bacterial_read_count_original"] / stats.df[,"Original_read_counts"]

# Proportion of reads that are Archaea
# stats.df[,"Archaeal_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("d__Archaea", project_otu_table_unfiltered.df$Domain),samples_passing_QC])
stats.df[,"Archaeal_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("d__Archaea", project_otu_table_unfiltered.df$Domain),samples_in_unfiltered])
stats.df[,"Archaeal_proportion_original"] <- stats.df[,"Archaeal_read_count_original"] / stats.df[,"Original_read_counts"]

# Proportion of reads that are Eukaryota
# stats.df[,"Eukaryal_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("d__Eukaryota", project_otu_table_unfiltered.df$Domain),samples_passing_QC])
stats.df[,"Eukaryal_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("d__Eukaryota", project_otu_table_unfiltered.df$Domain),samples_in_unfiltered])
stats.df[,"Eukaryal_proportion_original"] <- stats.df[,"Eukaryal_read_count_original"] / stats.df[,"Original_read_counts"]

# Proportion of reads that are Unassigned a taxonomy
# stats.df[,"Unassigned_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("Unassigned", project_otu_table_unfiltered.df$Domain),samples_passing_QC])
stats.df[,"Unassigned_read_count_original"] <- colSums(project_otu_table_unfiltered.df[grepl("Unassigned", project_otu_table_unfiltered.df$Domain),samples_in_unfiltered])
stats.df[,"Unassigned_proportion_original"] <- stats.df[,"Unassigned_read_count_original"] / stats.df[,"Original_read_counts"]

# stats.df[,"Other_read_count_original"] <- colSums(project_otu_table_unfiltered.df[!grepl("d__Bacteria|d__Archaea|d__Eukaryota|Unassigned", project_otu_table_unfiltered.df$Domain),samples_passing_QC])
# stats.df[,"Other_proportion_original"] <- stats.df[,"Other_read_count_original"] / stats.df[,"Original_read_counts"]

# min(stats.df[,"Unassigned_proportion_original"] + stats.df[,"Archaeal_proportion_original"] + stats.df[,"Bacterial_proportion_original"]  + stats.df[,"Eukaryal_proportion_original"])

# -------------------------------------
# Read counts and proportions filtered data
# temp <- otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID %in% rownames(otu.m),]
# otu_prior_to_removing_low_read_count_samples.m should be the same as otu.m except the low abundance samples have not been removed
temp <- otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID %in% rownames(otu_prior_to_removing_low_read_count_samples.m),]
bacterial_otus_filtered <- temp[temp$Domain == "d__Bacteria",]$OTU.ID
fungal_otus_filtered <- temp[grepl("Fungi", temp$taxonomy_species),]$OTU.ID
# names(colSums(otu_prior_to_removing_low_read_count_samples.m[!rownames(otu_prior_to_removing_low_read_count_samples.m) %in% rownames(otu.m),])[colSums(otu_prior_to_removing_low_read_count_samples.m[!rownames(otu_prior_to_removing_low_read_count_samples.m) %in% rownames(otu.m),]) > 0]) %in% colnames(otu.m)

# Proportion of reads in the filtered data that are bacterial
# stats.df[samples_passing_QC,"Bacterial_read_count_after_filtering"] <- colSums(otu.m[which(rownames(otu.m) %in% bacterial_otus_filtered),samples_passing_QC])
# temp <- stats.df
stats.df[,"Bacterial_read_count_after_filtering"] <- colSums(otu_prior_to_removing_low_read_count_samples.m[which(rownames(otu_prior_to_removing_low_read_count_samples.m) %in% bacterial_otus_filtered),])
# summary(stats.df[samples_passing_QC,"Bacterial_read_count_after_filtering"] == temp[samples_passing_QC,"Bacterial_read_count_after_filtering"])
stats.df[,"Bacterial_proportion_after_filtering"] <- stats.df[,"Bacterial_read_count_after_filtering"] / stats.df[,"Filtered_read_counts"]

# Proportion of reads in the filtered data that are Fungal
# stats.df[,"Fungal_read_count_after_filtering"] <- colSums(otu.m[which(rownames(otu.m) %in% fungal_otus_filtered),samples_passing_QC])
stats.df[,"Fungal_read_count_after_filtering"] <- colSums(otu_prior_to_removing_low_read_count_samples.m[which(rownames(otu_prior_to_removing_low_read_count_samples.m) %in% fungal_otus_filtered),])
stats.df[,"Fungal_proportion_after_filtering"] <- stats.df[,"Fungal_read_count_after_filtering"] / stats.df[,"Filtered_read_counts"]

# -------------------------------------
# Read counts and proportions filtered + rarefied data
temp <- otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID %in% rownames(otu_rare_count.m),]
bacterial_otus_filtered_rarefied <- temp[temp$Domain == "d__Bacteria",]$OTU.ID
fungal_otus_filtered_rarefied <- temp[grepl("Fungi", temp$taxonomy_species),]$OTU.ID

# Proportion of reads in the filtered data that are bacterial
stats.df[samples_passing_QC,"Bacterial_read_count_after_filtering_rarefied"] <- colSums(otu_rare_count.m[which(rownames(otu_rare_count.m) %in% bacterial_otus_filtered_rarefied),samples_passing_QC])
stats.df[samples_passing_QC,"Bacterial_proportion_after_filtering_rarefied"] <- stats.df[samples_passing_QC,"Bacterial_read_count_after_filtering_rarefied"] / stats.df[samples_passing_QC,"Filtered_rarefied_read_counts"]

# Proportion of reads in the filtered data that are Fungal
stats.df[samples_passing_QC,"Fungal_read_count_after_filtering_rarefied"] <- colSums(otu_rare_count.m[which(rownames(otu_rare_count.m) %in% fungal_otus_filtered_rarefied),samples_passing_QC])
stats.df[samples_passing_QC,"Fungal_proportion_after_filtering_rarefied"] <- stats.df[samples_passing_QC,"Fungal_read_count_after_filtering_rarefied"] / stats.df[samples_passing_QC,"Filtered_rarefied_read_counts"]

# stats.df[,"Fungal_read_count_after_filtering"] <- colSums(project_otu_table.df[grepl("Fungi", project_otu_table.df$taxonomy_species),samples_passing_QC])
# stats.df[,"Fungal_proportion_after_filtering"] <- stats.df[,"Fungal_read_count_after_filtering"] / stats.df[,"Filtered_read_counts"]
# stats.df[,"Bacterial_read_count_after_filtering"] <- colSums(project_otu_table.df[grepl("d__Bacteria", project_otu_table.df$Domain),samples_passing_QC])
# stats.df[,"Bacterial_proportion_after_filtering"] <- stats.df[,"Bacterial_read_count_after_filtering"] / stats.df[,"Filtered_read_counts"]

## ------------------------------------------------------------------------------------
## This will calculate the total number of features across all samples and the number of non-zero features in each sample
# Can either calculate the feature numbers on just samples passing QC
# stats.df[,"Features_total"] <- length(which(rowSums(otu_unfiltered.m[,samples_passing_QC]) > 0 ))
# stats.df[,"Features_original"] <- apply(otu_unfiltered.m[,samples_passing_QC], 2, function(x) { length(which(x > 0)) } )

# Or calculate on all samples
stats.df[,"Features_total"] <- length(which(rowSums(otu_unfiltered.m) > 0 ))
stats.df[,"Features_original"] <- apply(otu_unfiltered.m, 2, function(x) { length(which(x > 0)) } )
## ------------------------------------------------------------------------------------
stats.df[samples_passing_QC,"Features_filtered"] <- apply(otu.m[,samples_passing_QC], 2, function(x) { length(which(x > 0)) } )
stats.df[samples_not_retained,"Features_filtered"] <- apply(otu_prior_to_removing_low_read_count_samples.m[,samples_not_retained], 2, function(x) { length(which(x > 0)) } )
stats.df[samples_passing_QC,"Features_filtered_rarefied"] <- apply(otu_rare_count.m[,samples_passing_QC], 2, function(x) { length(which(x > 0)) } )

stats.df[,"Features_removed_from_filtering"] <- stats.df[,"Features_original"] - stats.df[,"Features_filtered"] 
stats.df[,"Features_removed_from_filtering_rarefied"] <- stats.df[,"Features_original"] - stats.df[,"Features_filtered_rarefied"] 

stats.df[,"Proportion_features_removed_from_filtering"] <- stats.df[,"Features_removed_from_filtering"] / stats.df[,"Features_original"]
stats.df[,"Proportion_features_removed_from_filtering_rarefied"] <- stats.df[,"Features_removed_from_filtering_rarefied"] / stats.df[,"Features_original"]


write.csv(stats.df, "Result_tables/other/QC_summary.csv", row.names = F, quote = F)


# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------
# Above we processed the frequencies for each OTU to calculate the relative abundances.
# However, we often want the abundances at not just the individual OTU level, but also different taxonomy levels.
# For example, we may want to know the abundance of a particular Family.
# Now we will generate the abundance tables at each taxonomy level from Phylum, Class, Order, Family and Genus

# Taxonomy-sample matrix to dataframe convertor
# Just converts a matrix where the row name is a taxonomy label (really can be anything)
m2df <- function(mymatrix, name_of_taxonomy_col = "taxonomy"){
  mydf <- as.data.frame(mymatrix)
  cur_names <- names(mydf)
  mydf[, name_of_taxonomy_col] <- rownames(mydf)
  rownames(mydf) <- NULL
  mydf <- mydf[,c(name_of_taxonomy_col,cur_names)]
  return(mydf)
}

# Merge the otu counts back with the taxonomy data
otu_metadata_merged.df <- merge(otu.df, otu_taxonomy_map.df, by.x = "OTU.ID", by.y = "OTU.ID")
# temp <- left_join(otu.df, otu_taxonomy_map.df, by = "OTU.ID")

# Do the same for the rarefied data
otu_metadata_merged_rare.df <- merge(otu_rare.df, otu_taxonomy_map.df, by.x = "OTU.ID", by.y = "OTU.ID")

# We are then going to create the dataframe for each taxonomy level containing the relative abundances.
# e.g. otu_phylum_rel.df,otu_class_rel.df etc.
# and the counts
# e.g. otu_phylum.df,otu_class.df etc.

# We use the 'full' taxonomy strings, e.g. taxonomy_genus, so that "Unassigned" or "Uncultured" from different lineages are kept separate!
for (tax_string_level in c("taxonomy_species", "taxonomy_genus", "taxonomy_family", "taxonomy_class", "taxonomy_order", "taxonomy_phylum")){
  # Collapse the dataframe by summing the counts for each unique taxonomy string within each sample
  otu_taxa_level.df <- otu_metadata_merged.df[c(tax_string_level, sample_ids)] %>% 
    group_by_(tax_string_level) %>% # Group the dataframe by the taxonomy string 
    # Summarise each group by applying the the 'sum' function to the counts of each member of the group, 
    # i.e. duplicate entries for each taxa level are collapsed into a single entry and their counts summed together
    dplyr::summarise_all(funs(sum)) %>%  
    as.data.frame() # convert back to dataframe
  
  otu_taxa_level_rare.df <- otu_metadata_merged_rare.df[c(tax_string_level, sample_ids)] %>%
    group_by_(tax_string_level) %>%
    dplyr::summarise_all(funs(sum)) %>%
    as.data.frame()

  # Now create the relative abundance matrix at the current taxa level
  otu_taxa_level_rel.m <- otu_taxa_level.df
  rownames(otu_taxa_level_rel.m) <- otu_taxa_level_rel.m[[tax_string_level]]
  otu_taxa_level_rel.m[tax_string_level] <- NULL
  otu_taxa_level_rel.m <- as.matrix(otu_taxa_level_rel.m)
  otu_taxa_level_rel.m <- t(t(otu_taxa_level_rel.m) / colSums(otu_taxa_level_rel.m))
  
  otu_taxa_level_rel_rare.m <- otu_taxa_level_rare.df
  rownames(otu_taxa_level_rel_rare.m) <- otu_taxa_level_rel_rare.m[[tax_string_level]]
  otu_taxa_level_rel_rare.m[tax_string_level] <- NULL
  otu_taxa_level_rel_rare.m <- as.matrix(otu_taxa_level_rel_rare.m)
  otu_taxa_level_rel_rare.m <- t(t(otu_taxa_level_rel_rare.m) / colSums(otu_taxa_level_rel_rare.m))
  
  if (grepl("phylum", tax_string_level)){
    # otu_phylum_rel.m <- otu_taxa_level_rel.m
    otu_phylum_rel.df <- m2df(otu_taxa_level_rel.m, tax_string_level)
    otu_phylum.df <- otu_taxa_level.df
    otu_phylum_rel_rare.df <- m2df(otu_taxa_level_rel_rare.m, tax_string_level)
    otu_phylum_rare.df <- otu_taxa_level_rare.df
  } 
  else if (grepl("class", tax_string_level)){
    # otu_class_rel.m <- otu_taxa_level_rel.m
    otu_class_rel.df <- m2df(otu_taxa_level_rel.m, tax_string_level)
    otu_class.df <- otu_taxa_level.df
    otu_class_rel_rare.df <- m2df(otu_taxa_level_rel_rare.m, tax_string_level)
    otu_class_rare.df <- otu_taxa_level_rare.df
  }
  else if (grepl("order", tax_string_level)){
    # otu_order_rel.m <- otu_taxa_level_rel.m
    otu_order_rel.df <- m2df(otu_taxa_level_rel.m, tax_string_level)
    otu_order.df <- otu_taxa_level.df
    otu_order_rel_rare.df <- m2df(otu_taxa_level_rel_rare.m, tax_string_level)
    otu_order_rare.df <- otu_taxa_level_rare.df
  }
  else if (grepl("family", tax_string_level)){
    # otu_family_rel.m <- otu_taxa_level_rel.m
    otu_family_rel.df <- m2df(otu_taxa_level_rel.m, tax_string_level)
    otu_family.df <- otu_taxa_level.df
    otu_family_rel_rare.df <- m2df(otu_taxa_level_rel_rare.m, tax_string_level)
    otu_family_rare.df <- otu_taxa_level_rare.df
  }
  else if (grepl("genus", tax_string_level)){
    # otu_genus_rel.m <- otu_taxa_level_rel.m
    otu_genus_rel.df <- m2df(otu_taxa_level_rel.m, tax_string_level)
    otu_genus.df <- otu_taxa_level.df
    otu_genus_rel_rare.df <- m2df(otu_taxa_level_rel_rare.m, tax_string_level)
    otu_genus_rare.df <- otu_taxa_level_rare.df
  }
  else if (grepl("species", tax_string_level)){
    # otu_species_rel.m <- otu_taxa_level_rel.m
    otu_species_rel.df <- m2df(otu_taxa_level_rel.m, tax_string_level)
    otu_species.df <- otu_taxa_level.df
    otu_species_rel_rare.df <- m2df(otu_taxa_level_rel_rare.m, tax_string_level)
    otu_species_rare.df <- otu_taxa_level_rare.df
  }
}


### Write the final counts and abundances for each taxonomy level to file
# Not rarefied
write.table(otu_species.df, file = "Result_tables/count_tables/Specie_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_species_rel.df, file = "Result_tables/relative_abundance_tables/Specie_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_genus.df, file = "Result_tables/count_tables/Genus_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_genus_rel.df, file = "Result_tables/relative_abundance_tables/Genus_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_family.df, file = "Result_tables/count_tables/Family_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_family_rel.df, file = "Result_tables/relative_abundance_tables/Family_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_order.df, file = "Result_tables/count_tables/Order_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_order_rel.df, file = "Result_tables/relative_abundance_tables/Order_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_class.df, file = "Result_tables/count_tables/Class_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_class_rel.df, file = "Result_tables/relative_abundance_tables/Class_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_phylum.df, file = "Result_tables/count_tables/Phylum_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_phylum_rel.df, file = "Result_tables/relative_abundance_tables/Phylum_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

# Rarefied
write.table(otu_species_rare.df, file = "Result_tables/count_tables/Specie_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_species_rel_rare.df, file = "Result_tables/relative_abundance_tables/Specie_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_genus_rare.df, file = "Result_tables/count_tables/Genus_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_genus_rel_rare.df, file = "Result_tables/relative_abundance_tables/Genus_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_family_rare.df, file = "Result_tables/count_tables/Family_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_family_rel_rare.df, file = "Result_tables/relative_abundance_tables/Family_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_order_rare.df, file = "Result_tables/count_tables/Order_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_order_rel_rare.df, file = "Result_tables/relative_abundance_tables/Order_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_class_rare.df, file = "Result_tables/count_tables/Class_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_class_rel_rare.df, file = "Result_tables/relative_abundance_tables/Class_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_phylum_rare.df, file = "Result_tables/count_tables/Phylum_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(otu_phylum_rel_rare.df, file = "Result_tables/relative_abundance_tables/Phylum_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

# ------------------------------------------------------------------------------------------------------------------------------
# Finally create and save a dataframe, separately for each Phylum, Class, Order, Family, Genus, Species and OTU,
# containing the abundances/counts/log(counts, 10) in each sample, metadata and taxonomy information.

# Include both the count, log(count), abundance, rarefied count, log(rarefied count) and rarefied abundance.

# There should four tables at this point each level with the same entries, e.g.
# Counts = otu.df
# Relative abundance = otu_rel.df
# Rarified counts = otu_rare.df
# Rarified relative abundance = otu_rel_rare.df

# df2matrix <- function(mydf, rowname_col = 1){
#   temp <- mydf[,c(1:length(names(mydf)))[c(1:length(names(mydf))) != rowname_col]]
#   mymatrix <- as.matrix(temp)
#   rownames(mymatrix) <- mydf[,rowname_col]
#   # mymatrix[,rowname_col] <- NULL
#   return(mymatrix)
# }

# Make row names of a dataframe the value of a defined column (default 1st) and then remove the column
clean_dataframe <- function(mydf, rowname_col = 1){
  my_clean.df <- mydf
  rownames(my_clean.df) <- my_clean.df[,rowname_col]
  my_clean.df[,1] <- NULL
  return(my_clean.df)
}

create_combined_dataframe <- function(counts.df, counts_rare.df, abundances.df, abundances_rare.df, mymetadata, mylevel = "OTU", otu_map.df = NULL){
  counts <- clean_dataframe(counts.df)
  rel_abundances <- clean_dataframe(abundances.df)
  counts_rare <- clean_dataframe(counts_rare.df)
  rel_abundances_rare <- clean_dataframe(abundances_rare.df)
  
  # Ensure ordering is the same
  rel_abundances <- rel_abundances[rownames(counts),]
  counts_rare <- counts_rare[rownames(counts),]
  rel_abundances_rare <- rel_abundances_rare[rownames(counts),]
  
  # Combine the datasets. Passing as.matrix(counts) captures the rownames as a column. This can be renamed after
  combined_data <- cbind(melt(as.matrix(counts), variable.name = "sample", value.name = "Read_count"),
                         melt(rel_abundances, value.name = "Relative_abundance")[,2, drop = F],
                         melt(counts_rare, value.name = "Read_count_rarefied")[,2, drop = F],
                         melt(rel_abundances_rare, value.name = "Relative_abundance_rarefied")[,2, drop = F])
  
  # Remove samples with a read count of zero
  combined_data <- combined_data[combined_data$Read_count > 0,]
  
  # Calculate logged read counts
  combined_data$Read_count_logged <- log(combined_data$Read_count, 10)
  combined_data$Read_count_rarefied_logged <- log(combined_data$Read_count_rarefied, 10)
  
  # Fix the Var2 column
  names(combined_data)[2] <- "Sample"
  
  # Merge with metadata. Assumes an Index column matching Sample
  combined_data <- merge(combined_data, mymetadata, by.x = "Sample", by.y = "Index")
  
  if (mylevel == "OTU.ID"){
    names(combined_data)[names(combined_data) == "Var1"] <- "OTU.ID"
    combined_data <- merge(combined_data, otu_map.df, by.x = "OTU.ID", by.y = "OTU.ID")
  }
  else if (mylevel == "Species"){
    names(combined_data)[names(combined_data) == "Var1"] <- "taxonomy_species"
    otu_map_reduced.df <- unique(otu_map.df[,c("Domain","Phylum", "Class", "Order", "Family", "Genus", "Species", "taxonomy_species")])
    combined_data <- merge(combined_data, otu_map_reduced.df, by.x = "taxonomy_species", by.y = "taxonomy_species")
  }
  else if (mylevel == "Genus"){
    names(combined_data)[names(combined_data) == "Var1"] <- "taxonomy_genus"
    otu_map_reduced.df <- unique(otu_map.df[,c("Domain","Phylum", "Class", "Order", "Family", "Genus", "taxonomy_genus")])
    combined_data <- merge(combined_data, otu_map_reduced.df, by.x = "taxonomy_genus", by.y = "taxonomy_genus")
  }
  else if (mylevel == "Family"){
    names(combined_data)[names(combined_data) == "Var1"] <- "taxonomy_family"
    otu_map_reduced.df <- unique(otu_map.df[,c("Domain","Phylum", "Class", "Order", "Family", "taxonomy_family")])
    combined_data <- merge(combined_data, otu_map_reduced.df, by.x = "taxonomy_family", by.y = "taxonomy_family")
  }
  else if (mylevel == "Order"){
    names(combined_data)[names(combined_data) == "Var1"] <- "taxonomy_order"
    otu_map_reduced.df <- unique(otu_map.df[,c("Domain","Phylum", "Class", "Order", "taxonomy_order")])
    combined_data <- merge(combined_data, otu_map_reduced.df, by.x = "taxonomy_order", by.y = "taxonomy_order")
  }
  else if (mylevel == "Class"){
    names(combined_data)[names(combined_data) == "Var1"] <- "taxonomy_class"
    otu_map_reduced.df <- unique(otu_map.df[,c("Domain","Phylum", "Class", "taxonomy_class")])
    combined_data <- merge(combined_data, otu_map_reduced.df, by.x = "taxonomy_class", by.y = "taxonomy_class")
  }
  else if (mylevel == "Phylum"){
    names(combined_data)[names(combined_data) == "Var1"] <- "taxonomy_phylum"
    otu_map_reduced.df <- unique(otu_map.df[,c("Domain","Phylum", "taxonomy_phylum")])
    combined_data <- merge(combined_data, otu_map_reduced.df, by.x = "taxonomy_phylum", by.y = "taxonomy_phylum")
  }
  return(combined_data)
}

reduced_tax_map <- otu_taxonomy_map.df
reduced_tax_map$RepSeq <- NULL

otu_combined <- create_combined_dataframe(counts.df = otu.df, 
                                          counts_rare.df = otu_rare.df,
                                          abundances.df = otu_rel.df,
                                          abundances_rare.df = otu_rel_rare.df,
                                          mylevel = "OTU.ID",
                                          mymetadata = metadata.df,
                                          otu_map.df = reduced_tax_map)

species_combined <- create_combined_dataframe(counts.df = otu_species.df, 
                                              counts_rare.df = otu_species_rare.df,
                                              abundances.df = otu_species_rel.df,
                                              abundances_rare.df = otu_species_rel_rare.df,
                                              mylevel = "Species",
                                              mymetadata = metadata.df,
                                              otu_map.df = reduced_tax_map)

genus_combined <- create_combined_dataframe(counts.df = otu_genus.df, 
                                            counts_rare.df = otu_genus_rare.df,
                                            abundances.df = otu_genus_rel.df,
                                            abundances_rare.df = otu_genus_rel_rare.df,
                                            mylevel = "Genus",
                                            mymetadata = metadata.df,
                                            otu_map.df = reduced_tax_map)

family_combined <- create_combined_dataframe(counts.df = otu_family.df, 
                                             counts_rare.df = otu_family_rare.df,
                                             abundances.df = otu_family_rel.df,
                                             abundances_rare.df = otu_family_rel_rare.df,
                                             mylevel = "Family",
                                             mymetadata = metadata.df,
                                             otu_map.df = reduced_tax_map)

order_combined <- create_combined_dataframe(counts.df = otu_order.df, 
                                            counts_rare.df = otu_order_rare.df,
                                            abundances.df = otu_order_rel.df,
                                            abundances_rare.df = otu_order_rel_rare.df,
                                            mylevel = "Order",
                                            mymetadata = metadata.df,
                                            otu_map.df = reduced_tax_map)

class_combined <- create_combined_dataframe(counts.df = otu_class.df, 
                                            counts_rare.df = otu_class_rare.df,
                                            abundances.df = otu_class_rel.df,
                                            abundances_rare.df = otu_class_rel_rare.df,
                                            mylevel = "Class",
                                            mymetadata = metadata.df,
                                            otu_map.df = reduced_tax_map)

phylum_combined <- create_combined_dataframe(counts.df = otu_phylum.df, 
                                             counts_rare.df = otu_phylum_rare.df,
                                             abundances.df = otu_phylum_rel.df,
                                             abundances_rare.df = otu_phylum_rel_rare.df,
                                             mylevel = "Phylum",
                                             mymetadata = metadata.df,
                                             otu_map.df = reduced_tax_map)

write.table(otu_combined, file = "Result_tables/other/OTU_counts_abundances_and_metadata.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(species_combined, file = "Result_tables/other/Specie_counts_abundances_and_metadata.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(genus_combined, file = "Result_tables/other/Genus_counts_abundances_and_metadata.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(family_combined, file = "Result_tables/other/Family_counts_abundances_and_metadata.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(order_combined, file = "Result_tables/other/Order_counts_abundances_and_metadata.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(class_combined, file = "Result_tables/other/Class_counts_abundances_and_metadata.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(phylum_combined, file = "Result_tables/other/Phylum_counts_abundances_and_metadata.csv", sep = ",", quote = F, col.names = T, row.names = F)

