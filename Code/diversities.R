# Diversity calculations (Shannon, Chao1, Simpson) for each sample.
# Significance tests of each discrete group comparing diversity indices.
# Generate boxplots for discrete 
# Comparison of continuous variables vs diversity indices

library(vegan)
library(reshape2)
library(dplyr)
library(ggplot2)
library(FSA)
#install.packages("FSA")

# source('http://bioconductor.org/biocLite.R')
# biocLite('phyloseq')
library(phyloseq)
library(nlme)


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

############################################################
# Various colour palletes
my_colour_pallete <- c("#8dd3c7","#ffffb3","#bebada","#fb8072", "#80b1d3", "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5", "#cc0000")
# From http://tools.medialab.sciences-po.fr/iwanthue/
my_colour_pallete_20 <- c("#66bd79","#a35bcf","#5bb643","#d14ea6","#a2b239","#5c6bcc","#dc892e","#5e93cd","#d64737","#49b6a8","#dc3c6e","#4f7e3c","#bd8cd5","#caab55","#914c88","#867230","#df82a2","#a65429","#ab4a5a","#e0896a")
my_colour_pallete_20_distinct <- c("#0057b4","#7fff56","#d600bc","#d8d500","#e76eff","#019932","#9f8fff","#ffc730","#007fac","#a20019","#06fefd","#ff6782","#00774c","#e0c8ff","#717a00","#4b2952","#e2ed7d","#46321e","#ffbd76","#ffb4c6")
my_colour_pallete_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_pallete_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
my_colour_pallete_15 <- c("#77b642","#7166d9","#cfa240","#b351bb","#4fac7f","#d44891","#79843a","#c68ad4","#d15a2c","#5ba7d9","#ce4355","#6570ba","#b67249","#9b4a6f","#df8398")
my_colour_pallete_32_distinct <- c("#ea7e00","#ca0074","#d1c69b","#474007","#bb00ad","#9c80ff","#be3300","#542e72","#00b9f5","#09436b","#8b0036","#9ac8e6","#ff1059","#959eff","#154a11","#0290f4","#ff7762","#7dbf00","#ff8194","#834c00","#006e73","#f9bb5d","#d6c943","#017229","#00d3a8","#732427","#36e191","#6a8200","#efb3ea","#3227bb","#ff90e1","#e92a12")
# lesion_pallete_7 <- c("#8558d6","#6ee268","#d247ad","#c9d743","#d7453e","#59a237","#d78f2a")
# patient_pallete_45 <- c("#d64530","#585fb1","#795d97","#9e4773","#3f6921","#71692c","#a2b93c","#d571cc","#9b3e97","#33947a","#98ad66","#448a4e","#869ae0","#5ce7af","#e085a3","#dfdc87","#d19be2","#5cb735","#e38269","#3db6c0","#50b565","#50902c","#a98a2c","#dde84a","#db3d76","#5fe485","#7c8329","#b3e791","#6fe965","#5ebce9","#3c86c1","#2a6a45","#65b688","#6651d1","#af4ed3","#df872f","#56e4db","#737cea","#ac464b","#dd37b5","#995b2b","#daac6f","#92e2be","#a2e24b","#e0be3a")
patient_pallete_270 <- c("#456c00","#77bbff","#75fb3f","#273300","#f5006f","#ac008b","#125700","#ffef77","#00278e","#3d005e","#d84100","#015686","#01289f","#ff8c4c","#0070b5","#8015cd","#feffd6","#02d8aa","#019cf4","#4f2f00","#bbffe9","#c52900","#1b002c","#a3ac00","#5d9200","#f29fff","#231500","#934cfc","#988a00","#002cbb","#ffeb36","#ffa758","#f1f9ff","#000045","#b4004b","#602900","#390048","#e6c400","#00ce3c","#ff7bd0","#8cff56","#e60051","#b89aff","#00474b","#d5fbff","#ff79c2","#1d0016","#00635d","#ff8e33","#992300","#ff6e91","#ffa081","#534a00","#61002d","#ffe1c1","#8c0072","#00405d","#89ffc6","#607500","#64ff6f","#002e52","#9b97ff","#b1ccff","#02c5cd","#5dffba","#beff45","#00112b","#b8ff74","#7f0027","#0074cd","#005c6f","#3f00af","#dd7900","#cced00","#77ffd6","#ffc5b5","#99ffb1","#01ea72","#f0ff88","#007f63","#abff9d","#391200","#003a74","#114b00","#0a0100","#ff5fff","#ffccfb","#00d6b7","#c7ff93","#1efa5a","#005437","#f6af00","#a60024","#ffb7e6","#ea0043","#c7ffbc","#72ab00","#789300","#585500","#c3ff14","#00f398","#ab4a00","#9b7600","#85e5ff","#006235","#130020","#006825","#ff735c","#007a7f","#02a3a4","#4856ff","#bf52ff","#00edbc","#a31bd6","#009642","#e93bee","#e400ae","#ffbdd2","#00cfc7","#f1ffaa","#009b7a","#dd00c9","#ff697d","#004a14","#ff72ac","#ff3d1f","#fffaa3","#5d0400","#027ba4","#01c774","#002655","#00941f","#0a32d7","#82acff","#ff8af3","#ff4165","#001104","#ffd6f2","#efebff","#aebc00","#3e0030","#c5abff","#00402e","#ff4bae","#0275f1","#be89ff","#ffd899","#00c765","#01b208","#97ffd4","#7e9fff","#00fde1","#0050c9","#ff8eb5","#c800cd","#005173","#ff2b95","#76ff7a","#ea0074","#001d70","#009856","#f100a8","#ba6b00","#0293df","#00462d","#ff6862","#f6ff65","#02bbda","#2c2200","#01a876","#e35a00","#e3000f","#ff819e","#5a0039","#a558ff","#e2ffb2","#784800","#016def","#b400a2","#00143c","#00212d","#403d00","#ff75fe","#975300","#166c00","#260008","#917fff","#ff8d89","#01bf7a","#ffa6bf","#800086","#90a100","#cce4ff","#dad800","#52c900","#46a700","#0c0039","#0b0052","#79009d","#003c85","#bb0034","#59e7ff","#af0064","#64001e","#c0007e","#000897","#bd8400","#2b007f","#318400","#31f1ff","#7c8600","#807300","#ffc072","#6f005f","#770040","#e62c00","#2e0019","#005599","#6535e1","#5b0099","#006bd5","#0142a1","#baaf00","#00ab2d","#ffcc40","#edffec","#ef0031","#153100","#abe9ff","#6bbd00","#e5ff4e","#ffdb43","#ffa5ef","#01c4f3","#ffbd8f","#84d100","#bbff84","#9fcdff","#7b0059","#ffe897","#ff8711","#ffa869","#febaff","#20003a","#94002b","#5387ff","#756dff","#fff494","#a5c1ff","#e0ffcf","#002417","#530076","#ff8459","#ffe4ec","#00b650","#0119b7","#c963ff","#a2ff64","#9c6800","#03b6f8","#00a0c2","#00240b","#6297ff","#bd0010","#fff7af","#7d2d00","#cf7aff","#af5600","#322c00","#500028")
my_colour_pallete_10_distinct <- c("#8eec45","#0265e8","#f6a800","#bf6549","#486900","#c655a0","#00d1b6","#ff4431","#aeb85c","#7e7fc8")

# ******************************************************************************************
# Taken from : https://jacobrprice.github.io/2017/08/26/phyloseq-to-vegan-and-back.html
# convert the sample_data() within a phyloseq object to a vegan compatible data object
pssd2veg <- function(physeq) {
  sd <- sample_data(physeq)
  return(as(sd,"data.frame"))
}

# convert the otu_table() within a phyloseq object to a vegan compatible data object
psotu2veg <- function(physeq) {
  OTU <- otu_table(physeq)
  if (taxa_are_rows(OTU)) {
    OTU <- t(OTU)
  }
  return(as(OTU, "matrix"))
}

# move an OTU table from vegan to phyloseq  
# otu_table(PhyloseqObject) <- otu_table(veganOTUobject, taxa_are_rows=TRUE)  
# move sample data from vegan to phyloseq
# sample_data(PhyloseqObject) <- as.data.frame(veganSampleDataObject)


# Assumes metric is a column, e.g Mean_relative_abundance
generate_diversity_boxplot <- function(mymetadata, variable, metric, fill_pallete = my_colour_pallete_206_distinct, variable_colours_available = F){
  internal_metadata <- mymetadata[!is.na(mymetadata[variable]),]
  variable_values <- factor(as.character(unique(internal_metadata[[variable]])))
  if (variable_colours_available == T){
    color_col_name <- paste0(variable, "_colour")
    variable_colours <- setNames(as.character(unique(internal_metadata[[color_col_name]])), as.character(unique(internal_metadata[[variable]])))
  } else{
    variable_colours <- setNames(fill_pallete[1:length(variable_values)], variable_values)  
  }
  
  myplot <- ggplot(internal_metadata, aes(x = get(variable),
                                          y = get(metric), 
                                          fill = factor(get(variable)))) +
    stat_boxplot(geom = "errorbar", 
                 position = position_dodge(width = 0.75, preserve = "single"), 
                 size = .2, 
                 # width = .5, 
                 linetype= "dashed") + 
    
    geom_boxplot(outlier.shape = 1,
                 outlier.size = .5,
                 position = position_dodge(width = 0.75, preserve = "single"), 
                 aes(ymin=..lower.., 
                     ymax=..upper..,
                     colour = factor(get(variable))), 
                 coef = 0, 
                 size = .2) + 
    
    geom_boxplot(outlier.shape = NA,
                 # outlier.size = .5,
                 position = position_dodge(width = 0.75, preserve = "single"), 
                 aes(ymin=..lower.., 
                     ymax=..upper..), 
                 coef = 0, 
                 size = .2) +
    # geom_point()+
    scale_color_manual(values = variable_colours, name = variable) +
    scale_fill_manual(values = variable_colours, name = variable) +
    # xlab(gsub("_", " ", variable)) +
    xlab(variable) +
    ylab(metric) +
    coord_flip() +
    common_theme
  # theme(
  #   axis.text.x = element_text(angle = 90),
  #   axis.text.y = element_text(size = axis_text_y),
  # )
  return(myplot)
}

# ******************************************************************************************

# --------------------------------------------------------------------------------
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Remove negative samples
metadata.df <- metadata.df[!metadata.df$Sampletype == "negative",]

# Set order of variables
metadata.df$Sampletype_pooled <- factor(metadata.df$Sampletype_pooled, levels = c("LC", "AK", "SCC"))
metadata.df$Sampletype_compromised_refined <- factor(metadata.df$Sampletype_compromised_refined, levels = c("C", "LC", "AK", "SCC"))
metadata.df$Sampletype_final <- factor(metadata.df$Sampletype_final, levels = c("C", "LC", "AK", "SCC"))
metadata.df$Patient_group <- factor(metadata.df$Patient_group, levels = c("Control", "AK", "SCC"))
metadata.df$Number_of_meds <- factor(metadata.df$Number_of_meds, levels = c("1", "2", "3"))
metadata.df$Fitzpatrick_skin_type <- factor(metadata.df$Fitzpatrick_skin_type, levels = c("1", "2", "3", "4"))

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# Load the count matrices
otu_rare.m <- as.matrix(read.csv("Result_tables/count_tables/OTU_counts_rarefied.csv", row.names =  1))
otu.m <- as.matrix(read.csv("Result_tables/count_tables/OTU_counts.csv", row.names =  1))

# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
metadata.df <- metadata.df[rownames(metadata.df) %in%colnames(otu_rare.m),]

# Only want immunocompromised and the snapshot immunocompetent samples
metadata.df <- subset(metadata.df, Project == "immunocompromised" | Snapshot_sample == "yes")

# Remove samples that are not in the metadata.
otu_rare.m <- otu_rare.m[,colnames(otu_rare.m) %in% rownames(metadata.df)]
otu.m <- otu.m[,colnames(otu.m) %in% rownames(metadata.df)]

# Create the true rare matrix for comparison
otu_true_rare.m <- t(rrarefy(t(otu.m[,colSums(otu.m) >= 4000]), 4000))

# Order the same as the metadata
# otu_shannon_diversity <- otu_shannon_diversity[rownames(metadata.df),,drop =F]

# Add diversity values to metadata dataframe
# metadata.df$Shannon_diversity <- otu_shannon_diversity$value


# Define the discrete variables
# For both cohorts
discrete_variables <- c("Project","Patient","Sampletype_final")
# For immunocompromised
discrete_variables_immunocompromised <- c("Number_of_meds", "Patient_group", "Fitzpatrick_skin_type")

# create phyloseq object
otu_rare_phyloseq <- otu_table(otu_rare.m, taxa_are_rows=TRUE)
otu_true_rare_phyloseq <- otu_table(otu_true_rare.m, taxa_are_rows=TRUE)

# Calculate number of unique features/OTUs per sample
temp <- otu_rare.m
temp[temp > 0] <- 1
temp <- melt(colSums(temp))
metadata.df$Number_of_features <- temp[rownames(metadata.df), ,]

# Estimate alpha diversities
otu_rare_alpha.df <- estimate_richness(otu_rare_phyloseq, measures = c("Chao1", "Simpson","Shannon"))
otu_rare_alpha.df <- otu_rare_alpha.df[rownames(metadata.df),]
otu_true_rare_alpha.df <- estimate_richness(otu_true_rare_phyloseq, measures = c("Chao1", "Simpson","Shannon"))
otu_true_rare_alpha.df <- otu_true_rare_alpha.df[rownames(metadata.df),]

# Combine the metadata and the diversity metrics into a single dataframe
full=cbind(metadata.df, otu_rare_alpha.df)
full_true_rare=cbind(metadata.df, otu_true_rare_alpha.df)

# Create combined variables
# full$Project_Sampletype_pooled <- with(full, paste0(Project, "_",Sampletype_pooled))
# full_true_rare$Project_Sampletype_pooled <- with(full_true_rare, paste0(Project, "_",Sampletype_pooled))
full$Project_Sampletype_final <- factor(with(full, paste0(Project, "_",Sampletype_final)))
full_true_rare$Project_Sampletype_final <- factor(with(full_true_rare, paste0(Project, "_",Sampletype_final)))

# EDIT 11/7/19 : Just use truly rarefied data
full <- full_true_rare

# Cohort specific data 
immunocompetent_data.df <- subset(full, Project == "immunocompetent")
immunocompromised_data.df <- subset(full, Project == "immunocompromised")
# -----------------------------------------------------------------------------------------------------------------
# Create a subsampled dataset. Ensure the same number of patients and the same number of samples.
# Try and make the selected samples well distributed, i.e. try and not bias a single patient

# FIRST IMMUNOCOMPROMISED
# Get the group with the lowest number of patients
lowest_patient_group.df <- immunocompromised_data.df %>% 
  group_by(Sampletype_final) %>% 
  select(Sampletype_final, Patient) %>% 
  distinct() %>% 
  tally() %>% 
  summarise(Sampletype_final = Sampletype_final[which.min(n)], Count = min(n)) %>% 
  as.data.frame()

# Get the group with the lowest number of samples
lowest_sample_group.df <- immunocompromised_data.df %>% 
  group_by(Sampletype_final) %>% 
  select(Sampletype_final, Index) %>% 
  distinct() %>% 
  tally() %>% 
  summarise(Sampletype_final = Sampletype_final[which.min(n)], Count = min(n)) %>% 
  as.data.frame()

# 9 patients, 17 samples

# We know that the number of patients is always lower than sample number for each group, 
lowest_patient_group.df$Count
lowest_sample_group.df$Count

# This is a horrible approach. Randomly sample samples until the number of unique patients
# is the minimum. To have the sample number of samples, we have to limit to 15 samples
set.seed(1234)
immunocompromised_down_sampled.df <- data.frame()
for (sf in unique(immunocompromised_data.df$Sampletype_final)){
  full_data_subset_sf.df <- subset(immunocompromised_data.df, Sampletype_final == sf)
  N_patients <- length(unique(full_data_subset_sf.df$Patient))
  N_samples <- length(unique(full_data_subset_sf.df$Index))
  iterations <- 0
  if (sf == "C"){
    while ( N_patients != lowest_patient_group.df$Count | N_samples != lowest_sample_group.df$Count-5){
      temp_downsampled <- full_data_subset_sf.df %>% sample_n(lowest_sample_group.df$Count-5)
      N_patients <- length(unique(temp_downsampled$Patient))
      N_samples <- length(unique(temp_downsampled$Index))
    }
  }
  while ( N_patients != lowest_patient_group.df$Count){
    temp_downsampled <- full_data_subset_sf.df %>% sample_n(lowest_sample_group.df$Count-5)
    N_patients <- length(unique(temp_downsampled$Patient))
    N_samples <- length(unique(temp_downsampled$Index))
    iterations <- sum(iterations, 1)
    # print(iterations)
    # print(N_patients)
    # print(N_samples)
  }
  immunocompromised_down_sampled.df <- rbind(immunocompromised_down_sampled.df, temp_downsampled)
}
# immunocompromised_down_sampled.df %>%
#   group_by(Sampletype_final) %>%
#   select(Sampletype_final, Index) %>%
#   distinct() %>%
#   tally() %>%
#   as.data.frame()
# 9 patients, 17 samples


# NOW IMMUNOCOMPETENT
lowest_patient_group.df <- immunocompetent_data.df %>% 
  group_by(Sampletype_final) %>% 
  select(Sampletype_final, Patient) %>% 
  distinct() %>% 
  tally() %>% 
  summarise(Sampletype_final = Sampletype_final[which.min(n)], Count = min(n)) %>% 
  as.data.frame()

# Get the group with the lowest number of samples
lowest_sample_group.df <- immunocompetent_data.df %>% 
  group_by(Sampletype_final) %>% 
  select(Sampletype_final, Index) %>% 
  distinct() %>% 
  tally() %>% 
  summarise(Sampletype_final = Sampletype_final[which.min(n)], Count = min(n)) %>% 
  as.data.frame()

# We know that the number of patients is always lower than sample number for each group, 
lowest_patient_group.df$Count
lowest_sample_group.df$Count
# 10 31

# This is a horrible approach. Randomly sample samples until the number of unique patients
# is the minimum. To have the sample number of samples, we have to limit to 31 samples
set.seed(1234)
immunocompetent_down_sampled.df <- data.frame()
for (sf in unique(immunocompetent_data.df$Sampletype_final)){
  full_data_subset_sf.df <- subset(immunocompetent_data.df, Sampletype_final == sf)
  N_patients <- length(unique(full_data_subset_sf.df$Patient))
  N_samples <- length(unique(full_data_subset_sf.df$Index))
  # print(paste0(sf, " ", N_patients, " ", N_samples, " 1"))
  iterations <- 0
  if (sf %in% c("SCC", "AK")){
    while ( N_patients != lowest_patient_group.df$Count | N_samples != lowest_sample_group.df$Count-1){
      
      temp_downsampled <- full_data_subset_sf.df %>% sample_n(lowest_sample_group.df$Count-1)
      N_patients <- length(unique(temp_downsampled$Patient))
      N_samples <- length(unique(temp_downsampled$Index))
      # print(paste0(sf, " ", N_patients, " ", N_samples, " 2"))
    }
  }
  while ( N_patients != lowest_patient_group.df$Count){
    # print(sf) 
    temp_downsampled <- full_data_subset_sf.df %>% sample_n(lowest_sample_group.df$Count-1)
    N_patients <- length(unique(temp_downsampled$Patient))
    N_samples <- length(unique(temp_downsampled$Index))
    iterations <- sum(iterations, 1)
    # print(paste0(sf, " ", N_patients, " ", N_samples, " 3"))
    # print(iterations)
    # print(N_patients)
    # print(N_samples)
  }
  immunocompetent_down_sampled.df <- rbind(immunocompetent_down_sampled.df, temp_downsampled)
}

immunocompetent_down_sampled.df %>%
  group_by(Sampletype_final) %>%
  select(Sampletype_final, Index) %>%
  distinct() %>%
  tally() %>%
  as.data.frame()

immunocompetent_down_sampled.df %>%
  group_by(Sampletype_final) %>%
  select(Sampletype_final, Patient) %>%
  distinct() %>%
  tally() %>%
  as.data.frame()
# 10 patients, 30 samples

full_downsampled.df <- rbind(immunocompetent_down_sampled.df, immunocompromised_down_sampled.df)

# -----------------------------------------------------------------------------------------------------------------

# ------------------------------------------
# Generate plots based on diversity values and metadata

generate_diversity_boxplot_2 <- function(mydata, variable, metric, variable_colours_available = T){
  internal_data.df <- mydata[!is.na(mydata[variable]),]
  variable_values <- factor(as.character(unique(internal_data.df[[variable]])))
  if (variable_colours_available == T){
    color_col_name <- paste0(variable, "_colour")
    variable_colours <- setNames(as.character(unique(internal_data.df[[color_col_name]])), as.character(unique(internal_data.df[[variable]])))
  } else{
    variable_colours <- setNames(my_colour_pallete_206_distinct[1:length(variable_values)], variable_values)  
  }
  myplot <- ggplot(internal_data.df, aes(x = get(variable), y = get(metric))) +
    geom_boxplot(outlier.shape = NA, aes(fill = get(variable))) +
    scale_fill_manual(values = variable_colours, name = variable) +
    geom_jitter(size=0.5, width = 0.10, height=0) +
    guides(fill=FALSE) +
    # scale_y_continuous(limits = c(0,4.5), breaks = seq(0,4.5,.5)) +
    xlab("") +
    ylab(metric)  +
    common_theme +
    theme(panel.border = element_blank(), 
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
          plot.title = element_text(size = 6))
  myplot
}

# For each cohort (Project)
myplot <- generate_diversity_boxplot_2(full, "Project", "Chao1", variable_colours_available = T)
ggsave(filename = paste0("Result_figures/diversity_analysis/Project_Chao1.pdf"),myplot, width = 9, height = 8,units = "cm")
myplot <- generate_diversity_boxplot_2(full, "Project", "Shannon", variable_colours_available = T)
ggsave(filename = paste0("Result_figures/diversity_analysis/Project_Shannon.pdf"),myplot, width = 9, height = 8,units = "cm")
myplot <- generate_diversity_boxplot_2(full, "Project", "Simpson", variable_colours_available = T)
ggsave(filename = paste0("Result_figures/diversity_analysis/Project_Simpson.pdf"),myplot, width =9, height = 8,units = "cm")

# myplot <- generate_diversity_boxplot_2(full_true_rare, "Project", "Chao1", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/Project_Chao1_true_rare.pdf"),myplot, width = 9, height = 8,units = "cm")


# For each Sampletype_pooled (cohorts combined)
# myplot <- generate_diversity_boxplot_2(full, "Sampletype_pooled", "Chao1", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts_sampletype_pooled_Chao1.pdf"),myplot, width = 8, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full, "Sampletype_pooled", "Shannon", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts_sampletype_pooled_Shannon.pdf"),myplot, width = 8, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full, "Sampletype_pooled", "Simpson", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts_sampletype_pooled_Simpson.pdf"),myplot, width = 8, height = 8,units = "cm")

# For each Sampletype_final for both cohorts
myplot <- generate_diversity_boxplot_2(full, "Sampletype_final", "Chao1", variable_colours_available = T) +facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_final_Chao1.pdf"),myplot, width = 11, height = 8,units = "cm")

myplot <- generate_diversity_boxplot_2(full, "Sampletype_final", "Shannon", variable_colours_available = T) +facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_final_Shannon.pdf"),myplot, width = 11, height = 8,units = "cm")

myplot <- generate_diversity_boxplot_2(full, "Sampletype_final", "Simpson", variable_colours_available = T) +facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_final_Simpson.pdf"),myplot, width = 11, height = 8,units = "cm")

# For each Sampletype_final both cohorts, downsampled
myplot <- generate_diversity_boxplot_2(full_downsampled.df, "Sampletype_final", "Chao1", variable_colours_available = T) +facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_final_down_sampled_Chao1.pdf"),myplot, width = 11, height = 8,units = "cm")

myplot <- generate_diversity_boxplot_2(full_downsampled.df, "Sampletype_final", "Shannon", variable_colours_available = T) +facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_final_down_sampled_Shannon.pdf"),myplot, width = 11, height = 8,units = "cm")

myplot <- generate_diversity_boxplot_2(full_downsampled.df, "Sampletype_final", "Simpson", variable_colours_available = T) +facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_final_down_sampled_Simpson.pdf"),myplot, width = 11, height = 8,units = "cm")

# For each Sampletype_final down sampled (immunocompromised)
myplot <- generate_diversity_boxplot_2(immunocompromised_down_sampled.df, "Sampletype_final", "Chao1", variable_colours_available = T)+facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised_sampletype_final_down_sampled_Chao1.pdf"),myplot, width = 8, height = 8,units = "cm")

myplot <- generate_diversity_boxplot_2(immunocompromised_down_sampled.df, "Sampletype_final", "Shannon", variable_colours_available = T)+facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised_sampletype_final_down_sampled_Shannon.pdf"),myplot, width = 8, height = 8,units = "cm")

myplot <- generate_diversity_boxplot_2(immunocompromised_down_sampled.df, "Sampletype_final", "Simpson", variable_colours_available = T)+facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised_sampletype_final_down_sampled_Simpson.pdf"),myplot, width = 8, height = 8,units = "cm")

# For each Sampletype_final down sampled (immunocompetent)
myplot <- generate_diversity_boxplot_2(immunocompetent_down_sampled.df, "Sampletype_final", "Chao1", variable_colours_available = T)+facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent_sampletype_final_down_sampled_Chao1.pdf"),myplot, width = 8, height = 8,units = "cm")

myplot <- generate_diversity_boxplot_2(immunocompetent_down_sampled.df, "Sampletype_final", "Shannon", variable_colours_available = T)+facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent_sampletype_final_down_sampled_Shannon.pdf"),myplot, width = 8, height = 8,units = "cm")

myplot <- generate_diversity_boxplot_2(immunocompetent_down_sampled.df, "Sampletype_final", "Simpson", variable_colours_available = T)+facet_wrap(~Project)
ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent_sampletype_final_down_sampled_Simpson.pdf"),myplot, width = 8, height = 8,units = "cm")


# ------------
# True rare, Sampletype_pooled
# myplot <- generate_diversity_boxplot_2(full_true_rare, "Sampletype_pooled", "Chao1", variable_colours_available = T) +facet_wrap(~Project)
# ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_pooled_true_rare_Chao1.pdf"),myplot, width = 11, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full_true_rare, "Sampletype_pooled", "Shannon", variable_colours_available = T) +facet_wrap(~Project)
# ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_pooled_true_rare_Shannon.pdf"),myplot, width = 11, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full_true_rare, "Sampletype_pooled", "Simpson", variable_colours_available = T) +facet_wrap(~Project)
# ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_pooled_true_rare_Simpson.pdf"),myplot, width = 11, height = 8,units = "cm")

# True rare, Sampletype_compromised_refined (immunocompromised)
# myplot <- generate_diversity_boxplot_2(full_true_rare, "Sampletype_compromised_refined", "Chao1", variable_colours_available = T)+facet_wrap(~Project)
# ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_compromised_refined_true_rare_Chao1.pdf"),myplot, width = 8, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full_true_rare, "Sampletype_compromised_refined", "Shannon", variable_colours_available = T)+facet_wrap(~Project)
# ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_compromised_refined_true_rare_Shannon.pdf"),myplot, width = 8, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full_true_rare, "Sampletype_compromised_refined", "Simpson", variable_colours_available = T)+facet_wrap(~Project)
# ggsave(filename = paste0("Result_figures/diversity_analysis/sampletype_compromised_refined_true_rare_Simpson.pdf"),myplot, width = 8, height = 8,units = "cm")

# ------------

# For Patient_group
# myplot <- generate_diversity_boxplot_2(full, "Patient_group", "Chao1", variable_colours_available = T)# + labs(title = "Patient group")
# ggsave(filename = paste0("Result_figures/diversity_analysis/Patient_group_Chao1.pdf"),myplot, width = 8, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full, "Patient_group", "Shannon", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/Patient_group_Shannon.pdf"),myplot, width = 8, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full, "Patient_group", "Simpson", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/Patient_group_Simpson.pdf"),myplot, width = 8, height = 8,units = "cm")

# For Number_of_meds
# myplot <- generate_diversity_boxplot_2(full, "Number_of_meds", "Chao1", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/Number_of_meds_Chao1.pdf"),myplot, width = 8, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full, "Number_of_meds", "Shannon", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/Number_of_meds_Shannon.pdf"),myplot, width = 8, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full, "Number_of_meds", "Simpson", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/Number_of_meds_Simpson.pdf"),myplot, width = 8, height = 8,units = "cm")

# For Fitzpatrick_skin_type
# myplot <- generate_diversity_boxplot_2(full, "Fitzpatrick_skin_type", "Chao1", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/Fitzpatrick_skin_type_Chao1.pdf"),myplot, width = 8, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full, "Fitzpatrick_skin_type", "Shannon", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/Fitzpatrick_skin_type_Shannon.pdf"),myplot, width = 8, height = 8,units = "cm")
# 
# myplot <- generate_diversity_boxplot_2(full, "Fitzpatrick_skin_type", "Simpson", variable_colours_available = T)
# ggsave(filename = paste0("Result_figures/diversity_analysis/Fitzpatrick_skin_type_Simpson.pdf"),myplot, width = 8, height = 8,units = "cm")

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Plot the Bacterial_load_CFU against the diversities
plot_CFU <- function(mydata, variable_to_plot, metric, variable_colours_available = T, use_shapes = T){
  internal_data.df <- mydata
  internal_data.df[[variable_to_plot]] <- factor(internal_data.df[[variable_to_plot]])
  variable_values <- levels(internal_data.df[[variable_to_plot]])

  # If variable colour column "variable_colour" in metadata, use colours from there
  if (variable_colours_available == T){
    colour_col_name <- paste0(variable_to_plot, "_colour")
    variable_colours <- setNames(as.character(unique(internal_data.df[[colour_col_name]])), as.character(variable_values))
  } else{
    variable_colours <- setNames(colour_palette[1:length(variable_values)], variable_values)  
  }
  if (use_shapes == T){
    variable_shapes <- setNames(rep(c(25,24,23,22,21),length(variable_values))[1:length(variable_values)],variable_values)
  } else{
    variable_shapes <- setNames(rep(c(21),length(variable_values))[1:length(variable_values)],variable_values)  
  }
  
  correlations.df <- data.frame(row.names = unique(internal_data.df[,variable_to_plot]))
  correlations.df[,variable_to_plot] <- rownames(correlations.df)
  correlations.df$p_value_pearson <- NA
  correlations.df$p_value_spearman <- NA
  correlations.df$cor_value_pearson <- NA
  correlations.df$cor_value_spearman <- NA
  
  for (group in unique(internal_data.df[,variable_to_plot])){
    data_subset <- subset(internal_data.df, get(variable_to_plot) == group)
    pearson_test <- with(data_subset, cor.test(Chao1, Bacterial_load_CFU,method = "pearson"))
    spearman_test <- with(data_subset, cor.test(Chao1, Bacterial_load_CFU,method = "spearman", exact = F))
    p_value_pearson <- round(pearson_test$p.value,4)
    p_value_spearman <- round(spearman_test$p.value,4)
    cor_value_pearson <- round(pearson_test$estimate,2)
    cor_value_spearman <- round(spearman_test$estimate,2)
    correlations.df[group,]$p_value_pearson <- p_value_pearson
    correlations.df[group,]$p_value_spearman <- p_value_spearman
    correlations.df[group,]$cor_value_pearson <- cor_value_pearson
    correlations.df[group,]$cor_value_spearman <- cor_value_spearman
  }

  myplot <- ggplot(internal_data.df, aes(x = get(metric), y = log(Bacterial_load_CFU, 10))) +
    geom_point(aes(shape = get(variable_to_plot), fill = get(variable_to_plot))) +
    geom_smooth(method = "lm",se = F,na.rm = T, linetype = "dashed", color = "black", size = .5) +
    geom_text(data= correlations.df,parse = T, aes(x = -Inf, y = Inf, label = paste0("rho==", cor_value_pearson, "*','~italic(p)==", p_value_pearson)), hjust=-0.2, vjust=1.2, size = 3) +
    geom_text(data= correlations.df,parse = T, aes(x = -Inf, y = Inf, label = paste0("r[s]==", cor_value_spearman, "*','~italic(p)==", p_value_spearman)), hjust=-0.2, vjust=3.2, size = 3) +
    scale_shape_manual(values = variable_shapes, name = variable_to_plot)+
    scale_fill_manual(values = variable_colours, name = variable_to_plot) +
    ylab(expression(paste(log[10]~"(Bacterial load CFU)"))) +
    xlab(metric) +
    facet_wrap(~get(variable_to_plot)) +
    common_theme +
    theme(strip.text = element_text(size = 10))
  
  myplot
}
  
# immunocompromised_data_filtered.df <- immunocompromised_data.df[!is.na(immunocompromised_data.df$Bacterial_load_CFU),]

immunocompromised_data_true_rare.df <- subset(full_true_rare, Project == "immunocompromised")
immunocompromised_data_true_rare.df <- immunocompromised_data_true_rare.df[!is.na(immunocompromised_data_true_rare.df$Bacterial_load_CFU),]

# Patient_group, chao1
# for (group in unique(immunocompromised_data.df$Patient_group)){
#     data_subset <- subset(immunocompromised_data.df, Patient_group == group)
#     myplot <- plot_CFU(data_subset, variable_to_plot = "Patient_group", metric = "Chao1") + 
#       scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1)) + theme(legend.position = "none")
#     ggsave(plot = myplot,
#            filename = paste0("Result_figures/diversity_analysis/", "Patient_group_",group, "__Chao1.pdf"),
#            width = 12,height = 10,units = "cm") 
# }

# Patient_group, Shannon
# for (group in unique(immunocompromised_data.df$Patient_group)){
#   data_subset <- subset(immunocompromised_data.df, Patient_group == group)
#   myplot <- plot_CFU(data_subset, variable_to_plot = "Patient_group", metric = "Shannon") + 
#     scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1)) + theme(legend.position = "none")
#   ggsave(plot = myplot,
#          filename = paste0("Result_figures/diversity_analysis/", "Patient_group_",group, "__Shannon.pdf"),
#          width = 12,height = 10,units = "cm") 
# }

# # Sampletype_pooled, chao1
# for (group in unique(immunocompromised_data.df$Sampletype_pooled)){
#   data_subset <- subset(immunocompromised_data.df, Sampletype_pooled == group)
#   myplot <- plot_CFU(data_subset, variable_to_plot = "Sampletype_pooled", metric = "Chao1") + 
#     scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1)) + theme(legend.position = "none")
#   ggsave(plot = myplot,
#          filename = paste0("Result_figures/diversity_analysis/", "Sampletype_pooled_",group, "__Chao1.pdf"),
#          width = 12,height = 10,units = "cm") 
# }
# # ---------------
# for (group in unique(immunocompromised_data_true_rare.df$Sampletype_pooled)){
#   data_subset <- subset(immunocompromised_data_true_rare.df, Sampletype_pooled == group)
#   myplot <- plot_CFU(data_subset, variable_to_plot = "Sampletype_pooled", metric = "Chao1") + 
#     scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1)) + theme(legend.position = "none")
#   ggsave(plot = myplot,
#          filename = paste0("Result_figures/diversity_analysis/", "Sampletype_pooled_",group, "_true_rare__Chao1.pdf"),
#          width = 12,height = 10,units = "cm") 
# }
# # ---------------
# 
# # Sampletype_pooled, Shannon
# for (group in unique(immunocompromised_data.df$Sampletype_pooled)){
#   data_subset <- subset(immunocompromised_data.df, Sampletype_pooled == group)
#   myplot <- plot_CFU(data_subset, variable_to_plot = "Sampletype_pooled", metric = "Shannon") + 
#     scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1)) + theme(legend.position = "none")
#   ggsave(plot = myplot,
#          filename = paste0("Result_figures/diversity_analysis/", "Sampletype_pooled_",group, "__Shannon.pdf"),
#          width = 12,height = 10,units = "cm") 
# }
# 
# 
# # Sampletype_compromised_refined, Chao1
# for (group in unique(immunocompromised_data.df$Sampletype_compromised_refined)){
#   data_subset <- subset(immunocompromised_data.df, Sampletype_compromised_refined == group)#& !is.na(Bacterial_load_CFU)
#   myplot <- plot_CFU(data_subset, variable_to_plot = "Sampletype_compromised_refined", metric = "Shannon") + 
#     scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1)) + theme(legend.position = "none")
#   ggsave(plot = myplot,
#          filename = paste0("Result_figures/diversity_analysis/", "Sampletype_compromised_refined_",group, "__Chao1.pdf"),
#          width = 12,height = 10,units = "cm") 
# }
# 
# 
# # Number_of_meds, chao1
# for (group in unique(immunocompromised_data.df$Number_of_meds)){
#   data_subset <- subset(immunocompromised_data.df, Number_of_meds == group)
#   myplot <- plot_CFU(data_subset, variable_to_plot = "Number_of_meds", metric = "Chao1") + 
#     scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1)) + theme(legend.position = "none")
#   ggsave(plot = myplot,
#          filename = paste0("Result_figures/diversity_analysis/", "Number_of_meds_",group, "__Chao1.pdf"),
#          width = 12,height = 10,units = "cm") 
# }
# 
# # Number_of_meds, Shannon
# for (group in unique(immunocompromised_data.df$Number_of_meds)){
#   data_subset <- subset(immunocompromised_data.df, Number_of_meds == group)
#   myplot <- plot_CFU(data_subset, variable_to_plot = "Number_of_meds", metric = "Shannon") + 
#     scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1)) + theme(legend.position = "none")
#   ggsave(plot = myplot,
#          filename = paste0("Result_figures/diversity_analysis/", "Number_of_meds_", group, "__Shannon.pdf"),
#          width = 12,height = 10,units = "cm") 
# }
# 
# # Fitzpatrick_skin_type, chao1
# for (group in unique(immunocompromised_data.df$Fitzpatrick_skin_type)){
#   data_subset <- subset(immunocompromised_data.df, Fitzpatrick_skin_type == group)
#   myplot <- plot_CFU(data_subset, variable_to_plot = "Fitzpatrick_skin_type", metric = "Chao1") + 
#     scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1)) + theme(legend.position = "none")
#   ggsave(plot = myplot,
#          filename = paste0("Result_figures/diversity_analysis/", "Fitzpatrick_skin_type_",group, "__Chao1.pdf"),
#          width = 12,height = 10,units = "cm") 
# }
# # Fitzpatrick_skin_type, Shannon
# for (group in unique(immunocompromised_data.df$Fitzpatrick_skin_type)){
#   data_subset <- subset(immunocompromised_data.df, Fitzpatrick_skin_type == group)
#   myplot <- plot_CFU(data_subset, variable_to_plot = "Fitzpatrick_skin_type", metric = "Shannon") + 
#     scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1)) + theme(legend.position = "none")
#   ggsave(plot = myplot,
#          filename = paste0("Result_figures/diversity_analysis/", "Fitzpatrick_skin_type_",group, "__Shannon.pdf"),
#          width = 12,height = 10,units = "cm") 
# }


# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

# Generate the diversity summary tables for each variable
# For each discrete variable, calculate the diversity index mean, max, min, median and stdev
# and write to file
# Use down sampled datasets

for (var in discrete_variables) {
  diversity_summary <- full_downsampled.df %>% 
    dplyr::group_by_(var) %>%
    dplyr::summarise(
      Shannon_Mean=mean(Shannon), 
      Shannon_Max=max(Shannon), 
      Shannon_Min=min(Shannon), 
      Shannon_Median=median(Shannon), 
      # Shannon_Std=sd(Shannon),
      
      Simpson_Mean=mean(Simpson), 
      Simpson_Max=max(Simpson), 
      Simpson_Min=min(Simpson), 
      Simpson_Median=median(Simpson), 
      # Simpson_Std=sd(Simpson),
      
      Chao1_Mean=mean(Chao1), 
      Chao1_Max=max(Chao1), 
      Chao1_Min=min(Chao1), 
      Chao1_Median=median(Chao1), 
      # Chao1_Std=sd(Chao1),
      N_patients=n_distinct(Patient),
      N_samples=n_distinct(Index)
      ) %>% 
    as.data.frame()
  outfilename <- paste0("Result_tables/diversity_analysis/", var, "_diversity_summary.csv")
  write.csv(x = diversity_summary, outfilename, row.names = F,quote = F)
}

# Repeat but within each Cohort
for (var in discrete_variables) {
  if (var  == "Project") {next}
  diversity_summary <- full_downsampled.df %>% 
    dplyr::group_by_("Project", var) %>%
    dplyr::summarise(
      Shannon_Mean=mean(Shannon), 
      Shannon_Max=max(Shannon), 
      Shannon_Min=min(Shannon), 
      Shannon_Median=median(Shannon), 
      # Shannon_Std=sd(Shannon),
      
      Simpson_Mean=mean(Simpson), 
      Simpson_Max=max(Simpson), 
      Simpson_Min=min(Simpson), 
      Simpson_Median=median(Simpson), 
      # Simpson_Std=sd(Simpson),
      
      Chao1_Mean=mean(Chao1), 
      Chao1_Max=max(Chao1), 
      Chao1_Min=min(Chao1), 
      Chao1_Median=median(Chao1), 
      # Chao1_Std=sd(Chao1),
      
      N_patients=n_distinct(Patient),
      N_samples=n_distinct(Index)
    ) %>% 
    as.data.frame()
  outfilename <- paste0("Result_tables/diversity_analysis/", var, "_diversity_summary_within_cohort.csv")
  write.csv(x = diversity_summary, outfilename, row.names = F,quote = F)
}

# Repeat for immunocompromised specific variables
# for (var in discrete_variables_immunocompromised) {
#   diversity_summary <- immunocompromised_data.df %>% 
#     dplyr::group_by_(var) %>%
#     dplyr::summarise(
#       Shannon_Mean=mean(Shannon), 
#       Shannon_Max=max(Shannon), 
#       Shannon_Min=min(Shannon), 
#       Shannon_Median=median(Shannon), 
#       # Shannon_Std=sd(Shannon),
#       
#       Simpson_Mean=mean(Simpson), 
#       Simpson_Max=max(Simpson), 
#       Simpson_Min=min(Simpson), 
#       Simpson_Median=median(Simpson), 
#       # Simpson_Std=sd(Simpson),
#       
#       Chao1_Mean=mean(Chao1), 
#       Chao1_Max=max(Chao1), 
#       Chao1_Min=min(Chao1), 
#       Chao1_Median=median(Chao1), 
#       # Chao1_Std=sd(Chao1),
#       N_patients=n_distinct(Patient),
#       N_samples=n_distinct(Index)
#     ) %>% 
#     as.data.frame()
#   outfilename <- paste0("Result_tables/diversity_analysis/", var, "_diversity_summary.csv")
#   write.csv(x = diversity_summary, outfilename, row.names = F,quote = F)
# }

# ------------------------------------------------------------------------------------------------------------------------------
# Takes awhile to calculate, uncomment to run

# Calculate the beta-diversity for each variable 
# Centre-log transform the counts first and use a euclidean distance. This should be equivalent or superior to 
# a bray curtis transform/distance used on counts. 
# It is unclear whether un-rarefied vs rarefied data should be used. Our data is partially rarefied, this should be ok.
# As far as I can tell in the literature, the euclidean distance between CLR values is an appropriate beta diversity measure

# beta_diversity_significances <- data.frame("Variable" = character(),
#                                            "P_value" = numeric(),
#                                            "R_value" = numeric()
# )
# for (myvar in discrete_variables){
#   metadata_subset.df <- metadata.df[!is.na(metadata.df[,myvar]),]
#   otu_rare_subset.m <- otu_rare.m[,rownames(metadata_subset.df)]
#   # print(myvar)
#   temp <- with(metadata_subset.df, anosim(t(clr(otu_rare_subset.m)),get(myvar), distance = "euclidean",permutations = 999))
#   beta_diversity_significances <- rbind(beta_diversity_significances, data.frame("Variable" = myvar,
#                                                                                  "P_value" = temp$signif,
#                                                                                  "R_value" = temp$statistic))
# }
# 
# beta_diversity_significances$padj <- round(p.adjust(beta_diversity_significances$P_value,method = "BH"),6)
# write.csv(beta_diversity_significances, file = "Result_tables/diversity_analysis/variable_beta_diversity_significance.csv", row.names = F, quote = F)
# 
# # Calculate the beta-diversity for each variable within each cohort
# beta_diversity_significances <- data.frame("Project" = character(),
#                                            "Variable" = character(),
#                                            "P_value" = numeric(),
#                                            "R_value" = numeric()
# )
# 
# for (cohort in unique(metadata.df$Project)){
#   discrete_variables_cohort <- discrete_variables
#   if (cohort == "immunocompromised"){
#     discrete_variables_cohort <- c(discrete_variables, discrete_variables_immunocompromised)
#   }
#   for (myvar in discrete_variables_cohort){
#     if (myvar == "Project") {next}
#     metadata_subset.df <- subset(metadata.df, Project == cohort)
#     metadata_subset.df <- metadata_subset.df[!is.na(metadata_subset.df[,myvar]),]
#     otu_rare_subset.m <- otu_rare.m[,rownames(metadata_subset.df)]
#     # print(all(colnames(otu_rare_subset.m) == rownames(metadata_subset.df)))
#     # print(myvar)
#     temp <- with(metadata_subset.df, anosim(t(clr(otu_rare_subset.m)),get(myvar), distance = "euclidean",permutations = 999))
#     beta_diversity_significances <- rbind(beta_diversity_significances, data.frame("Project" = cohort,
#                                                                                    "Variable" = myvar,
#                                                                                    "P_value" = temp$signif,
#                                                                                    "R_value" = temp$statistic))
#   }
# }
# # beta_diversity_significances$padj <- 0
# # beta_diversity_significances[beta_diversity_significances$Project == "immunocompetent",]$padj <- round(p.adjust(beta_diversity_significances[beta_diversity_significances$Project == "immunocompetent",]$P_value,method = "BH"),6)
# # beta_diversity_significances[beta_diversity_significances$Project == "immunocompromised",]$padj <- round(p.adjust(beta_diversity_significances[beta_diversity_significances$Project == "immunocompromised",]$P_value,method = "BH"),6)
# beta_diversity_significances$padj <- round(p.adjust(beta_diversity_significances$P_value,method = "BH"),6)
# write.csv(beta_diversity_significances, file = "Result_tables/diversity_analysis/within_cohort_variable_beta_diversity_significance.csv", row.names = F, quote = F)

# ------------------------------------------------------------------------------------------------------------------------------

# Now that we have calculated the diversities for each sample, we can test if diversity distributions are significantly different between groups:
# Lesion types
# Cohort
# 
# And for immunocompromised
# Patient_group (discrete)
# Number_of_meds (discrete)
# Fitzpatrick_skin_type (discrete)
# Sampletype_compromised_refined (discrete)

# Tests that can be used to compare multiple discrete groups include:
# kruskal-wallis (non-parametric, data does not need to be normal, typically used for more than two groups though can be used for pairs)
# anova
# t-tests (for two-groups)
# Wilcoxon rank sum test (non-parametric)
# general linear models
# general mix linear models (includes a random effect)

# ------------------------------

# ----------------------------
# Mann–Whitney U / Wilcox test
# Unpaired = "Wilcoxon rank sum test" or "Mann-Whitney U test"

calculate_diversity_significance <- function(mydata, variable){
  # Assumes there are Shannon, Chao1 and Simpson columns
  # This is run assuming unpaired data.
  # In reality, for skin for example, we may be comparing 
  # lesion types from the same patient. In this case the data is paired.
  # However this is not the case for all patients / lesions and hence would suggest
  # that unpaired is sufficient.
  results.df <- data.frame("Group_1" = character(),
                           "Group_2" = character(),
                           "Shannon_MannW_pvalue" = character(),
                           "Simpson_MannW_pvalue" = character(),
                           "Chao1_MannW_pvalue" = character(),
                           "Shannon_KrusW_pvalue" = character(),
                           "Simpson_KrusW_pvalue" = character(),
                           "Chao1_KrusW_pvalue" = character())
  group_combinations <- combn(as.character(unique(mydata[,variable])), 2)
  
  # kruskal_shannon_test <- kruskal.test(Shannon~get(variable), data = mydata)
  # kruskal_simpson_test <- kruskal.test(Simpson~get(variable), data = mydata)
  # kruskal_chao1_test <- kruskal.test(Chao1~get(variable), data = mydata)
  # all_group_result_string <- paste0("Kruskal-Wallis all groups: ",
  #       "\nShannon=",round(kruskal_shannon_test$p.value, 6),
  #       "\nSimpson=",round(kruskal_simpson_test$p.value, 6),
  #       "\nChao1=", round(kruskal_chao1_test$p.value, 6))
  # cat(sprintf(all_group_result_string))
  
  for (i in 1:ncol(group_combinations)) {
    group_1 <- group_combinations[1,i]
    group_2 <- group_combinations[2,i]
    group_1_meta <- subset(mydata, get(variable) == group_1)
    group_2_meta <- subset(mydata, get(variable) == group_2)
    
    # Mann-Whitney test on the Shannon diversity
    wilcox_shannon_test <- wilcox.test(group_1_meta$Shannon, group_2_meta$Shannon, exact = F)
    # Mann-Whitney test on the Simpson diversity
    wilcox_simpson_test <- wilcox.test(group_1_meta$Simpson, group_2_meta$Simpson, exact = F)
    # Mann-Whitney test on the Chao1 diversity
    wilcox_chao1_test <- wilcox.test(group_1_meta$Chao1, group_2_meta$Chao1, exact = F)
    
    # Kruskal-Wallis (pairwise) test on the Shannon diversity
    kruskal_shannon_test <- kruskal.test(Shannon~get(variable), data = subset(mydata, get(variable) %in% c(group_1, group_2)))
    # Kruskal-Wallis (pairwise) test on the Simpson diversity
    kruskal_simpson_test <- kruskal.test(Simpson~get(variable), data = subset(mydata, get(variable) %in% c(group_1, group_2)))
    # Kruskal-Wallis (pairwise) test on the Chao1 diversity
    kruskal_chao1_test <- kruskal.test(Chao1~get(variable), data = subset(mydata, get(variable) %in% c(group_1, group_2)))
    
    results.df <- rbind(results.df, data.frame("Group_1" = group_1, 
                                               "Group_2" = group_2, 
                                               "Shannon_MannW_pvalue" = round(wilcox_shannon_test$p.value,6),
                                               "Simpson_MannW_pvalue" = round(wilcox_simpson_test$p.value,6),
                                               "Chao1_MannW_pvalue" = round(wilcox_chao1_test$p.value,6),
                                               "Shannon_KrusW_pvalue" = round(kruskal_shannon_test$p.value,6),
                                               "Simpson_KrusW_pvalue" = round(kruskal_simpson_test$p.value,6),
                                               "Chao1_KrusW_pvalue" = round(kruskal_chao1_test$p.value,6)
                                               ))
  }
  results.df$Shannon_MannW_padj <- round(p.adjust(results.df$Shannon_MannW_pvalue,method = "BH"),6)
  results.df$Simpson_MannW_padj <- round(p.adjust(results.df$Simpson_MannW_pvalue,method = "BH"),6)
  results.df$Chao1_MannW_padj <- round(p.adjust(results.df$Chao1_MannW_pvalue,method = "BH"),6)
  results.df$Shannon_KrusW_padj <- round(p.adjust(results.df$Shannon_KrusW_pvalue,method = "BH"),6)
  results.df$Simpson_KrusW_padj <- round(p.adjust(results.df$Simpson_KrusW_pvalue,method = "BH"),6)
  results.df$Chao1_KrusW_padj <- round(p.adjust(results.df$Chao1_KrusW_pvalue,method = "BH"),6)
  results.df
}
# Cohort significance
cohorts_diversity_significance.df <- calculate_diversity_significance(full_downsampled.df, "Project")
write.csv(cohorts_diversity_significance.df, file = "Result_tables/diversity_analysis/cohort_wilcox.csv", row.names = F, quote = F)

# Sampletype_final significance
sampletype_final_diversity_significance.df <- calculate_diversity_significance(full_downsampled.df, "Sampletype_final")
sampletype_final_diversity_significance.df
write.csv(sampletype_final_diversity_significance.df, file = "Result_tables/diversity_analysis/sampletype_final_wilcox.csv", row.names = F, quote = F)

# --------------------
# Project_Sampletype_final significance
# temp <- subset(full_downsampled.df,Project_Sampletype_final != "immunocompromised_C")
# temp <- full_downsampled.df
# temp$Project_Sampletype_final <- factor(temp$Project_Sampletype_final)
both_cohorts_sampletype_final_diversity_significance.df <- calculate_diversity_significance(full_downsampled.df, "Project_Sampletype_final")
both_cohorts_sampletype_final_diversity_significance.df
# both_cohorts_sampletype_pooled_diversity_significance_true_rare.df <- calculate_diversity_significance(full_true_rare, "Project_Sampletype_final")
# both_cohorts_sampletype_pooled_diversity_significance_true_rare.df

# pairwise.wilcox.test(full$Chao1, full$Project_Sampletype_pooled, p.adjust.method = "BH",paired = F)
# both_cohorts_sampletype_pooled_diversity_significance.df$temp <- p.adjust(both_cohorts_sampletype_pooled_diversity_significance.df$Chao1_p.value,method = "BH")
# pairwise.wilcox.test(full$Chao1, full$Project_Sampletype_pooled, p.adjust.method = "none",paired = F)
write.csv(both_cohorts_sampletype_final_diversity_significance.df, file = "Result_tables/diversity_analysis/cohort_sampletype_final_wilcox.csv", row.names = F, quote = F)
# write.csv(both_cohorts_sampletype_pooled_diversity_significance_true_rare.df, file = "Result_tables/diversity_analysis/cohort_sampletype_true_rare_wilcox.csv", row.names = F, quote = F)
# --------------------

# ------------------------------------------------------------
# Immunocompromised specific
# Sampletype_compromised_refined
# immunocompromised_sampletype_compromised_refined_diversity_significance.df <- calculate_diversity_significance(immunocompromised_data.df, "Sampletype_compromised_refined")
# write.csv(immunocompromised_sampletype_compromised_refined_diversity_significance.df, file = "Result_tables/diversity_analysis/immunocompromised_sampletype_compromised_refined_group_wilcox.csv", row.names = F, quote = F)

# Sampletype_compromised_refined for down sampled data set
immunocompromised_sampletype_compromised_refined_down_sampled_diversity_significance.df <- calculate_diversity_significance(immunocompromised_down_sampled.df, "Sampletype_final")
write.csv(immunocompromised_sampletype_compromised_refined_down_sampled_diversity_significance.df, file = "Result_tables/diversity_analysis/immunocompromised_sampletype_final_down_sampled_group_wilcox.csv", row.names = F, quote = F)

# Patient_group
# immunocompromised_patient_group_diversity_significance.df <- calculate_diversity_significance(immunocompromised_data.df, "Patient_group")
# write.csv(immunocompromised_patient_group_diversity_significance.df, file = "Result_tables/diversity_analysis/immunocompromised_patient_group_wilcox.csv", row.names = F, quote = F)

# Number_of_meds
# immunocompromised_number_of_meds_diversity_significance.df <- calculate_diversity_significance(immunocompromised_data.df, "Number_of_meds")
# write.csv(immunocompromised_number_of_meds_diversity_significance.df, file = "Result_tables/diversity_analysis/immunocompromised_number_of_meds_wilcox.csv", row.names = F, quote = F)

# Fitzpatrick_skin_type
# immunocompromised_skin_type_diversity_significance.df <- calculate_diversity_significance(immunocompromised_data.df, "Fitzpatrick_skin_type")
# write.csv(immunocompromised_skin_type_diversity_significance.df, file = "Result_tables/diversity_analysis/immunocompromised_fitzpatrick_skin_type_wilcox.csv", row.names = F, quote = F)

# ------------------------------------------------------------
# Immunocompetent specific

# Sampletype_compromised_refined for down sampled data set
immunocompetent_sampletype_compromised_refined_down_sampled_diversity_significance.df <- calculate_diversity_significance(immunocompetent_down_sampled.df, "Sampletype_final")
write.csv(immunocompetent_sampletype_compromised_refined_down_sampled_diversity_significance.df, file = "Result_tables/diversity_analysis/immunocompetent_sampletype_final_down_sampled_group_wilcox.csv", row.names = F, quote = F)
# ------------------------------------------------------------

kruskal.test(Chao1~Sampletype_compromised_refined, data = immunocompromised_down_sampled.df)
kruskal.test(Shannon~Sampletype_compromised_refined, data = immunocompromised_down_sampled.df)
kruskal.test(Chao1~Sampletype_compromised_refined, data = immunocompromised_data.df)
kruskal.test(Shannon~Sampletype_compromised_refined, data = immunocompromised_data.df)

#     metadata_subset.df <- subset(full, Sample_Type == st) # Subset the metadata to only entries in the sample type
#     metadata_subset.df <- metadata_subset.df[!is.na(metadata_subset.df[[variable]]),] # remove NA entries
#     # ....................................
#     # All groups together
#     # Perform the Kruskal wallace test on the Chao1 diversity
#     kw_chao1_test <- kruskal.test(Chao1~get(variable), data = metadata_subset.df)




summary(aov(formula = Chao1~Sample_Type, data = full))
summary(aov(formula = Chao1~DX_Groups, data = full))
# ANOVA on 
summary(aov(formula = Chao1~Sample_Type + DX_Groups + AGE + BMI + Gender, data = full))

# lme requires a random effect variable to be specified. Simply make this the sample ID (Index or Sample_No, the latter is the patient ID)
full$Index <- rownames(full)


# Run linear mixed model for Chao1 index
chao1full_model <- lme(fixed = Chao1 ~ DX_Groups + Sample_Type + Gender + AGE + BMI,
                       data = full, random = ~ 1 | Index) 
summary(chao1full_model)

# Run linear model to test significance of variables across all variables
# lm will apply an anova or t-test where appropriate
lm_sample_type_chao <- lm(Chao1 ~ Sample_Type, data = full)
lm_dx_groups_chao <- lm(Chao1 ~ DX_Groups, data = full)

summary(aov(formula = Chao1~Sample_Type, data = full))
summary(lm_sample_type_chao)
summary(aov(formula = Chao1~DX_Groups, data = full))
summary(lm_dx_groups_chao)

lm_sample_type_simpson <- lm(Simpson ~ Sample_Type, data = full)
lm_dx_groups_simpson <- lm(Simpson ~ DX_Groups, data = full)



summary(lm_sample_type_simpson)
summary(lm_dx_groups_simpson)


# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
