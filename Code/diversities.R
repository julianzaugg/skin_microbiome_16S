# Diversity calculations Alpha (Shannon, Chao1, Simpson) and Beta (CLR transformed counts) diveristy for each sample.
# Significance tests of each discrete group comparing diversity indices.
# Generate boxplots for discrete 
# Comparison of continuous variables vs diversity indices

# "Alpha diversity measures the diversity within a single sample and is generally 
# based on the number and relative abundance of taxa at some rank (e.g. species or OTUs).
# Beta diversity also uses the number of relative abundance of taxa at some rank, but measures 
# variation between samples. In other words, an alpha diversity statistic describes a single 
# sample and a beta diversity statistic describes how two samples compare."

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()
library(vegan)
library(reshape2)
library(ggplot2)
library(dplyr)

# library(FSA)
library(phyloseq)
# library(nlme)


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

# --------------------------------------------------------------------------------
generate_diversity_boxplot <- function(mydata, variable, metric, variable_colours_available = T, fill_palette = NULL, rotate_x_text = F){
  internal_data.df <- mydata[!is.na(mydata[variable]),]
  internal_data.df[[variable]] <- factor(internal_data.df[[variable]])
  variable_values <- factor(as.character(unique(internal_data.df[[variable]])))
  if (variable_colours_available == T){
    color_col_name <- paste0(variable, "_colour")
    variable_colours <- setNames(as.character(unique(internal_data.df[[color_col_name]])), as.character(unique(internal_data.df[[variable]])))
  } else{
    if (is.null(fill_palette)){
      internal_colour_palette <- my_colour_palette_206_distinct
    } else{
      internal_colour_palette <- fill_palette
    }
    variable_colours <- setNames(internal_colour_palette[1:length(variable_values)], variable_values)  
  }
  myplot <- ggplot(internal_data.df, aes(x = get(variable), y = get(metric))) +
    geom_boxplot(outlier.shape = NA, aes(fill = get(variable))) +
    scale_fill_manual(values = variable_colours, name = variable) +
    # scale_x_discrete(labels = gsub("_", " ", internal_data.df[,variable])) +
    geom_jitter(size=0.5, width = 0.10, height=0) +
    guides(fill=FALSE) +
    # scale_y_continuous(limits = c(0,4.5), breaks = seq(0,4.5,.5)) +
    xlab(gsub("_", " ", variable)) +
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
          axis.text = element_text(size = 8, colour = "black"),
          axis.text.x = element_text(angle = ifelse(rotate_x_text, 0,90), vjust = .5),
          axis.title = element_text(size = 10,face = "bold"),
          complete = F,
          plot.title = element_text(size = 6,hjust = 0.5))
  myplot
}


setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")
source("Code/helper_functions.R")

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# Define the discrete variables
discrete_variables <- c("Lesion_type_refined","Gender","Patient", "Cohort", "Length_of_immunosuppression_group_1", "Length_of_immunosuppression_group_2")
discrete_variables_immunocompromised <- c("Length_of_immunosuppression_group_1", "Length_of_immunosuppression_group_2")

# Set order of variables
# AK C C_P SCC SCC_PL
metadata.df$Lesion_type_refined <- factor(metadata.df$Lesion_type_refined, levels = c("C", "C_P", "AK", "SCC_PL","SCC"))

# Only want immunocompromised and the snapshot immunocompetent samples
metadata.df <- subset(metadata.df, Cohort == "immunocompromised" | Snapshot_sample_5 == "yes")

# Load the counts
otu.m <- as.matrix(read.csv("Result_tables/count_tables/OTU_counts.csv", header =T, row.names = 1))
genus.m <-  as.matrix(read.csv("Result_tables/count_tables/Genus_counts.csv", header =T, row.names = 1))

# Order the matrices and metadata to be the same order
otu.m <- otu.m[,rownames(metadata.df)]
genus.m <- genus.m[,rownames(metadata.df)]

# Create the rarefied matrices
otu_rare.m <- t(rrarefy(t(otu.m[,colSums(otu.m) >= 2000]), 2000))
genus_rare.m <- t(rrarefy(t(genus.m[,colSums(genus.m) >= 2000]), 2000))

# create phyloseq object
otu_rare_phyloseq <- otu_table(otu_rare.m, taxa_are_rows=TRUE)
genus_rare_phyloseq <- otu_table(genus_rare.m, taxa_are_rows=TRUE)

# Estimate alpha diversities
both_cohorts_otu_rare_alpha.df <- estimate_richness(otu_rare_phyloseq, measures = c("Chao1", "Simpson","Shannon"))
both_cohorts_otu_rare_alpha.df <- both_cohorts_otu_rare_alpha.df[rownames(metadata.df),]

both_cohorts_genus_rare_alpha.df <- estimate_richness(genus_rare_phyloseq, measures = c("Chao1", "Simpson","Shannon"))
both_cohorts_genus_rare_alpha.df <- both_cohorts_genus_rare_alpha.df[rownames(metadata.df),]

# Combine with metadata
both_cohorts_otu_rare_alpha.df <- left_join(metadata.df[c("Index", 
                                             discrete_variables, 
                                             grep("_colour", names(metadata.df), value = T))],m2df(both_cohorts_otu_rare_alpha.df, "Index"), by = "Index")
both_cohorts_genus_rare_alpha.df <- left_join(metadata.df[c("Index",
                                               discrete_variables, 
                                               grep("_colour", names(metadata.df), value = T))],m2df(both_cohorts_genus_rare_alpha.df, "Index"), by = "Index")


# Write per-sample diversities to file
write.csv(both_cohorts_otu_rare_alpha.df,
          "Result_tables/diversity_analysis/both_cohorts/otu/sample_otu_alpha_diversities.csv", quote = F, row.names = F
)
write.csv(both_cohorts_genus_rare_alpha.df,
          "Result_tables/diversity_analysis/both_cohorts/genus/sample_genus_alpha_diversities.csv", quote = F, row.names = F
)

# Create Cohort specific datasets
immunocompetent_otu_rare_alpha.df <- subset(both_cohorts_otu_rare_alpha.df, Cohort == "immunocompetent")
immunocompromised_otu_rare_alpha.df <- subset(both_cohorts_otu_rare_alpha.df, Cohort == "immunocompromised")
immunocompetent_genus_rare_alpha.df <- subset(both_cohorts_genus_rare_alpha.df, Cohort == "immunocompetent")
immunocompromised_genus_rare_alpha.df <- subset(both_cohorts_genus_rare_alpha.df, Cohort == "immunocompromised")



# ------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------
# Calculate summaries and signifiance tests for each group for each variable, across all samples (both cohorts) and each
# individual cohort
# significance tests and p-value adjustments only apply to non-NA groups

summarise_diversities_each_variable <- function(mydata, variables){
  mydata_summary.df <- NULL
  for (myvar in variables){
    if (is.null(mydata_summary.df)){
      mydata_summary.df <- summarise_alpha_diversities(mydata, myvar)
      # Melt because we are combining variables into a single table
      mydata_summary.df <- melt(mydata_summary.df, measure.vars = myvar, value.name = "Group", variable.name = "Variable")
    } else{
      mydata_summary.df <- rbind(mydata_summary.df, melt(summarise_alpha_diversities(mydata, myvar),
                                                         measure.vars = myvar, value.name = "Group", variable.name = "Variable"))      
    }

  }
  mydata_summary.df
}

calculate_diversity_sigificances_each_variable <- function(mydata, variables){
  mydata_significances.df <- NULL
  for (myvar in variables){
    if (is.null(mydata_significances.df)){
      mydata_significances.df <- calculate_alpha_diversity_significance(mydata, myvar)      
    }
    else{
      mydata_significances.df <- rbind(mydata_significances.df,
                                       calculate_alpha_diversity_significance(mydata, myvar))      
    }
  }
  mydata_significances.df
}


both_cohorts_otu_alpha_diversity_summary.df <- summarise_diversities_each_variable(both_cohorts_otu_rare_alpha.df, variables = discrete_variables)
both_cohorts_otu_alpha_diversity_significances.df <- summarise_diversities_each_variable(both_cohorts_otu_rare_alpha.df, variables = discrete_variables)

both_cohorts_genus_alpha_diversity_summary.df <- summarise_diversities_each_variable(both_cohorts_genus_rare_alpha.df, variables = discrete_variables)
both_cohorts_genus_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(both_cohorts_genus_rare_alpha.df, variables = discrete_variables)

immunocompromised_otu_alpha_diversity_summary.df <- summarise_diversities_each_variable(immunocompromised_otu_rare_alpha.df, variables = discrete_variables)
immunocompromised_otu_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(immunocompromised_otu_rare_alpha.df, variables = discrete_variables)

immunocompromised_genus_alpha_diversity_summary.df <- summarise_diversities_each_variable(immunocompromised_genus_rare_alpha.df, variables = discrete_variables)
immunocompromised_genus_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(immunocompromised_genus_rare_alpha.df, variables = discrete_variables)

immunocompetent_otu_alpha_diversity_summary.df <- summarise_diversities_each_variable(immunocompetent_otu_rare_alpha.df, variables = discrete_variables)
immunocompetent_otu_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(immunocompetent_otu_rare_alpha.df, variables = discrete_variables)

immunocompetent_genus_alpha_diversity_summary.df <- summarise_diversities_each_variable(immunocompetent_genus_rare_alpha.df, variables = discrete_variables)
immunocompetent_genus_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(immunocompetent_genus_rare_alpha.df, variables = discrete_variables)

# Both cohorts,OTU level
write.csv(x = both_cohorts_otu_alpha_diversity_summary.df, 
          file = paste0("Result_tables/diversity_analysis/both_cohorts/otu/otu_alpha_diversities_summary.csv"), 
          quote = F, row.names = F)

write.csv(x = both_cohorts_otu_alpha_diversity_significances.df, 
          file = paste0("Result_tables/diversity_analysis/both_cohorts/otu/otu_alpha_diversities_significance.csv"), 
          quote = F, row.names = F)

# Both cohorts, Genus level
write.csv(x = both_cohorts_genus_alpha_diversity_summary.df, 
          file = paste0("Result_tables/diversity_analysis/both_cohorts/genus/genus_alpha_diversities_summary.csv"), 
          quote = F, row.names = F)

write.csv(x = both_cohorts_genus_alpha_diversity_significances.df, 
          file = paste0("Result_tables/diversity_analysis/both_cohorts/genus/genus_alpha_diversities_significance.csv"), 
          quote = F, row.names = F)

# immunocompromised, OTU level
write.csv(x = immunocompromised_otu_alpha_diversity_summary.df, 
          file = paste0("Result_tables/diversity_analysis/immunocompromised/otu/otu_alpha_diversities_summary.csv"), 
          quote = F, row.names = F)

write.csv(x = immunocompromised_otu_alpha_diversity_significances.df, 
          file = paste0("Result_tables/diversity_analysis/immunocompromised/otu/otu_alpha_diversities_significance.csv"), 
          quote = F, row.names = F)

# immunocompromised, Genus level
write.csv(x = immunocompromised_genus_alpha_diversity_summary.df, 
          file = paste0("Result_tables/diversity_analysis/immunocompromised/genus/genus_alpha_diversities_summary.csv"), 
          quote = F, row.names = F)

write.csv(x = immunocompromised_genus_alpha_diversity_significances.df, 
          file = paste0("Result_tables/diversity_analysis/immunocompromised/genus/genus_alpha_diversities_significance.csv"), 
          quote = F, row.names = F)

# immunocompetent, OTU level
write.csv(x = immunocompetent_otu_alpha_diversity_summary.df, 
          file = paste0("Result_tables/diversity_analysis/immunocompetent/otu/otu_alpha_diversities_summary.csv"), 
          quote = F, row.names = F)

write.csv(x = immunocompetent_otu_alpha_diversity_significances.df, 
          file = paste0("Result_tables/diversity_analysis/immunocompetent/otu/otu_alpha_diversities_significance.csv"), 
          quote = F, row.names = F)

# immunocompetent, Genus level
write.csv(x = immunocompetent_genus_alpha_diversity_summary.df, 
          file = paste0("Result_tables/diversity_analysis/immunocompetent/genus/genus_alpha_diversities_summary.csv"), 
          quote = F, row.names = F)

write.csv(x = immunocompetent_genus_alpha_diversity_significances.df, 
          file = paste0("Result_tables/diversity_analysis/immunocompetent/genus/genus_alpha_diversities_significance.csv"), 
          quote = F, row.names = F)


# ------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------
# Calculate summaries and signifiance tests for each group for each variable WITHIN EACH COHORT
# significance tests and p-value adjustments only apply to non-NA groups

# df_last_to_first <- function(mydf){
#   last_name <- names(mydf)[length(mydf)]
#   mydf[c(last_name,names(mydf)[1:length(names(mydf))-1])]
# }

# ------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------
# Make figures


"otu_Lesion_type_refined_downsampled_Chao1.pdf"
myplot <- generate_diversity_boxplot(immunocompromised_otu_rare_alpha.df, variable = "Lesion_type_refined",fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T) + 
  guides(fill = F, color = F) + 
  scale_y_continuous(limits = c(0,550), breaks = seq(0,550,50))

ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/otu/otu_Lesion_type_refined_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")

myplot <- generate_diversity_boxplot(immunocompromised_otu_rare_alpha.df, variable = "Lesion_type_refined",fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T) + 
  guides(fill = F, color = F) + 
  scale_y_continuous(limits = c(0,5), breaks = seq(0,7,.5))

ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/otu/otu_Lesion_type_refined_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")

# Generate boxplots
for (myvar in c("Lesion_type_refined", "Length_of_immunosuppression_group_1", "Length_of_immunosuppression_group_2")){
  # ----------------------------------------------------------------
  # both cohorts,, otu level
  myplot <- generate_diversity_boxplot(both_cohorts_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/otu/otu_",myvar,"_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(both_cohorts_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/otu/otu_",myvar,"_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(both_cohorts_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/otu/otu_",myvar,"_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  # ----------------------------------------------------------------
  # both cohorts,, genus level
  myplot <- generate_diversity_boxplot(both_cohorts_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/genus/genus_",myvar,"_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(both_cohorts_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/genus/genus_",myvar,"_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(both_cohorts_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/genus/genus_",myvar,"_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  # ----------------------------------------------------------------
  # Immunocompetent,, otu level
  myplot <- generate_diversity_boxplot(immunocompetent_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/otu/otu_",myvar,"_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompetent_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/otu/otu_",myvar,"_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompetent_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/otu/otu_",myvar,"_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  # ----------------------------------------------------------------
  # Immunocompetent,, genus level
  myplot <- generate_diversity_boxplot(immunocompetent_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,500), breaks = seq(0,500,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/genus/genus_",myvar,"_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompetent_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/genus/genus_",myvar,"_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompetent_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/genus/genus_",myvar,"_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  # ----------------------------------------------------------------
  # ----------------------------------------------------------------
  # Immunocompromised,, otu level
  myplot <- generate_diversity_boxplot(immunocompromised_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,500), breaks = seq(0,500,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/otu/otu_",myvar,"_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompromised_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/otu/otu_",myvar,"_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompromised_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/otu/otu_",myvar,"_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  # ----------------------------------------------------------------
  # Immunocompromised,, genus level
  myplot <- generate_diversity_boxplot(immunocompromised_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/genus/genus_",myvar,"_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompromised_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/genus/genus_",myvar,"_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompromised_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/genus/genus_",myvar,"_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  # ----------------------------------------------------------------
}


# -----------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------
# Create sub-/down-sampled datasets. Ensure the same number of patients and the same number of samples.
# Try and make the selected samples well distributed, i.e. try and not bias a single patient

# FIRST IMMUNOCOMPROMISED
# Get the group with the lowest number of patients
lowest_patient_group.df <- immunocompromised_genus_rare_alpha.df %>% 
  group_by(Lesion_type_refined) %>% 
  select(Lesion_type_refined, Patient) %>% 
  dplyr::distinct() %>% 
  dplyr::tally() %>% 
  dplyr::summarise(Lesion_type_refined = Lesion_type_refined[which.min(n)], Count = min(n)) %>% 
  as.data.frame()

# Get the group with the lowest number of samples
lowest_sample_group.df <- immunocompromised_genus_rare_alpha.df %>% 
  group_by(Lesion_type_refined) %>% 
  select(Lesion_type_refined, Index) %>% 
  dplyr::distinct() %>% 
  dplyr::tally() %>% 
  dplyr::summarise(Lesion_type_refined = Lesion_type_refined[which.min(n)], Count = min(n)) %>% 
  as.data.frame()

# We know that the number of patients is always lower than sample number for each group, 
lowest_patient_group.df
lowest_sample_group.df
# 8 patients, 21 samples

# This is a not-so-great approach. Randomly sample samples until the number of unique patients
# is the minimum. To have the same number of samples, we have to limit the number of samples (test different limits)
# Current limit is 16 samples (assuming 21 samples is the lowest total)
set.seed(1234)
immunocompromised_downsampled_genus_rare_alpha.df <- data.frame()
for (sf in unique(immunocompromised_genus_rare_alpha.df$Lesion_type_refined)){
  full_data_subset_sf.df <- subset(immunocompromised_genus_rare_alpha.df, Lesion_type_refined == sf)
  N_patients <- length(unique(full_data_subset_sf.df$Patient))
  N_samples <- length(unique(full_data_subset_sf.df$Index))
  iterations <- 0
  if (sf == "C"){
    while ( N_patients != lowest_patient_group.df$Count | N_samples != lowest_sample_group.df$Count-5){
      temp_downsampled <- full_data_subset_sf.df %>% sample_n(lowest_sample_group.df$Count-5)
      N_patients <- length(unique(temp_downsampled$Patient))
      N_samples <- length(unique(temp_downsampled$Index))
      # print(paste0(sf, " ", N_patients, " ", N_samples, " 2 ", iterations))
      # print(iterations)
    }
  }
  while ( N_patients != lowest_patient_group.df$Count){
    temp_downsampled <- full_data_subset_sf.df %>% sample_n(lowest_sample_group.df$Count-5)
    N_patients <- length(unique(temp_downsampled$Patient))
    N_samples <- length(unique(temp_downsampled$Index))
    iterations <- sum(iterations, 1)
    # print(paste0(sf, " ", N_patients, " ", N_samples, " 3 ", iterations))
    # print(iterations)
    # print(N_patients)
    # print(N_samples)
  }
  immunocompromised_downsampled_genus_rare_alpha.df <- rbind(immunocompromised_downsampled_genus_rare_alpha.df, temp_downsampled)
}

# Check numbers of samples are (approximately) equal
immunocompromised_downsampled_genus_rare_alpha.df %>%
  group_by(Lesion_type_refined) %>%
  select(Lesion_type_refined, Index) %>%
  distinct() %>%
  tally() %>%
  as.data.frame()

immunocompromised_downsampled_genus_rare_alpha.df %>%
  group_by(Lesion_type_refined) %>%
  select(Lesion_type_refined, Patient) %>%
  distinct() %>%
  tally() %>%
  as.data.frame()


# ------------------------------
# NOW IMMUNOCOMPETENT
lowest_patient_group.df <- immunocompetent_genus_rare_alpha.df %>% 
  group_by(Lesion_type_refined) %>% 
  select(Lesion_type_refined, Patient) %>% 
  distinct() %>% 
  tally() %>% 
  summarise(Lesion_type_refined = Lesion_type_refined[which.min(n)], Count = min(n)) %>% 
  as.data.frame()

# Get the group with the lowest number of samples
lowest_sample_group.df <- immunocompetent_genus_rare_alpha.df %>% 
  group_by(Lesion_type_refined) %>% 
  select(Lesion_type_refined, Index) %>% 
  distinct() %>% 
  tally() %>% 
  summarise(Lesion_type_refined = Lesion_type_refined[which.min(n)], Count = min(n)) %>% 
  as.data.frame()

# We know that the number of patients is always lower than sample number for each group, 
lowest_patient_group.df
lowest_sample_group.df
# 10 33

# Randomly sample samples until the number of unique patients
# is the minimum. To have the sample number of samples, we have to limit to 31 samples
set.seed(1234)
immunocompetent_downsampled_genus_rare_alpha.df <- data.frame()
for (sf in unique(immunocompetent_genus_rare_alpha.df$Lesion_type_refined)){
  full_data_subset_sf.df <- subset(immunocompetent_genus_rare_alpha.df, Lesion_type_refined == sf)
  N_patients <- length(unique(full_data_subset_sf.df$Patient))
  N_samples <- length(unique(full_data_subset_sf.df$Index))
  # print(paste0(sf, " ", N_patients, " ", N_samples, " 1"))
  iterations <- 0
  # if (sf %in% c("SCC", "AK")){
    while ( N_patients != lowest_patient_group.df$Count | N_samples != lowest_sample_group.df$Count-2){

      temp_downsampled <- full_data_subset_sf.df %>% sample_n(lowest_sample_group.df$Count-2)
      N_patients <- length(unique(temp_downsampled$Patient))
      N_samples <- length(unique(temp_downsampled$Index))
      iterations <- sum(iterations, 1)
      # print(paste0(sf, " ", N_patients, " ", N_samples, " 2 ", iterations))
    }
  # }
  while ( N_patients != lowest_patient_group.df$Count){
    # print(sf)
    temp_downsampled <- full_data_subset_sf.df %>% sample_n(lowest_sample_group.df$Count-2)
    N_patients <- length(unique(temp_downsampled$Patient))
    N_samples <- length(unique(temp_downsampled$Index))
    iterations <- sum(iterations, 1)
    # print(paste0(sf, " ", N_patients, " ", N_samples, " 3 ", iterations))
    # print(iterations)
    # print(N_patients)
    # print(N_samples)
  }
  immunocompetent_downsampled_genus_rare_alpha.df <- rbind(immunocompetent_downsampled_genus_rare_alpha.df, temp_downsampled)
}

immunocompetent_downsampled_genus_rare_alpha.df %>%
  group_by(Lesion_type_refined) %>%
  select(Lesion_type_refined, Index) %>%
  distinct() %>%
  tally() %>%
  as.data.frame()

immunocompetent_downsampled_genus_rare_alpha.df %>%
  group_by(Lesion_type_refined) %>%
  select(Lesion_type_refined, Patient) %>%
  distinct() %>%
  tally() %>%
  as.data.frame()
# 10 patients, 30 samples

both_cohorts_downsampled_genus_rare_alpha.df <- rbind(immunocompetent_downsampled_genus_rare_alpha.df, immunocompromised_downsampled_genus_rare_alpha.df)

immunocompetent_downsampled_otu_rare_alpha.df <- both_cohorts_otu_rare_alpha.df[both_cohorts_otu_rare_alpha.df$Index %in% immunocompetent_downsampled_genus_rare_alpha.df$Index,]
immunocompromised_downsampled_otu_rare_alpha.df <- both_cohorts_otu_rare_alpha.df[both_cohorts_otu_rare_alpha.df$Index %in% immunocompromised_downsampled_genus_rare_alpha.df$Index,]
both_cohorts_downsampled_otu_rare_alpha.df <- rbind(immunocompetent_downsampled_otu_rare_alpha.df, immunocompromised_downsampled_otu_rare_alpha.df)

dim(both_cohorts_downsampled_genus_rare_alpha.df)
dim(both_cohorts_downsampled_otu_rare_alpha.df)
dim(immunocompetent_downsampled_genus_rare_alpha.df)
dim(immunocompetent_downsampled_otu_rare_alpha.df)
dim(immunocompromised_downsampled_genus_rare_alpha.df)
dim(immunocompromised_downsampled_otu_rare_alpha.df)


down_sampled_counts.df <- both_cohorts_downsampled_genus_rare_alpha.df %>%
  group_by(Cohort, Lesion_type_refined) %>%
  dplyr::summarise(Patient_count = n_distinct(Patient), Sample_count = n_distinct(Index)) %>% 
  as.data.frame()

write.csv(down_sampled_counts.df, file = "Result_tables/other/downsampled_sample_patient_counts.csv",row.names = F, quote = F)

write.csv(both_cohorts_downsampled_genus_rare_alpha.df %>% select("Index", "Patient","Lesion_type_refined", "Cohort") %>% distinct(),
          file = "Result_tables/other/downsampled_samples.csv", row.names = F, quote = F)

# Write per-sample diversities to file
write.csv(both_cohorts_downsampled_otu_rare_alpha.df,
          "Result_tables/diversity_analysis/both_cohorts/otu/sample_otu_alpha_diversities_downsampled.csv", quote = F, row.names = F
)
write.csv(both_cohorts_downsampled_genus_rare_alpha.df,
          "Result_tables/diversity_analysis/both_cohorts/genus/sample_genus_alpha_diversities_downsampled.csv", quote = F, row.names = F
)
# -----------------------------------------------------------------------------------------------------------------

both_cohorts_downsampled_otu_alpha_diversity_summary.df <- summarise_diversities_each_variable(both_cohorts_downsampled_otu_rare_alpha.df, variables = discrete_variables)
both_cohorts_downsampled_otu_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(both_cohorts_downsampled_otu_rare_alpha.df, variables = discrete_variables)

both_cohorts_downsampled_genus_alpha_diversity_summary.df <- summarise_diversities_each_variable(both_cohorts_downsampled_genus_rare_alpha.df, variables = discrete_variables)
both_cohorts_downsampled_genus_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(both_cohorts_downsampled_genus_rare_alpha.df, variables = discrete_variables)

immunocompromised_downsampled_otu_alpha_diversity_summary.df <- summarise_diversities_each_variable(immunocompromised_downsampled_genus_rare_alpha.df, variables = discrete_variables)
immunocompromised_downsampled_otu_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(immunocompromised_downsampled_genus_rare_alpha.df, variables = discrete_variables)

immunocompromised_downsampled_genus_alpha_diversity_summary.df <- summarise_diversities_each_variable(immunocompromised_downsampled_genus_rare_alpha.df, variables = discrete_variables)
immunocompromised_downsampled_genus_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(immunocompromised_downsampled_genus_rare_alpha.df, variables = discrete_variables)

immunocompetent_downsampled_otu_alpha_diversity_summary.df <- summarise_diversities_each_variable(immunocompetent_downsampled_otu_rare_alpha.df, variables = discrete_variables)
immunocompetent_downsampled_otu_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(immunocompetent_downsampled_otu_rare_alpha.df, variables = discrete_variables)

immunocompetent_downsampled_genus_alpha_diversity_summary.df <- summarise_diversities_each_variable(immunocompetent_downsampled_genus_rare_alpha.df, variables = discrete_variables)
immunocompetent_downsampled_genus_alpha_diversity_significances.df <- calculate_diversity_sigificances_each_variable(immunocompetent_downsampled_genus_rare_alpha.df, variables = discrete_variables)

# Both cohorts, OTU level
write.csv(x = both_cohorts_downsampled_otu_alpha_diversity_summary.df, 
                      file = paste0("Result_tables/diversity_analysis/both_cohorts/otu/otu_alpha_diversities_summary_downsampled.csv"), 
                      quote = F, row.names = F)

write.csv(x = both_cohorts_downsampled_otu_alpha_diversity_significances.df, 
                      file = paste0("Result_tables/diversity_analysis/both_cohorts/otu/otu_alpha_diversities_significance_downsampled.csv"), 
                      quote = F, row.names = F)

# Both cohorts, Genus level
write.csv(x = both_cohorts_downsampled_genus_alpha_diversity_summary.df, 
                      file = paste0("Result_tables/diversity_analysis/both_cohorts/genus/genus_alpha_diversities_summary_downsampled.csv"), 
                      quote = F, row.names = F)

write.csv(x = both_cohorts_downsampled_genus_alpha_diversity_significances.df, 
                      file = paste0("Result_tables/diversity_analysis/both_cohorts/genus/genus_alpha_diversities_significance_downsampled.csv"), 
                      quote = F, row.names = F)

# immunocompromised, OTU level
write.csv(x = immunocompromised_downsampled_otu_alpha_diversity_summary.df, 
                      file = paste0("Result_tables/diversity_analysis/immunocompromised/otu/otu_alpha_diversities_summary_downsampled.csv"), 
                      quote = F, row.names = F)

write.csv(x = immunocompromised_downsampled_otu_alpha_diversity_significances.df, 
                      file = paste0("Result_tables/diversity_analysis/immunocompromised/otu/otu_alpha_diversities_significance_downsampled.csv"), 
                      quote = F, row.names = F)

# immunocompromised, Genus level
write.csv(x = immunocompromised_downsampled_genus_alpha_diversity_summary.df, 
                      file = paste0("Result_tables/diversity_analysis/immunocompromised/genus/genus_alpha_diversities_summary_downsampled.csv"), 
                      quote = F, row.names = F)

write.csv(x = immunocompromised_downsampled_genus_alpha_diversity_significances.df, 
                      file = paste0("Result_tables/diversity_analysis/immunocompromised/genus/genus_alpha_diversities_significance_downsampled.csv"), 
                      quote = F, row.names = F)

# immunocompetent, OTU level
write.csv(x = immunocompetent_downsampled_otu_alpha_diversity_summary.df, 
                      file = paste0("Result_tables/diversity_analysis/immunocompetent/otu/otu_alpha_diversities_summary_downsampled.csv"), 
                      quote = F, row.names = F)

write.csv(x = immunocompetent_downsampled_otu_alpha_diversity_significances.df, 
                      file = paste0("Result_tables/diversity_analysis/immunocompetent/otu/otu_alpha_diversities_significance_downsampled.csv"), 
                      quote = F, row.names = F)

# immunocompetent, Genus level
write.csv(x = immunocompetent_downsampled_genus_alpha_diversity_summary.df, 
                      file = paste0("Result_tables/diversity_analysis/immunocompetent/genus/genus_alpha_diversities_summary_downsampled.csv"), 
                      quote = F, row.names = F)

write.csv(x = immunocompetent_downsampled_genus_alpha_diversity_significances.df, 
                      file = paste0("Result_tables/diversity_analysis/immunocompetent/genus/genus_alpha_diversities_significance_downsampled.csv"), 
                      quote = F, row.names = F)


# Generate boxplots
for (myvar in c("Lesion_type_refined", "Length_of_immunosuppression_group_1", "Length_of_immunosuppression_group_2")){
  # ----------------------------------------------------------------
  # both cohorts, downsampled, otu level
  myplot <- generate_diversity_boxplot(both_cohorts_downsampled_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/otu/otu_",myvar,"_downsampled_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(both_cohorts_downsampled_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/otu/otu_",myvar,"_downsampled_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(both_cohorts_downsampled_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/otu/otu_",myvar,"_downsampled_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  # ----------------------------------------------------------------
  # both cohorts, downsampled, genus level
  myplot <- generate_diversity_boxplot(both_cohorts_downsampled_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/genus/genus_",myvar,"_downsampled_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(both_cohorts_downsampled_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/genus/genus_",myvar,"_downsampled_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(both_cohorts_downsampled_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/both_cohorts/genus/genus_",myvar,"_downsampled_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  # ----------------------------------------------------------------
  # Immunocompetent, downsampled, otu level
  myplot <- generate_diversity_boxplot(immunocompetent_downsampled_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/otu/otu_",myvar,"_downsampled_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompetent_downsampled_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/otu/otu_",myvar,"_downsampled_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompetent_downsampled_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/otu/otu_",myvar,"_downsampled_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  # ----------------------------------------------------------------
  # Immunocompetent, downsampled, genus level
  myplot <- generate_diversity_boxplot(immunocompetent_downsampled_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,500), breaks = seq(0,500,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/genus/genus_",myvar,"_downsampled_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompetent_downsampled_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/genus/genus_",myvar,"_downsampled_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompetent_downsampled_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompetent/genus/genus_",myvar,"_downsampled_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  # ----------------------------------------------------------------
  # ----------------------------------------------------------------
  # Immunocompromised, downsampled, otu level
  myplot <- generate_diversity_boxplot(immunocompromised_downsampled_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,500), breaks = seq(0,500,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/otu/otu_",myvar,"_downsampled_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompromised_downsampled_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/otu/otu_",myvar,"_downsampled_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompromised_downsampled_otu_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/otu/otu_",myvar,"_downsampled_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  # ----------------------------------------------------------------
  # Immunocompromised, downsampled, genus level
  myplot <- generate_diversity_boxplot(immunocompromised_downsampled_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Chao1",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Chao1") +
    scale_y_continuous(limits = c(0,200), breaks = seq(0,200,50))
  
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/genus/genus_",myvar,"_downsampled_Chao1.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompromised_downsampled_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Shannon",variable_colours_available = T,rotate_x_text = T) + 
    guides(fill = F, color = F) + 
    # ggtitle("Shannon") +
    scale_y_continuous(limits = c(0,5), breaks = seq(0,5,.5))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/genus/genus_",myvar,"_downsampled_Shannon.pdf"),myplot, width = 10, height = 10,units = "cm")
  
  myplot <- generate_diversity_boxplot(immunocompromised_downsampled_genus_rare_alpha.df, variable = myvar,fill_palette = my_colour_palette_10_distinct,metric = "Simpson",variable_colours_available = T,rotate_x_text = T) +
    guides(fill = F, color = F) +
    # ggtitle("Simpson") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))
  ggsave(filename = paste0("Result_figures/diversity_analysis/immunocompromised/genus/genus_",myvar,"_downsampled_Simpson.pdf"),myplot, width = 10, height = 10,units = "cm")
  # ----------------------------------------------------------------
}

# -----------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------
# Publication figures

# immunocompromised, all
source("Code/helper_functions.R")
immunocompromised_otu_shannon_boxplot <- generate_significance_boxplots(mydata.df = immunocompromised_otu_rare_alpha.df,
                                                                        variable_column = "Lesion_type_refined",
                                                                        value_column = "Shannon",
                                                                        variable_colours_available = T,
                                                                        significances.df = immunocompromised_otu_alpha_diversity_significances.df,
                                                                        p_value_column = "Shannon_MannW_padj",
                                                                        sig_threshold = 0.05,
                                                                        fill_palette = NULL,
                                                                        sig_line_scaling_percentage = .07,
                                                                        sig_vjust = 0.4,
                                                                        sig_tip_length = 0.01,
                                                                        sig_linetype = 1,
                                                                        sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,8), breaks = seq(0,8, .5))
immunocompromised_otu_shannon_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompromised_otu_shannon_boxplot.pdf",
       plot = immunocompromised_otu_shannon_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompromised_otu_chao1_boxplot <- generate_significance_boxplots(mydata.df = immunocompromised_otu_rare_alpha.df,
                                                                      variable_column = "Lesion_type_refined",
                                                                      value_column = "Chao1",
                                                                      variable_colours_available = T,
                                                                      significances.df = immunocompromised_otu_alpha_diversity_significances.df,
                                                                      p_value_column = "Chao1_MannW_padj",
                                                                      sig_threshold = 0.05,
                                                                      fill_palette = NULL,
                                                                      sig_line_scaling_percentage = .07,
                                                                      sig_vjust = 0.4,
                                                                      sig_tip_length = 0.01,
                                                                      sig_linetype = 1,
                                                                      sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,750), breaks = seq(0,750, 50))
immunocompromised_otu_chao1_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompromised_otu_chao1_boxplot.pdf",
       plot = immunocompromised_otu_chao1_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompromised_genus_shannon_boxplot <- generate_significance_boxplots(mydata.df = immunocompromised_genus_rare_alpha.df,
                                                                          variable_column = "Lesion_type_refined",
                                                                          value_column = "Shannon",
                                                                          variable_colours_available = T,
                                                                          significances.df = immunocompromised_genus_alpha_diversity_significances.df,
                                                                          p_value_column = "Shannon_MannW_padj",
                                                                          sig_threshold = 0.05,
                                                                          fill_palette = NULL,
                                                                          sig_line_scaling_percentage = .07,
                                                                          sig_vjust = 0.4,
                                                                          sig_tip_length = 0.01,
                                                                          sig_linetype = 1,
                                                                          sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,7), breaks = seq(0,7, .5))

ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompromised_genus_shannon_boxplot.pdf",
       plot = immunocompromised_genus_shannon_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompromised_genus_chao1_boxplot <- generate_significance_boxplots(mydata.df = immunocompromised_genus_rare_alpha.df,
                                                                          variable_column = "Lesion_type_refined",
                                                                          value_column = "Chao1",
                                                                          variable_colours_available = T,
                                                                          significances.df = immunocompromised_genus_alpha_diversity_significances.df,
                                                                          p_value_column = "Chao1_MannW_padj",
                                                                          sig_threshold = 0.05,
                                                                          fill_palette = NULL,
                                                                          sig_line_scaling_percentage = .07,
                                                                          sig_vjust = 0.4,
                                                                          sig_tip_length = 0.01,
                                                                          sig_linetype = 1,
                                                                          sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,360), breaks = seq(0,350, 50))
immunocompromised_genus_chao1_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompromised_genus_chao1_boxplot.pdf",
       plot = immunocompromised_genus_chao1_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

# ------------------------------------------------------
# ------------------------------------------------------
# immunocompetent, all
immunocompetent_otu_shannon_boxplot <- generate_significance_boxplots(mydata.df = immunocompetent_otu_rare_alpha.df,
                                                                      variable_column = "Lesion_type_refined",
                                                                      value_column = "Shannon",
                                                                      variable_colours_available = T,
                                                                      significances.df = immunocompetent_otu_alpha_diversity_significances.df,
                                                                      p_value_column = "Shannon_MannW_padj",
                                                                      sig_threshold = 0.05,
                                                                      fill_palette = NULL,
                                                                      sig_line_scaling_percentage = .07,
                                                                      sig_vjust = 0.4,
                                                                      sig_tip_length = 0.01,
                                                                      sig_linetype = 1,
                                                                      sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,8), breaks = seq(0,8, .5))
immunocompetent_otu_shannon_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompetent_otu_shannon_boxplot.pdf",
       plot = immunocompetent_otu_shannon_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompetent_otu_chao1_boxplot <- generate_significance_boxplots(mydata.df = immunocompetent_otu_rare_alpha.df,
                                                                    variable_column = "Lesion_type_refined",
                                                                    value_column = "Chao1",
                                                                    variable_colours_available = T,
                                                                    significances.df = immunocompetent_otu_alpha_diversity_significances.df,
                                                                    p_value_column = "Chao1_MannW_padj",
                                                                    sig_threshold = 0.05,
                                                                    fill_palette = NULL,
                                                                    sig_line_scaling_percentage = .07,
                                                                    sig_vjust = 0.4,
                                                                    sig_tip_length = 0.01,
                                                                    sig_linetype = 1,
                                                                    sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,750), breaks = seq(0,750, 50))
immunocompetent_otu_chao1_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompetent_otu_chao1_boxplot.pdf",
       plot = immunocompetent_otu_chao1_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompetent_genus_shannon_boxplot <- generate_significance_boxplots(mydata.df = immunocompetent_genus_rare_alpha.df,
                                                                        variable_column = "Lesion_type_refined",
                                                                        value_column = "Shannon",
                                                                        variable_colours_available = T,
                                                                        significances.df = immunocompetent_genus_alpha_diversity_significances.df,
                                                                        p_value_column = "Shannon_MannW_padj",
                                                                        sig_threshold = 0.05,
                                                                        fill_palette = NULL,
                                                                        sig_line_scaling_percentage = .07,
                                                                        sig_vjust = 0.4,
                                                                        sig_tip_length = 0.01,
                                                                        sig_linetype = 1,
                                                                        sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,7), breaks = seq(0,7, .5))

ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompetent_genus_shannon_boxplot.pdf",
       plot = immunocompetent_genus_shannon_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompetent_genus_chao1_boxplot <- generate_significance_boxplots(mydata.df = immunocompetent_genus_rare_alpha.df,
                                                                      variable_column = "Lesion_type_refined",
                                                                      value_column = "Chao1",
                                                                      variable_colours_available = T,
                                                                      significances.df = immunocompetent_genus_alpha_diversity_significances.df,
                                                                      p_value_column = "Chao1_MannW_padj",
                                                                      sig_threshold = 0.05,
                                                                      fill_palette = NULL,
                                                                      sig_line_scaling_percentage = .07,
                                                                      sig_vjust = 0.4,
                                                                      sig_tip_length = 0.01,
                                                                      sig_linetype = 1,
                                                                      sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,360), breaks = seq(0,350, 50))
immunocompetent_genus_chao1_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompetent_genus_chao1_boxplot.pdf",
       plot = immunocompetent_genus_chao1_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

# ------------------------------------------------------
# ------------------------------------------------------
# immunocompromised, downsampled
immunocompromised_downsampled_otu_shannon_boxplot <- generate_significance_boxplots(mydata.df = immunocompromised_downsampled_otu_rare_alpha.df,
                                                                                    variable_column = "Lesion_type_refined",
                                                                                    value_column = "Shannon",
                                                                                    variable_colours_available = T,
                                                                                    significances.df = immunocompromised_downsampled_otu_alpha_diversity_significances.df,
                                                                                    p_value_column = "Shannon_MannW_padj",
                                                                                    sig_threshold = 0.05,
                                                                                    fill_palette = NULL,
                                                                                    sig_line_scaling_percentage = .07,
                                                                                    sig_vjust = 0.4,
                                                                                    sig_tip_length = 0.01,
                                                                                    sig_linetype = 1,
                                                                                    sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,7), breaks = seq(0,8, .5))
immunocompromised_downsampled_otu_shannon_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompromised_downsampled_otu_shannon_boxplot.pdf",
       plot = immunocompromised_downsampled_otu_shannon_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompromised_downsampled_otu_chao1_boxplot <- generate_significance_boxplots(mydata.df = immunocompromised_downsampled_otu_rare_alpha.df,
                                                                                  variable_column = "Lesion_type_refined",
                                                                                  value_column = "Chao1",
                                                                                  variable_colours_available = T,
                                                                                  significances.df = immunocompromised_downsampled_otu_alpha_diversity_significances.df,
                                                                                  p_value_column = "Chao1_MannW_padj",
                                                                                  sig_threshold = 0.05,
                                                                                  fill_palette = NULL,
                                                                                  sig_line_scaling_percentage = .07,
                                                                                  sig_vjust = 0.4,
                                                                                  sig_tip_length = 0.01,
                                                                                  sig_linetype = 1,
                                                                                  sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,600), breaks = seq(0,750, 50))
immunocompromised_downsampled_otu_chao1_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompromised_downsampled_otu_chao1_boxplot.pdf",
       plot = immunocompromised_downsampled_otu_chao1_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompromised_downsampled_genus_shannon_boxplot <- generate_significance_boxplots(mydata.df = immunocompromised_downsampled_genus_rare_alpha.df,
                                                                                      variable_column = "Lesion_type_refined",
                                                                                      value_column = "Shannon",
                                                                                      variable_colours_available = T,
                                                                                      significances.df = immunocompromised_downsampled_genus_alpha_diversity_significances.df,
                                                                                      p_value_column = "Shannon_MannW_padj",
                                                                                      sig_threshold = 0.05,
                                                                                      fill_palette = NULL,
                                                                                      sig_line_scaling_percentage = .07,
                                                                                      sig_vjust = 0.4,
                                                                                      sig_tip_length = 0.01,
                                                                                      sig_linetype = 1,
                                                                                      sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,6), breaks = seq(0,7, .5))
immunocompromised_downsampled_genus_shannon_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompromised_downsampled_genus_shannon_boxplot.pdf",
       plot = immunocompromised_downsampled_genus_shannon_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompromised_downsampled_genus_chao1_boxplot <- generate_significance_boxplots(mydata.df = immunocompromised_downsampled_genus_rare_alpha.df,
                                                                                    variable_column = "Lesion_type_refined",
                                                                                    value_column = "Chao1",
                                                                                    variable_colours_available = T,
                                                                                    significances.df = immunocompromised_downsampled_genus_alpha_diversity_significances.df,
                                                                                    p_value_column = "Chao1_MannW_padj",
                                                                                    sig_threshold = 0.05,
                                                                                    fill_palette = NULL,
                                                                                    sig_line_scaling_percentage = .07,
                                                                                    sig_vjust = 0.4,
                                                                                    sig_tip_length = 0.01,
                                                                                    sig_linetype = 1,
                                                                                    sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,360), breaks = seq(0,350, 50))
immunocompromised_downsampled_genus_chao1_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompromised_downsampled_genus_chao1_boxplot.pdf",
       plot = immunocompromised_downsampled_genus_chao1_boxplot,width = 10, height = 12,device = "pdf",units = "cm")


# ------------------------------------------------------
# ------------------------------------------------------
# immunocompetent, downsampled
immunocompetent_downsampled_otu_shannon_boxplot <- generate_significance_boxplots(mydata.df = immunocompetent_downsampled_otu_rare_alpha.df,
                                                                                  variable_column = "Lesion_type_refined",
                                                                                  value_column = "Shannon",
                                                                                  variable_colours_available = T,
                                                                                  significances.df = immunocompetent_downsampled_otu_alpha_diversity_significances.df,
                                                                                  p_value_column = "Shannon_MannW_padj",
                                                                                  sig_threshold = 0.05,
                                                                                  fill_palette = NULL,
                                                                                  sig_line_scaling_percentage = .07,
                                                                                  sig_vjust = 0.4,
                                                                                  sig_tip_length = 0.01,
                                                                                  sig_linetype = 1,
                                                                                  sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,7), breaks = seq(0,8, .5))
immunocompetent_downsampled_otu_shannon_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompetent_downsampled_otu_shannon_boxplot.pdf",
       plot = immunocompetent_downsampled_otu_shannon_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompetent_downsampled_otu_chao1_boxplot <- generate_significance_boxplots(mydata.df = immunocompetent_downsampled_otu_rare_alpha.df,
                                                                                variable_column = "Lesion_type_refined",
                                                                                value_column = "Chao1",
                                                                                variable_colours_available = T,
                                                                                significances.df = immunocompetent_downsampled_otu_alpha_diversity_significances.df,
                                                                                p_value_column = "Chao1_MannW_padj",
                                                                                sig_threshold = 0.05,
                                                                                fill_palette = NULL,
                                                                                sig_line_scaling_percentage = .07,
                                                                                sig_vjust = 0.4,
                                                                                sig_tip_length = 0.01,
                                                                                sig_linetype = 1,
                                                                                sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,600), breaks = seq(0,750, 50))
immunocompetent_downsampled_otu_chao1_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompetent_downsampled_otu_chao1_boxplot.pdf",
       plot = immunocompetent_downsampled_otu_chao1_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompetent_downsampled_genus_shannon_boxplot <- generate_significance_boxplots(mydata.df = immunocompetent_downsampled_genus_rare_alpha.df,
                                                                                    variable_column = "Lesion_type_refined",
                                                                                    value_column = "Shannon",
                                                                                    variable_colours_available = T,
                                                                                    significances.df = immunocompetent_downsampled_genus_alpha_diversity_significances.df,
                                                                                    p_value_column = "Shannon_MannW_padj",
                                                                                    sig_threshold = 0.05,
                                                                                    fill_palette = NULL,
                                                                                    sig_line_scaling_percentage = .07,
                                                                                    sig_vjust = 0.4,
                                                                                    sig_tip_length = 0.01,
                                                                                    sig_linetype = 1,
                                                                                    sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,6), breaks = seq(0,7, .5))
immunocompetent_downsampled_genus_shannon_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompetent_downsampled_genus_shannon_boxplot.pdf",
       plot = immunocompetent_downsampled_genus_shannon_boxplot,width = 10, height = 12,device = "pdf",units = "cm")

immunocompetent_downsampled_genus_chao1_boxplot <- generate_significance_boxplots(mydata.df = immunocompetent_downsampled_genus_rare_alpha.df,
                                                                                  variable_column = "Lesion_type_refined",
                                                                                  value_column = "Chao1",
                                                                                  variable_colours_available = T,
                                                                                  significances.df = immunocompetent_downsampled_genus_alpha_diversity_significances.df,
                                                                                  p_value_column = "Chao1_MannW_padj",
                                                                                  sig_threshold = 0.05,
                                                                                  fill_palette = NULL,
                                                                                  sig_line_scaling_percentage = .07,
                                                                                  sig_vjust = 0.4,
                                                                                  sig_tip_length = 0.01,
                                                                                  sig_linetype = 1,
                                                                                  sig_colour = "grey40") +
  xlab("Lesion type") + scale_y_continuous(limits = c(0,360), breaks = seq(0,350, 50))
immunocompetent_downsampled_genus_chao1_boxplot
ggsave(filename = "Result_figures/abundance_analysis_plots/boxplots/immunocompetent_downsampled_genus_chao1_boxplot.pdf",
       plot = immunocompetent_downsampled_genus_chao1_boxplot,width = 10, height = 12,device = "pdf",units = "cm")


# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Plot the Bacterial_load_CFU against the diversities
# plot_CFU <- function(mydata, variable_to_plot, metric, variable_colours_available = T, use_shapes = T){
#   internal_data.df <- mydata
#   internal_data.df[[variable_to_plot]] <- factor(internal_data.df[[variable_to_plot]])
#   variable_values <- levels(internal_data.df[[variable_to_plot]])
# 
#   # If variable colour column "variable_colour" in metadata, use colours from there
#   if (variable_colours_available == T){
#     colour_col_name <- paste0(variable_to_plot, "_colour")
#     variable_colours <- setNames(as.character(unique(internal_data.df[[colour_col_name]])), as.character(variable_values))
#   } else{
#     variable_colours <- setNames(colour_palette[1:length(variable_values)], variable_values)  
#   }
#   if (use_shapes == T){
#     variable_shapes <- setNames(rep(c(25,24,23,22,21),length(variable_values))[1:length(variable_values)],variable_values)
#   } else{
#     variable_shapes <- setNames(rep(c(21),length(variable_values))[1:length(variable_values)],variable_values)  
#   }
#   
#   correlations.df <- data.frame(row.names = unique(internal_data.df[,variable_to_plot]))
#   correlations.df[,variable_to_plot] <- rownames(correlations.df)
#   correlations.df$p_value_pearson <- NA
#   correlations.df$p_value_spearman <- NA
#   correlations.df$cor_value_pearson <- NA
#   correlations.df$cor_value_spearman <- NA
#   
#   for (group in unique(internal_data.df[,variable_to_plot])){
#     data_subset <- subset(internal_data.df, get(variable_to_plot) == group)
#     pearson_test <- with(data_subset, cor.test(Chao1, Bacterial_load_CFU,method = "pearson"))
#     spearman_test <- with(data_subset, cor.test(Chao1, Bacterial_load_CFU,method = "spearman", exact = F))
#     p_value_pearson <- round(pearson_test$p.value,4)
#     p_value_spearman <- round(spearman_test$p.value,4)
#     cor_value_pearson <- round(pearson_test$estimate,2)
#     cor_value_spearman <- round(spearman_test$estimate,2)
#     correlations.df[group,]$p_value_pearson <- p_value_pearson
#     correlations.df[group,]$p_value_spearman <- p_value_spearman
#     correlations.df[group,]$cor_value_pearson <- cor_value_pearson
#     correlations.df[group,]$cor_value_spearman <- cor_value_spearman
#   }
# 
#   myplot <- ggplot(internal_data.df, aes(x = get(metric), y = log(Bacterial_load_CFU, 10))) +
#     geom_point(aes(shape = get(variable_to_plot), fill = get(variable_to_plot))) +
#     geom_smooth(method = "lm",se = F,na.rm = T, linetype = "dashed", color = "black", size = .5) +
#     geom_text(data= correlations.df,parse = T, aes(x = -Inf, y = Inf, label = paste0("rho==", cor_value_pearson, "*','~italic(p)==", p_value_pearson)), hjust=-0.2, vjust=1.2, size = 3) +
#     geom_text(data= correlations.df,parse = T, aes(x = -Inf, y = Inf, label = paste0("r[s]==", cor_value_spearman, "*','~italic(p)==", p_value_spearman)), hjust=-0.2, vjust=3.2, size = 3) +
#     scale_shape_manual(values = variable_shapes, name = variable_to_plot)+
#     scale_fill_manual(values = variable_colours, name = variable_to_plot) +
#     ylab(expression(paste(log[10]~"(Bacterial load CFU)"))) +
#     xlab(metric) +
#     facet_wrap(~get(variable_to_plot)) +
#     common_theme +
#     theme(strip.text = element_text(size = 10))
#   
#   myplot
# }
  
