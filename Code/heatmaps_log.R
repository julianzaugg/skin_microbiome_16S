#************************************
# Build heatmaps at OTU/ASV and varying taxonomy levels for the skin microbiome dataset
# This version is designed for log transformed read counts
#************************************

# Uncomment and run to install the libraries that might be needed 
# source("http://bioconductor.org/biocLite.R")
# biocLite("DESeq2")
#####
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("mixOmics", version = "3.8")
#####
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
# library(DESeq2)
library(mixOmics)
library(pheatmap)

####################################
# Define various colour palletes
my_colour_pallete <- c("#8dd3c7","#ffffb3","#bebada","#fb8072", "#80b1d3", "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5", "#cc0000")
# From http://tools.medialab.sciences-po.fr/iwanthue/
my_colour_pallete_20 <- c("#66bd79","#a35bcf","#5bb643","#d14ea6","#a2b239","#5c6bcc","#dc892e","#5e93cd","#d64737","#49b6a8","#dc3c6e","#4f7e3c","#bd8cd5","#caab55","#914c88","#867230","#df82a2","#a65429","#ab4a5a","#e0896a")
my_colour_pallete_20_distinct <- c("#0057b4","#7fff56","#d600bc","#d8d500","#e76eff","#019932","#9f8fff","#ffc730","#007fac","#a20019","#06fefd","#ff6782","#00774c","#e0c8ff","#717a00","#4b2952","#e2ed7d","#46321e","#ffbd76","#ffb4c6")
my_colour_pallete_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_pallete_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
my_colour_pallete_15 <- c("#77b642","#7166d9","#cfa240","#b351bb","#4fac7f","#d44891","#79843a","#c68ad4","#d15a2c","#5ba7d9","#ce4355","#6570ba","#b67249","#9b4a6f","#df8398")
lesion_pallete_7 <- c("#8558d6","#6ee268","#d247ad","#c9d743","#d7453e","#59a237","#d78f2a")
my_colour_pallete_32_distinct <- c("#ea7e00","#ca0074","#d1c69b","#474007","#bb00ad","#9c80ff","#be3300","#542e72","#00b9f5","#09436b","#8b0036","#9ac8e6","#ff1059","#959eff","#154a11","#0290f4","#ff7762","#7dbf00","#ff8194","#834c00","#006e73","#f9bb5d","#d6c943","#017229","#00d3a8","#732427","#36e191","#6a8200","#efb3ea","#3227bb","#ff90e1","#e92a12")
####################################
####################################
gm_mean = function(x, na.rm=TRUE){
  # The geometric mean, with some error-protection bits.
  exp(sum(log(x[x > 0 & !is.na(x)]), na.rm=na.rm) / length(x))
}
# gm_mean = function(x, na.rm=TRUE){
#   exp(mean(log(x), na.rm=na.rm) )
# }

# Center log ratio transform
clr = function(x, base=2){
  x <- log((x / gm_mean(x)), base)
  x[!is.finite(x) | is.na(x)] <- 0.0
  return(x)
}
####################################

################################################################################################
# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")

# Load the metadata.df
metadata.df <- read.table("data/metadata.tsv", sep ="\t", header = T)

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC and SCC lesions. 
# Remove samples for different lesion types (nasal,scar,scar_PL,KA,KA_PL,VV,VV_PL,SF,SF_PL,other,other_PL) from metadata and otu table
metadata.df <- metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC"),]

pool_1 <- c("C","AK_PL","IEC_PL","SCC_PL")
pool_2 <- c("AK","IEC")
pool_3 <- c("AK_PL","IEC_PL","SCC_PL")


metadata.df$Sampletype_pooled <- factor(as.character(lapply(metadata.df$Sampletype, function(x) ifelse(x %in% pool_1, "NLC", ifelse(x %in% pool_2, "AK", "SCC")))))
metadata.df$Sampletype_pooled_IEC_sep <- factor(as.character(lapply(metadata.df$Sampletype, function(x) ifelse(x %in% pool_1, "NLC", ifelse(x == "AK", "AK", ifelse(x =="IEC", "IEC", "SCC"))))))
metadata.df$Sampletype_pooled_C_sep <- factor(as.character(lapply(metadata.df$Sampletype, function(x) ifelse(x %in% pool_3, "NLC", 
                                                                                                             ifelse(x %in% pool_2, "AK", 
                                                                                                                    ifelse(x == "C", "C","SCC"))))))

log_matrix <- function(mymat){
  out <- log(mymat, 10)
  out[is.infinite(out)] <- 0
  return(out)
}

# Load abundance matrices
otu_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/OTU_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_genus_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Genus_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_family_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Family_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_order_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Order_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_class_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Class_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_phylum_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Phylum_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))

# Cleanup column (sample) names
colnames(otu_rare_log.m) <- gsub("_J.*", "", colnames(otu_rare_log.m))
colnames(otu_genus_rare_log.m) <- gsub("_J.*", "", colnames(otu_genus_rare_log.m))
colnames(otu_family_rare_log.m) <- gsub("_J.*", "", colnames(otu_family_rare_log.m))
colnames(otu_order_rare_log.m) <- gsub("_J.*", "", colnames(otu_order_rare_log.m))
colnames(otu_class_rare_log.m) <- gsub("_J.*", "", colnames(otu_class_rare_log.m))
colnames(otu_phylum_rare_log.m) <- gsub("_J.*", "", colnames(otu_phylum_rare_log.m))

# and correct metadata
rownames(metadata.df) <- gsub("_J.*", "", rownames(metadata.df))
metadata.df$Index <- gsub("_J.*", "", metadata.df$Index)

# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
samples_removed <- metadata.df$Index[!metadata.df$Index %in% colnames(otu_rare_log.m)]
metadata.df <- metadata.df[! metadata.df$Index %in% samples_removed,]
metadata.df$Patient <- factor(metadata.df$Patient)
metadata.df$Sampletype <- factor(metadata.df$Sampletype)

# Remove samples that are not in the metadata.
otu_rare_log.m <- otu_rare_log.m[,colnames(otu_rare_log.m) %in% metadata.df$Index]
otu_genus_rare_log.m <- otu_genus_rare_log.m[,colnames(otu_genus_rare_log.m) %in% metadata.df$Index]
otu_family_rare_log.m <- otu_family_rare_log.m[,colnames(otu_family_rare_log.m) %in% metadata.df$Index]
otu_order_rare_log.m <- otu_order_rare_log.m[,colnames(otu_order_rare_log.m) %in% metadata.df$Index]
otu_class_rare_log.m <- otu_class_rare_log.m[,colnames(otu_class_rare_log.m) %in% metadata.df$Index]
otu_phylum_rare_log.m <- otu_phylum_rare_log.m[,colnames(otu_phylum_rare_log.m) %in% metadata.df$Index]


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

# Function to create heatmap
make_lesion_heatmap <- function(myheatmap,
                                filename,
                                height,
                                width,
                                clust_cols = F,
                                clust_rows = F,
                                treeheight_col = 0,
                                treeheight_row = 0,
                                cutree_cols = 1){
  internal_heatmap <- myheatmap
  # First create an annotations dataframe specify colours for groups, e.g. lesions
  sample_annotations.df <- data.frame(row.names = colnames(internal_heatmap))
  sample_annotations.df <- cbind(sample_annotations.df, Lesion_type  = NA, Lesion_colour = NA)
  
  # Determine the lesion type for each sample
  for (sample_id in colnames(internal_heatmap)){
    # sample_lesion_type <- paste(metadata.df[metadata.df$Index == sample_id,]$Sampletype)
    sample_lesion_type <- paste(metadata.df[metadata.df$Index == sample_id,]$Sampletype_pooled_IEC_sep)
    sample_patient <- paste(metadata.df[metadata.df$Index == sample_id,]$Patient)
    if (sample_lesion_type == "" | is.na(sample_lesion_type)){
      sample_lesion_type <- "Unassigned"
    }
    sample_annotations.df[sample_id, "Lesion_type"] <- sample_lesion_type
    sample_annotations.df[sample_id, "Patient"] <- sample_patient
  }
  # Reassign the ordering found in the metadata
  sample_annotations.df$Lesion_type <- factor(sample_annotations.df$Lesion_type, levels = levels(metadata.df$Sampletype_pooled_IEC_sep))
  
  # Assign a distinct colour to each unique lesion type
  matched_colours <- NULL
  i = 1
  for (lt in unique(sample_annotations.df$Lesion_type)){
    matched_colours <- rbind(matched_colours, data.frame(Group = lt, Colour = lesion_pallete_7[i]))
    i = i + 1
  }
  # And store in annotations dataframe
  sample_annotations.df$Lesion_colour <- unlist(lapply(sample_annotations.df$Lesion_type, function(x) as.character(matched_colours[matched_colours$Group == x,]$Colour)))
  
  # Assign a distinct colour to each unique patient
  matched_colours <- NULL
  i = 1
  for (patient in unique(sample_annotations.df$Patient)){
    matched_colours <- rbind(matched_colours, data.frame(Group = patient, Colour = my_colour_pallete_32_distinct[i]))
    i = i + 1
  }
  # And store in annotations dataframe
  sample_annotations.df$Patient_colour <- unlist(lapply(sample_annotations.df$Patient, function(x) as.character(matched_colours[matched_colours$Group == x,]$Colour)))
  ########################
  # Specify colour scale for heatmap cells
  ## White - yellow - blue
  my_palette <- colorRampPalette(c("white", "#ffffcc","#cce1b8", "#91cabc", "#61b4c1","#335fa5","#28387a", "#071447"))(13)
  
  # Order heatmap by lesion type and patient
  heatmap_ordered.m <- internal_heatmap[,order(sample_annotations.df$Lesion_type, sample_annotations.df$Patient)]
  # heatmap_ordered.m <- internal_heatmap[,order(sample_annotations.df$Patient, sample_annotations.df$Lesion_type)]
  
  # Order the annotations by the lesion type and patient
  sample_annotations.df <- sample_annotations.df[order(sample_annotations.df$Lesion_type, sample_annotations.df$Patient),]
  # sample_annotations.df <- sample_annotations.df[order(sample_annotations.df$Patient, sample_annotations.df$Lesion_type),]

  # Order heatmap by lesion type
  # heatmap_ordered.m <- internal_heatmap[,order(sample_annotations.df$Lesion_type)]
  
  # Order the annotations by the lesion type
  # sample_annotations.df <- sample_annotations.df[order(sample_annotations.df$Lesion_type),]
  
  # Create an annotation list for each unique group-colour
  # print(sample_annotations.df)
  annotation_colours_lesion <- unique(sample_annotations.df$Lesion_colour)
  names(annotation_colours_lesion) <- unique(sample_annotations.df$Lesion_type)
  annotation_colours_patient <- unique(sample_annotations.df$Patient_colour)
  names(annotation_colours_patient) <- unique(sample_annotations.df$Patient)
  annotation_colours <- list(Lesion_type = annotation_colours_lesion, Patient = annotation_colours_patient)
  
  annotation_breaks <- unlist(lapply(unique(sample_annotations.df[,"Lesion_type"]), function(x) { min(which(sample_annotations.df[,"Lesion_type"] == x))})) - 1
  
  
  # pdf("Result_figures/heatmaps/lesion_genus_heatmap.pdf",height=15,width=40)
  pdf(filename,height=height,width=width)
  setHook("grid.newpage", function() pushViewport(viewport(x=1,y=1,width=0.9, height=0.9, name="vp", just=c("right","top"))), action="prepend")
  pheatmap(heatmap_ordered.m,
           treeheight_row = treeheight_row, # To hide row tree
           treeheight_col = treeheight_col, # To hide column tree
           cutree_cols = cutree_cols,
           # treeheight_col = 100,
           # gaps_col = annotation_breaks,
           #breaks = my_breaks,
           # scale = "column",
           annotation_names_col = F,
           color = my_palette,
           fontsize_row = 6,
           fontsize_col = 6,
           cluster_rows = clust_rows,
           cluster_cols = clust_cols,
           # legend_breaks = my_breaks,
           # legend_labels = my_breaks,
           annotation_col  = sample_annotations.df[,c("Lesion_type","Patient"),  drop = FALSE],
           annotation_colors = annotation_colours)
  setHook("grid.newpage", NULL, "replace")
  grid.text("Sample", y=-0.02, x =0.43, gp=gpar(fontsize=16))
  grid.text("Taxa", x=-0.02, y = 0.5, rot=90, gp=gpar(fontsize=16))
  dev.off()
}


generate_breaks = function(x, n, center = F){
  if(center){
    m = max(abs(c(min(x, na.rm = T), max(x, na.rm = T))))
    res = seq(-m, m, length.out = n + 1)
  }
  else{
    res = seq(min(x, na.rm = T), max(x, na.rm = T), length.out = n + 1)
  }
  
  return(res)
}
log(150,10)
log(300,10)
log(600,10)
log(1500,10)
0.05*30000
10**4
# 2.176091 = 150 reads (0.5 %)
heatmap_genus.m <- filter_heatmap_matrix(otu_genus_rare_log.m, row_max = 2.176091, prevalence = 0.3)
heatmap_class.m <- filter_heatmap_matrix(otu_class_rare_log.m, row_max = 2.176091, prevalence = 0.0)

# heatmap_rel_genus.m <- otu_genus_rel_rare.m[rownames(heatmap_genus.m),]

# If it’s not too much hassle, a heatmap based on the relative abundance would be really great.
# Is it possible to order the lesion type so that we see NLCs on left, then AK, IEC next, and SCC all the way to the right?
metadata.df$Sampletype_pooled_IEC_sep <- factor(metadata.df$Sampletype_pooled_IEC_sep, levels = c("NLC","AK","IEC","SCC"))
heatmap_genus.m <- filter_heatmap_matrix(otu_genus_rare_log.m, row_max = 2.176091, prevalence = 0.0)
make_lesion_heatmap(heatmap_genus.m,filename = "Result_figures/heatmaps/lesion_genus_heatmap_log_read_count.pdf", 25,35,clust_cols = T,clust_rows = T,treeheight_col = 60,treeheight_row = 60,cutree_cols = 1)
make_lesion_heatmap(heatmap_genus.m,filename = "Result_figures/heatmaps/lesion_genus_heatmap_log_read_count2.pdf", 25,35,clust_cols = F,clust_rows = T,treeheight_col = 60,treeheight_row = 60)

make_lesion_heatmap(heatmap_genus.m,filename = "Result_figures/heatmaps/lesion_genus_heatmap_log_read_count_clustered_colrow.pdf", 15,35,clust_cols = T,clust_rows = T)
make_lesion_heatmap(heatmap_class.m,filename = "Result_figures/heatmaps/lesion_class_heatmap_log_read_count_clustered_colrow.pdf", 10,30,clust_cols = T,clust_rows = T)

make_lesion_heatmap(heatmap_genus.m,filename = "Result_figures/heatmaps/lesion_genus_heatmap_log_read_count_clustered_row.pdf", 15,35,clust_cols = F,clust_rows = T)
make_lesion_heatmap(heatmap_class.m,filename = "Result_figures/heatmaps/lesion_class_heatmap_log_read_count_clustered_row.pdf", 10,30,clust_cols = F,clust_rows = T)

