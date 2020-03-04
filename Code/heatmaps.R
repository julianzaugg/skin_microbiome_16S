#************************************
# Build heatmaps at OTU/ASV and varying taxonomy levels for the skin microbiome dataset
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
# library(mixOmics)
library(pheatmap)
library(grid)

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
my_colour_pallete_10_distinct <- c("#8eec45","#0265e8","#f6a800","#bf6549","#486900","#c655a0","#00d1b6","#ff4431","#aeb85c","#7e7fc8")
patient_pallete_45 <- c("#d64530","#585fb1","#795d97","#9e4773","#3f6921","#71692c","#a2b93c","#d571cc","#9b3e97","#33947a","#98ad66","#448a4e","#869ae0","#5ce7af","#e085a3","#dfdc87","#d19be2","#5cb735","#e38269","#3db6c0","#50b565","#50902c","#a98a2c","#dde84a","#db3d76","#5fe485","#7c8329","#b3e791","#6fe965","#5ebce9","#3c86c1","#2a6a45","#65b688","#6651d1","#af4ed3","#df872f","#56e4db","#737cea","#ac464b","#dd37b5","#995b2b","#daac6f","#92e2be","#a2e24b","#e0be3a")
####################################
####################################
gm_mean = function(x, na.rm=TRUE){
  # The geometric mean, with some error-protection bits.
  exp(sum(log(x[x > 0 & !is.na(x)]), na.rm=na.rm) / length(x))
}
# gm_mean = function(x, na.rm=TRUE){
#   exp(mean(log(x), na.rm=na.rm) )
# }

log_matrix <- function(mymat){
  out <- log(mymat, 10)
  out[is.infinite(out)] <- 0
  return(out)
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

################################################################################################
# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")

# Load the metadata.df
# metadata.df <- read.table("data/metadata.tsv", sep ="\t", header = T)

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# metadata.df <- metadata.df[metadata.df$Project == "immunocompetent",]
# metadata.df <- metadata.df[metadata.df$Project == "immunosuppressed",]

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

# Load count matrices and covert to log space
otu_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/OTU_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_genus_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Genus_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_family_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Family_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_order_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Order_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_class_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Class_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_phylum_rare_log.m <- log_matrix(as.matrix(read.table(file = "Result_tables/count_tables/Phylum_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))


# Load count matrices and apply CLR transformation
otu_rare_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/OTU_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_species_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Specie_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_genus_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Genus_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_family_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Family_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_order_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Order_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_class_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Class_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_phylum_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Phylum_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))

# Or if we just want to use abundances
otu_genus_rel_rare.m <- as.matrix(read.table(file = "Result_tables/relative_abundance_tables/Genus_relative_abundances_rarefied.csv", sep = ",", header = T, row.names = 1))
otu_class_rel_rare.m <- as.matrix(read.table(file = "Result_tables/relative_abundance_tables/Class_relative_abundances_rarefied.csv", sep = ",", header = T, row.names = 1))

# Cleanup column (sample) names - log matrices
colnames(otu_rare_log.m) <- gsub("_J.*", "", colnames(otu_rare_log.m))
colnames(otu_genus_rare_log.m) <- gsub("_J.*", "", colnames(otu_genus_rare_log.m))
colnames(otu_family_rare_log.m) <- gsub("_J.*", "", colnames(otu_family_rare_log.m))
colnames(otu_order_rare_log.m) <- gsub("_J.*", "", colnames(otu_order_rare_log.m))
colnames(otu_class_rare_log.m) <- gsub("_J.*", "", colnames(otu_class_rare_log.m))
colnames(otu_phylum_rare_log.m) <- gsub("_J.*", "", colnames(otu_phylum_rare_log.m))

# Cleanup column (sample) names - CLR matrices
colnames(otu_rare_clr.m) <- gsub("_J.*", "", colnames(otu_rare_clr.m))
colnames(otu_rare_species_clr.m) <- gsub("_J.*", "", colnames(otu_rare_species_clr.m))
colnames(otu_rare_genus_clr.m) <- gsub("_J.*", "", colnames(otu_rare_genus_clr.m))
colnames(otu_rare_family_clr.m) <- gsub("_J.*", "", colnames(otu_rare_family_clr.m))
colnames(otu_rare_order_clr.m) <- gsub("_J.*", "", colnames(otu_rare_order_clr.m))
colnames(otu_rare_class_clr.m) <- gsub("_J.*", "", colnames(otu_rare_class_clr.m))
colnames(otu_rare_phylum_clr.m) <- gsub("_J.*", "", colnames(otu_rare_phylum_clr.m))


colnames(otu_genus_rel_rare.m) <- gsub("_J.*", "", colnames(otu_genus_rel_rare.m))
colnames(otu_class_rel_rare.m) <- gsub("_J.*", "", colnames(otu_class_rel_rare.m))


# and correct metadata
rownames(metadata.df) <- gsub("_J.*", "", rownames(metadata.df))
metadata.df$Index <- gsub("_J.*", "", metadata.df$Index)



################################################################
##### For evaluating scale of clr values compared to counts and abundances
# otu_rare_phylum.m <- as.matrix(read.table(file = "Result_tables/count_tables/Phylum_counts_rarefied.csv", sep = ",", header = T, row.names = 1))
# temp <- melt(sort(otu_rare_phylum.m[,"R1369_J1477"]))
# colSums(otu_rare_phylum.m)["R1369_J1477"]
# temp2 <- melt(sort(clr(otu_rare_phylum.m)[,"R1369_J1477"]))
# temp2$counts <- temp[rownames(temp2),]
# temp <- melt(sort(otu_rare_phylum.m[,"R1369_J1477"])/colSums(otu_rare_phylum.m)["R1369_J1477"])
# temp2$abundances <- temp[rownames(temp2),]
temp2$abundances <- temp2$abundances * 100
################################################################

# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
samples_removed <- metadata.df$Index[!metadata.df$Index %in% colnames(otu_genus_rare_log.m)]
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

# Remove samples that are not in the metadata.
otu_rare_clr.m <- otu_rare_clr.m[,colnames(otu_rare_clr.m) %in% metadata.df$Index]
otu_rare_species_clr.m <- otu_rare_species_clr.m[,colnames(otu_rare_species_clr.m) %in% metadata.df$Index]
otu_rare_genus_clr.m <- otu_rare_genus_clr.m[,colnames(otu_rare_genus_clr.m) %in% metadata.df$Index]
otu_rare_family_clr.m <- otu_rare_family_clr.m[,colnames(otu_rare_family_clr.m) %in% metadata.df$Index]
otu_rare_order_clr.m <- otu_rare_order_clr.m[,colnames(otu_rare_order_clr.m) %in% metadata.df$Index]
otu_rare_class_clr.m <- otu_rare_class_clr.m[,colnames(otu_rare_class_clr.m) %in% metadata.df$Index]
otu_rare_phylum_clr.m <- otu_rare_phylum_clr.m[,colnames(otu_rare_phylum_clr.m) %in% metadata.df$Index]


otu_genus_rel_rare.m <- otu_genus_rel_rare.m[,colnames(otu_genus_rel_rare.m) %in% metadata.df$Index]
otu_class_rel_rare.m <- otu_class_rel_rare.m[,colnames(otu_class_rel_rare.m) %in% metadata.df$Index]

# If there are negative values in the CLR matrices, assign them a value of zero
otu_rare_clr.m[which(otu_rare_clr.m < 0)] <- 0
otu_rare_species_clr.m[which(otu_rare_species_clr.m < 0)] <- 0
otu_rare_genus_clr.m[which(otu_rare_genus_clr.m < 0)] <- 0
otu_rare_family_clr.m[which(otu_rare_family_clr.m < 0)] <- 0
otu_rare_order_clr.m[which(otu_rare_order_clr.m < 0)] <- 0
otu_rare_class_clr.m[which(otu_rare_class_clr.m < 0)] <- 0
otu_rare_phylum_clr.m[which(otu_rare_phylum_clr.m < 0)] <- 0




# Function to create heatmap
make_heatmap <- function(myheatmap_matrix,
                         filename,
                         height,
                         width,
                         variables = "DX_Groups",
                         clust_cols = F,
                         clust_rows = F,
                         treeheight_col = 0,
                         treeheight_row = 0,
                         cutree_cols = 1, 
                         annotation_pallete){
  # internal_heatmap_matrix.m <- otu_family_rare_log.m
  # variables = c("DX_Groups", "Sample_Type")
  # sample_annotations.df <- data.frame(matrix(nrow = length(colnames(internal_heatmap_matrix.m)), ncol = length(variables)))
  
  internal_heatmap_matrix.m <- myheatmap_matrix
  # metadata.df$label <- paste0("Patient ", metadata.df[colnames(otu_phylum_rare_log.m),"Sample_No"], "; ", colnames(otu_phylum_rare_log.m))
  # colnames(internal_heatmap_matrix.m) <- lapply(colnames(internal_heatmap_matrix.m), function(x) paste0("Patient ", metadata.df[x,"Sample_No"], "; ", x))
  # print(colnames(internal_heatmap_matrix.m))
  sample_annotations.df <- data.frame(matrix(nrow = length(colnames(internal_heatmap_matrix.m)), ncol = length(variables)))
  rownames(sample_annotations.df) <- colnames(internal_heatmap_matrix.m)
  colnames(sample_annotations.df) <- variables
  # Determine the values for each variable for each sample
  for (sample_id in rownames(sample_annotations.df)){
    for (var in variables){
      sample_variable_value <- paste(metadata.df[metadata.df$Index == sample_id,var])
      sample_annotations.df[sample_id, var] <- sample_variable_value
    }
  }
  
  # Assign colours for each variable group
  for (var in variables){
    # Assign a distinct colour to each unique value for each variable
    matched_colours <- NULL
    i = 1
    for (var_val in unique(sample_annotations.df[,var])){
      if (is.na(var_val) | var_val == "" | var_val == "NA"){
        matched_colours <- rbind(matched_colours, data.frame(Group = var_val, Colour = "grey"))  
      } else{
        matched_colours <- rbind(matched_colours, data.frame(Group = var_val, Colour = annotation_pallete[i]))  
      }
      i = i + 1
    }
    sample_annotations.df[,paste0(var, "_colour")] <- unlist(lapply(sample_annotations.df[,var], function(x) as.character(matched_colours[matched_colours$Group == x,]$Colour)))
  }
  
  
  # Reassign the ordering found in the metadata
  # for (var in variables){
  #   sample_annotations.df[,var] <- factor(sample_annotations.df$Lesion_type, levels = levels(metadata.df$Sampletype_pooled_IEC_sep))
  # }
  
  # Colour pallete for heatmap
  my_palette <- colorRampPalette(c("white", "#ffffcc","#cce1b8", "#91cabc", "#61b4c1","#335fa5","#28387a", "#071447"))(13)
  
  heatmap_ordered.m <- internal_heatmap_matrix.m[,do.call(order, sample_annotations.df[variables])]
  sample_annotations.df <- sample_annotations.df[do.call(order, sample_annotations.df[variables]),]
  
  # heatmap_ordered.m <- internal_heatmap_matrix.m[,do.call(order, sample_annotations.df[, c("DX_Groups")])]
  # sample_annotations.df <- sample_annotations.df[do.call(order, sample_annotations.df[, c("DX_Groups")]),]
  
  # Create an annotation list for each unique group-colour
  annotation_colours <- list()
  for (var in variables){
    annotation_colours_var <- unique(sample_annotations.df[,paste0(var, "_colour")])
    names(annotation_colours_var) <- unique(sample_annotations.df[,var])
    annotation_colours[[var]] <- annotation_colours_var
  }
  
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
           border_color = "red",
           annotation_names_col = T,
           color = my_palette,
           fontsize_row = 6,
           fontsize_col = 6,
           cluster_rows = clust_rows,
           cluster_cols = clust_cols,
           # legend_breaks = my_breaks,
           # legend_labels = my_breaks,
           annotation_col  = sample_annotations.df[,variables,  drop = FALSE],
           annotation_colors = annotation_colours,
           drop_levels = T,
           
  )
  setHook("grid.newpage", NULL, "replace")
  grid.text("Sample", y=-0.02, x =0.43, gp=gpar(fontsize=16))
  grid.text("Taxa", x=-0.02, y = 0.5, rot=90, gp=gpar(fontsize=16))
  dev.off()
}

# from pheatmap
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

# 2.176091 = 150 reads (0.5 %)
heatmap_genus.m <- filter_heatmap_matrix(otu_genus_rare_log.m, row_max = 2.176091, prevalence = 0.3)
heatmap_class.m <- filter_heatmap_matrix(otu_class_rare_log.m, row_max = 2.176091, prevalence = 0.0)
heatmap_family.m <- filter_heatmap_matrix(otu_family_rare_log.m, row_max = 2.176091, prevalence = 0.0)
heatmap_phylum.m <- filter_heatmap_matrix(otu_phylum_rare_log.m, row_max = 1, prevalence = 0.1)
variables <- c( "Project", "Sampletype_pooled", "Sampletype", "Patient")

heatmap_family_clr.m <- filter_heatmap_matrix(otu_rare_family_clr.m, row_max = 0, prevalence = 0.2)


make_heatmap(myheatmap_matrix = heatmap_family.m,
             height = 20,
             width = 75,
             filename = "Result_figures/heatmaps/family_log_read_count.pdf", 
             variables = variables,
             clust_cols = T,clust_rows = T, treeheight_col = 60, treeheight_row = 60, cutree_cols = 1,
             annotation_pallete = my_colour_pallete_10_distinct)

make_heatmap(myheatmap_matrix = heatmap_family_clr.m,
             height = 10,
             width = 75,
             filename = "Result_figures/heatmaps/family_clr.pdf", 
             variables = variables,
             clust_cols = F,clust_rows = T, treeheight_col = 60, treeheight_row = 60, cutree_cols = 1,
             annotation_pallete = patient_pallete_45)
             # annotation_pallete = my_colour_pallete_10_distinct)


