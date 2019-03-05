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
metadata.df$Sampletype_pooled_C_sep <- factor(as.character(lapply(metadata.df$Sampletype, function(x) ifelse(x %in% pool_3, "NLC", 
                                                                                                             ifelse(x %in% pool_2, "AK", 
                                                                                                                    ifelse(x == "C", "C","SCC"))))))

# Load count matrices and apply CLR transformation
otu_rare_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/OTU_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_species_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Specie_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_genus_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Genus_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_family_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Family_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_order_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Order_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_class_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Class_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))
otu_rare_phylum_clr.m <- clr(as.matrix(read.table(file = "Result_tables/count_tables/Phylum_counts_rarefied.csv", sep = ",", header = T, row.names = 1)))


# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
samples_removed <- metadata.df$Index[!metadata.df$Index %in% colnames(otu_rare_clr.m)]
metadata.df <- metadata.df[! metadata.df$Index %in% samples_removed,]
metadata.df$Patient <- factor(metadata.df$Patient)
metadata.df$Sampletype <- factor(metadata.df$Sampletype)


# Remove samples that are not in the metadata.
otu_rare_clr.m <- otu_rare_clr.m[,colnames(otu_rare_clr.m) %in% metadata.df$Index]
otu_rare_species_clr.m <- otu_rare_species_clr.m[,colnames(otu_rare_species_clr.m) %in% metadata.df$Index]
otu_rare_genus_clr.m <- otu_rare_genus_clr.m[,colnames(otu_rare_genus_clr.m) %in% metadata.df$Index]
otu_rare_family_clr.m <- otu_rare_family_clr.m[,colnames(otu_rare_family_clr.m) %in% metadata.df$Index]
otu_rare_order_clr.m <- otu_rare_order_clr.m[,colnames(otu_rare_order_clr.m) %in% metadata.df$Index]
otu_rare_class_clr.m <- otu_rare_class_clr.m[,colnames(otu_rare_class_clr.m) %in% metadata.df$Index]
otu_rare_phylum_clr.m <- otu_rare_phylum_clr.m[,colnames(otu_rare_phylum_clr.m) %in% metadata.df$Index]

# If there are negative values, assign them a value of zero
otu_rare_clr.m[which(otu_rare_clr.m < 0)] <- 0
otu_rare_species_clr.m[which(otu_rare_species_clr.m < 0)] <- 0
otu_rare_genus_clr.m[which(otu_rare_genus_clr.m < 0)] <- 0
otu_rare_family_clr.m[which(otu_rare_family_clr.m < 0)] <- 0
otu_rare_order_clr.m[which(otu_rare_order_clr.m < 0)] <- 0
otu_rare_class_clr.m[which(otu_rare_class_clr.m < 0)] <- 0
otu_rare_phylum_clr.m[which(otu_rare_phylum_clr.m < 0)] <- 0


# heatmap.m <- otu_rare_clr.m
# heatmap.m <- otu_rare_species_clr.m
# heatmap.m <- otu_rare_genus_clr.m
# heatmap.m <- otu_rare_family_clr.m
# heatmap.m <- otu_rare_order_clr.m
# heatmap.m <- otu_rare_class_clr.m
heatmap.m <- otu_rare_phylum_clr.m


# Determine the corresponding metadata.df group for the sample id in the OTU table
# Change the metadata.df variable as desired
my_groups <- c()
for (sample_id in colnames(heatmap.m)){
  sample_group <- paste(metadata.df[metadata.df$Index == sample_id,]$Sampletype)
  # Combination of two variables
  # sample_group <- paste(metadata.df[metadata.df$sample_name == sample_id,]$preservation, 
  #                       metadata.df[metadata.df$sample_name == sample_id,]$treatment, sep = ";")
  if (sample_group == ""){
    sample_group <- "Unassigned"
  }
  my_groups <- c(my_groups, sample_group)
}

# Assign a distinct colour to each unique group
matched_colours <- NULL
i = 1
for (grp in unique(my_groups)){
  matched_colours <- rbind(matched_colours, data.frame(Group = grp, Colour = my_colour_pallete_20_distinct[i]))
  i = i + 1
}
# Generate the full colour vector; one colour for each entry in my_groups
my_group_colours <- as.vector(unlist(lapply(my_groups, function(x) {matched_colours[matched_colours$Group== x,]$Colour[1]})))

##################################################
# Colours in the OTU heatmap

## White - yellow - blue
my_palette <- colorRampPalette(c("white", "#ffffcc","#cce1b8", "#91cabc", "#61b4c1","#335fa5","#28387a", "#071447"))(13)

## Blue to red
# my_palette <- colorRampPalette(rev(brewer.pal(name = "RdBu", n = 11)))(13)
## White to red
# my_palette <- brewer.pal(name = "Reds", n = 9)
# my_palette[1] <- "white"
# my_palette <- colorRampPalette(my_palette)(13)
###
#my_palette <- colorRampPalette(c("navy", "yellow", "firebrick3"))(50)
# my_palette <- colorRampPalette(rev(brewer.pal(n = 7, name ="RdYlBu")))(30)

# And the breaks at which the colours are assigned
#col_breaks = c(0,0.5,1,5,10,20,30,40,50,60,70,80,90,100)
col_breaks = clr(c(0,0.5,1,5,10,20,30,40,50,60,70,80,90,100))
col_breaks[col_breaks < 0] <- 0
col_breaks <- round(col_breaks,2)

############
# Function to build the legend for the col breaks
build_col_break_legend <- function(){
  current=2
  previous=1
  col_break_legend=c()
  while (current <= length(col_breaks)){
    legend_entry <- paste(col_breaks[previous], col_breaks[current], sep ="-")
    previous <- current
    current <- current + 1
    col_break_legend <- c(col_break_legend, legend_entry)
  }
  return(col_break_legend)
}
col_break_legend <- build_col_break_legend()
############

# Optional - order the heatmap by the colours (hence groups)
heatmap_ordered.m <- heatmap.m[,order(my_group_colours)]
# Need to also order the colour vector
my_group_colours_ordered <- as.vector(my_group_colours[order(my_group_colours)])

# Optional - order the heatmap by the groups, allowing colours of similar hue to be separated
heatmap_ordered.m <- heatmap.m[,order(my_groups)]
# Need to also order the colour vector
my_group_colours_ordered <- as.vector(my_group_colours[order(my_groups)])

####################################################################################################
# Testing heatmap creation using CIM from mixomics

# pdf("Result_figures/relative_abundance_heatmap.pdf", height = 20, width = 30)
# cim(heatmap_ordered.m,
#     color = my_palette,
#     #comp = c(2,1),
#     #comp = c(2),
#     cluster = "none",
#     col.sideColors = cbind(my_group_colours_ordered,my_group_colours_ordered),
#     #margins = c(10,25),
#     scale = T,
#     center = F,
#     transpose = F,
#     keysize = c(1, .5),
# )
# dev.off()
# ####################################################################################################

pheatmap(heatmap_ordered.m,
         color = my_palette,
         cluster_rows = F, 
         cluster_cols = F,
         annotation_colors = my_group_colours_ordered)
# ####
# heatmap.2(heatmap_ordered.m,
#           col = my_palette, # color palette defined earlier
#           # breaks=col_breaks, # enable color transition at specified limits
#           ColSideColors = my_group_colours_ordered, # Colour row for columns
#           # Comment to order rows by name, alphabetically. If NULL, turns off automatic row clustering.
#           Rowv = NULL,
#           # Uncomment to order columns by their existing ordering, e.g. group if the matrix columns have previously been ordered by group. 
#           # If NULL, turns off automatic column clustering.
#           Colv = NULL,
#           sepcolor = "grey95",
#           sepwidth=c(0.01,0.01),
#           colsep = 1:ncol(heatmap_ordered.m),
#           rowsep = 1:nrow(heatmap_ordered.m),
#           trace = "none", # turns off trace lines inside the heat map
#           dendrogram = "none", # turn off drawing a dendrogram
#           key = T)

########################################################################
## RUN ONE OF THESE LINES BEFORE PLOTTING HEATMAP TO SAVE
## May need to adjust parameters as required
## Height and Width changes here will have major effects on how squished the plot is
# To save as a pdf
pdf("Result_figures/relative_abundance_heatmap.pdf",height=40,width=50)
# To save as a jpeg
# jpeg("Result_figures/relative_abundance_samples_heatmap.jpeg",quality = 300,res = 300,units = "cm",height=65,width=75)
# To save as a svg
# svg("Result_figures/relative_abundance_samples_heatmap.svg",height=28,width=43)
########################################################################

# See ?heatmap.2 for more details on the plotting parameters
heatmap.2(heatmap_ordered.m,
          col = my_palette, # color palette defined earlier
          # breaks=col_breaks, # enable color transition at specified limits
          ColSideColors = my_group_colours_ordered, # Colour row for columns
          # Comment to order rows by name, alphabetically. If NULL, turns off automatic row clustering.
          Rowv = NULL,
          # Uncomment to order columns by their existing ordering, e.g. group if the matrix columns have previously been ordered by group. 
          # If NULL, turns off automatic column clustering.
          Colv = NULL,
          sepcolor = "grey95",
          sepwidth=c(0.01,0.01),
          colsep = 1:ncol(heatmap_ordered.m),
          rowsep = 1:nrow(heatmap_ordered.m),
          trace = "none", # turns off trace lines inside the heat map
          dendrogram = "none", # turn off drawing a dendrogram
          key = T, # hide the colour key
          #margins=c(10,60), # height, width margins around plot. Width typically needs to be high to show full string
          margins=c(10,10), # height, width margins around plot. Width typically needs to be high to show full string
          # Column width. 
          # The first value will control the space on the left hand side of the plot
          # The second the actual size of columns in the plot.
          lwid=c(1, 10), 
          # Row height. 
          # The first value will control the space on the top of the plot. 
          # The second will influence the size (height) of the rows.
          lhei = c(.01,15,1), 
          cexRow = 1,# text size rows
          cexCol = 1 # text size cols 
)

legend(x = 0.01,y =.7,      # location of the legend on the heatmap plot
       legend = unique(my_groups[order(my_groups)]), # category labels
       col = unique(my_group_colours_ordered), # color key
       lty= 1,             # line style
       lwd = 5,            # line width
       cex = 1,
       title = "Group")



legend(x = 0.01,y =.85,      # location of the legend on the heatmap plot
       legend = col_break_legend, # category labels
       col = my_palette,  # color key
       lty= 1,             # line style
       lwd = 5,            # line width
       cex = 1,
       title = "Relative abundance %",
       pt.cex = 1.5)

dev.off()


