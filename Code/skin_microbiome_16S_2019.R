#************************************
# This script is the main data preparation script. Data is formatted for use elsewhere
# and relative abundances calculated at different taxonomy levels.
#
# NOTE - Although the below script refers to each representative sequence as an OTU, in reality
# these sequences are what you would call an amplicon sequence variant (ASV).
# For more information, see : https://www.nature.com/articles/ismej2017119
#************************************

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
# Define various colour palletes
my_colour_pallete <- c("#8dd3c7","#ffffb3","#bebada","#fb8072", "#80b1d3", "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5", "#cc0000")
# From http://tools.medialab.sciences-po.fr/iwanthue/
my_colour_pallete_20 <- c("#66bd79","#a35bcf","#5bb643","#d14ea6","#a2b239","#5c6bcc","#dc892e","#5e93cd","#d64737","#49b6a8","#dc3c6e","#4f7e3c","#bd8cd5","#caab55","#914c88","#867230","#df82a2","#a65429","#ab4a5a","#e0896a")
my_colour_pallete_20_distinct <- c("#0057b4","#7fff56","#d600bc","#d8d500","#e76eff","#019932","#9f8fff","#ffc730","#007fac","#a20019","#06fefd","#ff6782","#00774c","#e0c8ff","#717a00","#4b2952","#e2ed7d","#46321e","#ffbd76","#ffb4c6")
my_colour_pallete_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_pallete_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
my_colour_pallete_15 <- c("#77b642","#7166d9","#cfa240","#b351bb","#4fac7f","#d44891","#79843a","#c68ad4","#d15a2c","#5ba7d9","#ce4355","#6570ba","#b67249","#9b4a6f","#df8398")
my_colour_pallete_32_distinct <- c("#ea7e00","#ca0074","#d1c69b","#474007","#bb00ad","#9c80ff","#be3300","#542e72","#00b9f5","#09436b","#8b0036","#9ac8e6","#ff1059","#959eff","#154a11","#0290f4","#ff7762","#7dbf00","#ff8194","#834c00","#006e73","#f9bb5d","#d6c943","#017229","#00d3a8","#732427","#36e191","#6a8200","#efb3ea","#3227bb","#ff90e1","#e92a12")
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

dir.create(file.path("./Result_figures", "abundance_analysis_plots"), showWarnings = FALSE)
dir.create(file.path("./Result_figures", "pcoa_dbrda_plots"), showWarnings = FALSE)
dir.create(file.path("./Result_figures", "abundance_plots"), showWarnings = FALSE)
dir.create(file.path("./Result_figures", "shannon_simpson_diversities"), showWarnings = FALSE)
dir.create(file.path("./Result_figures", "heatmaps"), showWarnings = FALSE)
dir.create(file.path("./Result_figures", "exploratory_analysis"), showWarnings = FALSE)

# dir.create(file.path("./Result_figures/relative_abundances", "pdf"), showWarnings = FALSE)
# dir.create(file.path("./Result_figures/relative_abundances", "svg"), showWarnings = FALSE)
# dir.create(file.path("./Result_figures/relative_abundances", "jpg"), showWarnings = FALSE)
# dir.create(file.path("./Result_figures/shannon_simpson_diversities", "pdf"), showWarnings = FALSE)
# dir.create(file.path("./Result_figures/shannon_simpson_diversities", "svg"), showWarnings = FALSE)
# dir.create(file.path("./Result_figures/shannon_simpson_diversities", "jpg"), showWarnings = FALSE)

dir.create(file.path("./Result_tables", "abundance_analysis_tables"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "DESeq_results"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "other"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "count_tables"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "relative_abundance_tables"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "prevalences"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "stats_various"), showWarnings = FALSE)
dir.create(file.path("./Result_tables", "shannon_simpson_diversities"), showWarnings = FALSE)
###############################################################

# Load the OTU table
project_otu_table <- read.csv("data/features_statistics.csv")

# Fix name of first column
names(project_otu_table)[1] <- "OTU.ID"

# Get the sample ids from the OTU table
sample_ids_original <- grep("R[0-9].*|S[AB][0-9].*", names(project_otu_table), value = T)
sample_ids <- grep("R[0-9].*|S[AB][0-9].*", names(project_otu_table), value = T)

# Load the metadata
metadata.df <- read.table("data/metadata.tsv", header = T, sep = "\t")

# Results from the ACE amplicon pipeline `should' contain at least one observation/count in every row, however just to be sure
# remove any rows containing all zeros. To do this, simply keep any row where there is any value not equal to zero.
# project_otu_table[sample_ids] will return all columns with names matching the sample ids
# The command below will take each row (MARGIN = 1) for the sample columns and check if any value is not zero.
project_otu_table <- project_otu_table[apply(project_otu_table[sample_ids], MARGIN = 1, function(z) any(z!=0)),]

# Split the Taxon column into Domain, Phylum...Species 
project_otu_table <- separate(project_otu_table, "Taxon", into = c("Domain", "Phylum", "Class", "Order", "Family","Genus", "Species"), remove =F, sep = ";")

# Splitting taxa strings that are not specified at certain taxa levels will produce NA entries at those levels. 
# NA entries should be changed to "Unassigned"
project_otu_table[is.na(project_otu_table)] <- "Unassigned"

# Replace D_# at beginning of taxon rank string to corresponding taxa label, e.g. D_0 = d (domain) D_1 = p (phylum), etc.
project_otu_table$Domain <- as.character(lapply(project_otu_table$Domain, FUN = function(x) gsub("D_0", "d", x)))
project_otu_table$Phylum <- as.character(lapply(project_otu_table$Phylum, FUN = function(x) gsub("D_1", "p", x)))
project_otu_table$Class <- as.character(lapply(project_otu_table$Class, FUN = function(x) gsub("D_2", "c", x)))
project_otu_table$Order <- as.character(lapply(project_otu_table$Order, FUN = function(x) gsub("D_3", "o", x)))
project_otu_table$Family <- as.character(lapply(project_otu_table$Family, FUN = function(x) gsub("D_4", "f", x)))
project_otu_table$Genus <- as.character(lapply(project_otu_table$Genus, FUN = function(x) gsub("D_5", "g", x)))
project_otu_table$Species <- as.character(lapply(project_otu_table$Species, FUN = function(x) gsub("D_6", "s", x)))

# Recreate the full taxonomy string with the 'prettier' taxa labels
project_otu_table$taxonomy_species <- with(project_otu_table, paste(Domain, Phylum, Class, Order, Family, Genus, Species, sep =";"))

# Also create a taxonomy string up to the genus level, since species are very rarely characterised at the Specie level in amplicon data
project_otu_table$taxonomy_genus <- with(project_otu_table, paste(Domain, Phylum, Class, Order, Family, Genus, sep =";"))

# And just for easier plotting later, create taxonomy strings for phylum, class, order and family levels
project_otu_table$taxonomy_family <- with(project_otu_table, paste(Domain, Phylum, Class, Order, Family, sep =";"))
project_otu_table$taxonomy_order <- with(project_otu_table, paste(Domain, Phylum, Class, Order, sep =";"))
project_otu_table$taxonomy_class <- with(project_otu_table, paste(Domain, Phylum, Class, sep =";"))
project_otu_table$taxonomy_phylum <- with(project_otu_table, paste(Domain, Phylum, sep =";"))

# Store a version of the unfiltered project table
project_otu_table_unfiltered <- project_otu_table

##################
## Now filter metadata and ensure the OTU table matches

# We are currently only interested in a samples with the following lesion types. Filter the metadata to just those
# dim(metadata.df)
metadata.df <- metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC", "negative"),]
# dim(metadata.df)

# Sanity check whether all the samples are in the metadata, this should return nothing. 
# Otherwise fix the sample names in the metadata or simply remove the offending samples
missing_samples <- sample_ids[!sample_ids %in% metadata.df$Index]
if ( length(missing_samples) == 0) {
  print("No samples missing")
} else {
  print("Samples missing from metadata")
}

# Also the reverse, are there samples in the metadata missing from the data
missing_samples_metadata.df <- as.character(metadata.df$index[!metadata.df$index %in% sample_ids])

if ( length(missing_samples_metadata.df) == 0) {
  print("No samples missing")
} else {
  print("Samples missing from metadata")
}

# Remove samples that are not in the metadata
dim(project_otu_table)
project_otu_table <- project_otu_table[, ! colnames(project_otu_table) %in% missing_samples]
dim(project_otu_table)

# Remove any patients that are not immunocompromised (MST); remove immunocompetent (MS) patients
# from OTU table and metadata
MS_patients <- unique(metadata.df[grepl("MS[0-9]", metadata.df$Patient),]$Index)
project_otu_table <- project_otu_table[,!colnames(project_otu_table) %in% MS_patients]
metadata.df <- metadata.df[!metadata.df$Index %in% MS_patients,]

# Save the final filtered metadata use elsewhere
#write.table(metadata.df, file = "Result_tables/filtered_metadata.csv", sep = ",", quote = F, row.names = F)

# Reassign the sample ids 
sample_ids <- grep("R[0-9].*|S[AB][0-9].*", names(project_otu_table), value = T)



##################################################
#         Remove unwanted lineages

# Remove OTUs that are Unassigned
# project_otu_table <- project_otu_table[project_otu_table$Taxon != "Unassigned",]

# Discard anything not Bacterial or Fungal
project_otu_table <- project_otu_table[grepl("D_0__Bacteria|D_3__Fungi", project_otu_table$Taxon),]

# Discard anything not Bacterial or Fungal or Unassigned
# project_otu_table <- project_otu_table[grepl("D_0__Bacteria|D_3__Fungi|^Unassigned$", project_otu_table$Taxon),]

# Discard anything not Bacterial
# project_otu_table <- project_otu_table[grepl("D_0__Bacteria", project_otu_table$Taxon),]

##################################################

# Remove old Taxon column
project_otu_table$Taxon <- NULL
project_otu_table_unfiltered$Taxon <- NULL

# Store the OTUs and corresponding taxonomy information in a separate dataframe
otu_taxonomy_map <- project_otu_table[c("OTU.ID",
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
write.table(otu_taxonomy_map, file = "Result_tables/other/otu_taxonomy_map.csv", sep = ",", quote = F, row.names = F)


############################################################################################################################
# Now we can generate the tables that we will need for different analyses at both the OTU and various taxa levels

###############################
########## OTU LEVEL ##########
# Dataframe containing the counts for each OTU, for each sample
otu.df <- project_otu_table[c("OTU.ID", sample_ids)]
otu_unfiltered.df <- project_otu_table_unfiltered[c("OTU.ID", sample_ids)]

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

# Change nans to 0. Occurs when a sample has no hits at this point.
otu_rel.m[is.nan(otu_rel.m)] <- 0

############################################################
## TODO Calculate the fraction of human reads in each sample
# Get human OTUs
human_OTUs <- project_otu_table_unfiltered[grep("Mammalia", project_otu_table_unfiltered$taxonomy_species),]$OTU.ID
# otu_unfiltered.m[which(rownames(otu_unfiltered.m) %in% human_OTUs),]
sample_fraction_human_reads <- 
  melt(colSums(otu_unfiltered.m[rownames(otu_unfiltered.m) %in% human_OTUs,]) / 
         colSums(otu_unfiltered.m),value.name = "Fraction")
sample_fraction_human_reads$Patient <- unlist(lapply(rownames(sample_fraction_human_reads), function(x) metadata.df[metadata.df$Index == x,]$Patient))

myplot <- ggplot(sample_fraction_human_reads, aes(y= Fraction, x= Patient, fill = Patient)) +
  geom_boxplot() +
  scale_fill_manual(values = my_colour_pallete_32_distinct)+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  ylab("Human fraction") +
  xlab("Patient") +
  coord_flip() +
  common_theme
myplot
ggsave(plot = myplot, filename = "./Result_figures/exploratory_analysis/human_fraction_reads_boxplot.pdf", width=6, height=6)

####
bacterial_fungal_OTUs <- project_otu_table_unfiltered[grep("d__Bacteria|Fungi", project_otu_table_unfiltered$taxonomy_species),]$OTU.ID
sample_fraction_bacterial_fungal_reads <- 
  melt(colSums(otu_unfiltered.m[rownames(otu_unfiltered.m) %in% bacterial_fungal_OTUs,]) / 
         colSums(otu_unfiltered.m),value.name = "Fraction")

sample_fraction_bacterial_fungal_reads$Patient <- unlist(lapply(rownames(sample_fraction_bacterial_fungal_reads), function(x) metadata.df[metadata.df$Index == x,]$Patient))

myplot <- ggplot(sample_fraction_bacterial_fungal_reads, aes(y= Fraction, x= Patient, fill = Patient)) +
  geom_boxplot() +
  scale_fill_manual(values = my_colour_pallete_32_distinct)+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  ylab("Bacterial and Fungal fraction") +
  xlab("Patient") +
  coord_flip() +
  common_theme
myplot
ggsave(plot = myplot, filename = "./Result_figures/exploratory_analysis/bacterial_fungal_fraction_reads_boxplot.pdf", width=6, height=6)

###
fungal_OTUs <- project_otu_table_unfiltered[grep("Fungi", project_otu_table_unfiltered$taxonomy_species),]$OTU.ID
sample_fraction_fungal_reads <- 
  melt(colSums(otu_unfiltered.m[rownames(otu_unfiltered.m) %in% fungal_OTUs,]) / 
         colSums(otu_unfiltered.m),value.name = "Fraction")

sample_fraction_fungal_reads$Patient <- unlist(lapply(rownames(sample_fraction_fungal_reads), function(x) metadata.df[metadata.df$Index == x,]$Patient))

myplot <- ggplot(sample_fraction_fungal_reads, aes(y= Fraction, x= Patient, fill = Patient)) +
  geom_boxplot() +
  scale_fill_manual(values = my_colour_pallete_32_distinct)+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  ylab("Fungal fraction") +
  xlab("Patient") +
  coord_flip() +
  common_theme
myplot
ggsave(plot = myplot, filename = "./Result_figures/exploratory_analysis/fungal_fraction_reads_boxplot.pdf", width=6, height=6)

###

bacterial_OTUs <- project_otu_table_unfiltered[grep("d__Bacteria", project_otu_table_unfiltered$taxonomy_species),]$OTU.ID
sample_fraction_bacterial_reads <- 
  melt(colSums(otu_unfiltered.m[rownames(otu_unfiltered.m) %in% bacterial_OTUs,]) / 
         colSums(otu_unfiltered.m),value.name = "Fraction")

sample_fraction_bacterial_reads$Patient <- unlist(lapply(rownames(sample_fraction_bacterial_reads), function(x) metadata.df[metadata.df$Index == x,]$Patient))

myplot <- ggplot(sample_fraction_bacterial_reads, aes(y= Fraction, x= Patient, fill = Patient)) +
  geom_boxplot() +
  scale_fill_manual(values = my_colour_pallete_32_distinct)+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  ylab("Bacterial fraction") +
  xlab("Patient") +
  coord_flip() +
  common_theme
myplot
ggsave(plot = myplot, filename = "./Result_figures/exploratory_analysis/bacterial_fraction_reads_boxplot.pdf", width=6, height=6)


###
Unassigned_OTUs <- project_otu_table_unfiltered[project_otu_table_unfiltered$Domain == "Unassigned",]$OTU.ID

sample_fraction_unassigned_reads <- 
  melt(colSums(otu_unfiltered.m[rownames(otu_unfiltered.m) %in% Unassigned_OTUs,]) / 
         colSums(otu_unfiltered.m),value.name = "Fraction")

sample_fraction_unassigned_reads$Patient <- unlist(lapply(rownames(sample_fraction_unassigned_reads), function(x) metadata.df[metadata.df$Index == x,]$Patient))

myplot <- ggplot(sample_fraction_unassigned_reads, aes(y= Fraction, x= Patient, fill = Patient)) +
  geom_boxplot() +
  scale_fill_manual(values = my_colour_pallete_32_distinct)+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  ylab("Unassigned fraction") +
  xlab("Patient") +
  coord_flip() +
  common_theme
myplot
ggsave(plot = myplot, filename = "./Result_figures/exploratory_analysis/unassigned_fraction_reads_boxplot.pdf", width=6, height=6)


############################################################
# Filter those OTUs that are low abundance in all samples

# Generally, low abundance OTUs are removed.
# Check how many OTUs would be removed if we filtered any whose abundance is less than 0.05% (0.0005) in all samples
filter_percentage = 0.0005
otu_rel_low_abundance_otus.m <- otu_rel.m[apply(otu_rel.m[,sample_ids],1,function(z) all(z < filter_percentage)),]

# TODO - Collect low abundance OTUs that will be filtered out at this stage
otu_rel_low_abundance_otus.df <- as.data.frame(otu_rel_low_abundance_otus.m)
# FIXME write.table(otu_rel_low_abundance_otus.df, file = "Result_tables/count_tables/low_abundance_OTUs.csv", sep = ",", quote = F, col.names = T, row.names = F)
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

############################################################
### Keep only those OTUs from AK samples that have a higher mean relative abundance compared to the swab controls (C) or negatives

# Note that in David's code for the 2018 paper, he first rarefied before calculating contaminants presence/absence
# e.g. otu_rare_count.m <- t(rrarefy(x = t(otu_count_raw.m), sample = 2000))
# I think this makes sense. Large differences in read depth between samples will make it harder to determine the 
# true-positive contaminants from false-positive.
otu_rare_count.m <- t(rrarefy(x = t(otu.m), sample=30000))

# Calculate the relative abundance on this rarefied table. Can use this to calculate the mean relative abundance of each OTU in different groups.
# In theory, a contaminant will make up a larger percent of the abundance in the negative control.
otu_rare_rel.m <- t(t(otu_rare_count.m) / colSums(otu_rare_count.m))

# Change nans to 0. Occurs when a sample has no hits at this point.
otu_rare_rel.m[is.nan(otu_rare_rel.m)] <- 0

# First get the sample ids for various lesion type groups
# TODO - Do we want to actually use negative samples rather than the control samples? I think so!
negative_sample_ids <- as.character(metadata.df[metadata.df$Sampletype == "negative",]$Index)
AK_sample_ids <- as.character(metadata.df[metadata.df$Sampletype == "AK",]$Index)
C_sample_ids <- as.character(metadata.df[metadata.df$Sampletype == "C",]$Index)
pooled_AK_sample_ids <- as.character(metadata.df[metadata.df$Sampletype %in% c("AK", "IEC"),]$Index)
not_negative_sample_ids <- as.character(metadata.df[metadata.df$Sampletype != "negative",]$Index)
not_negative_or_control_sample_ids <- as.character(metadata.df[!metadata.df$Sampletype %in% c("negative", "C"),]$Index)
pooled_not_negative_sample_ids <- as.character(metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC"),]$Index)

###
# TODO Collect contaminating OTUs based on mean relative abundance differences

contaminating_otus_from_mean_abundances <- rownames(otu_rare_rel.m[rowMeans(otu_rare_rel.m[,not_negative_sample_ids]) < rowMeans(otu_rare_rel.m[,negative_sample_ids]),])
length(contaminating_otus_from_mean_abundances)

###
# David's approach to identifying contaminants. It is the prevalence/frequency/abundance among sample type, rather than the mean relative abundance.
# Determine contaminating otus from prevalence differences between groups
# This will calculate what percentage of samples in each group an OTU is present in
otu_pooled_not_negative_sample_prevalences <- apply(otu_rare_count.m[,pooled_not_negative_sample_ids], 1, function(x) {length(which(x > 0))}) /length(pooled_not_negative_sample_ids)
contaminating_otus_from_prevalences <- names(otu_pooled_not_negative_sample_prevalences[otu_pooled_not_negative_sample_prevalences < otu_negative_sample_prevalences])

print(paste("There are", length(contaminating_otus_from_mean_abundances), "contaminating OTUs based on mean abundances"))
print(paste("There are", length(contaminating_otus_from_prevalences), "contaminating OTUs based on prevalences"))

# length(contaminating_otus_from_mean_abundances[contaminating_otus_from_mean_abundances %in% contaminating_otus_from_prevalences])
# length(contaminating_otus_from_prevalences[contaminating_otus_from_prevalences %in% contaminating_otus_from_mean_abundances])

# Plot the OTU detection fraction in negative controls vs pooled samples
presences.df <- data.frame(row.names = rownames(otu_rare_count.m))
presences.df$not_negative <- melt(otu_pooled_not_negative_sample_prevalences)$value
presences.df$negative <- melt(otu_negative_sample_prevalences)$value
presences.df[,'contaminant'] <- "No"
presences.df[contaminating_otus_from_prevalences, 'contaminant'] <- "Yes" 
myplot <- ggplot(presences.df, aes(x =negative*100, y= not_negative*100)) + 
  geom_point(aes(color=contaminant)) +
  # geom_point(aes(color=contaminant, shape = contaminant)) +
  #scale_shape_manual(values = c(1,4)) +
  xlab("Percent of swab negative control samples OTU detected in") +
  ylab("Percent of non-negative samples OTU detected in") +
  # scale_x_continuous(breaks = seq(0,1,.25), limits = c(0,1.01)) +
  # scale_y_continuous(breaks = seq(0,1,.25), limits = c(0,1.01)) +
  scale_x_continuous(breaks = seq(0,100,25), limits = c(0,101)) +
  scale_y_continuous(breaks = seq(0,100,25), limits = c(0,101)) +
  common_theme
ggsave(plot = myplot, filename = "./Result_figures/exploratory_analysis/contaminant_otus_scatter_plot.pdf", width=6, height=6)
###
# And filter. Either filter the contaminating otus calculating from prevalences or from mean abundances
dim(otu_rel.m)
# otu_rel.m <- otu_rel.m[!rownames(otu_rel.m) %in% contaminating_otus_from_mean_abundances,]
otu_rel.m <- otu_rel.m[!rownames(otu_rel.m) %in% contaminating_otus_from_prevalences,]
dim(otu_rel.m)

# Re-normalise the matrix after filtering
otu_rel.m <- t(t(otu_rel.m) / colSums(otu_rel.m))

# Change nans to 0. Occurs when a sample has no hits at this point.
otu_rel.m[is.nan(otu_rel.m)] <- 0

# Now go back to OTU counts matrix and remove contaminate OTUs
otu.m <- otu.m[rownames(otu_rel.m),]

# Remove those samples with less than 5000 reads - used in previous paper
# TODO - write to file those samples that are removed
dim(otu.m)
otu.m <- otu.m[,colSums(otu.m) >= 5000]
dim(otu.m)


##############################################################
#         OPTIONAL - rarefying

# Note - For samples with a read count lower then the sample=# parameter, 
# the rrarefy function in vegan will return the this sample with its existing read count.
# Sample with counts higher than the sample parameter will be rarefied as normal and their counts will be capped at the sample parameter value

# Normally samples with OTU counts lower than our desired threshold need to be removed if we want to rarefy.
# This should have been performed at this point in normal circumstances. However, we are simply using rrarefy to cap the read depth so the fold difference
# between the highest and lowest is not extreme.

# Looking at rarefaction curve and the counts for each sample can be a good
# way to determine a good minimum count threshold for samples

# Counts for each Sample
column_sums <- colSums(otu.m)
column_sums.df <- melt(column_sums[order(column_sums)])
column_sums.df$sample <- rownames(column_sums.df)
rownames(column_sums.df) <- NULL
column_sums.df <- column_sums.df[c("sample", "value")]
column_sums.df$sample <- factor(column_sums.df$sample, levels = column_sums.df$sample)

myplot <- ggplot(column_sums.df, aes(x = sample, y = value)) + 
  geom_histogram(stat = "identity") +
  geom_hline(yintercept = 30000, color = 'red')+
  geom_hline(yintercept = 20000, color = 'red')+
  geom_hline(yintercept = mean(column_sums.df$value), color = 'blue')+
  geom_hline(yintercept = median(column_sums.df$value), color = 'purple')+
  scale_y_continuous(breaks = seq(0,max(column_sums.df$value), 5000)) +
  xlab("Sample") +
  ylab("Read count") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90,vjust = .5))
ggsave(plot = myplot, filename = "./Result_figures/exploratory_analysis/sample_read_depth_distribution.pdf", width=30, height=6)

myplot <- ggplot(column_sums.df, aes(x = value)) + 
  xlab("Read count") +
  ylab("Number of samples") +
  scale_x_continuous(breaks = seq(5000,max(column_sums.df$value), 5000)) +
  scale_y_continuous(breaks = seq(0,50, 2)) +
  geom_histogram(stat = "bin", bins = 60, colour = "black",fill = "grey") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))
ggsave(plot = myplot, filename = "./Result_figures/exploratory_analysis/sample_read_depth_distribution_2.pdf", width=6, height=6)

summary(column_sums.df$value)
mean(column_sums.df$value)
median(column_sums.df$value)
#row_sums <- #rowSums(otu.m) # counts for OTUs

# Rarefaction curve
# rarecurve(t(otu.m[,colSums(otu.m) > 1]),step = 500, label = F,xlim = c(0,30000),sample = 30000)

# Generate a rarefied OTU count matrix
# In the paper from 2018, a rarefaction maximum of 20,000 was used
otu_rare_count.m <- t(rrarefy(x = t(otu.m), sample=30000))

# Compare before and after
# colSums(otu.m)[1:5]
# colSums(otu_rare_count.m)[1:5]
# head(melt(rowSums(otu.m[,1:5])))
# head(melt(rowSums(otu_rare_count.m[,1:5])))

# TODO - number of and fraction of OTUs that are removed from rarefying

##############################################################

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
write.table(metadata.df[metadata.df$Index %in% samples_lost,], file = "Result_tables/other/metadata_samples_removed.csv", sep = ",", quote = F, col.names = T, row.names = F)



#######################################################################################################################################
# Finally create and save a dataframe containing all the final OTUs, their abundances in each sample and their taxonomy information.
# Include the metadata for each sample
# FIXME - currently just un-rarefied abundances

# Take the project_otu_table and filter to those otus in the filtered dataframe
filtered_project_otu_table <- project_otu_table[project_otu_table$OTU.ID %in% rownames(otu_rel.m), ]
# Remove all sample columns
filtered_project_otu_table <- filtered_project_otu_table[!names(filtered_project_otu_table) %in% sample_ids_original]
# Remove unnecessary columns
filtered_project_otu_table$Confidence <- NULL
filtered_project_otu_table$Frequency <- NULL
# Copy the OTU abundance matrix and turn into a dataframe
otu_abundance_metadata.df <- as.data.frame(otu_rel.m)
# otu_abundance_metadata.df <- as.data.frame(otu.m)
# Make OTU.ID a column and remove rownames
otu_abundance_metadata.df$OTU.ID <- rownames(otu_abundance_metadata.df)
rownames(otu_abundance_metadata.df) <- NULL
# Fix the ordering. sample_ids was defined in the previous section and is simply the column names of otu_rel.m
otu_abundance_metadata.df <- otu_abundance_metadata.df[c("OTU.ID", sample_ids)]
# Melt the dataframe so there is an entry for each sample
otu_abundance_metadata.df <- melt(otu_abundance_metadata.df, measure.vars = sample_ids, 
             variable.name = "Sample", 
             value.name = "Relative_abundance")
# Remove entries where the abundance is zero
otu_abundance_metadata.df <- otu_abundance_metadata.df[otu_abundance_metadata.df$Relative_abundance > 0,]
# Merge the dataframe with the metadata
otu_abundance_metadata.df <- merge(otu_abundance_metadata.df, metadata.df, by.x = "Sample", by.y = "Index")
# Merge the dataframe with the filter project_otu_table to get the taxonomy information and repseq
otu_abundance_metadata.df <- merge(otu_abundance_metadata.df, filtered_project_otu_table, by.x = "OTU.ID", by.y = "OTU.ID")
# Save the dataframe
write.table(otu_abundance_metadata.df, file = "Result_tables/other/OTU_abundances_and_metadata.csv", sep = ",", quote = F, col.names = T, row.names = F)

# otu_abundance_metadata.df %>% group_by(Sampletype) %>% 
  # dplyr::summarise(Count = sum(Relative_abundance)) %>% as.data.frame()

#######################################################################################################################################
# Above we processed the frequencies for each OTU to calculate the relative abundances.
# However, we often want the abundances at not just the individual OTU level, but also different taxonomy levels.
# For example, we may want to know the abundance of a particular Family.
# Now we will generate the abundance tables at each taxonomy level from Phylum, Class, Order, Family and Genus

# otu_metadata_merged.df
otu_metadata_merged.df <- merge(otu.df, otu_taxonomy_map, by.x = "OTU.ID", by.y = "OTU.ID")

# Do the same for the rarefied data
otu_metadata_merged_rare.df <- merge(otu_rare.df, otu_taxonomy_map, by.x = "OTU.ID", by.y = "OTU.ID")

dim(otu_metadata_merged.df)
dim(otu_metadata_merged_rare.df)

# We are then going to create and store the matrices for each taxonomy level containing the relative abundances. 
otu_phylum_rel.m <- NULL
otu_class_rel.m <- NULL
otu_order_rel.m <- NULL
otu_family_rel.m <- NULL
otu_genus_rel.m <- NULL
otu_species_rel.m <- NULL

otu_phylum_rel_rare.m <- NULL
otu_class_rel_rare.m <- NULL
otu_order_rel_rare.m <- NULL
otu_family_rel_rare.m <- NULL
otu_genus_rel_rare.m <- NULL
otu_species_rel_rare.m <- NULL

# And also the dataframes containing the counts in case they are needed
otu_phylum.df <- NULL
otu_class.df <- NULL
otu_order.df <- NULL
otu_family.df <- NULL
otu_genus.df <- NULL
otu_species.df <- NULL

otu_phylum_rare.df <- NULL
otu_class_rare.df <- NULL
otu_order_rare.df <- NULL
otu_family_rare.df <- NULL
otu_genus_rare.df <- NULL
otu_species_rare.df <- NULL

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
    otu_phylum_rel.m <- otu_taxa_level_rel.m
    otu_phylum.df <- otu_taxa_level.df
    otu_phylum_rel_rare.m <- otu_taxa_level_rel_rare.m
    otu_phylum_rare.df <- otu_taxa_level_rare.df
  } 
  else if (grepl("class", tax_string_level)){
    otu_class_rel.m <- otu_taxa_level_rel.m
    otu_class.df <- otu_taxa_level.df
    otu_class_rel_rare.m <- otu_taxa_level_rel_rare.m
    otu_class_rare.df <- otu_taxa_level_rare.df
  }
  else if (grepl("order", tax_string_level)){
    otu_order_rel.m <- otu_taxa_level_rel.m
    otu_order.df <- otu_taxa_level.df
    otu_order_rel_rare.m <- otu_taxa_level_rel_rare.m
    otu_order_rare.df <- otu_taxa_level_rare.df
  }
  else if (grepl("family", tax_string_level)){
    otu_family_rel.m <- otu_taxa_level_rel.m
    otu_family.df <- otu_taxa_level.df
    otu_family_rel_rare.m <- otu_taxa_level_rel_rare.m
    otu_family_rare.df <- otu_taxa_level_rare.df
  }
  else if (grepl("genus", tax_string_level)){
    otu_genus_rel.m <- otu_taxa_level_rel.m
    otu_genus.df <- otu_taxa_level.df
    otu_genus_rel_rare.m <- otu_taxa_level_rel_rare.m
    otu_genus_rare.df <- otu_taxa_level_rare.df
  }
  else if (grepl("species", tax_string_level)){
    otu_species_rel.m <- otu_taxa_level_rel.m
    otu_species.df <- otu_taxa_level.df
    otu_species_rel_rare.m <- otu_taxa_level_rel_rare.m
    otu_species_rare.df <- otu_taxa_level_rare.df
  }
}


### Write the final counts and abundances for each taxonomy level to file

# Taxonomy-sample matrix to dataframe convertor
m2df_tax_convert_save <- function(mymatrix, name_of_taxonomy_col = "taxonomy"){
  mydf <- as.data.frame(mymatrix)
  cur_names <- names(mydf)
  mydf[, name_of_taxonomy_col] <- rownames(mydf)
  rownames(mydf) <- NULL
  mydf <- mydf[,c(name_of_taxonomy_col,cur_names)]
  return(mydf)
}

# Not rarefied
write.table(otu_species.df, file = "Result_tables/count_tables/Specie_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_species_rel.m, "taxonomy_species"), file = "Result_tables/relative_abundance_tables/Specie_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_genus.df, file = "Result_tables/count_tables/Genus_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_genus_rel.m, "taxonomy_genus"), file = "Result_tables/relative_abundance_tables/Genus_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_family.df, file = "Result_tables/count_tables/Family_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_family_rel.m, "taxonomy_family"), file = "Result_tables/relative_abundance_tables/Family_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_order.df, file = "Result_tables/count_tables/Order_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_order_rel.m, "taxonomy_order"), file = "Result_tables/relative_abundance_tables/Order_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_class.df, file = "Result_tables/count_tables/Class_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_class_rel.m, "taxonomy_class"), file = "Result_tables/relative_abundance_tables/Class_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_phylum.df, file = "Result_tables/count_tables/Phylum_counts.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_phylum_rel.m, "taxonomy_phylum"), file = "Result_tables/relative_abundance_tables/Phylum_relative_abundances.csv", sep = ",", quote = F, col.names = T, row.names = F)

# Rarified
write.table(otu_species_rare.df, file = "Result_tables/count_tables/Specie_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_species_rel_rare.m, "taxonomy_species"), file = "Result_tables/relative_abundance_tables/Specie_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_genus_rare.df, file = "Result_tables/count_tables/Genus_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_genus_rel_rare.m, "taxonomy_genus"), file = "Result_tables/relative_abundance_tables/Genus_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_family_rare.df, file = "Result_tables/count_tables/Family_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_family_rel_rare.m, "taxonomy_family"), file = "Result_tables/relative_abundance_tables/Family_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_order_rare.df, file = "Result_tables/count_tables/Order_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_order_rel_rare.m, "taxonomy_order"), file = "Result_tables/relative_abundance_tables/Order_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_class_rare.df, file = "Result_tables/count_tables/Class_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_class_rel_rare.m, "taxonomy_class"), file = "Result_tables/relative_abundance_tables/Class_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

write.table(otu_phylum_rare.df, file = "Result_tables/count_tables/Phylum_counts_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)
write.table(m2df_tax_convert_save(otu_phylum_rel_rare.m, "taxonomy_phylum"), file = "Result_tables/relative_abundance_tables/Phylum_relative_abundances_rarefied.csv", sep = ",", quote = F, col.names = T, row.names = F)

