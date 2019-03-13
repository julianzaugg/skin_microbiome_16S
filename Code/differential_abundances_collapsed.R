# Differential abundance analysis, collapsing down to various taxonomic levels

library(DESeq2)
library(dplyr)
library(reshape2)



############################################################
######################## Functions #########################

# Function that calculates the geometric mean with some error-protection bits. 
# DESeq2 does not appear to work (will throw an error) if every OTU (or genus or genome etc.) 
# contains at least one count of zero in every row of the count data.
# Specifically, the function "dds<-DESeq(dds, betaPrior = FALSE)" will fail
# One way to address this is to use the function below as input to DESeq2 to transform the data.
# Calculate the geometric means prior to estimating the size factors
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

# For each rowname (OTU), get the corresponding taxonomy_species
# Assumes "OTU.ID" and "taxonomy_species" columns in the provided map dataframe
assign_taxonomy_to_otu <- function(otutable, taxon_map){
  taxonomies <- c()
  for (otuid in rownames(otutable)){
    taxonomies <- c(taxonomies, as.character(taxon_map[taxon_map$OTU.ID == otuid,]$taxonomy_species))
  }
  return(taxonomies)
}

# Filter and sort DESeq result tables
filter_and_sort_dds_results <- function(x, p_value_threshold = 0.05){
  filtered_table <- x
  filtered_table <- filtered_table[!is.na(filtered_table$padj),]
  filtered_table <- filtered_table[filtered_table$padj <= p_value_threshold,]
  filtered_table <- filtered_table[order(filtered_table$padj),]
  return(filtered_table)
}

############################################################
# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")

# Load count table at the OTU level. These are the counts for OTUs that were above our abundance thresholds
# otu.df <- read.table("Result_tables/count_tables/OTU_counts_rarefied.csv", sep =",", header =T)

# Load count table at the taxonomy levels of interest.
otu_species_rare.m <- as.matrix(read.table("Result_tables/count_tables/Specie_counts_rarefied.csv", sep =",", header =T, row.names = 1))
otu_genus_rare.m <-  as.matrix(read.table("Result_tables/count_tables/Genus_counts_rarefied.csv", sep =",", header =T, row.names = 1))

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC, NLC and SCC lesions. 
metadata.df <- metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC", "NLC"),]


# otu.df <- otu.df[,names(otu.df) %in% c("OTU.ID", as.character(metadata.df$Index))]
otu_species_rare.m <- otu_species_rare.m[,colnames(otu_species_rare.m) %in% as.character(metadata.df$Index)]
otu_genus_rare.m <- otu_genus_rare.m[,colnames(otu_genus_rare.m) %in% as.character(metadata.df$Index)]

# otu_species_rare.df <- otu_species_rare.df[,names(otu_species_rare.df) %in% c("taxonomy_species", as.character(metadata.df$Index))]
# otu_genus_rare.df <- otu_genus_rare.df[,names(otu_genus_rare.df) %in% c("taxonomy_genus", as.character(metadata.df$Index))]

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
samples_removed <- metadata.df$Index[!metadata.df$Index %in% colnames(otu_species_rare.m)]
metadata.df <- metadata.df[! metadata.df$Index %in% samples_removed,]

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# For DESeq2, variables, i.e. discrete columns, should be factorised. 
# DESeq2 may do this automatically but it is good to be explicit.
metadata.df$Sampletype <- factor(metadata.df$Sampletype)
metadata.df$Patient <- factor(metadata.df$Patient)
metadata.df$Project <- factor(metadata.df$Project)



# Re-create the OTU matrix
# otu.m <- otu.df
# rownames(otu.m) <- otu.m$OTU.ID
# otu.m$OTU.ID <- NULL
# otu.m <- as.matrix(otu.m)

# CLR transform the otu matrix. We don't use this for DESeq, but we can refer to it to get the 
# CLR transformed counts for OTUs of interest if necessary
# otu_clr.m <- clr(otu.m)


# ---------------------------------------------------------------------------------------------------------
# Perform differential abundance calculations at the OTU level and genus level, 
# comparing between the groups within variables of interest

# DESeq requires a count matrix ('countData'), a corresponding metadata.df ('colData') and a 'design' formula. The formula expresses
# how the counts for each OTU/genus depend on the variables defined in the 'colData'. See help(DESeqDataSetFromMatrix) for more information.
# The first column of the metadata.df ('colData') must match the ordering of the columns of the countData

# Ensure names of the otu / genus count matrices match the order of the metadata.df!
# Assumes number of samples in metadata.df and count data are the same
all(names(otu_species_rare.m) == metadata.df$Index) # Should be 'True'
# all(names(otu_genus.m) == metadata.df$Sample) # Should be 'True'

# Can order the columns of the count matrices by the order of the metadata.df as follows
otu_species_rare.m <- otu_species_rare.m[,order(metadata.df$Index)]
otu_genus_rare.m <- otu_genus_rare.m[,order(metadata.df$Index)]
dim(otu_species_rare.m)
dim(otu_species_rare.m[which(apply(otu_species_rare.m, 1, max) >= 60),])

# dim(otu_genus_rare.m)
# dim(otu_genus_rare.m[which(apply(otu_genus_rare.m, 1, max) >= 15),])

dim(otu_genus_rare.m)
temp <- t(t(otu_genus_rare.m)/colSums(otu_genus_rare.m))
otu_genus_rare.m <- otu_genus_rare.m[which(apply(otu_genus_rare.m, 1, max) >= 75),]
dim(otu_genus_rare.m)

plot(density(colSums(temp[rownames(temp) %in% rownames(otu_genus_rare.m),])))
rug(colSums(temp[rownames(temp) %in% rownames(otu_genus_rare.m),]))
summary(colSums(temp[rownames(temp) %in% rownames(otu_genus_rare.m),]))
boxplot(colSums(temp[rownames(temp) %in% rownames(otu_genus_rare.m),]))
# Filter to only OTUs that have more than 15 reads in at least one sample
# dim(otu.m)
# otu.m <- otu.m[which(apply(otu.m, 1, max) >= 15),]
# dim(otu.m)
# otu.m <- otu.m[which(apply(otu.m, 1, max) >= 30),]
# dim(otu.m)
# otu.m <- otu.m[which(apply(otu.m, 1, max) >= 150),]
# dim(otu.m)
#150 / 30000 * 100
# ---------------------------------------------------------------------------------------------------------
### Create new variables where different lesions types have been grouped

metadata.df$Sampletype_fixed <- factor(as.character(lapply(metadata.df$Sampletype, function(x)ifelse(x == "C", "NLC", as.character(x)))))
melt(colSums(t(t(otu_genus_rare.m)/colSums(otu_genus_rare.m))))# # SCC and all
metadata.df$Sampletype_SCC <- factor(as.character(lapply(metadata.df$Sampletype, function(x)ifelse(x == "SCC", "SCC", "all"))))
# # SCC+SCC_PL and all
metadata.df$Sampletype_SCC_both <- factor(as.character(lapply(metadata.df$Sampletype, function(x)ifelse(x == "SCC" | x == "SCC_PL", "SCC", "all"))))
# # AK and all
metadata.df$Sampletype_AK <- factor(as.character(lapply(metadata.df$Sampletype, function(x)ifelse(x == "AK", "AK", "all"))))

metadata.df$Sampletype_pooled_cohort <- factor(paste0(metadata.df$Sampletype_pooled, "_", metadata.df$Project))



# ---------------------------------------------------------------------------------------------------------
# Genus level
dds <-DESeqDataSetFromMatrix(countData = otu_genus_rare.m,
                             colData = metadata.df,
                             design = ~ Sampletype_pooled_cohort)

geoMeans <- apply(counts(dds), 1, gm_mean)

# https://www.rdocumentation.org/packages/DESeq2/versions/1.12.3/topics/estimateSizeFactors
# dds <- estimateSizeFactors(dds, geoMeans = geoMeans) # Needed if zeros in every row
dds <- estimateSizeFactors(dds)
dds <- DESeq(dds, parallel = T)
# saveRDS(dds, file = paste0("Result_objects/DESeq_Sampletype_pooled_cohort_genus_DDS_", format(Sys.Date(), "%d%m%y"), ".RData"))
resultsNames(dds)
temp <- results(dds, contrast = c("Sampletype_pooled_cohort","NLC_immunocompromised","AK_immunocompromised"), alpha=0.05, independentFiltering = F, cooksCutoff = F)

sample_type_combinations_cohort_pooled <- combn(as.character(unique(metadata.df$Sampletype_pooled_cohort)), 2)
for (i in 1:ncol(sample_type_combinations_cohort_pooled)){
  group_1 <- as.character(sample_type_combinations_cohort_pooled[1,i])
  group_2 <- as.character(sample_type_combinations_cohort_pooled[2,i])
  print(paste0(group_1, "_vs_", group_2))
  resMFSource <- results(dds, contrast = c("Sampletype_pooled_cohort",group_1,group_2), alpha=0.05, independentFiltering = F, cooksCutoff = F)
  #lfcShrink(dds)?
  resMFSourceOrdered <- resMFSource[order(resMFSource$padj),]
  resMFSourceOrdered <- filter_and_sort_dds_results(resMFSourceOrdered)
  # Write the results to file
  result_name <- paste(group_1, group_2, sep = "_vs_")
  outfilename <- paste("Result_tables/DESeq_results/", result_name, "__pooled_cohort_genus.csv", sep= "")
  write.csv(as.data.frame(resMFSourceOrdered),file=outfilename, quote = F)
}


