library(DESeq2)



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


# Load count table at the OTU level. These are the counts for OTUs that were above our abundance thresholds
otu.df <- read.table("Result_tables/count_tables/OTU_counts_rarified.csv", sep =",", header =T)

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the metadata.df
metadata.df <- read.table("data/metadata.tsv", sep ="\t", header = T)

# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
samples_removed <- metadata.df$Index[!metadata.df$Index %in% names(otu.df)]
metadata.df <- metadata.df[! metadata.df$Index %in% samples_removed,]

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# For DESeq2, variables, i.e. discrete columns, should be factorised. 
# DESeq2 may do this automatically but it is good to be explicit.
metadata.df$Sampletype <- factor(metadata.df$Sampletype)
metadata.df$Patient <- factor(metadata.df$Patient)



# Re-create the OTU matrix
otu.m <- otu.df
rownames(otu.m) <- otu.m$OTU.ID
otu.m$OTU.ID <- NULL

# CLR transform the otu matrix. We don't use this for DESeq, but we can refer to it to get the 
# CLR transformed counts for OTUs of interest if necessary
otu_clr.m <- clr(otu.m)


########################################################################################################################
# Perform differential abundance calculations at the OTU level and genus level, 
# comparing between the groups within variables of interest

# DESeq requires a count matrix ('countData'), a corresponding metadata.df ('colData') and a 'design' formula. The formula expresses
# how the counts for each OTU/genus depend on the variables defined in the 'colData'. See help(DESeqDataSetFromMatrix) for more information.
# The first column of the metadata.df ('colData') must match the ordering of the columns of the countData

# Ensure names of the otu / genus count matrices match the order of the metadata.df!
# Assumes number of samples in metadata.df and count data are the same
all(names(otu.m) == metadata.df$Index) # Should be 'True'
# all(names(otu_genus.m) == metadata.df$Sample) # Should be 'True'

# Can order the columns of the count matrices by the order of the metadata.df as follows
otu.m <- otu.m[,order(metadata.df$Index)]

# Filter to only OTUs that have more than 10 reads in at least one sample
# otu.m <- otu.m[which(apply(otu.m, 1, max) >= 10),]

################################################################################
# Just to cut down on numbers for fasta script testing
# dim(otu.m)
# otu.m <- otu.m[which(apply(otu.m, 1, max) >= 1000),] # Just to cut down on numbers for quick script testing
# dim(otu.m)
################################################################################

####################################
### Create new variables where different lesions types have been grouped
# SCC and all
metadata.df$Sampletype_SCC <- factor(as.character(lapply(metadata.df$Sampletype, function(x)ifelse(x == "SCC", "SCC", "all"))))
# SCC+SCC_PL and all
metadata.df$Sampletype_SCC_both <- factor(as.character(lapply(metadata.df$Sampletype, function(x)ifelse(x == "SCC" | x == "SCC_PL", "SCC", "all"))))
# AK and all
metadata.df$Sampletype_AK <- factor(as.character(lapply(metadata.df$Sampletype, function(x)ifelse(x == "AK", "AK", "all"))))
# SCC_PL + C
metadata.df$Sampletype_SCCPL_C <- factor(as.character(lapply(as.character(metadata.df$Sampletype),function(x)ifelse(x == "SCC_PL" | x == "C", "SCCPL_C", x))))
####################################

#### First we will just compare each Sampletype against all other sample types

# To compare specific groups, it is useful to set the levels of metadata.df. 
# The first entry in levels is the reference. All other levels will be compared to this.
# By default and for convenience, set the control group (C) as the reference.
# We do have the option later to compare any of the groups regardless of setting the reference.
metadata.df$Sampletype <- relevel(metadata.df$Sampletype, ref = "C") # control as the reference
#metadata.df$Sampletype <- relevel(metadata.df$Sampletype, ref = "negative") # negative as the reference



# Create DESeq data set matrix. 
dds <-DESeqDataSetFromMatrix(countData = otu.m, 
                            colData = metadata.df, 
                            design = ~ Sampletype)

# https://www.rdocumentation.org/packages/DESeq2/versions/1.12.3/topics/estimateSizeFactors
# dds <- estimateSizeFactors(dds, geoMeans = geoMeans) # Needed if zeros in every row 
dds <- estimateSizeFactors(dds)
# This may take awhile to run
dds <- DESeq(dds) 
#dds <- DESeq(dds, betaPrior = FALSE)


# Generate the results for the different comparisons using "contrast".
# P-values produced by DESeq are adjusted for false-discovery-rate (FDR) using, by default, the Benjamin-Hochberg correction.
# The alpha parameter controls the FDR threshold. Here a value FDR threhold of 0.05 is used, i.e. the proportion of false positives 
# we expect amongst our differentially abundant otus/genera is less than 5%.

# Determine what are the pairwise combinations among the Sampletypes
sample_type_combinations <- combn(as.character(unique(metadata.df$Sampletype)), 2)
# Loop and compare each combination
for (i in 1:ncol(sample_type_combinations)){
  group_1 <- as.character(sample_type_combinations[1,i])
  group_2 <- as.character(sample_type_combinations[2,i])
  resMFSource <- results(dds, contrast = c("Sampletype",group_1,group_2), alpha=0.05, independentFiltering = F, cooksCutoff = F)
  #lfcShrink(dds)?
  resMFSourceOrdered <- resMFSource[order(resMFSource$padj),]
  resMFSourceOrdered$taxonomy <- assign_taxonomy_to_otu(resMFSourceOrdered, otu_taxonomy_map.df)
  resMFSourceOrdered <- filter_and_sort_dds_results(resMFSourceOrdered)
  # Write the results to file
  result_name <- paste(group_1, group_2, sep = "_vs_")
  outfilename <- paste("Result_tables/DESeq_results/", result_name, ".csv", sep= "")
  write.csv(as.data.frame(resMFSourceOrdered),file=outfilename, quote = F)
  
  # Frequency bar graph of adjusted p-values
  # p_value_distribution <- ggplot(as.data.frame(resMFSourceOrdered), aes(x = padj)) + 
  #   stat_bin(bins = 50, fill =  "white", colour = "black") + 
  #   ylab("Frequency") +
  #   xlab("Adjusted p-value") +
  #   ggtitle() + 
  #   common_theme
}

##############################
### TESTING
# p-value distribution
# resMFSource <- results(dds, contrast = c("Sampletype","SCC","SCC_PL"), alpha=0.05, independentFiltering = F, cooksCutoff = F)
# resMFSourceOrdered <- resMFSource[order(resMFSource$padj),]
# resMFSourceOrdered$taxonomy <- assign_taxonomy_to_otu(resMFSourceOrdered, otu_taxonomy_map.df)
# resMFSourceOrdered <- filter_and_sort_dds_results(resMFSourceOrdered)
# plot(hist(resMFSourceOrdered$padj))
# p_value_distribution <- ggplot(as.data.frame(resMFSourceOrdered), aes(x = padj)) + 
#   stat_bin(bins = 50, fill =  "white", colour = "black") + 
#   ylab("Frequency") +
#   xlab("Adjusted p-value") +
#   ggtitle() + 
#   common_theme
##############################

# Compare SCC vs SCC_PL+C, SCC vs all, SCC+SCC_PL vs all, Compare AK vs all
dds_SCCPL_C <-DESeqDataSetFromMatrix(countData = otu.m, colData = metadata.df, design = ~ Sampletype_SCCPL_C)
dds_SCC <- DESeqDataSetFromMatrix(countData = otu.m, colData = metadata.df, design = ~ Sampletype_SCC)
dds_SCC_SCCPL <- DESeqDataSetFromMatrix(countData = otu.m, colData = metadata.df, design = ~ Sampletype_SCC_both)
dds_AK <- DESeqDataSetFromMatrix(countData = otu.m, colData = metadata.df, design = ~ Sampletype_AK)

geoMeans <- apply(counts(dds_SCCPL_C), 1, gm_mean)
#dds <- estimateSizeFactors(dds_SCCPL_C, geoMeans = geoMeans)
dds_SCCPL_C <- estimateSizeFactors(dds_SCCPL_C)

geoMeans <- apply(counts(dds_SCC), 1, gm_mean)
#dds_SCC <- estimateSizeFactors(dds_SCC, geoMeans = geoMeans)
dds_SCC <- estimateSizeFactors(dds_SCC)

geoMeans <- apply(counts(dds_SCC_SCCPL), 1, gm_mean)
#dds_SCCPL <- estimateSizeFactors(dds_SCC_SCCPL, geoMeans = geoMeans)
dds_SCCPL <- estimateSizeFactors(dds_SCC_SCCPL)

geoMeans <- apply(counts(dds_AK), 1, gm_mean)
#dds_AK <- estimateSizeFactors(dds_AK, geoMeans = geoMeans)
dds_AK <- estimateSizeFactors(dds_AK)

# Calculate results
# dds_SCCPL_C <- DESeq(dds_SCCPL_C, betaPrior = FALSE)
# dds_SCC<-DESeq(dds_SCC, betaPrior = FALSE)
# dds_SCCPL<-DESeq(dds_SCC_SCCPL, betaPrior = FALSE)
# dds_AK<-DESeq(dds_AK, betaPrior = FALSE)
dds_SCCPL_C <- DESeq(dds_SCCPL_C)
dds_SCC<-DESeq(dds_SCC)
dds_SCC_SCCPL<-DESeq(dds_SCC_SCCPL)
dds_AK<-DESeq(dds_AK)

# Get results for each contrast
res_SCCPL_C <- results(dds_SCCPL_C, contrast = c("Sampletype_SCCPL_C", "SCC","SCCPL_C"), alpha = 0.05, independentFiltering = F, cooksCutoff = F)
res_SCC <- results(dds_SCC, contrast = c("Sampletype_SCC", "SCC", "all"),  alpha = 0.05, independentFiltering = F, cooksCutoff = F)
res_SCC_SCCPL <- results(dds_SCC_SCCPL, contrast = c("Sampletype_SCC_both", "SCC", "all"), alpha = 0.05, independentFiltering = F, cooksCutoff = F)
res_AK <- results(dds_AK, contrast = c("Sampletype_AK", "AK", "all"), alpha = 0.05, independentFiltering = F, cooksCutoff = F)

# Order the result tables
res_SCCPL_C_Ordered <- res_SCCPL_C[order(res_SCCPL_C$padj),]
res_SCC_Ordered <- res_SCC[order(res_SCC$padj),]
res_SCC_SCCPL_Ordered <- res_SCC_SCCPL[order(res_SCC_SCCPL$padj),]
res_AK_Ordered <- res_AK[order(res_AK$padj),]

# Assign taxonomies
res_SCCPL_C_Ordered$taxonomy <- assign_taxonomy_to_otu(res_SCCPL_C_Ordered, otu_taxonomy_map.df)
res_SCC_Ordered$taxonomy <- assign_taxonomy_to_otu(res_SCC_Ordered, otu_taxonomy_map.df)
res_SCC_SCCPL_Ordered$taxonomy <- assign_taxonomy_to_otu(res_SCC_SCCPL_Ordered, otu_taxonomy_map.df)
res_AK_Ordered$taxonomy <- assign_taxonomy_to_otu(res_AK_Ordered, otu_taxonomy_map.df)

# Filter result tables
res_SCCPL_C_Ordered <- filter_and_sort_dds_results(res_SCCPL_C_Ordered)
res_SCC_Ordered <- filter_and_sort_dds_results(res_SCC_Ordered)
res_SCC_SCCPL_Ordered <- filter_and_sort_dds_results(res_SCC_SCCPL_Ordered)
res_AK_Ordered <- filter_and_sort_dds_results(res_AK_Ordered)

# Write the results to file
write.csv(as.data.frame(res_SCCPL_C_Ordered),file="Result_tables/DESeq_results/SCC_vs_SCC_PL_C.csv", quote = F)
write.csv(as.data.frame(res_SCC_Ordered),file="Result_tables/DESeq_results/SCC_vs_all.csv", quote = F)
write.csv(as.data.frame(res_SCC_SCCPL_Ordered),file="Result_tables/DESeq_results/SCC_both_vs_all.csv", quote = F)
write.csv(as.data.frame(res_AK_Ordered),file="Result_tables/DESeq_results/AK_vs_all.csv", quote = F)
