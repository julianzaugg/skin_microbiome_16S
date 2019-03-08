

library(vegan)
library(ggplot2)
library(ggfortify)

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
lesion_pallete_7 <- c("#8558d6","#6ee268","#d247ad","#c9d743","#d7453e","#59a237","#d78f2a")
patient_pallete_45 <- c("#d64530","#585fb1","#795d97","#9e4773","#3f6921","#71692c","#a2b93c","#d571cc","#9b3e97","#33947a","#98ad66","#448a4e","#869ae0","#5ce7af","#e085a3","#dfdc87","#d19be2","#5cb735","#e38269","#3db6c0","#50b565","#50902c","#a98a2c","#dde84a","#db3d76","#5fe485","#7c8329","#b3e791","#6fe965","#5ebce9","#3c86c1","#2a6a45","#65b688","#6651d1","#af4ed3","#df872f","#56e4db","#737cea","#ac464b","#dd37b5","#995b2b","#daac6f","#92e2be","#a2e24b","#e0be3a")
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

# Load count table at the OTU level. These are the counts for OTUs that were above our abundance thresholds
otu_rare.df <- read.table("Result_tables/count_tables/OTU_counts_rarefied.csv", sep =",", header =T)
otu.df <- read.table("Result_tables/count_tables/OTU_counts.csv", sep =",", header =T)

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the metadata.df
# metadata.df <- read.table("data/metadata.tsv", sep ="\t", header = T)
metadata.df <- read.table("data/metadata_immunocompromised_competent.tsv", sep ="\t", header = T)

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
samples_removed <- metadata.df$Index[!metadata.df$Index %in% names(otu_rare.df)]
metadata.df <- metadata.df[! metadata.df$Index %in% samples_removed,]
metadata.df$Patient <- factor(metadata.df$Patient)
metadata.df$Sampletype <- factor(metadata.df$Sampletype)
metadata.df$Project <- factor(metadata.df$Project)

# metadata.df <- metadata.df[metadata.df$Project == "immunocompetent",]

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC and SCC lesions. 
# Remove samples for different lesion types (nasal,scar,scar_PL,KA,KA_PL,VV,VV_PL,SF,SF_PL,other,other_PL) from metadata and otu table
metadata.df <- metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC", "NLC"),]
otu_rare.df <- otu_rare.df[,names(otu_rare.df) %in% c("OTU.ID", as.character(metadata.df$Index))]
otu.df <- otu.df[,names(otu.df) %in% c("OTU.ID", as.character(metadata.df$Index))]

pool_1 <- c("C","AK_PL","IEC_PL","SCC_PL", "NLC")
pool_2 <- c("AK","IEC")
pool_3 <- c("AK_PL","IEC_PL","SCC_PL")


metadata.df$Sampletype_pooled <- factor(as.character(lapply(metadata.df$Sampletype, function(x) ifelse(x %in% pool_1, "NLC", ifelse(x %in% pool_2, "AK", "SCC")))))
metadata.df$Sampletype_pooled_C_sep <- factor(as.character(lapply(metadata.df$Sampletype, function(x) ifelse(x %in% pool_3, "NLC", 
                                                                                                             ifelse(x %in% pool_2, "AK", 
                                                                                                                    ifelse(x %in% c("C", "NLC"), "C","SCC"))))))

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# Create matrices
otu_rare.m <- otu_rare.df
rownames(otu_rare.m) <- otu_rare.df$OTU.ID
otu_rare.m$OTU.ID <- NULL
otu_rare.m <- as.matrix(otu_rare.m)

otu.m <- otu.df
rownames(otu.m) <- otu.df$OTU.ID
otu.m$OTU.ID <- NULL
otu.m <- as.matrix(otu.m)

# Order the matrices and metadata to be the same order
metadata.df <- metadata.df[order(rownames(metadata.df)),]
otu_rare.m <- otu_rare.m[,order(rownames(metadata.df))]
otu.m <- otu.m[,order(rownames(metadata.df))]

# Just get OTUs with more than counts of 3
dim(otu_rare.m)
otu_rare_filtered.m <- otu_rare.m[apply(otu_rare.m,1,max) >= 3,]
dim(otu_rare_filtered.m)

dim(otu.m)
otu_filtered.m <- otu_rare.m[apply(otu.m,1,max) >= 3,]
dim(otu_filtered.m)

# CLR transform the otu matrix.
#otu_clr.m <- apply(otu_rare_count.m, MARGIN = 2, FUN = clr)
otu_rare_clr_filtered.m <- clr(otu_rare_filtered.m)
# head(otu_rare_clr_filtered.m)[,1:2]
# head(apply(otu_rare_filtered.m, MARGIN = 2, FUN = clr))[,1:2]
otu_clr_filtered.m <- clr(otu_filtered.m)

# If there are negative values, assign them a value of zero
otu_rare_clr_filtered.m[which(otu_rare_clr_filtered.m < 0)] <- 0
otu_clr_filtered.m[which(otu_clr_filtered.m < 0)] <- 0

# Determine which samples are missing metadata and remove them
variables_of_interest <- c("Sampletype", "Patient","Sampletype_pooled", "Project")
# samples_to_remove <- get_samples_missing_data(metadata.df, variables_of_interest)
# metadata.df <- metadata.df[!rownames(metadata.df) %in% samples_to_remove,]
#metadata.df <- metadata.df["Sampletype"]
metadata.df <- metadata.df[variables_of_interest]

#############################
# PERMANOVA analysis

# Permutational Multivariate Analysis of Variance (PERMANOVA) can be used to 
# determine if the structure of the microbial communities is significantly different between
# environmental variables. This is done using the adonis function from Vegan with a distance metric, e.g. Bray-Curtis, and a
# specified number of permutations, e.g. 10,000.
# The analysis measures the degree each environmental variable affects the community composition and indicates 
# the significance of that effect on beta diversity (described by p-values and R2 values). 
# The R2 value corresponds to the proportion of variability observed in the dissimilarity.


# The function below will only calculate the the significance of individual variables
run_permanova <- function(my_community_data, my_metadata, my_variables){
  stat_sig_table <- NULL
  for (var_name in my_variables) {
    result <- adonis(my_community_data~get(var_name),data = my_metadata, permu=999,method="euclidean")
    SumOfSqs <- round(result$aov.tab$SumsOfSqs[1], 3)
    meanSqs <- round(result$aov.tab$MeanSqs[1], 3)
    F.model <- round(result$aov.tab$F.Model[1], 3)
    R2 <- round(result$aov.tab$R2[1], 3)
    p_value <- round(result$aov.tab$`Pr(>F)`[1], 5)
    stat_sig_table <- rbind(stat_sig_table, data.frame(var_name,
                                                       SumOfSqs,
                                                       meanSqs,
                                                       F.model,
                                                       R2,
                                                       p_value))
  }
  names(stat_sig_table) <- c("Variable", "SumOfSqs","MeanSqs","F.Model","R2","P-value")
  stat_sig_table <- stat_sig_table[order(stat_sig_table$"P-value"),]
  stat_sig_table
}

# adonis(t(otu_rare_clr_filtered.m)~Sampletype,data = metadata.df, permu=999,method="euclidean")
# adonis(t(otu_rare_clr_filtered.m)~Sampletype,data = metadata.df, permu=999,method="bray")

# adonis(t(otu_rare_clr_filtered.m)~Patient,data = metadata.df, permu=999,method="euclidean")
# adonis(t(otu_rare_clr_filtered.m)~Sampletype,data = metadata.df, permu=999,method="euclidean")
# adonis(t(otu_rare_clr_filtered.m)~Sampletype_pooled,data = metadata.df, permu=999,method="euclidean")
# adonis(t(otu_rare_clr_filtered.m)~Patient+Sampletype+Sampletype_pooled+Patient:Sampletype+Patient:Sampletype_pooled,data = metadata.df, permu=999,method="euclidean")
# adonis(t(otu_rare_clr_filtered.m)~Patient+Sampletype_pooled+Sampletype+Patient:Sampletype+Patient:Sampletype_pooled,data = metadata.df, permu=999,method="euclidean")

# permanova_results_OTU <- run_permanova(t(otu_rare_clr_filtered.m), metadata.df, variables_of_interest)
# write.csv(permanova_results_OTU,file="Result_tables/stats_various/PERMANOVA_otu_clr_rarified.csv",row.names = F)

#############################
# ENVFIT analysis

# To support whether there is a significant association with, or contribution from, environmental variables 
# to the microbial community composition, the envfit function of Vegan can be used to calculate the goodness of 
# fit of environmental variables against a PCoA ordination. The goodness of fit is described by R2 and corresponding p-values. 
# The R2 values correspond to the proportion of the variability that can be attributed to the variables. The function uses a 
# permutation approach to calculate these values by randomly permuting the data and then performing a statistical test for 
# goodness of fit.

# The function below will run envfit for continuous and discrete variables in combination with the community matrix.
# Assumes rownames in metadata are sample names and variables are columns in the metadata. If variables are not provided, or
# none of the variables provided are in the metadata, the function will loop over all columns that are in the metadata.
run_envfit <- function(my_community_data, my_metadata, my_variables){
  
  if (!missing(my_variables)){
    internal_function_variables <- my_variables[my_variables %in% colnames(my_metadata)]
    if (!length(internal_function_variables) > 0){
      warning("None of the specified variables were found in the metadata, using all columns.")
      internal_function_variables <- colnames(my_metadata)
    }
  }
  else {
    internal_function_variables <- colnames(my_metadata)
  }
  internal_function_metadata <- my_metadata[internal_function_variables]
  
  # Unconstrained multivariate ordination of dissimilarity matrices
  #m.pcoa <- capscale(my_community_data~1, data = internal_function_metadata, dist = "bray") # ~1 makes it unconstrained
  m.pcoa <- capscale(my_community_data~1, data = internal_function_metadata, dist = "euclidean") # ~1 makes it unconstrained
  
  # Fit environmental variables to the ordination
  myfit <- envfit(m.pcoa, env = internal_function_metadata, permutations = 999)
  
  envfit_table <- NULL
  
  # Calculate the significance of continuous variables, if any
  for (var_name in names(myfit$vectors$r)){
    pvalue <- round(myfit$vectors$pvals[1],5)
    r2 <- round(myfit$vectors$r[1],3)
    envfit_table <- rbind(envfit_table, data.frame(var_name, pvalue, r2))
  }
  
  # Calculate the significance of discrete variables, if any
  for (var_name in names(myfit$factors$r)){
    pvalue <- round(myfit$factors$pvals[[var_name]],5)
    r2 <- round(myfit$factors$r[[var_name]],3)
    envfit_table <- rbind(envfit_table, data.frame(var_name, pvalue, r2))
  }
  
  # Get columns of interest
  envfit_table <- envfit_table[c("var_name","r2","pvalue")]
  
  # Order by the p-value
  envfit_table <- envfit_table[order(envfit_table$pvalue),]
  
  # Change column names
  names(envfit_table) <- c("Variable","R2", "P-value")
  
  return(envfit_table)
}

# envfit_results_OTU <- run_envfit(t(otu_rare_clr_filtered.m), metadata.df, variables_of_interest)
# write.csv(envfit_results_OTU,file="Result_tables/stats_various/ENVFIT_otu_clr_rarified.csv", row.names = F)


#######################################################################################
# Ordination analysis

# PCoA analysis (same as PCA but option of distance; unconstrained ordination)

############
# Manually specify plots
# Note - don't use bray-curtis as we are using CLR transformed values
# m.pcoa <- capscale(t(otu_rare_clr_filtered.m)~1, data = metadata.df, dist = "bray") # ~1 makes it unconstrained
# m.pcoa <- capscale(t(otu_rare_clr_filtered.m)~1, data = metadata.df, dist = "euclidean") # ~1 makes it unconstrained
#m.pcoa <- capscale(t(otu_clr_filtered.m)~1, data = metadata.df, dist = "euclidean") # ~1 makes it unconstrained
m.pcoa <- rda(t(otu_rare_clr_filtered.m), data = metadata.df) # ~1 makes it unconstrained
# m.pcoa <- prcomp(t(otu_rare_clr_filtered.m), center = T) # ~1 makes it unconstrained
#m.pcoa <- capscale(t(otu_rare_clr_filtered.m)~1, data = metadata.df, dist = "euclidean", scale = F) # ~1 makes it unconstrained
# m.pcoa <- rda(t(otu_rare_clr_filtered.m), data = metadata.df, center = F)
# temp <- prcomp(t(otu_rare_clr_filtered.m), center = F)
# ?prcomp
# plot(temp$x)
################################################################################################

pcoa.scores <- scores(m.pcoa, choices=c(1,2,3))

# temp <- prcomp(t(otu_rare_clr_filtered.m), center = T)
# autoplot(temp)

#temp <- prcomp(otu_rare_clr_filtered.m)
# Get component x,y coordinates
pcoa_site_scores <- scores(m.pcoa, display = "sites")
pcoa_specie_scores <- scores(m.pcoa, display = "species")
pcoa_percentages <- (m.pcoa$CA$eig/sum(m.pcoa$CA$eig)) * 100

# If prcomp
# paste0("PC1 (", round((summary(m.pcoa)$importance[2,'PC1']*100), 2), "%)")
# paste0("PC2 (", round((summary(m.pcoa)$importance[2,'PC2']*100), 2), "%)")

x_max <- round(lapply(max(pcoa_site_scores[,1]), function(x) ifelse(x > 0, x + 1, x - 1))[[1]])
x_min <- round(lapply(min(pcoa_site_scores[,1]), function(x) ifelse(x > 0, x + 1, x - 1))[[1]])
y_max <- round(lapply(max(pcoa_site_scores[,2]), function(x) ifelse(x > 0, x + 1, x - 1))[[1]])
y_min <- round(lapply(min(pcoa_site_scores[,2]), function(x) ifelse(x > 0, x + 1, x - 1))[[1]])


my_xlab = paste("MDS1 (", round(pcoa_percentages[1],1), "%)", sep = "")
my_ylab = paste("MDS2 (", round(pcoa_percentages[2],1), "%)", sep = "")

# Colour by patient first
#metadata.df <- metadata.df[order(metadata.df$Patient),]
# Order the metadata by name and then Patient
metadata_ordered.df <- metadata.df[order(rownames(metadata.df)),]
metadata_ordered.df <- metadata_ordered.df[order(metadata_ordered.df$Patient),]

pdf("Result_figures/pcoa_dbrda_plots/patient_pcoa.pdf",height=10,width=10)
# plot(m.pcoa,          
#      type='n',         
#      xlim = c(-9,4), 
#      ylim = c(-5,9), 
#      xlab = my_xlab, 
#      ylab = my_ylab,
#      xaxt = "n")
plot(m.pcoa,
     type='n',
     xlim = c(x_min-2,x_max),
     ylim = c(y_min,y_max),
     xlab = my_xlab,
     ylab = my_ylab)
# Make grid
grid(NULL,NULL, lty = 2, col = "grey80")

# Assign (unique) colours and shapes for each grouping variable
variable_values <- factor(sort(as.character(unique(metadata_ordered.df[["Patient"]]))))
# variable_colours <- setNames(my_colour_pallete_32_distinct[1:length(variable_values)], variable_values)
variable_colours <- setNames(patient_pallete_45[1:length(variable_values)], variable_values)
variable_shapes <- setNames(rep(c(21),length(variable_values))[1:length(variable_values)],variable_values)
#variable_shapes <- setNames(c(25,24,23,22,21,8,6,5,4,3,2,1)[1:length(variable_values)],variable_values)


# Order the site scores by the order of the rows in the metadata
pcoa_site_scores <- pcoa_site_scores[rownames(metadata_ordered.df),]

all_sample_colours <- as.character(
  lapply(
    as.character(metadata_ordered.df[rownames(pcoa_site_scores),"Patient"]), 
    function(x) variable_colours[x]
  )
)

all_sample_shapes <- as.numeric(
  lapply(
    as.character(sort(metadata_ordered.df[rownames(pcoa_site_scores),"Patient"])), 
    function(x) variable_shapes[x][[1]]
  )
)

# Plot ellipses that are filled
plot_ellipses <- function (label_ellipse = F) {
  for (member in variable_values){
    if (nrow(metadata_ordered.df[metadata_ordered.df[["Patient"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
      ordiellipse(pcoa_site_scores,
                  groups = metadata_ordered.df$Patient,
                  kind = "ehull",
                  border = variable_colours[member][[1]],
                  col = variable_colours[member][[1]],
                  show.groups = member,
                  alpha = .05,
                  draw = "polygon",
                  label = label_ellipse)
    }
  }
}
# plot_ellipses(T)

#Plot spiders
plot_spiders <- function (label_spider = F) {
  for (member in variable_values){
    if (nrow(metadata_ordered.df[metadata_ordered.df[["Patient"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
      ordispider(pcoa_site_scores,
                  groups = metadata_ordered.df$Patient,
                  border = variable_colours[member][[1]],
                  col = variable_colours[member][[1]],
                  show.groups = member,
                  #alpha = .05,
                  label = label_spider)
    }
  }
}
# plot_spiders(F)

points(pcoa_site_scores, 
       cex = 0.8,
       pch = all_sample_shapes,
       #col = all_sample_colours,
       col = "black",
       bg = all_sample_colours,
)

# text(x = pcoa_site_scores[,1],
#      y = pcoa_site_scores[,2],
#      labels = rownames(pcoa_site_scores),
#      cex = .5,
#      pos = 2)

legend(
  title = expression(bold("Patient")),
  title.col="black",
  x = x_min-2,
  y = y_max-2, 
  legend= variable_values, 
  pch= unique(all_sample_shapes), 
  #col= unique(all_sample_colours),
  col= "black",
  pt.bg = unique(all_sample_colours),
  #bg = "white",
  bty = "n",
  ncol = 2,
  cex = 0.8
)
# axis(side = 1, at = c(-9:4), labels = c(-9:4) )
dev.off()


#######
metadata_ordered.df <- metadata.df[order(rownames(metadata.df)),]
metadata_ordered.df <- metadata_ordered.df[order(metadata_ordered.df$Patient),]

pdf("Result_figures/pcoa_dbrda_plots/Project_pcoa.pdf",height=10,width=10)
# plot(m.pcoa,          
#      type='n',         
#      xlim = c(-9,4), 
#      ylim = c(-5,9), 
#      xlab = my_xlab, 
#      ylab = my_ylab,
#      xaxt = "n")
plot(m.pcoa,
     type='n',
     xlim = c(x_min-2,x_max),
     ylim = c(y_min,y_max),
     xlab = my_xlab,
     ylab = my_ylab)
# Make grid
grid(NULL,NULL, lty = 2, col = "grey80")

# Assign (unique) colours and shapes for each grouping variable
variable_values <- factor(sort(as.character(unique(metadata_ordered.df[["Project"]]))))
# variable_colours <- setNames(my_colour_pallete_32_distinct[1:length(variable_values)], variable_values)
variable_colours <- setNames(patient_pallete_45[1:length(variable_values)], variable_values)
variable_shapes <- setNames(rep(c(21),length(variable_values))[1:length(variable_values)],variable_values)
#variable_shapes <- setNames(c(25,24,23,22,21,8,6,5,4,3,2,1)[1:length(variable_values)],variable_values)


# Order the site scores by the order of the rows in the metadata
pcoa_site_scores <- pcoa_site_scores[rownames(metadata_ordered.df),]

all_sample_colours <- as.character(
  lapply(
    as.character(metadata_ordered.df[rownames(pcoa_site_scores),"Project"]), 
    function(x) variable_colours[x]
  )
)

all_sample_shapes <- as.numeric(
  lapply(
    as.character(sort(metadata_ordered.df[rownames(pcoa_site_scores),"Project"])), 
    function(x) variable_shapes[x][[1]]
  )
)

# Plot ellipses that are filled
plot_ellipses <- function (label_ellipse = F) {
  for (member in variable_values){
    if (nrow(metadata_ordered.df[metadata_ordered.df[["Project"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
      ordiellipse(pcoa_site_scores,
                  groups = metadata_ordered.df$Project,
                  kind = "ehull",
                  border = variable_colours[member][[1]],
                  col = variable_colours[member][[1]],
                  show.groups = member,
                  alpha = .05,
                  draw = "polygon",
                  label = label_ellipse)
    }
  }
}
# plot_ellipses(T)

#Plot spiders
plot_spiders <- function (label_spider = F) {
  for (member in variable_values){
    if (nrow(metadata_ordered.df[metadata_ordered.df[["Project"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
      ordispider(pcoa_site_scores,
                 groups = metadata_ordered.df$Project,
                 border = variable_colours[member][[1]],
                 col = variable_colours[member][[1]],
                 show.groups = member,
                 #alpha = .05,
                 label = label_spider)
    }
  }
}
# plot_spiders(F)

points(pcoa_site_scores, 
       cex = 0.8,
       pch = all_sample_shapes,
       #col = all_sample_colours,
       col = "black",
       bg = all_sample_colours,
)

# text(x = pcoa_site_scores[,1],
#      y = pcoa_site_scores[,2],
#      labels = rownames(pcoa_site_scores),
#      cex = .5,
#      pos = 2)

legend(
  title = expression(bold("Project")),
  title.col="black",
  x = x_min-2,
  y = y_max-2, 
  legend= variable_values, 
  pch= unique(all_sample_shapes), 
  #col= unique(all_sample_colours),
  col= "black",
  pt.bg = unique(all_sample_colours),
  #bg = "white",
  bty = "n",
  ncol = 2,
  cex = 0.8
)
# axis(side = 1, at = c(-9:4), labels = c(-9:4) )
dev.off()




#######


# Colour by sampletype
metadata_ordered.df <- metadata.df[order(rownames(metadata.df)),]
metadata_ordered.df <- metadata_ordered.df[order(metadata_ordered.df$Sampletype),]

pdf("Result_figures/pcoa_dbrda_plots/sampletype_pcoa.pdf",height=10,width=10)

plot(m.pcoa,
     type='n',
     xlim = c(x_min,x_max),
     ylim = c(y_min,y_max),
     xlab = my_xlab,
     ylab = my_ylab)
# Make grid
grid(NULL,NULL, lty = 2, col = "grey80")

# Assign (unique) colours and shapes for each grouping variable
variable_values <- factor(sort(as.character(unique(metadata_ordered.df[["Sampletype"]]))))
variable_colours <- setNames(my_colour_pallete_32_distinct[1:length(variable_values)], variable_values)
variable_shapes <- setNames(rep(c(21),length(variable_values))[1:length(variable_values)],variable_values)
#variable_shapes <- setNames(c(25,24,23,22,21,8,6,5,4,3,2,1)[1:length(variable_values)],variable_values)


# Order the site scores by the order of the rows in the metadata
pcoa_site_scores <- pcoa_site_scores[rownames(metadata_ordered.df),]

all_sample_colours <- as.character(
  lapply(
    as.character(metadata_ordered.df[rownames(pcoa_site_scores),"Sampletype"]), 
    function(x) variable_colours[x]
  )
)

all_sample_shapes <- as.numeric(
  lapply(
    as.character(sort(metadata_ordered.df[rownames(pcoa_site_scores),"Sampletype"])), 
    function(x) variable_shapes[x][[1]]
  )
)

# Plot ellipses that are filled
plot_ellipses <- function (label_ellipse = F) {
  for (member in variable_values){
    if (nrow(metadata_ordered.df[metadata_ordered.df[["Sampletype"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
      ordiellipse(pcoa_site_scores,
                  groups = metadata_ordered.df$Sampletype,
                  kind = "ehull",
                  border = variable_colours[member][[1]],
                  col = variable_colours[member][[1]],
                  show.groups = member,
                  alpha = .05,
                  draw = "polygon",
                  label = label_ellipse)
    }
  }
}
# plot_ellipses(F)

#Plot spiders
plot_spiders <- function (label_spider = F) {
  for (member in variable_values){
    if (nrow(metadata_ordered.df[metadata_ordered.df[["Sampletype"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
      ordispider(pcoa_site_scores,
                 groups = metadata_ordered.df$Sampletype,
                 col = variable_colours[member][[1]],
                 show.groups = member,
                 #alpha = .05,
                 label = label_spider)
    }
  }
}
# plot_spiders(F)


points(pcoa_site_scores, 
       cex = 0.8,
       pch = all_sample_shapes,
       #col = all_sample_colours,
       col = "black",
       bg = all_sample_colours,
)

# text(x = pcoa_site_scores[,1],
#      y = pcoa_site_scores[,2],
#      labels = rownames(pcoa_site_scores),
#      cex = .5,
#      pos = 2)

legend(
  title = expression(bold("Sampletype")),
  title.col="black",
  x = x_min,
  y = y_max, 
  legend= variable_values, 
  pch= unique(all_sample_shapes), 
  #col= unique(all_sample_colours),
  col= "black",
  pt.bg = unique(all_sample_colours),
  #bg = "white",
  bty = "n",
  ncol = 2,
  cex = 0.8
)
# axis(side = 1, at = c(-9:4), labels = c(-9:4) )
dev.off()


# Colour by sampletype pooled
metadata_ordered.df <- metadata.df[order(rownames(metadata.df)),]
metadata_ordered.df <- metadata_ordered.df[order(metadata_ordered.df$Sampletype_pooled),]

pdf("Result_figures/pcoa_dbrda_plots/sampletype_pooled_pcoa.pdf",height=10,width=10)

plot(m.pcoa,
     type='n',
     xlim = c(x_min,x_max),
     ylim = c(y_min,y_max),
     xlab = my_xlab,
     ylab = my_ylab)
# Make grid
grid(NULL,NULL, lty = 2, col = "grey80")

# Assign (unique) colours and shapes for each grouping variable
variable_values <- factor(sort(as.character(unique(metadata_ordered.df[["Sampletype_pooled"]]))))
variable_colours <- setNames(my_colour_pallete_32_distinct[1:length(variable_values)], variable_values)
variable_shapes <- setNames(rep(c(21),length(variable_values))[1:length(variable_values)],variable_values)
#variable_shapes <- setNames(c(25,24,23,22,21,8,6,5,4,3,2,1)[1:length(variable_values)],variable_values)


# Order the site scores by the order of the rows in the metadata
pcoa_site_scores <- pcoa_site_scores[rownames(metadata_ordered.df),]

all_sample_colours <- as.character(
  lapply(
    as.character(metadata_ordered.df[rownames(pcoa_site_scores),"Sampletype_pooled"]), 
    function(x) variable_colours[x]
  )
)

all_sample_shapes <- as.numeric(
  lapply(
    as.character(sort(metadata_ordered.df[rownames(pcoa_site_scores),"Sampletype_pooled"])), 
    function(x) variable_shapes[x][[1]]
  )
)

# Plot ellipses that are filled
plot_ellipses <- function (label_ellipse = F) {
  for (member in variable_values){
    if (nrow(metadata_ordered.df[metadata_ordered.df[["Sampletype_pooled"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
      ordiellipse(pcoa_site_scores,
                  groups = metadata_ordered.df$Sampletype_pooled,
                  kind = "ehull",
                  border = variable_colours[member][[1]],
                  col = variable_colours[member][[1]],
                  show.groups = member,
                  alpha = .05,
                  draw = "polygon",
                  label = label_ellipse)
    }
  }
}
# plot_ellipses(F)

#Plot spiders
plot_spiders <- function (label_spider = F) {
  for (member in variable_values){
    if (nrow(metadata_ordered.df[metadata_ordered.df[["Sampletype_pooled"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
      ordispider(pcoa_site_scores,
                 groups = metadata_ordered.df$Sampletype_pooled,
                 col = variable_colours[member][[1]],
                 show.groups = member,
                 #lwd = .1,
                 label = label_spider)
    }
  }
}
# plot_spiders(F)

points(pcoa_site_scores, 
       cex = 0.8,
       pch = all_sample_shapes,
       #col = all_sample_colours,
       col = "black",
       bg = all_sample_colours,
)

# text(x = pcoa_site_scores[,1],
#      y = pcoa_site_scores[,2],
#      labels = rownames(pcoa_site_scores),
#      cex = .5,
#      pos = 2)

legend(
  title = expression(bold("Sampletype pooled")),
  title.col="black",
  x = x_min,
  y = y_max, 
  legend= variable_values, 
  pch= unique(all_sample_shapes), 
  #col= unique(all_sample_colours),
  col= "black",
  pt.bg = unique(all_sample_colours),
  #bg = "white",
  bty = "n",
  ncol = 2,
  cex = 0.8
)
# axis(side = 1, at = c(-9:4), labels = c(-9:4) )
dev.off()


# #######################################################################################
# # db-RDA analysis (constrained ordination)
# 
# # Distance-based redundancy analysis (db-RDA) is a constrained ordination method and is an extension of PCoA. 
# # db-RDA is used to predict the shifts in the community based on the environmental variables.
# # Constrained ordination focuses on only the variation that can be explained by the constraining environmental variables. 
# # The approach makes a few statistical assumptions about how the predictor (environmental) variables influence the dependent variables (species). 
# # The results explain a smaller proportion of total variation than unconstrained ordination, 
# # but they are more strongly interpretable with the constraints.
# 
# #### Constraining by Patient
# #mydbRDA <- capscale(t(otu_rare_clr_filtered.m)~Patient, data = metadata.df, dist = "bray")
# mydbRDA <- capscale(t(otu_rare_clr_filtered.m)~Patient, data = metadata.df, dist = "euclidean")
# 
# # Extract scores and calculate eigenvalue percentages
# mydbRDA.scores <- scores(mydbRDA, choices=c(1,2,3))
# mydbRDA_percentages <- (mydbRDA$CA$eig/sum(mydbRDA$CA$eig)) * 100
# 
# # Make axis labels
# my_xlab = paste("MDS1 (", round(mydbRDA_percentages[1],1), "%)", sep = "")
# my_ylab = paste("MDS2 (", round(mydbRDA_percentages[2],1), "%)", sep = "")
# 
# pdf("Result_figures/pcoa_dbrda_plots/patient_dbrda.pdf",height=8,width=8)
# 
# # Generate plot
# plot(mydbRDA,          
#      type='n',         
#      xlim = c(-3,2), 
#      ylim = c(-2,2), 
#      xlab = my_xlab, 
#      ylab = my_ylab)
# 
# # Set the background colour
# #rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray97")
# 
# # Plot grid
# grid(NULL,NULL, lty = 2, col = "grey80")
# 
# # Define the mapping between unique values of the variable to colours and shapes.
# variable_values <- factor(sort(as.character(unique(metadata.df[["Patient"]]))))
# variable_colours <- setNames(my_colour_pallete_32_distinct[1:length(variable_values)], variable_values)
# #variable_shapes <- setNames(c(25,24,23,22,21,8,6,5,4,3,2,1)[1:length(variable_values)],variable_values)
# # variable_shapes <- setNames(rep(c(25,24,23,22,21,8,6,5,4,3,2,1),5)[1:length(variable_values)],variable_values)
# variable_shapes <- setNames(rep(c(21),length(variable_values))[1:length(variable_values)],variable_values)
# # Extract scores specific to sites (samples)
# dbrda_site_scores <- scores(mydbRDA, display = "sites")
# 
# 
# all_sample_colours <- as.character(
#   lapply(
#     as.character(
#       metadata.df[rownames(dbrda_site_scores),"Patient"]
#     ), 
#     function(x) variable_colours[x]
#   )
# )
# 
# all_sample_shapes <- as.numeric(
#   lapply(
#     as.character(
#       metadata.df[rownames(dbrda_site_scores),"Patient"]
#     ), 
#     function(x) variable_shapes[x]
#   )
# )
# 
# 
# 
# # Plot ellipses that are filled
# for (member in variable_values){
#   if (nrow(metadata.df[metadata.df[["Patient"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
#     ordiellipse(mydbRDA,
#                 groups = metadata.df$Patient,
#                 kind = "ehull",
#                 border = variable_colours[member][[1]],
#                 col = variable_colours[member][[1]],
#                 show.groups = member,
#                 alpha = .05,
#                 draw = "polygon")
#   }
# }
# 
# 
# # Plot the points for the samples
# points(mydbRDA, 
#        cex = 0.8,
#        pch = all_sample_shapes,  # Shape of point
#        col = all_sample_colours, # Outline color of point
#        #bg = all_sample_colours, # Background colour for point
# )
# 
# 
# # Plot spiders
# # for (member in variable_values){
# #   if (nrow(metadata.df[metadata.df[["Patient"]] == member,]) > 1){ # if too few samples, skip plotting spider
# #     ordispider(mydbRDA,
# #                groups = metadata.df$Patient,
# #                kind = "ehull",
# #                col = variable_colours[member][[1]],
# #                show.groups = member)
# #  }
# # }
# # plot the legend
# legend(
#   title = expression(bold("Patient")),
#   title.col="black",
#   x = -3,
#   y =2, 
#   legend= variable_values, 
#   pch= unique(all_sample_shapes), 
#   col= unique(all_sample_colours),
#   # pt.bg = unique(all_sample_colours),
#   bg = "white",
#   box.col = NA,
#   cex = 0.8
# )
# dev.off()
# 
# 
# #### Constraining by Sampletype
# mydbRDA <- capscale(t(otu_rare_clr_filtered.m)~Sampletype, data = metadata.df, dist = "bray")
# mydbRDA.scores <- scores(mydbRDA, choices=c(1,2,3))
# mydbRDA_percentages <- (mydbRDA$CA$eig/sum(mydbRDA$CA$eig)) * 100
# 
# my_xlab = paste("MDS1 (", round(mydbRDA_percentages[1],1), "%)", sep = "")
# my_ylab = paste("MDS2 (", round(mydbRDA_percentages[2],1), "%)", sep = "")
# 
# pdf("Result_figures/pcoa_dbrda_plots/lesion_type_dbrda.pdf",height=8,width=8)
# 
# plot(mydbRDA,          
#      type='n',         
#      # xlim = c(-3,2),
#      # ylim = c(-2,2),
#      xlab = my_xlab, 
#      ylab = my_ylab)
# 
# # Set the background colour
# #rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray97")
# 
# grid(NULL,NULL, lty = 2, col = "grey80")
# 
# variable_values <- factor(sort(as.character(unique(metadata.df[["Sampletype"]]))))
# variable_colours <- setNames(my_colour_pallete_20_distinct[1:length(variable_values)], variable_values)
# #variable_shapes <- setNames(c(25,24,23,22,21,8,6,5,4,3,2,1)[1:length(variable_values)],variable_values)
# # variable_shapes <- setNames(rep(c(25,24,23,22,21,8,6,5,4,3,2,1),5)[1:length(variable_values)],variable_values)
# variable_shapes <- setNames(rep(c(21),length(variable_values))[1:length(variable_values)],variable_values)
# dbrda_site_scores <- scores(mydbRDA, display = "sites")
# 
# 
# all_sample_colours <- as.character(
#   lapply(
#     as.character(
#       metadata.df[rownames(dbrda_site_scores),"Sampletype"]
#     ), 
#     function(x) variable_colours[x]
#   )
# )
# 
# all_sample_shapes <- as.numeric(
#   lapply(
#     as.character(
#       metadata.df[rownames(dbrda_site_scores),"Sampletype"]
#     ), 
#     function(x) variable_shapes[x]
#   )
# )
# 
# # Plot ellipses that are filled
# for (member in variable_values){
#   if (nrow(metadata.df[metadata.df[["Sampletype"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
#     ordiellipse(mydbRDA,
#                 groups = metadata.df$Sampletype,
#                 kind = "ehull",
#                 border = variable_colours[member][[1]],
#                 col = variable_colours[member][[1]],
#                 show.groups = member,
#                 alpha = .05,
#                 draw = "polygon")
#   }
# }
# 
# # Plot just ellipses
# for (member in variable_values){
#   if (nrow(metadata.df[metadata.df[["Sampletype"]] == member,]) > 2){ # if too few samples, skip plotting ellipse
#     ordiellipse(mydbRDA,
#                 groups = metadata.df$Sampletype,
#                 kind = "ehull",
#                 col = variable_colours[member][[1]],
#                 show.groups = member)
#   }
# }
# 
# points(mydbRDA, 
#        cex = 0.8,
#        pch = all_sample_shapes,  # Shape of point
#        col = all_sample_colours, # Outline color of point
#        #bg = all_sample_colours, # Background colour for point
# )
# 
# 
# # Plot spiders
# # for (member in variable_values){
# #   if (nrow(metadata.df[metadata.df[["Sampletype"]] == member,]) > 1){ # if too few samples, skip plotting spider
# #     ordispider(mydbRDA,
# #                groups = metadata.df$Sampletype,
# #                kind = "ehull",
# #                col = variable_colours[member][[1]],
# #                show.groups = member)
# #   }
# # }
# 
# # plot the legend
# legend(
#   title = expression(bold("Lesion type")),
#   title.col="black",
#   x = -3,
#   y =2, 
#   legend= variable_values, 
#   pch= unique(all_sample_shapes), 
#   col= unique(all_sample_colours),
#   # pt.bg = unique(all_sample_colours),
#   bg = "white",
#   box.col = NA,
#   cex = 0.8
# )
# 
# dev.off()
