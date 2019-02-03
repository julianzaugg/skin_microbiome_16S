

library(vegan)

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

# Function that takes the metadata and a list of variables (column names) and returns those samples (rownames) with NA entries
get_samples_missing_data <- function(my_metadata, variables){
  samples_missing_data <- c()
  for (name in variables) {
    samples_missing_data <- c(samples_missing_data, rownames(my_metadata[is.na(my_metadata[[name]]),]))
  }
  return(unique(samples_missing_data))
}
############################################################

# Load count table at the OTU level. These are the counts for OTUs that were above our abundance thresholds
otu_rare.df <- read.table("Result_tables/count_tables/OTU_counts_rarified.csv", sep =",", header =T)

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the metadata.df
metadata.df <- read.table("data/metadata.tsv", sep ="\t", header = T)
# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
samples_removed <- metadata.df$Index[!metadata.df$Index %in% names(otu_rare.df)]
metadata.df <- metadata.df[! metadata.df$Index %in% samples_removed,]

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# Create matrices
otu_rare.m <- otu_rare.df
rownames(otu_rare.m) <- otu_rare.df$OTU.ID
otu_rare.m$OTU.ID <- NULL
otu_rare.m <- as.matrix(otu_rare.m)

# Order the matrices and metadata to be the same order
metadata.df <- metadata.df[order(rownames(metadata.df)),]
otu_rare.m <- otu_rare.m[,order(rownames(metadata.df))]

# CLR transform the otu matrix.
otu_rare_clr.m <- clr(otu_rare.m)


# variables_of_interest <- c("Sampletype", "Sampletype")
samples_to_remove <- get_samples_missing_data(metadata.df, variables_of_interest)
# Subset of the metadata describing the variables of interest for samples with entries across all variables
metadata.df <- metadata.df[!rownames(metadata.df) %in% samples_to_remove,]
metadata.df <- metadata.df["Sampletype"]

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
    result <- adonis(my_community_data~get(var_name),data = my_metadata, permu=999,method="bray")
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

permanova_results_OTU <- run_permanova(otu_rare_clr.m, metadata.df, variables_of_interest)
write.csv(permanova_results_OTU,file="Result_tables/stats_various/PERMANOVA_otu_clr_rarified.csv",row.names = F)

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
  m.pcoa <- capscale(my_community_data~1, data = internal_function_metadata, dist = "bray") # ~1 makes it unconstrained
  
  # Fit environmental variables to the ordination
  myfit <- envfit(m.pcoa, env = internal_function_metadata, permutations = 10000)
  
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

envfit_results_OTU <- run_envfit(otu_rare_clr.m, metadata.df, c("Sampletype"))

write.csv(envfit_results_OTU,file="Result_tables/stats_various/ENVFIT_otu.csv", row.names = F)


#######################################################################################
# Ordination analysis

# PCoA analysis (same as PCA but option of distance; unconstrained ordination)

# A basic function to generate a plot for PCoA. 
# Assumes the "variable_of_interest" is a column in the metadata.
# Community data : rows = samples, columns = OTU/genus
generate_pcoa_plot <- function(my_community_data,
                               my_metadata,
                               variable_of_interest,
                               legend_title = "Groups",
                               include_ellipses = TRUE,
                               plot_sample_labels = FALSE)
{
  # We don't actually need to specify data = "" since the environmental variables are not used in unconstrained ordination
  m.pcoa <- capscale(my_community_data~1, data = my_metadata, dist = "bray") # ~1 makes it unconstrained
  
  # Get species and site scores from ordination
  pcoa.scores <- scores(m.pcoa, choices=c(1,2,3))
  
  # Calculate the percentages from the eigenvalues
  pcoa_percentages <- (m.pcoa$CA$eig/sum(m.pcoa$CA$eig)) * 100
  
  # Axis labels for components
  my_xlab = paste("MDS1 (", round(pcoa_percentages[1],1), "%)", sep = "")
  my_ylab = paste("MDS2 (", round(pcoa_percentages[2],1), "%)", sep = "")
  
  # Generate plot. see help(par) for information on plotting parameters
  plot(m.pcoa,           # PCOA object
       type='n',         
       xlim = c(-2,2), 
       ylim = c(-2,2), 
       xlab = my_xlab, 
       ylab = my_ylab)
  
  # Plot grid
  # grid(NULL,NULL, lty = 2, col = "cornsilk2")
  grid(NULL,NULL, lty = 2, col = "grey80")
  
  # Define the mapping between unique values of the variable to colours and shapes.
  variable_values <- factor(sort(as.character(unique(my_metadata[[variable_of_interest]]))))
  variable_colours <- setNames(my_colour_pallete_15[1:length(variable_values)], variable_values)
  #variable_shapes <- setNames(c(1,2,3,4,5,6,8,21,22,23,24,25)[1:length(variable_values)],variable_values)
  variable_shapes <- setNames(c(25,24,23,22,21,8,6,5,4,3,2,1)[1:length(variable_values)],variable_values)
  
  pcoa_site_scores <- scores(m.pcoa, display = "sites")
  
  all_sample_colours <- as.character(
    lapply(
      as.character(
        my_metadata[rownames(pcoa_site_scores),variable_of_interest]
      ), 
      function(x) variable_colours[x]
    )
  )
  
  all_sample_shapes <- as.numeric(
    lapply(
      as.character(
        my_metadata[rownames(pcoa_site_scores),variable_of_interest]
      ), 
      function(x) variable_shapes[x]
    )
  )
  # Plot the points for the samples
  points(m.pcoa, 
         cex = 0.8,
         pch = all_sample_shapes,
         col = all_sample_colours, 
         bg = all_sample_colours,
  )
  
  if (plot_sample_labels == TRUE){
    text(m.pcoa, 
         cex = 0.3)
  }
  
  if (include_ellipses == TRUE){
    for (member in variable_values){
      if (nrow(my_metadata[my_metadata[[variable_of_interest]] == member,]) > 2){ # if too few samples, skip plotting ellipse
        ordiellipse(m.pcoa,
                    groups = my_metadata[[variable_of_interest]],
                    kind = "ehull",
                    col = variable_colours[member][[1]],
                    show.groups = member)
      }
    }
  }
  
  # plot the legend
  legend(
    title = legend_title,
    x = -2.3,
    y =2, 
    legend= variable_values, 
    pch= unique(all_sample_shapes), 
    col= unique(all_sample_colours),
    pt.bg = unique(all_sample_colours),
    #bg = NA,
    box.col = NA,
    cex = .8
  )
}

generate_pcoa_plot(otu_rare_clr.m, metadata.df, "Sampletype", "Sampletype")

m.pcoa <- capscale(otu_rare_clr.m~Sampletype, data = metadata.df, dist = "bray") # ~1 makes it unconstrained

# 
# 
# # We don't actually need to specify data = "" since the environmental variables are not used in unconstrained ordination
# m.pcoa <- capscale(my_community_data~1, data = my_metadata, dist = "bray") # ~1 makes it unconstrained
# 
# # Get species and site scores from ordination
# pcoa.scores <- scores(m.pcoa, choices=c(1,2,3))
# 
# # Calculate the percentages from the eigenvalues
# pcoa_percentages <- (m.pcoa$CA$eig/sum(m.pcoa$CA$eig)) * 100
# 
# # Axis labels for components
# my_xlab = paste("MDS1 (", round(pcoa_percentages[1],1), "%)", sep = "")
# my_ylab = paste("MDS2 (", round(pcoa_percentages[2],1), "%)", sep = "")
# 
# # Generate plot. see help(par) for information on plotting parameters
# plot(m.pcoa,           # PCOA object
#      type='n',         
#      xlim = c(-2,2), 
#      ylim = c(-2,2), 
#      xlab = my_xlab, 
#      ylab = my_ylab)
# 
# # Plot grid
# # grid(NULL,NULL, lty = 2, col = "cornsilk2")
# grid(NULL,NULL, lty = 2, col = "grey80")


