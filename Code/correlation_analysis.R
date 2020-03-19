# Correlation analysis.
# Determine those taxa that appear to have significant positive / negative correlations with eachother

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()

library(ggraph)
library(tidygraph)



setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
source("code/helper_functions.R")

# Load feature taxonomy map
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)
rownames(otu_taxonomy_map.df) <- otu_taxonomy_map.df$OTU.ID

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)
rownames(metadata.df) <- metadata.df$Index

# Define the variables of interest
discrete_variables <- c("Lesion_type_refined","Gender","Patient", "Cohort", "Length_of_immunosuppression_group_1", "Length_of_immunosuppression_group_2")

# Load count matrices
otu.m <-  as.matrix(read.table("Result_tables/count_tables/OTU_counts.csv", sep =",", header =T, row.names = 1))
genus.m <-  as.matrix(read.table("Result_tables/count_tables/Genus_counts.csv", sep =",", header =T, row.names = 1))

# Load combined data (counts, abundances and metadata)
otu_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/OTU_counts_abundances_and_metadata.csv", header = T)
genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Genus_counts_abundances_and_metadata.csv", header = T)

# Load mixomics results
mixomics_immunosuppressed_genus_Lesion_type_refined_comp1.df <- read.csv("Result_tables/mixomics/immunosuppressed_genus_Lesion_type_refined__comp_1.loadings.csv", header = T)
mixomics_immunosuppressed_genus_Lesion_type_refined_comp2.df <- read.csv("Result_tables/mixomics/immunosuppressed_genus_Lesion_type_refined__comp_2.loadings.csv", header = T)
mixomics_immunosuppressed_genus_Lesion_type_refined.df <- rbind(mixomics_immunosuppressed_genus_Lesion_type_refined_comp1.df,mixomics_immunosuppressed_genus_Lesion_type_refined_comp2.df)
mixomics_immunosuppressed_genus_Lesion_type_refined.df$taxonomy_genus <- NULL
names(mixomics_immunosuppressed_genus_Lesion_type_refined.df)[1] <- "taxonomy_genus"
mixomics_immunosuppressed_genus_Lesion_type_refined.df <- mixomics_immunosuppressed_genus_Lesion_type_refined.df[,c("taxonomy_genus", "GroupContrib", "importance", "abs_importance")]


# Location of all FastSpar results
fastspar_results_base <- "/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/External_results/fastspar/all_results"

genus_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  # paste(phylostring[2],phylostring[3], phylostring[6], sep = ";")
                  # paste(phylostring[3], phylostring[6], sep = ";")
                  paste(phylostring[3], phylostring[5], phylostring[6], sep = ";")
                }))
}

# Read correlation and pvalues 
# generate correlation network
# If otu, change node names to taxonomy

# features that were associated (spls-da) with lesion type, and significant (p <0.05) and correlation > 0.1
genus_test_fastspar_cor.m <- as.matrix(read.table("External_results/fastspar/all_results/immunosuppressed_SCC_genus_correlation.tsv",
                                                          sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))
genus_test_fastspar_pval.m <- as.matrix(read.table("External_results/fastspar/all_results/immunosuppressed_SCC_genus_pvalues.tsv",
                                                  sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))



# Genus , lesion and importance

correlation_network.l <- generate_correlation_network(cor_matrix = genus_test_fastspar_cor.m,
                                                      p_matrix = genus_test_fastspar_pval.m,
                                                      p_value_threshold = 0.01,
                                                      cor_threshold = 0.4,
                                                      node_size = 4,
                                                      node_colour = "grey20",
                                                      node_fill = "grey20",
                                                      label_colour = "black",
                                                      label_size = 3,
                                                      plot_height = 20,
                                                      plot_width = 20,
                                                      edge_width_min = .3,
                                                      edge_width_max = 2,
                                                      network_layout = "kk",
                                                      show_node_label = F,
                                                      relabeller_function = genus_relabeller_function,
                                                      # exclude_to_from_df = edges_to_remove.df,
                                                      # filename="Result_figures/correlation_analysis/networks/test_correlation_graph.pdf",
                                                      myseed = 1, edgetype = "fan",show_p_label = F,file_type = "pdf")

correlation_network.l$network_plot


lesion_type_genus_summary.df <- generate_taxa_summary(genus_data.df, taxa_column = "taxonomy_genus",group_by_columns = c("Cohort", "Lesion_type_refined"))
lesion_type_genus_summary.df

corr_cor <- genus_test_fastspar_cor.m[rownames(genus_test_fastspar_cor.m) %in% unique(lesion_type_genus_summary.df[lesion_type_genus_summary.df$Mean_relative_abundance > 0.01,]$taxonomy_genus),
                                      colnames(genus_test_fastspar_cor.m) %in% unique(lesion_type_genus_summary.df[lesion_type_genus_summary.df$Mean_relative_abundance > 0.01,]$taxonomy_genus)]
corr_pval <- genus_test_fastspar_pval.m[rownames(genus_test_fastspar_pval.m) %in% unique(lesion_type_genus_summary.df[lesion_type_genus_summary.df$Mean_relative_abundance > 0.01,]$taxonomy_genus),
                                        colnames(genus_test_fastspar_pval.m) %in% unique(lesion_type_genus_summary.df[lesion_type_genus_summary.df$Mean_relative_abundance > 0.01,]$taxonomy_genus)]

dim(corr_cor)
dim(corr_pval)
source("Code/helper_functions.R")
plot_corrplot(correlation_matrix = corr_cor,
              p_value_matrix = corr_pval,
              method = "circle",
              label_size = 1,
              p_value_threshold = .05,
              plot_title_size = .6,plot_height = 50, plot_width = 50,
              insig = "blank", insig_pch_col = "grey20",plot_title = "", insig_pch = 4,
              file_type = "pdf",
              make_insig_na = F,
              colour_label_size = 3,
              outline = T,
              grid_colour = "grey",
              filename = "Result_figures/correlation_analysis/corrplots/test_corrplot.pdf")
corrplot()