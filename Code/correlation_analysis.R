# Correlation analysis.
# Determine those taxa that appear to have significant positive / negative correlations with each other
# External correlations and p-values should be calculated with FastSpar
# Networks can be further filtered based on results from sPLS-DA reported associations

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()

library(ggraph)
library(tidygraph)


genus_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  # paste(phylostring[2],phylostring[3], phylostring[6], sep = ";")
                  # paste(phylostring[3], phylostring[6], sep = ";")
                  paste(phylostring[3], phylostring[5], phylostring[6], sep = ";")
                }))
}


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

# Location of all FastSpar results
fastspar_results_base <- "/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/External_results/fastspar/all_results"


# Read correlation and pvalues 
# generate correlation network
# If otu, change node names to taxonomy

# ------------------------------------------------------------------------------------------------------------------------
# immunosuppressed, ssc + scc_pl 

# Load fastspar results
immunosuppressed_scc_sccpl_otu_fastspar_cor.m <- as.matrix(read.table("External_results/fastspar/all_results/immunosuppressed_SCC_SCCPL_otu_correlation.tsv",
                                                  sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))
immunosuppressed_scc_sccpl_otu_fastspar_pval.m <- as.matrix(read.table("External_results/fastspar/all_results/immunosuppressed_SCC_SCCPL_otu_pvalues.tsv",
                                                   sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))

# Load mixomics results
mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined_comp1.df <- read.csv("Result_tables/mixomics/immunosuppressed_scc_sccpl_otu_Lesion_type_refined__comp_1.loadings.csv", header = T)
mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined_comp2.df <- read.csv("Result_tables/mixomics/immunosuppressed_scc_sccpl_otu_Lesion_type_refined__comp_2.loadings.csv", header = T)
mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df <- rbind(mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined_comp1.df,
                                                                        mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined_comp2.df)
rownames(mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df) <- mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df[,1]

# mixomics_immunosuppressed_genus_Lesion_type_refined.df$taxonomy_genus <- NULL
# names(mixomics_immunosuppressed_genus_Lesion_type_refined.df)[1] <- "taxonomy_genus"
# mixomics_immunosuppressed_genus_Lesion_type_refined.df <- mixomics_immunosuppressed_genus_Lesion_type_refined.df[,c("taxonomy_genus", "GroupContrib", "importance", "abs_importance")]
feature_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  paste0(x, ":\n", species_relabeller_function(as.character(otu_taxonomy_map.df[x,]$taxonomy_species)))
                  # species_relabeller_function(as.character(otu_taxonomy_map.df[x,]$taxonomy_species))
                }))
}


cor_filtered.m <- immunosuppressed_scc_sccpl_otu_fastspar_cor.m[mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df$OTU.ID,
                                                               mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df$OTU.ID]
pval_filtered.m <- immunosuppressed_scc_sccpl_otu_fastspar_pval.m[mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df$OTU.ID,
                                                                  mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df$OTU.ID]

# FIXME 
# Create graphs with node shapes matching taxa.
# Size of node should be importance values from splsDA.
# Node colour should be associated lesion (SCC / SCC_PL)
# line thickness should be p-values?
# Remove entries not associated with anything according to splsda
# FIXME handle nodes with the same name. Need unique, though option to rename in plot!


generate_correlation_network2 <- function(myedges.df,
                                          mynodes.df, 
                                          p_value_threshold = 0.05, cor_threshold = 0.5){
  
  
}

# Create edge dataframe from correlation matrix
edges.df <- melt(cor_filtered.m, value.name = "Correlation",varnames = c("from", "to"))

# Add p-values
edges.df$P_value <- melt(pval_filtered.m)$value

# Filter edges entries by the Correlation / p-value
edges.df <- edges.df[abs(edges.df$Correlation) >= 0.0,]
edges.df <- edges.df[edges.df$P_value <= 0.1,]

# Create node dataframe from entries in edges
nodes.v <- unique(c(as.character(edges.df[,'from']), as.character(edges.df[,'to'])))
nodes.df <- data.frame(row.names = nodes.v)
nodes.df[,'name'] <- nodes.v
nodes.df[,'Association'] <- mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df[nodes.v,'GroupContrib']
nodes.df[,'Importance'] <- abs(mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df[nodes.v,'importance'])
nodes.df[,'taxonomy_genus'] <- unlist(lapply(nodes.df$name, function(x) as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,]$taxonomy_genus)))
nodes.df[,'Family'] <- unlist(lapply(nodes.df$name, function(x) as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,]$Family)))
nodes.df[,'Genus'] <- unlist(lapply(nodes.df$name, function(x) as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,]$Genus)))
nodes.df[,'Species'] <- unlist(lapply(nodes.df$name, function(x) as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,]$Species)))
nodes.df[,'Taxa_label'] <- with(nodes.df, paste0(Family,";", Genus, ";", Species))

# Filter nodes by column values
nodes.df <- nodes.df[complete.cases(nodes.df),]

# Filter edges to match
edges.df <- edges.df[unique(which(edges.df$from %in% nodes.df$name & edges.df$to %in% nodes.df$name)),]

# Remove bidirectional links
edges.df$ordered_label <- apply(edges.df, 1, function(x) {paste0(sort(c(as.character(x[1]), as.character(x[2]))),collapse = ":")})
edges.df <- edges.df[!duplicated(edges.df$ordered_label),]

# geom_edge_link(aes(colour = cor, width=-log10(pvalue)), alpha=0.7) +
  # geom_node_point(mapping = aes(colour=Association, size=Importance, shape=Genus)) +
  # geom_node_text(aes(label=paste0(Node, Genus2)), repel = T)

# Create graph from node and edge dataframes
mygraph <- tbl_graph(nodes = nodes.df, edges = edges.df,directed = F)
mygraph <- mygraph %>%
  # Remove loops
  activate(edges) %>%
  filter(!edge_is_loop()) %>%
  # Remove isolated nodes
  activate(nodes) %>%
  filter(!node_is_isolated())

ggraph(mygraph, layout = "kk") +
  geom_edge_fan(aes(colour = Correlation, width = -log10(P_value)), alpha=0.7) +
  geom_node_point(mapping = aes(colour=Association, size=Importance, shape = Genus)) +
  geom_node_text(aes(label=paste0(name,"\n", Genus)), repel = T) +
  scale_edge_color_continuous(name="Correlation", low = "blue", high = "green", limits = c(-1,1)) +
  scale_size_continuous(range = c(3,10))+ 
  scale_edge_width_continuous(name="-log10(Pval)", range = c(0.5,2)) +
  guides(byrow=F, ncol=2, 
         colour=guide_legend(override.aes=list(size=8)), 
         shape=guide_legend(override.aes=list(size=8))) +
  theme(legend.position = 'bottom', legend.box = 'horizontal', legend.direction = 'vertical',
        text=element_text(size=16), 
        plot.title = element_text(hjust = 0.5)
  )
 
geom_edge_fan(aes(colour = Correlation, width=abs(Correlation)), show.legend = T, alpha = 1,
              angle_calc = "along", label_colour = "black",
              label_dodge=unit(1,"mm"),label_push=unit(-1,"mm"))


head(graph.df)
head(flare$edges)
head(flare$vertices)
head(highschool)

temp <- graph_from_data_frame(graph.df,vertices = nodes.df,directed = F)
temp <- as_tbl_graph(graph.df)
temp
ggraph(temp, layout = 'fr') +
  geom_edge_link(aes(colour = Correlation), alpha=0.7) +
  geom_node_point(aes(colour=Association, size=Importance))
f


library(igraph)
temp <- graph_from_data_frame(graph.df,vertices = nodes.df)
ggraph(temp) + 
  geom_edge_link(aes(colour = Correlation),  show.legend = T, alpha = 1,
                 angle_calc = "along", label_colour = "black",
                 label_dodge=unit(1,"mm"),label_push=unit(-1,"mm")) +
  geom_node_point()


as_tbl_graph(nodes.df, edges = graph.df)
# names(graph.df) <- c("Variable_1", "Variable_2", "Correlation")
graph.df <- as_tbl_graph(graph.df, nodes = nodes.df) %>%
  # Remove loops
  activate(edges) %>%
  filter(!edge_is_loop()) %>%
  # Remove isolated nodes
  activate(nodes) %>%
  filter(!node_is_isolated())
graph.df %>% activate(nodes) %>% as.data.frame()

ggraph(graph.df) + 
  geom_edge_link(aes(colour = Correlation, shape = Association),  show.legend = T, alpha = 1,
                 angle_calc = "along", label_colour = "black",
                 label_dodge=unit(1,"mm"),label_push=unit(-1,"mm")) +
  geom_node_point()

rownames(mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df) <- 

head(graph.df)



correlation_network.l <- generate_correlation_network(cor_matrix = cor_filtered.m,
                                                      p_matrix = pval_filtered.m,
                                                      p_value_threshold = 0.05,
                                                      cor_threshold = 0.1,
                                                      node_size = 4,
                                                      node_colour = "grey20",
                                                      node_fill = "grey20",
                                                      label_colour = "black",
                                                      label_size = 3,
                                                      plot_height = 15,
                                                      plot_width = 15,
                                                      edge_width_min = .3,
                                                      edge_width_max = 2,
                                                      network_layout = "kk",
                                                      show_node_label = T,
                                                      relabeller_function = feature_relabeller_function,
                                                      # exclude_to_from_df = edges_to_remove.df,
                                                      filename="Result_figures/correlation_analysis/networks/test_correlation_graph.pdf",
                                                      myseed = 1, edgetype = "fan",show_p_label = F,file_type = "pdf")


plot_corrplot(correlation_matrix = cor_filtered.m,
              p_value_matrix = pval_filtered.m,
              method = "circle",
              label_size = 1,
              p_value_threshold = .05,
              plot_title_size = .6,plot_height = 25, plot_width = 25,
              insig = "blank", insig_pch_col = "grey20",plot_title = "", insig_pch = 4,
              file_type = "pdf",
              make_insig_na = F,
              colour_label_size = 3,
              outline = T,
              grid_colour = "grey",
              relabeller_function = feature_relabeller_function,
              filename = "Result_figures/correlation_analysis/corrplots/test_corrplot.pdf")
# ------------------------------------------------------------------------------------------------------------------------















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