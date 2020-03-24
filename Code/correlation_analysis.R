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
library(svglite)

genus_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  # paste(phylostring[2],phylostring[3], phylostring[6], sep = ";")
                  # paste(phylostring[3], phylostring[6], sep = ";")
                  paste(phylostring[3], phylostring[5], phylostring[6], sep = ";")
                }))
}

species_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  paste(phylostring[3],phylostring[5],phylostring[6], phylostring[7], sep = ";")
                  # paste(phylostring[3], phylostring[7], sep = ";")
                }))
}

feature_relabeller_function <- function(my_labels, include_feature_ID = F){
  unlist(lapply(my_labels,
                function(x) {
                  phylostring <- unlist(strsplit(as.character(otu_taxonomy_map.df[x,]$taxonomy_species),split = ";"))
                  if (include_feature_ID == T){
                    paste0(x, ":\n", paste(phylostring[3],phylostring[6], phylostring[7], sep = ";"))
                  } else{
                    paste(phylostring[3],phylostring[6], phylostring[7], sep = ";")
                  }
                  
                  
                }))
}


# Create graphs with node shapes matching taxa.
# Size of node should be importance values from splsDA.
# Node colour should be associated lesion (SCC / SCC_PL)
# line thickness should be p-values
# Remove entries not associated with anything according to splsda
# Should be noted that nodes with the same name will overlap, can be a useful
#   way to show whether multiple features with the same name (higher collapsed taxa level)
#   have different (+/-) correlations
# FIXME handle nodes with the same name. Need unique, though option to rename in plot!

generate_correlation_network2 <- function(myedges.df,
                                          mynodes.df, 
                                          node_label_column,
                                          node_shape_column,
                                          node_size_column,
                                          node_colour_column,
                                          edge_colour_column,
                                          edge_width_column,
                                          edge_width_name = "-log10(Pval)",
                                          label_size = 1,
                                          plot_height = 10,
                                          plot_width = 10,
                                          variable_colours_available = T,
                                          network_layout = "fr",
                                          filename = NULL,
                                          file_type = "pdf"){
  # myedges.df - "from" "to" and relavent edge metadata
  # mynodes.df - "name" (of node) and relavent node metadata
  # 
  # myedges.df = network_data$edges.df
  # mynodes.df = network_data$nodes.df
  # node_label_column = "name"
  # node_shape_column = "Association"
  # node_size_column = "Importance"
  # node_colour_column = "Association"
  # edge_colour_column = "Correlation"
  # edge_width_column = "P_value"
  # filename = "Result_figures/correlation_analysis/networks/test_correlation_graph2.pdf"
  # file_type = "pdf"
  # plot_height = 7
  # plot_width = 7
  
  internal_nodes.df <- mynodes.df
  internal_edges.df <- myedges.df
  
  # Filter nodes by column values
  internal_nodes.df <- internal_nodes.df[complete.cases(internal_nodes.df),]
  
  # Filter edges to match
  internal_edges.df <- internal_edges.df[unique(which(internal_edges.df$from %in% internal_nodes.df$name & 
                                                        internal_edges.df$to %in% internal_nodes.df$name)),]
  
  # Remove bidirectional links
  internal_edges.df$ordered_label <- apply(internal_edges.df, 1, function(x) {paste0(sort(c(as.character(x[1]), as.character(x[2]))),collapse = ":")})
  internal_edges.df <- internal_edges.df[!duplicated(internal_edges.df$ordered_label),]
  
  # Create graph from node and edge dataframes
  mygraph <- tbl_graph(nodes = internal_nodes.df, edges = internal_edges.df,directed = F)
  mygraph <- mygraph %>%
    # Remove loops
    activate(edges) %>%
    filter(!edge_is_loop()) %>%
    # Remove isolated nodes
    activate(nodes) %>%
    filter(!node_is_isolated())
  
  if (!is.null(node_shape_column)){
    # node_shapes <- setNames(rep(c(25,24,23,22,21),
    #                             length(unique(internal_nodes.df[,node_shape_column])))[1:length(unique(internal_nodes.df[,node_shape_column]))],
    #                         unique(internal_nodes.df[,node_shape_column]))
    node_shapes <- setNames(rep(c(19,18,17,16,15),
                                length(unique(internal_nodes.df[,node_shape_column])))[1:length(unique(internal_nodes.df[,node_shape_column]))],
                            unique(internal_nodes.df[,node_shape_column]))
  } else{
    node_shapes <- setNames(rep(c(21),length(unique(internal_nodes.df[,node_shape_column])))[1:length(unique(internal_nodes.df[,node_shape_column]))],unique(internal_nodes.df[,node_shape_column]))  
  }

  my_palette <- colorRampPalette(c("#17468a","#ffdd47","#99113a"))
  
  # node_values <- levels(mydata.df[[variable_column]])
  if (variable_colours_available == T){
    color_col_name <- paste0(node_colour_column, "_colour")
    node_colours <- setNames(as.character(unique(internal_nodes.df[[color_col_name]])), as.character(unique(internal_nodes.df[[node_colour_column]])))
  } else{
    # my_colour_palette_30_distinct
    internal_colour_palette <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4",
                                   "#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8",
                                   "#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d",
                                   "#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a",
                                   "#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
    node_colours <- setNames(internal_colour_palette[1:length(as.character(unique(internal_nodes.df[[node_colour_column]])))], as.character(unique(internal_nodes.df[[node_colour_column]])))  
  }
  set_graph_style(family = "Arial Narrow", face = "plain", size = 11,
                  text_size = 11, text_colour = "black")
  correlation_graph_plot <- ggraph(mygraph, layout = network_layout) +
    geom_edge_fan(aes(colour = get(edge_colour_column), width = get(edge_width_column)), alpha=1) +
    geom_node_point(mapping = aes(fill=get(node_colour_column),
                                  size= get(node_size_column), 
                                  shape = get(node_shape_column), 
                                  colour = get(node_colour_column))) +
    geom_node_text(aes(label=get(node_label_column)),
                   size = label_size,
                   nudge_y = -.01, #fontface = "bold",
                   point.padding = unit(0.3, "lines"),
                   segment.size = 0.3,
                   segment.colour = "grey",
                   repel = T) +
    scale_edge_color_gradientn(colors = my_palette(20),
                               breaks = seq(-1,1,.25),
                               limits = c(-1,1), 
                               name = edge_colour_column,
                               guide = guide_edge_colourbar(barheight = 8)) +
    scale_edge_width_continuous(range = c(0.5,2),
                                name= edge_width_name) +
    scale_size_continuous(range = c(3,10), name = node_size_column) + 
    scale_fill_manual(values = node_colours, name = node_colour_column) + 
    scale_colour_manual(values = node_colours, name = node_colour_column) +
    scale_shape_manual(values = node_shapes, name = node_shape_column) +
    guides(byrow=F, ncol=2, 
           colour=guide_legend(override.aes=list(size=6)), 
           shape=guide_legend(override.aes=list(size=6))) +
    theme(legend.position = 'bottom', 
          legend.box = 'horizontal', 
          legend.direction = 'vertical',
          legend.title = element_text(face = "bold"),
          
          # text=element_text(size=10), 
          plot.title = element_text(hjust = 0.5)
    )
  
  # Save plot to file
  if (!is.null(filename)){
    if (file_type == "pdf"){
      cairo_pdf(filename = filename,height = plot_height, width = plot_width)
      plot(correlation_graph_plot)
      dev.off()      
    } else if (file_type == "svg"){
      # Cairo::CairoSVG(file = filename,width = plot_width,height = plot_height)
      # svg(filename = filename,height = plot_height, width = plot_width)
      svglite(file = filename,height = plot_height, width = plot_width)
      plot(correlation_graph_plot)
      dev.off()
    }
  }
  correlation_graph_plot
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

# ------------------------------------------------------------------------------------------------------------------------
# immunosuppressed, ssc + scc_pl 

# Load fastspar results
immunosuppressed_scc_sccpl_otu_fastspar_cor.m <- as.matrix(read.table("External_results/fastspar/all_results/immunosuppressed_SCC_SCCPL_otu_correlation.tsv",
                                                  sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))
immunosuppressed_scc_sccpl_otu_fastspar_pval.m <- as.matrix(read.table("External_results/fastspar/all_results/immunosuppressed_SCC_SCCPL_otu_pvalues.tsv",
                                                   sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))
# -------------------
immunocompetent_scc_sccpl_otu_fastspar_cor.m <- as.matrix(read.table("External_results/fastspar/all_results/immunocompetent_SCC_SCCPL_otu_correlation.tsv",
                                                                      sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))
immunocompetent_scc_sccpl_otu_fastspar_pval.m <- as.matrix(read.table("External_results/fastspar/all_results/immunocompetent_SCC_SCCPL_otu_pvalues.tsv",
                                                                       sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))

# -------------------
immunosuppressed_scc_sccpl_genus_fastspar_cor.m <- as.matrix(read.table("External_results/fastspar/all_results/immunosuppressed_SCC_SCCPL_genus_correlation.tsv",
                                                                     sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))

immunosuppressed_scc_sccpl_genus_fastspar_pval.m <- as.matrix(read.table("External_results/fastspar/all_results/immunosuppressed_SCC_SCCPL_genus_pvalues.tsv",
                                                                      sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))

# -------------------

immunocompetent_scc_sccpl_genus_fastspar_cor.m <- as.matrix(read.table("External_results/fastspar/all_results/immunocompetent_SCC_SCCPL_genus_correlation.tsv",
                                                                       sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))

immunocompetent_scc_sccpl_genus_fastspar_pval.m <- as.matrix(read.table("External_results/fastspar/all_results/immunocompetent_SCC_SCCPL_genus_pvalues.tsv",
                                                                        sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))


# ------------------------------------------------
# Load mixomics results
mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined_comp1.df <- read.csv("Result_tables/mixomics/immunosuppressed_scc_sccpl_otu_Lesion_type_refined__comp_1.loadings.csv", header = T)
mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined_comp2.df <- read.csv("Result_tables/mixomics/immunosuppressed_scc_sccpl_otu_Lesion_type_refined__comp_2.loadings.csv", header = T)
mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df <- rbind(mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined_comp1.df,
                                                                        mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined_comp2.df)
# -------------------
mixomics_immunocompetent_scc_sccpl_otu_Lesion_type_refined_comp1.df <- read.csv("Result_tables/mixomics/immunocompetent_scc_sccpl_otu_Lesion_type_refined__comp_1.loadings.csv", header = T)
mixomics_immunocompetent_scc_sccpl_otu_Lesion_type_refined_comp2.df <- read.csv("Result_tables/mixomics/immunocompetent_scc_sccpl_otu_Lesion_type_refined__comp_2.loadings.csv", header = T)
mixomics_immunocompetent_scc_sccpl_otu_Lesion_type_refined.df <- rbind(mixomics_immunocompetent_scc_sccpl_otu_Lesion_type_refined_comp1.df,
                                                                       mixomics_immunocompetent_scc_sccpl_otu_Lesion_type_refined_comp2.df)

# -------------------
mixomics_immunosuppressed_scc_sccpl_genus_Lesion_type_refined_comp1.df <- read.csv("Result_tables/mixomics/immunosuppressed_scc_sccpl_genus_Lesion_type_refined__comp_1.loadings.csv", header = T)
mixomics_immunosuppressed_scc_sccpl_genus_Lesion_type_refined_comp2.df <- read.csv("Result_tables/mixomics/immunosuppressed_scc_sccpl_genus_Lesion_type_refined__comp_2.loadings.csv", header = T)
mixomics_immunosuppressed_scc_sccpl_genus_Lesion_type_refined.df <- rbind(mixomics_immunosuppressed_scc_sccpl_genus_Lesion_type_refined_comp1.df,
                                                                        mixomics_immunosuppressed_scc_sccpl_genus_Lesion_type_refined_comp2.df)
# -------------------
mixomics_immunocompetent_scc_sccpl_genus_Lesion_type_refined_comp1.df <- read.csv("Result_tables/mixomics/immunocompetent_scc_sccpl_genus_Lesion_type_refined__comp_1.loadings.csv", header = T)
mixomics_immunocompetent_scc_sccpl_genus_Lesion_type_refined_comp2.df <- read.csv("Result_tables/mixomics/immunocompetent_scc_sccpl_genus_Lesion_type_refined__comp_2.loadings.csv", header = T)
mixomics_immunocompetent_scc_sccpl_genus_Lesion_type_refined.df <- rbind(mixomics_immunocompetent_scc_sccpl_genus_Lesion_type_refined_comp1.df,
                                                                         mixomics_immunocompetent_scc_sccpl_genus_Lesion_type_refined_comp2.df)

# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
generate_lesion_network_dataframes <- function(mixomics_results,
                                                       correlations,
                                                       pvalues,
                                                       cor_filter = 0.1,
                                                       pvalue_filter = 0.05){
  edges.df <- melt(correlations)
  temp <- melt(pvalues)
  edges.df <- cbind(edges.df, temp$value)
  names(edges.df) <- c("from", "to", "Correlation", "Pvalue")
  edges.df$Pvalue_logged <- -log10(edges.df$Pvalue)
  edges.df <- edges.df[abs(edges.df$Correlation) >= cor_filter,]
  edges.df <- edges.df[edges.df$Pvalue <= pvalue_filter,]
  edges.df <- edges.df[edges.df$from %in% mixomics_results$OTU.ID,]
  edges.df <- edges.df[edges.df$to %in% mixomics_results$OTU.ID,]
  
  edges.df$ordered_label <- apply(edges.df, 1, function(x) {paste0(sort(c(as.character(x[1]), as.character(x[2]))),collapse = ":")})
  edges.df <- edges.df[!duplicated(edges.df$ordered_label),]
  
  internal_mixomics_results <- 
    mixomics_results %>% 
    dplyr::group_by(OTU.ID) %>%
    dplyr::arrange(dplyr::desc(abs_importance)) %>%
    dplyr::top_n(1, abs_importance) %>% as.data.frame()
  rownames(internal_mixomics_results) <- internal_mixomics_results$OTU.ID
  
  nodes.v <- unique(c(as.character(edges.df$from), as.character(edges.df$to)))
  nodes.df <- data.frame(row.names = nodes.v)
  nodes.df[,'name'] <- nodes.v
  nodes.df[,'Importance'] <- abs(internal_mixomics_results[nodes.v,'importance'])
  nodes.df[,"Association"] <- as.character(internal_mixomics_results[nodes.v,"GroupContrib"])
  nodes.df[,'Association_colour'] <- unlist(lapply(as.character(nodes.df$Association), 
                                                   function(x) as.character(metadata.df[metadata.df$Lesion_type_refined == x,]$Lesion_type_refined_colour[1])))
  
  nodes.df$Association <- factor(nodes.df$Association, levels = c("C", "C_P", "AK", "SCC_PL", "SCC")[c("C", "C_P", "AK", "SCC_PL", "SCC") %in% nodes.df$Association])
  
  return(list(nodes.df = nodes.df, edges.df = edges.df))
}


network_data <- generate_lesion_network_dataframes(mixomics_results = mixomics_immunosuppressed_scc_sccpl_genus_Lesion_type_refined.df,
                                                   correlations = immunosuppressed_scc_sccpl_genus_fastspar_cor.m,
                                                   pvalues = immunosuppressed_scc_sccpl_genus_fastspar_pval.m,
                                                   cor_filter = 0.1,
                                                   pvalue_filter = 0.05)
network_data$nodes.df$label <- genus_relabeller_function(network_data$nodes.df$name)
set.seed(1234)
myplot <- generate_correlation_network2(
  myedges.df = network_data$edges.df,
  mynodes.df = network_data$nodes.df,
  variable_colours_available = T,
  node_label_column = "label",
  node_shape_column = "Association",
  node_size_column = "Importance",
  node_colour_column = "Association",
  edge_colour_column = "Correlation",
  edge_width_column = "Pvalue_logged",
  label_size = 3,
  filename = "Result_figures/correlation_analysis/networks/immunosuppressed_scc_sccpl_genus_mixomics_filtered_correlation_graph.pdf",
  file_type = "pdf",
  plot_height = 10,
  plot_width = 10)

# ------------------------------------------------
network_data <- generate_lesion_network_dataframes(mixomics_results = mixomics_immunosuppressed_scc_sccpl_otu_Lesion_type_refined.df,
                                                   correlations = immunosuppressed_scc_sccpl_otu_fastspar_cor.m,
                                                   pvalues = immunosuppressed_scc_sccpl_otu_fastspar_pval.m,
                                                   cor_filter = 0.1,
                                                   pvalue_filter = 0.05)

network_data$nodes.df$label <- feature_relabeller_function(network_data$nodes.df$name,include_feature_ID = F)
# unlist(lapply(network_data$nodes.df$name, function(x) as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,"taxonomy_species"])))

set.seed(1234)
myplot <- generate_correlation_network2(
  myedges.df = network_data$edges.df,
  mynodes.df = network_data$nodes.df,
  variable_colours_available = T,
  node_label_column = "label",
  node_shape_column = "Association",
  node_size_column = "Importance",
  node_colour_column = "Association",
  edge_colour_column = "Correlation",
  edge_width_column = "Pvalue_logged",
  filename = "Result_figures/correlation_analysis/networks/immunosuppressed_scc_sccpl_otu_mixomics_filtered_correlation_graph.pdf",
  file_type = "pdf",
  label_size = 3,
  plot_height = 10,
  plot_width = 10)

# ------------------------------------------------

network_data <- generate_lesion_network_dataframes(mixomics_results = mixomics_immunocompetent_scc_sccpl_genus_Lesion_type_refined.df,
                                                   correlations = immunocompetent_scc_sccpl_genus_fastspar_cor.m,
                                                   pvalues = immunocompetent_scc_sccpl_genus_fastspar_pval.m,
                                                   cor_filter = 0.1,
                                                   pvalue_filter = 0.01)

network_data$nodes.df$label <- genus_relabeller_function(network_data$nodes.df$name)

set.seed(1234)
myplot <- generate_correlation_network2(
  myedges.df = network_data$edges.df,
  mynodes.df = network_data$nodes.df,
  variable_colours_available = T,
  node_label_column = "label",
  node_shape_column = "Association",
  node_size_column = "Importance",
  node_colour_column = "Association",
  edge_colour_column = "Correlation",
  edge_width_column = "Pvalue_logged",
  filename = "Result_figures/correlation_analysis/networks/immunocompetent_scc_sccpl_genus_mixomics_filtered_correlation_graph.pdf",
  file_type = "pdf",
  network_layout = "stress",
  label_size = 3,
  plot_height = 15,
  plot_width = 15)

# ------------------------------------------------
network_data <- generate_lesion_network_dataframes(mixomics_results = mixomics_immunocompetent_scc_sccpl_otu_Lesion_type_refined.df,
                                                   correlations = immunocompetent_scc_sccpl_otu_fastspar_cor.m,
                                                   pvalues = immunocompetent_scc_sccpl_otu_fastspar_pval.m,
                                                   cor_filter = 0.1,
                                                   pvalue_filter = 0.05)

network_data$nodes.df$label <- feature_relabeller_function(network_data$nodes.df$name,include_feature_ID = F)
# unlist(lapply(network_data$nodes.df$name, function(x) as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,"taxonomy_species"])))

set.seed(1234)
myplot <- generate_correlation_network2(
  myedges.df = network_data$edges.df,
  mynodes.df = network_data$nodes.df,
  variable_colours_available = T,
  node_label_column = "label",
  node_shape_column = "Association",
  node_size_column = "Importance",
  node_colour_column = "Association",
  edge_colour_column = "Correlation",
  edge_width_column = "Pvalue_logged",
  filename = "Result_figures/correlation_analysis/networks/immunocompetent_scc_sccpl_otu_mixomics_filtered_correlation_graph.pdf",
  file_type = "pdf",
  label_size = 3,
  plot_height = 10,
  plot_width = 10)

# ------------------------------------------------


# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------






# features that were associated (spls-da) with lesion type, and significant (p <0.05) and correlation > 0.1
genus_test_fastspar_cor.m <- as.matrix(read.table("External_results/fastspar/all_results/immunosuppressed_SCC_genus_correlation.tsv",
                                                          sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))
genus_test_fastspar_pval.m <- as.matrix(read.table("External_results/fastspar/all_results/immunosuppressed_SCC_genus_pvalues.tsv",
                                                  sep ="\t",header = T,row.names = 1,comment.char = "", check.names = F))





lesion_type_genus_summary.df <- generate_taxa_summary(genus_data.df, taxa_column = "taxonomy_genus",group_by_columns = c("Cohort", "Lesion_type_refined"))


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
              plot_title_size = .6,plot_height = 25, plot_width = 25,
              insig = "blank", insig_pch_col = "grey20",plot_title = "", insig_pch = 4,
              file_type = "pdf",
              make_insig_na = F,
              colour_label_size = 3,
              outline = T,
              grid_colour = "grey",
              filename = "Result_figures/correlation_analysis/corrplots/test_corrplot.pdf")
