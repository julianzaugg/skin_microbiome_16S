# Script to generate the mean relative abundance figure for publication for negative controls
# This is based on abundance values for negative samples from both cohorts prior to contaminant removal 
# but after low abundance feature removal

# invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()

library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(cowplot)



# ------------------------------------------------------------------------------------------
# Various colour palettes
my_colour_palette_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_palette_12_soft <-c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621", "#c75a93", "#7c7731")

# ------------------------------------------------------------------------------------------
### Calculate the (min, max, mean, median, stdev, #samples) abundances of each taxa at each taxa level


source("Code/helper_functions.R")


# ------------------------------------------------------------------------------------------
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")

negative_genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Negative_samples_Genus_counts_abundances_and_metadata.csv")


negative_genus_data.df$taxonomy_label <- with(negative_genus_data.df, paste0(Domain,";", Class,";", Genus))

negative_taxa_summary.df <- generate_taxa_summary(mydata = negative_genus_data.df, taxa_column = "taxonomy_label", group_by_columns = c("Sample", "Lesion_type_refined"))

# Identify the top genus for each sample
negative_top_taxa_summary.df <- filter_summary_to_top_n(taxa_summary = negative_taxa_summary.df, 
                                                     grouping_variables = c("Sample"),
                                                     abundance_column = "Mean_relative_abundance",
                                                     my_top_n = 6)

# Create palette based on unique set
genus_palette <- setNames(my_colour_palette_206_distinct[1:length(unique(negative_top_taxa_summary.df$taxonomy_label))], unique(negative_top_taxa_summary.df$taxonomy_label))
genus_palette["Other"] <- "grey"

# Take the full table and re-label any taxa not in the top to "Other"
negative_taxa_summary.df[!negative_taxa_summary.df$taxonomy_label %in% negative_top_taxa_summary.df$taxonomy_label,]$taxonomy_label <- "Other"

# Normalise the Mean_relative_abundance_rarefied values within each lesion
# negative_taxa_summary.df <- negative_taxa_summary.df %>% group_by(Lesion_type_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()

# Need a single entry for the Other group
negative_taxa_summary.df <- negative_taxa_summary.df %>% group_by(Sample,Lesion_type_refined, taxonomy_label) %>% dplyr::summarise(Relative_abundance = sum(Summed_relative_abundance)) %>% as.data.frame()

negative_taxa_summary.df %>% group_by(Sample) %>% summarise(total = sum(Relative_abundance)) %>% as.data.frame()

negative_taxa_summary.df$value_label <- as.character(lapply(negative_taxa_summary.df$Relative_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), "")))

negative_just_legend_plot <- ggplot(negative_taxa_summary.df, aes(x = Sample, y = Relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 5)) +
  xlab("Sample") +
  ylab("Relative abundance") +
  common_theme

negative_abundance_plot <- ggplot(negative_taxa_summary.df, aes(x = Sample, y = Relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 2,color = "grey10") +
  coord_flip() +
  scale_fill_manual(values = genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample") +
  ylab("Relative abundance (%)") +
  common_theme



# ---------------------------------------------
negative_genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Negative_samples_Genus_counts_abundances_and_metadata.csv")
negative_genus_data.df$taxonomy_label <- with(negative_genus_data.df, paste0(Domain,";", Class,";", Genus))

negative_taxa_summary.df <- generate_taxa_summary(mydata = negative_genus_data.df, taxa_column = "taxonomy_label", group_by_columns = c("Sample", "Lesion_type_refined"))

negative_genus_data_filtered.df <- filter_summary_to_top_n(taxa_summary = negative_taxa_summary.df, 
                                                          grouping_variables = c("Sample"),
                                                          abundance_column = "Mean_relative_abundance",
                                                          my_top_n = 10)

heatmap.m <- negative_taxa_summary.df[c("Sample", "taxonomy_label","Mean_relative_abundance")]
heatmap.m <- heatmap.m[heatmap.m$taxonomy_label %in% negative_genus_data_filtered.df$taxonomy_label,]
heatmap.m <- heatmap.m %>% spread(Sample, Mean_relative_abundance,fill = 0)
heatmap.m <- df2matrix(heatmap.m)

names(negative_genus_data.df)
heatmap_metadata.df <- unique(negative_genus_data.df[c("Sample", "Lesion_type_refined","Patient","Job.ID", "Cohort","Length_of_immunosuppression_group_1", "Length_of_immunosuppression_group_2","Date_sampled","Transplant", grep("colour", names(negative_genus_data.df), value =T))])
rownames(heatmap_metadata.df) <- heatmap_metadata.df$Sample

make_heatmap(heatmap.m*100, 
             mymetadata = heatmap_metadata.df,
             filename = paste0("Result_figures/heatmaps/Negative_sample_genus_top_10_mean_relative_abundance_heatmap.pdf"),
             variables = c("Cohort","Job.ID","Lesion_type_refined","Patient","Transplant", "Length_of_immunosuppression_group_1", "Length_of_immunosuppression_group_2"),
             column_title = "",
             row_title = "Genus",
             plot_height = 10,
             plot_width = 12,
             cluster_columns = F,
             cluster_rows = T,
             column_title_size = 10,
             row_title_size = 10,
             # annotation_name_size = 8,
             my_annotation_palette = my_colour_palette_15,
             legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, "> 60"),
             my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
             legend_title = "Relative abundance %",
             discrete_legend = T,
             palette_choice = 'purple',
             show_row_dend = F,
             row_dend_width = unit(25, "cm")
)


library(vegan)
heatmap.m <- negative_taxa_summary.df[c("Sample", "taxonomy_label","Max_read_count")]
# heatmap.m <- heatmap.m[heatmap.m$taxonomy_label %in% negative_genus_data_filtered.df$taxonomy_label,]
heatmap.m <- heatmap.m %>% spread(Sample, Max_read_count,fill = 0)

heatmap_clr.m <- clr(df2matrix(heatmap.m))
heatmap_clr.m[which(heatmap_clr.m < 0)] <- 0

temp <- rda(t(heatmap_clr.m), data = heatmap_metadata.df) # ~1 makes it unconstrained

my_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  # paste(phylostring[2],phylostring[3], phylostring[6], sep = ";")
                  paste(phylostring[3], phylostring[6], sep = ";")
                }))
}
heatmap_metadata.df$Cohort_Job.ID <- with(heatmap_metadata.df, paste0(Cohort, "__",Job.ID))

generate_pca(temp, mymetadata = heatmap_metadata.df,
             plot_height = 6, plot_width = 6,
             legend_x = 3, legend_y = -2,
             point_size = .7, point_line_thickness = 0.3,point_alpha =1,
             legend_title = "",
             legend_cex = .5,
             plot_title = "",
             # limits = c(-9,10,-4,4),
             plot_spiders = F,
             plot_ellipses = F,
             plot_hulls = T,
             use_shapes = T,
             ellipse_border_width = .5,
             include_legend = T,
             label_ellipse = F, ellipse_label_size = .5,
             colour_palette = my_colour_palette_15,
             variable_to_plot = "Cohort_Job.ID", legend_cols = 1,
             variable_colours_available = F,
             num_top_species = 3,
             plot_arrows = F,arrow_alpha = .2, arrow_colour = "grey20",arrow_scalar = 3,arrow_thickness = .5,
             label_arrows = T, arrow_label_size = .4, arrow_label_colour = "black", arrow_label_font_type = 1,
             filename = paste0("Result_figures/ordination_plots/Negative_samples_genus_pca.pdf"))
