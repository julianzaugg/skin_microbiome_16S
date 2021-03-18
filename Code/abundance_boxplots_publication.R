
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)


setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
source("Code/helper_functions.R")

common_theme <- theme(
  panel.border = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black", size = 0.5),
  panel.background = element_blank(),
  strip.background = element_rect(fill = "white", colour = "white", size = 1),
  strip.text = element_text(size = 6),
  legend.key=element_blank(),
  legend.direction="vertical",
  legend.background = element_rect(colour ="white", size = .3),
  legend.text.align = 0,
  legend.title = element_text(size=10, face="bold"),
  legend.title.align = 0.5,
  legend.margin = margin(c(2,2,2,2)),
  legend.key.height=unit(.4,"cm"),
  legend.text = element_text(size = 8),
  axis.text = element_text(size = 8, colour = "black"),
  axis.title = element_text(size = 10,face = "bold"),
  complete = F,
  plot.title = element_text(size = 8))


# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Load the abundance matrix
genus_rel.df <- read.table("Result_tables/relative_abundance_tables/Genus_relative_abundances.csv", 
                          sep =",", header = T)

# load the combined abundance and metadata tables
genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Genus_counts_abundances_and_metadata.csv")

# Calculate taxa summary
temp <- generate_taxa_summary(genus_data.df, taxa_column = "taxonomy_genus",
                      group_by_columns = c("Cohort", "Sample_type"))

# Filter to top # per sample
top_genus <- filter_summary_to_top_n(temp,
                        grouping_variables = c("Cohort", "Sample_type"),
                        abundance_column = "Mean_relative_abundance",
                        my_top_n = 30)
top_genus <- unique(top_genus$taxonomy_genus)

# Filter abundance matrix to top taxa
genus_rel.df <- genus_rel.df[genus_rel.df$taxonomy_genus %in% top_genus,]

# forearm_swab_ids_IS <- c("1382","1383","1384", "1385","1470","1471","1561","1562",
#                          "1599","1600","1649", "1650")
# 
# temp <- metadata.df[with(metadata.df, which(Swab_ID %in% forearm_swab_ids_IS | all(!Sample_type %in% c("SCC", "SCC_PL") & Cohort != "immunosuppressed" ))),]
# temp <- metadata.df[with(metadata.df, which(Cohort == "immunosuppressed" & !Sample_type %in% c("SCC", "SCC_PL") | Swab_ID %in% forearm_swab_ids_IS)),]
# temp <- rbind(temp, subset(metadata.df, Cohort == "immunocompetent"))
# genus_rel.df <- genus_rel.df[,c("taxonomy_genus", temp$Index)]

# Create cohort specific data sets
# immunosuppressed_metadata.df <- metadata.df[metadata.df$Cohort == "immunosuppressed",]
# immunocompetent_metadata.df <- metadata.df[metadata.df$Cohort == "immunocompetent",]
# immunosuppressed_genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed")
# immunocompetent_genus_data.df <- subset(genus_data.df, Cohort == "immunocompetent")


# genus_rel_specific.df <- genus_rel.df[grepl("g__Staphylococcus|g__Paracoccus|g__Cutibacterium|g__Malassezia|g__Micrococcus", genus_rel.df$taxonomy_genus),]
# genus_rel_specific.df <- genus_rel.df[grepl("g__Staphylococcus|g__Paracoccus|g__Cutibacterium|g__Malassezia|g__Micrococcus|g__Pseudomonas|g__Streptococcus|g__Corynebacterium", genus_rel.df$taxonomy_genus),]
# specific_taxa <- c("Staphylococcus","Paracoccus","Cutibacterium","Malassezia","Micrococcus","Pseudomonas","Streptococcus","Corynebacterium")
specific_taxa <- c("Staphylococcus","Paracoccus","Cutibacterium","Malassezia","Micrococcus","Pseudomonas")
# g__Pseudomonas|g__Streptococcus|g__Corynebacterium
genus_rel_specific.df <- genus_rel.df

genus_rel_specific.df <- melt(genus_rel_specific.df, variable.name = "Index", value.name = "Relative_abundance")
genus_rel_specific.df <- left_join(genus_rel_specific.df, metadata.df, by = "Index")
genus_rel_specific.df <- genus_rel_specific.df %>% select(Index, Sample_type, Sample_type_colour, Cohort, Relative_abundance,taxonomy_genus)
genus_rel_specific.df$Genus <- gsub("g__", "", unlist(lapply(as.character(genus_rel_specific.df$taxonomy_genus), function(x) unlist(strsplit(x,";"))[6])))

# genus_rel_specific.df$Genus <- unlist(lapply(as.character(genus_rel_specific.df$taxonomy_genus), function(x) paste0(unlist(strsplit(x,";"))[5:6], collapse = ";")))
# genus_rel_specific.df$Genus <- gsub("f__", "", genus_rel_specific.df$Genus)
# genus_rel_specific.df$Genus <- gsub("g__", "", genus_rel_specific.df$Genus)
genus_rel_specific.df$Relative_abundance <- genus_rel_specific.df$Relative_abundance  *100

# Order with Staphylococcus first
genus_rel_specific.df$Genus <- factor(genus_rel_specific.df$Genus, levels = c("Staphylococcus", 
                                                                              sort(unique(genus_rel_specific.df$Genus)[unique(genus_rel_specific.df$Genus) != "Staphylococcus"])))

# Cohort specific data
IS_genus_rel_specific.df <- subset(genus_rel_specific.df, Cohort == "immunosuppressed")
IC_genus_rel_specific.df <- subset(genus_rel_specific.df, Cohort == "immunocompetent")

IS_genus_rel_specific.df$Sample_type <- factor(IS_genus_rel_specific.df$Sample_type, levels = c("NS", "PDS", "AK", "SCC_PL", "SCC"))
IC_genus_rel_specific.df$Sample_type <- factor(IC_genus_rel_specific.df$Sample_type, levels = c("PDS", "AK", "SCC_PL", "SCC"))

# IS_genus_rel_specific.df %>% group_by(Sample_type) %>% tally(n_distinct(Index))
# metadata.df %>% group_by(Cohort, Sample_type) %>% tally(n_distinct(Index))

# Calculate the significance values for taxa between multiple groups
calculate_taxa_significances_multiple <- function(mydata, variable_column, value_column, taxonomy_column){
  results.df <- data.frame("Taxonomy" = character(),
                           "Variable" = character(),
                           "Group_1" = character(),
                           "Group_2" = character(),
                           "Dunn_pvalue" = character(),
                           "Dunn_padj" = character(),
                           "KrusW_pvalue" = character()
  )
  for (taxa in unique(mydata[,taxonomy_column])){ # For each taxa in the taxonomy column
    taxa_data <- subset(mydata, get(taxonomy_column) == taxa)
    n_groups = length(as.character(unique(taxa_data[,variable_column])))
    if (any(is.na(taxa_data[,variable_column]))){
      return()
    }
    if (all(taxa_data[,value_column] == 0)){
      next()
    }
    # print(taxa)
    # print(n_groups)
    if (n_groups > 2){
      kw <- kruskal.test(get(value_column)~get(variable_column), data = taxa_data)
      dunn <- dunnTest(x = get(value_column)~get(variable_column), data = taxa_data, method = "bh", alpha = 0.05)
      dunn <- separate(dunn$res, Comparison, into = c("Group_1", "Group_2"), sep = " - ")[,c("Group_1","Group_2","P.unadj","P.adj")]
      names(dunn) <- c("Group_1","Group_2","Dunn_pvalue","Dunn_padj")
      dunn$Taxonomy <- taxa
      dunn$KrusW_pvalue <- kw$p.value
      dunn$Variable <- variable_column
      results.df <- rbind(results.df, dunn)
    }
  }
  results.df[,c("Taxonomy", "Variable", "Group_1","Group_2", "Dunn_pvalue", "Dunn_padj", "KrusW_pvalue")]
}


# Calculate significance values comparing sample types
IS_genus_significances <- calculate_taxa_significances_multiple(mydata = IS_genus_rel_specific.df, 
                                      variable_column = "Sample_type",
                                      value_column = "Relative_abundance",
                                      taxonomy_column = "Genus")

IC_genus_significances <- calculate_taxa_significances_multiple(mydata = IC_genus_rel_specific.df, 
                                                                variable_column = "Sample_type",
                                                                value_column = "Relative_abundance",
                                                                taxonomy_column = "Genus")
# ----------------------------------------------------------------
# mean(with(IS_genus_rel_specific.df, IS_genus_rel_specific.df[Genus == "Brachybacterium" & Sample_type == "SCC_PL",])$Relative_abundance)
# temp <- IS_genus_rel_specific.df %>% 
  # group_by(Cohort, Sample_type,Genus) %>%
  # dplyr::summarise(Mean_abundance = mean(Relative_abundance))
# temp[temp$Genus == "Brachybacterium",]

combined_abundances.df <- 
  rbind(IS_genus_rel_specific.df, IC_genus_rel_specific.df) %>%
  group_by(Cohort, Sample_type,Genus) %>%
  dplyr::summarise(Mean_abundance = mean(Relative_abundance))

combined_abundances.df$c_st_g <- with(combined_abundances.df, paste0(Cohort, Sample_type,Genus))

# Add group sizes
immunosuppressed_group_count.df <- 
  IS_genus_rel_specific.df %>% select(Index, Sample_type) %>% 
  group_by(Sample_type) %>%
  distinct() %>%
  tally()

immunocompetent_group_count.df <- 
  IC_genus_rel_specific.df %>% select(Index, Sample_type) %>% 
  group_by(Sample_type) %>%
  distinct() %>%
  tally()

immunosuppressed_group_count.l <- setNames(immunosuppressed_group_count.df$n,immunosuppressed_group_count.df$Sample_type)
immunocompetent_group_count.l <- setNames(immunocompetent_group_count.df$n,immunocompetent_group_count.df$Sample_type)

IS_genus_significances$N_Group_1 <- 
  unlist(lapply(IS_genus_significances$Group_1, function(x)immunosuppressed_group_count.l[[x]]))
IS_genus_significances$N_Group_2 <- 
  unlist(lapply(IS_genus_significances$Group_2, function(x)immunosuppressed_group_count.l[[x]]))

IC_genus_significances$N_Group_1 <- 
  unlist(lapply(IC_genus_significances$Group_1, function(x)immunocompetent_group_count.l[[x]]))
IC_genus_significances$N_Group_2 <- 
  unlist(lapply(IC_genus_significances$Group_2, function(x)immunocompetent_group_count.l[[x]]))
# ----------------------------------------------------------------

# Add cohort
IS_genus_significances$Cohort <- "immunosuppressed"
IC_genus_significances$Cohort <- "immunocompetent"

# Add mean abundances
IS_genus_significances$Mean_abundance_Group_1 <- unlist(lapply(with(IS_genus_significances, paste0(Cohort, Group_1,Taxonomy)),
                                                               function(x) combined_abundances.df[combined_abundances.df$c_st_g == x,"Mean_abundance"][[1]]))
IS_genus_significances$Mean_abundance_Group_2 <- unlist(lapply(with(IS_genus_significances, paste0(Cohort, Group_2,Taxonomy)),
                                                               function(x) combined_abundances.df[combined_abundances.df$c_st_g == x,"Mean_abundance"][[1]]))

IC_genus_significances$Mean_abundance_Group_1 <- unlist(lapply(with(IC_genus_significances, paste0(Cohort, Group_1,Taxonomy)),
                                                               function(x) combined_abundances.df[combined_abundances.df$c_st_g == x,"Mean_abundance"][[1]]))
IC_genus_significances$Mean_abundance_Group_2 <- unlist(lapply(with(IC_genus_significances, paste0(Cohort, Group_2,Taxonomy)),
                                                               function(x) combined_abundances.df[combined_abundances.df$c_st_g == x,"Mean_abundance"][[1]]))


generate_p_labels <- function(sig_table){
  for (sig_column in c("Dunn_padj")){
    metric = strsplit(sig_column, "_")[[1]][1]
    sig_table[,paste0(metric, "_p_label")] <-
      as.character(lapply(sig_table[,sig_column], 
                          function(x) ifelse(x <= 0.001, "***", 
                                             ifelse(x <= 0.01, "**",
                                                    ifelse(x <= 0.05, "*", "ns")))))
  }
  sig_table
}
# subset(IS_genus_significances, Taxonomy == "Staphylococcus")
genus_significances_combined <- rbind(IS_genus_significances,IC_genus_significances)
genus_significances_combined <- subset(genus_significances_combined, Dunn_padj <= 0.05)
genus_significances_combined <- genus_significances_combined[order(genus_significances_combined$Taxonomy),]
genus_significances_combined <- generate_p_labels(genus_significances_combined)
# genus_significances_combined <- genus_significances_combined[genus_significances_combined$Taxonomy %in% specific_taxa,]


write.csv(file = "Result_tables/abundance_analysis_tables/genus_significances_combined_publication.csv",
          x = genus_significances_combined, row.names = F,quote = F)

# Create p-value label
# IS_genus_significances$P_value_label <- as.character(lapply(IS_genus_significances[,"Dunn_padj"], function(x) ifelse(x <= 0.001, "***", 
#                                                                                                       ifelse(x <= 0.01, "**", 
#                                                                                                              ifelse(x <= 0.05, "*", "ns")))))
# 
# # Create p-value label
# IC_genus_significances$P_value_label <- as.character(lapply(IC_genus_significances[,"Dunn_padj"], function(x) ifelse(x <= 0.001, "***", 
#                                                                                                                      ifelse(x <= 0.01, "**", 
#                                                                                                                             ifelse(x <= 0.05, "*", "ns")))))

sig_threshold = 0.05 # p-values less than this will be displayed
sig_line_scaling_percentage = 0.08 # percent of max y value for spacing significance lines
sig_vjust = 0.03 # amount to vertically adjust the significance annotations
sig_tip_length = 0.01 # length of tips on significance lines
sig_linetype = 1 # linetype of significance lines
sig_colour = "grey20" # colour of significance lines
sig_line_starting_scale = 1.05

process_significances <- function(my_sig_data,my_abundance_data,variable_column,value_column){
  my_sig_data <- my_sig_data[which(my_sig_data[,"Dunn_padj"] <= sig_threshold),]
  
  # Determine the maximum diversity value for the pair of groups being compared
  for (row in 1:nrow(my_sig_data)){
    group_1 <- as.character(my_sig_data[row,"Group_1"])
    group_2 <- as.character(my_sig_data[row,"Group_2"])
    y_max <- max(my_abundance_data[which(my_abundance_data[,variable_column] == group_1),][,value_column],
                 my_abundance_data[which(my_abundance_data[,variable_column] == group_2),][,value_column])
    my_sig_data[row,"y_max"] <- y_max
    my_sig_data[row,"level_index_group_1"] <- which(levels(my_abundance_data[,variable_column]) == group_1)
    my_sig_data[row,"level_index_group_2"] <- which(levels(my_abundance_data[,variable_column]) == group_2)
    my_sig_data[row,"level_distance"] <- abs(my_sig_data[row,"level_index_group_2"] - my_sig_data[row,"level_index_group_1"])
  }
  
  my_sig_data <- my_sig_data[order(my_sig_data$level_distance),]
  scale <- sig_line_starting_scale # starting scale
  for (row in 1:nrow(my_sig_data)){
    my_sig_data[row, "y_position"] <- max(my_sig_data[, "y_max"]) * scale
    scale <- scale + sig_line_scaling_percentage # increase scale value
  }
  
  my_sig_data$Group_1 <- factor(my_sig_data$Group_1, levels = levels(my_abundance_data[,variable_column]))
  my_sig_data$Group_2 <- factor(my_sig_data$Group_2, levels = levels(my_abundance_data[,variable_column]))
  
  my_sig_data$P_value_label <- as.character(lapply(my_sig_data[,"Dunn_padj"], function(x) ifelse(x <= 0.001, "***", 
                                                                                                 ifelse(x <= 0.01, "**", 
                                                                                                        ifelse(x <= 0.05, "*", "ns")))))
  
  my_sig_data
}

# Add p-value annotations
IS_genus_significances <- process_significances(my_sig_data = IS_genus_significances,
                   my_abundance_data = IS_genus_rel_specific.df,
                   variable_column = "Sample_type",
                   value_column = "Relative_abundance")


IC_genus_significances <- process_significances(my_sig_data = IC_genus_significances,
                                             my_abundance_data = IC_genus_rel_specific.df,
                                             variable_column = "Sample_type",
                                             value_column = "Relative_abundance")

# ------------------------------------------------------
# For manual analysis, limit to taxa of interest and significant comparisons
temp <- rbind(IS_genus_significances,IC_genus_significances)

temp <- temp[temp$Dunn_padj <= 0.05,]
temp <- subset(temp, Taxonomy %in% specific_taxa)
write.csv(x = temp,
          file = "Result_tables/abundance_analysis_tables/temp.csv", row.names = F, quote = F)
# ------------------------------------------------------

sample_type_colours <- unique(IS_genus_rel_specific.df[,c("Sample_type", "Sample_type_colour")])
sample_type_colours <- setNames(as.character(sample_type_colours[,"Sample_type_colour"]), 
                                sample_type_colours[,"Sample_type"])

variable_shapes <- setNames(c(25,24,23,22,21), c("NS", "PDS", "AK", "SCC_PL", "SCC"))

IS_genus_rel_specific.df <- subset(IS_genus_rel_specific.df, Genus %in% specific_taxa)
IC_genus_rel_specific.df <- subset(IC_genus_rel_specific.df, Genus %in% specific_taxa)

IS_genus_plot <- ggplot(IS_genus_rel_specific.df, aes(x = Genus, 
                                     y = Relative_abundance, 
                                     fill = Sample_type, 
                                     shape = Sample_type)) +
  geom_boxplot(position = position_dodge(width =.75), 
               outlier.shape = NA, width=.5,lwd =.3) +
  geom_jitter(size = .6,stroke =.1,
              position = position_jitterdodge(jitter.width = .2,
                                              dodge.width = .75)) +
  scale_shape_manual(values = variable_shapes,name = "Sample type") +
  scale_fill_manual(values = sample_type_colours, name = "Sample type") +
  # stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
  #              width = 0.75, size = .2, linetype = "solid", colour = "red",
  #              position = "dodge") +
  stat_summary(fun = "mean", colour = "grey2", geom = "point",
               shape = 16,size = 1,
               position = position_dodge(width = .75),show.legend = F) +
  # stat_summary(fun = "mean", colour = "grey20", geom = "point",
               # shape = 16,lwd = 3,
               # position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) + 
  ylab("Relative abundance") +
  common_theme +
  theme(axis.text.x = element_text(face = "italic"))

#   myplot <- myplot + ggsignif::geom_signif(data = sig_subset.df,
# manual = T,
# inherit.aes = F,
# aes(xmin = Group_1, xmax = Group_2, annotations = P_value_label, y_position = y_position),
# linetype = sig_linetype,
# color = sig_colour,
# size = .5,
# tip_length = sig_tip_length, vjust = sig_vjust)

# IS_genus_plot + geom_signif(
#                             xmin = 1, xmax = 1.1,
#                             y_position = 100, annotations = "*",
#                             linetype = sig_linetype,
#                             colour = sig_colour,
#                             size = .5,
#                             tip_length = sig_tip_length
#                          )
# IS_genus_plot + ggsignif::geom_signif(data = IS_genus_significances,
#                                          manual = T,
#                                          inherit.aes = F,
#                                       
#                                          aes(xmin = Group_1, xmax = Group_2, annotations = P_value_label, y_position = y_position),
#                                          linetype = sig_linetype,
#                                          color = sig_colour,
#                                          size = .5,
#                                          tip_length = sig_tip_length, vjust = sig_vjust)



ggsave(plot = IS_genus_plot, 
       filename = "Result_figures/abundance_analysis_plots/IS_abundance_boxplot_publication.pdf",
       width = 18, height = 12, units = "cm"
       )


IC_genus_plot <- ggplot(IC_genus_rel_specific.df, aes(x = Genus, 
                                                      y = Relative_abundance, 
                                                      fill = Sample_type, 
                                                      shape = Sample_type)) +
  geom_boxplot(position = position_dodge(width =.75), 
               outlier.shape = NA, width=.5,lwd =.3) +
  geom_jitter(size = .6,stroke =.1,
              position = position_jitterdodge(jitter.width = .2,
                                              dodge.width = .75)) +
  scale_shape_manual(values = variable_shapes,name = "Sample type") +
  scale_fill_manual(values = sample_type_colours, name = "Sample type") +
  # scale_colour_manual(values = sample_type_colours, name = "Sample type") +
  # stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
  #              width = 0.75, size = .2, linetype = "solid", colour = "red",
  #              position = "dodge") +
  stat_summary(fun = "mean", colour = "grey2", geom = "point",
               shape = 16,size = 1,
               position = position_dodge(width = .75),show.legend = F) +
  # stat_summary(fun = "mean", colour = "grey20", geom = "point",
  # shape = 16,lwd = 3,
  # position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) + 
  ylab("Relative abundance") +
  common_theme +
  theme(axis.text.x = element_text(face = "italic"))
IC_genus_plot

ggsave(plot = IC_genus_plot, 
       filename = "Result_figures/abundance_analysis_plots/IC_abundance_boxplot_publication.pdf",
       width = 18, height = 12, units = "cm"
)

# Extract the legend
my_legend <- cowplot::get_legend(IS_genus_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 7),
                                          legend.title = element_text(size =8, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)


grid_plot <- plot_grid(plotlist = list(IS_genus_plot + 
                                         labs(title = "Organ transplant recipient") + 
                                         theme(legend.position = "none",
                                               plot.title = element_text(size = 10, 
                                                                         face = "bold",
                                                                         hjust = 0.5)), 
                                       IC_genus_plot + labs(title = "Immunocompetent") + 
                                         theme(axis.title.y = element_blank(),
                                               axis.text.y = element_blank(),
                                               legend.position = "none",
                                               plot.title = element_text(size = 10, 
                                                                         face = "bold",
                                                                         hjust = 0.5
                                                                         ))),
                       ncol =2,rel_widths = c(1,.85))
grid_plot <- plot_grid(plotlist = list(grid_plot, my_legend), 
                       nrow = 2, rel_heights = c(1,.1))

ggsave(plot = grid_plot, 
       filename = "Result_figures/abundance_analysis_plots/IS_IC_abundance_boxplot_publication.pdf",
       width = 30, height = 12, units = "cm"
)

ggsave(plot = grid_plot, 
       filename = "Result_figures/abundance_analysis_plots/IS_IC_abundance_boxplot_publication.svg",
       width = 30, height = 12, units = "cm",device = "svg"
)

