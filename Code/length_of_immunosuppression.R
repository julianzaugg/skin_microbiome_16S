# Analysis into whether there is a correlation between the length of immunosuppression
# and the microbiome of patients

# Abundance plots for discrete time groupings, further broken down by transplant type. 

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)
# install.packages("cowplot")

common_theme <- theme(
  panel.border = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
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
  legend.key.height= unit(.3,"cm"),
  legend.key.width = unit(.3,"cm"),
  legend.text = element_text(size = 8),
  axis.line = element_line(colour = "black", size = 0.5),
  axis.text = element_text(size = 6, colour = "black"),
  axis.title = element_text(size = 7,face = "bold"),
  complete = F,
  plot.title = element_text(size = 8))


# ------------------------------------------------------------------------------------------
# Various colour palettes
my_colour_palette_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_palette_12_soft <-c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621", "#c75a93", "#7c7731")
my_colour_palette_50 <- c("#b27a2c","#bfab43","#60d2b3","#e78121","#ad3a8a","#e6a63e","#37773b","#b7ca7d","#b372df","#8fd341","#9787da","#dd6b9a","#47b83d","#e18388","#62da89","#7b6e21","#deb078","#e0845c","#6d55de","#db73c8","#6952ac","#87cc8c","#49cdd6","#91a6e7","#969955","#5e7921","#e33b2c","#994a6d","#4b6fe1","#bdd027","#4eacdb","#46a062","#9c36b7","#e136a7","#95592d","#4773b6","#da4753","#8e5999","#e3c536","#60e56a","#579b35","#d757de","#df356f","#338b70","#af3a28","#a44750","#a6be48","#5c6a31","#df9dd7","#d35e2c")

### Calculate the (min, max, mean, median, stdev, #samples) abundances of each taxa at each taxa level
# generate_taxa_summary <- function(mydata, taxa_column, group_by_columns){
#   select_columns <- c(taxa_column, group_by_columns, "Sample", "Patient", "Read_count", "Read_count_rarefied", "Relative_abundance", "Relative_abundance_rarefied")
#   total_samples <- length(unique(mydata$Sample))
#   total_patients <- length(unique(mydata$Patient))
#   taxa_group_summary <- 
#     mydata %>%
#     # dplyr::filter(retained = "yes") %>% # keep only those samples that were retained
#     dplyr::select_(.dots = select_columns) %>%
#     dplyr::group_by_(.dots = c(taxa_column, group_by_columns)) %>%
#     dplyr::dplyr::mutate(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% # number of unique samples/index
#     dplyr::group_by_(.dots = c(group_by_columns)) %>%
#     dplyr::dplyr::mutate(N_total_samples_in_group = n_distinct(Sample),
#                   N_total_patients_in_group = n_distinct(Patient))  %>%
#     dplyr::group_by_(.dots = c(group_by_columns, taxa_column)) %>%
#     dplyr::select(-Sample, -Patient) %>%
#     dplyr::summarise(N_samples = max(N_samples),
#                      N_total_samples_in_group = max(N_total_samples_in_group),
#                      N_patients = max(N_patients),
#                      N_total_patients_in_group = max(N_total_patients_in_group),
#                      Percent_group_samples = round((max(N_samples) / max(N_total_samples_in_group))*100, 2),
#                      Percent_total_samples = round((max(N_samples) / total_samples)*100, 2),
#                      Percent_group_patients = round((max(N_patients) / max(N_total_patients_in_group))*100, 2),
#                      Percent_total_patients = round((max(N_patients) / total_patients)*100, 2),
#                      Mean_read_count = round(mean(Read_count), 2),
#                      Median_read_count = median(Read_count),
#                      Min_read_count = min(Read_count),
#                      Max_read_count = max(Read_count),
#                      Summed_read_count = sum(Read_count),
#                      
#                      Mean_read_count_rarefied = round(mean(Read_count_rarefied),2),
#                      Median_read_count_rarefied = median(Read_count_rarefied),
#                      Min_read_count_rarefied = min(Read_count_rarefied),
#                      Max_read_count_rarefied = max(Read_count_rarefied),
#                      Summed_read_count_rarefied = sum(Read_count_rarefied),
#                      
#                      Mean_relative_abundance = round(mean(Relative_abundance), 5),
#                      Median_relative_abundance = round(median(Relative_abundance), 5),
#                      Min_relative_abundance = round(min(Relative_abundance),5),
#                      Max_relative_abundance = round(max(Relative_abundance),5),
#                      Summed_relative_abundance = round(sum(Relative_abundance),5),
#                      
#                      Mean_relative_abundance = round(mean(Relative_abundance_rarefied), 5),
#                      Median_relative_abundance_rarefied = round(median(Relative_abundance_rarefied), 5),
#                      Min_relative_abundance_rarefied = round(min(Relative_abundance_rarefied), 5),
#                      Max_relative_abundance_rarefied = round(max(Relative_abundance_rarefied), 5),
#                      Summed_relative_abundance_rarefied = round(sum(Relative_abundance_rarefied),5),
#                      
#     ) %>%
#     as.data.frame()
#   return(taxa_group_summary)
# }


# ------------------------------------------------------------------------------------------
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
source("Code/helper_functions.R")

# Load the OTU - taxonomy mapping file
# otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Set rownames to be index
rownames(metadata.df) <- metadata.df$Index

# Load the genus data
genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/genus_counts_abundances_and_metadata.csv")
# length(unique(genus_data.df$Sample)) == length(metadata.df$Index)

# Create a cleaned up taxonomy label
genus_data.df$taxonomy_label <- with(genus_data.df, paste0(Domain,";", Class,";", Genus))

# Create immunocompromised specific dataset
immunocompromised_data.df <- subset(genus_data.df, Cohort == "immunocompromised")
immunocompromised_data.df <- immunocompromised_data.df[!is.na(immunocompromised_data.df$Length_of_immunosuppression_group_1),]
immunocompromised_data.df <- immunocompromised_data.df[!is.na(immunocompromised_data.df$Length_of_immunosuppression_group_2),]

# Remove samples that do not have a bacterial load CFU value
immunocompromised_data.df <- immunocompromised_data.df[!is.na(immunocompromised_data.df$Bacterial_load_CFU),]

# Faceting by Lesion_type_refined
# C (swabs from proper control patients as indicated before)
# C_P (all C and AK_PL from remaining patients)
# AK (AK)
# SCC_PL (SCC_PL and IEC_PL)
# SCC (SCC and IEC)
immunocompromised_data.df$Lesion_type_refined <- factor(immunocompromised_data.df$Lesion_type_refined, levels = c("C","C_P", "AK", "SCC_PL", "SCC"))
immunocompromised_data.df$Length_of_immunosuppression_group_1 <- factor(immunocompromised_data.df$Length_of_immunosuppression_group_1, levels = rev(c("2-6", "7-15", "16 and higher")))
immunocompromised_data.df$Length_of_immunosuppression_group_2 <- factor(immunocompromised_data.df$Length_of_immunosuppression_group_2, levels = rev(c("2-8", "9-20", "21 and higher")))

# Faceting by Length_of_immunosuppression
# immunocompromised_data.df$Lesion_type_refined <- factor(immunocompromised_data.df$Lesion_type_refined, levels = rev(c("C","C_P", "AK", "SCC_PL", "SCC")))
# immunocompromised_data.df$Length_of_immunosuppression_group_1 <- factor(immunocompromised_data.df$Length_of_immunosuppression_group_1, levels = c("2-6", "7-15", "16 and higher"))
# immunocompromised_data.df$Length_of_immunosuppression_group_2 <- factor(immunocompromised_data.df$Length_of_immunosuppression_group_2, levels = c("2-8", "9-20", "21 and higher"))

# ----------
# Calculate the number of samples for each grouping and the number with bacterial loads of zero or higher
# unique(subset(immunocompromised_data.df,Lesion_type_refined  == "C")$Patient)
immunocompromised_data.df %>% group_by(Length_of_immunosuppression_group_1, Lesion_type_refined) %>% 
  summarise(number_of_samples = max(n_distinct(Sample)), number_of_patients = max(n_distinct(Patient)))

LOS_group_1_sample_summary.df <- immunocompromised_data.df %>% select(Sample,Patient,Lesion_type_refined, Length_of_immunosuppression_group_1, Bacterial_load_CFU) %>% 
  unique() %>%
  group_by(Length_of_immunosuppression_group_1, Lesion_type_refined) %>%
  dplyr::mutate(number_of_samples = n_distinct(Sample), number_of_patients = n_distinct(Patient))  %>%
  summarise(number_of_samples = max(number_of_samples),
            number_of_patients = max(number_of_patients),
            samples_with_bacterial_load_greater_than_zero = sum(Bacterial_load_CFU > 0, na.rm = T),
            samples_with_bacterial_load_zero_or_higher = sum(Bacterial_load_CFU >= 0, na.rm = T)) %>%
  as.data.frame()

LOS_group_2_sample_summary.df <- immunocompromised_data.df %>% select(Sample,Patient,Lesion_type_refined, Length_of_immunosuppression_group_2, Bacterial_load_CFU) %>% 
  unique() %>%
  group_by(Length_of_immunosuppression_group_2, Lesion_type_refined) %>%
  dplyr::mutate(number_of_samples = n_distinct(Sample),number_of_patients = n_distinct(Patient))  %>%
  summarise(number_of_samples = max(number_of_samples),
            number_of_patients = max(number_of_patients),
            samples_with_bacterial_load_greater_than_zero = sum(Bacterial_load_CFU > 0, na.rm = T),
            samples_with_bacterial_load_zero_or_higher = sum(Bacterial_load_CFU >= 0, na.rm = T)) %>%
  as.data.frame()
names(LOS_group_1_sample_summary.df)[1] <- "Length_of_immunosuppression_group"
names(LOS_group_2_sample_summary.df)[1] <- "Length_of_immunosuppression_group"
LOS_group_1_sample_summary.df$Grouping <- "Grouping_1"
LOS_group_2_sample_summary.df$Grouping <- "Grouping_2"
LOS_group_1_sample_summary.df <- LOS_group_1_sample_summary.df[c("Grouping", names(LOS_group_1_sample_summary.df)[names(LOS_group_1_sample_summary.df) != "Grouping"])]
LOS_group_2_sample_summary.df <- LOS_group_2_sample_summary.df[c("Grouping", names(LOS_group_2_sample_summary.df)[names(LOS_group_2_sample_summary.df) != "Grouping"])]

write.csv(x = rbind(LOS_group_1_sample_summary.df, LOS_group_2_sample_summary.df), 
          file ="Result_tables/other/length_of_suppression_sample_summary.csv", quote = F, row.names = F)
# --------------------------------------------------

LOI_group_1_summary.df <- generate_taxa_summary(mydata = immunocompromised_data.df, taxa_column = "taxonomy_label", group_by_columns = c("Length_of_immunosuppression_group_1", "Lesion_type_refined"))
LOI_group_2_summary.df <- generate_taxa_summary(mydata = immunocompromised_data.df, taxa_column = "taxonomy_label", group_by_columns = c("Length_of_immunosuppression_group_2", "Lesion_type_refined"))

lesion_summary.df <- generate_taxa_summary(mydata = immunocompromised_data.df, taxa_column = "taxonomy_label", group_by_columns = c("Lesion_type_refined"))

# Write summary to file
write.csv(LOI_group_1_summary.df, file = "Result_tables/abundance_analysis_tables/length_of_immunosuppression_grouping_1_lesion_abundance_summary.csv", row.names = F, quote = F)
write.csv(LOI_group_2_summary.df, file = "Result_tables/abundance_analysis_tables/length_of_immunosuppression_grouping_2_lesion_abundance_summary.csv", row.names = F, quote = F)

# Identify the top genus for each lesion type
LOI_group_1_top_summary.df <- filter_summary_to_top_n(taxa_summary = LOI_group_1_summary.df,
                                                                  grouping_variables = c("Length_of_immunosuppression_group_1","Lesion_type_refined"),
                                                                  abundance_column = "Mean_relative_abundance",
                                                                  my_top_n = 5)

LOI_group_2_top_summary.df <- filter_summary_to_top_n(taxa_summary = LOI_group_2_summary.df,
                                                      grouping_variables = c("Length_of_immunosuppression_group_2","Lesion_type_refined"),
                                                      abundance_column = "Mean_relative_abundance",
                                                      my_top_n = 5)
lesion_top_summary.df <- filter_summary_to_top_n(taxa_summary = lesion_summary.df,
                                                      grouping_variables = c("Lesion_type_refined"),
                                                      abundance_column = "Mean_relative_abundance",
                                                      my_top_n = 7)

# Top unique set of genera across the length of immunosuppresion groups
LOI_group_lesions_top_genus <- unique(c(LOI_group_1_top_summary.df$taxonomy_label, 
                                        LOI_group_2_top_summary.df$taxonomy_label))

# ------------------------------------------------------
# Create palette based on unique set of genera
# 'Other' taxa are those taxa that are not in the top n taxa

# For both length of immunosuppresion groups
genus_palette_both_groups <- setNames(my_colour_palette_30_distinct[1:length(LOI_group_lesions_top_genus)], LOI_group_lesions_top_genus)
genus_palette_both_groups["Other"] <- "grey"

# For each LOI group
genus_palette_LOI_group1 <- setNames(my_colour_palette_30_distinct[1:length(unique(LOI_group_1_top_summary.df$taxonomy_label))], unique(LOI_group_1_top_summary.df$taxonomy_label))
genus_palette_LOI_group2 <- setNames(my_colour_palette_30_distinct[1:length(unique(LOI_group_2_top_summary.df$taxonomy_label))], unique(LOI_group_2_top_summary.df$taxonomy_label))
genus_palette_LOI_group1["Other"] <- "grey"
genus_palette_LOI_group2["Other"] <- "grey"

# For each lesion type
genus_palette_lesions <-setNames(my_colour_palette_30_distinct[1:length(unique(lesion_top_summary.df$taxonomy_label))], unique(lesion_top_summary.df$taxonomy_label))
genus_palette_lesions["Other"] <- "grey"

# ------------------
# Take the full LOI summary tables and re-label any taxa not in the top taxa for each length of suppression group to "Other"
# LOI_group_1_summary.df[!LOI_group_1_summary.df$taxonomy_label %in% LOI_group_1_top_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# LOI_group_2_summary.df[!LOI_group_2_summary.df$taxonomy_label %in% LOI_group_2_top_summary.df$taxonomy_label,]$taxonomy_label <- "Other"

# Take the full LOI summary tables and re-label any taxa not in the top taxa for each lesion group to "Other"
LOI_group_1_summary.df[!LOI_group_1_summary.df$taxonomy_label %in% lesion_top_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
LOI_group_2_summary.df[!LOI_group_2_summary.df$taxonomy_label %in% lesion_top_summary.df$taxonomy_label,]$taxonomy_label <- "Other"
# ------------------

# Normalise the Mean_relative_abundance values within each length of suppression group
LOI_group_1_summary.df <- LOI_group_1_summary.df %>% group_by(Length_of_immunosuppression_group_1, Lesion_type_refined) %>% dplyr::mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
LOI_group_2_summary.df <- LOI_group_2_summary.df %>% group_by(Length_of_immunosuppression_group_2, Lesion_type_refined) %>% dplyr::mutate(Normalised_mean_relative_abundance = Mean_relative_abundance/sum(Mean_relative_abundance)) %>% as.data.frame()
# LOI_group_1_summary.df <- LOI_group_1_summary.df %>% group_by(Length_of_immunosuppression_group_1, Lesion_type_refined) %>% dplyr::mutate(Normalised_mean_relative_abundance = Median_relative_abundance_rarefied/sum(Median_relative_abundance_rarefied)) %>% as.data.frame()

# Ensure there is a single entry for the Other group. This is required for plotting.
LOI_group_1_summary.df <- LOI_group_1_summary.df %>% 
  group_by(Length_of_immunosuppression_group_1, Lesion_type_refined, taxonomy_label) %>% 
  dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% 
  as.data.frame()

LOI_group_2_summary.df <- LOI_group_2_summary.df %>% 
  group_by(Length_of_immunosuppression_group_2, Lesion_type_refined, taxonomy_label) %>% 
  dplyr::summarise(Normalised_mean_relative_abundance = sum(Normalised_mean_relative_abundance)) %>% 
  as.data.frame()

# -------------------------
# Calculate the mean bacterial loads for each lesion type + suppression group
LOI_group_1_mean_bacterial_loads.df <- immunocompromised_data.df %>% group_by(Length_of_immunosuppression_group_1, Lesion_type_refined) %>% dplyr::summarise(Mean_bacterial_load = mean(Bacterial_load_CFU, na.rm = T)) %>% as.data.frame()
rownames(LOI_group_1_mean_bacterial_loads.df) <- with(LOI_group_1_mean_bacterial_loads.df, paste0(Length_of_immunosuppression_group_1, "__", Lesion_type_refined))
LOI_group_1_summary.df$Group <- with(LOI_group_1_summary.df, paste0(Length_of_immunosuppression_group_1, "__", Lesion_type_refined))

LOI_group_2_mean_bacterial_loads.df <- immunocompromised_data.df %>% group_by(Length_of_immunosuppression_group_2, Lesion_type_refined) %>% dplyr::summarise(Mean_bacterial_load = mean(Bacterial_load_CFU, na.rm = T)) %>% as.data.frame()
rownames(LOI_group_2_mean_bacterial_loads.df) <- with(LOI_group_2_mean_bacterial_loads.df, paste0(Length_of_immunosuppression_group_2, "__", Lesion_type_refined))
LOI_group_2_summary.df$Group <- with(LOI_group_2_summary.df, paste0(Length_of_immunosuppression_group_2, "__", Lesion_type_refined))

# -------------------------
# Calculate abundance value scaled by the bacterial load
LOI_group_1_summary.df$Mean_relative_abundance_BL_proportional <-
  apply(LOI_group_1_summary.df, 1, function(x) as.numeric(x["Normalised_mean_relative_abundance"]) * 
          LOI_group_1_mean_bacterial_loads.df[x["Group"],"Mean_bacterial_load"])

LOI_group_2_summary.df$Mean_relative_abundance_BL_proportional <-
  apply(LOI_group_2_summary.df, 1, function(x) as.numeric(x["Normalised_mean_relative_abundance"]) * 
          LOI_group_2_mean_bacterial_loads.df[x["Group"],"Mean_bacterial_load"])

# Order the taxonomy by the abundance. This is only approximate (applies across all groups).
# FIXME find a way to order specific to each group...may require separate plots that are combined
LOI_group_1_summary.df <- LOI_group_1_summary.df %>% group_by(Length_of_immunosuppression_group_1, Lesion_type_refined) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(unique(LOI_group_1_summary.df$taxonomy_label)[unique(LOI_group_1_summary.df$taxonomy_label) != "Other"], "Other")
LOI_group_1_summary.df$taxonomy_label <- factor(LOI_group_1_summary.df$taxonomy_label, levels = my_levels)
LOI_group_1_summary.df$value_label <- lapply(LOI_group_1_summary.df$Normalised_mean_relative_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), ""))

LOI_group_2_summary.df <- LOI_group_2_summary.df %>% group_by(Length_of_immunosuppression_group_2, Lesion_type_refined) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
my_levels <- c(unique(LOI_group_2_summary.df$taxonomy_label)[unique(LOI_group_2_summary.df$taxonomy_label) != "Other"], "Other")
LOI_group_2_summary.df$taxonomy_label <- factor(LOI_group_2_summary.df$taxonomy_label, levels = my_levels)
LOI_group_2_summary.df$value_label <- lapply(LOI_group_2_summary.df$Normalised_mean_relative_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), ""))



# ----------------------------
# Plot abundance bar graphs
# Grouping 1
LOI_group_1_just_legend_plot <- ggplot(LOI_group_1_summary.df, aes(x = Length_of_immunosuppression_group_1, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = genus_palette_lesions, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 5)) +
  common_theme

LOI_group_1_abundance_plot <- ggplot(LOI_group_1_summary.df, aes(x = Length_of_immunosuppression_group_1, y = Normalised_mean_relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 1.5,color = "grey10") +
  coord_flip() +
  scale_fill_manual(values = genus_palette_lesions, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,101)) +
  xlab("Length of immunosuppression (years)") +
  ylab("Normalised mean relative abundance (%)") +
  common_theme + 
  facet_wrap(~Lesion_type_refined, ncol = 1)

LOI_group_1_BL_abundance_plot <- ggplot(LOI_group_1_summary.df, aes(x = Length_of_immunosuppression_group_1, y = Mean_relative_abundance_BL_proportional, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = genus_palette_lesions, guide = F) +
  scale_y_continuous(breaks = seq(0,10000, by = 1000), limits=c(0,5001)) +
  xlab("") +
  ylab("Normalised mean relative abundance scaled by mean bacterial load (CFU)") +
  common_theme + 
  facet_wrap(~Lesion_type_refined, ncol = 1) +
  theme(axis.text.y = element_blank())

# Extract the legend
my_legend_taxa <- cowplot::get_legend(LOI_group_1_just_legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 6),
                                          legend.title = element_text(size=7, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)

grid_plot <- plot_grid(plotlist = list(LOI_group_1_abundance_plot, NULL, LOI_group_1_BL_abundance_plot),ncol = 3,nrow=1, rel_widths = c(1,-.02,1),align = "hv")
# plot_grid(plotlist = list(LOI_group_1_abundance_plot, NULL, LOI_group_1_BL_abundance_plot),ncol = 3,nrow=1, rel_widths = c(1,-.05,1),align = "hv")
grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.4), ncol = 1, nrow=2)
grid_plot
# Make a grid of plots with the list of plots for both cohorts
# grid_plot <- plot_grid(plotlist = list(LOI_group_1_abundance_plot, my_legend_taxa),ncol = 1,nrow=2, rel_heights = c(1,.2))
# grid_plot
# grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.4), ncol = 1, nrow=2)
ggsave(filename = "Result_figures/abundance_analysis_plots/length_of_immunosuppression_grouping_1.pdf", plot = grid_plot, width = 30, height = 15, units = "cm")

# Grouping 2
LOI_group_2_just_legend_plot <- ggplot(LOI_group_2_summary.df, aes(x = Length_of_immunosuppression_group_2, y = Normalised_mean_relative_abundance, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = genus_palette_lesions, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 5)) +
  common_theme

LOI_group_2_abundance_plot <- ggplot(LOI_group_2_summary.df, aes(x = Length_of_immunosuppression_group_2, y = Normalised_mean_relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  geom_text(aes(label = value_label), position = position_stack(vjust = 0.5), size = 1.5,color = "grey10") +
  coord_flip() +
  scale_fill_manual(values = genus_palette_lesions, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,101)) +
  xlab("") +
  ylab("Normalised mean relative abundance (%)") +
  common_theme + 
  facet_wrap(~Lesion_type_refined, ncol = 1)

LOI_group_2_BL_abundance_plot <- ggplot(LOI_group_2_summary.df, aes(x = Length_of_immunosuppression_group_2, y = Mean_relative_abundance_BL_proportional, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = genus_palette_lesions, guide = F) +
  scale_y_continuous(breaks = seq(0,10000, by = 1000), limits=c(0,5001)) +
  xlab("") +
  ylab("Normalised mean relative abundance scaled by mean bacterial load (CFU)") +
  common_theme + 
  facet_wrap(~Lesion_type_refined, ncol = 1) + 
  theme(axis.text.y = element_blank())

# Extract the legend
my_legend_taxa <- cowplot::get_legend(LOI_group_2_just_legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 6),
                                          legend.title = element_text(size=7, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)

grid_plot <- plot_grid(plotlist = list(LOI_group_2_abundance_plot, NULL, LOI_group_2_BL_abundance_plot),ncol = 3,nrow=1, rel_widths = c(1,-.02,1),align = "hv")
grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.4), ncol = 1, nrow=2)
grid_plot

# Make a grid of plots with the list of plots for both cohorts
# grid_plot <- plot_grid(plotlist = list(LOI_group_2_abundance_plot, my_legend_taxa),ncol = 1,nrow=2, rel_heights = c(1,.2))
# grid_plot
# grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.4), ncol = 1, nrow=2)
ggsave(filename = "Result_figures/abundance_analysis_plots/length_of_immunosuppression_grouping_2.pdf", plot = grid_plot, width = 30, height = 15, units = "cm")
