# Script to generate the mean relative abundance and bacterial load figure for publication

library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)


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

# ------------------------------------------------------------------------------------------
### Calculate the (min, max, mean, median, stdev, #samples) abundances of each taxa at each taxa level
generate_taxa_summary <- function(mydata, taxa_column, group_by_columns){
  # select_columns <- c(taxa_column, group_by_columns, "Sample", "Patient", "Read_count", "Read_count_rarefied", "Relative_abundance", "Relative_abundance_rarefied", "Bacterial_load_CFU")
  select_columns <- c(taxa_column, group_by_columns, "Sample", "Patient", "Read_count", "Read_count_rarefied", "Relative_abundance", "Relative_abundance_rarefied")
  total_samples <- length(unique(mydata$Sample))
  total_patients <- length(unique(mydata$Patient))
  taxa_group_summary <- 
    mydata %>%
    # dplyr::filter(retained = "yes") %>% # keep only those samples that were retained
    dplyr::select_(.dots = select_columns) %>%
    dplyr::group_by_(.dots = c(taxa_column, group_by_columns)) %>%
    dplyr::mutate(N_samples = n_distinct(Sample), N_patients = n_distinct(Patient)) %>% # number of unique samples/index
    dplyr::group_by_(.dots = c(group_by_columns)) %>%
    dplyr::mutate(N_total_samples_in_group = n_distinct(Sample),
                  N_total_patients_in_group = n_distinct(Patient))  %>%
    dplyr::group_by_(.dots = c(group_by_columns, taxa_column)) %>%
    dplyr::select(-Sample, -Patient) %>%
    dplyr::summarise(N_samples = max(N_samples),
                     N_total_samples_in_group = max(N_total_samples_in_group),
                     N_patients = max(N_patients),
                     N_total_patients_in_group = max(N_total_patients_in_group),
                     Percent_group_samples = round((max(N_samples) / max(N_total_samples_in_group))*100, 2),
                     Percent_total_samples = round((max(N_samples) / total_samples)*100, 2),
                     Percent_group_patients = round((max(N_patients) / max(N_total_patients_in_group))*100, 2),
                     Percent_total_patients = round((max(N_patients) / total_patients)*100, 2),
                     Mean_read_count = round(mean(Read_count), 2),
                     Median_read_count = median(Read_count),
                     Min_read_count = min(Read_count),
                     Max_read_count = max(Read_count),
                     Summed_read_count = sum(Read_count),
                     
                     Mean_read_count_rarefied = round(mean(Read_count_rarefied),2),
                     Median_read_count_rarefied = median(Read_count_rarefied),
                     Min_read_count_rarefied = min(Read_count_rarefied),
                     Max_read_count_rarefied = max(Read_count_rarefied),
                     Summed_read_count_rarefied = sum(Read_count_rarefied),
                     
                     Mean_relative_abundance = round(mean(Relative_abundance), 5),
                     Median_relative_abundance = round(median(Relative_abundance), 5),
                     Min_relative_abundance = round(min(Relative_abundance),5),
                     Max_relative_abundance = round(max(Relative_abundance),5),
                     Summed_relative_abundance = round(sum(Relative_abundance),5),
                     
                     Mean_relative_abundance_rarefied = round(mean(Relative_abundance_rarefied), 5),
                     Median_relative_abundance_rarefied = round(median(Relative_abundance_rarefied), 5),
                     Min_relative_abundance_rarefied = round(min(Relative_abundance_rarefied), 5),
                     Max_relative_abundance_rarefied = round(max(Relative_abundance_rarefied), 5),
                     Summed_relative_abundance_rarefied = round(sum(Relative_abundance_rarefied),5),
                     
                     # Mean_bacterial_load = round(mean(Bacterial_load_CFU), 5),
                     # Median_bacterial_load = round(median(Bacterial_load_CFU), 5),
                     # Min_bacterial_load = round(min(Bacterial_load_CFU), 5),
                     # Max_bacterial_load = round(max(Bacterial_load_CFU), 5),
                     # Summed_bacterial_load = round(sum(Bacterial_load_CFU),5)
    ) %>%
    as.data.frame()
  return(taxa_group_summary)
}

filter_summary_to_top_n <- function(taxa_summary, grouping_variables, abundance_column, my_top_n = 10){
  # Get the top N taxa as described in a provided taxa summary table.
  out <- 
    taxa_summary %>%
    dplyr::group_by_(.dots = c(grouping_variables)) %>%
    dplyr::arrange(dplyr::desc(get(abundance_column))) %>%
    dplyr::top_n(my_top_n, get(abundance_column)) %>% 
    # dplyr::arrange_(.dots = c(grouping_variables),abundance_column) %>%
    dplyr::arrange_(.dots = c(grouping_variables)) %>%
    as.data.frame()
  return(out)
}


# ------------------------------------------------------------------------------------------
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC and SCC lesions. 
# Remove samples for different lesion types (nasal,scar,scar_PL,KA,KA_PL,VV,VV_PL,SF,SF_PL,other,other_PL) from metadata and otu table
metadata.df <- metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC", "NLC"),]

# Factorise discrete columns
metadata.df$Sampletype_compromised_refined <- factor(metadata.df$Sampletype_compromised_refined)

# Load filtered/processed abundance data with metadata
genus_data.df <- read.csv("Result_tables/other/genus_counts_abundances_and_metadata.csv")

genus_data.df <- genus_data.df[genus_data.df$Sample %in% rownames(metadata.df),]

# Set levels
genus_data.df$Sampletype_pooled <- factor(genus_data.df$Sampletype_pooled, levels = c("LC", "AK","SCC"))
genus_data.df$Sampletype_compromised_refined <- factor(genus_data.df$Sampletype_compromised_refined, levels = c("C","LC", "AK","SCC"))
# genus_data.df$Sampletype_final <- factor(genus_data.df$Sampletype_final, levels = c("C","LC", "AK","SCC"))
genus_data.df$Sampletype_final <- factor(genus_data.df$Sampletype_final, levels = c("SCC","AK","LC", "C"))

# ----------------------------------------------------------------------------------------
# Subset to immunocompromised samples
immunocompromised_data.df <- subset(genus_data.df, Project == "immunocompromised")

# Remove samples that do not have a bacterial load CFU value
immunocompromised_data.df <- immunocompromised_data.df[!is.na(immunocompromised_data.df$Bacterial_load_CFU),]

# Remove factorisation as we will re-assign later
immunocompromised_data.df$taxonomy_genus <- as.character(immunocompromised_data.df$taxonomy_genus)

# Generate full genus summary for each lesion type
immunocompromised_genus_summary.df <- generate_taxa_summary(immunocompromised_data.df,
                                                            taxa_column = "taxonomy_genus", 
                                                            group_by_columns = c("Sampletype_final"))

# Identify the top genus for each lesion type
immunocompromised_top_genus_summary.df <- filter_summary_to_top_n(taxa_summary = immunocompromised_genus_summary.df, 
                        grouping_variables = c("Sampletype_final"),
                        abundance_column = "Mean_relative_abundance_rarefied",
                        my_top_n = 6)

# Take the full table and re-label any taxa not in the top to "Other"
immunocompromised_data.df[!immunocompromised_data.df$taxonomy_genus %in% immunocompromised_top_genus_summary.df$taxonomy_genus,]$taxonomy_genus <- "Other"
length(unique(immunocompromised_data.df$taxonomy_genus))

# Create Family_Genus value. Will be the label used.
immunocompromised_data.df$Family_Genus <- gsub(".*(f__.*)", "\\1",immunocompromised_data.df[,"taxonomy_genus"])
# "d__Eukaryota;p__Opisthokonta;c__Nucletmycea;o__Fungi;D_9__Malasseziaceae;D_10__Malassezia" should become "d__Eukaryota;o__Fungi;D_10__Malassezia"
immunocompromised_data.df[grep("Malassezi",immunocompromised_data.df$Family_Genus),]$Family_Genus <- "d__Eukaryota;o__Fungi;D_10__Malassezia"

# Put the filtered / processed table back through the summary function to get the final Mean abundance values
immunocompromised_genus_summary_processed.df <- generate_taxa_summary(immunocompromised_data.df,
                                                                      taxa_column = "Family_Genus", 
                                                                      group_by_columns = c("Sampletype_final"))

# Before normalisation
# immunocompromised_genus_summary_processed.df[c("Sampletype_compromised_refined","Family_Genus","Mean_relative_abundance_rarefied")]

# Normalise the Mean_relative_abundance_rarefied values
for (lesion in unique(immunocompromised_genus_summary_processed.df$Sampletype_final)){
  immunocompromised_genus_summary_processed.df[immunocompromised_genus_summary_processed.df$Sampletype_final == lesion,]$Mean_relative_abundance_rarefied <- 
  immunocompromised_genus_summary_processed.df[immunocompromised_genus_summary_processed.df$Sampletype_final == lesion,]$Mean_relative_abundance_rarefied / 
    sum(immunocompromised_genus_summary_processed.df[immunocompromised_genus_summary_processed.df$Sampletype_final == lesion,]$Mean_relative_abundance_rarefied)
}
# After normalisation
# immunocompromised_genus_summary_processed.df[c("Sampletype_final","Family_Genus","Mean_relative_abundance_rarefied")]

# Calculate the mean bacterial loads for each lesion type
immunocompromised_sf_mean_bacterial_loads.df <- immunocompromised_data.df %>% group_by(Sampletype_final) %>% dplyr::summarise(Mean_bacterial_load = mean(Bacterial_load_CFU)) %>% as.data.frame()
rownames(immunocompromised_sf_mean_bacterial_loads.df) <- immunocompromised_sf_mean_bacterial_loads.df$Sampletype_final

# Calculate abundance value proportional to bacterial load
immunocompromised_genus_summary_processed.df$Mean_relative_abundance_rarefied_BL_proportional <-
  apply(immunocompromised_genus_summary_processed.df, 1, function(x) as.numeric(x["Mean_relative_abundance_rarefied"]) * immunocompromised_sf_mean_bacterial_loads.df[x["Sampletype_final"],"Mean_bacterial_load"])

# immunocompromised_genus_summary_processed.df[c("Sampletype_final","Mean_relative_abundance_rarefied", "Mean_relative_abundance_rarefied_BL_proportional")]
      
# Get unique list of taxa, ensure "Other" is first
most_abundant_taxa <- sort(unique(immunocompromised_genus_summary_processed.df$Family_Genus))
most_abundant_taxa <- c("Other", most_abundant_taxa[!grepl("Other", most_abundant_taxa)])

# Create palette
# immunocompromised_genus_palette <- setNames(my_colour_palette_30_distinct[1:length(most_abundant_taxa)], most_abundant_taxa)
immunocompromised_genus_palette <- setNames(my_colour_palette_12_soft[1:length(most_abundant_taxa)], most_abundant_taxa)
immunocompromised_genus_palette["Other"] <- "grey"


just_legend_plot <- ggplot(immunocompromised_genus_summary_processed.df, aes(x = Sampletype_final, y = Mean_relative_abundance_rarefied, fill = Family_Genus)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = immunocompromised_genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 3)) +
  xlab("Sample site") +
  ylab("Mean relative abundance") +
  common_theme 

abundance_plot <- ggplot(immunocompromised_genus_summary_processed.df, aes(x = Sampletype_final, y = Mean_relative_abundance_rarefied*100, fill = Family_Genus)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = immunocompromised_genus_palette, guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  ylab("Mean relative abundance (%)") +
  common_theme

BL_abundance_plot <- ggplot(immunocompromised_genus_summary_processed.df, aes(x = Sampletype_final, y = Mean_relative_abundance_rarefied_BL_proportional, fill = Family_Genus)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = immunocompromised_genus_palette,guide = F) +
  scale_y_continuous(breaks = seq(0,30000, by = 5000), limits=c(0,30001)) +
  # xlab("Sample site") +
  xlab("") +
  ylab("Mean relative abundance scaled by mean bacterial load (CFU)") +
  
  common_theme +
  theme(axis.text.y = element_blank())


# Extract the legend
my_legend_taxa <- cowplot::get_legend(just_legend_plot + 
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

# Make a grid of plots with the list of plots for both cohorts
grid_plot <- plot_grid(plotlist = list(abundance_plot, NULL, BL_abundance_plot),ncol = 3,nrow=1, rel_widths = c(1,-.12,1),align = "hv")
grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.4), ncol = 1, nrow=2)
ggsave(filename = "Result_figures/abundance_analysis_plots/sampletype_relative_abundaunce_and_bacterial_load.pdf", plot = grid_plot, width = 20, height = 6, units = "cm")

