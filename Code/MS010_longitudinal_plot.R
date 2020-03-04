# Script to generate the mean relative abundance figure for publication for MS010

# invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
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

# ------------------------------------------------------------------------------------------
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
source("Code/helper_functions.R")

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Filter to just immunosuppressed or snapshot samples
metadata.df <- subset(metadata.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")

# Make rownames the index
rownames(metadata.df) <- metadata.df$Index

# Load the list of samples of interest
MS010_lesions_of_interest_info.df <- read.table("data/MS010_SCC_longitudinal.tsv", sep = "\t", header = T)


swab_ids <- sort(with(MS010_lesions_of_interest_info.df, c(SCC_lesion_ID, Matching_SCC_PL_lesion_ID)))

#remove 348 and 349 as they are from a different lesion (apparently)
swab_ids <- swab_ids[! swab_ids %in% c(348, 349)]
# subset(metadata.df, Swab_ID %in% swab_ids)

# SCC	Matching_LC	Date
# 348 349 11.06.15
# 564	565	09.07.15
# 678	679	29.07.15
# 956	957	03.09.15
# 1200	1201	10.02.16
# swab_ids <- c(564,565,678,679,956,957,1200,1201)
# swab_ids <- 957
#S8560_J311, 957 didn't pass QC but 348 and 349 did

genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Genus_counts_abundances_and_metadata.csv")
genus_data.df$taxonomy_label <- with(genus_data.df, paste0(Domain,";", Class,";", Genus))
# length(unique(genus_data.df$taxonomy_genus))
# length(unique(genus_data.df[genus_data.df$Relative_abundance > 0.005,]$taxonomy_genus))

# Subset to swabs of interest
MS010_data.df <- subset(genus_data.df, Swab_ID %in% swab_ids)

# Generate taxa summary
MS010_taxa_summary.df <- generate_taxa_summary(mydata = MS010_data.df, taxa_column = "taxonomy_label", group_by_columns = c("Sample", "Date_sampled", "Lesion_type_refined"))

# Identify the top genus for each sample
MS010_top_taxa_summary.df <- filter_summary_to_top_n(taxa_summary = MS010_taxa_summary.df, 
                                                                  grouping_variables = c("Sample","Date_sampled"),
                                                                  abundance_column = "Mean_relative_abundance",
                                                                  my_top_n = 6)

# Create palette based on unique set
genus_palette <- setNames(my_colour_palette_30_distinct[1:length(unique(MS010_top_taxa_summary.df$taxonomy_label))], unique(MS010_top_taxa_summary.df$taxonomy_label))
genus_palette["Other"] <- "grey"

# Take the full table and re-label any taxa not in the top taxa to "Other"
MS010_taxa_summary.df[!MS010_taxa_summary.df$taxonomy_label %in% MS010_top_taxa_summary.df$taxonomy_label,]$taxonomy_label <- "Other"

# Collapse to a single entry for the Other group
MS010_taxa_summary.df <- MS010_taxa_summary.df %>% group_by(Sample, Date_sampled, Lesion_type_refined, taxonomy_label) %>% dplyr::summarise(Relative_abundance = sum(Summed_relative_abundance)) %>% as.data.frame()

# This should be equal to the number of samples (taxa abundances per sample should sum to 1)
MS010_taxa_summary.df %>% group_by(Sample) %>% summarise(total = sum(Relative_abundance))

# ---------------------------------------------
# Set order of labels
# "11.06.2015" "09.07.2015" "29.07.2015" "03.09.2015" "10.02.2016" 

# First order by abundance within each lesion type
MS010_taxa_summary.df <- MS010_taxa_summary.df %>% group_by(Lesion_type_refined) %>% arrange(Relative_abundance) %>% as.data.frame()

# Split date string into day, month and year columns
MS010_taxa_summary.df <- tidyr::separate(data= MS010_taxa_summary.df, col = "Date_sampled", into = c("Day", "Month", "Year"), sep = "/", remove = F, convert = T)

# Order by the year, month, day and then lesion type
# head(MS010_taxa_summary.df)
MS010_taxa_summary.df <- MS010_taxa_summary.df[with(MS010_taxa_summary.df, order(Year, Month,Day,Lesion_type_refined, decreasing = T)),]
# MS010_taxa_summary.df[c("Year", "Month","Day")]
# head(MS010_taxa_summary.df)

# Create the label for plotting and set the levels
MS010_taxa_summary.df$Label <- with(MS010_taxa_summary.df, paste(paste0(Day, "/", Month, "/", Year), Sample, Lesion_type_refined, sep =", "))
MS010_taxa_summary.df$Label <- factor(MS010_taxa_summary.df$Label, levels = unique(MS010_taxa_summary.df$Label))

# Set the levels for the taxonomy label. Ensure Other group is last
my_levels <- c(as.character(unique(MS010_taxa_summary.df$taxonomy_label))[as.character(unique(MS010_taxa_summary.df$taxonomy_label)) != "Other"], "Other")
MS010_taxa_summary.df$taxonomy_label <- factor(MS010_taxa_summary.df$taxonomy_label, levels = my_levels)
# ---------------------------------------------

# Generate plot
myplot <- ggplot(MS010_taxa_summary.df, aes(x = Label, y = Relative_abundance*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  # geom_text(aes(label = Relative_abundance*100), position = position_stack(vjust = 0.5), size = 2,color = "grey10") +
  coord_flip() +
  # scale_fill_manual(values = genus_palette, guide = F) +
  scale_fill_manual(values = genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 7)) +
  scale_y_continuous(breaks = seq(0,100, by = 10), limits=  c(0,101), expand =  c(0,0)) +
  xlab("Sample") +
  ylab("Relative abundance (%)") +
  common_theme + theme(legend.position = "bottom", 
                       legend.justification = "center",
                       legend.direction = "horizontal",
                       legend.box.just = "bottom",
                       legend.text = element_text(size = 6),
                       legend.title = element_text(size=7, face="bold")
                       )
myplot
ggsave(filename = "Result_figures/abundance_analysis_plots/MS010_taxa_summary.pdf", plot = myplot, height = 9, width = 30, units = "cm")
