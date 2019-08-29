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
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)
rownames(metadata.df) <- metadata.df$Index

MS010_lesions_of_interest_info <- read.table("data/MS010_SCC_longitudinal.tsv", sep = "\t", header = T)

swab_ids <- sort(with(MS010_lesions_of_interest_info,c(SCC, Matching_LC)))
#remove 348 and 349 as they are a different lesion
swab_ids <- swab_ids[! swab_ids %in% c(348, 349)]

metadata.df <- metadata.df[!metadata.df$Sampletype == "negative",]

# SCC	Matching_LC	Date
# 348 349 11.06.15
# 564	565	09.07.15
# 678	679	29.07.15
# 956	957	03.09.15
# 1200	1201	10.02.16
# swab_ids <- c(564,565,678,679,956,957,1200,1201)
# swab_ids <- 957
#S8560_J311, 957 didn't pass QC but 348 and 349 did

genus_data.df <- read.csv("Result_tables/other/genus_counts_abundances_and_metadata.csv")
genus_data.df$taxonomy_label <- with(genus_data.df, paste0(Domain,";", Class,";", Genus))

# Subset to swabs of interest
MS010_data.df <- subset(genus_data.df, Swab_ID %in% swab_ids)

MS010_taxa_summary.df <- generate_taxa_summary(mydata = MS010_data.df, taxa_column = "taxonomy_label", group_by_columns = c("Sample", "Date_sampled", "Sampletype_final_refined"))

# subset(MS010_taxa_summary.df, Sample == "S8559_J311")
# sum(subset(MS010_taxa_summary.df, Sample == "S8559_J311")$Summed_relative_abundance_rarefied)

# Identify the top genus for each sample
MS010_top_taxa_summary.df <- filter_summary_to_top_n(taxa_summary = MS010_taxa_summary.df, 
                                                                  grouping_variables = c("Sample","Date_sampled"),
                                                                  abundance_column = "Mean_relative_abundance_rarefied",
                                                                  my_top_n = 6)

# Create palette based on unique set
genus_palette <- setNames(my_colour_palette_30_distinct[1:length(unique(MS010_top_taxa_summary.df$taxonomy_label))], unique(MS010_top_taxa_summary.df$taxonomy_label))
genus_palette["Other"] <- "grey"

# Take the full table and re-label any taxa not in the top to "Other"
MS010_taxa_summary.df[!MS010_taxa_summary.df$taxonomy_label %in% MS010_top_taxa_summary.df$taxonomy_label,]$taxonomy_label <- "Other"

# Need a single entry for the Other group
MS010_taxa_summary.df <- MS010_taxa_summary.df %>% group_by(Sample,Date_sampled,Sampletype_final_refined, taxonomy_label) %>% dplyr::summarise(Relative_abundance_rarefied = sum(Summed_relative_abundance_rarefied)) %>% as.data.frame()

MS010_taxa_summary.df %>% group_by(Sample) %>% summarise(total = sum(Relative_abundance_rarefied))


# Set order of labels
# "11.06.2015" "09.07.2015" "29.07.2015" "03.09.2015" "10.02.2016" 
MS010_taxa_summary.df <- MS010_taxa_summary.df %>% group_by(Sampletype_final_refined) %>% arrange(Relative_abundance_rarefied) %>% as.data.frame()
MS010_taxa_summary.df <- tidyr::separate(data= MS010_taxa_summary.df, col = "Date_sampled", into = c("Day", "Month", "Year"), sep = "/", remove = F)
MS010_taxa_summary.df <- MS010_taxa_summary.df[with(MS010_taxa_summary.df, order(Year, Month,Day,Sampletype_final_refined, decreasing = T)),]
MS010_taxa_summary.df$Label <- with(MS010_taxa_summary.df, paste(paste0(Day, "/", Month, "/", Year), Sample, Sampletype_final_refined, sep =", "))
MS010_taxa_summary.df$Label <- factor(MS010_taxa_summary.df$Label, levels = unique(MS010_taxa_summary.df$Label))

my_levels <- c(as.character(unique(MS010_taxa_summary.df$taxonomy_label))[as.character(unique(MS010_taxa_summary.df$taxonomy_label)) != "Other"], "Other")
MS010_taxa_summary.df$taxonomy_label <- factor(MS010_taxa_summary.df$taxonomy_label, levels = my_levels)

myplot <- ggplot(MS010_taxa_summary.df, aes(x = Label, y = Relative_abundance_rarefied*100, fill = taxonomy_label)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  # geom_text(aes(label = Relative_abundance_rarefied*100), position = position_stack(vjust = 0.5), size = 2,color = "grey10") +
  coord_flip() +
  # scale_fill_manual(values = genus_palette, guide = F) +
  scale_fill_manual(values = genus_palette, name = "Taxonomy", guide = guide_legend(title.position = "top",nrow= 5)) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample") +
  ylab("Relative abundance (%)") +
  common_theme + theme(legend.position = "bottom", 
                       legend.justification = "center",
                       legend.direction = "horizontal",
                       legend.box.just = "bottom",
                       legend.text = element_text(size = 6),
                       legend.title = element_text(size=7, face="bold")
                       )

ggsave(filename = "Result_figures/abundance_analysis_plots/MS010_taxa_summary.pdf", plot = myplot, height = 8, width = 25, units = "cm")
