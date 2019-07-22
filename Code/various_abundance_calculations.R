# Script to calculate / format the abundances for different groupings
# Some basic plotting of abundance data

library(dplyr)
library(reshape2)
library(ggplot2)

# install.packages("VennDiagram")
library(VennDiagram)



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
  axis.text = element_text(size = 9, colour = "black"),
  axis.title = element_text(size = 10,face = "bold"),
  complete = F,
  plot.title = element_text(size = 8))

# ------------------------------------------------------------------------------------------
# Various colour palettes
my_colour_palette <- c("#8dd3c7","#ffffb3","#bebada","#fb8072", "#80b1d3", "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5", "#cc0000")
# From http://tools.medialab.sciences-po.fr/iwanthue/
my_colour_palette_20 <- c("#66bd79","#a35bcf","#5bb643","#d14ea6","#a2b239","#5c6bcc","#dc892e","#5e93cd","#d64737","#49b6a8","#dc3c6e","#4f7e3c","#bd8cd5","#caab55","#914c88","#867230","#df82a2","#a65429","#ab4a5a","#e0896a")
my_colour_palette_20_distinct <- c("#0057b4","#7fff56","#d600bc","#d8d500","#e76eff","#019932","#9f8fff","#ffc730","#007fac","#a20019","#06fefd","#ff6782","#00774c","#e0c8ff","#717a00","#4b2952","#e2ed7d","#46321e","#ffbd76","#ffb4c6")
my_colour_palette_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_palette_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
my_colour_palette_15 <- c("#77b642","#7166d9","#cfa240","#b351bb","#4fac7f","#d44891","#79843a","#c68ad4","#d15a2c","#5ba7d9","#ce4355","#6570ba","#b67249","#9b4a6f","#df8398")
my_colour_palette_32_distinct <- c("#ea7e00","#ca0074","#d1c69b","#474007","#bb00ad","#9c80ff","#be3300","#542e72","#00b9f5","#09436b","#8b0036","#9ac8e6","#ff1059","#959eff","#154a11","#0290f4","#ff7762","#7dbf00","#ff8194","#834c00","#006e73","#f9bb5d","#d6c943","#017229","#00d3a8","#732427","#36e191","#6a8200","#efb3ea","#3227bb","#ff90e1","#e92a12")
# lesion_palette_7 <- c("#8558d6","#6ee268","#d247ad","#c9d743","#d7453e","#59a237","#d78f2a")
# patient_palette_45 <- c("#d64530","#585fb1","#795d97","#9e4773","#3f6921","#71692c","#a2b93c","#d571cc","#9b3e97","#33947a","#98ad66","#448a4e","#869ae0","#5ce7af","#e085a3","#dfdc87","#d19be2","#5cb735","#e38269","#3db6c0","#50b565","#50902c","#a98a2c","#dde84a","#db3d76","#5fe485","#7c8329","#b3e791","#6fe965","#5ebce9","#3c86c1","#2a6a45","#65b688","#6651d1","#af4ed3","#df872f","#56e4db","#737cea","#ac464b","#dd37b5","#995b2b","#daac6f","#92e2be","#a2e24b","#e0be3a")
my_colour_palette_10_distinct <- c("#8eec45","#0265e8","#f6a800","#bf6549","#486900","#c655a0","#00d1b6","#ff4431","#aeb85c","#7e7fc8")
my_colour_palette_10_soft <- c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621")

my_colour_palette_12_soft <-c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621", "#c75a93", "#7c7731")


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
metadata.df$Patient <- factor(metadata.df$Patient)
metadata.df$Sampletype <- factor(metadata.df$Sampletype)
metadata.df$Sampletype_pooled <- factor(metadata.df$Sampletype_pooled)
metadata.df$Sampletype_compromised_refined <- factor(metadata.df$Sampletype_compromised_refined)
metadata.df$Project <- factor(metadata.df$Project)
metadata.df$Patient_group <- factor(metadata.df$Patient_group)
metadata.df$Gender <- factor(metadata.df$Gender)
metadata.df$Number_of_meds <- factor(metadata.df$Number_of_meds)

# Load unfiltered data
# unfiltered_data.df <- read.csv(file = "Result_tables/other/project_otu_table_unfiltered.csv",header = T)

# Load filtered/processed abundance data with metadata
# otu_data.df <- read.csv("Result_tables/other/OTU_counts_abundances_and_metadata.csv")
genus_data.df <- read.csv("Result_tables/other/genus_counts_abundances_and_metadata.csv")
# family_data.df <- read.csv("Result_tables/other/family_counts_abundances_and_metadata.csv")
# order_data.df <- read.csv("Result_tables/other/order_counts_abundances_and_metadata.csv")
# class_data.df <- read.csv("Result_tables/other/class_counts_abundances_and_metadata.csv")
# phylum_data.df <- read.csv("Result_tables/other/phylum_counts_abundances_and_metadata.csv")

# First, filter out non-snapshot samples from immunocompetent
genus_data.df <- subset(genus_data.df, Project == "immunocompromised" | Snapshot_sample == "yes")

genus_data.df <- genus_data.df[genus_data.df$Sample %in% rownames(metadata.df),]

# Set levels
genus_data.df$Sampletype_pooled <- factor(genus_data.df$Sampletype_pooled, levels = c("LC", "AK","SCC"))
genus_data.df$Sampletype_compromised_refined <- factor(genus_data.df$Sampletype_compromised_refined, levels = c("C","LC", "AK","SCC"))
genus_data.df$Patient_group <- factor(genus_data.df$Patient_group, levels = c("Control", "AK","SCC"))
genus_data.df$Number_of_meds <- factor(genus_data.df$Number_of_meds, levels = c("1", "2","3"))
genus_data.df$Fitzpatrick_skin_type <- factor(genus_data.df$Fitzpatrick_skin_type, levels = c("1", "2","3","4"))
genus_data.df$Project_Sampletype_pooled <- with(genus_data.df, paste0(Project, "_", Sampletype_pooled))
genus_data.df$Project_Sampletype_pooled <- factor(genus_data.df$Project_Sampletype_pooled , levels = sort(unique(genus_data.df$Project_Sampletype_pooled)))

genus_data.df$Sampletype_final <- factor(genus_data.df$Sampletype_final, levels = c("C", "LC", "AK", "SCC"))


# --------------------------------------------------------------------------------------------
# FUNCTIONS

### Calculate the (min, max, mean, median, stdev, #samples) abundances of each taxa at each taxa level
generate_taxa_summary <- function(mydata, taxa_column, group_by_columns){
  # select_columns <- c(taxa_column, group_by_columns, "Sample", "Read_count", "Read_count_rarefied", "Relative_abundance", "Relative_abundance_rarefied")
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
                     Summed_relative_abundance_rarefied = round(sum(Relative_abundance_rarefied),5)
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


# Boxplot for relative abundances. If data has not been filtered to a single taxa entry,
# the plot will be a mix and should have factet_wrap(~TAXA) applied. Or just pre-filter.
generate_abundance_boxplot <- function(mydata, variable, metric, variable_colours_available = T){
  internal_data.df <- mydata[!is.na(mydata[variable]),]
  variable_values <- factor(as.character(unique(internal_data.df[[variable]])))
  if (variable_colours_available == T){
    color_col_name <- paste0(variable, "_colour")
    variable_colours <- setNames(as.character(unique(internal_data.df[[color_col_name]])), as.character(unique(internal_data.df[[variable]])))
  } else{
    variable_colours <- setNames(my_colour_palette_206_distinct[1:length(variable_values)], variable_values)  
  }
  myplot <- ggplot(internal_data.df, aes(x = get(variable), y = get(metric))) +
    geom_boxplot(outlier.shape = NA, aes(fill = get(variable))) +
    scale_fill_manual(values = variable_colours, name = variable) +
    geom_jitter(size=0.5, width = 0.10, height=0) +
    guides(fill=FALSE) +
    # scale_y_continuous(limits = c(0,4.5), breaks = seq(0,4.5,.5)) +
    xlab("") +
    ylab(metric)  +
    # common_theme +
    theme(panel.border = element_blank(), 
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
          axis.text = element_text(size = 9, colour = "black"),
          axis.title = element_text(size = 10,face = "bold"),
          complete = F,
          plot.title = element_text(size = 6, hjust = 0.5,face = "bold"),
          plot.subtitle = element_text(size =6, hjust = 0.5))
  myplot
}

# mydata = data table
# taxa_column, e.g. taxonomy_genus or Genus
# variable, e.g. Sampletype_pooled
# metric, e.g. Relative_abundance_rarefied
generate_multiple_abundance_boxplot <-  function(mydata, taxa_column, variable, metric, variable_colours_available = T, add_points = F){
  internal_data.df <- mydata[!is.na(mydata[variable]),]
  variable_values <- factor(as.character(unique(internal_data.df[[variable]])))
  if (variable_colours_available == T){
    color_col_name <- paste0(variable, "_colour")
    variable_colours <- setNames(as.character(unique(internal_data.df[[color_col_name]])), as.character(unique(internal_data.df[[variable]])))
  } else{
    variable_colours <- setNames(my_colour_palette_206_distinct[1:length(variable_values)], variable_values)  
  }
  # Check for groups that are missing taxa. If any are missing, create a single fake entry.
  # This forces ggplot boxplots to have all the discrete groups
  for (entry in unique(internal_data.df[,taxa_column])){
    data_subset <- subset(internal_data.df, get(taxa_column) == entry) # get those entries matching the taxa
    # Determine those groups defined in the full data that are missing from the subset 
    missing_groups <- unique(internal_data.df[,variable])[!unique(internal_data.df[,variable]) %in% data_subset[,variable]]
    for (mg in missing_groups){
      temp <- data_subset[1,]
      temp[,taxa_column] <- entry
      temp[,variable] <- mg
      # temp[,metric] <- NA
      temp[,metric] <- 0
      internal_data.df <- rbind(internal_data.df, temp)
    }
  }

  myplot <- ggplot(internal_data.df, aes(x = get(taxa_column), y = get(metric), fill = get(variable))) +
    geom_boxplot(outlier.shape = NA) +
    scale_fill_manual(values = variable_colours, name = variable) +
    coord_flip() +
    # guides(fill=FALSE) +
    # scale_y_continuous(limits = c(0,4.5), breaks = seq(0,4.5,.5)) +
    xlab("") +
    ylab(metric)  +
    theme(panel.border = element_blank(), 
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
          legend.title = element_text(size=8, face="bold"),
          legend.title.align = 0.5,
          legend.margin = margin(c(2,2,2,2)),
          legend.key.height=unit(.4,"cm"),
          legend.text = element_text(size = 8),
          axis.text = element_text(size = 9, colour = "black"),
          axis.title = element_text(size = 10,face = "bold"),
          complete = F,
          plot.title = element_text(size = 6))
  if(add_points){
    myplot <- myplot + geom_point(position = position_jitterdodge(), size = 0.2)
  }
  myplot
}

# Calculate the significance between abundances for taxa
calculate_abundance_significance <- function(mydata, variable, taxonomy_column, metric){
  internal_data.df <- mydata
  results.df <- data.frame("Taxonomy" = character(),
                           "Group_1" = character(),
                           "Group_2" = character(),
                           "MannW_pvalue" = character(),
                           "KrusW_pvalue" = character())
  # "Shannon_MannW_pvalue" = character(),
  group_combinations <- combn(as.character(unique(internal_data.df[,variable])), 2)
  for (taxa in unique(internal_data.df[,taxonomy_column])){ # For each taxa in the taxonomy column
    for (i in 1:ncol(group_combinations)) { # For each group combination
      group_1 <- group_combinations[1,i]
      group_2 <- group_combinations[2,i]
      group_1_meta <- subset(internal_data.df, get(variable) == group_1 & get(taxonomy_column) == taxa)
      group_2_meta <- subset(internal_data.df, get(variable) == group_2 & get(taxonomy_column) == taxa)
      if (any(c(nrow(group_1_meta) < 2, nrow(group_2_meta) < 2))){
        next
      }
      # Mann-Whitney test
      wilcox_test <- wilcox.test(group_1_meta[,metric], group_2_meta[,metric], exact = F)
    
      # Kruskal-Wallis
      kruskal_test <- kruskal.test(get(metric)~get(variable), data = subset(mydata, get(variable) %in% c(group_1, group_2)))

      results.df <- rbind(results.df, data.frame("Taxonomy" = taxa,
                                                 "Group_1" = group_1, 
                                                 "Group_2" = group_2, 
                                                 "MannW_pvalue" = round(wilcox_test$p.value,6),
                                                 "KrusW_pvalue" = round(kruskal_test$p.value,6)
      ))
    }
  }
  
  results.df$MannW_padj <- round(p.adjust(results.df$MannW_pvalue,method = "BH"),6)
  results.df$KrusW_padj <- round(p.adjust(results.df$KrusW_pvalue,method = "BH"),6)
  results.df
}

# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# Project
project_genus_summary.df <- generate_taxa_summary(genus_data.df,
                                                  taxa_column = "taxonomy_genus", 
                                                  group_by_columns = c("Project"))

project_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary = project_genus_summary.df, 
                                                        grouping_variables = c("Project"), 
                                                        abundance_column = "Mean_relative_abundance_rarefied", 
                                                        my_top_n = 10)

project_genus_summary_top_significances.df <- calculate_abundance_significance(mydata = subset(genus_data.df, taxonomy_genus %in% project_genus_summary_top.df$taxonomy_genus), 
                                         variable = "Project", taxonomy_column = "taxonomy_genus",metric = "Relative_abundance_rarefied")

write.csv(project_genus_summary_top_significances.df,"Result_tables/abundance_analysis_tables/project__genus_wilcox.csv", row.names = F)

# Make group boxplot for top taxa
temp <- genus_data.df
temp <- temp[temp$taxonomy_genus %in% project_genus_summary_top.df$taxonomy_genus,]
temp$Family_Genus <- with(temp, paste0(Family, ";", Genus))
myplot <- generate_multiple_abundance_boxplot(temp, "Family_Genus", "Project","Relative_abundance_rarefied", add_points = F) + 
  geom_point(position = position_jitterdodge(), size = .3,stroke = 0.1, shape = 21,aes(fill = Project)) +
  # scale_y_continuous(limits = c(-1,1)) +
  # facet_wrap(~Project) + 
  ylab("Relative abundance") + 
  xlab("Taxa") +
  theme(axis.text.y = element_text(size =6),
        axis.text.x = element_text(size =6),
        axis.title = element_text(size =6))
ggsave(plot = myplot,
       filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Project_top_taxa.pdf"),
       height = 10,
       width = 16,
       units = "cm")

# Make individual boxplots for the top taxa
for (genus in project_genus_summary_top.df$taxonomy_genus){
  data_subset <- subset(genus_data.df, taxonomy_genus == genus)
  base_name <- gsub(".*(f__.*)", "\\1",genus)
  myplot <- generate_abundance_boxplot(data_subset, variable = "Project", metric = "Relative_abundance_rarefied") +
    ylab("Relative abundance") +
    scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
    ggtitle(base_name)
  
  ggsave(plot = myplot,
         filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Project/genus__",base_name, ".pdf"),
         height = 8,
         width = 12,
         units = "cm")
}

# ------------------------------------------  
# Project_Sampletype_pooled (just for significance calculations)
project_sampletype_pooled_genus_summary.df <- generate_taxa_summary(genus_data.df,
                                                                    taxa_column = "taxonomy_genus", 
                                                                    group_by_columns = c("Project_Sampletype_pooled"))
project_sampletype_pooled_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary =  project_sampletype_pooled_genus_summary.df, 
                                                                          grouping_variables = c("Project_Sampletype_pooled"), 
                                                                          abundance_column = "Mean_relative_abundance_rarefied", 
                                                                          my_top_n = 9)
project_sampletype_pooled_genus_top_significances.df <- calculate_abundance_significance(mydata = subset(genus_data.df, taxonomy_genus %in% project_sampletype_pooled_genus_summary_top.df$taxonomy_genus), 
                                                                                         variable = "Project_Sampletype_pooled", taxonomy_column = "taxonomy_genus",metric = "Relative_abundance_rarefied")

project_sampletype_pooled_genus_top_significances.df <- separate(project_sampletype_pooled_genus_top_significances.df,col = "Group_1", into = c("Group_1_Project", "Group_1_Sampletype_pooled"),sep = "_")
project_sampletype_pooled_genus_top_significances.df <- separate(project_sampletype_pooled_genus_top_significances.df,col = "Group_2", into = c("Group_2_Project", "Group_2_Sampletype_pooled"),sep = "_")
project_sampletype_pooled_genus_top_significances.df <- project_sampletype_pooled_genus_top_significances.df[c("Taxonomy", "Group_1_Project", "Group_2_Project", 
                                                       "Group_1_Sampletype_pooled", "Group_2_Sampletype_pooled",
                                                       "MannW_pvalue", "MannW_padj", "KrusW_pvalue", "KrusW_padj")]
project_sampletype_pooled_genus_top_significances.df$Same_project <- project_sampletype_pooled_genus_top_significances.df$Group_1_Project == project_sampletype_pooled_genus_top_significances.df$Group_2_Project
project_sampletype_pooled_genus_top_significances.df$Same_sampletype <- project_sampletype_pooled_genus_top_significances.df$Group_1_Sampletype_pooled == project_sampletype_pooled_genus_top_significances.df$Group_2_Sampletype_pooled

write.csv(project_sampletype_pooled_genus_top_significances.df,"Result_tables/abundance_analysis_tables/Project_sampletype_pooled__genus_wilcox.csv", row.names = F)


# ------------------------------------------
# Sampletype_pooled vs Sampletype_pooled for each Project
project_sampletype_pooled_genus_summary.df <- generate_taxa_summary(genus_data.df,
                                                                    taxa_column = "taxonomy_genus", 
                                                                    group_by_columns = c("Project", "Sampletype_pooled"))
project_sampletype_pooled_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary =  project_sampletype_pooled_genus_summary.df, 
                                                                          grouping_variables = c("Project", "Sampletype_pooled"), 
                                                                          abundance_column = "Mean_relative_abundance_rarefied", 
                                                                          my_top_n = 9)

# immunocompetent
project_sampletype_pooled_genus_top_significances.df <- calculate_abundance_significance(mydata = subset(genus_data.df, Project == "immunocompetent" & taxonomy_genus %in% project_sampletype_pooled_genus_summary_top.df$taxonomy_genus), 
                                                                               variable = "Sampletype_pooled", taxonomy_column = "taxonomy_genus",metric = "Relative_abundance_rarefied")
project_sampletype_pooled_genus_top_significances.df$Project <- "immunocompetent"
write.csv(project_sampletype_pooled_genus_top_significances.df,"Result_tables/abundance_analysis_tables/immunocompetent_sampletype_pooled__genus_wilcox.csv", row.names = F)
# immunocompromised
project_sampletype_pooled_genus_top_significances.df <- calculate_abundance_significance(mydata = subset(genus_data.df, Project == "immunocompromised" & taxonomy_genus %in% project_sampletype_pooled_genus_summary_top.df$taxonomy_genus), 
                                                                                         variable = "Sampletype_pooled", taxonomy_column = "taxonomy_genus",metric = "Relative_abundance_rarefied")
project_sampletype_pooled_genus_top_significances.df$Project <- "immunocompromised"
write.csv(project_sampletype_pooled_genus_top_significances.df,"Result_tables/abundance_analysis_tables/immunocompromised_sampletype_pooled__genus_wilcox.csv", row.names = F)

# Make group boxplot for top taxa
temp <- genus_data.df
temp <- temp[temp$taxonomy_genus %in% project_sampletype_pooled_genus_summary_top.df$taxonomy_genus,]
temp$Family_Genus <- with(temp, paste0(Family, ";", Genus))
myplot <- generate_multiple_abundance_boxplot(temp, "Family_Genus", "Sampletype_pooled","Relative_abundance_rarefied", add_points = F) + 
  geom_point(position = position_jitterdodge(), size = .3,stroke = 0.1, shape = 21,aes(fill = Sampletype_pooled)) +
  # scale_y_continuous(limits = c(-1,1)) +
  facet_wrap(~Project) +
  ylab("Relative abundance") + 
  xlab("Taxa") +
  theme(axis.text.y = element_text(size =6),
        axis.text.x = element_text(size =6),
        axis.title = element_text(size =6))
ggsave(plot = myplot,
       filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Project_sampletype_pooled_top_taxa.pdf"),
       height = 15,
       width = 16,
       units = "cm")


# Make individual boxplots for the top taxa
for (project in unique(project_genus_summary_top.df$Project)){
  for (genus in project_genus_summary_top.df$taxonomy_genus){
    data_subset <- subset(genus_data.df, taxonomy_genus == genus & Project == project)
    base_name <- gsub(".*(f__.*)", "\\1",genus)
    myplot <- generate_abundance_boxplot(data_subset, variable = "Sampletype_pooled", metric = "Relative_abundance_rarefied") +
      ylab("Relative abundance") +
      scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
      labs(title = project,
           subtitle = base_name)
    if (nchar(base_name) > 50){
      myplot <- myplot + theme(plot.subtitle = element_text(size = 3.5, hjust = 0.5))
    } else{
      myplot <- myplot + theme(plot.subtitle = element_text(size = 6, hjust = 0.5))
    }
      # ggtitle(paste0(project, "\n", base_name))
    
    ggsave(plot = myplot,
           filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Project_sampletype_pooled/", project, "_genus__",base_name, ".pdf"),
           height = 8,
           width = 8,
           units = "cm")
  }
}
# ------------------------------------------
# Sampletype_pooled (competent) vs Sampletype_compromised_refined (compromised)
modified_genus_data.df <- genus_data.df
modified_genus_data.df$Sampletype_compromised_refined_both_projects <- NA
modified_genus_data.df$Sampletype_compromised_refined_both_projects_colour <- NA
modified_genus_data.df[modified_genus_data.df$Project == "immunocompetent",]$Sampletype_compromised_refined_both_projects <- 
  as.character(modified_genus_data.df[modified_genus_data.df$Project == "immunocompetent",]$Sampletype_pooled)

modified_genus_data.df[modified_genus_data.df$Project == "immunocompetent",]$Sampletype_compromised_refined_both_projects_colour <-
  as.character(modified_genus_data.df[modified_genus_data.df$Project == "immunocompetent",]$Sampletype_pooled_colour)

modified_genus_data.df[modified_genus_data.df$Project == "immunocompromised",]$Sampletype_compromised_refined_both_projects <- 
  as.character(modified_genus_data.df[modified_genus_data.df$Project == "immunocompromised",]$Sampletype_compromised_refined)

modified_genus_data.df[modified_genus_data.df$Project == "immunocompetent",]$Sampletype_compromised_refined_both_projects_colour <-
  as.character(modified_genus_data.df[modified_genus_data.df$Project == "immunocompetent",]$Sampletype_pooled_colour)

modified_genus_data.df$Sampletype_compromised_refined_both_projects <- factor(modified_genus_data.df$Sampletype_compromised_refined_both_projects, levels = c("C", "LC", "AK", "SCC"))



project_Sampletype_compromised_refined_both_projects_genus_summary.df <- generate_taxa_summary(modified_genus_data.df,
                                                                    taxa_column = "taxonomy_genus", 
                                                                    group_by_columns = c("Project", "Sampletype_compromised_refined_both_projects"))
project_Sampletype_compromised_refined_both_projects_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary =  project_Sampletype_compromised_refined_both_projects_genus_summary.df, 
                                                                          grouping_variables = c("Project", "Sampletype_compromised_refined_both_projects"), 
                                                                          abundance_column = "Mean_relative_abundance_rarefied", 
                                                                          my_top_n = 9)

# immunocompetent
project_Sampletype_compromised_refined_both_projects_genus_top_significances.df <- calculate_abundance_significance(mydata = subset(modified_genus_data.df, Project == "immunocompetent" & taxonomy_genus %in% project_Sampletype_compromised_refined_both_projects_genus_summary_top.df$taxonomy_genus), 
                                                                                         variable = "Sampletype_compromised_refined_both_projects", taxonomy_column = "taxonomy_genus",metric = "Relative_abundance_rarefied")
project_Sampletype_compromised_refined_both_projects_genus_top_significances.df$Project <- "immunocompetent"
write.csv(project_Sampletype_compromised_refined_both_projects_genus_top_significances.df,"Result_tables/abundance_analysis_tables/immunocompetent_sampletype_compromised_refined_both_projects__genus_wilcox.csv", row.names = F)

# immunocompromised
project_Sampletype_compromised_refined_both_projects_genus_top_significances.df <- calculate_abundance_significance(mydata = subset(modified_genus_data.df, Project == "immunocompromised" & taxonomy_genus %in% project_Sampletype_compromised_refined_both_projects_genus_summary_top.df$taxonomy_genus), 
                                                                                         variable = "Sampletype_compromised_refined_both_projects", taxonomy_column = "taxonomy_genus",metric = "Relative_abundance_rarefied")
project_Sampletype_compromised_refined_both_projects_genus_top_significances.df$Project <- "immunocompromised"
write.csv(project_Sampletype_compromised_refined_both_projects_genus_top_significances.df,"Result_tables/abundance_analysis_tables/immunocompromised_sampletype_compromised_refined_both_projects__genus_wilcox.csv", row.names = F)

# Make group boxplot for top taxa
# temp <- modified_genus_data.df
# temp <- temp[temp$taxonomy_genus %in% project_Sampletype_compromised_refined_both_projects_genus_summary_top.df$taxonomy_genus,]
# temp$Family_Genus <- with(temp, paste0(Family, ";", Genus))
# myplot <- generate_multiple_abundance_boxplot(temp, "Family_Genus", "Sampletype_compromised_refined_both_projects","Relative_abundance_rarefied", add_points = F, variable_colours_available = T) + 
#   geom_point(position = position_jitterdodge(), size = .3,stroke = 0.1, shape = 21,aes(fill = Sampletype_compromised_refined_both_projects)) +
#   # scale_y_continuous(limits = c(-1,1)) +
#   facet_wrap(~Project) +
#   ylab("Relative abundance") + 
#   xlab("Taxa") +
#   theme(axis.text.y = element_text(size =6),
#         axis.text.x = element_text(size =6),
#         axis.title = element_text(size =6))
# ggsave(plot = myplot,
#        filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Project_Sampletype_compromised_refined_both_projects_top_taxa.pdf"),
#        height = 25,
#        width = 20,
#        units = "cm")


# Make individual boxplots for the top taxa
for (project in c("immunocompromised")){
  for (genus in project_genus_summary_top.df$taxonomy_genus){
    data_subset <- subset(modified_genus_data.df, taxonomy_genus == genus & Project == project)
    base_name <- gsub(".*(f__.*)", "\\1",genus)
    myplot <- generate_abundance_boxplot(data_subset, variable = "Sampletype_compromised_refined", metric = "Relative_abundance_rarefied") +
      ylab("Relative abundance") +
      scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
      labs(title = project,
           subtitle = base_name)
    if (nchar(base_name) > 50){
      myplot <- myplot + theme(plot.subtitle = element_text(size = 3.5, hjust = 0.5))
    } else{
      myplot <- myplot + theme(plot.subtitle = element_text(size = 6, hjust = 0.5))
    }
    # ggtitle(paste0(project, "\n", base_name))
    
    ggsave(plot = myplot,
           filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Project_sampletype_compromised_refined/", project, "_genus__",base_name, ".pdf"),
           height = 8,
           width = 8,
           units = "cm")
  }
}



# ------------------------------------------
# Patient_group for immunocompromised
immunocompromised_patient_group_genus_summary.df <- generate_taxa_summary(subset(genus_data.df, Project == "immunocompromised"),
                                                                          taxa_column = "taxonomy_genus", 
                                                                          group_by_columns = c("Patient_group"))
immunocompromised_patient_group_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary =  immunocompromised_patient_group_genus_summary.df, 
                                                                          grouping_variables = c("Patient_group"), 
                                                                          abundance_column = "Mean_relative_abundance_rarefied", 
                                                                          my_top_n = 9)

# immunocompromised
immunocompromised_patient_group_genus_top_significances.df <- calculate_abundance_significance(mydata = subset(genus_data.df, Project == "immunocompromised" & 
                                                                                                                 taxonomy_genus %in% immunocompromised_patient_group_genus_summary_top.df$taxonomy_genus), 
                                                                                         variable = "Patient_group", taxonomy_column = "taxonomy_genus",metric = "Relative_abundance_rarefied")
immunocompromised_patient_group_genus_top_significances.df$Project <- "immunocompromised"
write.csv(immunocompromised_patient_group_genus_top_significances.df,"Result_tables/abundance_analysis_tables/immunocompromised_patient_group__genus_wilcox.csv", row.names = F)


# Make group boxplot for top taxa
temp <- subset(genus_data.df, Project == "immunocompromised")
temp <- temp[temp$taxonomy_genus %in% immunocompromised_patient_group_genus_summary_top.df$taxonomy_genus,]
temp$Family_Genus <- with(temp, paste0(Family, ";", Genus))
myplot <- generate_multiple_abundance_boxplot(temp, "Family_Genus", "Patient_group","Relative_abundance_rarefied", add_points = F) + 
  geom_point(position = position_jitterdodge(), size = .3,stroke = 0.1, shape = 21,aes(fill = Patient_group)) +
  ylab("Relative abundance") + 
  xlab("Taxa") +
  theme(axis.text.y = element_text(size =6),
        axis.text.x = element_text(size =6),
        axis.title = element_text(size =6))
ggsave(plot = myplot,
       filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Patient_group_top_taxa.pdf"),
       height = 15,
       width = 16,
       units = "cm")


# Make individual boxplots for the top taxa
for (genus in immunocompromised_patient_group_genus_summary_top.df$taxonomy_genus){
  data_subset <- subset(genus_data.df, Project == "immunocompromised" & taxonomy_genus == genus)
  base_name <- gsub(".*(f__.*)", "\\1",genus)
  myplot <- generate_abundance_boxplot(data_subset, variable = "Patient_group", metric = "Relative_abundance_rarefied") +
    ylab("Relative abundance") +
    scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
    labs(title = paste0("Patient group"),
         subtitle = base_name)
  if (nchar(base_name) > 50){
    myplot <- myplot + theme(plot.subtitle = element_text(size = 3.5, hjust = 0.5))
  } else{
    myplot <- myplot + theme(plot.subtitle = element_text(size = 6, hjust = 0.5))
  }
  ggsave(plot = myplot,
         filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Patient_group/patient_group_genus__",base_name, ".pdf"),
         height = 8,
         width = 8,
         units = "cm")
}
# ------------------------------------------
# Number_of_meds for immunocompromised
immunocompromised_number_of_meds_genus_summary.df <- generate_taxa_summary(subset(genus_data.df, Project == "immunocompromised"),
                                                                          taxa_column = "taxonomy_genus", 
                                                                          group_by_columns = c("Number_of_meds"))
immunocompromised_number_of_meds_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary =  immunocompromised_number_of_meds_genus_summary.df, 
                                                                                grouping_variables = c("Number_of_meds"), 
                                                                                abundance_column = "Mean_relative_abundance_rarefied", 
                                                                                my_top_n = 9)

# Calculate abundance significance
immunocompromised_number_of_meds_genus_top_significances.df <- calculate_abundance_significance(mydata = subset(genus_data.df, Project == "immunocompromised" & taxonomy_genus %in% immunocompromised_number_of_meds_genus_summary_top.df$taxonomy_genus), 
                                                                                               variable = "Number_of_meds", taxonomy_column = "taxonomy_genus",metric = "Relative_abundance_rarefied")
immunocompromised_number_of_meds_genus_top_significances.df$Project <- "immunocompromised"
write.csv(immunocompromised_number_of_meds_genus_top_significances.df,"Result_tables/abundance_analysis_tables/immunocompromised_Number_of_meds__genus_wilcox.csv", row.names = F)


# Make group boxplot for top taxa
temp <- subset(genus_data.df, Project == "immunocompromised")
temp <- temp[temp$taxonomy_genus %in% immunocompromised_number_of_meds_genus_summary_top.df$taxonomy_genus,]
temp$Family_Genus <- with(temp, paste0(Family, ";", Genus))
myplot <- generate_multiple_abundance_boxplot(temp, "Family_Genus", "Number_of_meds","Relative_abundance_rarefied", add_points = F) + 
  geom_point(position = position_jitterdodge(), size = .3,stroke = 0.1, shape = 21,aes(fill = Number_of_meds)) +
  ylab("Relative abundance") + 
  xlab("Taxa") +
  theme(axis.text.y = element_text(size =6),
        axis.text.x = element_text(size =6),
        axis.title = element_text(size =6))
ggsave(plot = myplot,
       filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Number_of_meds_top_taxa.pdf"),
       height = 15,
       width = 16,
       units = "cm")


# Make individual boxplots for the top taxa
for (genus in immunocompromised_number_of_meds_genus_summary_top.df$taxonomy_genus){
  data_subset <- subset(genus_data.df, Project == "immunocompromised" & taxonomy_genus == genus)
  base_name <- gsub(".*(f__.*)", "\\1",genus)
  myplot <- generate_abundance_boxplot(data_subset, variable = "Number_of_meds", metric = "Relative_abundance_rarefied") +
    ylab("Relative abundance") +
    scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
    labs(title = paste0("Number of meds"),
         subtitle = base_name)
  if (nchar(base_name) > 50){
    myplot <- myplot + theme(plot.subtitle = element_text(size = 3.5, hjust = 0.5))
  } else{
    myplot <- myplot + theme(plot.subtitle = element_text(size = 6, hjust = 0.5))
  }
  ggsave(plot = myplot,
         filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Number_of_meds/Number_of_meds_genus__",base_name, ".pdf"),
         height = 8,
         width = 8,
         units = "cm")
}

# ------------------------------------------
# Fitzpatrick_skin_type for immunocompromised
immunocompromised_skin_type_genus_summary.df <- generate_taxa_summary(subset(genus_data.df, Project == "immunocompromised"),
                                                                           taxa_column = "taxonomy_genus", 
                                                                           group_by_columns = c("Fitzpatrick_skin_type"))
immunocompromised_skin_type_genus_summary_top.df <- filter_summary_to_top_n(taxa_summary =  immunocompromised_skin_type_genus_summary.df, 
                                                                                 grouping_variables = c("Fitzpatrick_skin_type"), 
                                                                                 abundance_column = "Mean_relative_abundance_rarefied", 
                                                                                 my_top_n = 9)
# Calculate abundance significance
immunocompromised_skin_type_genus_top_significances.df <- calculate_abundance_significance(mydata = subset(genus_data.df, Project == "immunocompromised" & taxonomy_genus %in% immunocompromised_skin_type_genus_summary_top.df$taxonomy_genus), 
                                                                                                variable = "Fitzpatrick_skin_type", taxonomy_column = "taxonomy_genus",metric = "Relative_abundance_rarefied")
immunocompromised_skin_type_genus_top_significances.df$Project <- "immunocompromised"
write.csv(immunocompromised_skin_type_genus_top_significances.df,"Result_tables/abundance_analysis_tables/immunocompromised_Fitzpatrick_skin_type__genus_wilcox.csv", row.names = F)

# Make group boxplot for top taxa
temp <- subset(genus_data.df, Project == "immunocompromised")
temp <- temp[temp$taxonomy_genus %in% immunocompromised_skin_type_genus_summary_top.df$taxonomy_genus,]
temp$Family_Genus <- with(temp, paste0(Family, ";", Genus))
myplot <- generate_multiple_abundance_boxplot(temp, "Family_Genus", "Fitzpatrick_skin_type","Relative_abundance_rarefied", add_points = F) + 
  geom_point(position = position_jitterdodge(dodge.width = .75), size = .3,stroke = 0.1, shape = 21,aes(fill = Fitzpatrick_skin_type)) +
  ylab("Relative abundance") + 
  xlab("Taxa") +
  theme(axis.text.y = element_text(size =6),
        axis.text.x = element_text(size =6),
        axis.title = element_text(size =6))
ggsave(plot = myplot,
       filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Fitzpatrick_skin_type_top_taxa.pdf"),
       height = 15,
       width = 16,
       units = "cm")


# Make individual boxplots for the top taxa
for (genus in immunocompromised_number_of_meds_genus_summary_top.df$taxonomy_genus){
  data_subset <- subset(genus_data.df, Project == "immunocompromised" & taxonomy_genus == genus)
  base_name <- gsub(".*(f__.*)", "\\1",genus)
  myplot <- generate_abundance_boxplot(data_subset, variable = "Fitzpatrick_skin_type", metric = "Relative_abundance_rarefied") +
    ylab("Relative abundance") +
    scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
    labs(title = paste0("Fitzpatrick skin type"),
         subtitle = base_name)
  if (nchar(base_name) > 50){
    myplot <- myplot + theme(plot.subtitle = element_text(size = 3.5, hjust = 0.5))
  } else{
    myplot <- myplot + theme(plot.subtitle = element_text(size = 6, hjust = 0.5))
  }
  ggsave(plot = myplot,
         filename = paste0("Result_figures/abundance_analysis_plots/boxplots/Fitzpatrick_skin_type/Fitzpatrick_skin_type_genus__",base_name, ".pdf"),
         height = 8,
         width = 8,
         units = "cm")
}














# otu_cohort_sampletype_summary <- generate_taxa_summary(otu_data.df, "OTU.ID", group_by_columns = c("Project", "Sampletype_pooled"))
# otu_cohort_sampletype_summary$taxonomy_species <- as.character(unlist(lapply(as.character(otu_cohort_sampletype_summary$OTU.ID), function(x) otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID == x,]$taxonomy_species)))

# Write sample type summaries to file
# write.csv(genus_cohort_sampletype_summary,file = "Result_tables/abundance_analysis_tables/genus_cohort_sampletype_taxa_summary.csv", row.names = F, quote = F)

# --------------------------


genus_cohort_sample_type_top_10 <- filter_summary_to_top_n(taxa_summary = genus_cohort_sampletype_summary,
                                                                                  grouping_variables = c("Project", "Sampletype_pooled"), 
                                                                                  abundance_column = "Mean_relative_abundance_rarefied",
                                                                                    my_top_n = 10)

write.csv(genus_cohort_sample_type_top_10,file = "Result_tables/abundance_analysis_tables/genus_cohort_sampletype_taxa_summary_top_10_mean_relative_abundance.csv", row.names = F, quote = F)


# ------------------------------------------------------------------------------------------------------------
# Function to generate a more simplified summary for any group(s) (ignore taxa).
# No need to calculate number/percent samples in group.
generate_summary <- function(mydata, group_by_columns = c("Sample_Type")){
  select_columns <- c(group_by_columns, "Sample", "Read_count", "Read_count_rarefied", "Relative_abundance", "Relative_abundance_rarefied")
  total_samples <- length(unique(mydata$Sample))
  group_summary <- 
    mydata %>%
    # dplyr::filter(retained = "yes") %>% # keep only those samples that were retained
    dplyr::select_(.dots = select_columns) %>%
    dplyr::group_by_(.dots = c(group_by_columns)) %>%
    dplyr::mutate(N_samples = n_distinct(Sample)) %>% # number of unique samples/index
    dplyr::select(-Sample) %>%
    dplyr::summarise(N_samples = max(N_samples),
                     Percent_total_samples = round((max(N_samples) / total_samples)*100, 2),
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
                     Summed_relative_abundance_rarefied = round(sum(Relative_abundance_rarefied),5)
    ) %>%
    as.data.frame()
  return(group_summary)
}

# Calculate the read count total for each site for UC, CD and CONTROL samples
cohort_sampletype_summary.df <- generate_summary(phylum_data.df, group_by_columns = c("Project","Sampletype_pooled"))

# Save 
write.csv(cohort_sampletype_summary.df,file = "Result_tables/abundance_analysis_tables/cohort_sampletype_summary.csv", row.names = F, quote = F)

# Get the colour palette for the sample sites
sample_type_colours.df <- unique(phylum_data.df[,c("Sampletype_pooled","Sampletype_pooled_colour")])
sample_type_colours.df <- setNames( as.character(sample_type_colours.df$Sampletype_pooled_colour), as.character(sample_type_colours.df$Sampletype_pooled))

# Plot the mean read count for each sample site as a bar graph
mean_read_count_bar_graph <- ggplot(cohort_sampletype_summary.df, aes(x = Sampletype_pooled, y = Mean_read_count)) +
  geom_bar(position = "dodge", stat= "identity", aes(fill = Sampletype_pooled), color = "black",size =.2) +
  geom_text(aes(label = paste0("n = ",N_samples)),vjust = -0.6,size = 3) +
  # geom_text(aes(label = count, x = type, y = count), position = position_dodge(width = 0.8), vjust = -0.6
  scale_fill_manual(values = sample_type_colours.df, guide = F) +
  xlab("Sample type") +
  ylab("Mean read count") +
  scale_y_continuous(breaks = seq(0,4000, 500), limits = c(0,3501)) + facet_wrap(~Project)
mean_read_count_bar_graph
ggsave(plot = mean_read_count_bar_graph, filename ="Result_figures/abundance_analysis_plots/cohort_sampletype_mean_read_count_bar_graph.pdf", width = 10, height = 5)

# Plot the summed read count for each sample site as a bar graph
summed_read_count_bar_graph <- ggplot(cohort_sampletype_summary.df, aes(x = Sampletype_pooled, y = Summed_read_count_rarefied)) +
  geom_bar(position = "dodge", stat= "identity", aes(fill = Sampletype_pooled), color = "black",size =.2) +
  geom_text(aes(label = paste0("n = ",N_samples)),vjust = -0.6,size = 3) +
  # geom_text(aes(label = count, x = type, y = count), position = position_dodge(width = 0.8), vjust = -0.6
  scale_fill_manual(values = sample_type_colours.df, guide = F) +
  xlab("Sample type") +
  ylab("Summed read count (rarefied)") + facet_wrap(~Project)
  # scale_y_continuous(breaks = seq(0,4000, 500), limits = c(0,3501))
summed_read_count_bar_graph
ggsave(plot = summed_read_count_bar_graph, filename ="Result_figures/abundance_analysis_plots/cohort_sampletype_summed_read_count_bar_graph.pdf", width = 10, height = 5)


mean_relative_abundaunce_bar_graph <- ggplot(cohort_sampletype_summary.df, aes(x = Sampletype_pooled, y = Mean_relative_abundance_rarefied)) +
  geom_bar(position = "dodge", stat= "identity", aes(fill = Sampletype_pooled), color = "black",size =.2) +
  geom_text(aes(label = paste0("n = ",N_samples)),vjust = -0.6,size = 3) +
  # geom_text(aes(label = count, x = type, y = count), position = position_dodge(width = 0.8), vjust = -0.6
  scale_fill_manual(values = sample_type_colours.df, guide = F) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  xlab("Sample type") +
  ylab("Mean relative abundance (rarefied)") + facet_wrap(~Project)
  # common_theme
# scale_y_continuous(breaks = seq(0,4000, 500), limits = c(0,3501))
mean_relative_abundaunce_bar_graph
ggsave(plot = mean_relative_abundaunce_bar_graph, filename ="Result_figures/abundance_analysis_plots/cohort_sampletype_mean_relative_abundance_bar_graph.pdf", width = 10, height = 5)





# ------------------------------------------------------------------------------------------------------------
# Calculate the prevalences etc. of each taxa, not grouped

# phylum_summary <- generate_summary(phylum_data.df,group_by_columns = c("taxonomy_phylum"))
# class_summary <- generate_summary(class_data.df,group_by_columns = c("taxonomy_class"))
# order_summary <- generate_summary(order_data.df,group_by_columns = c("taxonomy_order"))
# family_summary <- generate_summary(family_data.df,group_by_columns = c("taxonomy_family"))
# genus_summary <- generate_summary(genus_data.df,group_by_columns = c("taxonomy_genus"))

# --------------------------------------------------------------------------
### Identify the core taxa for each group and make a venn diagram

# Core taxa should be, for example, >=90% prevalent in the respective group

# ----------------------------------------------------
##  Core taxa within each sample type, limited to UC, CONTROL and CD samples

# Genus level, filtered to high prevalence
genus_sample_type_CD_UC_control_summary_high_prev <- genus_sample_type_CD_UC_control_summary[genus_sample_type_CD_UC_control_summary$Percent_group_samples >= 50,]
core_genus_sample_type_CD_UC_control_summary_high_prev <- as.character(unique(genus_sample_type_CD_UC_control_summary_high_prev$taxonomy_genus))

# OTU level, filtered to high prevalence
otu_sample_type_CD_UC_control_summary_high_prev <- otu_sample_type_CD_UC_control_summary[otu_sample_type_CD_UC_control_summary$Percent_group_samples >= 50,]
core_otu_sample_type_CD_UC_control_summary_high_prev <- as.character(unique(otu_sample_type_CD_UC_control_summary_high_prev$OTU.ID))

# And by disease state
genus_sample_type_dx_group_CD_UC_control_summary_high_prev <- genus_sample_type_dx_group_CD_UC_control_summary[genus_sample_type_dx_group_CD_UC_control_summary$Percent_group_samples >= 50,]
otu_sample_type_dx_group_CD_UC_control_summary_high_prev <- otu_sample_type_dx_group_CD_UC_control_summary[otu_sample_type_dx_group_CD_UC_control_summary$Percent_group_samples >= 50,]

# The not filtered summaries are : otu_sample_type_CD_UC_control_summary and genus_sample_type_CD_UC_control_summary
# ---------------------------------------------------------------
# Make a venn diagram for each pair/triplet of groups

# First for each pair of sample types
colour_list <- unique(genus_data.df[,c("Sample_Type","Sample_Type_colour")])
colour_list <- setNames(as.character(colour_list[,2]), as.character(colour_list[,1]))

generate_sample_type_venn_plots <- function(summary_table,taxonomy_column, output_dir, prefix = ""){
  
  # The combined set of taxa from all groups
  all_taxa  <- as.character(unique(summary_table[,taxonomy_column]))
  
  # Get the combinations for all sample types
  sample_type_combinations <- combn(as.character(unique(summary_table$Sample_Type)),2)
  shared_results.df <- data.frame()
  for (i in 1:ncol(sample_type_combinations)){
    grid.newpage()
    # Set the two groups to compare
    group_1 <- sample_type_combinations[1,i]
    group_2 <- sample_type_combinations[2,i]
    # Get the taxa for both groups
    group_1_taxa <- as.character(summary_table[summary_table$Sample_Type == group_1,taxonomy_column])
    group_2_taxa <- as.character(summary_table[summary_table$Sample_Type == group_2,taxonomy_column])
    # Number of taxa from combined set in each group
    # same as length(group_1_taxa)
    N_taxa_group_1 <- sum(all_taxa %in% group_1_taxa, na.rm = T)
    N_taxa_group_2 <- sum(all_taxa %in% group_2_taxa, na.rm = T)
    # Number of taxa shared by both groups
    shared_taxa <- intersect(group_1_taxa, group_2_taxa)
    N_shared <- length(shared_taxa)
    for (shared_taxa_entry in shared_taxa){
      if (taxonomy_column == "OTU.ID"){
        tax_string <- as.character(summary_table[summary_table$OTU.ID == shared_taxa_entry,"taxonomy_species"][1])
        shared_results.df <- unique(rbind(shared_results.df, data.frame(group_1, group_2, shared_taxa_entry, tax_string)))
      } else{
        shared_results.df <- unique(rbind(shared_results.df, data.frame(group_1, group_2, shared_taxa_entry)))  
      }
      
    }
    pdf(file = paste0(output_dir, prefix, group_1, "_vs_", group_2, ".pdf"),
        height = 5,
        width = 5)
    # Venn diagram orders the sets by their size. So we need to flip the labels/colours to match
    if (N_taxa_group_2 < N_taxa_group_1){
      draw.pairwise.venn(
        area1 = N_taxa_group_1, 
        area2 = N_taxa_group_2,
        cross.area = N_shared,
        category = c(group_1, group_2),
        fill = as.character(c(colour_list[group_1], colour_list[group_2])),
        euler.d = F,
        scaled = F,
      )
    } else{
      draw.pairwise.venn(
        area1 = N_taxa_group_2, 
        area2 = N_taxa_group_1,
        cross.area = N_shared,
        category = c(group_2,group_1),
        fill = rev(as.character(c(colour_list[group_1], colour_list[group_2]))),
        euler.d = F,
        scaled = F,
      )
    }
    dev.off()
  }
  return(shared_results.df)
}

# Genus level, high prevalence
genus_sample_type_high_prev_shared <- generate_sample_type_venn_plots(genus_sample_type_CD_UC_control_summary_high_prev, 
                                "taxonomy_genus",
                                output_dir = "Result_figures/abundance_analysis_plots/paper_UC_CD_CONTROL/venn_diagrams/genus/",
                                prefix = "genus_prevalence_50pc_sample_type__CD_UC_CONTROL__")
# OTU level, high prevalence
otu_sample_type_high_prev_shared <- generate_sample_type_venn_plots(otu_sample_type_CD_UC_control_summary_high_prev, 
                                "OTU.ID",
                                output_dir = "Result_figures/abundance_analysis_plots/paper_UC_CD_CONTROL/venn_diagrams/otu/",
                                prefix = "otu_prevalence_50pc_sample_type__CD_UC_CONTROL__")

# Genus level, all  
genus_sample_type_shared <- generate_sample_type_venn_plots(genus_sample_type_CD_UC_control_summary, 
                                                          "taxonomy_genus",
                                                          output_dir = "Result_figures/abundance_analysis_plots/paper_UC_CD_CONTROL/venn_diagrams/genus/",
                                                          prefix = "genus_all_sample_type__CD_UC_CONTROL__")

# OTU level, all
otu_sample_type_shared <- generate_sample_type_venn_plots(otu_sample_type_CD_UC_control_summary, 
                                "OTU.ID",
                                output_dir = "Result_figures/abundance_analysis_plots/paper_UC_CD_CONTROL/venn_diagrams/otu/",
                                prefix = "otu_all_sample_type__CD_UC_CONTROL__")

# Save the tables describing the shared taxa
write.csv(genus_sample_type_high_prev_shared, file = "Result_tables/abundance_analysis_tables/paper_UC_CD_CONTROL/genus_sample_type_shared_taxa_high_prev.csv",quote = F, row.names = F)
write.csv(genus_sample_type_shared, file = "Result_tables/abundance_analysis_tables/paper_UC_CD_CONTROL/genus_sample_type_shared_taxa.csv",quote = F, row.names = F)
write.csv(otu_sample_type_high_prev_shared, file = "Result_tables/abundance_analysis_tables/paper_UC_CD_CONTROL/otu_sample_type_shared_taxa_high_prev.csv",quote = F, row.names = F)
write.csv(otu_sample_type_shared, file = "Result_tables/abundance_analysis_tables/paper_UC_CD_CONTROL/otu_sample_type_shared_taxa.csv",quote = F, row.names = F)

# ------------------------------------------------------------------------------------
# Now we want to do the same for disease groups. Since we only have three disease groups in the current analysis,
# specifically UC, CD and Control, we can make a triplet plot.

# Function assumes the input summary table has been reduced to a single sample type 
# (this could be done in the function)
colour_list <- unique(genus_data.df[,c("DX_Groups","DX_Groups_colour")])
colour_list <- setNames(as.character(colour_list[,2]), as.character(colour_list[,1]))

generate_sample_type_dx_groups_venn_plots <- function(summary_table, taxonomy_column, output_dir, prefix = ""){
  
  for (st in unique(summary_table$Sample_Type)){
    grid.newpage()
    print(st)
    temp <- subset(summary_table, Sample_Type == st)
    all_taxa  <- as.character(unique(temp[,taxonomy_column]))
    group_1 <- "CD"
    group_2 <- "CONTROL"
    group_3 <- "UC"
    
    pdf(file = paste0(output_dir, prefix,st, "__", group_1, "_vs_", group_2, "_vs_", group_3, ".pdf"),
        height = 5,
        width = 5)
    
    group_1_taxa <- as.character(temp[temp$DX_Groups == group_1,taxonomy_column])
    group_2_taxa <- as.character(temp[temp$DX_Groups == group_2,taxonomy_column])
    group_3_taxa <- as.character(temp[temp$DX_Groups == group_3,taxonomy_column])
    
    N_taxa_group_1 <- sum(all_taxa %in% group_1_taxa, na.rm = T) # 33, CD
    N_taxa_group_2 <- sum(all_taxa %in% group_2_taxa, na.rm = T) # 23, CONTROL
    N_taxa_group_3 <- sum(all_taxa %in% group_3_taxa, na.rm = T) # 22, UC
    
    N_taxa_group_1_in_group_2 <- sum(group_1_taxa %in% group_2_taxa)
    N_taxa_group_1_in_group_3 <- sum(group_1_taxa %in% group_3_taxa)
    N_taxa_group_2_in_group_3 <- sum(group_2_taxa %in% group_3_taxa)
    
    shared_taxa <- Reduce(intersect, list(group_1_taxa,group_2_taxa,group_3_taxa))
    N_shared <- length(shared_taxa)
    
    draw.triple.venn(
      area1 = N_taxa_group_1,
      area2 = N_taxa_group_2,
      area3 = N_taxa_group_3,
      n12 = N_taxa_group_1_in_group_2,
      n23 = N_taxa_group_2_in_group_3,
      n13 = N_taxa_group_1_in_group_3,
      n123 = N_shared,
      category = c(group_1,group_2, group_3),
      fill = as.character(c(colour_list[group_1], colour_list[group_2], colour_list[group_3]))
    )
    dev.off()
  }
}
generate_sample_type_dx_groups_venn_plots(genus_sample_type_dx_group_CD_UC_control_summary,
                                          "taxonomy_genus",
                                          output_dir = "Result_figures/abundance_analysis_plots/paper_UC_CD_CONTROL/venn_diagrams/genus_within_sample_type/",
                                          prefix = "genus_all_sample_type_")

generate_sample_type_dx_groups_venn_plots(genus_sample_type_dx_group_CD_UC_control_summary_high_prev,
                                          "taxonomy_genus",
                                          output_dir = "Result_figures/abundance_analysis_plots/paper_UC_CD_CONTROL/venn_diagrams/genus_within_sample_type/",
                                          prefix = "genus_prevalence_50pc_sample_type_")

generate_sample_type_dx_groups_venn_plots(otu_sample_type_dx_group_CD_UC_control_summary,
                                          "OTU.ID",
                                          output_dir = "Result_figures/abundance_analysis_plots/paper_UC_CD_CONTROL/venn_diagrams/otu_within_sample_type/",
                                          prefix = "otu_all_sample_type_")

generate_sample_type_dx_groups_venn_plots(otu_sample_type_dx_group_CD_UC_control_summary_high_prev,
                                          "OTU.ID",
                                          output_dir = "Result_figures/abundance_analysis_plots/paper_UC_CD_CONTROL/venn_diagrams/otu_within_sample_type/",
                                          prefix = "otu_prevalence_50pc_sample_type_")


########################
# testing

# temp <- subset(genus_sample_type_dx_group_CD_UC_control_summary_high_prev, Sample_Type == "DU")
# summary_table <- temp
# taxonomy_column <- "taxonomy_genus"
# 
# all_taxa  <- as.character(unique(summary_table[,taxonomy_column]))
# group_1 <- "CD"
# group_2 <- "CONTROL"
# group_3 <- "UC"
# 
# group_1_taxa <- as.character(summary_table[summary_table$DX_Groups == group_1,taxonomy_column])
# group_2_taxa <- as.character(summary_table[summary_table$DX_Groups == group_2,taxonomy_column])
# group_3_taxa <- as.character(summary_table[summary_table$DX_Groups == group_3,taxonomy_column])
# 
# N_taxa_group_1 <- sum(all_taxa %in% group_1_taxa, na.rm = T) # 33, CD
# N_taxa_group_2 <- sum(all_taxa %in% group_2_taxa, na.rm = T) # 23, CONTROL
# N_taxa_group_3 <- sum(all_taxa %in% group_3_taxa, na.rm = T) # 22, UC
# 
# N_taxa_group_1_in_group_2 <- sum(group_1_taxa %in% group_2_taxa)
# N_taxa_group_1_in_group_3 <- sum(group_1_taxa %in% group_3_taxa)
# N_taxa_group_2_in_group_3 <- sum(group_2_taxa %in% group_3_taxa)
# 
# shared_taxa <- Reduce(intersect, list(group_1_taxa,group_2_taxa,group_3_taxa))
# N_shared <- length(shared_taxa)
# # middle is highest
# # left second highest
# # right lowest
# grid.newpage()
# draw.triple.venn(
#   area1 = N_taxa_group_1,
#   area2 = N_taxa_group_2,
#   area3 = N_taxa_group_3,
#   n12 = N_taxa_group_1_in_group_2,
#   n23 = N_taxa_group_2_in_group_3,
#   n13 = N_taxa_group_1_in_group_3,
#   n123 = N_shared,
#   category = c(group_1,group_2, group_3),
#   fill = as.character(c(colour_list[group_1], colour_list[group_2], colour_list[group_3]))
# )
########################

# --------------------------------------------------------------------------------

# Core each sample type
# core_DNS <- as.character(genus_abundances_sample_type_filtered[genus_abundances_sample_type_filtered$Sample_Type == "DNS","taxonomy_genus"])
# core_DU <- as.character(genus_abundances_sample_type_filtered[genus_abundances_sample_type_filtered$Sample_Type == "DU","taxonomy_genus"])
# core_G <- as.character(genus_abundances_sample_type_filtered[genus_abundances_sample_type_filtered$Sample_Type == "G","taxonomy_genus"])
# core_GNS <- as.character(genus_abundances_sample_type_filtered[genus_abundances_sample_type_filtered$Sample_Type == "GNS","taxonomy_genus"])
# core_O <- as.character(genus_abundances_sample_type_filtered[genus_abundances_sample_type_filtered$Sample_Type == "O","taxonomy_genus"])
# core_R <- as.character(genus_abundances_sample_type_filtered[genus_abundances_sample_type_filtered$Sample_Type == "R","taxonomy_genus"])
# core_RC <- as.character(genus_abundances_sample_type_filtered[genus_abundances_sample_type_filtered$Sample_Type == "RC","taxonomy_genus"])
# core_TI <- as.character(genus_abundances_sample_type_filtered[genus_abundances_sample_type_filtered$Sample_Type == "TI","taxonomy_genus"])

# N core taxa in each sample type
# DNS,DU,G,GNS,O,R,RC,TI
# N_core_DNS <- sum(core_taxa_sample_type_filtered_all %in% core_DNS, na.rm = T)
# N_core_DU <- sum(core_taxa_sample_type_filtered_all %in% core_DU, na.rm = T)
# N_core_G <- sum(core_taxa_sample_type_filtered_all %in% core_G, na.rm = T)
# N_core_GNS <- sum(core_taxa_sample_type_filtered_all %in% core_GNS, na.rm = T)
# N_core_O <- sum(core_taxa_sample_type_filtered_all %in% core_O, na.rm = T)
# N_core_R <- sum(core_taxa_sample_type_filtered_all %in% core_R, na.rm = T)
# N_core_RC <- sum(core_taxa_sample_type_filtered_all %in% core_RC, na.rm = T)
# N_core_TI <- sum(core_taxa_sample_type_filtered_all %in% core_TI, na.rm = T)



# shared_core_pairs.df <- data.frame()
# sample_type_combinations <- combn(as.character(unique(genus_abundances_sample_type_filtered$Sample_Type)),2)
# for (i in 1:ncol(sample_type_combinations)){
#   group_1 <- sample_type_combinations[1,i]
#   group_2 <- sample_type_combinations[2,i]
#   core_group_1 <- as.character(genus_abundances_sample_type_filtered[genus_abundances_sample_type_filtered$Sample_Type == group_1,"taxonomy_genus"])
#   core_group_2 <- as.character(genus_abundances_sample_type_filtered[genus_abundances_sample_type_filtered$Sample_Type == group_2,"taxonomy_genus"])
#   N_shared <- length(intersect(core_group_1, core_group_2))
#   shared_core_pairs.df <- rbind(shared_core_pairs.df, data.frame(group_1, group_2, N_shared)) 
# }
# temp <- data.frame(shared_core_pairs.df$group_2, shared_core_pairs.df$group_1, shared_core_pairs.df$N_shared)
# names(temp) <- c("group_1", "group_2", "N_shared")
# shared_core_pairs.df <- bind_rows(shared_core_pairs.df, temp)


# ---------------------------------------------------------------
# Make a 5-way venn diagram, just to test


# grid.newpage()
# draw.quintuple.venn(area1 = N_core_DU,
#                     area2 = N_core_G,
#                     area3 = N_core_O,
#                     area4 = N_core_R,
#                     area5 = N_core_TI,
#                     
#                     n12 = subset(shared_core_pairs.df, group_1 == "DU" & group_2 == "G")$N_shared,
#                     n13 = subset(shared_core_pairs.df, group_1 == "DU" & group_2 == "O")$N_shared,
#                     n14 = subset(shared_core_pairs.df, group_1 == "DU" & group_2 == "R")$N_shared,
#                     n15 = subset(shared_core_pairs.df, group_1 == "DU" & group_2 == "TI")$N_shared,
#                     
#                     n23 = subset(shared_core_pairs.df, group_1 == "G" & group_2 == "O")$N_shared,
#                     n24 = subset(shared_core_pairs.df, group_1 == "G" & group_2 == "R")$N_shared,
#                     n25 = subset(shared_core_pairs.df, group_1 == "G" & group_2 == "TI")$N_shared,
#                     
#                     n34 = subset(shared_core_pairs.df, group_1 == "O" & group_2 == "R")$N_shared,
#                     n35 = subset(shared_core_pairs.df, group_1 == "O" & group_2 == "TI")$N_shared,
#                     
#                     n45 = subset(shared_core_pairs.df, group_1 == "R" & group_2 == "TI")$N_shared,
#                     
#                     n123 = length(Reduce(intersect, list(core_DU,core_G,core_O))),
#                     n124 = length(Reduce(intersect, list(core_DU,core_G,core_R))),
#                     n125 = length(Reduce(intersect, list(core_DU,core_G,core_TI))),
#                     
#                     n134 = length(Reduce(intersect, list(core_DU,core_O,core_R))),
#                     n135 = length(Reduce(intersect, list(core_DU,core_O,core_TI))),
#                     
#                     n145 = length(Reduce(intersect, list(core_DU,core_R,core_TI))),
#                     
#                     n234 = length(Reduce(intersect, list(core_G,core_O,core_R))),
#                     n235 = length(Reduce(intersect, list(core_G,core_O,core_TI))),
#                     
#                     n245 = length(Reduce(intersect, list(core_G,core_R,core_TI))),
#                     
#                     n345 = length(Reduce(intersect, list(core_O,core_R,core_TI))),
#                     
#                     n1234 = length(Reduce(intersect, list(core_DU,core_G,core_O,core_R))),
#                     n1235 = length(Reduce(intersect, list(core_DU,core_G,core_O,core_TI))),
#                     n1245 = length(Reduce(intersect, list(core_DU,core_G,core_R,core_TI))),
#                     n1345 = length(Reduce(intersect, list(core_DU,core_O,core_R,core_TI))),
#                     n2345 = length(Reduce(intersect, list(core_G,core_O,core_R,core_TI))),
#                     n12345 = length(Reduce(intersect, list(core_DU,core_G,core_O,core_R,core_TI))),
#                     
#                     category = c("DU", "G", "O", "R", "TI"),
#                     fill = as.character(c(colour_list["DU"], colour_list["G"], colour_list["O"], colour_list["R"], colour_list["TI"]))
#                     # fill = unique(as.character(genus_data.df$Sample_Type_colour))[1:5]
# )
