# Analysis of staphyloccocus features that have had the species inferred
# from mutation analysis (inspection of alignment against reference database)

library(ggplot2)
# library(plyr)
library(dplyr)
library(tidyr)
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
  axis.text = element_text(size = 4, colour = "black"),
  axis.title = element_text(size = 5,face = "bold"),
  complete = F,
  plot.title = element_text(size = 8))

# ------------------------------------------------------------------------------------------
# Various colour palettes
my_colour_palette_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_palette_12_soft <-c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621", "#c75a93", "#7c7731")
my_colour_palette_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
# ------------------------------------------------------------------------------------------

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

################################################################################################
# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")

# Load specie inference
inferred_species_data.df <- read.table("data/Staphylococcus_otu_inferred_species.tsv", header = T, sep = "\t")
rownames(inferred_species_data.df) <- inferred_species_data.df$OTU.ID

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index


# Load OTU data
otu_data.df <- read.csv("Result_tables/other/OTU_counts_abundances_and_metadata.csv", header = T)

# Remove non-Staph
otu_data.df <- subset(otu_data.df, Genus == "g__Staphylococcus")

# Remove negative samples
otu_data.df <- otu_data.df[!otu_data.df$Sampletype == "negative",]

# Assign inferred species
otu_data.df$Inferred_species <- unlist(lapply(otu_data.df$OTU.ID, function(x) ifelse(x %in% inferred_species_data.df$OTU.ID,
                                              as.character(inferred_species_data.df[as.character(x),"Inferred_species"]), "Other Staphyloccocus")))

# Only want immunocompromised and the snapshot immunocompetent samples
otu_data.df <- subset(otu_data.df, Project == "immunocompromised" | Snapshot_sample_1 == "yes")

otu_data.df$Sampletype_final_refined <- factor(otu_data.df$Sampletype_final_refined, levels = rev(c("C", "C_P", "AK", "SCC_PL", "SCC")))

# Cohort specific data 
immunocompetent_data.df <- subset(otu_data.df, Project == "immunocompetent")
immunocompromised_data.df <- subset(otu_data.df, Project == "immunocompromised")

# immunocompromised_data.df <- immunocompromised_data.df[!is.na(immunocompromised_data.df$Bacterial_load_CFU),]

# What is the mean etc. abundance of each species in each lesion type
# basically what is the abundance of each species in each sample and then calculate the mean over the samples

# Get the abundances etc of each inferred species for each sample. The sum of the Summed_relative_abundance(_rarefied) values for each sample should equal 
# the abundance of g__Staphylococcus within the sample as a whole!
immunocompromised_taxa_summary.df <- generate_taxa_summary(mydata = immunocompromised_data.df, taxa_column = "Inferred_species",group_by_columns = c("Sampletype_final_refined","Sample"))
immunocompetent_taxa_summary.df <- generate_taxa_summary(mydata = immunocompetent_data.df, taxa_column = "Inferred_species",group_by_columns = c("Sampletype_final_refined","Sample"))

# Calculate the number of samples per group
generate_taxa_summary(mydata = immunocompromised_data.df, taxa_column = "Inferred_species",group_by_columns = c("Sampletype_final_refined","Sample", "Patient"))

immunocompromised_sample_counts_inferred_species.df <- generate_taxa_summary(mydata = immunocompromised_data.df, taxa_column = "Inferred_species",group_by_columns = c("Sampletype_final_refined","Sample", "Patient")) %>% 
  group_by(Sampletype_final_refined) %>%
  mutate(Total_number_of_samples_in_group = n_distinct(Sample), 
         Total_number_of_patients_in_group = n_distinct(Patient)) %>%
  group_by(Sampletype_final_refined,Inferred_species) %>% 
  summarise(Number_of_samples_present = n_distinct(Sample), 
            Number_of_patients_present = n_distinct(Patient),
            Total_number_of_samples_in_group = max(Total_number_of_samples_in_group),
            Total_number_of_patients_in_group = max(Total_number_of_patients_in_group)) %>%
  mutate(Percent_of_samples = round(Number_of_samples_present/Total_number_of_samples_in_group*100, 2), 
         Percent_of_patients = round(Number_of_patients_present/Total_number_of_patients_in_group*100, 2)) %>%
  as.data.frame()

immunocompetent_sample_counts_inferred_species.df <-  generate_taxa_summary(mydata = immunocompetent_data.df, taxa_column = "Inferred_species",group_by_columns = c("Sampletype_final_refined","Sample", "Patient")) %>% 
  group_by(Sampletype_final_refined) %>%
  mutate(Total_number_of_samples_in_group = n_distinct(Sample), 
         Total_number_of_patients_in_group = n_distinct(Patient)) %>%
  group_by(Sampletype_final_refined,Inferred_species) %>% 
  summarise(Number_of_samples_present = n_distinct(Sample), 
            Number_of_patients_present = n_distinct(Patient),
            Total_number_of_samples_in_group = max(Total_number_of_samples_in_group),
            Total_number_of_patients_in_group = max(Total_number_of_patients_in_group)) %>%
  mutate(Percent_of_samples = round(Number_of_samples_present/Total_number_of_samples_in_group*100, 2), 
         Percent_of_patients = round(Number_of_patients_present/Total_number_of_patients_in_group*100, 2)) %>%
  as.data.frame()

# Write per sample count summary to file
write.csv(immunocompromised_sample_counts_inferred_species.df, file = "Result_tables/abundance_analysis_tables/immunocompromised_inferred_species_sample_counts.csv")
write.csv(immunocompetent_sample_counts_inferred_species.df, file = "Result_tables/abundance_analysis_tables/immunocompetent_inferred_species_sample_counts.csv")

immunocompromised_taxa_summary.df <- merge(immunocompromised_taxa_summary.df, metadata.df[c("Index","Patient", "Project")], by.x =  "Sample", by.y = "Index")
immunocompetent_taxa_summary.df <- merge(immunocompetent_taxa_summary.df, metadata.df[c("Index","Patient", "Project")], by.x =  "Sample", by.y = "Index")


# Write per-sample summary to file 
write.csv(x = immunocompromised_taxa_summary.df %>% 
  select(-N_samples, -N_total_samples_in_group, -N_patients, -N_total_patients_in_group, -Percent_group_samples, -Percent_total_samples, -Percent_group_patients, -Percent_total_patients) %>% as.data.frame(),
  file = "Result_tables/abundance_analysis_tables/immunocompromised_inferred_species_summary.csv", quote = F, row.names = F)

write.csv(x = immunocompetent_taxa_summary.df %>% 
            select(-N_samples, -N_total_samples_in_group, -N_patients, -N_total_patients_in_group, -Percent_group_samples, -Percent_total_samples, -Percent_group_patients, -Percent_total_patients) %>% as.data.frame(),
          file = "Result_tables/abundance_analysis_tables/immunocompetent_inferred_species_summary.csv", quote = F, row.names = F)

# ----------------------------------------------------------------------------------------
# Make boxplots showing the abundance distribution each inferred species, NOT the distribution of individual features within each species group
# sum(subset(immunocompromised_taxa_summary.df, Sample == "SA6595_J1427")$Summed_relative_abundance_rarefied)
# temp <- immunocompromised_taxa_summary.df %>% group_by(Sampletype_final_refined, Sample) %>% summarise(Summed_relative_abundance_rarefied = sum(Summed_relative_abundance_rarefied))
# Re-assign colours
temp <- unique(immunocompromised_data.df[c("Sampletype_final_refined", "Sampletype_final_refined_colour")])
colour_palette <- setNames(as.character(temp$Sampletype_final_refined_colour), temp$Sampletype_final_refined)
immunocompromised_temp.df <- immunocompromised_taxa_summary.df
immunocompetent_temp.df <- immunocompetent_taxa_summary.df
immunocompromised_temp.df$Sampletype_final_refined <- factor(immunocompromised_temp.df$Sampletype_final_refined, levels = c("C", "C_P", "AK", "SCC_PL", "SCC"))
immunocompetent_temp.df$Sampletype_final_refined <- factor(immunocompetent_temp.df$Sampletype_final_refined, levels = c("C", "C_P", "AK", "SCC_PL", "SCC"))
immunocompromised_temp.df$Sampletype_final_refined_colour <- unlist(lapply(immunocompromised_temp.df$Sampletype_final_refined, function(x) colour_palette[[x]]))
immunocompetent_temp.df$Sampletype_final_refined_colour <- unlist(lapply(immunocompetent_temp.df$Sampletype_final_refined, function(x) colour_palette[[x]]))
unique(immunocompromised_temp.df[c("Sampletype_final_refined","Sampletype_final_refined_colour")])
unique(immunocompetent_temp.df[c("Sampletype_final_refined","Sampletype_final_refined_colour")])

# Use Summed_relative_abundance_rarefied as it will be the sum of the abundances of each feature within the inferred species groups
immunocompromised_temp.df$Summed_relative_abundance_rarefied <- round(immunocompromised_temp.df$Summed_relative_abundance_rarefied * 100, 3)
immunocompetent_temp.df$Summed_relative_abundance_rarefied <- round(immunocompetent_temp.df$Summed_relative_abundance_rarefied * 100, 3)

roundUp <- function(x, to = 10) {
  # myvalue <- 10^ceiling(log10(x))
  myvalue <- to*(x%/%to + as.logical(x%%to))
  # myvalue <- ifelse(myvalue < 10 & myvalue > 1, 10, ifelse(myvalue < 1, 1, myvalue))
  myvalue
}
roundUp(.1)


immunocompromised_data.df[immunocompromised_data.df$Sample == "R1431_J1425",]
immunocompromised_data.df[immunocompromised_data.df$Sample == "R1455_J1425",]

for (cohort in c("immunocompromised", "immunocompetent")){
  if (cohort == "immunocompromised"){
    data.df <- immunocompromised_temp.df
  } else{
    data.df <- immunocompetent_temp.df
  }
  for (IS in unique(data.df$Inferred_species)){
    data_subset <- subset(data.df, Inferred_species == IS)
    max_y_scale <- roundUp(max(data_subset$Summed_relative_abundance_rarefied))
    if (max(data_subset$Summed_relative_abundance_rarefied) < 1){
      max_y_scale <- roundUp(max(data_subset$Summed_relative_abundance_rarefied), to = 1)
    }
    
    mysteps = 10
    if (max_y_scale == 10) {
      mysteps = 1
    } else if(max_y_scale == 1){
      mysteps = .1
    } else if(max_y_scale > 10 & max_y_scale < 30){
      mysteps = 1
    }
    
    myplot <- generate_abundance_boxplot(data_subset,variable = "Sampletype_final_refined", metric = "Summed_relative_abundance_rarefied",variable_colours_available = T) +
      scale_y_continuous(breaks = seq(0,max_y_scale,mysteps))  +
      ylab("Relative abundance (%)") +
      labs(title = cohort,
           subtitle = IS)
    out_name <- gsub("[/ ]", "_", IS)
    ggsave(plot = myplot,
           filename = paste0("Result_figures/abundance_analysis_plots/boxplots/", cohort, "_staph_species__", out_name, ".pdf"),
           height = 8,
           width = 8,
           units = "cm")
  }
}
# ----------------------------------------------------------------------------------------


# Now calculate the mean abundance of each inferred Staphylococcus species across all samples. This is now the mean of the summed values
immunocompromised_taxa_summary.df <- immunocompromised_taxa_summary.df %>% 
  group_by(Sampletype_final_refined, Inferred_species) %>% 
  dplyr::summarise(Mean_relative_abundance_rarefied = mean(Summed_relative_abundance_rarefied)) %>%
  as.data.frame()

immunocompetent_taxa_summary.df <- immunocompetent_taxa_summary.df %>% 
  group_by(Sampletype_final_refined, Inferred_species) %>% 
  dplyr::summarise(Mean_relative_abundance_rarefied = mean(Summed_relative_abundance_rarefied)) %>%
  as.data.frame()


# immunocompromised_taxa_summary.df <- immunocompromised_taxa_summary.df %>% group_by(Sampletype_final_refined, Inferred_species) %>% dplyr::summarise(Mean_relative_abundance_rarefied = sum(Summed_relative_abundance_rarefied))
# immunocompromised_taxa_summary.df <- immunocompromised_taxa_summary.df %>% group_by(Sampletype_final_refined) %>% dplyr::summarise(Mean_relative_abundance_rarefied = sum(Summed_relative_abundance_rarefied))
# immunocompromised_taxa_summary.df
# ------------------
# This should equal...
# immunocompromised_taxa_summary.df <- immunocompromised_taxa_summary.df %>% group_by(Sampletype_final_refined, Sample) %>% dplyr::summarise(Summed_relative_abundance_rarefied = sum(Summed_relative_abundance_rarefied))
# immunocompromised_taxa_summary.df %>% group_by(Sampletype_final_refined) %>% dplyr::summarise(Mean_relative_abundance_rarefied = mean(Summed_relative_abundance_rarefied))
# This..
# immunocompromised_genus_summary.df[grepl("g__Staph", immunocompromised_genus_summary.df$taxonomy_label),c("Sampletype_final_refined","Mean_relative_abundance_rarefied")]
# immunocompromised_genus_summary.df[grepl("g__Staph", immunocompromised_genus_summary.df$taxonomy_label),c("Sampletype_final_refined","Summed_relative_abundance_rarefied")]
# ------------------

# Normalise the mean abundances
immunocompromised_taxa_summary.df <- immunocompromised_taxa_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()
immunocompetent_taxa_summary.df <- immunocompetent_taxa_summary.df %>% group_by(Sampletype_final_refined) %>% mutate(Normalised_mean_relative_abundance = Mean_relative_abundance_rarefied/sum(Mean_relative_abundance_rarefied)) %>% as.data.frame()

# Order taxonomy by the abundance. This is only approximate.
immunocompromised_taxa_summary.df <- immunocompromised_taxa_summary.df %>% group_by(Sampletype_final_refined) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
immunocompromised_taxa_summary.df$Inferred_species <- factor(immunocompromised_taxa_summary.df$Inferred_species, levels = unique(immunocompromised_taxa_summary.df$Inferred_species))
immunocompetent_taxa_summary.df <- immunocompetent_taxa_summary.df %>% group_by(Sampletype_final_refined) %>% arrange(Normalised_mean_relative_abundance) %>% as.data.frame()
immunocompetent_taxa_summary.df$Inferred_species <- factor(immunocompetent_taxa_summary.df$Inferred_species, levels = unique(immunocompetent_taxa_summary.df$Inferred_species))

# Combine the cohorts
immunocompromised_taxa_summary.df$Project <- "immunocompromised"
immunocompetent_taxa_summary.df$Project <- "immunocompetent"
both_cohorts_taxa_summary.df <- rbind(immunocompromised_taxa_summary.df,immunocompetent_taxa_summary.df)
both_cohorts_taxa_summary.df$Inferred_species <- factor(both_cohorts_taxa_summary.df$Inferred_species, levels = sort(as.character(unique(both_cohorts_taxa_summary.df$Inferred_species))))


# inferred_species_list <- sort(unique(as.character(immunocompromised_taxa_summary.df$Inferred_species, immunocompetent_taxa_summary.df$Inferred_species)))
inferred_species_list <- sort(unique(as.character(both_cohorts_taxa_summary.df$Inferred_species)))
inferred_species_list <- c("Other Staphyloccocus", inferred_species_list[!grepl("Other Staphyloccocus", inferred_species_list)])

both_cohorts_palette <- setNames(my_colour_palette_12_soft[1:length(inferred_species_list)], inferred_species_list)
both_cohorts_palette["Other Staphyloccocus"] <- "grey"

legend_plot <- ggplot(both_cohorts_taxa_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance*100, fill = Inferred_species)) +
  facet_wrap(~Project, scales = "free") +
  geom_bar(stat = "identity", colour = "black", lwd = .2) +
  coord_flip() +
  scale_fill_manual(values = both_cohorts_palette, name = "Inferred Staphylococcus species",guide = guide_legend(title.position = "top",nrow= 2)) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  ylab("Normalised mean relative abundance (%)") +
  common_theme

immunocompromised_plot <- ggplot(immunocompromised_taxa_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance*100, fill = Inferred_species)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_palette, name = "Inferred Staphylococcus species", guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  # ylab("Normalised mean relative abundance (%)") +
  ggtitle("immunocompromised") +
  ylab("") +
  common_theme +
  theme(plot.title = element_text(size = 6))

immunocompromised_plot

immunocompetent_plot <- ggplot(immunocompetent_taxa_summary.df, aes(x = Sampletype_final_refined, y = Normalised_mean_relative_abundance*100, fill = Inferred_species)) +
  geom_bar(stat = "identity", colour = "black", lwd = .2) + 
  coord_flip() +
  scale_fill_manual(values = both_cohorts_palette, name = "Inferred Staphylococcus species", guide = F) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  xlab("Sample type") +
  # ylab("") +
  ylab("Normalised mean relative abundance (%)") +
  ggtitle("immunocompetent") +

  common_theme +
  theme(plot.title = element_text(size = 6))

immunocompetent_plot

# Extract the legend
my_legend_taxa <- cowplot::get_legend(legend_plot + 
                                        theme(
                                          legend.position = "right",
                                          legend.text = element_text(size = 5),
                                          legend.title = element_text(size=6, face="bold"),
                                          legend.justification = "center",
                                          legend.direction = "horizontal",
                                          legend.box.just = "bottom",
                                          # plot.margin = unit(c(0, 0, 0, 0), "cm")
                                        )
)

# Make a grid of plots with the list of plots for both cohorts
grid_plot <- plot_grid(plotlist = list(immunocompromised_plot,NULL, immunocompetent_plot),ncol = 1,nrow=3, rel_heights = c(1,-.15,.8),align = "hv")
grid_plot <- plot_grid(grid_plot, my_legend_taxa, rel_heights = c(1,0.2), ncol = 1, nrow=2)
ggsave(filename = "Result_figures/abundance_analysis_plots/inferred_staph_species_abundance.pdf", plot = grid_plot, width = 10, height = 9, units = "cm")

