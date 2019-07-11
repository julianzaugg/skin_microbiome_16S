

library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(cowplot)

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

common_theme <- theme(
  panel.border = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black", size = 0.5),
  panel.background = element_blank(),
  strip.background = element_rect(fill = "white", colour = "white", size = 1),
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

# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")

# Just load the combined metadata and abundance tables for simplicity
# otu_data.df <- read.csv("Result_tables/other/OTU_counts_abundances_and_metadata.csv")
species_data.df <- read.csv("Result_tables/other/specie_counts_abundances_and_metadata.csv")
genus_data.df <- read.csv("Result_tables/other/genus_counts_abundances_and_metadata.csv")
family_data.df <- read.csv("Result_tables/other/family_counts_abundances_and_metadata.csv")
order_data.df <- read.csv("Result_tables/other/order_counts_abundances_and_metadata.csv")
class_data.df <- read.csv("Result_tables/other/class_counts_abundances_and_metadata.csv")
phylum_data.df <- read.csv("Result_tables/other/phylum_counts_abundances_and_metadata.csv")

# Remove negative samples
species_data.df <- subset(species_data.df, Sampletype != "negative")
genus_data.df <- subset(genus_data.df, Sampletype != "negative")
family_data.df <- subset(family_data.df, Sampletype != "negative")
order_data.df <- subset(order_data.df, Sampletype != "negative")
class_data.df <- subset(class_data.df, Sampletype != "negative")
phylum_data.df <- subset(phylum_data.df, Sampletype != "negative")


# ------------------------------------------------------------------------------------------
generate_taxa_summary <- function(mydata, taxa_column, abundance_column= "Relative_abundance_rarefied"){
  # Generate a summary table of each taxa for each group. Use this to filter to taxa of interest
  # For each taxa, count the :
  #   number of samples it is in
  #   max, min and summed abundance
  taxa_group_summary <-
    mydata %>% 
    dplyr::select(taxa_column, abundance_column) %>%
    group_by_(taxa_column) %>% # use the _ form of the function to allow variables to be used
    dplyr::mutate(In_N_samples = n()) %>%
    
    dplyr::summarise(In_N_samples = max(In_N_samples),
                     Max_abundance = max(get(abundance_column)),
                     Min_abundance = min(get(abundance_column)),
                     Mean_abundance = mean(get(abundance_column)),
                     Summed_abundance = sum(get(abundance_column))) %>%
    arrange(dplyr::desc(Max_abundance)) %>%
    group_by_(taxa_column) %>%
    as.data.frame()
  return(taxa_group_summary)
}

get_top_taxa <- function(taxa_summary, my_top_n = 10){
  taxa_column <- names(taxa_summary)[1]
  most_abundant_taxa <- taxa_summary %>% arrange(dplyr::desc(Mean_abundance)) %>% head(my_top_n) %>% select_(taxa_column) %>% .[[1]] %>% as.character()
  return(most_abundant_taxa)
}

# Function that takes the full abundance + metadata dataframe and the taxa summary dataframe, determines most abundant taxa and 
# relabels the low(er) abundance taxa to "Other"
relabel_low_abundance_taxa <- function(mydata, taxa_summary, my_top_n = 10){
  internal_data <- mydata
  taxa_column <- names(taxa_summary)[1]
  # Get the top_n taxa by the mean abundance
  most_abundant_taxa <- taxa_summary %>% arrange(dplyr::desc(Mean_abundance)) %>% head(my_top_n) %>% select_(taxa_column) %>% .[[1]] %>% as.character()
  internal_data[,taxa_column] <- as.character(internal_data[,taxa_column])
  internal_data[!internal_data[,taxa_column] %in%  most_abundant_taxa, taxa_column] <- "Other"
  return(internal_data)
}



# ------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------
#                 Create pie charts

make_pie_graph <- function(mydata, taxonomy_column, mytitle = NULL,  mysubtitle = NULL, my_colour_list = NULL, n_taxa = 10){
  # Assumes two columns, first is the category, second is count
  # my_colour_list should be a named list with the corresponding colours provided
  internal_mydata <- mydata
  variable <- names(mydata)[1]
  
  # Generate taxonomy summary (to get mean for each genus)
  taxa_summary.df <- generate_taxa_summary(internal_mydata, taxa_column = taxonomy_column, abundance_column = "Relative_abundance_rarefied")
  # Relabel each taxa not in top 10 to 'Other'
  taxa_data_processed.df <- relabel_low_abundance_taxa(internal_mydata, taxa_summary.df, my_top_n =n_taxa)
  # Put the re-labelled data back through the summary function. This will calculate the mean abundance for the Other taxa!
  taxa_data_processed_summary.df <- generate_taxa_summary(taxa_data_processed.df, taxa_column = taxonomy_column, abundance_column = "Relative_abundance_rarefied")
  # Normalise the mean abundance
  taxa_data_processed_summary.df$normalised_mean_abundance <- taxa_data_processed_summary.df$Mean_abundance / sum(taxa_data_processed_summary.df$Mean_abundance)
  # Create pie label
  # taxa_data_processed_summary.df$pie_label <- lapply(taxa_data_processed_summary.df$normalised_mean_abundance, function(x) ifelse(x >= 0.01, paste0(round(x*100), "%"), "<1%"))
  taxa_data_processed_summary.df$pie_label <- lapply(taxa_data_processed_summary.df$normalised_mean_abundance, function(x) ifelse(x >= 0.05, paste0(round(x*100), "%"), ""))
  # Order by normalised_mean_abundance
  taxa_data_processed_summary.df <- taxa_data_processed_summary.df[rev(order(taxa_data_processed_summary.df$normalised_mean_abundance)),]
  
  # Create the taxa label
  # FIXME - double check working correctly for all but genus
  taxa_data_processed_summary.df$taxa_label <- gsub(".*(f__.*)", "\\1",taxa_data_processed_summary.df[,taxonomy_column])
  
  # if ("taxa_label" %in% names(taxa_data_processed_summary.df)){
    # taxa_data_processed_summary.df$taxa_label <- gsub(".*(f__.*)", "\\1",taxa_data_processed_summary.df$taxonomy_genus)  
    # taxa_data_processed_summary.df$taxa_label <- gsub(".*(f__.*)", "\\1",taxa_data_processed_summary.df[,taxonomy_column])
  # } else{
    # last_assigned_taxa <- unlist(lapply(strsplit(gsub(";Unassigned.*","", taxa_data_processed_summary.df[,taxonomy_column]), ";"), function(x) rev(x)[1]))
    # taxa_data_processed_summary.df$taxa_label <- last_assigned_taxa
  # }
  
  # Factor taxonomy column and label to correct level ordering
  taxa_data_processed_summary.df[,taxonomy_column] <- factor(taxa_data_processed_summary.df[,taxonomy_column], levels = unique(as.character(taxa_data_processed_summary.df[,taxonomy_column])))
  
  tl.v <- unique(taxa_data_processed_summary.df$taxa_label)
  # taxa_data_processed_summary.df$taxa_label <- factor(taxa_data_processed_summary.df$taxa_label, levels = unique(as.character(taxa_data_processed_summary.df$taxa_label)))
  taxa_data_processed_summary.df$taxa_label <- factor(taxa_data_processed_summary.df$taxa_label, levels = c(as.character(tl.v[tl.v != "Other"]), "Other"))
  # Assign the y location for the labels
  taxa_data_processed_summary.df <- 
    taxa_data_processed_summary.df %>%
    arrange(dplyr::desc(get(taxonomy_column))) %>%
    mutate(lab.ypos = cumsum(normalised_mean_abundance) - .5*normalised_mean_abundance)
  
  # Assign colours for taxa. Assumes Other is first (it should be based on the factoring performed above)
  if (is.null(my_colour_list)){
    if (length(unique(taxa_data_processed_summary.df$taxa_label)) <= 11){
      taxa_colours.l <- setNames(c("grey", my_colour_palette_12_soft), taxa_data_processed_summary.df$taxa_label)  
      # print(taxa_colours.l)
    } else{
      taxa_colours.l <- setNames(c("grey", my_colour_palette_30_distinct), taxa_data_processed_summary.df$taxa_label)
    }  
  } else{
    taxa_colours.l <- my_colour_list
  }
  
  
  pie_chart <- ggplot(taxa_data_processed_summary.df,aes(x ="", y= normalised_mean_abundance, fill = taxa_label)) + 
    geom_bar(width = 1, stat = "identity", color = "white",size = .2) +
    coord_polar("y", start=0) +
    scale_fill_manual(values = taxa_colours.l, name = "Taxonomy") +
    geom_text(aes(y = lab.ypos, label = pie_label), color = "white", size =3) +
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.title = element_blank(),
          legend.text = element_text(size = 8),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, size =8),
          plot.subtitle = element_text(size = 8, hjust = .5))
  if (!is.null(mytitle) | !is.null(mysubtitle)){
    pie_chart <- pie_chart + labs(title = mytitle, subtitle = mysubtitle)
  }
  pie_chart
} 
# -------------------------------------------------------
# immunocompromised, Sampletype_compromised_refined
immunocompromised_lesions_top_genus <- c()
immunocompromised_genus_data.df <- subset(genus_data.df, Project == "immunocompromised")

for (lesion in unique(immunocompromised_genus_data.df$Sampletype_compromised_refined)){
  if (is.na(lesion)) {next}
  data_subset <- subset(genus_data.df, Project == "immunocompromised" & Sampletype_compromised_refined == lesion)
  taxa_summary <- generate_taxa_summary(data_subset, taxa_column = "taxonomy_genus",abundance_column = "Relative_abundance_rarefied")
  immunocompromised_lesions_top_genus <- unique(c(immunocompromised_lesions_top_genus, get_top_taxa(taxa_summary,my_top_n = 10)))
}
immunocompromised_genus_palette <- setNames(my_colour_palette_30_distinct[1:length(immunocompromised_lesions_top_genus)], immunocompromised_lesions_top_genus)
names(immunocompromised_genus_palette) <- gsub(".*(f__.*)", "\\1",names(immunocompromised_genus_palette))
immunocompromised_genus_palette["Other"] <- "grey"

for (lesion in unique(immunocompromised_genus_data.df$Sampletype_compromised_refined)){
  if (is.na(lesion)) {next}
  data_subset <- subset(immunocompromised_genus_data.df, Sampletype_compromised_refined == lesion)
  n_patients <- length(unique(data_subset$Patient))
  n_samples <- length(unique(data_subset$Sample))
  my_title <- paste0("immunocompromised, ", lesion) 
  my_subtitle <- paste0("# samples = ", n_samples, "\n# patients = ", n_patients) 
  myplot <- make_pie_graph(data_subset, "taxonomy_genus", mytitle = my_title, mysubtitle = my_subtitle, n_taxa = 10, my_colour_list = immunocompromised_genus_palette) 
  outname <- paste0("immunocompromised", "_", lesion, "_sampletype_compromised_refined_top_genera_pie_chart.pdf") 
  ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/",outname), height = 10, width =25, units = "cm")
}

myplot <- make_pie_graph(genus_data.df[genus_data.df$taxonomy_genus %in% immunocompromised_lesions_top_genus,], "taxonomy_genus", n_taxa = 20, my_colour_list = immunocompromised_genus_palette) 
ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/immunocompromised_sampletype_compromised_refined_for_legend.pdf"), height = 15, width =45, units = "cm")

# -------------------------------------------------------
# immunocompromised, Sampletype_compromised_refined, just forearm
forearm_swab_ids <- c("1383","1385","1470","1561","1599","1647","1649")
forearm_indices <- c("R1383_J1425", "SA6550_J1427", "R1368_J1477", "R1460_J1425", "R1498_J1425", "SB4909_J1426", "SB4911_J1426")
# R1383_J1425 MST
# SA6550_J1427 MST
# R1368_J1477 MST
# R1460_J1425 MST
# R1498_J1425 MST
# 1647 SB4909_J1426 REMOVED
# 1649 SB4911_J1426 REMOVED
immunocompromised_lesions_top_genus <- c()
unique(immunocompromised_forearm.df[c("Sample","Sampletype", "Sampletype_compromised_refined", "Swab_ID")])

immunocompromised_forearm.df <- immunocompromised_genus_data.df[immunocompromised_genus_data.df$Swab_ID %in% forearm_swab_ids,]
immunocompromised_forearm.df <- subset(immunocompromised_forearm.df, Sampletype_compromised_refined == "SCC") # ensure only SCC samples
immunocompromised_forearm.df$Sampletype_compromised_refined <- factor(immunocompromised_forearm.df$Sampletype_compromised_refined)


for (lesion in unique(immunocompromised_forearm.df$Sampletype_compromised_refined)){
  if (is.na(lesion)) {next}
  data_subset <- subset(immunocompromised_forearm.df, Sampletype_compromised_refined == lesion)
  taxa_summary <- generate_taxa_summary(data_subset, taxa_column = "taxonomy_genus",abundance_column = "Relative_abundance_rarefied")
  immunocompromised_lesions_top_genus <- unique(c(immunocompromised_lesions_top_genus, get_top_taxa(taxa_summary,my_top_n = 10)))
}
immunocompromised_genus_palette <- setNames(my_colour_palette_30_distinct[1:length(immunocompromised_lesions_top_genus)], immunocompromised_lesions_top_genus)
names(immunocompromised_genus_palette) <- gsub(".*(f__.*)", "\\1",names(immunocompromised_genus_palette))
immunocompromised_genus_palette["Other"] <- "grey"

# data_subset <- subset(immunocompromised_forearm.df, Sampletype_compromised_refined == "SCC")

for (lesion in unique(immunocompromised_forearm.df$Sampletype_compromised_refined)){
  print(lesion)
  if (is.na(lesion)) {next}
  data_subset <- subset(immunocompromised_forearm.df, Sampletype_compromised_refined == lesion)
  n_patients <- length(unique(data_subset$Patient))
  n_samples <- length(unique(data_subset$Sample))
  my_title <- paste0("immunocompromised, forearm,", lesion) 
  my_subtitle <- paste0("# samples = ", n_samples, "\n# patients = ", n_patients) 
  myplot <- make_pie_graph(data_subset, "taxonomy_genus", mytitle = my_title, mysubtitle = my_subtitle, n_taxa = 10, my_colour_list = immunocompromised_genus_palette) 
  outname <- paste0("immunocompromised", "_", lesion, "_sampletype_compromised_refined_forearm_top_genera_pie_chart.pdf") 
  ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/",outname), height = 10, width =25, units = "cm")
}

myplot <- make_pie_graph(genus_data.df[genus_data.df$taxonomy_genus %in% immunocompromised_lesions_top_genus,], "taxonomy_genus", n_taxa = 20, my_colour_list = immunocompromised_genus_palette) 
ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/immunocompromised_sampletype_compromised_refined_forearm_for_legend.pdf"), height = 15, width =45, units = "cm")




# -------------------------------------------------------
# immunocompetent, Sampletype_pooled
immunocompetent_lesions_top_genus <- c()
for (lesion in unique(genus_data.df$Sampletype_pooled)){
  data_subset <- subset(genus_data.df, Project == "immunocompetent" & Sampletype_pooled == lesion)
  taxa_summary <- generate_taxa_summary(data_subset, taxa_column = "taxonomy_genus",abundance_column = "Relative_abundance_rarefied")
  immunocompetent_lesions_top_genus <- unique(c(immunocompetent_lesions_top_genus, get_top_taxa(taxa_summary,my_top_n = 10)))
}
immunocompetent_genus_palette <- setNames(my_colour_palette_30_distinct[1:length(immunocompetent_lesions_top_genus)], immunocompetent_lesions_top_genus)
names(immunocompetent_genus_palette) <- gsub(".*(f__.*)", "\\1",names(immunocompetent_genus_palette))
immunocompetent_genus_palette["Other"] <- "grey"

temp <- subset(genus_data.df, Project == "immunocompetent")
for (lesion in unique(temp$Sampletype_pooled)){
  data_subset <- subset(temp, Sampletype_pooled == lesion)
  n_patients <- length(unique(data_subset$Patient))
  n_samples <- length(unique(data_subset$Sample))
  my_title <- paste0("immunocompetent, ", lesion) 
  my_subtitle <- paste0("# samples = ", n_samples, "\n# patients = ", n_patients) 
  myplot <- make_pie_graph(data_subset, "taxonomy_genus",  mytitle = my_title, mysubtitle = my_subtitle, n_taxa = 10, my_colour_list = immunocompetent_genus_palette) 
  outname <- paste0("immunocompetent", "_", lesion, "_sampletype_pooled_top_genera_pie_chart.pdf") 
  ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/",outname), height = 10, width =25, units = "cm")
}
myplot <- make_pie_graph(genus_data.df[genus_data.df$taxonomy_genus %in% immunocompetent_lesions_top_genus,], "taxonomy_genus", n_taxa = 20, my_colour_list = immunocompetent_genus_palette) 
ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/immunocompetent_sampletype_pooled_for_legend.pdf"), height = 10, width =45, units = "cm")


# -------------------------------------------------------
# 
temp <- subset(genus_data.df, Project == "immunocompromised")
immunocompromised_patient_group_top_genus <- c()
for (pg in unique(temp$Patient_group)){
  data_subset <- subset(temp, Patient_group == pg)
  taxa_summary <- generate_taxa_summary(data_subset, taxa_column = "taxonomy_genus",abundance_column = "Relative_abundance_rarefied")
  immunocompromised_patient_group_top_genus <- unique(c(immunocompromised_patient_group_top_genus, get_top_taxa(taxa_summary,my_top_n = 10)))
}
immunocompromised_genus_palette <- setNames(my_colour_palette_30_distinct[1:length(immunocompromised_patient_group_top_genus)], immunocompromised_patient_group_top_genus)
names(immunocompromised_genus_palette) <- gsub(".*(f__.*)", "\\1",names(immunocompromised_genus_palette))
immunocompromised_genus_palette["Other"] <- "grey"

for (pg in unique(temp$Patient_group)){
  data_subset <- subset(temp, Patient_group == pg)
  n_patients <- length(unique(data_subset$Patient))
  n_samples <- length(unique(data_subset$Sample))
  my_title <- paste0("immunocompromised, ", pg) 
  my_subtitle <- paste0("# samples = ", n_samples, "\n# patients = ", n_patients) 
  
  myplot <- make_pie_graph(data_subset, "taxonomy_genus", mytitle = my_title,mysubtitle = my_subtitle, n_taxa = 10, my_colour_list = immunocompromised_genus_palette) 
  outname <- paste0("immunocompromised", "_", pg, "_patient_group_top_genera_pie_chart.pdf") 
  ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/",outname), height = 10, width =25, units = "cm")
}

myplot <- make_pie_graph(temp[temp$taxonomy_genus %in% immunocompromised_patient_group_top_genus,], "taxonomy_genus", n_taxa = 20, my_colour_list = immunocompromised_genus_palette) 
ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/immunocompromised_patient_group_for_legend.pdf"), height = 10, width =45, units = "cm")
# -------------------------------------------------------
# Number of medications
immunocompromised_nmed_top_genus <- c()
for (nmed in unique(temp$Number_of_meds)){
  data_subset <- subset(temp, Number_of_meds == nmed)
  taxa_summary <- generate_taxa_summary(data_subset, taxa_column = "taxonomy_genus",abundance_column = "Relative_abundance_rarefied")
  immunocompromised_nmed_top_genus <- unique(c(immunocompromised_nmed_top_genus, get_top_taxa(taxa_summary,my_top_n = 10)))
}
immunocompromised_genus_palette <- setNames(my_colour_palette_30_distinct[1:length(immunocompromised_nmed_top_genus)], immunocompromised_nmed_top_genus)
names(immunocompromised_genus_palette) <- gsub(".*(f__.*)", "\\1",names(immunocompromised_genus_palette))
immunocompromised_genus_palette["Other"] <- "grey"

for (nmed in unique(temp$Number_of_meds)){
  data_subset <- subset(temp, Number_of_meds == nmed)
  n_patients <- length(unique(data_subset$Patient))
  n_samples <- length(unique(data_subset$Sample))
  my_title <- paste0("immunocompromised, ", nmed) 
  my_subtitle <- paste0("# samples = ", n_samples, "\n# patients = ", n_patients) 
  myplot <- make_pie_graph(data_subset, "taxonomy_genus",mytitle = my_title, mysubtitle = my_subtitle, n_taxa = 10, my_colour_list = immunocompromised_genus_palette) 
  outname <- paste0("immunocompromised", "_", nmed, "_number_of_medications_top_genera_pie_chart.pdf") 
  ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/",outname), height = 10, width =25, units = "cm")
}

myplot <- make_pie_graph(temp[temp$taxonomy_genus %in% immunocompromised_nmed_top_genus,], "taxonomy_genus", n_taxa = 20, my_colour_list = immunocompromised_genus_palette) 
ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/immunocompromised_number_of_medications_for_legend.pdf"), height = 12, width =35, units = "cm")
# -------------------------------------------------------
# Fitzpatrick_skin_type
immunocompromised_f_skin_type_top_genus <- c()
for (fst in unique(temp$Fitzpatrick_skin_type)){
  data_subset <- subset(temp, Fitzpatrick_skin_type == fst)
  taxa_summary <- generate_taxa_summary(data_subset, taxa_column = "taxonomy_genus",abundance_column = "Relative_abundance_rarefied")
  immunocompromised_f_skin_type_top_genus <- unique(c(immunocompromised_f_skin_type_top_genus, get_top_taxa(taxa_summary,my_top_n = 10)))
}
immunocompromised_genus_palette <- setNames(my_colour_palette_30_distinct[1:length(immunocompromised_f_skin_type_top_genus)], immunocompromised_f_skin_type_top_genus)
names(immunocompromised_genus_palette) <- gsub(".*(f__.*)", "\\1",names(immunocompromised_genus_palette))
immunocompromised_genus_palette["Other"] <- "grey"

for (fst in unique(temp$Fitzpatrick_skin_type)){
  data_subset <- subset(temp, Fitzpatrick_skin_type == fst)
  n_patients <- length(unique(data_subset$Patient))
  n_samples <- length(unique(data_subset$Sample))
  my_title <- paste0("immunocompromised, ", fst)
  my_subtitle <- paste0("# samples = ", n_samples, "\n# patients = ", n_patients) 
  myplot <- make_pie_graph(data_subset, "taxonomy_genus", mytitle = my_title, mysubtitle = my_subtitle, n_taxa = 10, my_colour_list = immunocompromised_genus_palette) 
  outname <- paste0("immunocompromised", "_", fst, "_fitzpatrick_skin_type_top_genera_pie_chart.pdf") 
  ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/",outname), height = 12, width =25, units = "cm")
}

myplot <- make_pie_graph(temp[temp$taxonomy_genus %in% immunocompromised_f_skin_type_top_genus,], "taxonomy_genus", n_taxa = 20, my_colour_list = immunocompromised_genus_palette) 
ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/immunocompromised_fitzpatrick_skin_type_for_legend.pdf"), height = 12, width =25, units = "cm")



# -------------------------------------------------------
# Project (cohort)
project_top_genus <- c()
for (cohort in unique(genus_data.df$Project)){
  data_subset <- subset(genus_data.df, Project == cohort)
  taxa_summary <- generate_taxa_summary(data_subset, taxa_column = "taxonomy_genus",abundance_column = "Relative_abundance_rarefied")
  project_top_genus <- unique(c(project_top_genus, get_top_taxa(taxa_summary,my_top_n = 9)))
}
project_genus_palette <- setNames(my_colour_palette_12_soft[1:length(project_top_genus)], project_top_genus)
names(project_genus_palette) <- gsub(".*(f__.*)", "\\1",names(project_genus_palette))
project_genus_palette["Other"] <- "grey"

for (cohort in unique(genus_data.df$Project)){
  data_subset <- subset(genus_data.df, Project == cohort)
  n_patients <- length(unique(data_subset$Patient))
  n_samples <- length(unique(data_subset$Sample))
  my_title <- cohort
  my_subtitle <- paste0("# samples = ", n_samples, "\n# patients = ", n_patients) 
  myplot <- make_pie_graph(data_subset, "taxonomy_genus", mytitle = my_title, mysubtitle =my_subtitle,  n_taxa = 9, my_colour_list = project_genus_palette) 
  outname <- paste0(cohort, "_top_genera_pie_chart.pdf") 
  ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/",outname), height = 10, width =25, units = "cm")
}

myplot <- make_pie_graph(genus_data.df[genus_data.df$taxonomy_genus %in% project_top_genus,], "taxonomy_genus", n_taxa = 12, my_colour_list = project_genus_palette) 
ggsave(myplot, filename = paste0("Result_figures/abundance_analysis_plots/project_for_legend.pdf"), height = 12, width =35, units = "cm")






# --------------------
# top 10 genus by mean relative abundances. Values are normalised for pie chart.
# genus_data.df <- species_data.df
# names(genus_data.df)[names(genus_data.df) == "taxonomy_species"] <- "taxonomy_genus"

# Generate taxonomy summary (to get mean for each genus)
genus_taxa_summary.df <- generate_taxa_summary(genus_data.df,
                                               taxa_column = "taxonomy_genus",
                                               abundance_column = "Relative_abundance_rarefied")
# Relabel each genus not in top 10 to 'Other'
genus_data_processed.df <- relabel_low_abundance_taxa(genus_data.df, genus_taxa_summary.df, my_top_n =10)

# Put the re-labelled data back through the summary function. This will calculate the mean abundance for the Other taxa!
genus_data_processed_summary.df <- generate_taxa_summary(genus_data_processed.df,
                                                         taxa_column = "taxonomy_genus",
                                                         abundance_column = "Relative_abundance_rarefied")

# Normalise the mean abundance
genus_data_processed_summary.df$normalised_mean_abundance <- genus_data_processed_summary.df$Mean_abundance / sum(genus_data_processed_summary.df$Mean_abundance)

# Create pie label
genus_data_processed_summary.df$pie_label <- lapply(genus_data_processed_summary.df$normalised_mean_abundance, function(x) ifelse(x >= 0.01, paste0(round(x*100), "%"), "<1%"))

# Order by normalised_mean_abundance
genus_data_processed_summary.df <- genus_data_processed_summary.df[rev(order(genus_data_processed_summary.df$normalised_mean_abundance)),]

# Create the taxa label
genus_data_processed_summary.df$taxa_label <- gsub(".*(f__.*)", "\\1",genus_data_processed_summary.df$taxonomy_genus)


# Factor taxonomy_genus and label to correct level ordering
genus_data_processed_summary.df$taxonomy_genus <- factor(genus_data_processed_summary.df$taxonomy_genus, levels = unique(as.character(genus_data_processed_summary.df$taxonomy_genus)))
genus_data_processed_summary.df$taxa_label <- factor(genus_data_processed_summary.df$taxa_label, levels = unique(as.character(genus_data_processed_summary.df$taxa_label)))


# Assign the y location for the labels
genus_data_processed_summary.df <- 
  genus_data_processed_summary.df %>%
  arrange(desc(taxonomy_genus)) %>%
  mutate(lab.ypos = cumsum(normalised_mean_abundance) - .5*normalised_mean_abundance)

# Fix the ordering so that Other is first level
# genus_data_processed_summary.df$taxonomy_genus <- relevel(factor(genus_data_processed_summary.df$taxonomy_genus), "Other")

# Assign colours for taxa
taxa_colours.l <- setNames(c("grey", my_colour_palette_12_soft), genus_data_processed_summary.df$taxa_label)

pie_chart <- ggplot(genus_data_processed_summary.df,aes(x ="", y= normalised_mean_abundance, fill = taxa_label)) + 
  geom_bar(width = 1, stat = "identity", color = "white",size = .2) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = taxa_colours.l, name = "Genus") +
  ggtitle("(Normalised) mean relative abundances across all samples") +
  # geom_text(aes(y = lab.ypos, label = pie_label), color = "white") +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size =8),
        plot.subtitle = element_text(size = 8, hjust = .5))


ggsave(pie_chart, filename = "Result_figures/abundance_analysis_plots/pie_chart_top_genera.pdf", height = 10, width =30, units = "cm")



# ------------------------------------------------------------------------
# top 10 genus by mean relative abundances for each community. Values are normalised for pie chart.
# Generate taxonomy summary (to get mean for each genus)
genus_taxa_summary_immunocompromised.df <- generate_taxa_summary(subset(genus_data.df, Project == "immunocompromised"),taxa_column = "taxonomy_genus", abundance_column = "Relative_abundance_rarefied")
genus_taxa_summary_immunocompetent.df <- generate_taxa_summary(subset(genus_data.df, Project == "immunocompetent"),taxa_column = "taxonomy_genus", abundance_column = "Relative_abundance_rarefied")

# Relabel each genus not in top 10 to 'Other'
genus_data_processed_immunocompromised.df <- relabel_low_abundance_taxa(subset(genus_data.df, Project == "immunocompromised"), genus_taxa_summary_immunocompromised.df, my_top_n =8)
genus_data_processed_immunocompetent.df <- relabel_low_abundance_taxa(subset(genus_data.df, Project == "immunocompetent"), genus_taxa_summary_immunocompetent.df, my_top_n =8)

# Put the re-labelled data back through the summary function. This will calculate the mean abundance for the Other taxa!
genus_data_processed_immunocompromised_summary.df <- generate_taxa_summary(genus_data_processed_immunocompromised.df,taxa_column = "taxonomy_genus",abundance_column = "Relative_abundance_rarefied")
genus_data_processed_immunocompetent_summary.df <- generate_taxa_summary(genus_data_processed_immunocompetent.df,taxa_column = "taxonomy_genus",abundance_column = "Relative_abundance_rarefied")

# Assign community
genus_data_processed_immunocompromised_summary.df$Project <- "immunocompromised"
genus_data_processed_immunocompetent_summary.df$Project <- "immunocompetent"

# Normalise the mean abundance
genus_data_processed_immunocompromised_summary.df$normalised_mean_abundance <- genus_data_processed_immunocompromised_summary.df$Mean_abundance / sum(genus_data_processed_immunocompromised_summary.df$Mean_abundance)
genus_data_processed_immunocompetent_summary.df$normalised_mean_abundance <- genus_data_processed_immunocompetent_summary.df$Mean_abundance / sum(genus_data_processed_immunocompetent_summary.df$Mean_abundance)

# Create pie label
genus_data_processed_immunocompromised_summary.df$pie_label <- lapply(genus_data_processed_immunocompromised_summary.df$normalised_mean_abundance, function(x) ifelse(x >= 0.01, paste0(round(x*100), "%"), "<1%"))
genus_data_processed_immunocompetent_summary.df$pie_label <- lapply(genus_data_processed_immunocompetent_summary.df$normalised_mean_abundance, function(x) ifelse(x >= 0.01, paste0(round(x*100), "%"), "<1%"))

# Order by normalised_mean_abundance
genus_data_processed_immunocompromised_summary.df <- genus_data_processed_immunocompromised_summary.df[rev(order(genus_data_processed_immunocompromised_summary.df$normalised_mean_abundance)),]
genus_data_processed_immunocompetent_summary.df <- genus_data_processed_immunocompetent_summary.df[rev(order(genus_data_processed_immunocompetent_summary.df$normalised_mean_abundance)),]

# Create the taxa label
genus_data_processed_immunocompromised_summary.df$taxa_label <- gsub(".*(f__.*)", "\\1",genus_data_processed_immunocompromised_summary.df$taxonomy_genus)
genus_data_processed_immunocompetent_summary.df$taxa_label <- gsub(".*(f__.*)", "\\1",genus_data_processed_immunocompetent_summary.df$taxonomy_genus)

# Factor taxonomy_genus and label to correct level ordering
genus_data_processed_immunocompromised_summary.df$taxonomy_genus <- factor(genus_data_processed_immunocompromised_summary.df$taxonomy_genus, levels = unique(as.character(genus_data_processed_immunocompromised_summary.df$taxonomy_genus)))
genus_data_processed_immunocompetent_summary.df$taxonomy_genus <- factor(genus_data_processed_immunocompetent_summary.df$taxonomy_genus, levels = unique(as.character(genus_data_processed_immunocompetent_summary.df$taxonomy_genus)))
genus_data_processed_immunocompromised_summary.df$taxa_label <- factor(genus_data_processed_immunocompromised_summary.df$taxa_label, levels = unique(as.character(genus_data_processed_immunocompromised_summary.df$taxa_label)))
genus_data_processed_immunocompetent_summary.df$taxa_label <- factor(genus_data_processed_immunocompetent_summary.df$taxa_label, levels = unique(as.character(genus_data_processed_immunocompetent_summary.df$taxa_label)))

# Assign the y location for the labels
genus_data_processed_immunocompromised_summary.df <-
  genus_data_processed_immunocompromised_summary.df %>%
  arrange(desc(taxonomy_genus)) %>%
  mutate(lab.ypos = cumsum(normalised_mean_abundance) - .5*normalised_mean_abundance)

genus_data_processed_immunocompetent_summary.df <-
  genus_data_processed_immunocompetent_summary.df %>%
  arrange(desc(taxonomy_genus)) %>%
  mutate(lab.ypos = cumsum(normalised_mean_abundance) - .5*normalised_mean_abundance)


# Assign colours for taxa
# taxa_colours_immunocompromised.l <- setNames(c("grey", my_colour_palette_12_soft), unique(genus_data_processed_immunocompromised_summary.df$taxonomy_genus))
# taxa_colours_immunocompetent.l <- setNames(c("grey", my_colour_palette_12_soft), unique(genus_data_processed_immunocompetent_summary.df$taxonomy_genus)) 
taxa_colours.l <- setNames(c("grey", my_colour_palette_12_soft),unique(c(as.character(genus_data_processed_immunocompromised_summary.df$taxa_label), as.character(genus_data_processed_immunocompetent_summary.df$taxa_label))))


pie_chart_immunocompromised <- ggplot(genus_data_processed_immunocompromised_summary.df,aes(x ="", y= normalised_mean_abundance, fill = taxa_label)) + 
  geom_bar(width = 1, stat = "identity", color = "white",size = .2) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = taxa_colours.l, name = "Genus") +
  labs(title="Immunocompromised",
       subtitle="(Normalised) mean relative abundances across all samples") +
       # geom_text(aes(y = lab.ypos, label = pie_label), color = "white") +
       theme(axis.text = element_blank(),
       axis.line = element_blank(),
       axis.title = element_blank(),
       legend.text = element_text(size = 8),
       axis.ticks = element_blank(),
       plot.title = element_text(hjust = 0.5),
       plot.subtitle = element_text(size = 8, hjust = .5))
pie_chart_immunocompromised
ggsave(pie_chart_immunocompromised, filename = "Result_figures/abundance_analysis_plots/pie_chart_top_genera_immunocompromised.pdf", height = 10, width =30, units = "cm")
       
       
pie_chart_immunocompetent <- ggplot(genus_data_processed_immunocompetent_summary.df,aes(x ="", y= normalised_mean_abundance, fill = taxa_label)) + 
       geom_bar(width = 1, stat = "identity", color = "white",size = .2) +
       coord_polar("y", start=0) +
       scale_fill_manual(values = taxa_colours.l, name = "Genus") +
       labs(title="Immunocompetent",
       subtitle="(Normalised) mean relative abundances across all samples") +
       # geom_text(aes(y = lab.ypos, label = pie_label), color = "white") +
       theme(axis.text = element_blank(),
       axis.line = element_blank(),
       axis.title = element_blank(),
       legend.text = element_text(size = 8),
       axis.ticks = element_blank(),
       plot.title = element_text(hjust = 0.5),
       plot.subtitle = element_text(size = 8, hjust = 0.5))
pie_chart_immunocompetent
ggsave(pie_chart_immunocompetent, filename = "Result_figures/abundance_analysis_plots/pie_chart_top_genera_immunocompetent.pdf", height = 10, width =30, units = "cm")

