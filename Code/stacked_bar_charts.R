# Stacked bar graphs for abundances
# Showing taxa abundances for each patient for each sample / lesion type
# Requires cowplot!!


library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(cowplot)

# ------------------------------------------------------------------------------------------
# Various colour palletes
my_colour_pallete <- c("#8dd3c7","#ffffb3","#bebada","#fb8072", "#80b1d3", "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5", "#cc0000")
# From http://tools.medialab.sciences-po.fr/iwanthue/
my_colour_pallete_20 <- c("#66bd79","#a35bcf","#5bb643","#d14ea6","#a2b239","#5c6bcc","#dc892e","#5e93cd","#d64737","#49b6a8","#dc3c6e","#4f7e3c","#bd8cd5","#caab55","#914c88","#867230","#df82a2","#a65429","#ab4a5a","#e0896a")
my_colour_pallete_20_distinct <- c("#0057b4","#7fff56","#d600bc","#d8d500","#e76eff","#019932","#9f8fff","#ffc730","#007fac","#a20019","#06fefd","#ff6782","#00774c","#e0c8ff","#717a00","#4b2952","#e2ed7d","#46321e","#ffbd76","#ffb4c6")
my_colour_pallete_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_pallete_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
my_colour_pallete_15 <- c("#77b642","#7166d9","#cfa240","#b351bb","#4fac7f","#d44891","#79843a","#c68ad4","#d15a2c","#5ba7d9","#ce4355","#6570ba","#b67249","#9b4a6f","#df8398")
my_colour_pallete_32_distinct <- c("#ea7e00","#ca0074","#d1c69b","#474007","#bb00ad","#9c80ff","#be3300","#542e72","#00b9f5","#09436b","#8b0036","#9ac8e6","#ff1059","#959eff","#154a11","#0290f4","#ff7762","#7dbf00","#ff8194","#834c00","#006e73","#f9bb5d","#d6c943","#017229","#00d3a8","#732427","#36e191","#6a8200","#efb3ea","#3227bb","#ff90e1","#e92a12")
lesion_pallete_7 <- c("#8558d6","#6ee268","#d247ad","#c9d743","#d7453e","#59a237","#d78f2a")
patient_pallete_45 <- c("#d64530","#585fb1","#795d97","#9e4773","#3f6921","#71692c","#a2b93c","#d571cc","#9b3e97","#33947a","#98ad66","#448a4e","#869ae0","#5ce7af","#e085a3","#dfdc87","#d19be2","#5cb735","#e38269","#3db6c0","#50b565","#50902c","#a98a2c","#dde84a","#db3d76","#5fe485","#7c8329","#b3e791","#6fe965","#5ebce9","#3c86c1","#2a6a45","#65b688","#6651d1","#af4ed3","#df872f","#56e4db","#737cea","#ac464b","#dd37b5","#995b2b","#daac6f","#92e2be","#a2e24b","#e0be3a")
my_colour_pallete_10_distinct <- c("#8eec45","#0265e8","#f6a800","#bf6549","#486900","#c655a0","#00d1b6","#ff4431","#aeb85c","#7e7fc8")
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
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")


# Just load the combined metadata and abundance tables for simplicity
# otu_data.df <- read.csv("Result_tables/other/OTU_counts_abundances_and_metadata.csv")
genus_data.df <- read.csv("Result_tables/other/genus_counts_abundances_and_metadata.csv")
family_data.df <- read.csv("Result_tables/other/family_counts_abundances_and_metadata.csv")
order_data.df <- read.csv("Result_tables/other/order_counts_abundances_and_metadata.csv")
class_data.df <- read.csv("Result_tables/other/class_counts_abundances_and_metadata.csv")
phylum_data.df <- read.csv("Result_tables/other/phylum_counts_abundances_and_metadata.csv")


# ------------------------------------------------------------------------------------------

generate_taxa_summary <- function(mydata, taxa_column, abundance_column= "Relative_abundance_rarified"){
  # Generate a summary table of each taxa for each group. Use this to filter to taxa of interest
  # For each taxa, count the :
  #   number of samples it is in
  #   max and min abundance
  taxa_group_summary <-
    mydata %>% 
    dplyr::select(taxa_column,  abundance_column) %>%
    group_by_(taxa_column) %>% # use the _ form of the function to allow variables to be used
    dplyr::mutate(In_N_samples = n()) %>%
    
    dplyr::summarise(In_N_samples = max(In_N_samples),
                     Max_abundance = max(get(abundance_column)),
                     Min_abundance = min(get(abundance_column)),
                     Summed_abundance = sum(get(abundance_column))) %>%
    arrange(desc(Max_abundance)) %>%
    group_by_(taxa_column) %>%
    as.data.frame()
  return(taxa_group_summary)
}

# Get taxonomy summary for each dataset
# genus_taxa_summary.df <- generate_taxa_summary(genus_data.df,taxa_column = "taxonomy_genus", abundance_column = "Relative_abundance_rarified")
# family_taxa_summary.df <- generate_taxa_summary(family_data.df,taxa_column = "taxonomy_family", abundance_column = "Relative_abundance_rarified")
# order_taxa_summary.df <- generate_taxa_summary(order_data.df,taxa_column = "taxonomy_order", abundance_column = "Relative_abundance_rarified")
# class_taxa_summary.df <- generate_taxa_summary(class_data.df,taxa_column = "taxonomy_class", abundance_column = "Relative_abundance_rarified")
# phylum_taxa_summary.df <- generate_taxa_summary(phylum_data.df,taxa_column = "taxonomy_phylum", abundance_column = "Relative_abundance_rarified")

genus_taxa_summary.df <- generate_taxa_summary(genus_data.df,taxa_column = "taxonomy_genus", abundance_column = "Read_count_rarified")
family_taxa_summary.df <- generate_taxa_summary(family_data.df,taxa_column = "taxonomy_family", abundance_column = "Read_count_rarified")
order_taxa_summary.df <- generate_taxa_summary(order_data.df,taxa_column = "taxonomy_order", abundance_column = "Read_count_rarified")
class_taxa_summary.df <- generate_taxa_summary(class_data.df,taxa_column = "taxonomy_class", abundance_column = "Read_count_rarified")
phylum_taxa_summary.df <- generate_taxa_summary(phylum_data.df,taxa_column = "taxonomy_phylum", abundance_column = "Read_count_rarified")
# sum(class_data.df[class_data.df$taxonomy_class == "d__Bacteria;p__Firmicutes;c__Bacilli", "Relative_abundance_rarified"])
# sum(phylum_data.df[phylum_data.df$taxonomy_phylum == "d__Bacteria;p__Actinobacteria", "Read_count_rarified"])

# Function that takes the full abundance + metadata dataframe and the taxa summary dataframe, determines most abundant taxa and 
# relabels the low(er) abundance taxa to "Other"
relabel_low_abundance_taxa <- function(mydata, taxa_summary, my_top_n = 10){
  internal_data <- mydata
  taxa_column <- names(taxa_summary)[1]
  # Get the top_n taxa by the max abundance
  # most_abundant_taxa <- taxa_summary %>% arrange(desc(Max_abundance)) %>% head(my_top_n) %>% select_(taxa_column) %>% .[[1]] %>% as.character()
  most_abundant_taxa <- taxa_summary %>% arrange(desc(Summed_abundance)) %>% head(my_top_n) %>% select_(taxa_column) %>% .[[1]] %>% as.character()
  internal_data[,taxa_column] <- as.character(internal_data[,taxa_column])
  internal_data[!internal_data[,taxa_column] %in%  most_abundant_taxa, taxa_column] <- "Other"
  return(internal_data)
}

# Relabel lower abundance taxa to "Other"
class_data_processed.df <- relabel_low_abundance_taxa(class_data.df, class_taxa_summary.df, my_top_n =10)

# Count the number of samples for each patient and filter out patients that have too few samples
samples_each_patient <- class_data_processed.df %>%
  group_by(Patient) %>%
  dplyr::summarise(Count = n_distinct(Sample)) %>%
  as.data.frame()
class_data_processed.df <- class_data_processed.df[class_data_processed.df$Patient %in% as.character(samples_each_patient[samples_each_patient$Count > 5,"Patient"]),]

# Create a separate bar plot for each patient
barplots.l <- list()
for (patient in unique(class_data_processed.df$Patient)){
  patient_data <- subset(class_data_processed.df, Patient == patient)
  
  # Order by the lesion type
  patient_data$Sampletype <- factor(patient_data$Sampletype, levels = sort(unique(as.character(patient_data$Sampletype))))
  patient_data <- patient_data[order(patient_data$Sampletype),]
  
  # Set the lesion colours
  # temp <- unique(patient_data[,c("Sample", "Sampletype")])
  # variable_values <- factor(as.character(unique(temp$Sampletype)))
  # variable_colours <- setNames(lesion_pallete_7[1:length(variable_values)], variable_values)
  
  temp <- unique(patient_data[,c("Sample", "Sampletype", "Sampletype_colour")])
  all_sample_colours <- temp$Sampletype_colour
  
  # all_sample_colours <- as.character(
  #   lapply(
  #     as.character(temp$Sampletype), 
  #     function(x) variable_colours[x]
  #   )
  # )
  
  myplot <- ggplot(patient_data, aes(x = Sample, y = Relative_abundance_rarified, fill = taxonomy_class)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c(my_colour_pallete_10_distinct, "grey"), name = "Taxonomy", guide = F) +
    ggtitle(patient) +
    # ylab("Relative abundance") +
    # xlab("Lesion type")
    ylab(NULL) + xlab(NULL) +
  theme(
    # text=element_text(family=font, size=fig_font_size),
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    plot.margin=unit(c(0,0,0,0), "null"),
    plot.title = element_text(hjust = 0.5, size =8),
    panel.background=element_blank(),
    panel.border=element_blank(),
    axis.line = element_blank()) +
    
    # Add points to the bottom of each bar indicating the lesion type
    annotate(geom='point', x = seq(1,length(unique(patient_data$Sample)), 1), y = -0.03, size=.5, shape = 1, color = all_sample_colours)
  barplots.l[[patient]] <- myplot # Store the plot
}
# barplots.l$MS009

# Remove patients with too few samples in each group
# sort(names(barplots.l))
barplots.l <- barplots.l[!names(barplots.l) %in% c("MS014")]

# ggsave(
#   filename = paste0("Result_figures/immunocompetent_taxa_barplots.pdf"), 
#   arrangeGrob(
#     barplots.l[['MS001']], 
#     barplots.l[['MS002']], 
#     barplots.l[['MS004']], 
#     barplots.l[['MS005']], 
#     barplots.l[['MS006']], 
#     barplots.l[['MS007']], 
#     barplots.l[['MS008']], 
#     barplots.l[['MS009']], 
#     barplots.l[['MS010']], 
#     barplots.l[['MS011']], 
#     ncol=5, nrow = 2), width = 25, height = 8)

#"MST002","MST003","MST008","MST012","MST013","MST014","MST015","MST016","MST018","MST023","MST029","MST030","MST032"
# paste0(sort(names(barplots.l)),collapse = ",")
# "MS001,MS002,MS004,MS005,MS006,MS007,MS008,MS009,MS010,MS011,MS013,MS014,MST001,MST002,MST003,MST004,MST005,MST006,MST007,MST008,MST009,MST010,MST012,MST013,MST014,MST015,MST016,MST017,MST018,MST019,MST020,MST021,MST022,MST023,MST024,MST025,MST026,MST027,MST028,MST029,MST030,MST031,MST032"

# ggsave(
#   filename = paste0("Result_figures/immunocompromised_taxa_barplots.pdf"), 
#   arrangeGrob(
#     barplots.l[['MST002']],
#     barplots.l[['MST003']],
#     barplots.l[['MST008']],
#     barplots.l[['MST012']],
#     barplots.l[['MST013']],
#     barplots.l[['MST014']],
#     barplots.l[['MST015']],
#     barplots.l[['MST016']],
#     barplots.l[['MST018']],
#     barplots.l[['MST023']],
#     barplots.l[['MST029']],
#     barplots.l[['MST030']],
#     barplots.l[['MST032']],
#     ncol=5, nrow = 3), width = 5, height = 4)

MST_plot_names <- sort(grep("MST", names(barplots.l), value = T))
MS_plot_names <- sort(names(barplots.l)[!names(barplots.l) %in% MST_plot_names])

# Create another plot to make the legend. Since this contains all samples, the legend will have all taxa. 
all_sample_plot <- ggplot(class_data_processed.df, aes(x = Sample, y = Relative_abundance_rarified, fill = taxonomy_class)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(my_colour_pallete_10_distinct[1:10], "grey"), name = "Taxonomy") +
  ggtitle(patient)


# Create another plot to make the legend for the lesion point.
colour_list <- unique(class_data_processed.df[,c("Sampletype","Sampletype_colour")])
colour_list <- setNames(as.character(colour_list[,2]), as.character(colour_list[,1]))

all_sample_plot_for_lesion_colour <- ggplot(class_data_processed.df, aes(x = Sample, y = Relative_abundance_rarified, colour = Sampletype)) +
  geom_point(shape = 1) +
  scale_colour_manual(values = colour_list, name = "Lesion type")

# Extract the legend
my_legend_taxa <- cowplot::get_legend(all_sample_plot + theme(legend.position = "right", 
                                                         legend.text = element_text(size = 6),
                                                         legend.title = element_text(size =7, face = "bold"),
                                                         legend.justification = "center",
                                                         plot.margin = unit(c(0, 0, 0, 0), "cm")))
my_legend_lesion <- cowplot::get_legend(all_sample_plot_for_lesion_colour + theme(legend.position = "right", 
                                                                          legend.text = element_text(size = 6),
                                                                          legend.title = element_text(size =7, face = "bold"),
                                                                          legend.justification = "center"))
my_legend <- plot_grid(my_legend_taxa, my_legend_lesion, ncol = 1,nrow = 2) 
  

# Make a grid of plots with the list of plots for both cohorts
grid_plot_competent <- cowplot::plot_grid(plotlist = barplots.l[MS_plot_names])
grid_plot_competent <- plot_grid(grid_plot_competent, my_legend, rel_widths = c(1,.3), ncol = 2)

ggsave(grid_plot_competent, filename = "Result_figures/stacked_bar_charts_immunocompetent.pdf", height = 20, width =40, units = "cm")


grid_plot_compromised <- cowplot::plot_grid(plotlist = barplots.l[MST_plot_names])
grid_plot_compromised <- plot_grid(grid_plot_compromised, my_legend, rel_widths = c(1,.3), ncol = 2)
ggsave(grid_plot_compromised, filename = "Result_figures/stacked_bar_charts_immunocompromised.pdf", height = 20, width =40, units = "cm")


