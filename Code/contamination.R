# Evaluation of the contaminates / negative control samples


library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")

source("Code/helper_functions.R")


# Load the and process metadata
metadata.df <- read.table("data/metadata_immunosuppressed_competent.tsv", header = T, sep = "\t")
metadata.df$Index <- with(metadata.df,paste0(Internal_name, "_", Job_ID))
metadata.df[metadata.df == ''] <- NA
metadata.df$Sample_type_original <- as.character(metadata.df$Sample_type_original)
metadata.df[metadata.df$Sample_type_original == "SwabCo",]$Sample_type_original <- "negative"
metadata.df$Sample_type_original <- factor(metadata.df$Sample_type_original)
rownames(metadata.df) <- metadata.df$Index
metadata.df <- subset(metadata.df, !is.na(Cohort))
metadata.df <- metadata.df[with(metadata.df, which(Final_publication_sample == "yes" & !is.na(Final_publication_sample))),]


# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# Compare read counts for negative controls for skin samples, and also controls/samples on the same run 
# as the immunosuppressed samples

P199_counts <- read.table(file = "data/P119_read_counts.tsv", header =T,sep = "\t")
P199_counts <- P199_counts[order(P199_counts$Read_count),]
P199_counts$Rank <- seq(nrow(P199_counts))
P199_counts[P199_counts$Is_negative_control == "",]$Is_negative_control <- "no"
P199_counts$Skin_sample <- "Not skin sample"
P199_counts[P199_counts$Job_ID %in% c("J1425","J1426", "J1427"),]$Skin_sample <- "Skin sample"

negative_controls_raw_counts <- ggplot(P199_counts,aes(x = Rank, y = Read_count, 
                                                       fill = Is_negative_control,
                                                       shape = Is_negative_control,
                                                       alpha = Is_negative_control)) + 
  geom_point() +
  scale_alpha_manual(values = c(.5,1),name = "Is negative control")+
  # geom_text(data = subset(P199_counts,Is_negative_control =="yes"), aes(label = Job_ID),nudge_y = 10000)+
  geom_vline(data = subset(P199_counts, Is_negative_control == "yes"),aes(xintercept = Rank), colour = "red", linetype = "dashed", lwd = .2) +
  xlab("Rank") +
  ylab("Raw read counts (R1)") +
  facet_wrap(~Job_ID) +
  scale_y_continuous(labels = comma,breaks = seq(0,200000,25000)) +
  scale_shape_manual(name = "Is negative control",values = c(21,22,23,24,25,3)) +
  # scale_shape_manual(name = "Sample type",values = c(25,24,23,22,21,3)) +
  scale_fill_manual(values = list("yes" = "red", "no" = "royalblue"), name = "Is negative control")
negative_controls_raw_counts
ggsave(plot = negative_controls_raw_counts,
       filename = "Result_figures/contaminant_analysis/P119_negative_controls_and_samples_raw_counts_per_job.pdf",
       height = 8,
       width = 12)


negative_controls_raw_counts <- ggplot(P199_counts,aes(x = Rank, y = Read_count, 
                                                       fill = Is_negative_control,
                                                       shape = Is_negative_control,
                                                       alpha = Is_negative_control)) + 
  geom_point() +
  scale_alpha_manual(values = c(.5,1),name = "Is negative control")+
  # geom_text(data = subset(P199_counts,Is_negative_control =="yes"), aes(label = Job_ID),nudge_y = 10000)+
  geom_vline(data = subset(P199_counts, Is_negative_control == "yes"),
             aes(xintercept = Rank), colour = "red", linetype = "dashed", lwd = .2) +
  # geom_vline(data = subset(P199_counts, Is_negative_control == "yes" & !Job_ID %in% c("J1425","J1426", "J1427")),
  # aes(xintercept = Rank), colour = "purple", linetype = "dashed", lwd = .2) +
  xlab("Rank") +
  ylab("Raw read counts (R1)") +
  facet_wrap(~Skin_sample) +
  scale_y_continuous(labels = comma,breaks = seq(0,200000,25000)) +
  scale_shape_manual(name = "Is negative control",values = c(21,22,23,24,25,3)) +
  # scale_shape_manual(name = "Sample type",values = c(25,24,23,22,21,3)) +
  scale_fill_manual(values = list("yes" = "red", "no" = "royalblue"), name = "Is negative control")
negative_controls_raw_counts
ggsave(plot = negative_controls_raw_counts,
       filename = "Result_figures/contaminant_analysis/P119_other_vs_skin_raw_counts.pdf",
       height = 4,
       width = 7)


temp <- metadata.df
temp <- temp[order(temp$R1_read_count_raw),]
temp$Rank <- seq(nrow(temp))
temp$Is_negative_control <- "no"
temp[temp$Sample_type == "negative",]$Is_negative_control <- "yes"

P199_counts
negative_controls_raw_counts <- 
  ggplot(temp,aes(x = Rank, y = R1_read_count_raw, fill = Is_negative_control, shape = Is_negative_control)) + 
  geom_point() +
  geom_vline(data = subset(temp, Is_negative_control == "yes"),aes(xintercept = Rank), colour = "red", linetype = "dashed", lwd = .2) +
  xlab("Rank") +
  ylab("Raw read counts (R1)") +
  facet_grid(~Cohort) +
  scale_y_continuous(labels = comma) +
  scale_shape_manual(name = "Is negative control",values = c(21,22,23,24,25,3)) +
  # scale_shape_manual(name = "Sample type",values = c(25,24,23,22,21,3)) +
  scale_fill_manual(values = list("yes" = "red", "no" = "royalblue"), name = "Is negative control")
# scale_fill_brewer(palette = "Dark2", name = "Is negative control")
negative_controls_raw_counts
ggsave(plot = negative_controls_raw_counts,
       filename = "Result_figures/contaminant_analysis/negative_controls_and_samples_raw_counts_per_cohort.pdf",
       height = 5,
       width = 10)

median(subset(temp, Cohort == "immunocompetent" & Sample_type == "negative")$R1_read_count_raw)
median(subset(temp, Cohort == "immunosuppressed" & Sample_type == "negative")$R1_read_count_raw)
mean(subset(temp, Cohort == "immunocompetent" & Sample_type == "negative")$R1_read_count_raw)
mean(subset(temp, Cohort == "immunosuppressed" & Sample_type == "negative")$R1_read_count_raw)

negative_controls_raw_counts <- 
  ggplot(temp,aes(x = Rank, y = R1_read_count_raw, fill = Is_negative_control, shape = Is_negative_control)) + 
  geom_point() +
  geom_vline(data = subset(temp, Is_negative_control == "yes"),aes(xintercept = Rank), colour = "red", linetype = "dashed", lwd = .2) +
  xlab("Rank") +
  ylab("Raw read counts (R1)") +
  facet_grid(~Cohort) +
  scale_y_continuous(labels = comma) +
  scale_shape_manual(name = "Is negative control",values = c(21,22,23,24,25,3)) +
  # scale_shape_manual(name = "Sample type",values = c(25,24,23,22,21,3)) +
  scale_fill_manual(values = list("yes" = "red", "no" = "royalblue"), name = "Is negative control")
# scale_fill_brewer(palette = "Dark2", name = "Is negative control")
negative_controls_raw_counts
ggsave(plot = negative_controls_raw_counts,
       filename = "Result_figures/contaminant_analysis/negative_controls_and_samples_raw_counts_per_job.pdf",
       height = 5,
       width = 20)
# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------

genus_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  # paste(phylostring[2],phylostring[3], phylostring[6], sep = ";")
                  paste(phylostring[3], phylostring[6], sep = ";")
                }))
}

# Load the consolidated feature data, should be be pre-filtering
# Basically just the feature count data with taxonomy data
project_data.df <- read.csv("Result_tables/other/feature_data_consolidated.csv",header =  T)
sample_ids <- grep("R[0-9].*|S[AB][0-9].*|S[0-9].*", names(project_data.df), value = T)
taxa_data.df <- generate_tax_level_data(project_data.df,sample_ids = sample_ids,
                                        tax_string_levels = "taxonomy_genus")

# Genus abundance matrix
genus_rel.m <- taxa_data.df$taxonomy_genus$abundances
# rownames(genus_rel.m)[grepl("^Un", rownames(genus_rel.m))]
# genus_rel.m <- taxa_data.df$abundances[apply(taxa_data.df$abundances, 1, max) * 100 > 5,]
# temp <- melt(sort(colSums(genus_rel.m) - colSums(genus_rel.m[grepl("Unassigned;Unassigned;Unassigned;Unassigned;Unassigned", rownames(genus_rel.m)),])))
# temp2 <- melt(sort(colSums(genus_rel.m) - colSums(genus_rel.m[grepl("Unassigned;Unassigned;Unassigned;Unassigned;Unassigned;Unassigned", rownames(genus_rel.m)),,drop =F])))
# plot(temp$value)
# median(temp$value)
# mean(temp2$value)
# median(temp2$value)

genus_rel.df <- m2df(genus_rel.m,"taxonomy_genus")
genus_rel_melt.df <- melt(genus_rel.df,variable.name = "Sample", value.name = "Abundance")
# genus_rel_melt_filt.df <- genus_rel_melt.df %>% group_by(Sample) %>% dplyr::filter(Abundance > .02) %>% dplyr::top_n(5, Abundance)  %>% as.data.frame()

# Filter to get most abundant taxa
temp <- genus_rel_melt.df %>% 
  group_by(Sample) %>% 
  # dplyr::top_n(5, Abundance) %>%
  dplyr::filter(Abundance > .2) %>% 
  as.data.frame()
top_genus <- unique(temp$taxonomy_genus) # Get list of taxa

# Add metadata
genus_rel_melt_filt.df <- left_join(genus_rel_melt.df,metadata.df[,c("Index", "Sample_type", "Cohort")],
                                    by = c("Sample" = "Index"))
# Taxa not in most abundant list are "Other"
genus_rel_melt_filt.df$taxonomy_genus[!genus_rel_melt_filt.df$taxonomy_genus %in% top_genus] <- "Other"

# Limit to negative samples
genus_rel_melt_filt.df <- subset(genus_rel_melt_filt.df,Sample_type == "negative")

# Sum abundances for each taxa, just results in a single entry for 'Other'
genus_rel_melt_filt.df <- 
  genus_rel_melt_filt.df %>% 
  group_by(Sample,Cohort,Sample_type, taxonomy_genus) %>% 
  dplyr::summarise(Abundance = max(Abundance))

# unique(genus_rel_melt_filt.df$taxonomy_genus)
# gsub(".*(f__.*);.*", "\\1",genus_rel_melt_filt.df$taxonomy_genus)
# gsub(".*(g__.*)", "\\1",genus_rel_melt_filt.df$taxonomy_genus)

# Create label, which is just the lowest resolved taxonomy
genus_rel_melt_filt.df$Label <- unlist(lapply(genus_rel_melt_filt.df$taxonomy_genus, first_resolved_taxonomy))

# Create colour palette
# my_colour_palette_206_distinct
col_palette <- setNames(my_colour_palette_20[1:length(unique(genus_rel_melt_filt.df$Label))], unique(genus_rel_melt_filt.df$Label))
col_palette["Unassigned"] <- "grey"
col_palette["Other"] <- "grey50"

myplot <- ggplot(genus_rel_melt_filt.df,aes(x = Sample, y = Abundance*100, fill = Label)) + 
  geom_bar(stat = "identity",colour = "black",lwd = .1) +
  xlab("Sample") +
  ylab("Relative abundance") +
  scale_fill_manual(values = col_palette, 
                    guide = guide_legend(ncol=2), name = "Genus")+
  # coord_flip()
  facet_wrap(Cohort~Sample_type,scales= "free_x") +
  scale_y_continuous(limits = c(0,100)) +
  # facet_grid(Cohort~Sample_type,scales= "free_x") +
  labs(title ="Genus per sample",subtitle = "(> 2% abundance)") +
  theme_classic() +
  theme(legend.text = element_text(size = 5),
        legend.key.size = unit(.2,"cm"),
        axis.text.x =  element_text(angle = 45,hjust = 1,vjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
  
myplot
ggsave(filename = "Result_figures/contaminant_analysis/stacked_abundances_negative_samples.pdf",
       plot =myplot,
       width = 50,
       height = 10,
       units = "cm")


# Count data for taxa
genus.m <- taxa_data.df$taxonomy_genus$counts
genus.df <- m2df(genus.m,"taxonomy_genus")
genus_melt.df <- melt(genus.df,variable.name = "Sample", value.name = "Counts")
genus_melt.df <- left_join(genus_melt.df,metadata.df[,c("Index", "Sample_type", "Cohort")],by = c("Sample" = "Index"))

# Sum counts for each sample
temp <- genus_melt.df %>% group_by(Sample,Sample_type,Cohort) %>% summarise(Counts = sum(Counts)) %>% as.data.frame()
# rank by count
temp <- temp[order(temp$Counts),]
temp$Rank <- seq(nrow(temp))
# Label as negative/not-negative
temp$Is_negative_control <- "no"
temp[temp$Sample_type == "negative",]$Is_negative_control <- "yes"

negative_controls_feature_counts <- 
  ggplot(temp,aes(x = Rank, y = Counts, fill = Is_negative_control, shape = Is_negative_control)) + 
  geom_point() +
  geom_vline(data = subset(temp, Is_negative_control == "yes"),aes(xintercept = Rank), colour = "red", linetype = "dashed", lwd = .2) +
  xlab("Rank") +
  ylab("Feature counts") +
  ggtitle("All features") +
  facet_grid(~Cohort) +
  scale_y_continuous(labels = comma,limits =c(0,200000), breaks = seq(0,400000,50000)) +
  scale_shape_manual(name = "Is negative control",values = c(21,22,23,24,25,3)) +
  # scale_shape_manual(name = "Sample type",values = c(25,24,23,22,21,3)) +
  scale_fill_manual(values = list("yes" = "red", "no" = "royalblue"), name = "Is negative control") +
  theme(plot.title = element_text(hjust = 0.5))
negative_controls_feature_counts

ggsave(plot = negative_controls_feature_counts,
       filename = "Result_figures/contaminant_analysis/negative_controls_and_samples_feature_counts_per_cohort.pdf",
       height = 5,
       width = 10)

# Remove Unassigned and Mammalia
temp <- genus_melt.df %>% 
  filter(taxonomy_genus != "Unassigned;Unassigned;Unassigned;Unassigned;Unassigned;Unassigned") %>%
  # filter("Unassigned;Unassigned;Unassigned;Unassigned;Unassigned;Unassigned") %>%
  filter(!grepl(".*;p__.*;Unassigned;Unassigned;Unassigned;Unassigned;Unassigned", taxonomy_genus)) %>% 
  filter(!grepl("f__Mammalia", taxonomy_genus)) %>% 
  group_by(Sample,Sample_type,Cohort) %>% summarise(Counts = sum(Counts)) %>% as.data.frame()
# temp[grepl("Mammal", temp$taxonomy_genus),]

# Rank by count
temp <- temp[order(temp$Counts),]
temp$Rank <- seq(nrow(temp))
temp$Is_negative_control <- "no"
temp[temp$Sample_type == "negative",]$Is_negative_control <- "yes"

negative_controls_feature_counts <- 
  ggplot(temp,aes(x = Rank, y = Counts, fill = Is_negative_control, shape = Is_negative_control)) + 
  geom_point() +
  geom_vline(data = subset(temp, Is_negative_control == "yes"),aes(xintercept = Rank), colour = "red", linetype = "dashed", lwd = .2) +
  xlab("Rank") +
  ylab("Feature counts") +
  ggtitle("Minus Unassigned and Mammalia features") +
  facet_grid(~Cohort) +
  scale_y_continuous(labels = comma,limits =c(0,200000), breaks = seq(0,400000,50000)) +
  scale_shape_manual(name = "Is negative control",values = c(21,22,23,24,25,3)) +
  # scale_shape_manual(name = "Sample type",values = c(25,24,23,22,21,3)) +
  scale_fill_manual(values = list("yes" = "red", "no" = "royalblue"), name = "Is negative control") +
  theme(plot.title = element_text(hjust = 0.5))
negative_controls_feature_counts

ggsave(plot = negative_controls_feature_counts,
       filename = "Result_figures/contaminant_analysis/negative_controls_and_samples_feature_counts_per_cohort_minus_Unassigned_Mammalia.pdf",
       height = 5,
       width = 10)


# Taxa not in top genus (defined above) are "Other"
genus_melt.df$taxonomy_genus[!genus_melt.df$taxonomy_genus %in% top_genus] <- "Other"
# Limit to Negative samples
genus_melt.df <- subset(genus_melt.df,Sample_type == "negative")
# Sum counts for each taxa (ensure one entry for "Other")
genus_melt.df <- 
  genus_melt.df %>% 
  group_by(Sample,Cohort,Sample_type, taxonomy_genus) %>% 
  dplyr::summarise(Counts = max(Counts))

genus_melt.df$Label <- unlist(lapply(genus_melt.df$taxonomy_genus, first_resolved_taxonomy))


myplot <- ggplot(genus_melt.df,aes(x = Sample, y = Counts, fill = Label)) + 
  geom_bar(stat = "identity", colour = "black",lwd = .1) +
  xlab("Sample") +
  ylab("ASV Counts") +
  scale_fill_manual(values = col_palette, 
                    guide = guide_legend(ncol=2), name = "Genus")+
  labs(title ="Genus per sample",subtitle = "(> 2% abundance)") +
  # coord_flip()
  facet_wrap(Cohort~Sample_type,scales= "free") +
  scale_y_continuous(breaks = seq(0,60000, by = 10000), limits = c(0,60000)) +
  # facet_grid(Cohort~Sample_type,scales= "free_x") +
  theme_classic() +
  theme(legend.text = element_text(size = 5),
        legend.key.size = unit(.2,"cm"),
        axis.text.x =  element_text(angle = 45,hjust = 1,vjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave(filename = "Result_figures/contaminant_analysis/stacked_asv_counts_negative_samples.pdf",
       plot =myplot,
       width = 50,
       height = 15,
       units = "cm")


# colSums(read.csv("Result_tables/relative_abundance_tables/Contaminants_Specie_relative_abundances.csv", header = T, row.names = 1))




# my_heatmap <- make_heatmap(myheatmap_matrix = genus_rel.m*100,
#                            filename = "Result_figures/contaminant_analysis/negative_controls_and_samples_5PC_heatmap.pdf",
#                            plot_height = 10,
#                            plot_width = 60,
#                            palette_choice = "purple",
#                            discrete_legend = T,
#                            legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, "> 60"),
#                            my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100
#              )



