library(dplyr)
library(reshape2)
library(ggplot2)


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

my_colour_pallete_12_soft <-c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621", "#c75a93", "#7c7731")


setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")


# Load QC results
qc_results.df <- read.csv("Result_tables/other/QC_summary.csv")

# Remove negative samples
qc_results.df <- qc_results.df[!qc_results.df$Sampletype == "negative",]

# length(qc_results.df$Patient[which(grepl("MST", qc_results.df$Patient))]) 
# length(unique(qc_results.df$Patient[which(grepl("MST", qc_results.df$Patient))]))
# length(qc_results.df$Patient[which(!grepl("MST", qc_results.df$Patient))])
# length(unique(qc_results.df$Patient[which(!grepl("MST", qc_results.df$Patient))]))

qc_results.df %>%
  group_by(Project, Sampletype_final) %>%
  summarise(N_patient = n_distinct(Patient), N_sample = n_distinct(Sample)) %>%
  as.data.frame()

#10 samples, 9 patients CORRECT
# sort(subset(qc_results.df, Project == "immunosuppressed" & Sampletype_final == "SCC")$Patient)


# How many reads were lost from rarefaction and filtering
qc_results.df$Reads_removed_from_filtering_rarefied

# Proportionally how many reads were lost from rarefaction
qc_results.df$Proportion_reads_removed_from_filtering_rarefied

# How many samples does rarefaction remove reads from (difference between filtered and rarefied, NOT original and rarefied)
length(which(qc_results.df$Filtered_rarefied_read_counts - qc_results.df$Filtered_read_counts < 0)) # 127 samples
# summary(qc_results.df$Filtered_rarefied_read_counts - qc_results.df$Filtered_read_counts < 0)
# How many samples does rarefaction remove reads from (difference between original and rarefied)
length(which(qc_results.df$Filtered_rarefied_read_counts - qc_results.df$Original_read_counts < 0)) # 1017 samples

# Proportion of reads lost in samples due to rarefaction
temp <- qc_results.df[which(qc_results.df$Filtered_rarefied_read_counts - qc_results.df$Filtered_read_counts < 0),] 
proportions <- with(temp,sort(round(unique(Proportion_reads_removed_from_filtering_rarefied - Proportion_reads_removed_from_filtering) * 100,2)))
median(proportions)
min(proportions)
max(proportions)

# Number of features lost from rarefaction
with(temp, Features_removed_from_filtering_rarefied - Features_removed_from_filtering) 
proportions <- with(temp,sort(round(unique(Proportion_features_removed_from_filtering_rarefied - Proportion_features_removed_from_filtering) * 100,2)))
median(proportions)
max(proportions)


# ------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------
# PLOTS

# Proportion of reads for each 'Domain' for both projects, before filtering and rarefection.
# Using the same sample order, also plot the number of reads for each sample

domain_proportions.df <- qc_results.df[c("Sample","Sampletype_final","Sample_retained", "Project", 
                                         "Bacterial_proportion_original", 
                                         "Archaeal_proportion_original", 
                                         "Unassigned_proportion_original",
                                         "Mammal_proportion_original",
                                         "Fungal_proportion_original")]

# domain_proportions.df <- qc_results.df[c("Sample","Sampletype_final", "Project", "Bacterial_proportion_original", "Archaeal_proportion_original", "Unassigned_proportion_original","Eukaryal_proportion_original")]
domain_proportions.df <- domain_proportions.df[order(domain_proportions.df$Bacterial_proportion_original),]
sample_order <- domain_proportions.df$Sample
# names(domain_proportions.df) <- c("Sample", "Sampletype_final", "Project", "Bacteria", "Archaea", "Unassigned", "Eukarya")
names(domain_proportions.df) <- c("Sample", "Sampletype_final","Sample_retained", "Project", "Bacteria", "Archaea", "Unassigned", "Mammalia", "Fungal")
domain_proportions.df$Other <- 1 - rowSums(domain_proportions.df[c("Bacteria", "Archaea", "Unassigned", "Mammalia", "Fungal")])
domain_proportions.df <- melt(domain_proportions.df,id.vars = c("Sample", "Sampletype_final","Sample_retained", "Project"), variable.name = "Domain", value.name = "Abundance")
# domain_proportions.df$Domain <- factor(domain_proportions.df$Domain, levels = c("Archaea","Bacteria","Eukarya", "Unassigned"))
domain_proportions.df$Domain <- factor(domain_proportions.df$Domain, levels = c("Archaea","Bacteria","Fungal","Mammalia", "Unassigned", "Other"))
domain_proportions.df$Sample <- factor(domain_proportions.df$Sample, levels = sample_order)

# Set up consistent colours
domain_colours <- setNames(c(rev(my_colour_pallete_12_soft[1:5]), "grey60"),c("Archaea","Bacteria","Fungal","Mammalia", "Unassigned", "Other")) 

myplot <- ggplot(subset(domain_proportions.df, Project == "immunosuppressed"), aes(x = Sample, y = Abundance, fill = Domain)) + 
  # geom_bar(stat ="identity", aes(colour = Sample_retained)) + 
  geom_bar(stat ="identity") + 
  # annotate(geom = "point", x = seq(1,length(unique(domain_proportions.df$Sample))), y = -0.03, size=1,stroke= 0) +
  xlab("Sample") +
  ylab("Abundance") +
  scale_fill_manual(values = domain_colours) +
  facet_wrap(~Project, scales = "free_x") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.5),
        axis.line = element_line(size = .3),
        axis.ticks = element_line(size = .3))

# annotate(geom='point', x = seq(1,length(unique(patient_data$Sample)), 1), y = -0.03, size=1,stroke= 0, shape = all_sample_shapes, color ="black", fill = all_sample_colours)
  
ggsave(filename = "Result_figures/exploratory_analysis/immunosuppressed_domain_proportions__original.pdf", 
       plot = myplot,
       width = 30,
       height = 10,
       units = "cm")

myplot <- ggplot(subset(domain_proportions.df, Project == "immunocompetent"), aes(x = Sample, y = Abundance, fill = Domain)) + 
  geom_bar(stat ="identity") + 
  xlab("Sample") +
  ylab("Abundance") +
  scale_fill_manual(values = domain_colours) +
  facet_wrap(~Project, scales = "free_x") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90, size = 2, vjust = 0.5),
        axis.line = element_line(size = .3),
        axis.ticks = element_line(size = .3)) 


ggsave(filename = "Result_figures/exploratory_analysis/immunocompetent_domain_proportions__original.pdf", 
       plot = myplot,
       width = 80,
       height = 10,
       units = "cm")

# Now for read counts, keep same order
# domain_read_counts.df <- qc_results.df[c("Sample","Sampletype_final", "Project", "Bacterial_read_count_original", "Archaeal_read_count_original", "Unassigned_read_count_original","Eukaryal_read_count_original")]
domain_read_counts.df <- qc_results.df[c("Sample","Sampletype_final", "Project", 
                                         "Bacterial_read_count_original", 
                                         "Archaeal_read_count_original", 
                                         "Unassigned_read_count_original",
                                         "Mammal_read_count_original", 
                                         "Fungal_read_count_original")]
# names(domain_read_counts.df) <- c("Sample", "Sampletype_final", "Project", "Bacteria", "Archaea", "Unassigned", "Eukarya")
names(domain_read_counts.df) <- c("Sample", "Sampletype_final", "Project", "Bacteria", "Archaea", "Unassigned", "Mammalia", "Fungal")

# summary(qc_results.df$Sample == domain_read_counts.df$Sample)
domain_read_counts.df$Other <- qc_results.df$Original_read_counts - rowSums(domain_read_counts.df[c("Bacteria", "Archaea", "Unassigned", "Mammalia", "Fungal")])
domain_read_counts.df <- melt(domain_read_counts.df,id.vars = c("Sample", "Sampletype_final", "Project"), variable.name = "Domain", value.name = "Read_count")
# domain_read_counts.df$Domain <- factor(domain_read_counts.df$Domain, levels = c("Archaea","Bacteria","Eukarya", "Unassigned"))
domain_read_counts.df$Domain <- factor(domain_read_counts.df$Domain, levels = c("Archaea","Bacteria","Fungal","Mammalia", "Unassigned", "Other"))
domain_read_counts.df$Sample <- factor(domain_read_counts.df$Sample, levels = sample_order)

myplot <- ggplot(subset(domain_read_counts.df, Project == "immunosuppressed"), aes(x = Sample, y = Read_count, fill = Domain)) + 
  geom_bar(stat ="identity") + 
  xlab("Sample") +
  ylab("Read count") +
  scale_fill_manual(values = domain_colours) +
  facet_wrap(~Project, scales = "free_x") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.5),
        axis.line = element_line(size = .3),
        axis.ticks = element_line(size = .3)) 
ggsave(filename = "Result_figures/exploratory_analysis/immunosuppressed_domain_read_counts__original.pdf", 
       plot = myplot,
       width = 30,
       height = 10,
       units = "cm")

myplot <- ggplot(subset(domain_read_counts.df, Project == "immunocompetent"), aes(x = Sample, y = Read_count, fill = Domain)) + 
  geom_bar(stat ="identity") + 
  xlab("Sample") +
  ylab("Read count") +
  scale_fill_manual(values = domain_colours) +
  facet_wrap(~Project, scales = "free_x") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90, size = 2, vjust = 0.5),
        axis.line = element_line(size = .3),
        axis.ticks = element_line(size = .3)) 


ggsave(filename = "Result_figures/exploratory_analysis/immunocompetent_domain_read_counts__original.pdf", 
       plot = myplot,
       width = 80,
       height = 10,
       units = "cm")

# -------------------------------------------------------------------------------------------
# Proportion of reads for each 'Domain' for both projects, AFTER filtering and rarefection.
qc_results.df$Fungal_proportion_after_filtering
domain_proportions.df <- qc_results.df[c("Sample","Sampletype_final", "Project", "Bacterial_proportion_after_filtering_rarefied", "Fungal_proportion_after_filtering_rarefied")]
domain_proportions.df <- domain_proportions.df[which(!is.na(domain_proportions.df$Bacterial_proportion_after_filtering_rarefied)),]
domain_proportions.df <- domain_proportions.df[order(domain_proportions.df$Bacterial_proportion_after_filtering_rarefied),]
sample_order <- domain_proportions.df$Sample
names(domain_proportions.df) <- c("Sample", "Sampletype_final", "Project", "Bacteria", "Fungal")

domain_proportions.df <- melt(domain_proportions.df,id.vars = c("Sample", "Sampletype_final", "Project"), variable.name = "Domain", value.name = "Abundance")
domain_proportions.df$Domain <- factor(domain_proportions.df$Domain, levels = c("Bacteria","Fungal"))
domain_proportions.df$Sample <- factor(domain_proportions.df$Sample, levels = sample_order)

myplot <- ggplot(subset(domain_proportions.df, Project == "immunosuppressed"), aes(x = Sample, y = Abundance, fill = Domain)) + 
  geom_bar(stat ="identity") + 
  xlab("Sample") +
  ylab("Abundance") +
  scale_fill_manual(values = domain_colours) +
  facet_wrap(~Project, scales = "free_x") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90, size = 4),
        axis.line = element_line(size = .3),
        axis.ticks = element_line(size = .3)) 

ggsave(filename = "Result_figures/exploratory_analysis/immunosuppressed_domain_proportions__filtered_rarefied.pdf", 
       plot = myplot,
       width = 30,
       height = 10,
       units = "cm")


myplot <- ggplot(subset(domain_proportions.df, Project == "immunocompetent"), aes(x = Sample, y = Abundance, fill = Domain)) + 
  geom_bar(stat ="identity") + 
  xlab("Sample") +
  ylab("Abundance") +
  scale_fill_manual(values = domain_colours) +
  facet_wrap(~Project, scales = "free_x") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90, size = 2),
        axis.line = element_line(size = .3),
        axis.ticks = element_line(size = .3)) 

ggsave(filename = "Result_figures/exploratory_analysis/immunocompetent_domain_proportions__filtered_rarefied.pdf", 
       plot = myplot,
       width = 60,
       height = 10,
       units = "cm")

# Now for read counts, keep same order
domain_read_counts.df <- qc_results.df[c("Sample","Sampletype_final", "Project", 
                                         "Bacterial_read_count_after_filtering_rarefied",
                                         "Fungal_read_count_after_filtering_rarefied")]
domain_read_counts.df <- domain_read_counts.df[which(!is.na(domain_read_counts.df$Bacterial_read_count_after_filtering_rarefied)),]
names(domain_read_counts.df) <- c("Sample", "Sampletype_final", "Project", "Bacteria","Fungal")

domain_read_counts.df <- melt(domain_read_counts.df,id.vars = c("Sample", "Sampletype_final", "Project"), variable.name = "Domain", value.name = "Read_count")
domain_read_counts.df$Domain <- factor(domain_read_counts.df$Domain, levels = c("Archaea","Bacteria","Fungal"))
domain_read_counts.df$Sample <- factor(domain_read_counts.df$Sample, levels = sample_order)

myplot <- ggplot(subset(domain_read_counts.df, Project == "immunosuppressed"), aes(x = Sample, y = Read_count, fill = Domain)) + 
  geom_bar(stat ="identity") + 
  xlab("Sample") +
  ylab("Read count") +
  scale_fill_manual(values = domain_colours) +
  facet_wrap(~Project, scales = "free_x") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.5),
        axis.line = element_line(size = .3),
        axis.ticks = element_line(size = .3)) 
ggsave(filename = "Result_figures/exploratory_analysis/immunosuppressed_domain_read_counts__filtered_rarefied.pdf", 
       plot = myplot,
       width = 30,
       height = 10,
       units = "cm")

myplot <- ggplot(subset(domain_read_counts.df, Project == "immunocompetent"), aes(x = Sample, y = Read_count, fill = Domain)) + 
  geom_bar(stat ="identity") + 
  xlab("Sample") +
  ylab("Read count") +
  scale_fill_manual(values = domain_colours) +
  facet_wrap(~Project, scales = "free_x") +
  common_theme +
  theme(axis.text.x = element_text(angle = 90, size = 2, vjust = 0.5),
        axis.line = element_line(size = .3),
        axis.ticks = element_line(size = .3)) 


ggsave(filename = "Result_figures/exploratory_analysis/immunocompetent_domain_read_counts__filtered_rarefied.pdf", 
       plot = myplot,
       width = 60,
       height = 10,
       units = "cm")


# -------------------------------------------------------------------------------------------

