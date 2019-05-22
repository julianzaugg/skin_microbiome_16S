# Calculate microbial load

# THIS IS NOT POSSIBLE WITH 16S data. Usually determined from qPCR.


library(dplyr)
library(reshape2)
library(ggplot2)




setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC and SCC lesions. 
# Remove samples for different lesion types (nasal,scar,scar_PL,KA,KA_PL,VV,VV_PL,SF,SF_PL,other,other_PL,negative) from metadata and otu table
# metadata.df <- metadata.df[!metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC", "NLC"),]
metadata.df <- metadata.df[!metadata.df$Sampletype %in% c("negative"),]

# Load unfiltered data
# unfiltered_data.df <- read.csv(file = "Result_tables/other/project_otu_table_unfiltered.csv",header = T)

# Load filtered/processed abundance data with metadata
# otu_data.df <- read.csv("Result_tables/other/OTU_counts_abundances_and_metadata.csv")
# genus_data.df <- read.csv("Result_tables/other/genus_counts_abundances_and_metadata.csv")
# family_data.df <- read.csv("Result_tables/other/family_counts_abundances_and_metadata.csv")
# order_data.df <- read.csv("Result_tables/other/order_counts_abundances_and_metadata.csv")
# class_data.df <- read.csv("Result_tables/other/class_counts_abundances_and_metadata.csv")
phylum_data.df <- read.csv("Result_tables/other/phylum_counts_abundances_and_metadata.csv")


# Remove samples from the data that are not in the metadata
otu_data.df <- otu_data.df[otu_data.df$Sample %in% as.character(metadata.df$Index),]
phylum_data.df <- phylum_data.df[phylum_data.df$Sample %in% as.character(metadata.df$Index),]

# Remove samples from the metadata not in the data
metadata.df <- metadata.df[metadata.df$Index %in% otu_data.df$Sample,]

# Set rownames
rownames(metadata.df) <- metadata.df$Index

dim(metadata.df)
dim(otu_data.df)
dim(phylum_data.df)


# Total bacterial read count for each sample
# lesion type (unpooled and pooled)
phylum_data_filtered.df <- phylum_data.df[grepl("Bacteria", phylum_data.df$Domain),]

bacterial_summed_read_counts.df <- 
  phylum_data_filtered.df %>% 
  group_by(Sample) %>%
  summarise(Bacterial_reads_total = sum(Read_count)) %>%
  as.data.frame()

bacterial_summed_read_counts.df <- merge(bacterial_summed_read_counts.df, metadata.df,by.x = "Sample", by.y = "Index")



bacterial_summed_read_counts.df
ggplot(bacterial_summed_read_counts.df, aes(x= Patient_grouping, y = Bacterial_reads_total, fill = Patient_grouping)) +
  stat_boxplot(stat = "identity") +
  geom_point()

ggplot(bacterial_summed_read_counts.df, aes(x= Sampletype, y = Bacterial_reads_total, fill = Sampletype)) +
  stat_boxplot(stat = "identity") +
  geom_point()




# bacterial read count for each lesion type (unpooled and pooled)
# mean read count for each lesion type (unpooled and pooled)
# number of features for each lesion type (unpooled and pooled)



