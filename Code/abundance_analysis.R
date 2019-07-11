# OLD!!!!!!
# DO NOT USE


library(ggplot2)
library(dplyr)
library(reshape2)

####################################
# Define various colour palletes
my_colour_pallete <- c("#8dd3c7","#ffffb3","#bebada","#fb8072", "#80b1d3", "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5", "#cc0000")
# From http://tools.medialab.sciences-po.fr/iwanthue/
my_colour_pallete_20 <- c("#66bd79","#a35bcf","#5bb643","#d14ea6","#a2b239","#5c6bcc","#dc892e","#5e93cd","#d64737","#49b6a8","#dc3c6e","#4f7e3c","#bd8cd5","#caab55","#914c88","#867230","#df82a2","#a65429","#ab4a5a","#e0896a")
my_colour_pallete_20_distinct <- c("#0057b4","#7fff56","#d600bc","#d8d500","#e76eff","#019932","#9f8fff","#ffc730","#007fac","#a20019","#06fefd","#ff6782","#00774c","#e0c8ff","#717a00","#4b2952","#e2ed7d","#46321e","#ffbd76","#ffb4c6")
my_colour_pallete_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_pallete_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
my_colour_pallete_15 <- c("#77b642","#7166d9","#cfa240","#b351bb","#4fac7f","#d44891","#79843a","#c68ad4","#d15a2c","#5ba7d9","#ce4355","#6570ba","#b67249","#9b4a6f","#df8398")
my_colour_pallete_32_distinct <- c("#ea7e00","#ca0074","#d1c69b","#474007","#bb00ad","#9c80ff","#be3300","#542e72","#00b9f5","#09436b","#8b0036","#9ac8e6","#ff1059","#959eff","#154a11","#0290f4","#ff7762","#7dbf00","#ff8194","#834c00","#006e73","#f9bb5d","#d6c943","#017229","#00d3a8","#732427","#36e191","#6a8200","#efb3ea","#3227bb","#ff90e1","#e92a12")
my_colour_pallete_61_distinct <- c("#930084","#0dbe38","#922cbe","#aadd26","#8050e1","#e3d100","#6a77ff","#9ee34f","#ff6aed","#2d8e00","#ff2ba7","#88e66a","#f883ff","#9fb200","#015ccc","#fab400","#211359","#72e78b","#ff1084","#008228","#590068","#77e5a1","#cb0060","#01b385","#d62e09","#01d3d0","#9a0029","#019680","#9f0056","#006d26","#ff8ae0","#005123","#de9fff","#d17100","#018edb","#ff854f","#014b8a","#b9db81","#4c0043","#dad193","#00275b","#ff9466","#9e92ff","#9f5100","#91abff","#855d00","#0099c4","#630a00","#eac3f0","#575400","#ff77b0","#007363","#78003c","#acb388","#142142","#ffc3a2","#71001a","#ffbfc2","#3f2500","#6b4866","#845a47")
####################################

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
####################################
gm_mean = function(x, na.rm=TRUE){
  # The geometric mean, with some error-protection bits.
  exp(sum(log(x[x > 0 & !is.na(x)]), na.rm=na.rm) / length(x))
}
# gm_mean = function(x, na.rm=TRUE){
#   exp(mean(log(x), na.rm=na.rm) )
# }

# Center log ratio transform
clr = function(x, base=2){
  x <- log((x / gm_mean(x)), base)
  x[!is.finite(x) | is.na(x)] <- 0.0
  return(x)
}
####################################
# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")

# Load the metadata.df
metadata.df <- read.table("data/metadata.tsv", header = T, sep = "\t")

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

# Remove uncessary columns
metadata.df$Sampletype_2 <- NULL
metadata.df$Swab.sample.name <- NULL
metadata.df$Pool <- NULL

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC and SCC lesions. 
# Remove samples for different lesion types (nasal,scar,scar_PL,KA,KA_PL,VV,VV_PL,SF,SF_PL,other,other_PL) from metadata and otu table
metadata.df <- metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC"),]
pool_1 <- c("C","AK_PL","IEC_PL","SCC_PL")
pool_2 <- c("AK","IEC")
pool_3 <- c("AK_PL","IEC_PL","SCC_PL")


metadata.df$Sampletype_pooled <- factor(as.character(lapply(metadata.df$Sampletype, function(x) ifelse(x %in% pool_1, "NLC", ifelse(x %in% pool_2, "AK", "SCC")))))
metadata.df$Sampletype_pooled_C_sep <- factor(as.character(lapply(metadata.df$Sampletype, function(x) ifelse(x %in% pool_3, "NLC", 
                                                                                                             ifelse(x %in% pool_2, "AK", 
                                                                                                                    ifelse(x == "C", "C","SCC"))))))


# Load rarefied counts
otu_rare.df <- read.table(file = "Result_tables/count_tables/OTU_counts_rarefied.csv", sep = ",", header = T)
otu_species_rare.df <- read.table(file = "Result_tables/count_tables/Specie_counts_rarefied.csv", sep = ",", header = T)
otu_genus_rare.df <- read.table(file = "Result_tables/count_tables/Genus_counts_rarefied.csv", sep = ",", header = T)
otu_family_rare.df <- read.table(file = "Result_tables/count_tables/Family_counts_rarefied.csv", sep = ",", header = T)
otu_order_rare.df <- read.table(file = "Result_tables/count_tables/Order_counts_rarefied.csv", sep = ",", header = T)
otu_class_rare.df <- read.table(file = "Result_tables/count_tables/Class_counts_rarefied.csv", sep = ",", header = T)
otu_phylum_rare.df <- read.table(file = "Result_tables/count_tables/Phylum_counts_rarefied.csv", sep = ",", header = T)

# Melt count dataframes
otu_rare_melt.df <- melt(otu_rare.df, variable.name = "Index", value.name = "Count")
otu_species_rare_melt.df <- melt(otu_species_rare.df, variable.name = "Index", value.name = "Count")
otu_genus_rare_melt.df <- melt(otu_genus_rare.df, variable.name = "Index", value.name = "Count")
otu_family_rare_melt.df <- melt(otu_family_rare.df, variable.name = "Index", value.name = "Count")
otu_order_rare_melt.df <- melt(otu_order_rare.df, variable.name = "Index", value.name = "Count")
otu_class_rare_melt.df <- melt(otu_class_rare.df, variable.name = "Index", value.name = "Count")
otu_phylum_rare_melt.df <- melt(otu_phylum_rare.df, variable.name = "Index", value.name = "Count")

# Remove samples that are not in the metadata. Merge will do this, but good to be explicit.
otu_rare_melt.df <- otu_rare_melt.df[otu_rare_melt.df$Index %in% metadata.df$Index,]
otu_species_rare_melt.df <- otu_species_rare_melt.df[otu_species_rare_melt.df$Index %in% metadata.df$Index,]
otu_genus_rare_melt.df <- otu_genus_rare_melt.df[otu_genus_rare_melt.df$Index %in% metadata.df$Index,]
otu_family_rare_melt.df <- otu_family_rare_melt.df[otu_family_rare_melt.df$Index %in% metadata.df$Index,]
otu_order_rare_melt.df <- otu_order_rare_melt.df[otu_order_rare_melt.df$Index %in% metadata.df$Index,]
otu_class_rare_melt.df <- otu_class_rare_melt.df[otu_class_rare_melt.df$Index %in% metadata.df$Index,]
otu_phylum_rare_melt.df <- otu_phylum_rare_melt.df[otu_phylum_rare_melt.df$Index %in% metadata.df$Index,]


# Combine melted dataframes with metadata
otu_rare_melt.df <- merge(otu_rare_melt.df, metadata.df, by.x = "Index", by.y = "Index")
otu_species_rare_melt.df <- merge(otu_species_rare_melt.df, metadata.df, by.x = "Index", by.y = "Index")
otu_genus_rare_melt.df <- merge(otu_genus_rare_melt.df, metadata.df, by.x = "Index", by.y = "Index")
otu_family_rare_melt.df <- merge(otu_family_rare_melt.df, metadata.df, by.x = "Index", by.y = "Index")
otu_order_rare_melt.df <- merge(otu_order_rare_melt.df, metadata.df, by.x = "Index", by.y = "Index")
otu_class_rare_melt.df <- merge(otu_class_rare_melt.df, metadata.df, by.x = "Index", by.y = "Index")
otu_phylum_rare_melt.df <- merge(otu_phylum_rare_melt.df, metadata.df, by.x = "Index", by.y = "Index")


# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
samples_removed <- metadata.df$Index[!metadata.df$Index %in% colnames(otu_rare.df)]
metadata.df <- metadata.df[! metadata.df$Index %in% samples_removed,]
metadata.df$Patient <- factor(metadata.df$Patient)
metadata.df$Sampletype <- factor(metadata.df$Sampletype)



# Make consistent colour palletes. Associate each taxonomy string with a specific hex code.
my_colour_pallete_distinct_class <- setNames(rev(my_colour_pallete_61_distinct)[1:length(unique(otu_class_rare.df$taxonomy_class))], unique(otu_class_rare.df$taxonomy_class))
my_colour_pallete_distinct_phylum <- setNames(my_colour_pallete_32_distinct[1:length(unique(otu_phylum_rare.df$taxonomy_phylum))], unique(otu_phylum_rare.df$taxonomy_phylum))

## WORK IN PROGRESS

## Plots 
# Abundance of each taxa level up to class for each patient
# Abundance of each taxa level for each lesion type for each patient

# Turn counts into abundances 
temp <- otu_class_rare_melt.df %>% 
  # Grouping by the taxonomy is uncessary as we are using pre-grouped tables.
  group_by(Patient, Index) %>% 
  mutate(Abundance = round((Count/sum(Count))*100,3)) %>%
  arrange(Patient,Index, desc(Abundance)) %>%
  top_n(10, Abundance) %>%
  as.data.frame()

#temp <- temp[temp$Sampletype %in% c("SCC", "SCC_PL", "C"),]
#x_axis_labels <- temp$
ggplot(temp, aes(x = Index, y = Abundance, fill = taxonomy_class)) +
  geom_histogram(stat = "identity") +
  #scale_x_discrete(labels = Sampletype) +
  scale_fill_manual(values = my_colour_pallete_distinct_class, guide = guide_legend(ncol = 2)) +
  facet_wrap(~Patient, scales = "free_x") +
  common_theme +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 8),
    #legend.key.size = unit(.1, units = "cm"),
    legend.key.width= unit(.2, units = "cm"),
    legend.key.height = unit(.2, units = "cm")
    )
  


sample_phylum_abundances <- tax_separated_table_unfiltered%>% 
  group_by(sample, Phylum) %>% # Group by feeding type and phylum
  dplyr::summarise (Abundance = sum(Abundance)) %>% # For eaching feeding type, sum the % abundances for all genomes in the phylum
  mutate(Abundance = round((Abundance / sum(Abundance))*100, 2)) %>% #re-normalise the abundance for each phyla by all abundances
  arrange(sample, desc(Abundance)) %>%
  filter(Abundance > 0) %>%
  as.data.frame()


relative_abundance_phylum <- ggplot(feeding_type_phylum, aes(x = Feeding_type, y = Abundance, fill = Phylum)) + 
  xlab("Feeding type") + 
  ylab("Abundance %") + 
  #geom_histogram(stat = "identity",color = "black",size = .2) +
  geom_histogram(stat = "identity") +
  scale_fill_manual(values = my_colour_pallete_30_distinct_phylum) + 
  scale_y_continuous(breaks = seq(0,100,by = 10), limits = c(0,101)) +
  scale_x_discrete(labels = c("EN", "PN","EN+PN", "oral")) +
  common_theme +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        #legend.key.size = unit(.1, units = "cm"),
        legend.key.width= unit(.2, units = "cm"),
        legend.key.height = unit(.2, units = "cm"))