

library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)

setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")


metadata.df <- read.table("data/metadata_immunosuppressed_competent.tsv", header = T, sep = "\t")

qcpr.df <- read.table("data/qcpr_data.tsv", sep = "\t", header = T)

# qcpr.df[qcpr.df == "Undetermined" | qcpr.df == ""] <- NA
qcpr.df <- data.table(qcpr.df)
for (col in names(qcpr.df)) set(qcpr.df, i = which(qcpr.df[[col]] %in% c("Undetermined","")),j=col, value = NA)
as.data.frame(qcpr.df)
qcpr.df$Ct.Mean <- NULL

qcpr.df <- qcpr.df %>% 
  group_by(Sample.Name) %>%
  mutate(CT_ID = paste0("CT_", 1:n()),
         Quantity_ID = paste0("Quantity_", 1:n()))

# dcast(setDT(qcpr.df), Sample.Name ~ CT_ID+Quantity_ID, value.var = c("CT", "Quantity"))

qcpr.df <- pivot_wider(qcpr.df, names_from = c("CT_ID", "Quantity_ID"), values_from = c("CT", "Quantity"))
names(qcpr.df) <- c("Swab_ID", paste0("CT_",seq(1,3)),paste0("CT_Quantity_",seq(1,3)))

dim(metadata.df)
qcpr.df$Swab_ID <- factor(qcpr.df$Swab_ID)
metadata.df <- left_join(metadata.df, qcpr.df, by = "Swab_ID")
dim(metadata.df)

write.table()

