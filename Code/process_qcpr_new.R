

library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)

setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")


metadata.df <- read.table("data/metadata_immunosuppressed_competent.tsv", header = T, sep = "\t")
metadata.df$Swab_ID <- factor(metadata.df$Swab_ID)
temp <- read.table(pipe("pbpaste"), sep = "\t", header = T)
temp$Swab_ID <- factor(temp$Swab_ID)
metadata.df <- left_join(metadata.df, temp, by = c("Swab_ID" = "Swab_ID"))
write.csv(x = metadata.df, file = "test.csv", quote = F, row.names = F)

qcpr.df <- read.table("data/qcpr_data_120620.tsv", sep = "\t", header = T)

qcpr.df <- qcpr.df[,c("Sample", "Assay", "Mean.c.ul")]
qcpr.df <- qcpr.df[!is.na(qcpr.df$Mean.c.ul),]
qcpr.df <- qcpr.df[qcpr.df$Mean.c.ul != "",]

# qcpr.df[qcpr.df == "Undetermined" | qcpr.df == ""] <- NA
qcpr.df <- data.table(qcpr.df)
for (col in names(qcpr.df)) set(qcpr.df, i = which(qcpr.df[[col]] %in% c("Undetermined","")),j=col, value = NA)
as.data.frame(qcpr.df)
# qcpr.df$Ct.Mean <- NULL
head(qcpr.df)

qcpr.df <- dcast(qcpr.df,formula = Sample~Assay)

# qcpr.df <- qcpr.df %>% 
#   group_by(Sample, Assay) %>%
#   mutate(CT_ID = paste0(Assay, "_CT_", 1:n()))
# head(qcpr.df)

# qcpr.df <- data.table(qcpr.df[!is.na(qcpr.df$Ct),])
# qcpr.df <- data.table(qcpr.df)
# qcpr.df <- qcpr.df[,-"Assay"]
# head(qcpr.df)
# dcast(setDT(qcpr.df), Sample.Name ~ CT_ID+Quantity_ID, value.var = c("CT", "Quantity"))
# unique(qcpr.df$Sample) %in% metadata.df$Swab_ID

# qcpr.df <- data.table(pivot_wider(qcpr.df, names_from = c("CT_ID"), values_from = c("Ct")))

names(qcpr.df)<- c("Swab_ID", "S.aureus Geq/ul x5", "Staph spp. Geq/ul x5")

qcpr.df <- unique(qcpr.df)
# names(qcpr.df) <- c("Swab_ID", "Assay", paste0("CT_",seq(1,7)))

qcpr.df[qcpr.df$Swab_ID == "1191",]

dim(metadata.df)
qcpr.df$Swab_ID <- factor(qcpr.df$Swab_ID)
# metadata.df$Swab_ID
metadata.df <- left_join(metadata.df, qcpr.df, by = "Swab_ID")
dim(metadata.df)
# metadata.df$Swab_ID == 

write.csv(x = metadata.df, file = "test.csv", quote = F, row.names = F)

