# Generate input count tables for fastspar analysis, at both the feature and genus levels:
#   For each subject
#   For each lesion type within a cohort
#   For SCC_PL and SCC together (previous paper did this)

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()
library(reshape2)
library(dplyr)


filter_count_matrix <- function(mymatrix, row_min = 0, prevalence = 0){
  internal_matrix <- mymatrix
  internal_matrix <- internal_matrix[which(apply(internal_matrix, 1, max) >= row_min), ]
  # keep only OTUs/taxa that are in more than this fraction of samples
  filter_fraction <- prevalence
  entry_prevalences <- apply(internal_matrix, 1, function(x) {length(which(x > 0))})/dim(internal_matrix)[2]
  entries_from_prevalences <- names(entry_prevalences)[entry_prevalences > filter_fraction]
  entries_from_prevalences <- entries_from_prevalences[!is.na(entries_from_prevalences)]
  return(internal_matrix[entries_from_prevalences,])
}

setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")

# Load metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", header =T)

# Load count tables
otu.df <- read.csv("Result_tables/count_tables/Otu_counts.csv", header =T)
genus.df <- read.csv("Result_tables/count_tables/Genus_counts.csv", header =T)

otu_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/OTU_counts_abundances_and_metadata.csv",header = T)
genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Genus_counts_abundances_and_metadata.csv",header = T)

# Generate taxonomy summary for each cohort
otu_taxa_summary.df <- generate_taxa_summary(mydata = otu_data.df,taxa_column = "OTU.ID",
                                               group_by_columns = c("Cohort"))
genus_taxa_summary.df <- generate_taxa_summary(mydata = genus_data.df,taxa_column = "taxonomy_genus",
                                               group_by_columns = c("Cohort"))

# Filter summary to entries that are at least in 10% of samples from either cohort
otu_taxa_summary_filtered.df <- otu_taxa_summary.df[which(otu_taxa_summary.df$Percent_group_samples >= 10),]
genus_taxa_summary_filtered.df <- genus_taxa_summary.df[which(genus_taxa_summary.df$Percent_group_samples >= 10),]

# Filter the count dataframes to prevelant taxa
otu_filtered.df <- otu.df[otu.df$OTU.ID %in% as.character(unique(otu_taxa_summary_filtered.df$OTU.ID)),]
genus_filtered.df <- genus.df[genus.df$taxonomy_genus %in% as.character(unique(genus_taxa_summary_filtered.df$taxonomy_genus)),]

# 169 suppressed and 214 competent
# dim(subset(metadata.df, Cohort == "immunocompetent"))
# dim(subset(metadata.df, Cohort == "immunosuppressed"))

dim(genus.df)
dim(genus_filtered.df)
genus_filtered.df[apply(df2matrix(genus_filtered.df), 1, max),]



dim(otu.df)
dim(otu_filtered.df)

# For each patient
for (patient in unique(metadata.df$Patient)){
  patient_samples <- as.character(subset(metadata.df, Patient == patient)$Index)
  if (length(patient_samples) < 2){
    print(paste0("Patient ", patient, " has less than 2 samples, skipping"))
    next()
  }
  otu_patient_data.df <- otu_filtered.df[,c("OTU.ID", patient_samples)]
  genus_patient_data.df <- genus_filtered.df[,c("taxonomy_genus", patient_samples)]

  otu_patient_data.df <- otu_patient_data.df[otu_patient_data.df$OTU.ID %in% rownames(filter_count_matrix(df2matrix(otu_patient_data.df),row_min = 1)),]
  genus_patient_data.df <- genus_patient_data.df[genus_patient_data.df$taxonomy_genus %in% rownames(filter_count_matrix(df2matrix(genus_patient_data.df),row_min = 1)),]
  
  names(otu_patient_data.df)[1] <- "#OTU ID"
  names(genus_patient_data.df)[1] <- "#OTU ID"
  
  write.table(x = otu_patient_data.df, file = paste0("Result_tables/fastspar_inputs/per_patient/", patient, "__otu_counts_fastspar.tsv"), sep = "\t", quote = F, row.names = F)
  write.table(x = genus_patient_data.df, file = paste0("Result_tables/fastspar_inputs/per_patient/", patient, "__genus_counts_fastspar.tsv"), sep = "\t", quote = F, row.names = F)
}

# For each lesion_type_refined in each cohort
for (cohort in as.character(unique(metadata.df$Cohort))){
  for (lesion in as.character(unique(metadata.df$Lesion_type_refined))){
    sample_list <- as.character(subset(metadata.df, Cohort == cohort & Lesion_type_refined == lesion)$Index)
    if (length(sample_list) < 2){
      print(paste0("Cohort ", cohort, " +", " lesion ", lesion, " has less than 2 samples, skipping"))
      next()
    }
    otu_subset_data.df <- otu_filtered.df[,c("OTU.ID", sample_list)]
    genus_subset_data.df <- genus_filtered.df[,c("taxonomy_genus", sample_list)]
    
    otu_subset_data.df <- otu_subset_data.df[otu_subset_data.df$OTU.ID %in% rownames(filter_count_matrix(df2matrix(otu_subset_data.df),row_min = 1)),]
    genus_subset_data.df <- genus_subset_data.df[genus_subset_data.df$taxonomy_genus %in% rownames(filter_count_matrix(df2matrix(genus_subset_data.df),row_min = 1)),]
    
    names(otu_subset_data.df)[1] <- "#OTU ID"
    names(genus_subset_data.df)[1] <- "#OTU ID"
    
    write.table(x = otu_subset_data.df, file = paste0("Result_tables/fastspar_inputs/per_lesion_type_cohort/", cohort, "_", lesion, "__otu_counts_fastspar.tsv"), sep = "\t", quote = F, row.names = F)
    write.table(x = genus_subset_data.df, file = paste0("Result_tables/fastspar_inputs/per_lesion_type_cohort/", cohort, "_", lesion, "__genus_counts_fastspar.tsv"), sep = "\t", quote = F, row.names = F)
  }
}
