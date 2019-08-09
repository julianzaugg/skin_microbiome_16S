# Perform differential abundance analysis 
# - Comparing lesion types within patient
# - Comparing the same lesion types between cohorts
# - ?Compare patients that have a SCC / no SCC but have AK or IEC / or just Normal

library(DESeq2)
# library(BiocParallel)



############################################################
######################## Functions #########################

# Function that calculates the geometric mean with some error-protection bits. 
# DESeq2 does not appear to work (will throw an error) if every OTU (or genus or genome etc.) 
# contains at least one count of zero in every row of the count data.
# Specifically, the function "dds<-DESeq(dds, betaPrior = FALSE)" will fail
# One way to address this is to use the function below as input to DESeq2 to transform the data.
# Calculate the geometric means prior to estimating the size factors
gm_mean = function(x, na.rm=TRUE){
  # The geometric mean, with some error-protection bits.
  exp(sum(log(x[x > 0 & !is.na(x)]), na.rm=na.rm) / length(x))
}

# Center log ratio transform
clr = function(x, base=2){
  x <- log((x / gm_mean(x)), base)
  x[!is.finite(x) | is.na(x)] <- 0.0
  return(x)
}

matrix2df <- function(mymatrix, column_name){
  out <- as.data.frame(mymatrix)
  out_names <- colnames(out)
  out$placeholder <- rownames(out)
  rownames(out) <- NULL
  names(out)[length(names(out))] <- column_name
  out <- out[,c(column_name, out_names)]
  return(out)
}

# For each rowname (OTU), get the corresponding taxonomy_species
# Assumes "OTU.ID" and "taxonomy_species" columns in the provided map dataframe
assign_taxonomy_to_otu <- function(otutable, taxon_map){
  taxonomies <- c()
  for (otuid in rownames(otutable)){
    taxonomies <- c(taxonomies, as.character(taxon_map[taxon_map$OTU.ID == otuid,]$taxonomy_species))
  }
  return(taxonomies)
}

# Filter and sort DESeq result tables
filter_and_sort_dds_results <- function(x, p_value_threshold = 0.05){
  filtered_table <- x
  filtered_table <- filtered_table[!is.na(filtered_table$padj),]
  filtered_table <- filtered_table[filtered_table$padj <= p_value_threshold,]
  filtered_table <- filtered_table[order(filtered_table$padj),]
  return(filtered_table)
}


filter_matrix_rows <- function(my_matrix, row_max){
  rows_before <- dim(my_matrix)[1]
  rows_after <- dim(my_matrix[apply(my_matrix,1,max) >= row_max,])[1]
  print(paste0("Rows before = ", rows_before))
  print(paste0("Rows after = ", rows_after))
  print(paste0("Lost % = ", round((rows_before-rows_after)/rows_before*100, 2), "%"))
  return(my_matrix[apply(my_matrix,1,max) >= row_max,])
}

############################################################

# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")


# Load count table at the OTU level. These are the counts for OTUs that were above our abundance thresholds
otu_rare.m <- as.matrix(read.table("Result_tables/count_tables/OTU_counts_rarefied.csv", sep =",", header =T, row.names = 1))
genus_rare.m <- as.matrix(read.table("Result_tables/count_tables/Genus_counts_rarefied.csv", sep =",", header =T, row.names = 1))

# Filter out features that do not have at # reads in at least one sample
head(melt(sort(colSums(otu_rare.m))))
otu_rare.m <- filter_matrix_rows(otu_rare.m,15)
# dim(otu_rare.m[apply(otu_rare.m, 1, max) == 0,])
head(melt(sort(colSums(otu_rare.m))))

genus_rare.m <- filter_matrix_rows(genus_rare.m,15)

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# We are only interested in C,AK_PL,IEC_PL,SCC_PL,AK,IEC, NLC and SCC lesions. 
# metadata.df <- metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC", "NLC"),]
metadata.df <- metadata.df[metadata.df$Sampletype %in% c("C","AK_PL","IEC_PL","SCC_PL","AK","IEC","SCC", "LC"),]

# Filter to snapshot or immunocompromised samples
metadata.df <- subset(metadata.df,Project == "immunocompromised" | Snapshot_sample == "yes")

# Only keep columns (samples) in the metadata
otu_rare.m <- otu_rare.m[,colnames(otu_rare.m) %in% as.character(metadata.df$Index)]
genus_rare.m <- genus_rare.m[,colnames(genus_rare.m) %in% as.character(metadata.df$Index)]

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
samples_removed <- metadata.df$Index[!metadata.df$Index %in% colnames(otu_rare.m)]
metadata.df <- metadata.df[! metadata.df$Index %in% samples_removed,]

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# Rownames should match the sample columns in the otu table
rownames(metadata.df) <- metadata.df$Index

# Order the otu_tables the same order as the metadata
otu_rare.m <- otu_rare.m[,rownames(metadata.df)]
genus_rare.m <- genus_rare.m[,rownames(metadata.df)]

dim(otu_rare.m)
dim(metadata.df)

# Ensure names of the otu / genus count matrices match the order of the metadata.df!
# Assumes number of samples in metadata.df and count data are the same
all(colnames(otu_rare.m) == metadata.df$Index) # Should be 'True'
all(colnames(otu_rare.m) == rownames(metadata.df)) # Should be 'True'

### Create new variables where different lesions types have been grouped
metadata.df$Sampletype_pooled_cohort <- factor(paste0(metadata.df$Sampletype_pooled, "_", metadata.df$Project))
metadata.df$Sampletype_final_cohort <- factor(paste0(metadata.df$Sampletype_final, "_", metadata.df$Project))

# Factorise other variables
metadata.df$Sampletype_pooled <- factor(metadata.df$Sampletype_pooled)
metadata.df$Sampletype_final <- factor(metadata.df$Sampletype_final)

# Create cohort specific data sets
immunocompromised_metadata.df <- metadata.df[metadata.df$Project == "immunocompromised",]
immunocompromised_otu_rare.m <- otu_rare.m[,rownames(immunocompromised_metadata.df)]
immunocompromised_genus_rare.m <- genus_rare.m[,rownames(immunocompromised_metadata.df)]

immunocompetent_metadata.df <- metadata.df[metadata.df$Project == "immunocompetent",]
immunocompetent_otu_rare.m <- otu_rare.m[,rownames(immunocompetent_metadata.df)]
immunocompetent_genus_rare.m <- genus_rare.m[,rownames(immunocompetent_metadata.df)]


# ---------------------------------------------------------------------------------------------------------
# Perform differential abundance calculations at the OTU level and genus level, 
# comparing between the groups within variables of interest

# DESeq requires a count matrix ('countData'), a corresponding metadata.df ('colData') and a 'design' formula. The formula expresses
# how the counts for each OTU/genus depend on the variables defined in the 'colData'. See help(DESeqDataSetFromMatrix) for more information.
# The first column of the metadata.df ('colData') must match the ordering of the columns of the countData

patient_lesion_counts <- data.frame()
for (patient in unique(metadata.df$Patient)){
  temp <- melt(summary(factor(metadata.df[metadata.df$Patient == patient,]$Sampletype_pooled)), value.name = "Count")
  temp$Lesion <- rownames(temp)
  rownames(temp) <- NULL
  temp$Patient <- patient
  patient_lesion_counts <- rbind(patient_lesion_counts, temp)
}
print(patient_lesion_counts,row.names = F)


# Compare Sampletype_final within each patient (can use another lesion grouping)
run_per_patient_deseq <- function(my_otu_matrix, my_metadata, variable = "Sampletype_final", cohort, my_levels = NULL, assign_taxonomy = T){

  all_patients_combined_results.df <- data.frame()
  for (patient in unique(my_metadata$Patient)){ # for each patient
    print(paste0("processing ", patient ))
    patient_combined_results.df <- data.frame()
    patient_metadata.df <- my_metadata[my_metadata$Patient == patient,] # Get the patient metadata
    patient_samples.v <- as.character(patient_metadata.df$Index) # Get the number of samples associated with the patient
    patient_sample_lesion_types.v <- factor(patient_metadata.df[,variable]) # get the lesion types
    
    # If the number of samples is 1 or there is only one lesion type/group
    if (length(patient_samples.v) == 1 || length(unique(patient_sample_lesion_types.v)) == 1){
      next
    }
    print(melt(summary(patient_sample_lesion_types.v), value.name = "Count"))
    
    # Extract the samples counts specific to the patient
    patient_feature_table.m <- my_otu_matrix[,patient_samples.v]
    
    # Can filter the features further if required
    # patient_feature_table.m <- patient_feature_table.m[which(apply(patient_feature_table.m, 1, max) > 10),]
    
    # Order the patient feature table and the metadata to be the same
    patient_feature_table.m <- patient_feature_table.m[,rownames(patient_metadata.df)]
    patient_metadata.df <- patient_metadata.df[order(rownames(patient_metadata.df)),]
    
    # Refactor the variable column so that the levels are consistent
    if (!is.null(my_levels)){
      patient_metadata.df[,variable] <- factor(patient_metadata.df[,variable], levels = my_levels)
    } else{
      patient_metadata.df[,variable] <- factor(patient_metadata.df[,variable], levels = sort(unique(as.character(patient_metadata.df[,variable]))))  
    }
    
    # If the column and rownames do not match, entries are missing
    if (!all(rownames(patient_metadata.df) == colnames(patient_feature_table.m))){
      print("Colnames and metadata names don't match!!!")
      break
    }
    
    # if (max(apply(patient_feature_table.m, 1, min)) == 0) {
    #   patient_feature_table.m = patient_feature_table.m + 1
    # }
    
    # Run DESeq
    dds <-DESeqDataSetFromMatrix(countData = patient_feature_table.m, colData = patient_metadata.df, design = as.formula(paste0("~",variable)))
    geoMeans <- apply(counts(dds), 1, gm_mean)
    dds <- estimateSizeFactors(dds, geoMeans = geoMeans)
    dds <- try(DESeq(dds, test = "Wald", fitType = "parametric", parallel = T))
    
    if(inherits(dds, "try-error")) {
      next
    }
    
    # Get the lesion/sampletype combinations
    # sample_type_combinations <- combn(as.character(unique(patient_metadata.df[,variable])), 2)
    # Sort to be consistent between patients
    # sample_type_combinations <- combn(sort(unique(as.character(patient_metadata.df[,variable]))),2)
    if (!is.null(my_levels)){
      my_levels_filtered <- unique(my_levels[my_levels %in% patient_sample_lesion_types.v])
      sample_type_combinations <- combn(rev(my_levels_filtered), 2)
    }else{
      sample_type_combinations <- combn(sort(unique(as.character(patient_metadata.df[,variable]))),2)
    }
    print(sample_type_combinations)
    for (i in 1:ncol(sample_type_combinations)){
      # Set group 1 and group 2
      group_1 <- as.character(sample_type_combinations[1,i])
      group_2 <- as.character(sample_type_combinations[2,i])
      
      # Get the number of samples in each group
      n_group_1 <- dim(subset(patient_metadata.df, get(variable) == group_1))[1]
      n_group_2 <- dim(subset(patient_metadata.df, get(variable) == group_2))[1]
      
      print(paste0("processing : ", patient, "_", group_1, "_vs_", group_2))
      
      # Get the results from contrasting these groups
      resMFSource <- results(dds, contrast = c(variable,group_1,group_2), alpha=0.01, independentFiltering = F, cooksCutoff = F, parallel = T)
      
      resMFSource$Group_1 <- group_1
      resMFSource$Group_2 <- group_2
      resMFSource$Variable <- patient
      resMFSource$N_Group_1 <- n_group_1
      resMFSource$N_Group_2 <- n_group_2
      
      # Assign the taxonomy
      if (assign_taxonomy == T){
        resMFSource$Taxonomy <- assign_taxonomy_to_otu(resMFSource, otu_taxonomy_map.df)   
        # Convert to dataframe
        resMFSource <- matrix2df(resMFSource, "OTU")
      } else{
        # Convert to dataframe
        resMFSource <- matrix2df(resMFSource, "Taxonomy")
      }
      
      # Order the results by the adjusted p-value and filter out entries with p-values below threshold
      resMFSourceOrdered <- filter_and_sort_dds_results(resMFSource, 0.01)
      
      # Add the result to the combined dataframe for the patient
      patient_combined_results.df <- rbind(patient_combined_results.df, resMFSourceOrdered)
      all_patients_combined_results.df <- rbind(all_patients_combined_results.df, resMFSourceOrdered)
    }
    # Write the results for the patient to file
    if (assign_taxonomy == T){
      result_name <- paste0(patient,"_otu__",variable)
      outfilename <- paste("Result_tables/DESeq_results/by_patient/",cohort, "_", result_name, ".csv", sep= "")
    } else{
      result_name <- paste0(patient,"_genus__",variable)
      outfilename <- paste("Result_tables/DESeq_results/by_patient/",cohort, "_", result_name, ".csv", sep= "")
    }
    write.csv(patient_combined_results.df, file=outfilename, quote = F, row.names = F)
  }
  # Write the results for all patients to file (assumes no errors and all patient results generated at the same time)
  if (assign_taxonomy == T){
    outfilename <- paste("Result_tables/DESeq_results/by_patient/",cohort, "_patient_otu__", variable, "_combined.csv", sep= "")
  } else{
    outfilename <- paste("Result_tables/DESeq_results/by_patient/",cohort, "_patient_genus__", variable, "_combined.csv", sep= "")
  }
  
  write.csv(all_patients_combined_results.df, file=outfilename, quote = F, row.names = F)
}

run_per_patient_deseq(immunocompromised_otu_rare.m,immunocompromised_metadata.df,  "Sampletype_final", cohort="immunocompromised", my_levels <- c("C","LC", "AK", "SCC"),assign_taxonomy = T)
run_per_patient_deseq(immunocompromised_genus_rare.m,immunocompromised_metadata.df, "Sampletype_final", cohort="immunocompromised", my_levels <- c("C","LC", "AK", "SCC"),assign_taxonomy = F)

run_per_patient_deseq(immunocompetent_otu_rare.m,immunocompetent_metadata.df, "Sampletype_final", cohort="immunocompetent", my_levels <- c("LC", "AK", "SCC"),assign_taxonomy = T)
run_per_patient_deseq(immunocompetent_genus_rare.m,immunocompetent_metadata.df, "Sampletype_final", cohort="immunocompetent", my_levels <- c("LC", "AK", "SCC"),assign_taxonomy = F)


# Comparing the same lesion types between cohorts
# Always compare compromised vs competent
run_lesion_cohorts_deseq <- function(my_otu_matrix, variable = "Sampletype_final", assign_taxonomy = T){

  all_combined_results.df <- data.frame()
  for (lesion in unique(metadata.df[,variable])){
    lesion_results.df <- data.frame()
    lesion_metadata.df <- metadata.df[metadata.df[,variable] == lesion,]
    
    # Create column with the variable and the project (cohort)
    variable_project_name <- paste0(variable,"_Project")
    lesion_metadata.df[,variable_project_name] <- factor(with(lesion_metadata.df, paste0(get(variable), "_", Project)))
    
    lesion_samples.v <- as.character(lesion_metadata.df$Index) # Get the samples associated with the lesion type
    lesion_cohorts.v <- factor(lesion_metadata.df$Project) # get the lesion cohorts

    # If the number of samples is 1 or there is only one cohort
    if (length(lesion_samples.v) == 1 || length(unique(lesion_cohorts.v)) == 1){
      next
    }
    
    # Extract the samples counts specific to the lesion
    lesion_feature_table.m <- my_otu_matrix[,lesion_samples.v]
    
    # Can filter the features further if required
    # lesion_feature_table.m <- lesion_feature_table.m[which(apply(lesion_feature_table.m, 1, max) > 10),]
    
    # Order the feature table and the metadata to be the same
    lesion_feature_table.m <- lesion_feature_table.m[,order(rownames(lesion_metadata.df))]
    lesion_metadata.df <- lesion_metadata.df[order(rownames(lesion_metadata.df)),]
    
    # Since we know the cohorts we are processing, and we know we are processing one lesion type at a time,
    # make it so we only compare immunocompromised to immunocompetent
    my_levels <- c(paste0(lesion, "_immunocompromised"), paste0(lesion, "_immunocompetent"))
    sample_type_combinations <- combn(rev(my_levels), 2)

    # Refactor the variable + project column so that the levels are consistent
    lesion_metadata.df[,variable_project_name] <- factor(lesion_metadata.df[,variable_project_name], levels = my_levels)  
    
    # If the column and rownames do not match, entries are missing
    if (!all(rownames(lesion_metadata.df) == colnames(lesion_feature_table.m))){
      print("Colnames and metadata names don't match!!!")
      break
    }
    
    # Run DESeq
    dds <-DESeqDataSetFromMatrix(countData = lesion_feature_table.m, colData = lesion_metadata.df, design = as.formula(paste0("~",variable_project_name)))
    geoMeans <- apply(counts(dds), 1, gm_mean)
    dds <- estimateSizeFactors(dds, geoMeans = geoMeans)
    dds <- try(DESeq(dds, test = "Wald", fitType = "parametric", parallel = T))

    if(inherits(dds, "try-error")) {
      next
    }

    # sample_type_combinations <- combn(rev(my_levels), 2)
    sample_type_combinations <- combn(my_levels, 2)
    
    for (i in 1:ncol(sample_type_combinations)){
      # Set group 1 and group 2
      group_1 <- as.character(sample_type_combinations[1,i])
      group_2 <- as.character(sample_type_combinations[2,i])
      
      # Get the number of samples in each group
      n_group_1 <- dim(subset(lesion_metadata.df, get(variable_project_name) == group_1))[1]
      n_group_2 <- dim(subset(lesion_metadata.df, get(variable_project_name) == group_2))[1]
      
      group_1_meta <- subset(lesion_metadata.df, get(variable_project_name) == group_1)
      group_2_meta <- subset(lesion_metadata.df, get(variable_project_name) == group_2)
      n_patients_group_1 <- length(unique(group_1_meta$Patient))
      n_patients_group_2 <- length(unique(group_2_meta$Patient))
      
      print(paste0("processing : ", lesion, "_", group_1, "_vs_", group_2))
      
      # Get the results from contrasting these groups
      resMFSource <- results(dds, contrast = c(variable_project_name, group_1,group_2), alpha=0.01, independentFiltering = F, cooksCutoff = F, parallel = T)
      
      resMFSource$Group_1 <- group_1
      resMFSource$Group_2 <- group_2
      resMFSource$Variable <- lesion
      resMFSource$N_Group_1 <- n_group_1
      resMFSource$N_Group_2 <- n_group_2
      resMFSource$N_patients_Group_1 <- n_patients_group_1
      resMFSource$N_patients_Group_2 <- n_patients_group_2
      
      # Assign the taxonomy
      if (assign_taxonomy == T){
        resMFSource$Taxonomy <- assign_taxonomy_to_otu(resMFSource, otu_taxonomy_map.df)   
        # Convert to dataframe
        resMFSource <- matrix2df(resMFSource, "OTU")
      } else{
        # Convert to dataframe
        resMFSource <- matrix2df(resMFSource, "Taxonomy")
      }
      
      # Order the results by the adjusted p-value and filter out entries with p-values below threshold
      resMFSourceOrdered <- filter_and_sort_dds_results(resMFSource, 0.01)
      
      # Add the result to the combined dataframe for the patient
      lesion_results.df <- rbind(lesion_results.df, resMFSourceOrdered)
      all_combined_results.df <- rbind(all_combined_results.df, resMFSourceOrdered)
    }
    # Write the results for the lesion to file
    if (assign_taxonomy == T){
      result_name <- paste0(lesion,"_otu__",variable)
      outfilename <- paste("Result_tables/DESeq_results/by_lesion_cohort/", result_name, ".csv", sep= "")
    } else{
      result_name <- paste0(lesion,"_genus__",variable)
      outfilename <- paste("Result_tables/DESeq_results/by_lesion_cohort/", result_name, ".csv", sep= "")
    }
    write.csv(lesion_results.df, file=outfilename, quote = F, row.names = F)
  }
  # Write the results
  if (assign_taxonomy == T){
    outfilename <- paste("Result_tables/DESeq_results/by_lesion_cohort/lesion_cohort_otu_combined.csv", sep= "")
  } else{
    outfilename <- paste("Result_tables/DESeq_results/by_lesion_cohort/lesion_cohort_genus_combined.csv", sep= "")
  }
  write.csv(all_combined_results.df, file=outfilename, quote = F, row.names = F)
}

run_lesion_cohorts_deseq(otu_rare.m, "Sampletype_final",assign_taxonomy = T)
run_lesion_cohorts_deseq(genus_rare.m, "Sampletype_final",assign_taxonomy = F)


# Compare all lesion types within a cohort
run_cohort_lesion_type_deseq <- function(my_otu_matrix, my_metadata, cohort, assign_taxonomy = T){
  
  internal_otu_matrix.m <- my_otu_matrix
  internal_metadata.df <- my_metadata
  
  # Ensure factored
  # internal_metadata.df$Sampletype_compromised_refined <- factor(internal_metadata.df$Sampletype_compromised_refined)
  internal_metadata.df$Sampletype_compromised_refined <- factor(internal_metadata.df$Sampletype_final)
  
  # Order the feature table and the metadata to be the same
  internal_otu_matrix.m <- internal_otu_matrix.m[,order(rownames(internal_metadata.df))]
  internal_metadata.df <- internal_metadata.df[order(rownames(internal_metadata.df)),]
  
  # If the column and rownames do not match, entries are missing
  if (!all(rownames(internal_metadata.df) == colnames(internal_otu_matrix.m))){
    print("Colnames and metadata names don't match!!!")
    break
  }
  
  # Run DESeq
  dds <- DESeqDataSetFromMatrix(countData = internal_otu_matrix.m, colData = internal_metadata.df, design = ~Sampletype_final)
  geoMeans <- apply(counts(dds), 1, gm_mean)
  dds <- estimateSizeFactors(dds, geoMeans = geoMeans)
  dds <- try(DESeq(dds, test = "Wald", fitType = "parametric", parallel = T))
  
  if(inherits(dds, "try-error")) {
    next
  }
  
  # Result dataframe
  all_combined_results.df <- data.frame()

  if (cohort == "immunocompromised"){
    my_levels <- c("C" ,"LC", "AK", "SCC")  
  } else{
    my_levels <- c("LC", "AK", "SCC")
  }
  sample_type_combinations <- combn(rev(my_levels), 2)
  
  for (i in 1:ncol(sample_type_combinations)){
    # Set group 1 and group 2
    group_1 <- as.character(sample_type_combinations[1,i])
    group_2 <- as.character(sample_type_combinations[2,i])
    
    # Get the number of samples in each group
    n_group_1 <- dim(subset(internal_metadata.df, Sampletype_final == group_1))[1]
    n_group_2 <- dim(subset(internal_metadata.df, Sampletype_final == group_2))[1]
    
    print(paste0("processing : ", group_1, "_vs_", group_2))
    
    # Get the results from contrasting these groups
    resMFSource <- results(dds, contrast = c("Sampletype_final",group_1,group_2), alpha=0.01, independentFiltering = F, cooksCutoff = F, parallel = T)
    
    group_1_meta <- subset(internal_metadata.df, Sampletype_final == group_1)
    group_2_meta <- subset(internal_metadata.df, Sampletype_final == group_2)
    n_patients_group_1 <- length(unique(group_1_meta$Patient))
    n_patients_group_2 <- length(unique(group_2_meta$Patient))
    
    resMFSource$Group_1 <- group_1
    resMFSource$Group_2 <- group_2
    resMFSource$Variable <- "Sampletype_final"
    resMFSource$N_Group_1 <- n_group_1
    resMFSource$N_Group_2 <- n_group_2
    resMFSource$N_patients_Group_1 <- n_patients_group_1
    resMFSource$N_patients_Group_2 <- n_patients_group_2
    
    # Assign the taxonomy
    if (assign_taxonomy == T){
      resMFSource$Taxonomy <- assign_taxonomy_to_otu(resMFSource, otu_taxonomy_map.df)   
      # Convert to dataframe
      resMFSource <- matrix2df(resMFSource, "OTU")
    } else{
      # Convert to dataframe
      resMFSource <- matrix2df(resMFSource, "Taxonomy")
    }
    
    # Order the results by the adjusted p-value and filter out entries with p-values below threshold
    resMFSourceOrdered <- filter_and_sort_dds_results(resMFSource, 0.01)
    
    # Add the result to the combined dataframe
    all_combined_results.df <- rbind(all_combined_results.df, resMFSourceOrdered)
  }
  # Write the results
  if (assign_taxonomy == T){
    outfilename <- paste0("Result_tables/DESeq_results/",cohort, "_otu_sampletype_final.csv", sep= "")
  } else{
    outfilename <- paste0("Result_tables/DESeq_results/",cohort, "_genus_sampletype_final.csv", sep= "")
  }
  write.csv(all_combined_results.df, file=outfilename, quote = F, row.names = F)
}

run_cohort_lesion_type_deseq(immunocompromised_otu_rare.m, immunocompromised_metadata.df, "immunocompromised", assign_taxonomy = T)
run_cohort_lesion_type_deseq(immunocompromised_genus_rare.m, immunocompromised_metadata.df, "immunocompromised", assign_taxonomy = F)

run_cohort_lesion_type_deseq(immunocompetent_otu_rare.m, immunocompetent_metadata.df, "immunocompetent", assign_taxonomy = T)
run_cohort_lesion_type_deseq(immunocompetent_genus_rare.m, immunocompetent_metadata.df, "immunocompetent", assign_taxonomy = F)








# Number of medications. Compare those patients taking 1 vs 2 vs 3 medications
run_immunocompromised_n_meds_deseq <- function(my_otu_matrix, my_metadata, assign_taxonomy = T){
  
  internal_otu_matrix.m <- my_otu_matrix
  internal_metadata.df <- my_metadata
  # Ensure factored
  internal_metadata.df$Number_of_meds <- factor(internal_metadata.df$Number_of_meds)


  # Order the feature table and the metadata to be the same
  internal_otu_matrix.m <- internal_otu_matrix.m[,order(rownames(internal_metadata.df))]
  internal_metadata.df <- internal_metadata.df[order(rownames(internal_metadata.df)),]
  
  # If the column and rownames do not match, entries are missing
  if (!all(rownames(internal_metadata.df) == colnames(internal_otu_matrix.m))){
    print("Colnames and metadata names don't match!!!")
    break
  }
  
  # Run DESeq
  dds <-DESeqDataSetFromMatrix(countData = internal_otu_matrix.m, colData = internal_metadata.df, design = ~Number_of_meds)
  geoMeans <- apply(counts(dds), 1, gm_mean)
  dds <- estimateSizeFactors(dds, geoMeans = geoMeans)
  dds <- try(DESeq(dds, test = "Wald", fitType = "parametric", parallel = T))
  
  if(inherits(dds, "try-error")) {
    next
  }
  
  # Result dataframe
  all_combined_results.df <- data.frame()
  
  sample_type_combinations <- combn(sort(unique(as.character(internal_metadata.df[,"Number_of_meds"]))),2)
  print(sample_type_combinations)
  # sample_type_combinations <- combn(rev(my_levels_filtered), 2)
  for (i in 1:ncol(sample_type_combinations)){
    # Set group 1 and group 2
    group_1 <- as.character(sample_type_combinations[1,i])
    group_2 <- as.character(sample_type_combinations[2,i])
    
    # Get the number of samples in each group
    n_group_1 <- dim(subset(internal_metadata.df, Number_of_meds == group_1))[1]
    n_group_2 <- dim(subset(internal_metadata.df, Number_of_meds == group_2))[1]
    
    print(paste0("processing : ", group_1, "_vs_", group_2))
    
    # Get the results from contrasting these groups
    resMFSource <- results(dds, contrast = c("Number_of_meds",group_1,group_2), alpha=0.01, independentFiltering = F, cooksCutoff = F, parallel = T)
    
    group_1_meta <- subset(internal_metadata.df, Number_of_meds == group_1)
    group_2_meta <- subset(internal_metadata.df, Number_of_meds == group_2)
    n_patients_group_1 <- length(unique(group_1_meta$Patient))
    n_patients_group_2 <- length(unique(group_2_meta$Patient))
    
    resMFSource$Group_1 <- group_1
    resMFSource$Group_2 <- group_2
    resMFSource$Variable <- "Number_of_meds"
    resMFSource$N_Group_1 <- n_group_1
    resMFSource$N_Group_2 <- n_group_2
    resMFSource$N_patients_Group_1 <- n_patients_group_1
    resMFSource$N_patients_Group_2 <- n_patients_group_2
    
    # Assign the taxonomy
    if (assign_taxonomy == T){
      resMFSource$Taxonomy <- assign_taxonomy_to_otu(resMFSource, otu_taxonomy_map.df)   
      # Convert to dataframe
      resMFSource <- matrix2df(resMFSource, "OTU")
    } else{
      # Convert to dataframe
      resMFSource <- matrix2df(resMFSource, "Taxonomy")
    }
    
    # Order the results by the adjusted p-value and filter out entries with p-values below threshold
    resMFSourceOrdered <- filter_and_sort_dds_results(resMFSource, 0.01)
    
    # Add the result to the combined dataframe for the patient
    all_combined_results.df <- rbind(all_combined_results.df, resMFSourceOrdered)
  }
  # Write the results
  if (assign_taxonomy == T){
    outfilename <- paste("Result_tables/DESeq_results/immunocompromised_otu_Number_of_meds.csv", sep= "")
  } else{
    outfilename <- paste("Result_tables/DESeq_results/immunocompromised_genus_Number_of_meds.csv", sep= "")
  }
  write.csv(all_combined_results.df, file=outfilename, quote = F, row.names = F)
  
}
immunocompromised_metadata.df <- metadata.df[metadata.df$Project == "immunocompromised",]
immunocompromised_otu_rare.m <- otu_rare.m[,rownames(immunocompromised_metadata.df)]
immunocompromised_genus_rare.m <- genus_rare.m[,rownames(immunocompromised_metadata.df)]

run_immunocompromised_n_meds_deseq(immunocompromised_otu_rare.m, immunocompromised_metadata.df, assign_taxonomy = T)
run_immunocompromised_n_meds_deseq(immunocompromised_genus_rare.m, immunocompromised_metadata.df, assign_taxonomy = F)

# Patient group
run_immunocompromised_patient_group_deseq <- function(my_otu_matrix, my_metadata, assign_taxonomy = T){
  
  internal_otu_matrix.m <- my_otu_matrix
  internal_metadata.df <- my_metadata
  
  # Ensure factored
  internal_metadata.df$Number_of_meds <- factor(internal_metadata.df$Patient_group)
  
  # Order the feature table and the metadata to be the same
  internal_otu_matrix.m <- internal_otu_matrix.m[,order(rownames(internal_metadata.df))]
  internal_metadata.df <- internal_metadata.df[order(rownames(internal_metadata.df)),]
  
  # If the column and rownames do not match, entries are missing
  if (!all(rownames(internal_metadata.df) == colnames(internal_otu_matrix.m))){
    print("Colnames and metadata names don't match!!!")
    break
  }
  
  # Run DESeq
  dds <-DESeqDataSetFromMatrix(countData = internal_otu_matrix.m, colData = internal_metadata.df, design = ~Patient_group)
  geoMeans <- apply(counts(dds), 1, gm_mean)
  dds <- estimateSizeFactors(dds, geoMeans = geoMeans)
  dds <- try(DESeq(dds, test = "Wald", fitType = "parametric", parallel = T))
  
  if(inherits(dds, "try-error")) {
    next
  }
  
  # Result dataframe
  all_combined_results.df <- data.frame()
  
  my_levels <- c("Control", "AK", "SCC")
  sample_type_combinations <- combn(rev(my_levels), 2)

  for (i in 1:ncol(sample_type_combinations)){
    # Set group 1 and group 2
    group_1 <- as.character(sample_type_combinations[1,i])
    group_2 <- as.character(sample_type_combinations[2,i])
    
    # Get the number of samples in each group
    n_group_1 <- dim(subset(internal_metadata.df, Patient_group == group_1))[1]
    n_group_2 <- dim(subset(internal_metadata.df, Patient_group == group_2))[1]
    
    print(paste0("processing : ", group_1, "_vs_", group_2))
    
    # Get the results from contrasting these groups
    resMFSource <- results(dds, contrast = c("Patient_group",group_1,group_2), alpha=0.01, independentFiltering = F, cooksCutoff = F, parallel = T)
    
    group_1_meta <- subset(internal_metadata.df, Patient_group == group_1)
    group_2_meta <- subset(internal_metadata.df, Patient_group == group_2)
    n_patients_group_1 <- length(unique(group_1_meta$Patient))
    n_patients_group_2 <- length(unique(group_2_meta$Patient))
    
    resMFSource$Group_1 <- group_1
    resMFSource$Group_2 <- group_2
    resMFSource$Variable <- "Patient_group"
    resMFSource$N_Group_1 <- n_group_1
    resMFSource$N_Group_2 <- n_group_2
    resMFSource$N_patients_Group_1 <- n_patients_group_1
    resMFSource$N_patients_Group_2 <- n_patients_group_2
    
    # Assign the taxonomy
    if (assign_taxonomy == T){
      resMFSource$Taxonomy <- assign_taxonomy_to_otu(resMFSource, otu_taxonomy_map.df)   
      # Convert to dataframe
      resMFSource <- matrix2df(resMFSource, "OTU")
    } else{
      # Convert to dataframe
      resMFSource <- matrix2df(resMFSource, "Taxonomy")
    }
    
    # Order the results by the adjusted p-value and filter out entries with p-values below threshold
    resMFSourceOrdered <- filter_and_sort_dds_results(resMFSource, 0.01)
    
    # Add the result to the combined dataframe for the patient
    all_combined_results.df <- rbind(all_combined_results.df, resMFSourceOrdered)
  }
  # Write the results for all lesion+cohort to file (assumes no errors and all results generated at the same time)
  if (assign_taxonomy == T){
    outfilename <- paste("Result_tables/DESeq_results/immunocompromised_otu_Patient_group.csv", sep= "")
  } else{
    outfilename <- paste("Result_tables/DESeq_results/immunocompromised_genus_Patient_group.csv", sep= "")
  }
  write.csv(all_combined_results.df, file=outfilename, quote = F, row.names = F)
  
}
immunocompromised_metadata.df <- metadata.df[metadata.df$Project == "immunocompromised",]
immunocompromised_otu_rare.m <- otu_rare.m[,rownames(immunocompromised_metadata.df)]
immunocompromised_genus_rare.m <- genus_rare.m[,rownames(immunocompromised_metadata.df)]

run_immunocompromised_patient_group_deseq(immunocompromised_otu_rare.m, immunocompromised_metadata.df, assign_taxonomy = T)
run_immunocompromised_patient_group_deseq(immunocompromised_genus_rare.m, immunocompromised_metadata.df, assign_taxonomy = F)

# ------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------
# ----------------------------------- TESTING ----------------------------------------
# MST016__Sampletype_pooled
# patient_metadata.df <- metadata.df[metadata.df$Patient == "MST016",] # Get the patient metadata
# patient_samples.v <- as.character(patient_metadata.df$Index) # Get the number of samples associated with the patient
# patient_sample_lesion_types.v <- factor(patient_metadata.df[,"Sampletype_pooled"]) # get the lesion types
# patient_feature_table.m <- otu_rare.m[,patient_samples.v]
# patient_feature_table.m <- patient_feature_table.m[,order(rownames(patient_metadata.df))]
# patient_metadata.df <- patient_metadata.df[order(rownames(patient_metadata.df)),]
# 
# patient_metadata.df$Sampletype_pooled <- factor(patient_metadata.df$Sampletype_pooled, levels = c("NLC", "AK", "SCC"))
# # combn(unique(patient_metadata.df$Sampletype_pooled), 2, simplify = F)
# combn(unique(patient_metadata.df$Sampletype_pooled), 2, simplify = T)
# my_levels <- c("NLC", "AK", "SCC")
# 
# reference_level <- my_levels[1]
# 
# combn(rev(my_levels), 2)
# 
# for (i in 1:length(my_levels)){
#   group_1 <- my_levels[i]
#   for (j in 1:length(my_levels)){
#     group_2 <- my_levels[j]
#     if (group_1 == group_2){next}
#     print(paste0(group_1, "_", group_2))
#   }
#   
# }
# 
# # If levels provided, first is the reference and combinations will respect this
# 
# dds <-DESeqDataSetFromMatrix(countData = patient_feature_table.m, colData = patient_metadata.df, design = ~Sampletype_pooled)
# geoMeans <- apply(counts(dds), 1, gm_mean)
# dds <- estimateSizeFactors(dds, geoMeans = geoMeans)
# dds <- try(DESeq(dds, test = "Wald", fitType = "parametric", parallel = T))
# resMFSource <- results(dds, contrast = c("Sampletype_pooled","NLC","SCC"), alpha=0.01, independentFiltering = F, cooksCutoff = F, parallel = T)
# resMFSource$Group_1 <- "NLC"
# resMFSource$Group_2 <- "SCC"
# resMFSource$Variable <- "MST016"
# resMFSource$Taxonomy <- assign_taxonomy_to_otu(resMFSource, otu_taxonomy_map.df)  
# resMFSource <- matrix2df(resMFSource, "OTU")
# resMFSourceOrdered <- filter_and_sort_dds_results(resMFSource, 0.01)

# ------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------
