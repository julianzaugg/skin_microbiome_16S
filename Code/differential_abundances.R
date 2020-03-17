# Perform differential abundance analysis 
# - Comparing lesion types within patient
# - Comparing the same lesion types between cohorts
# - ?Compare patients that have a SCC / no SCC but have AK or IEC / or just Normal

library(DESeq2)
# library(BiocParallel)



############################################################
######################## Functions #########################

m2df <- function(mymatrix, column_name){
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
  filtered_matrix <- my_matrix[apply(my_matrix,1,max) >= row_max,]
  rows_after <- dim(filtered_matrix)[1]
  print(paste0("Rows before = ", rows_before))
  print(paste0("Rows after = ", rows_after))
  print(paste0("Lost % = ", round((rows_before-rows_after)/rows_before*100, 2), "%"))
  return(filtered_matrix)
}

############################################################

# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
source("Code/helper_functions.R")

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Filter to snapshot or immunosuppressed samples
metadata.df <- subset(metadata.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")

# Define the variables of interest
discrete_variables <- c("Lesion_type_refined","Gender","Patient", "Cohort", "Length_of_immunosuppression_group_1", "Length_of_immunosuppression_group_2")

# Load count table at the OTU level. These are the counts for OTUs that were above our abundance thresholds
otu.m <- as.matrix(read.table("Result_tables/count_tables/OTU_counts.csv", sep =",", header =T, row.names = 1))
genus.m <- as.matrix(read.table("Result_tables/count_tables/Genus_counts.csv", sep =",", header =T, row.names = 1))

# Filter out features/taxa that do not have at # reads in at least one sample
head(melt(sort(colSums(otu.m))))
otu.m <- filter_matrix_rows(otu.m,10)
genus.m <- filter_matrix_rows(genus.m,10)
# dim(otu.m[apply(otu.m, 1, max) == 0,])
head(melt(sort(colSums(otu.m))))

# Only keep columns (samples) in the metadata
# metadata.df$Index <- with(metadata.df, paste0(Internal_name, "_", Job_ID))
# colnames(otu.m)[!colnames(otu.m) %in% as.character(metadata.df$Index)]
otu.m <- otu.m[,colnames(otu.m) %in% as.character(metadata.df$Index)]
genus.m <- genus.m[,colnames(genus.m) %in% as.character(metadata.df$Index)]

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# Rownames should match the sample columns in the otu table
rownames(metadata.df) <- metadata.df$Index

# Order the otu_tables the same order as the metadata
otu.m <- otu.m[,rownames(metadata.df)]
genus.m <- genus.m[,rownames(metadata.df)]

# Ensure names of the otu / genus count matrices match the order of the metadata.df!
# Assumes number of samples in metadata.df and count data are the same
all(colnames(otu.m) == metadata.df$Index) # Should be 'True'
all(colnames(otu.m) == rownames(metadata.df)) # Should be 'True'

### IGNORE Create new variables where different lesions types have been grouped

# Convert variables to factors
metadata.df[discrete_variables] <- lapply(metadata.df[discrete_variables], factor)

# ---------------------------------------------------------------------------------------------------------
# Perform differential abundance calculations at the OTU level and genus level, 
# comparing between the groups within variables of interest

# DESeq requires a count matrix ('countData'), a corresponding metadata.df ('colData') and a 'design' formula. The formula expresses
# how the counts for each OTU/genus depend on the variables defined in the 'colData'. See help(DESeqDataSetFromMatrix) for more information.
# The first column of the metadata.df ('colData') must match the ordering of the columns of the countData



patient_lesion_counts <- metadata.df %>% group_by(Patient, Lesion_type_refined) %>% dplyr::summarise(Count = n()) %>% as.data.frame()

compare_groups_deseq <- function(mydata.m, mymetadata.df, myvariables, assign_taxonomy = T){
  # Compare groups for all variables
  combined_results_ordered.df <- data.frame()

  for (myvar in myvariables){
    print(paste0("Processing ", myvar))
    
    # Get all non-NA entries in the metadata
    mymetadata_filtered.df <- mymetadata.df[!is.na(mymetadata.df[,myvar]),]
    
    # Ensure factored variable
    mymetadata_filtered.df[,myvar] <- factor(mymetadata_filtered.df[,myvar])
    
    # Extract corresponding entries from data
    mydata_filtered.m <- mydata.m[,rownames(mymetadata_filtered.df)]
    
    # If the number of samples is 1 or there is only one unique variable
    if (dim(mymetadata_filtered.df)[2] == 1 | length(unique(mymetadata_filtered.df[,myvar])) == 1){
      print("Only one sample or only one unique group")
      break
    }
    if (dim(mymetadata_filtered.df)[1] == 0 | dim(mydata_filtered.m)[2] == 0){
      print("No samples after filtering")
      break
    }
    
    # If the column and rownames do not match, entries are missing
    if (!all(rownames(mymetadata_filtered.df) == colnames(mydata_filtered.m))){
      print("Colnames and metadata names don't match!!!")
      break
    }
    
    # Run DESeq
    dds <- DESeqDataSetFromMatrix(countData = mydata_filtered.m, colData = mymetadata_filtered.df, design = as.formula(paste0("~", myvar)))
    
    geoMeans <- apply(counts(dds), 1, gm_mean)
    dds <- estimateSizeFactors(dds, geoMeans = geoMeans)
    dds <- try(DESeq(dds, test = "Wald", fitType = "parametric", parallel = F))
    if(inherits(dds, "try-error")) {
      next
    }
    group_combinations <- combn(sort(unique(mymetadata_filtered.df[,myvar])),2)
    
    for (i in 1:ncol(group_combinations)){
      group_1 <- as.character(group_combinations[1,i])
      group_2 <- as.character(group_combinations[2,i])
      
      # group_1_meta <- subset(full, get(myvar) == group_1)
      # group_2_meta <- subset(full, get(myvar) == group_2)
      # n_group_1 <- dim(group_1_meta)[1]
      # n_group_2 <- dim(group_2_meta)[1]
      
      n_group_1 <- dim(subset(mymetadata_filtered.df, get(myvar) == group_1))[1]
      n_group_2 <- dim(subset(mymetadata_filtered.df, get(myvar) == group_2))[1]
      
      # Extract results for contrasted groups
      print(paste0(myvar, ": ", group_1, " vs ", group_2))
      resMFSource <- results(dds, contrast = c(myvar,group_1,group_2), alpha=0.01, independentFiltering = F, cooksCutoff = F,parallel = T)
      # print(resMFSource)
      resMFSource$Group_1 <- group_1
      resMFSource$Group_2 <- group_2
      resMFSource$Variable <- myvar
      resMFSource$N_Group_1 <- n_group_1
      resMFSource$N_Group_2 <- n_group_2
      
      # Assign the taxonomy to the results. Assumes feature.
      if (assign_taxonomy == T){
        resMFSource$Taxonomy <- assign_taxonomy_to_otu(resMFSource, otu_taxonomy_map.df)   
        # Convert to dataframe
        resMFSource <- m2df(resMFSource, "OTU")
      } else{
        # Convert to dataframe
        resMFSource <- m2df(resMFSource, "Taxonomy")
      }
      # print(resMFSource)
      resMFSource <- filter_and_sort_dds_results(resMFSource, 0.01)
      combined_results_ordered.df <- rbind(combined_results_ordered.df, resMFSource)
    }
  }
  combined_results_ordered.df
}

compare_groups_deseq_within_group <- function(mydata.m, mymetadata.df, myvariables, within_group_variable, assign_taxonomy = F){
  combined_results.df <- data.frame()
  reduced_variables <- myvariables[which(!myvariables == within_group_variable)]
  for (myvar_value in unique(metadata.df[,within_group_variable])){
    print(paste0("Processing ", myvar_value))
    temp <- compare_groups_deseq(mydata.m = mydata.m, 
                                 mymetadata.df = subset(mymetadata.df, get(within_group_variable) == myvar_value), 
                                 myvariables = reduced_variables, 
                                 assign_taxonomy = assign_taxonomy)
    if (dim(temp)[1] == 0){
      next
    }
    temp[,within_group_variable] <- myvar_value
    combined_results.df <- rbind(combined_results.df, temp)
  }
  combined_results.df
}

# May be commented out to avoid re-running (very slow)

# # Compare groups
# otu_group_comparison.df <- compare_groups_deseq(mydata.m = otu.m, mymetadata.df = metadata.df, myvariables = c("Lesion_type_refined"), assign_taxonomy = T)
# write.csv(x =otu_group_comparison.df,file ="Result_tables/DESeq_results/OTU_deseq.csv",quote = F, row.names =F)
# 
# genus_group_comparison.df <- compare_groups_deseq(mydata.m = genus.m, mymetadata.df = metadata.df, myvariables = c("Lesion_type_refined"), assign_taxonomy = F)
# write.csv(x =genus_group_comparison.df,file ="Result_tables/DESeq_results/Genus_deseq.csv",quote = F, row.names =F)
# 
# 
# # Compare all lesion types within each patient
# otu_group_comparison_within_patient.df <- compare_groups_deseq_within_group(mydata.m = otu.m,
#                                                                             mymetadata.df = metadata.df,
#                                                                             myvariables = c("Lesion_type_refined"),
#                                                                             within_group_variable = "Patient",
#                                                                             assign_taxonomy = T)
# write.csv(x =otu_group_comparison_within_patient.df,file ="Result_tables/DESeq_results/OTU_within_patient_deseq.csv",quote = F, row.names =F)
# 
# genus_group_comparison_within_patient.df <- compare_groups_deseq_within_group(mydata.m = genus.m,
#                                                                               mymetadata.df = metadata.df,
#                                                                               myvariables = c("Lesion_type_refined"),
#                                                                               within_group_variable = "Patient",
#                                                                               assign_taxonomy = F)
# write.csv(x =genus_group_comparison_within_patient.df,file ="Result_tables/DESeq_results/Genus_within_patient_deseq.csv",quote = F, row.names =F)
# 
# # Compare all lesion types within each cohort
# otu_group_comparison_within_cohort.df <- compare_groups_deseq_within_group(mydata.m = otu.m,
#                                                                            mymetadata.df = metadata.df,
#                                                                            myvariables = c("Lesion_type_refined"),
#                                                                            within_group_variable = "Cohort",
#                                                                            assign_taxonomy = T)
# write.csv(x =otu_group_comparison_within_cohort.df,file ="Result_tables/DESeq_results/OTU_within_cohort_deseq.csv",quote = F, row.names =F)
# 
# genus_group_comparison_within_cohort.df <- compare_groups_deseq_within_group(mydata.m = genus.m,
#                                                                              mymetadata.df = metadata.df,
#                                                                              myvariables = c("Lesion_type_refined"),
#                                                                              within_group_variable = "Cohort",
#                                                                              assign_taxonomy = F)
# write.csv(x =genus_group_comparison_within_cohort.df,file ="Result_tables/DESeq_results/Genus_within_cohort_deseq.csv",quote = F, row.names =F)
# 
# # Comparing the same lesion types between cohorts. Always compare suppressed vs competent, e.g. suppressed AK vs competent AK
# # The trick is to group by the lesion type and then only compare groups within the Cohort variable
# otu_cohort_comparison_within_lesion.df <- compare_groups_deseq_within_group(mydata.m = otu.m,
#                                                                             mymetadata.df = metadata.df,
#                                                                             myvariables = c("Cohort"),
#                                                                             within_group_variable = "Lesion_type_refined",
#                                                                             assign_taxonomy = T)
# write.csv(x =otu_cohort_comparison_within_lesion.df,file ="Result_tables/DESeq_results/OTU_cohort_within_lesion_deseq.csv",quote = F, row.names =F)
# 
# genus_cohort_comparison_within_lesion.df <- compare_groups_deseq_within_group(mydata.m = genus.m,
#                                                                               mymetadata.df = metadata.df,
#                                                                               myvariables = c("Cohort"),
#                                                                               within_group_variable = "Lesion_type_refined",
#                                                                               assign_taxonomy = F)
# write.csv(x =genus_cohort_comparison_within_lesion.df,file ="Result_tables/DESeq_results/Genus_cohort_within_lesion_deseq.csv",quote = F, row.names =F)


# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# Publication figures

genus_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  paste(phylostring[3], phylostring[5], phylostring[6], sep = ";")
                }))
}

species_relabeller_function <- function(my_labels){
  unlist(lapply(my_labels, 
                function(x) {
                  phylostring <- unlist(strsplit(x, split = ";"))
                  paste(phylostring[3],phylostring[5],phylostring[6], phylostring[7], sep = ";")
                  # paste(phylostring[3], phylostring[7], sep = ";")
                }))
}

metadata.df$Lesion_type_refined <- factor(metadata.df$Lesion_type_refined, levels = c("C", "C_P", "AK", "SCC_PL", "SCC"))

# Create cohort specific data sets
immunosuppressed_metadata.df <- metadata.df[metadata.df$Cohort == "immunosuppressed",]
immunocompetent_metadata.df <- metadata.df[metadata.df$Cohort == "immunocompetent",]

# Load abundances
otu_rel.m <- as.matrix(read.csv("Result_tables/relative_abundance_tables/OTU_relative_abundances.csv",row.names = 1))
genus_rel.m <- as.matrix(read.csv("Result_tables/relative_abundance_tables/Genus_relative_abundances.csv",row.names = 1))

# Load combined data
otu_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/OTU_counts_abundances_and_metadata.csv", header = T)
genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/Genus_counts_abundances_and_metadata.csv", header = T)
otu_data.df <- subset(otu_data.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")
genus_data.df <- subset(genus_data.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")

otu_data.df$Lesion_type_refined <- factor(otu_data.df$Lesion_type_refined, levels = c("C", "C_P", "AK", "SCC_PL", "SCC"))
genus_data.df$Lesion_type_refined <- factor(genus_data.df$Lesion_type_refined, levels = c("C", "C_P", "AK", "SCC_PL", "SCC"))

# Load DESeq results
otu_within_cohort_deseq_results <- read.csv("Result_tables/DESeq_results/OTU_within_cohort_deseq.csv", header =T)
genus_within_cohort_deseq_results <- read.csv("Result_tables/DESeq_results/Genus_within_cohort_deseq.csv", header =T)

otu_within_patient_deseq_results <- read.csv("Result_tables/DESeq_results/OTU_within_patient_deseq.csv", header =T)
genus_within_patient_deseq_results <- read.csv("Result_tables/DESeq_results/Genus_within_patient_deseq.csv", header =T)

otu_cohort_within_lesion_deseq_results <- read.csv("Result_tables/DESeq_results/OTU_cohort_within_lesion_deseq.csv", header =T)
genus_cohort_within_lesion_deseq_results <- read.csv("Result_tables/DESeq_results/Genus_cohort_within_lesion_deseq.csv", header =T)

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
make_publication_plot <- function(taxa, 
                                  sample_data.df,
                                  significances_IS.df,
                                  significances_IC.df,
                                  taxonomy_level,
                                  taxa_title = NULL,
                                  relabeller_function = NULL,
                                  p_value_column = "padj",
                                  value_column = "Read_count_logged",
                                  ymin_break = 0, ymax_break = 100, ybreak = 10,ymax_limit = 140, ...){
  
  significances_subset_immunosuppressed.df <- significances_IS.df[grepl(taxa,significances_IS.df$Taxonomy),]
  significances_subset_immunocompetent.df <- significances_IC.df[grepl(taxa,significances_IC.df$Taxonomy),]
  
  data_subset_immunosuppressed.df <- subset(sample_data.df, Cohort == "immunosuppressed")
  data_subset_immunosuppressed.df <- data_subset_immunosuppressed.df[grepl(taxa,data_subset_immunosuppressed.df[,taxonomy_level]),]
  
  data_subset_immunocompetent.df <- subset(sample_data.df, Cohort == "immunocompetent")
  data_subset_immunocompetent.df <- data_subset_immunocompetent.df[grepl(taxa,data_subset_immunocompetent.df[,taxonomy_level]),]
  
  data_subset_immunosuppressed.df$Relative_abundance <- data_subset_immunosuppressed.df$Relative_abundance * 100
  data_subset_immunocompetent.df$Relative_abundance <- data_subset_immunocompetent.df$Relative_abundance * 100
  
  if (is.null(taxa_title)){
    if (!is.null(relabeller_function)){
      taxa_title <- relabeller_function(as.character(unique(data_subset_immunosuppressed.df[,taxonomy_level])[[1]]))
    } else{
      taxa_title <- unique(data_subset_immunosuppressed.df[,taxonomy_level])[[1]]  
    }
  }
  
  IS_plot <-  generate_significance_boxplots(mydata.df = data_subset_immunosuppressed.df,
                                             variable_column = "Lesion_type_refined",
                                             value_column = value_column,
                                             variable_colours_available = T,
                                             significances.df = significances_subset_immunosuppressed.df,
                                             p_value_column = p_value_column,
                                             sig_threshold = 0.05, ...) +
    labs(title = "Immunosuppressed") +
    xlab("Lesion type") + 
    ylab(gsub("_", " ", value_column)) + 
    scale_y_continuous(breaks = seq(ymin_break,ymax_break,ybreak), limits = c(0,ymax_limit)) +
    theme(plot.title = element_text(size = 8),
          plot.subtitle = element_text(size = 5,hjust = .5),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) 
    # theme( axis.line = element_blank())
    # geom_segment(aes(x=-Inf,xend=Inf,y=-Inf,yend=-Inf),size = .5) + # Added to match trimmed y axis 
    # geom_segment(aes(x=0,xend=0,y=-Inf,yend=ymax_break + ymax_break*.1),size = .5) + # Added to trim y axis 
    # theme(axis.title.y = element_text(hjust = 0.3)) # Added to adjust when trimming y axis 
  
  IC_plot <- generate_significance_boxplots(mydata.df = data_subset_immunocompetent.df,
                                            variable_column = "Lesion_type_refined",
                                            value_column = value_column,
                                            variable_colours_available = T,
                                            significances.df = significances_subset_immunocompetent.df,
                                            p_value_column = p_value_column,
                                            sig_threshold = 0.05,...) +
    labs(title = "Immunocompetent") +
    xlab("Lesion type") + 
    ylab("") +
    # scale_y_continuous( breaks = seq(0,1,.1), limits = c(0,1.4)) +
    scale_y_continuous(breaks = seq(ymin_break,ymax_break,ybreak), limits = c(0,ymax_limit)) +
    theme(plot.title = element_text(size = 8),
          plot.subtitle = element_text(size = 5,hjust = .5),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) 
    # theme(axis.line = element_blank()) +
    # geom_segment(aes(x=-Inf,xend=Inf,y=-Inf,yend=-Inf),size = .5) + # Added to match trimmed y axis 
    # geom_segment(aes(x=0,xend=0,y=-Inf,yend=ymax_break+ymax_break*.1),size = .5)  # Added to trim y axis 
  
  title <- ggdraw() + 
    draw_label(
      taxa_title,
      fontface = 'bold',
      x = .5,
      hjust = 0.5,
      size = 6
    ) 
  grid_plot <- plot_grid(plotlist = list(IS_plot, NULL, IC_plot),ncol = 3,nrow=1, rel_widths = c(1,-.01,1),align = "hv")
  plot_grid(title, grid_plot,ncol = 1, rel_heights = c(0.1, 1))
  
}



# -------- Boxplots for differentially abundant features (deseq). Show read counts for each sample.
# ------ Within Cohort
# ---- Immunosuppressed

# otus_to_plot.v <- rownames(scc.results.order.df)[which(scc.results.order.df[,'padj'] < 0.1)]
# scc_sig_otus_counts.df = data.frame()
# for (otuid in otus_to_plot.v) {
#   scc_counts.df <- plotCounts(pdds, gene = otuid, intgroup = "sample_type", returnData = T, normalized = T)
#   scc_counts.df[,'OTU'] <- otuid
#   scc_counts.df[,'clr'] <- otu_clr.m[otuid, rownames(scc_counts.df)]
#   scc_sig_otus_counts.df <- rbind(scc_sig_otus_counts.df, scc_counts.df)
# }


# ------------------------------------------------------------------------------------------------
# -------- Boxplots for differentially abundant genera (deseq). Show read counts for each sample.
# -------- see unique(genus_within_cohort_deseq_results$Taxonomy)
# ------ Within Cohort
# ---- Immunosuppressed

deseq_subset.df <- genus_within_cohort_deseq_results[genus_within_cohort_deseq_results$Cohort == "immunosuppressed",]
deseq_subset2.df <- genus_within_cohort_deseq_results[genus_within_cohort_deseq_results$Cohort == "immunocompetent",]

# Subset data main data
genus_data_subset.df <- subset(genus_data.df, taxonomy_genus %in% deseq_subset.df$Taxonomy)
genus_data_subset.df <- genus_data_subset.df[genus_data_subset.df$Cohort == "immunosuppressed",]

source("Code/helper_functions.R")
myplot <- make_publication_plot(taxa = "g__Staphylococcus",
                                significances_IS.df = deseq_subset.df,
                                significances_IC.df = deseq_subset2.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                p_value_column = "padj",
                                value_column = "Relative_abundance",
                                ymin_break = 0, ymax_break = 100,ybreak = 10,ymax_limit = 150,sig_vjust = 0.5)
                                # ymin_break = 0, ymax_break = 1000,ybreak = 2,ymax_limit = 8,sig_vjust = 0.5)
myplot
ggsave(plot = myplot, filename = "Result_figures/DESeq_plots/g__Staphylococcus_boxplot.pdf",width = 15,height = 12,units = "cm")


myplot <- make_publication_plot(taxa = "g__Pseudomonas",
                                significances_IS.df = deseq_subset.df,
                                significances_IC.df = deseq_subset2.df,
                                taxonomy_level = "taxonomy_genus",
                                sample_data.df = genus_data.df,
                                relabeller_function = genus_relabeller_function,
                                p_value_column = "padj",
                                value_column = "Relative_abundance",
                                ymin_break = 0, ymax_break = 40,ybreak = 5,ymax_limit = 40, sig_line_starting_scale = 1.7,sig_line_scaling_percentage = 0.2)
myplot
ggsave(plot = myplot, filename = "Result_figures/DESeq_plots/g__Pseudomonas_boxplot.pdf",width = 15,height = 12,units = "cm")
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# -------- Heatmap showing differentially abundant taxa (deseq)
# ------ Within Cohort
# ---- Immunosuppressed
# --Genus

# Filter to differentially abundant taxa
heatmap.m <- genus_rel.m[as.character(unique(genus_within_cohort_deseq_results$Taxonomy)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% immunosuppressed_metadata.df$Index]

source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = immunosuppressed_metadata.df, 
                     variables = c("Lesion_type_refined"),
                     column_title = "Sample",
                     row_title = "Genus",
                     plot_height = 2.5,
                     plot_width = 18,
                     cluster_columns = T,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("#08306B","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     row_name_size = 4,
                     col_name_size = 4,
                     grid_thickness = .5,
                     filename = paste0("Result_figures/DESeq_plots/lesion_type_within_cohort_immunosuppressed_genus.pdf"))

# ---- Immunosuppressed
# --OTU
heatmap.m <- otu_rel.m[as.character(unique(otu_within_cohort_deseq_results$OTU)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% immunosuppressed_metadata.df$Index]

my_row_names.df <- data.frame(OTU.ID = rownames(heatmap.m), 
                              taxonomy_species=as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID %in% rownames(heatmap.m),]$taxonomy_species),stringsAsFactors = F)
my_row_names.df$taxonomy_species <- species_relabeller_function(my_row_names.df$taxonomy_species)

myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = immunosuppressed_metadata.df, 
                     variables = c("Lesion_type_refined"),
                     column_title = "Sample",
                     row_title = "Feature",
                     my_row_labels = my_row_names.df,
                     plot_height = 3,
                     plot_width = 18,
                     cluster_columns = T,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("blue","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     row_name_size = 4,
                     col_name_size = 4,
                     grid_thickness = .5,
                     col_annotation_label_size = 6,
                     col_annotation_legend_grid_height = .2,
                     col_annotation_legend_grid_width = .2,
                     filename = paste0("Result_figures/DESeq_plots/lesion_type_within_cohort_immunosuppressed_otu.pdf"))

# ---- Immunocompetent
# --Genus
heatmap.m <- genus_rel.m[as.character(unique(genus_within_cohort_deseq_results$Taxonomy)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% immunocompetent_metadata.df$Index]

source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = immunocompetent_metadata.df, 
                     variables = c("Lesion_type_refined"),
                     column_title = "Sample",
                     row_title = "Genus",
                     plot_height = 3,
                     plot_width = 30,
                     cluster_columns = T,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("#08306B","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     filename = paste0("Result_figures/DESeq_plots/lesion_type_within_cohort_immunocompetent_genus.pdf"))


# ---- Immunocompetent
# --OTU
heatmap.m <- otu_rel.m[as.character(unique(otu_within_cohort_deseq_results$OTU)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% immunocompetent_metadata.df$Index]

my_row_names.df <- data.frame(OTU.ID = rownames(heatmap.m), 
                              taxonomy_species=as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID %in% rownames(heatmap.m),]$taxonomy_species),stringsAsFactors = F)
my_row_names.df$taxonomy_species <- species_relabeller_function(my_row_names.df$taxonomy_species)
source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = immunocompetent_metadata.df, 
                     variables = c("Lesion_type_refined"),
                     column_title = "Sample",
                     row_title = "Feature",
                     my_row_labels = my_row_names.df,
                     plot_height = 3.2,
                     plot_width = 40,
                     cluster_columns = T,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("blue","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     row_name_size = 4,
                     col_name_size = 4,
                     grid_thickness = .5,
                     col_annotation_label_size = 6,
                     col_annotation_legend_grid_height = .2,
                     col_annotation_legend_grid_width = .2,
                     filename = paste0("Result_figures/DESeq_plots/lesion_type_within_cohort_immunocompetent_otu.pdf"))

# ---------------------------------------------
# ------ Cohort within Lesion type

# C_meta.df <- subset(metadata.df, Lesion_type_refined == "C")
CP_meta.df <- subset(metadata.df, Lesion_type_refined == "C_P")
AK_meta.df <- subset(metadata.df, Lesion_type_refined == "AK")
SCC_PL_meta.df <- subset(metadata.df, Lesion_type_refined == "SCC_PL")
SCC_meta.df <- subset(metadata.df, Lesion_type_refined == "SCC")

# ---- C_P
heatmap.m <- genus_rel.m[as.character(unique(genus_cohort_within_lesion_deseq_results$Taxonomy)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% rownames(CP_meta.df)]
# metadata.df[metadata.df$Index %in% colnames(heatmap.m),]
source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = CP_meta.df, 
                     variables = c("Cohort"),
                     column_title = "Sample",
                     row_title = "Genus",
                     plot_height = 5,
                     plot_width = 22,
                     cluster_columns = F,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("#08306B","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     filename = paste0("Result_figures/DESeq_plots/Cohort_within_Lesion_type_refined_CP_genus.pdf"))
# -- OTU
heatmap.m <- otu_rel.m[as.character(unique(otu_cohort_within_lesion_deseq_results$OTU)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% rownames(CP_meta.df)]
my_row_names.df <- data.frame(OTU.ID = rownames(heatmap.m), 
                              taxonomy_species=as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID %in% rownames(heatmap.m),]$taxonomy_species),stringsAsFactors = F)
my_row_names.df$taxonomy_species <- species_relabeller_function(my_row_names.df$taxonomy_species)
source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = CP_meta.df, 
                     variables = c("Cohort"),
                     column_title = "Sample",
                     row_title = "Feature",
                     my_row_labels = my_row_names.df,
                     plot_height = 3.2,
                     plot_width = 15,
                     cluster_columns = F,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("blue","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     row_name_size = 4,
                     col_name_size = 4,
                     grid_thickness = .5,
                     col_annotation_label_size = 6,
                     col_annotation_legend_grid_height = .2,
                     col_annotation_legend_grid_width = .2,
                     filename = paste0("Result_figures/DESeq_plots/Cohort_within_Lesion_type_refined_CP_otu.pdf"))

# ---- AK
heatmap.m <- genus_rel.m[as.character(unique(genus_cohort_within_lesion_deseq_results$Taxonomy)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% rownames(AK_meta.df)]
# metadata.df[metadata.df$Index %in% colnames(heatmap.m),]
source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = AK_meta.df, 
                     variables = c("Cohort"),
                     column_title = "Sample",
                     row_title = "Genus",
                     plot_height = 5,
                     plot_width = 22,
                     cluster_columns = F,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("#08306B","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     filename = paste0("Result_figures/DESeq_plots/Cohort_within_Lesion_type_refined_AK_genus.pdf"))
# -- OTU
heatmap.m <- otu_rel.m[as.character(unique(otu_cohort_within_lesion_deseq_results$OTU)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% rownames(AK_meta.df)]
my_row_names.df <- data.frame(OTU.ID = rownames(heatmap.m), 
                              taxonomy_species=as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID %in% rownames(heatmap.m),]$taxonomy_species),stringsAsFactors = F)
my_row_names.df$taxonomy_species <- species_relabeller_function(my_row_names.df$taxonomy_species)
source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = AK_meta.df, 
                     variables = c("Cohort"),
                     column_title = "Sample",
                     row_title = "Feature",
                     my_row_labels = my_row_names.df,
                     plot_height = 3.2,
                     plot_width = 15,
                     cluster_columns = F,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("blue","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     row_name_size = 4,
                     col_name_size = 4,
                     grid_thickness = .5,
                     col_annotation_label_size = 6,
                     col_annotation_legend_grid_height = .2,
                     col_annotation_legend_grid_width = .2,
                     filename = paste0("Result_figures/DESeq_plots/Cohort_within_Lesion_type_refined_AK_otu.pdf"))

# ---- SCC_PL
heatmap.m <- genus_rel.m[as.character(unique(genus_cohort_within_lesion_deseq_results$Taxonomy)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% rownames(SCC_PL_meta.df)]
# metadata.df[metadata.df$Index %in% colnames(heatmap.m),]
source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = SCC_PL_meta.df, 
                     variables = c("Cohort"),
                     column_title = "Sample",
                     row_title = "Genus",
                     plot_height = 5,
                     plot_width = 12,
                     cluster_columns = F,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("#08306B","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     filename = paste0("Result_figures/DESeq_plots/Cohort_within_Lesion_type_refined_SCC_PL_genus.pdf"))
# -- OTU
heatmap.m <- otu_rel.m[as.character(unique(otu_cohort_within_lesion_deseq_results$OTU)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% rownames(SCC_PL_meta.df)]
my_row_names.df <- data.frame(OTU.ID = rownames(heatmap.m), 
                              taxonomy_species=as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID %in% rownames(heatmap.m),]$taxonomy_species),stringsAsFactors = F)
my_row_names.df$taxonomy_species <- species_relabeller_function(my_row_names.df$taxonomy_species)
source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = SCC_PL_meta.df, 
                     variables = c("Cohort"),
                     column_title = "Sample",
                     row_title = "Feature",
                     my_row_labels = my_row_names.df,
                     plot_height = 3.2,
                     plot_width = 8,
                     cluster_columns = F,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("blue","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     row_name_size = 4,
                     col_name_size = 4,
                     grid_thickness = .5,
                     col_annotation_label_size = 6,
                     col_annotation_legend_grid_height = .2,
                     col_annotation_legend_grid_width = .2,
                     filename = paste0("Result_figures/DESeq_plots/Cohort_within_Lesion_type_refined_SCC_PL_otu.pdf"))


# ---- SCC
# -- Genus
heatmap.m <- genus_rel.m[as.character(unique(genus_cohort_within_lesion_deseq_results$Taxonomy)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% rownames(SCC_meta.df)]
# metadata.df[metadata.df$Index %in% colnames(heatmap.m),]
source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = SCC_meta.df, 
                     variables = c("Cohort"),
                     column_title = "Sample",
                     row_title = "Genus",
                     plot_height = 5,
                     plot_width = 12,
                     cluster_columns = F,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("#08306B","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     filename = paste0("Result_figures/DESeq_plots/Cohort_within_Lesion_type_refined_SCC_genus.pdf"))
# -- OTU
heatmap.m <- otu_rel.m[as.character(unique(otu_cohort_within_lesion_deseq_results$OTU)),]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% rownames(SCC_meta.df)]
my_row_names.df <- data.frame(OTU.ID = rownames(heatmap.m), 
                              taxonomy_species=as.character(otu_taxonomy_map.df[otu_taxonomy_map.df$OTU.ID %in% rownames(heatmap.m),]$taxonomy_species),stringsAsFactors = F)
my_row_names.df$taxonomy_species <- species_relabeller_function(my_row_names.df$taxonomy_species)
source("Code/helper_functions.R")
myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = SCC_meta.df, 
                     variables = c("Cohort"),
                     column_title = "Sample",
                     row_title = "Feature",
                     my_row_labels = my_row_names.df,
                     plot_height = 3.2,
                     plot_width = 8,
                     cluster_columns = F,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("blue","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     row_name_size = 4,
                     col_name_size = 4,
                     grid_thickness = .5,
                     col_annotation_label_size = 6,
                     col_annotation_legend_grid_height = .2,
                     col_annotation_legend_grid_width = .2,
                     filename = paste0("Result_figures/DESeq_plots/Cohort_within_Lesion_type_refined_SCC_otu.pdf"))

# ------------------------------------------------------------------------------------
# ------ Within patient
# ---- Immunosuppressed
# --Genus
as.character(sort(unique(genus_within_patient_deseq_results$Patient)))
patient_genus_within_patient_deseq_results <- subset(genus_within_patient_deseq_results, Patient == "MS002")
patient_meta.df <- subset(metadata.df, Patient == "MS002")

heatmap.m <- genus_rel.m[as.character(unique(genus_within_patient_deseq_results$Taxonomy)),,drop = F]
heatmap.m <- heatmap.m[,colnames(heatmap.m) %in% immunosuppressed_metadata.df$Index, drop =F]
colnames(heatmap.m)[!colnames(heatmap.m) %in% immunosuppressed_metadata.df$Index]
dim(metadata.df)
dim(heatmap.m)

myhm <- make_heatmap(heatmap.m*100, 
                     mymetadata = immunosuppressed_metadata.df, 
                     variables = c("Patient", "Lesion_type_refined"),
                     column_title = "Sample",
                     row_title = "Genus",
                     plot_height = 10,
                     plot_width = 35,
                     cluster_columns = F,
                     cluster_rows = T,
                     show_column_dend = T,
                     show_row_dend = F,
                     column_title_size = 10,
                     row_title_size = 10,
                     annotation_bar_name_size = 6,
                     show_cell_values = F,
                     # my_annotation_palette = my_colour_palette_15,
                     # my_palette = c("#08306B","#FFD92F","#67001F"),
                     legend_labels = c(c(0, 0.001, 0.005,0.05, seq(.1,.5,.1))*100, ">= 60"),
                     my_breaks = c(0, 0.001, 0.005,0.05, seq(.1,.6,.1))*100,
                     discrete_legend = T,
                     legend_title = "Relative abundance %",
                     palette_choice = 'bluered',
                     row_dend_width = unit(3, "cm"),
                     simple_anno_size = unit(.25, "cm"),
                     show_top_annotation = T,
                     filename = paste0("Result_figures/DESeq_plots/within_Patient_immunosuppressed_genus.pdf"))
