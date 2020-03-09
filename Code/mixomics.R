# mixOmics
# sPLS-DA selects the most discriminative OTUs/features that best characterise each group for a specific variable

library(mixOmics)
# BiocManager::install('mixOmics')
library(reshape2)


# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

filter_matrix_rows <- function(my_matrix, row_max){
  rows_before <- dim(my_matrix)[1]
  rows_after <- dim(my_matrix[apply(my_matrix,1,max) >= row_max,])[1]
  print(paste0("Rows before = ", rows_before))
  print(paste0("Rows after = ", rows_after))
  print(paste0("Lost % = ", round((rows_before-rows_after)/rows_before*100, 2), "%"))
  return(my_matrix[apply(my_matrix,1,max) >= row_max,])
}



# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------


# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
source("Code/helper_functions.R")

# Load the OTU - taxonomy mapping file
otu_taxonomy_map.df <- read.csv("Result_tables/other/otu_taxonomy_map.csv", header = T)
rownames(otu_taxonomy_map.df) <- otu_taxonomy_map.df$OTU.ID

# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Filter out features that do not have at # reads in at least one sample
head(melt(sort(colSums(otu.m))))
otu.m <- filter_matrx_rows(otu.m,0)
head(melt(sort(colSums(otu.m))))
genus.m <- filter_matrix_rows(genus.m,0)

# Filter to immunosuppressed or snapshot samples
metadata.df <- subset(metadata.df, Cohort == "immunosuppressed" | Snapshot_sample_5 == "yes")

# Order the metadata.df by the index value
metadata.df <- metadata.df[order(metadata.df$Index),]

# Rownames should match the sample columns in the otu table
rownames(metadata.df) <- metadata.df$Index

# Load count tables. These are the counts for taxa that were above our abundance thresholds
otu.m <- as.matrix(read.table("Result_tables/count_tables/OTU_counts.csv", sep =",", header =T, row.names = 1))
genus.m <- as.matrix(read.table("Result_tables/count_tables/Genus_counts.csv", sep =",", header =T, row.names = 1))

# Only keep columns (samples) in the metadata
otu.m <- otu.m[,colnames(otu.m) %in% as.character(metadata.df$Index)]
genus.m <- genus.m[,colnames(genus.m) %in% as.character(metadata.df$Index)]

# Since we likely removed samples from the count matrix
# in the main script, remove them from the metadata.df here
# samples_removed <- metadata.df$Index[!metadata.df$Index %in% colnames(otu.m)]
# metadata.df <- metadata.df[! metadata.df$Index %in% samples_removed,]

# Order the tables the same order as the metadata
otu.m <- otu.m[,rownames(metadata.df)]
genus.m <- genus.m[,rownames(metadata.df)]

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# Set up and run mixomics

# If calculating per patient, do note that we cannot have only one of a particular group in the outcome.
# We will need to filter the sample corresponding to the single group out.
# In addition, MST patients tend to have very few samples, e.g. < 10. splsda does not tend to
# work with validation = 'Mfold', ncomp > 3 and folds > 2 for these patients.
# We will likely need to apply a leave-one-out (validation = "loo") for this to work. It is unclear
# how reliable the results from such models will be.

# Although patient is the main source of variability in the data, given so few samples for MST patients,
# more reliable models will likely be obtained by not breaking groups down by patient.


# Mixomics expects that the data matrix has samples as rows and predictors (features) as columns
# set.seed(2543) # for reproducibility, can be any number

# See http://mixomics.org/case-studies/splsda-mydata/ for example on how to apply splsda

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
run_mixomics <- function(my_otu_matrix, my_metadata, outcome_variable, prefix = "", my_levels = NULL, variable_colours_available = T, use_shapes = T){
  
  # Assign internal data
  internal_otu_matrix.m <- my_otu_matrix
  internal_metadata.df <- my_metadata
  
  # Ensure the order of the feature table and the metadata are the same
  internal_otu_matrix.m <- internal_otu_matrix.m[,order(rownames(internal_metadata.df))]
  internal_metadata.df <- internal_metadata.df[order(rownames(internal_metadata.df)),]
  
  # ------------------------------------------------------------------------------------
  # Ensure outcome variable is factored
  # Refactor the variable column so that the levels are consistent
  if (!is.null(my_levels)){
    internal_metadata.df[,outcome_variable] <- factor(internal_metadata.df[,outcome_variable], levels = my_levels)
  } else{
    internal_metadata.df[,outcome_variable] <- factor(internal_metadata.df[,outcome_variable], levels = sort(unique(as.character(internal_metadata.df[,outcome_variable]))))  
  }
  outcome <- internal_metadata.df[,outcome_variable]
  # ------------------------------------------------------------------------------------
  
  # Add pseudo count
  internal_otu_matrix.m <- internal_otu_matrix.m + 0.1
  
  # Number of features to test/keep
  list.keepX = c(c(5:10), seq(15, 50, 5), seq(60, 200, 10))
  
  # This tests what number of features and components to use to give the best discriminatory power
  tune.splsda.mydata <- tune.splsda(X=t(internal_otu_matrix.m), Y = outcome, 
                                   validation = 'Mfold', dist = 'max.dist', 
                                   measure = "BER", logratio = "CLR",
                                   progressBar = TRUE, test.keepX = list.keepX,
                                   scale = TRUE, ncomp = 4,
                                   folds = 5, nrepeat = 10, cpus = 4)
  
  tune.splsda.mydata$choice.ncomp # what does mixomics think is the best number of components?
  tune.splsda.mydata$choice.keepX # what does mixomics think is the best number of OTUs?
  
  # Save the plot describing the classification error
  pdf(paste0("Result_figures/mixomics/",prefix, outcome_variable, "__plsda_tune.pdf"), width=10, height=6)
  plot(tune.splsda.mydata)
  dev.off()
  
  error <- tune.splsda.mydata$error.rate # error rate per component for the keepX grid
  ncomp <- tune.splsda.mydata$choice.ncomp$ncomp # optimal number of components based on t-tests
  
  print(tune.splsda.mydata$choice.ncomp)
  
  # if ncomp = 1, change to 2
  if (ncomp == 1){
    ncomp=2
  }
  
  # Optimal number of variables/features to keep for each component
  select.keepX <- tune.splsda.mydata$choice.keepX[1:ncomp]
  
  # Final model. Use settings from tuning (ncomp and select.keepX)
  splsda.mydata <- splsda(X=t(internal_otu_matrix.m), Y = outcome, logratio = "CLR",keepX = select.keepX,scale = TRUE, ncomp = ncomp)

  
  # ------------------------------------------------------------------------------------
  # Set up the colour and shape for variables for plotIndiv
  variable_values <- levels(internal_metadata.df[[outcome_variable]])
  
  # If variable colour column "variable_colour" in metadata, use colours from there
  if (variable_colours_available == T){
    colour_col_name <- paste0(outcome_variable, "_colour")
    variable_colours <- setNames(as.character(unique(internal_metadata.df[[colour_col_name]])), as.character(unique(internal_metadata.df[[outcome_variable]])))
  } else{
    variable_colours <- setNames(colour_palette[1:length(variable_values)], variable_values)  
  }
  if (use_shapes == T){
    variable_shapes <- setNames(rep(c(25,24,23,22,21),length(variable_values))[1:length(variable_values)],variable_values)
  } else{
    variable_shapes <- setNames(rep(c(21),length(variable_values))[1:length(variable_values)],variable_values)  
  }

  # Plot the first two components
  pdf(paste0("Result_figures/mixomics/",prefix, outcome_variable, "__plotIndiv_comp_1_2.pdf"), width=10, height=6)
  plotIndiv(splsda.mydata, comp = c(1,2), 
            group = outcome,
            ellipse = T, 
            legend = T,
            ind.names = F,
            title = 'sPLS-DA, comp 1 & 2',
            pch = variable_shapes,
            star = F,
            col.per.group = variable_colours)
  dev.off()
  
  # If there is a third component, plot it against 1 as well
  if (ncomp > 3){
    pdf(paste0("Result_figures/mixomics/",prefix, outcome_variable, "__plotIndiv_comp_1_3.pdf"), width=10, height=6)
    plotIndiv(splsda.mydata, comp = c(1,3), 
              group = outcome,
              ellipse = T, 
              legend = T,
              ind.names = F,
              title = 'sPLS-DA, comp 1 & 3',
              pch = variable_shapes,
              star = F,
              col.per.group = variable_colours)
    dev.off()
  }
  
  
  # ------------------------------------------------------------------------------------
  
  # Test performance of final model. Do not include cpus = "" as this breaks (potential bug?)
  perf.mydata <- perf(splsda.mydata, validation = "Mfold", folds = 5, dist = 'max.dist', nrepeat = 10, progressBar = FALSE)
  
  
  # selectVar(splsda.mydata, comp = 1) = outputs the selected features and their coeffecients (loading vector)
  # The absolute value of $value reflects the importance of a feature in the microbial signature. The 
  # sign of the value (+/-) indicates positive or negative correlations between the features, relative the proportions of others.
  
  # We can combine the selected features with their stability measures from the performance testing
  ind.match = match(selectVar(splsda.mydata, comp = 1)$name, names(perf.mydata$features$stable[[1]]))
  # Extract the frequency of selection of those selected variables
  # This tells us how many times each of the OTUs were picked up as discriminatory in each of the subtests mixomics dis
  Freq = as.numeric(perf.mydata$features$stable[[1]][ind.match])
  freqtable1 = data.frame(selectVar(splsda.mydata, comp = 1)$value, Freq)
  # freqtable1
  # ------------------------------------------------------------------------------------
  
  # Calculate the loadings for each component
  for (comp in 1:ncomp){
    pdf(paste0("Result_figures/mixomics/",prefix, outcome_variable, "__","comp_", comp, "_loadings.splsda.waterfall.pdf"), width=10, height=10)
    splsda.loadings <- plotLoadings(splsda.mydata, 
                           contrib = 'max', 
                           method = 'mean', 
                           comp = comp, 
                           size.title = 1, 
                           size.name = 0.5,
                           legend = T,
                           legend.color = variable_colours)
    dev.off()
    splsda.loadings[,"taxonomy_genus"] <- otu_taxonomy_map.df[rownames(splsda.loadings),]$taxonomy_genus
    splsda.loadings[,"Genus"] <- otu_taxonomy_map.df[rownames(splsda.loadings),]$Genus
    splsda.loadings[,'abs_importance'] <- abs(splsda.loadings[,"importance"])
    splsda.loadings <- m2df(splsda.loadings, "OTU.ID")
    write.csv(x = splsda.loadings, file = paste0("Result_tables/mixomics/",prefix, outcome_variable, "__", "comp_", comp, ".loadings.csv"), quote = F, row.names = F)
  }
}


# Compare variables in immunosuppressed cohort
immunosuppressed_metadata.df <- metadata.df[metadata.df$Cohort == "immunosuppressed",]
immunosuppressed_otu.m <- otu.m[,rownames(immunosuppressed_metadata.df)]
immunosuppressed_genus.m <- genus.m[,rownames(immunosuppressed_metadata.df)]

immunocompetent_metadata.df <- metadata.df[metadata.df$Cohort == "immunocompetent",]
immunocompetent_otu.m <- otu.m[,rownames(immunocompetent_metadata.df)]
immunocompetent_genus.m <- genus.m[,rownames(immunocompetent_metadata.df)]

# Like DESeq, filter out features that do not have at # reads in at least one sample
dim(immunosuppressed_otu.m)
immunosuppressed_otu.m <- filter_matrix_rows(immunosuppressed_otu.m,10)
immunocompetent_otu.m <- filter_matrix_rows(immunocompetent_otu.m,10)
immunosuppressed_genus.m <- filter_matrix_rows(immunosuppressed_genus.m,10)
immunocompetent_genus.m <- filter_matrix_rows(immunocompetent_genus.m,10)

# TODO 8/3/20
# remove taxa by prevalence, at least 10% of all samples
# What removes more, minimum read count or prevalence?

# Could use sum instead
# immunosuppressed_otu.m <- immunosuppressed_otu.m[which(apply(X = immunosuppressed_otu.m, MARGIN = 1, FUN = sum) >= 30),]
dim(immunosuppressed_otu.m)

# Lesion_type_refined
run_mixomics(my_otu_matrix = immunosuppressed_otu.m, 
             my_metadata = immunosuppressed_metadata.df, 
             prefix = "immunosuppressed_",
             use_shapes = T,
             outcome_variable = "Lesion_type_refined",
             my_levels = c("C", "C_P", "AK","SCC_PL","SCC"))

run_mixomics(my_otu_matrix = immunocompetent_otu.m, 
             my_metadata = immunocompetent_metadata.df, 
             prefix = "immunocompetent_",
             use_shapes = T,
             outcome_variable = "Lesion_type_refined",
             my_levels = c("C_P", "AK","SCC_PL","SCC"))

# # Patient_group
# run_mixomics(my_otu_matrix = immunosuppressed_otu.m, 
#              my_metadata = immunosuppressed_metadata.df, 
#              prefix = "immunosuppressed_",
#              use_shapes = T,
#              outcome_variable = "Patient_group",
#              my_levels = c("Control","AK","SCC"))
# 
# # Number_of_meds
# run_mixomics(my_otu_matrix = immunosuppressed_otu.m, 
#              my_metadata = immunosuppressed_metadata.df, 
#              prefix = "immunosuppressed_",
#              use_shapes = T,
#              outcome_variable = "Number_of_meds",
#              my_levels = c("1","2","3"))


# Compare cohorts
otu_rare_filtered.m <- filter_matrix_rows(otu.m,15)
run_mixomics(my_otu_matrix = otu_rare_filtered.m,
             my_metadata = metadata.df,
             prefix = "both_cohorts_",
             use_shapes = T,
             outcome_variable = "Cohort",
             my_levels = c("immunocompetent","immunosuppressed"))



immunosuppressed_otu.m <- immunosuppressed_otu.m + 0.1

# Number of features to test/keep
list.keepX = c(c(5:10), seq(15, 50, 5), seq(60, 100, 10))


# First we are going to test Sampletype_pooled
outcome <- factor(immunosuppressed_metadata.df$Sampletype_pooled)


tune.splsda.mydata <- tune.splsda(X=t(immunosuppressed_otu.m), Y = outcome, 
                                 validation = 'Mfold', dist = 'max.dist', 
                                 measure = "BER", logratio = "CLR",
                                 progressBar = TRUE, test.keepX = list.keepX,
                                 scale = TRUE, ncomp = 4,
                                 folds = 5, nrepeat = 10, cpus = 4)

error <- tune.splsda.mydata$error.rate # error rate per component for the keepX grid
ncomp <- tune.splsda.mydata$choice.ncomp$ncomp # optimal number of components based on t-tests

# if ncomp = 1, change to 2
if (ncomp == 1){
  ncomp=2  
}

select.keepX <- tune.splsda.mydata$choice.keepX[1:ncomp] # optimal number of variables/features to select


#settings from tuning (check ncomp)
# Final model. Use settings from tuning (ncomp and select.keepX)
splsda.mydata <- splsda(X=t(immunosuppressed_otu.m), Y = outcome, logratio = "CLR", keepX = select.keepX, scale = TRUE, ncomp = ncomp)

#here is a plot with sample ids
plotIndiv(splsda.mydata, comp = c(1,2), 
          group = outcome,
          ellipse = T, 
          legend = FALSE,
          ind.names = F,
          title = 'sPLS-DA, comp 1 & 2',
          pch = c(21,22,3),
          star = F,
          col.per.group = c("orange", "grey","red"))

#test performance of final model
perf.mydata <- perf(splsda.mydata, validation = "Mfold", folds = 5, dist = 'max.dist', nrepeat = 10, progressBar = FALSE)

plot(perf.mydata)
perf.mydata$error.rate #this tells us our error rate for classification
perf.mydata$error.rate$BER


# selectVar(splsda.mydata, comp = 1) = outputs the selected features and their coeffecients (loading vector)
# The absolute value of $value reflects the importance of a feature in the microbial signature. The 
# sign of the value (+/-) indicates positive or negative correlations between the features, relative the proportions of others.

# We can combine the selected features with their stability measures from the performance testing
ind.match = match(selectVar(splsda.mydata, comp = 1)$name, names(perf.mydata$features$stable[[1]]))
#extract the frequency of selection of those selected variables
#this tells us how many times each of the OTUs were picked up as discriminatory in each of the subtests mixomics dis
Freq = as.numeric(perf.mydata$features$stable[[1]][ind.match])
freqtable1 = data.frame(selectVar(splsda.mydata, comp = 1)$value, Freq)
freqtable1 # this is for component 1

#generate loadings plot of selected OTUs
splsda.loadings <- plotLoadings(splsda.mydata, comp = 1, 
             group = immunosuppressed_metadata.df$Sampletype_pooled, 
             title = 'Loadings on comp 1', 
             contrib = 'max', 
             method = 'mean',
             ndisplay = 30)
# TODO - can add additional data to loading dataframe
# e.g. temp[,'taxonomy'] <- taxonomy.v[rownames(temp)]
# Use the taxonomy map to determine the taxonomy string



#generate heatmap of those selected OTUs
palette(c("orange", "blue","red"))
type.col <- palette()[as.numeric(outcome)]

cim(splsda.mydata, comp=1, title ="Component 1", row.names = immunosuppressed_metadata.df$Sampletype_pooled, transpose = TRUE, row.sideColors = type.col)





# 
# test_patient_meta.df <- metadata.df[metadata.df$Patient == "MST012",]
# 
# # test_patient_meta.df <- metadata.df[metadata.df$Cohort == "immunosuppressed",]
# test_patient_otu.m <- otu.m[,rownames(test_patient_meta.df)]
# dim(test_patient_otu.m)
# test_patient_otu.m <- test_patient_otu.m[which(apply(X = test_patient_otu.m, MARGIN = 1, FUN = sum) >= 50),]
# dim(test_patient_otu.m)
# test_patient_otu.m <- test_patient_otu.m + 0.1
# 
# for (patient in sort(unique(metadata.df$Patient))){
#   if (!grepl("MST", patient)) {next}
#   # print("****************************")
#   print(patient)
#   print(summary(metadata.df[metadata.df$Patient == patient,]$Sampletype_pooled))
#   print("****************************")
# }
# 
# 
# test_patient_meta.df <- test_patient_meta.df[!test_patient_meta.df$Sampletype_pooled == "SCC",]
# test_patient_otu.m <- test_patient_otu.m[,rownames(test_patient_meta.df)]
# 
# outcome <- factor(test_patient_meta.df$Sampletype_pooled)
# outcome
# 
# list.keepX = c(c(5:10), seq(15, 50, 5), seq(60, 100, 10))
# dim(test_patient_otu.m)
# length(outcome)
# # This tests what number of OTUs and components to use to give us the best discriminatory power
# # folds tells it how many samples to keep in each test group - this has to be less than the number of samples in the smallest group
# tune.splsda.mydata <- tune.splsda(X=t(test_patient_otu.m), 
#                                  Y = outcome, 
#                                  ncomp = 2,
#                                  # validation = 'Mfold',
#                                  validation = 'loo',
#                                  folds = 2, 
#                                  progressBar = TRUE,
#                                  dist = 'max.dist', 
#                                  measure = "BER",
#                                  test.keepX = list.keepX,
#                                  nrepeat = 1,
#                                  logratio = "CLR",
#                                  cpus = 2)
# 
# tune.splsda.mydata$choice.ncomp #what does mixomics think is the best number of components?
# tune.splsda.mydata$choice.keepX #what does mixomics think is the best number of OTUs?
# plot(tune.splsda.mydata)
# error <- tune.splsda.mydata$error.rate # error rate per component for the keepX grid
# ncomp <- tune.splsda.mydata$choice.ncomp$ncomp # optimal number of components based on t-tests
# 
# # temp <- pca(t(test_patient_otu.m), ncomp =3, logratio = "CLR")
# # plot(temp$x)
# # temp <- matrix2df(temp$x, "Sample")
# 
# # for each patient (or group)
# # Get rarified counts for associated samples
# # filter out features with low counts (David filtered OTUs that suppressed less than 200 reads across all samples for a patient)
# # transpose and maybe add small pseudo count (0.1)
# #
# 
# ?splsda()
