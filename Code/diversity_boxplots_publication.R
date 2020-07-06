# Diversity boxplots Alpha (Shannon, Chao1, Simpson) for publication



detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()
library(vegan)
library(reshape2)
library(ggplot2)
library(dplyr)

# library(FSA)
library(phyloseq)
# library(nlme)
library(FSA)
# install.packages("FSA")


common_theme <- theme(
  panel.border = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black", size = 0.5),
  panel.background = element_blank(),
  strip.background = element_rect(fill = "white", colour = "white", size = 1),
  strip.text = element_text(size = 6),
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



setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")
source("Code/helper_functions.R")


summarise_alpha_diversities <- function(mydata, group_by_columns){
  summary.df <- mydata %>% 
    dplyr::group_by_(.dots = c(group_by_columns)) %>%
    dplyr::summarise(Shannon_Mean =mean(Shannon),
                     Shannon_Stdev=sd(Shannon),
                     Shannon_Max=max(Shannon), 
                     Shannon_Min=min(Shannon), 
                     Shannon_Median=median(Shannon), 
                     
                     Simpson_Mean=mean(Simpson), 
                     Simpson_Stdev=sd(Simpson),
                     Simpson_Max=max(Simpson), 
                     Simpson_Min=min(Simpson), 
                     Simpson_Median=median(Simpson), 
                     
                     Chao1_Mean=mean(Chao1), 
                     Chao1_Stdev=sd(Chao1),
                     Chao1_Max=max(Chao1), 
                     Chao1_Min=min(Chao1), 
                     Chao1_Median=median(Chao1),
                     
                     N_patients=n_distinct(Subject),
                     N_samples = n_distinct(Index)
    ) %>% as.data.frame()
  summary.df
}


summarise_diversities_each_variable <- function(mydata, variables){
  mydata_summary.df <- NULL
  for (myvar in variables){
    if (is.null(mydata_summary.df)){
      mydata_summary.df <- summarise_alpha_diversities(mydata, myvar)
      # Melt because we are combining variables into a single table
      mydata_summary.df <- melt(mydata_summary.df, measure.vars = myvar, value.name = "Group", variable.name = "Variable")
    } else{
      mydata_summary.df <- rbind(mydata_summary.df, melt(summarise_alpha_diversities(mydata, myvar),
                                                         measure.vars = myvar, value.name = "Group", variable.name = "Variable"))      
    }
    
  }
  mydata_summary.df
}


library(FSA)
calculate_alpha_diversity_significance_multiple <- function(mydata, variable){
  # Assumes there are Shannon, Chao1 and Simpson columns
  n_groups = length(as.character(unique(mydata[,variable])))
  if (any(is.na(mydata[,variable]))){
    return()
  }
  if (n_groups > 2){
    kw_shannon <- kruskal.test(Shannon~get(variable), data = mydata)
    kw_simpson <- kruskal.test(Simpson~get(variable), data = mydata)
    kw_chao1 <- kruskal.test(Shannon~get(variable), data = mydata)
    dunn_shannon <- dunnTest(x = Shannon~get(variable), data = mydata, method = "bh", alpha = 0.05)
    dunn_simpson <- dunnTest(x = Simpson~get(variable), data = mydata, method = "bh", alpha = 0.05)
    dunn_chao1 <- dunnTest(x = Chao1~get(variable), data = mydata, method = "bh", alpha = 0.05)
    
    dunn_shannon <- separate(dunn_shannon$res, Comparison, into = c("Group_1", "Group_2"), sep = " - ")[,c("Group_1","Group_2","P.unadj","P.adj")]
    names(dunn_shannon) <- c("Group_1","Group_2","Shannon_Dunn_pvalue","Shannon_Dunn_padj")
    
    dunn_simpson <- separate(dunn_simpson$res, Comparison, into = c("Group_1", "Group_2"), sep = " - ")[,c("Group_1","Group_2","P.unadj","P.adj")]
    names(dunn_simpson) <- c("Group_1","Group_2","Simpson_Dunn_pvalue","Simpson_Dunn_padj")
    
    dunn_chao1 <- separate(dunn_chao1$res, Comparison, into = c("Group_1", "Group_2"), sep = " - ")[,c("Group_1","Group_2","P.unadj","P.adj")]
    names(dunn_chao1) <- c("Group_1","Group_2","Chao1_Dunn_pvalue","Chao1_Dunn_padj")
    
    multiple_group_comparison.df <- merge(merge(x = dunn_shannon, y = dunn_simpson, by = c("Group_1", "Group_2")), y = dunn_chao1,  by = c("Group_1", "Group_2"))
    multiple_group_comparison.df$Shannon_KrusW_pvalue <- kw_shannon$p.value
    multiple_group_comparison.df$Simpson_KrusW_pvalue <- kw_simpson$p.value
    multiple_group_comparison.df$Chao1_KrusW_pvalue <- kw_chao1$p.value
    multiple_group_comparison.df$Variable = variable
    return(multiple_group_comparison.df)
  }
  multiple_group_comparison.df <- data.frame("Variable" = character(),
                                             "Group_1" = character(),
                                             "Group_2" = character(),
                                             "Shannon_Dunn_pvalue" = character(),
                                             "Shannon_Dunn_padj" = character(),
                                             "Simpson_Dunn_pvalue" = character(),
                                             "Simpson_Dunn_padj" = character(),
                                             "Chao1_Dunn_pvalue" = character(),
                                             "Chao1_Dunn_padj" = character(),
                                             "Shannon_KrusW_pvalue" = character(),
                                             "Simpson_KrusW_pvalue" = character(),
                                             "Chao1_KrusW_pvalue" = character())
  return(multiple_group_comparison.df)
  
}





# Load the processed metadata
metadata.df <- read.csv("Result_tables/other/processed_metadata.csv", sep =",", header = T)

# Set the Index to be the rowname
rownames(metadata.df) <- metadata.df$Index

names(metadata.df)[names(metadata.df) == "Patient"] <- "Subject"
metadata.df$Cohort  <- as.character(metadata.df$Cohort)
metadata.df[metadata.df$Cohort == "immunosuppressed", ]$Cohort <- "organ transplant recipient"

# Define the discrete variables
discrete_variables <- c("Sample_type", "Cohort", "Subject")

metadata.df$Sample_type <- factor(metadata.df$Sample_type, levels = c("HS", "PDS", "AK", "SCC_PL","SCC"))


# Load the counts
genus.m <-  as.matrix(read.csv("Result_tables/count_tables/Genus_counts.csv", header =T, row.names = 1))

# Order the matrices and metadata to be the same order
genus.m <- genus.m[,rownames(metadata.df)]

# Create the rarefied matrices
genus_rare.m <- t(rrarefy(t(genus.m[,colSums(genus.m) >= 2000]), 2000))

# Create phyloseq object
genus_rare_phyloseq <- otu_table(genus_rare.m, taxa_are_rows=TRUE)

# Estimate alpha diversities
both_cohorts_genus_rare_alpha.df <- estimate_richness(genus_rare_phyloseq, measures = c("Chao1", "Simpson","Shannon"))
both_cohorts_genus_rare_alpha.df <- both_cohorts_genus_rare_alpha.df[rownames(metadata.df),]

# Combine with metadata
both_cohorts_genus_rare_alpha.df <- left_join(metadata.df[c("Index",
                                                            discrete_variables, 
                                                            grep("_colour", names(metadata.df), value = T))],m2df(both_cohorts_genus_rare_alpha.df, "Index"), by = "Index")


# Create Cohort specific datasets
immunocompetent_genus_rare_alpha.df <- subset(both_cohorts_genus_rare_alpha.df, Cohort == "immunocompetent")
immunosuppressed_genus_rare_alpha.df <- subset(both_cohorts_genus_rare_alpha.df, Cohort == "organ transplant recipient")

immunosuppressed_genus_alpha_diversity_summary.df <- summarise_diversities_each_variable(immunosuppressed_genus_rare_alpha.df, variables = discrete_variables)
immunosuppressed_genus_alpha_dunn_significances.df <- calculate_alpha_diversity_significance_multiple(immunosuppressed_genus_rare_alpha.df,variable = "Sample_type")
immunocompetent_genus_alpha_diversity_summary.df <- summarise_diversities_each_variable(immunocompetent_genus_rare_alpha.df, variables = discrete_variables)
immunocompetent_genus_alpha_dunn_significances.df <- calculate_alpha_diversity_significance_multiple(immunocompetent_genus_rare_alpha.df,variable = "Sample_type")



process_significances <- function(my_sig_data,my_abundance_data,variable_column,value_column){
  my_sig_data <- my_sig_data[which(my_sig_data[,"Dunn_padj"] <= sig_threshold),]
  
  # Determine the maximum diversity value for the pair of groups being compared
  for (row in 1:nrow(my_sig_data)){
    group_1 <- as.character(my_sig_data[row,"Group_1"])
    group_2 <- as.character(my_sig_data[row,"Group_2"])
    y_max <- max(my_abundance_data[which(my_abundance_data[,variable_column] == group_1),][,value_column],
                 my_abundance_data[which(my_abundance_data[,variable_column] == group_2),][,value_column])
    my_sig_data[row,"y_max"] <- y_max
    my_sig_data[row,"level_index_group_1"] <- which(levels(my_abundance_data[,variable_column]) == group_1)
    my_sig_data[row,"level_index_group_2"] <- which(levels(my_abundance_data[,variable_column]) == group_2)
    my_sig_data[row,"level_distance"] <- abs(my_sig_data[row,"level_index_group_2"] - my_sig_data[row,"level_index_group_1"])
  }
  
  my_sig_data <- my_sig_data[order(my_sig_data$level_distance),]
  scale <- sig_line_starting_scale # starting scale
  for (row in 1:nrow(my_sig_data)){
    my_sig_data[row, "y_position"] <- max(my_sig_data[, "y_max"]) * scale
    scale <- scale + sig_line_scaling_percentage # increase scale value
  }
  
  my_sig_data$Group_1 <- factor(my_sig_data$Group_1, levels = levels(my_abundance_data[,variable_column]))
  my_sig_data$Group_2 <- factor(my_sig_data$Group_2, levels = levels(my_abundance_data[,variable_column]))
  
  my_sig_data$P_value_label <- as.character(lapply(my_sig_data[,"Dunn_padj"], function(x) ifelse(x <= 0.001, "***", 
                                                                                                 ifelse(x <= 0.01, "**", 
                                                                                                        ifelse(x <= 0.05, "*", "ns")))))
  
  my_sig_data
}


immunosuppressed_genus_shannon_boxplot <- ggplot(IS_genus_rel_specific.df, aes(x = Genus, 
                                                      y = Relative_abundance, 
                                                      fill = Sample_type, 
                                                      shape = Sample_type)) +
  geom_boxplot(position = position_dodge(width =.75), 
               outlier.shape = NA, width=.5,lwd =.3) +
  geom_jitter(size = .6,stroke =.1,
              position = position_jitterdodge(jitter.width = .2,
                                              dodge.width = .75)) +
  scale_shape_manual(values = c(25,24,23,22,21),name = "Sample type") +
  scale_fill_manual(values = sample_type_colours, name = "Sample type") +
  # stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
  #              width = 0.75, size = .2, linetype = "solid", colour = "red",
  #              position = "dodge") +
  stat_summary(fun = "mean", colour = "grey2", geom = "point",
               shape = 16,size = 1,
               position = position_dodge(width = .75),show.legend = F) +
  # stat_summary(fun = "mean", colour = "grey20", geom = "point",
  # shape = 16,lwd = 3,
  # position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) + 
  ylab("Relative abundance") +
  common_theme +
  theme(axis.text.x = element_text(face = "italic"))