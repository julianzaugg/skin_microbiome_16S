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

metadata.df$Sample_type <- factor(metadata.df$Sample_type, levels = c("NS", "PDS", "AK", "SCC_PL","SCC"))


# Load the counts
genus.m <-  as.matrix(read.csv("Result_tables/count_tables/Genus_counts.csv", header =T, row.names = 1))

# Order the matrices and metadata to be the same order
genus.m <- genus.m[,rownames(metadata.df)]

# Create the rarefied matrices
rarefy_threshold <- 5000
# rarecurve(t(genus.m[,colSums(genus.m) > 1]),step = 500, label = F,xlim = c(0,10000),sample = 10000)
genus_rare.m <- t(rrarefy(t(genus.m[,colSums(genus.m) >= rarefy_threshold]), rarefy_threshold))

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

immunosuppressed_group_count.df <- immunosuppressed_genus_rare_alpha.df %>% 
  filter(!is.na(Shannon)) %>%
  group_by(Sample_type) %>% 
  dplyr::summarise(N_Group_1 = n())
immunosuppressed_group_count.l <- setNames(immunosuppressed_group_count.df$N_Group_1,immunosuppressed_group_count.df$Sample_type)

immunocompetent_group_count.df <- immunocompetent_genus_rare_alpha.df %>% 
  filter(!is.na(Shannon)) %>%
  group_by(Sample_type) %>% 
  dplyr::summarise(N_Group_1 = n())
immunocompetent_group_count.l <- setNames(immunocompetent_group_count.df$N_Group_1,immunocompetent_group_count.df$Sample_type)

# Add group sizes
immunosuppressed_genus_alpha_dunn_significances.df$N_Group_1 <- 
  unlist(lapply(immunosuppressed_genus_alpha_dunn_significances.df$Group_1, function(x)immunosuppressed_group_count.l[[x]]))
immunosuppressed_genus_alpha_dunn_significances.df$N_Group_2 <- 
  unlist(lapply(immunosuppressed_genus_alpha_dunn_significances.df$Group_2, function(x)immunosuppressed_group_count.l[[x]]))

immunocompetent_genus_alpha_dunn_significances.df$N_Group_1 <- 
  unlist(lapply(immunocompetent_genus_alpha_dunn_significances.df$Group_1, function(x)immunocompetent_group_count.l[[x]]))
immunocompetent_genus_alpha_dunn_significances.df$N_Group_2 <- 
  unlist(lapply(immunocompetent_genus_alpha_dunn_significances.df$Group_2, function(x)immunocompetent_group_count.l[[x]]))

# Add rarefied depth
immunosuppressed_genus_alpha_dunn_significances.df$Rarefied_depth <- rarefy_threshold
immunocompetent_genus_alpha_dunn_significances.df$Rarefied_depth <- rarefy_threshold


# 
# 
# sig_threshold = 0.05 # p-values less than this will be displayed
# sig_line_scaling_percentage = 0.08 # percent of max y value for spacing significance lines
# sig_vjust = 0.03 # amount to vertically adjust the significance annotations
# sig_tip_length = 0.01 # length of tips on significance lines
# sig_linetype = 1 # linetype of significance lines
# sig_colour = "grey20" # colour of significance lines
# sig_line_starting_scale = 1.05
# process_significances <- function(my_sig_data,my_diversity_data,variable_column,value_column,stat_column = "Shannon_Dunn_padj"){
#   my_sig_data <- my_sig_data[which(my_sig_data[,stat_column] <= sig_threshold),]
#   
#   # Determine the maximum diversity value for the pair of groups being compared
#   for (row in 1:nrow(my_sig_data)){
#     group_1 <- as.character(my_sig_data[row,"Group_1"])
#     group_2 <- as.character(my_sig_data[row,"Group_2"])
#     y_max <- max(my_diversity_data[which(my_diversity_data[,variable_column] == group_1),][,value_column],
#                  my_diversity_data[which(my_diversity_data[,variable_column] == group_2),][,value_column])
#     my_sig_data[row,"y_max"] <- y_max
#     my_sig_data[row,"level_index_group_1"] <- which(levels(my_diversity_data[,variable_column]) == group_1)
#     my_sig_data[row,"level_index_group_2"] <- which(levels(my_diversity_data[,variable_column]) == group_2)
#     my_sig_data[row,"level_distance"] <- abs(my_sig_data[row,"level_index_group_2"] - my_sig_data[row,"level_index_group_1"])
#   }
#   
#   my_sig_data <- my_sig_data[order(my_sig_data$level_distance),]
#   scale <- sig_line_starting_scale # starting scale
#   for (row in 1:nrow(my_sig_data)){
#     my_sig_data[row, "y_position"] <- max(my_sig_data[, "y_max"]) * scale
#     scale <- scale + sig_line_scaling_percentage # increase scale value
#   }
#   
#   my_sig_data$Group_1 <- factor(my_sig_data$Group_1, levels = levels(my_diversity_data[,variable_column]))
#   my_sig_data$Group_2 <- factor(my_sig_data$Group_2, levels = levels(my_diversity_data[,variable_column]))
#   
  # my_sig_data$P_value_label <- as.character(lapply(my_sig_data[,stat_column], function(x) ifelse(x <= 0.001, "***",
  #                                                                                                ifelse(x <= 0.01, "**",
  #                                                                                                       ifelse(x <= 0.05, "*", "ns")))))
#   
#   my_sig_data
# }

generate_p_labels <- function(sig_table){
  for (sig_column in c("Chao1_Dunn_padj", "Shannon_Dunn_padj", "Simpson_Dunn_padj")){
    metric = strsplit(sig_column, "_")[[1]][1]
    sig_table[,paste0(metric, "_p_label")] <-
      as.character(lapply(sig_table[,sig_column], 
                          function(x) ifelse(x <= 0.001, "***", 
                                             ifelse(x <= 0.01, "**",
                                                    ifelse(x <= 0.05, "*", "ns")))))
  }
  sig_table
}
immunosuppressed_genus_alpha_dunn_significances.df <- generate_p_labels(immunosuppressed_genus_alpha_dunn_significances.df)
immunocompetent_genus_alpha_dunn_significances.df <- generate_p_labels(immunocompetent_genus_alpha_dunn_significances.df)

immunosuppressed_genus_alpha_dunn_significances.df$Cohort  <- "organ transplant recipient"
immunocompetent_genus_alpha_dunn_significances.df$Cohort  <- "immunocompetent"
write.csv(x = rbind(immunosuppressed_genus_alpha_dunn_significances.df,immunocompetent_genus_alpha_dunn_significances.df),
          file = paste0("Result_tables/diversity_analysis/IS_IC_diversity_signficances_",rarefy_threshold,".csv"),
          row.names =F,
          quote = F)

sample_type_colours <- unique(metadata.df[,c("Sample_type", "Sample_type_colour")])
sample_type_colours <- setNames(as.character(sample_type_colours[,"Sample_type_colour"]), 
                                sample_type_colours[,"Sample_type"])
variable_shapes <- setNames(c(25,24,23,22,21), c("NS", "PDS", "AK", "SCC_PL", "SCC"))

immunosuppressed_genus_shannon_boxplot <- 
  ggplot(immunosuppressed_genus_rare_alpha.df,aes(x = Sample_type, 
                                                  y = Shannon, 
                                                  fill = Sample_type, 
                                                  # colour = Sample_type, 
                                                  shape = Sample_type)) +
  # geom_violin() +
  geom_boxplot(position = position_dodge(width =.75),
               outlier.shape = NA, width=.5,lwd =.3) +
  geom_jitter(size = .6,stroke =.1,
              position = position_jitterdodge(jitter.width = .75,
                                              dodge.width = .75))  +
  scale_shape_manual(values = variable_shapes,
                     name = "Sample type") +
  scale_fill_manual(values = sample_type_colours, 
                    name = "Sample type") +
  # scale_colour_manual(values = sample_type_colours, 
  #                   name = "Sample type") +
  stat_summary(fun = "mean", colour = "grey2", geom = "point",
               shape = 16,size = 1, position = position_dodge(width = .75),show.legend = F) +
  xlab("Sample type") +
  ylab("Shannon") +
  common_theme +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5, .5))

immunosuppressed_genus_chao1_boxplot <- 
  ggplot(immunosuppressed_genus_rare_alpha.df,aes(x = Sample_type, 
                                                  y = Chao1, 
                                                  fill = Sample_type, 
                                                  shape = Sample_type)) +
  geom_boxplot(position = position_dodge(width =.75), 
               outlier.shape = NA, width=.5,lwd =.3) +
  geom_jitter(size = .6,stroke =.1,
              position = position_jitterdodge(jitter.width = .75,
                                              dodge.width = .75))  +
  scale_shape_manual(values = variable_shapes,
                     name = "Sample type") +
  scale_fill_manual(values = sample_type_colours, 
                    name = "Sample type") +
  stat_summary(fun = "mean", colour = "grey2", geom = "point",
               shape = 16,size = 1, position = position_dodge(width = .75),show.legend = F) +
  xlab("Sample type") +
  ylab("Chao1") +
  common_theme +
  scale_y_continuous(limits = c(0,300), breaks = seq(0,350, 50))
immunosuppressed_genus_chao1_boxplot

immunosuppressed_genus_simpson_boxplot <- 
  ggplot(immunosuppressed_genus_rare_alpha.df,aes(x = Sample_type, 
                                                  y = Simpson, 
                                                  fill = Sample_type, 
                                                  shape = Sample_type)) +
  geom_boxplot(position = position_dodge(width =.75), 
               outlier.shape = NA, width=.5,lwd =.3) +
  geom_jitter(size = .6,stroke =.1,
              position = position_jitterdodge(jitter.width = .75,
                                              dodge.width = .75))  +
  scale_shape_manual(values = variable_shapes,
                     name = "Sample type") +
  scale_fill_manual(values = sample_type_colours, 
                    name = "Sample type") +
  stat_summary(fun = "mean", colour = "grey2", geom = "point",
               shape = 16,size = 1, position = position_dodge(width = .75),show.legend = F) +
  xlab("Sample type") +
  ylab("Simpson") +
  common_theme +
  scale_y_continuous(limits = c(0,1.2), breaks = seq(0,1, .1))
immunosuppressed_genus_simpson_boxplot




immunocompetent_genus_shannon_boxplot <- 
  ggplot(immunocompetent_genus_rare_alpha.df,aes(x = Sample_type, 
                                                 y = Shannon, 
                                                 fill = Sample_type, 
                                                 shape = Sample_type)) +
  geom_boxplot(position = position_dodge(width =.75), 
               outlier.shape = NA, width=.5,lwd =.3) +
  geom_jitter(size = .6,stroke =.1,
              position = position_jitterdodge(jitter.width = .75,
                                              dodge.width = .75))  +
  scale_shape_manual(values = variable_shapes,
                     name = "Sample type") +
  scale_fill_manual(values = sample_type_colours, 
                    name = "Sample type") +
  stat_summary(fun = "mean", colour = "grey2", geom = "point",
               shape = 16,size = 1, position = position_dodge(width = .75),show.legend = F) +
  xlab("Sample type") +
  ylab("Shannon") +
  common_theme +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5, 0.5))

immunocompetent_genus_chao1_boxplot <- 
  ggplot(immunocompetent_genus_rare_alpha.df,aes(x = Sample_type, 
                                                 y = Chao1, 
                                                 fill = Sample_type, 
                                                 shape = Sample_type)) +
  geom_boxplot(position = position_dodge(width =.75), 
               outlier.shape = NA, width=.5,lwd =.3) +
  geom_jitter(size = .6,stroke =.1,
              position = position_jitterdodge(jitter.width = .75,
                                              dodge.width = .75))  +
  scale_shape_manual(values = variable_shapes,
                     name = "Sample type") +
  scale_fill_manual(values = sample_type_colours, 
                    name = "Sample type") +
  stat_summary(fun = "mean", colour = "grey2", geom = "point",
               shape = 16,size = 1, position = position_dodge(width = .75),show.legend = F) +
  xlab("Sample type") +
  ylab("Chao1") +
  common_theme +
  scale_y_continuous(limits = c(0,300), breaks = seq(0,350, 50))
immunocompetent_genus_chao1_boxplot

immunocompetent_genus_simpson_boxplot <- 
  ggplot(immunocompetent_genus_rare_alpha.df,aes(x = Sample_type, 
                                                 y = Simpson, 
                                                 fill = Sample_type, 
                                                 shape = Sample_type)) +
  geom_boxplot(position = position_dodge(width =.75), 
               outlier.shape = NA, width=.5,lwd =.3) +
  geom_jitter(size = .6,stroke =.1,
              position = position_jitterdodge(jitter.width = .75,
                                              dodge.width = .75))  +
  scale_shape_manual(values = variable_shapes,
                     name = "Sample type") +
  scale_fill_manual(values = sample_type_colours, 
                    name = "Sample type") +
  stat_summary(fun = "mean", colour = "grey2", geom = "point",
               shape = 16,size = 1, position = position_dodge(width = .75),show.legend = F) +
  xlab("Sample type") +
  ylab("Simpson") +
  common_theme +
  scale_y_continuous(limits = c(0,1.2), breaks = seq(0,1, .1))
immunocompetent_genus_simpson_boxplot

# Extract the legend
my_legend <- cowplot::get_legend(immunosuppressed_genus_chao1_boxplot + 
                                   theme(
                                     legend.position = "right",
                                     legend.text = element_text(size = 7),
                                     legend.title = element_text(size =8, face="bold"),
                                     legend.justification = "center",
                                     legend.direction = "horizontal",
                                     legend.box.just = "bottom",
                                     plot.margin = unit(c(0, 0, 0, 0), "cm")
                                   )
)


library(cowplot)
chao1 <- plot_grid(plotlist = list(immunosuppressed_genus_chao1_boxplot + 
                                     # labs(title = "Organ transplant recipient") + 
                                     theme(axis.title.x = element_blank(),
                                           axis.text.x = element_blank(),
                                           legend.position = "none",
                                           plot.title = element_text(face = "bold", hjust = 0.5,size = 10))
                                     , 
                                   immunocompetent_genus_chao1_boxplot + 
                                     # labs(title = "Immunocompetent") + 
                                     theme(axis.title.x = element_blank(),
                                           axis.title.y = element_blank(),
                                           axis.text.x = element_blank(),
                                           axis.text.y = element_blank(),
                                           legend.position = "none",
                                           plot.title = element_text(face = "bold", hjust = 0.5,size = 10))
                                   ),
                   rel_widths = c(1,.8))

shannon <- plot_grid(plotlist = list(immunosuppressed_genus_shannon_boxplot + 
                                       theme(
                                         # axis.title.x = element_blank(),
                                         # axis.title.y = element_blank(),
                                         # axis.text.x = element_blank(),
                                         # axis.text.y = element_blank(),
                                         legend.position = "none",
                                         plot.margin = unit(c(5.5,5.5,5.5,6),"pt")
                                         ), 
                                     immunocompetent_genus_shannon_boxplot + 
                                       theme(
                                         # axis.title.x = element_blank(),
                                         axis.title.y = element_blank(),
                                         # axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         legend.position = "none")
                                     ),
                     rel_widths = c(1,.8))
simpson <- plot_grid(plotlist = list(immunosuppressed_genus_simpson_boxplot + 
                                       theme(legend.position = "none",
                                             plot.title = element_text(face = "bold", hjust = 0.5,size = 10),
                                             axis.title.x = element_blank(),
                                             # axis.title.y = element_blank(),
                                             axis.text.x = element_blank(),
                                             # axis.text.y = element_blank(),
                                             )+
                                       labs(title = "Organ transplant recipient"),
                                     immunocompetent_genus_simpson_boxplot + 
                                       theme(
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank(),
                                         axis.text.x = element_blank(),
                                         axis.text.y = element_blank(),
                                         legend.position = "none",
                                         plot.title = element_text(face = "bold", hjust = 0.5,size = 10)
                                         ) +
                                       labs(title = "Immunocompetent")

                                     
                                     ),
                     rel_widths = c(1,.8))

grid_plot <- plot_grid(plotlist = list(simpson,chao1,shannon),
                       ncol = 1,nrow = 3,
                       rel_heights = c(1,1,1), 
                       align = "hv")

# grid_plot <- plot_grid(plotlist = list(grid_plot, my_legend), 
                       # nrow = 2, rel_heights = c(1,.1))


# grid_plot
ggsave(filename = "Result_figures/diversity_analysis/IS_IC_diversity_boxplots.pdf", 
       plot = grid_plot, width = 13, 
       height = 20, units = "cm", device = "pdf")

ggsave(filename = "Result_figures/diversity_analysis/IS_IC_diversity_boxplots.svg", 
       plot = grid_plot, width = 13, 
       height = 20, units = "cm", device = "svg")

