library(ggplot2)
library(reshape2)
library(dplyr)
library(cowplot)

library(ggsignif)

# install.packages("lemon")
library(lemon)



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


############################################################
# Various colour palletes
my_colour_pallete <- c("#8dd3c7","#ffffb3","#bebada","#fb8072", "#80b1d3", "#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5", "#cc0000")
# From http://tools.medialab.sciences-po.fr/iwanthue/
my_colour_pallete_20 <- c("#66bd79","#a35bcf","#5bb643","#d14ea6","#a2b239","#5c6bcc","#dc892e","#5e93cd","#d64737","#49b6a8","#dc3c6e","#4f7e3c","#bd8cd5","#caab55","#914c88","#867230","#df82a2","#a65429","#ab4a5a","#e0896a")
my_colour_pallete_20_distinct <- c("#0057b4","#7fff56","#d600bc","#d8d500","#e76eff","#019932","#9f8fff","#ffc730","#007fac","#a20019","#06fefd","#ff6782","#00774c","#e0c8ff","#717a00","#4b2952","#e2ed7d","#46321e","#ffbd76","#ffb4c6")
my_colour_pallete_30_distinct <- c("#009348","#f579fe","#4fe16e","#b40085","#4d7e00","#4742b4","#f0c031","#016dd9","#d45200","#7499ff","#ef4d2d","#01c9c8","#f8394b","#88d7a6","#d20063","#c8cc5d","#882986","#fdb95d","#404f8f","#917300","#f3aefc","#5c5800","#ff75c3","#00674a","#ba001c","#979760","#8b354c","#ff875f","#943105","#cf9478")
my_colour_pallete_206_distinct <- c("#cfefb4","#7d8b00","#a70079","#552155","#632900","#ffb173","#fbdcf2","#015a6a","#43fdf7","#ff443a","#008186","#3b8aff","#8b5fff","#ff9777","#4200a9","#85f6fd","#c96000","#36218a","#d28900","#0137d7","#30325b","#ff836b","#008b4f","#21ff9d","#00794d","#870052","#e9ec4b","#ce006b","#6e0044","#8a6500","#006971","#432e4b","#ca8dff","#f20059","#44ffe2","#00be5c","#a0d2ff","#1914ab","#4d284e","#59d7ff","#ab9aff","#0151d9","#1de740","#e24500","#9fc400","#610769","#0a4600","#1e365b","#018f3f","#b15fff","#009c5e","#005290","#506100","#f49aff","#0187c1","#ffb5f4","#daf100","#70081d","#ff9890","#c1baff","#ffbe5a","#1b3466","#ff2a7f","#ff5d3c","#e47800","#ac6bff","#1f6000","#006627","#4f4000","#dcd6ff","#ffd7c1","#ed2de4","#a50038","#a5a8ff","#0f2f7f","#b11700","#00e06b","#ffabb8","#015780","#82eaff","#1b2a88","#6f1600","#d3ef9c","#746e00","#01d851","#625300","#01d799","#96fd6c","#ff5ca1","#7b0017","#004c2b","#baf678","#f8aaff","#007c1b","#01a88a","#a71ed8","#fb8cff","#840079","#276d00","#556655","#02b0de","#c0efd7","#63193e","#8e9984","#017ac9","#ff925f","#ff63d7","#294100","#28baff","#5b2523","#35ab00","#69132e","#8a3b00","#a67700","#7fff6a","#002f96","#681a0b","#4d3003","#ff7de6","#0190d8","#a69700","#ff6282","#d3f266","#ffc4cf","#ffac3c","#d064ff","#d07aff","#c3005d","#9d0067","#0167c1","#8cfe82","#ffd68f","#8cfcaf","#f50096","#00c2a2","#aa5e00","#02c16d","#4e4bf6","#ffd962","#004793","#93d800","#462a58","#323a03","#4f9eff","#2b3a25","#2defff","#02edd6","#864e00","#ffc59f","#e7e9ab","#014cc4","#437bff","#00afba","#ff7d82","#8a1ed4","#ff48b3","#acf7ab","#005550","#7600a6","#bc0028","#00adab","#02dfbf","#ba004c","#004760","#ebc5ff","#0162d7","#9b3900","#5869ff","#ff6160","#87b6ff","#ff6796","#ff8422","#ff8440","#b500a8","#937fff","#0132bd","#f48e00","#1e8800","#462370","#3e3614","#9ca800","#efe5bf","#aeb6a0","#d9aaff","#d8ef89","#cec800","#ffb8b3","#4a2c42","#01715b","#b8ebff","#ff9ec0","#ff93ec","#ffe0aa","#65b300","#6a8b00","#f6e77c","#ff85c0","#5de522","#a5f6ca","#c70077","#5a4149","#a3b700","#ff63c4","#63fecd","#93f6e7","#01b4a4")
my_colour_pallete_15 <- c("#77b642","#7166d9","#cfa240","#b351bb","#4fac7f","#d44891","#79843a","#c68ad4","#d15a2c","#5ba7d9","#ce4355","#6570ba","#b67249","#9b4a6f","#df8398")
my_colour_pallete_32_distinct <- c("#ea7e00","#ca0074","#d1c69b","#474007","#bb00ad","#9c80ff","#be3300","#542e72","#00b9f5","#09436b","#8b0036","#9ac8e6","#ff1059","#959eff","#154a11","#0290f4","#ff7762","#7dbf00","#ff8194","#834c00","#006e73","#f9bb5d","#d6c943","#017229","#00d3a8","#732427","#36e191","#6a8200","#efb3ea","#3227bb","#ff90e1","#e92a12")
lesion_pallete_7 <- c("#8558d6","#6ee268","#d247ad","#c9d743","#d7453e","#59a237","#d78f2a")
patient_pallete_45 <- c("#d64530","#585fb1","#795d97","#9e4773","#3f6921","#71692c","#a2b93c","#d571cc","#9b3e97","#33947a","#98ad66","#448a4e","#869ae0","#5ce7af","#e085a3","#dfdc87","#d19be2","#5cb735","#e38269","#3db6c0","#50b565","#50902c","#a98a2c","#dde84a","#db3d76","#5fe485","#7c8329","#b3e791","#6fe965","#5ebce9","#3c86c1","#2a6a45","#65b688","#6651d1","#af4ed3","#df872f","#56e4db","#737cea","#ac464b","#dd37b5","#995b2b","#daac6f","#92e2be","#a2e24b","#e0be3a")
my_colour_pallete_10_distinct <- c("#8eec45","#0265e8","#f6a800","#bf6549","#486900","#c655a0","#00d1b6","#ff4431","#aeb85c","#7e7fc8")

my_colour_pallete_12_soft <-c("#9E788F","#4C5B61","#678D58","#AD5233","#A0A083","#4D456A","#588578","#D0AC4C","#2A7BA0","#931621", "#c75a93", "#7c7731")
############################################################
# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S")

otu_data.df <- read.csv("Result_tables/other/OTU_counts_abundances_and_metadata.csv")
genus_data.df <- read.csv("Result_tables/other/genus_counts_abundances_and_metadata.csv")

immunosuppressed_sampletype_final_otu_deseq <- read.csv("Result_tables/DESeq_results/immunosuppressed_otu_sampletype_final.csv", header = T)
immunosuppressed_sampletype_final_genus_deseq <- read.csv("Result_tables/DESeq_results/immunosuppressed_genus_sampletype_final.csv", header = T)

immunocompetent_sampletype_final_otu_deseq <- read.csv("Result_tables/DESeq_results/immunocompetent_otu_sampletype_final.csv", header = T)
immunocompetent_sampletype_final_genus_deseq <- read.csv("Result_tables/DESeq_results/immunocompetent_genus_sampletype_final.csv", header = T)


# -------------------------------------------


# Take DESeq result table and counts+metadata table (can be OTU or genus) and generate log10(read count) boxplots
# for variable. Bit hacky.
# data_merge_variable = "OTU.ID",
# deseq_merge_variable = "OTU",
# data_merge_variable = "taxonomy_genus",
# deseq_merge_variable = "Taxonomy",
# my_deseq_results
make_deseq_boxplots <- function(my_data, variable, taxonomy_level,
                                outlocation = "Result_figures/DESeq_plots/boxplots/",
                                individual_plot_width = 10, individual_plot_height = 10,
                                n_columns = 4){
  internal_data.df <- my_data
  # internal_data.df <- merge(internal_data.df, my_deseq_results, by.x = data_merge_variable, by.y =deseq_merge_variable, all.x = T)
  
  if (taxonomy_level == "OTU"){
    internal_data.df$label <- with(internal_data.df, paste0(Genus, "\n", OTU.ID))
    temp <- internal_data.df[grepl("Unassigned", internal_data.df$label),]
    if (dim(temp)[1] != 0){
      last_assigned_taxa <- unlist(lapply(strsplit(gsub(";Unassigned.*","", temp$taxonomy_genus), ";"), function(x) rev(x)[1]))
      internal_data.df[grepl("Unassigned", internal_data.df$label),]$label <- with(internal_data.df[grepl("Unassigned", internal_data.df$label),],paste0(last_assigned_taxa, "\n", OTU.ID))
    }
  } else if (taxonomy_level == "Genus") {
    internal_data.df$label <- as.character(internal_data.df$Genus)
    temp <- internal_data.df[grepl("Unassigned", internal_data.df$label),]
    if (dim(temp)[1] != 0){
      last_assigned_taxa <- unlist(lapply(strsplit(gsub(";Unassigned.*","", temp$taxonomy_genus), ";"), function(x) rev(x)[1]))
      internal_data.df[grepl("Unassigned", internal_data.df$label),]$label <- with(internal_data.df[grepl("Unassigned", internal_data.df$label),], 
                                                                                   paste0(last_assigned_taxa))
    }
  } else{
    break
  }
  fill_colours <- setNames(unique(as.character(internal_data.df[,paste0(variable,"_colour")])), unique(as.character(internal_data.df[,variable])))
  
  # Generate the individual plots
  plot_list <- list()
  
  # Order the labels (whole table) by alphabetical order
  internal_data.df <- internal_data.df[order(internal_data.df$label),]
  
  # Check for groups that are missing. If any are missing, create a single fake entry.
  # This forces ggplot boxplots to have all the discrete groups
  for (entry in unique(internal_data.df$label)){
    data_subset <- subset(internal_data.df, label == entry)
    missing_groups <- unique(internal_data.df[,variable])[!unique(internal_data.df[,variable]) %in% data_subset[,variable]]
    for (mg in missing_groups){
      temp <- data_subset[1,]
      temp$label <- entry
      temp[,variable] <- mg
      temp$Read_count_rarefied <- NA
      temp$Read_count_rarefied_logged <- NA
      temp$Relative_abundance <- NA
      temp$Relative_abundance_rarefied <- NA
      data_subset <- rbind(data_subset, temp)
    }
    # print(data_subset[data_subset$Sample == "R1487_J1425",]$label)
    myplot <- ggplot(data_subset, aes(x = get(variable), y = Read_count_rarefied_logged)) +
      # myplot <- ggplot(data_subset, aes(x = get(variable), y = Relative_abundance_rarefied)) +
      geom_boxplot(outlier.shape = NA, aes(fill = get(variable))) +
      scale_fill_manual(values = fill_colours, name = variable) +
      geom_jitter(size=0.5, width = 0.10, height=0) +
      guides(fill=FALSE) +
      ggtitle(entry) +
      scale_y_continuous(limits = c(0,4.5), breaks = seq(0,4.5,.5)) +
      xlab("") +
      ylab("Log 10 (read count)")  +
      common_theme +
      theme(panel.border = element_blank(), 
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
            plot.title = element_text(size = 6))
    
    
    plot_list[[gsub("\n","_",entry)]] <- myplot
    outname <- paste0(as.character(entry), "__", variable, ".pdf")
    outname <- gsub("^[a-zA-Z]__(.*)", "\\1", outname)
    outname <- gsub("\n", "_", outname)
    # ggsave(plot = myplot,filename =  paste0("Result_figures/DESeq_plots/boxplots/sampletype_pooled/", outname), width = 10, height = 10, units = "cm")
    ggsave(plot = myplot,filename =  paste0(outlocation, outname), width = individual_plot_width, height = individual_plot_height, units = "cm")
  }
  
  # Create legend by creating plot with everything to ensure legend is complete
  all_sample_plot_for_colour <- ggplot(internal_data.df, aes(x = get(variable), y = Read_count_rarefied_logged)) +
    geom_boxplot(aes(fill =  get(variable))) +
    scale_fill_manual(values = fill_colours, name = variable)
  
  my_legend_colour <- cowplot::get_legend(all_sample_plot_for_colour + theme(legend.position = "right", 
                                                                             legend.text = element_text(size = 9),
                                                                             legend.title = element_text(size =10, face = "bold"),
                                                                             legend.justification = "center",
                                                                             plot.margin = unit(c(0, 0, 0, 0), "cm")))
  
  grid_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = n_columns)
  grid_plot <- plot_grid(grid_plot, my_legend_colour, rel_widths = c(1,.4), ncol = 2)
  return(grid_plot)
}

# ----------------------------------------------------------------
# immunosuppressed

# Filter to project
genus_data_filtered.df <- genus_data.df[genus_data.df$Project == "immunosuppressed",]
otu_data_filtered.df <- otu_data.df[otu_data.df$Project == "immunosuppressed",]

# Remove negative if present
genus_data_filtered.df <- genus_data_filtered.df[genus_data_filtered.df$Sampletype != "negative",]
otu_data_filtered.df <- otu_data_filtered.df[otu_data_filtered.df$Sampletype != "negative",]

# Set the ordering of the groups levels (variable of interest)
genus_data_filtered.df$Sampletype_final <- factor(genus_data_filtered.df$Sampletype_final, levels = c("C", "LC", "AK", "SCC"))
otu_data_filtered.df$Sampletype_final <- factor(otu_data_filtered.df$Sampletype_final, levels = c("C", "LC", "AK", "SCC"))

# Filter data to significant genera/OTUs for each deseq result
sampletype_final_genus_data_filtered.df <- genus_data_filtered.df[genus_data_filtered.df$taxonomy_genus %in% immunosuppressed_sampletype_final_genus_deseq$Taxonomy,]
sampletype_final_otu_data_filtered.df <- otu_data_filtered.df[otu_data_filtered.df$OTU.ID %in% immunosuppressed_sampletype_final_otu_deseq$OTU,]

grid_plot <- make_deseq_boxplots(my_data = sampletype_final_genus_data_filtered.df, 
                                 # my_deseq_results = immunosuppressed_sampletype_pooled_genus_deseq, 
                                 variable = "Sampletype_final", 
                                 taxonomy_level = "Genus",
                                 outlocation = "Result_figures/DESeq_plots/boxplots/sampletype_final_genus/")

ggsave(plot = grid_plot,filename =  paste0("Result_figures/DESeq_plots/boxplots/all_genus_sampletype_final.pdf"), width = 30, height = 15, units = "cm")

grid_plot <- make_deseq_boxplots(my_data = sampletype_final_otu_data_filtered.df, 
                                 # my_deseq_results = immunosuppressed_sampletype_pooled_otu_deseq, 
                                 variable = "Sampletype_final", 
                                 taxonomy_level = "OTU",
                                 outlocation = "Result_figures/DESeq_plots/boxplots/sampletype_final_otu/")

ggsave(plot = grid_plot,filename =  paste0("Result_figures/DESeq_plots/boxplots/all_otu_sampletype_final.pdf"), width = 30, height = 15, units = "cm")

# ------------------------------------------------------------------------------------------------------------------------------

plot_genus_deseq <- function(deseq_result_table, facet_plot = T, limit_to = NULL, title = NULL, pallete = my_colour_pallete_30_distinct,
                             include_grid = F,point_size =1 ){
  internal_table <- deseq_result_table
  # Separate into different taxa levels
  internal_table <- tidyr::separate(internal_table,col = "Taxonomy", into = c("Domain", "Phylum", "Class", "Order","Family", "Genus"), sep = ";")
  # Combine the groups into single variable
  internal_table$Group_1_Group_2 <- with(internal_table, paste0(Group_1, "_vs_", Group_2))
  
  if (!is.null(limit_to)){
    internal_table <- internal_table[grep(limit_to, internal_table$Group_1_Group_2),]
  }
  
  # Order by the log fold change
  internal_table <- internal_table[order(internal_table$log2FoldChange),]
  # if (facet_plot == T){
  #   internal_table <- internal_table[order(internal_table$Group_1_Group_2),]
  # }
  
  # Get the maximum log fold value for each genus
  x <- with(internal_table, tapply(log2FoldChange, Genus, function(x) max(x)))
  x <- sort(x, T)
  
  # Order the levels of the genus by the value
  internal_table$Genus <- factor(internal_table$Genus, levels = names(x))
  
  myplot <- ggplot(internal_table, aes(x= Genus, y= log2FoldChange))
  
  if (include_grid == T){
    myplot <- myplot + background_grid(major = "x", colour.major = "grey", size.major = .1)
    # minor = "x", colour.minor = "red", size.minor = .1)
    # only_minor
  }
  myplot <- myplot + 
    geom_hline(yintercept = 0, linetype = "dashed", size =.3, colour = "grey") +
    geom_point(aes(fill = Class), shape = 21,stroke = .1,size = point_size, color = "black", alpha= .8) +
    # geom_point(aes(fill = Class, color = Sample_Type), shape = 21,stroke = .1,size = 2, alpha= .8) +
    scale_fill_manual(values = pallete) +
    theme(axis.text.x = element_text(angle = 90,size = 6,vjust = .5),
          axis.text.y = element_text(size = 6),
          axis.title = element_text(size = 6),
          plot.title = element_text(size = 8),
          strip.text = element_text(size= 6),
          strip.background = element_blank(),
          legend.title = element_text(size = 6,face = "bold"),
          legend.text = element_text(size = 6),
          legend.title.align = 0.5,
          legend.margin = margin(c(2,2,2,2)),
          legend.key.height=unit(.4,"cm"),
          axis.line.x = element_line(size = 0.3),
          axis.line.y = element_line(size = 0.3),
          axis.ticks = element_line(size = 0.3)
    )
  
  if (!is.null(title)){
    myplot <- myplot + ggtitle(title)
  }
  
  if (facet_plot == T){
    myplot <- myplot + 
      # facet_wrap(~Group_1_Group_2, ncol = 1) 
      facet_rep_wrap(~Group_1_Group_2, ncol = 1, repeat.tick.labels = F) # from 'lemon'. To keep ticks in facets.
    # geom_hline(yintercept=max(internal_table$log2FoldCchange))
    # scale_x_discrete(breaks = levels(internal_table$Genus))
  }
  return(myplot)
}

# ----------------------------------------------------------------------------------------------------
# Load the deseq data. Assume that there is a Group_1 and Group_2 column

# Per patient results
sampletype_pooled_otu_deseq <- read.csv("Result_tables/DESeq_results/by_patient/patient_otu__Sampletype_pooled_combined.csv", header = T)
sampletype_pooled_genus_deseq <- read.csv("Result_tables/DESeq_results/by_patient/patient_genus__Sampletype_pooled_combined.csv", header = T)

immunosuppressed_sampletype_pooled_otu_deseq <- sampletype_pooled_otu_deseq[grepl("MST", sampletype_pooled_otu_deseq$Variable),]
immunosuppressed_sampletype_pooled_genus_deseq <- sampletype_pooled_genus_deseq[grepl("MST", sampletype_pooled_genus_deseq$Variable),]


myplot <- plot_genus_deseq(immunosuppressed_sampletype_pooled_otu_deseq,facet_plot = T,
                           limit_to = ".", title = "Immunosuppressed\nDifferentially abundant features between lesion types\ncalculated by patient at feature level",
                           pallete = my_colour_pallete_20,include_grid = T)

ggsave(filename = paste0("Result_figures/DESeq_plots/immunosuppressed_by_patient_otu_DESeq.pdf"),
       plot = myplot,
       width = 15,
       height = 15,
       units = "cm")

myplot <- plot_genus_deseq(immunosuppressed_sampletype_pooled_genus_deseq,facet_plot = T,
                           limit_to = ".", title = "Immunosuppressed\nDifferentially abundant features between lesion types\ncalculated by patient at genus level",
                           pallete = my_colour_pallete_20,include_grid = T)

ggsave(filename = paste0("Result_figures/DESeq_plots/immunosuppressed_by_patient_genus_DESeq.pdf"),
       plot = myplot,
       width = 10,
       height = 15,
       units = "cm")



immunocompetent_sampletype_pooled_otu_deseq <- sampletype_pooled_otu_deseq[!grepl("MST", sampletype_pooled_otu_deseq$Variable),]
immunocompetent_sampletype_pooled_genus_deseq <- sampletype_pooled_genus_deseq[!grepl("MST", sampletype_pooled_genus_deseq$Variable),]

myplot <- plot_genus_deseq(immunocompetent_sampletype_pooled_otu_deseq,facet_plot = T,
                           limit_to = ".", title = "Immunocompetent\nDifferentially abundant features between lesion types\ncalculated by patient at feature level",
                           pallete = my_colour_pallete_20,include_grid = T) 
myplot

ggsave(filename = paste0("Result_figures/DESeq_plots/immunocompetent_by_patient_otu_DESeq.pdf"),
       plot = myplot,
       width = 10,
       height = 12,
       units = "cm")

myplot <- plot_genus_deseq(immunocompetent_sampletype_pooled_genus_deseq,facet_plot = T,
                           limit_to = ".", title = "Immunocompetent\nDifferentially abundant features between lesion types\ncalculated by patient at genus level",
                           pallete = my_colour_pallete_20,include_grid = T) 
myplot

ggsave(filename = paste0("Result_figures/DESeq_plots/immunocompetent_by_patient_genus_DESeq.pdf"),
       plot = myplot,
       width = 10,
       height = 12,
       units = "cm")
# ----------------------------------------------------------------------------------------------------
# Number of meds
immunosuppressed_number_of_meds_otu_deseq <- read.csv("Result_tables/DESeq_results/immunosuppressed_otu_Number_of_meds.csv", header = T)
immunosuppressed_number_of_meds_genus_deseq <- read.csv("Result_tables/DESeq_results/immunosuppressed_genus_Number_of_meds.csv", header = T)
myplot <- plot_genus_deseq(immunosuppressed_number_of_meds_otu_deseq,facet_plot = T,
                           limit_to = ".", title = "Immunosuppressed\nDifferentially abundant features\nnumber of medications",
                           pallete = my_colour_pallete_20,include_grid = T) 
myplot

ggsave(filename = paste0("Result_figures/DESeq_plots/immunosuppressed_Number_of_meds_DESeq.pdf"),
       plot = myplot,
       width = 10,
       height = 12,
       units = "cm")
# ----------------------------------------------------------------------------------------------------
# Patient group
immunosuppressed_patient_group_otu_deseq <- read.csv("Result_tables/DESeq_results/immunosuppressed_otu_Patient_group.csv", header = T)
immunosuppressed_patient_group_genus_deseq <- read.csv("Result_tables/DESeq_results/immunosuppressed_genus_Patient_group.csv", header = T)
myplot <- plot_genus_deseq(immunosuppressed_patient_group_otu_deseq,facet_plot = T,
                           limit_to = ".", title = "Immunosuppressed\nDifferentially abundant features\nPatient group",
                           pallete = my_colour_pallete_20,include_grid = T) 
myplot

ggsave(filename = paste0("Result_figures/DESeq_plots/immunosuppressed_Patient_group_DESeq.pdf"),
       plot = myplot,
       width = 10,
       height = 12,
       units = "cm")

# ----------------------------------------------------------------------------------------------------
# Per cohort results (comparing same lesion type between different cohorts)
lesion_cohort_otu_deseq <- read.csv("Result_tables/DESeq_results/by_lesion_cohort/lesion_cohort_otu_combined.csv", header = T)
lesion_cohort_genus_deseq <- read.csv("Result_tables/DESeq_results/by_lesion_cohort/lesion_cohort_genus_combined.csv", header = T)

myplot <- plot_genus_deseq(lesion_cohort_otu_deseq,facet_plot = T,
                           limit_to = ".", title = "Differentially abundant features\nSame lesion type between cohorts at feature level",
                           pallete = my_colour_pallete_20,include_grid = T) 
myplot


ggsave(filename = paste0("Result_figures/DESeq_plots/lesion_type_between_cohorts_otu_DESeq.pdf"),
       plot = myplot,
       width = 12,
       height = 14,
       units = "cm")

myplot <- plot_genus_deseq(lesion_cohort_genus_deseq,facet_plot = T,
                           limit_to = ".", title = "Differentially abundant features\nSame lesion type between cohorts at genus level",
                           pallete = my_colour_pallete_20,include_grid = T) 
myplot


ggsave(filename = paste0("Result_figures/DESeq_plots/lesion_type_between_cohorts_genus_DESeq.pdf"),
       plot = myplot,
       width = 16,
       height = 18,
       units = "cm")
# ----------------------------------------------------------------------------------------------------
immunosuppressed_sampletype_pooled_otu_deseq <- read.csv("Result_tables/DESeq_results/immunosuppressed_otu_sampletype_pooled.csv", header = T)
immunosuppressed_sampletype_pooled_genus_deseq <- read.csv("Result_tables/DESeq_results/immunosuppressed_genus_sampletype_pooled.csv", header = T)

myplot_otu <- plot_genus_deseq(immunosuppressed_sampletype_pooled_otu_deseq,facet_plot = T,
                           limit_to = ".", title = "Immunosuppressed\nDifferentially abundant features\nSampletype pooled",
                           pallete = my_colour_pallete_20,include_grid = T) 

myplot_genus <- plot_genus_deseq(immunosuppressed_sampletype_pooled_genus_deseq,facet_plot = T,
                               limit_to = ".", title = "Immunosuppressed\nDifferentially abundant features\nSampletype pooled",
                               pallete = my_colour_pallete_20,include_grid = T) 

ggsave(filename = paste0("Result_figures/DESeq_plots/immunosuppressed_sampletype_pooled_otu_DESeq.pdf"),
       plot = myplot_otu,
       width =8,
       height = 12,
       units = "cm")

ggsave(filename = paste0("Result_figures/DESeq_plots/immunosuppressed_sampletype_pooled_genus_DESeq.pdf"),
       plot = myplot_genus,
       width = 10,
       height = 12,
       units = "cm")


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# Boxplots for the abundances / log(read count, 10) for significantly differential features


# -------------------------------------------
# Patient group, immunosuppressed

# Filter data to significant OTUs
# otu_data_filtered.df <- otu_data.df[otu_data.df$OTU.ID %in% immunosuppressed_patient_group_otu_deseq$OTU,]
# 
# # Filter to immunosuppressed
# otu_data_filtered.df <- otu_data_filtered.df[otu_data_filtered.df$Project == "immunosuppressed",]
# # otu_data_filtered.df[otu_data_filtered.df$OTU.ID == "32f5ab03a3b49ab88fad406ac4592280",]$Patient_group
# 
# # Combine deseq results with read counts/metadata
# otu_data_filtered.df <- merge(otu_data_filtered.df, immunosuppressed_patient_group_otu_deseq,by.x = "OTU.ID", by.y ="OTU")
# otu_data_filtered.df <- otu_data_filtered.df[otu_data_filtered.df$Sampletype != "negative",]
# 
# # Set the ordering of the patient group levels (variable of interest)
# otu_data_filtered.df$Patient_group <- factor(otu_data_filtered.df$Patient_group, levels = c("Control", "AK", "SCC"))
# 
# # Create the facet label. Genus (or lowest taxa) with OTU hash
# otu_data_filtered.df$label <- with(otu_data_filtered.df, paste0(Genus, "\n", OTU.ID))
# temp <- otu_data_filtered.df[grepl("Unassigned", otu_data_filtered.df$label),]
# last_assigned_taxa <- unlist(lapply(strsplit(gsub(";Unassigned.*","", temp$taxonomy_genus), ";"), function(x) rev(x)[1]))
# otu_data_filtered.df[grepl("Unassigned", otu_data_filtered.df$label),]$label <- with(otu_data_filtered.df[grepl("Unassigned", otu_data_filtered.df$label),], 
#                                                                                      paste0(last_assigned_taxa, "\n", OTU.ID))
# 
# fill_colours <- setNames(unique(as.character(otu_data_filtered.df$Patient_group_colour)), unique(as.character(otu_data_filtered.df$Patient_group)))
# 
# # Generate the individual plots
# plot_list <- list()
# for (otu in unique(otu_data_filtered.df$OTU.ID)){
#   data_subset <- subset(otu_data_filtered.df, OTU.ID == otu)
#   missing_groups <- unique(otu_data_filtered.df$Patient_group)[!unique(otu_data_filtered.df$Patient_group) %in% data_subset$Patient_group]
#   for (mg in missing_groups){
#     temp <- data_subset[1,]
#     temp$OTU.ID <- otu
#     temp$Patient_group <- mg
#     temp$Read_count_rarefied <- NA
#     data_subset <- rbind(data_subset, temp)
#   }
#   
#   myplot <- ggplot(data_subset, aes(x = Patient_group, y = log(Read_count_rarefied,10))) +
#     geom_boxplot(outlier.shape = NA, aes(fill = Patient_group)) +
#     scale_fill_manual(values = fill_colours, name = "Patient group") +
#     geom_jitter(size=0.5, width = 0.10, height=0) +
#     guides(fill=FALSE) +
#     ggtitle(unique(data_subset$label)) +
#     scale_y_continuous(limits = c(0,4)) +
#     # xlab("Patient group") +
#     xlab("") +
#     ylab("Log 10 (read count)")  +
#     common_theme +
#     theme(strip.text = element_text(size = 6))
#     
#   # signifiance.df <- unique(data_subset[c("Group_1","Group_2","padj")])
#   # for (i in 1:nrow(signifiance.df)){
#   #   group_1 <- signifiance.df[i,"Group_1"]
#   #   group_2 <- signifiance.df[i,"Group_2"]
#   #   myplot <- myplot + geom_signif(comparisons = list(c(group_1, group_2)), annotation =c("*"),textsize = 6,) # ggsignif package
#   # }
#   
#     
#   plot_list[[gsub("\n","_",unique(data_subset$label))]] <- myplot
#   outname <- paste0(as.character(unique(data_subset$Genus)), "__", as.character(unique(data_subset$OTU.ID)))
#   ggsave(plot = myplot,filename =  paste0("Result_figures/DESeq_plots/boxplots/patient_group/", outname, "_patient_group.pdf"), width = 10, height = 10, units = "cm")
# }

# Create legend by creating plot with everything to ensure legend is complete
# fill_colours <- setNames(unique(as.character(otu_data_filtered.df$Patient_group_colour)), unique(as.character(otu_data_filtered.df$Patient_group)))
# all_sample_plot_for_colour <- ggplot(otu_data_filtered.df, aes(x = Patient_group, y = log(Read_count_rarefied,10))) +
#   geom_boxplot(aes(fill = Patient_group)) +
#   scale_fill_manual(values = fill_colours, name = "Patient group")
# 
# my_legend_colour <- cowplot::get_legend(all_sample_plot_for_colour + theme(legend.position = "right", 
#                                                                            legend.text = element_text(size = 9),
#                                                                            legend.title = element_text(size =10, face = "bold"),
#                                                                            legend.justification = "center",
#                                                                            plot.margin = unit(c(0, 0, 0, 0), "cm")))
# 
# grid_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 5)
# grid_plot <- plot_grid(grid_plot, my_legend_colour, rel_widths = c(1,.4), ncol = 2)
# ggsave(plot = grid_plot,filename =  paste0("Result_figures/DESeq_plots/boxplots/all_patient_group.pdf"), width = 50, height = 40, units = "cm")


