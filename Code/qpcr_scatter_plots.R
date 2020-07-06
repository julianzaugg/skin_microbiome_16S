# Script to generate the mean relative abundance and bacterial load figure for publication

# invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)


common_theme <- theme(
  panel.border = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
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
  legend.key.height= unit(.3,"cm"),
  legend.key.width = unit(.3,"cm"),
  legend.text = element_text(size = 8),
  axis.line = element_line(colour = "black", size = 0.5),
  axis.text = element_text(size = 6, colour = "black"),
  axis.title = element_text(size = 7,face = "bold"),
  complete = F,
  plot.title = element_text(size = 8))


setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
source("Code/helper_functions.R")

# Load filtered/processed abundance data with metadata
# genus_data.df <- read.csv("Result_tables/combined_counts_abundances_and_metadata_tables/genus_counts_abundances_and_metadata.csv")
# genus_data.df <- genus_data.df[genus_data.df$Sample %in% rownames(metadata.df),]


# Scatter plots for qPCR values 
# temp <- genus_data.df %>% select(Sample,Cohort, Sample_type, Sample_type_colour, S_aureus_qPCR, Staph_spp_qPCR, qPCR_16S) %>% unique()
consolidated_metadata.df <- read.csv("Result_tables/other/metadata_consolidated.csv")
unique(consolidated_metadata.df[c("Sample_type","Sample_type_colour")])

consolidated_metadata.df <- consolidated_metadata.df[consolidated_metadata.df$Sample_type != "negative",]
# consolidated_metadata.df <- left_join(consolidated_metadata.df, unique(genus_data.df[,c("Sample_type", "Sample_type_colour")]), by = "Sample_type")

# temp2 <- consolidated_metadata.df %>% 
#   melt(measure.vars = c("S_aureus_qPCR", "Staph_spp_qPCR", "qPCR_16S","Human_qPCR", "Total_DNA"), variable.name = "Assay") %>%
#   group_by(Cohort, Sample_type, Assay) %>%
#   filter(!is.na(value)) %>%
#   dplyr::summarise(Mean_value = mean(value),
#                    SD_value = sd(value),
#                    N_samples = n()) %>%
#   as.data.frame()
# ggplot(temp2, aes(x = factor(paste0(Sample_type, "__", Assay)), y = Mean_value)) +
#   geom_bar(stat = "identity", aes(fill = Sample_type)) + 
#   geom_errorbar(aes(ymin = Mean_value - sqrt(SD_value), ymax = Mean_value + sqrt(SD_value))) +
#   facet_wrap(~Cohort, ncol = 1) +
#   theme_light() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
#   scale_y_continuous(limits = c(0, 1000))

temp <- consolidated_metadata.df %>% select(Index,Swab_ID, Cohort, Sample_type, Sample_type_colour, 
                                            S_aureus_qPCR, Staph_spp_qPCR, qPCR_16S,Human_qPCR, Total_DNA) %>% unique()

# names(temp) <- c("Sample", "Cohort", "Sample_type", "Sample_type_colour","S. aur", "S. spp", "Total 16S")
names(temp) <- c("Index","Swab_ID",  "Cohort", "Sample_type", "Sample_type_colour","S. aur", "S. spp", "Total 16S","Human_qPCR", "Total_DNA")
temp <- melt(temp, measure.vars = c("S. aur", "S. spp", "Total 16S","Human_qPCR","Total_DNA"), variable.name = "Assay")
temp$Sample_type <- factor(temp$Sample_type, levels = c("HS", "PDS", "AK", "SCC_PL", "SCC"))
temp$Group <- factor(with(temp, paste0(Sample_type, " (", Assay, ")")))

temp$Group <- factor(temp$Group, as.character(unique(temp$Group[order(temp$Sample_type)])))

variable_shapes <- setNames(rep(c(25,24,23,22,21),length(unique(temp$Sample_type)))[1:length(unique(temp$Sample_type))],unique(temp$Sample_type))
temp$Cohort <- factor(temp$Cohort, levels = c("immunosuppressed", "immunocompetent"))

temp$value_x100_log_10 <- log(temp$value*100,10)
temp$value_x100_log_10[is.infinite(temp$value_x100_log_10)] <- 0
temp <- temp %>%
  dplyr::group_by(Cohort, Sample_type, Assay) %>%
  dplyr::mutate(Mean_value = mean(value,na.rm = T),
                Mean_value_x100_log_10 = log(Mean_value *100,10)) %>%
  as.data.frame()
temp <- temp[!is.na(temp$value),]
unique(temp[c("Sample_type", "Sample_type_colour")])

# temp[is.na(temp)] <- ""
write.csv(temp,file = "Result_tables/abundance_analysis_tables/qpcr_plot_table.csv", quote = F, row.names = F)

immunosuppressed_qcpr <- subset(temp, Cohort == "immunosuppressed")
immunocompetent_qcpr <- subset(temp, Cohort == "immunocompetent")

immunosuppressed_qcpr$Sample_type <- factor(immunosuppressed_qcpr$Sample_type, levels = c("HS", "PDS", "AK", "SCC_PL", "SCC"))
immunocompetent_qcpr$Sample_type <- factor(immunocompetent_qcpr$Sample_type, levels = c("PDS", "AK", "SCC_PL", "SCC"))

immunosuppressed_qcpr_staph <- subset(immunosuppressed_qcpr, Assay %in% c("S. aur", "S. spp"))
immunocompetent_qcpr_staph <- subset(immunocompetent_qcpr, Assay %in% c("S. aur", "S. spp"))
immunosuppressed_qcpr_16S <- subset(immunosuppressed_qcpr, !Assay %in% c("S. aur", "S. spp"))
immunocompetent_qcpr_16S <- subset(immunocompetent_qcpr, !Assay %in% c("S. aur", "S. spp"))

# Staph specific qPCR plots
immunosuppressed_qcpr_plot <- 
  ggplot(immunosuppressed_qcpr_staph, 
         aes(x = Sample_type,
             y = value_x100_log_10,
             # y = value,
             colour = Sample_type_colour, 
             fill = Sample_type_colour,
             shape = Sample_type)) +
  geom_jitter(show.legend = F,size = .6, alpha = 1,position = position_jitter(width = .15)) +
  geom_errorbar(data = unique(immunosuppressed_qcpr_staph[,c("Sample_type", "Assay", "Mean_value_x100_log_10")]),
                aes(ymax = Mean_value_x100_log_10, ymin = Mean_value_x100_log_10,x = Sample_type),inherit.aes = F, 
                width = .2, linetype = "solid", colour = "black") +
  # stat_summary(fun = mean, geom = "errorbar",colour ="black", aes(ymax = ..y.., ymin = ..y..),
  # width = .2, linetype = "solid") +
  facet_wrap(~Assay,scales = "free_x") +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_shape_manual(values = variable_shapes) +
  scale_y_continuous(limits = c(-.1,5), breaks = seq(0,5,.5))+
  # coord_cartesian(ylim =  c(0,1000)) +
  labs(title = "Immunosuppressed") +
  # ylab("Genome equivalents per ul") +
  ylab(expression(log[10]~"(Genome equivalents per"~mu~"l x 100)")) +
  xlab("Sample type (Assay)") +
  # theme_minimal_grid() +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))
immunosuppressed_qcpr_plot

immunocompetent_qcpr_plot <- 
  ggplot(immunocompetent_qcpr_staph, 
         aes(x = Sample_type,
             y = value_x100_log_10,
             # y = value,
             colour = Sample_type_colour, 
             fill = Sample_type_colour,
             shape = Sample_type)) +
  geom_jitter(show.legend = F,size = .6, alpha = 1,position = position_jitter(width = .15)) +
  geom_errorbar(data = unique(immunocompetent_qcpr_staph[,c("Sample_type", "Assay", "Mean_value_x100_log_10")]),
                aes(ymax = Mean_value_x100_log_10, ymin = Mean_value_x100_log_10,x = Sample_type),inherit.aes = F, 
                width = .2, linetype = "solid", colour = "black") +
  # stat_summary(fun = mean, geom = "errorbar",colour ="black", aes(ymax = ..y.., ymin = ..y..),
  # width = .2, linetype = "solid") +
  facet_wrap(~Assay,scales = "free_x") +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_shape_manual(values = variable_shapes) +
  scale_y_continuous(limits = c(-.1,5), breaks = seq(0,5,.5))+
  # coord_cartesian(ylim =  c(0,1000)) +
  labs(title = "Immunocompetent") +
  ylab(expression(log[10]~"(Genome equivalents per"~mu~"l x 100)")) +
  # ylab("Genome equivalents per ul") +
  xlab("Sample type (Assay)") +
  # theme_minimal_grid() +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(filename = "Result_figures/abundance_analysis_plots/immunosuppressed_Staph_qcpr_plot.pdf",
       plot = immunosuppressed_qcpr_plot,
       width = 12,
       height = 8,
       units = "cm")

ggsave(filename = "Result_figures/abundance_analysis_plots/immunocompetent_Staph_qcpr_plot.pdf",
       plot = immunocompetent_qcpr_plot,
       width = 10,
       height = 8,
       units = "cm")

# -------------------------------------------------------
#           Total DNA + Human DNA + Microbial load 16S

# Immunosuppressed
immunosuppressed_total_dna_plot <- 
  ggplot(subset(immunosuppressed_qcpr_16S, Assay == "Total_DNA"), 
         aes(x = Sample_type,
             # y = value_x100_log_10,
             y = value,
             colour = Sample_type_colour, 
             fill = Sample_type_colour,
             shape = Sample_type)) +
  geom_jitter(show.legend = F,size = .6, alpha = 1,position = position_jitter(width = .15)) +
  # geom_errorbar(data = unique(immunosuppressed_qcpr_staph[,c("Sample_type", "Assay", "Mean_value_x100_log_10")]),
  #               aes(ymax = Mean_value_x100_log_10, ymin = Mean_value_x100_log_10,x = Sample_type),inherit.aes = F, 
  #               width = .2, linetype = "solid", colour = "black") +
  geom_errorbar(data = unique(subset(immunosuppressed_qcpr_16S[,c("Sample_type", "Assay", "Mean_value")],Assay == "Total_DNA")),
                aes(ymax = Mean_value, ymin = Mean_value,x = Sample_type),inherit.aes = F, 
                width = .2, linetype = "solid", colour = "black") +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_shape_manual(values = variable_shapes) +
  scale_y_continuous(limits = c(-.01,5), breaks = seq(0,5,.5))+
  # coord_cartesian(ylim =  c(0,1000)) +
  labs(subtitle = "Total DNA (Qubit)") +
  ylab("ng per ul") +
  # ylab(expression(log[10]~"(Genome equivalents per"~mu~"l x 100)")) +
  xlab("Sample type (Assay)") +
  # theme_minimal_grid() +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 6))

immunosuppressed_human_dna_plot <- 
  ggplot(subset(immunosuppressed_qcpr_16S, Assay == "Human_qPCR"), 
         aes(x = Sample_type,
             # y = value_x100_log_10,
             y = value,
             colour = Sample_type_colour, 
             fill = Sample_type_colour,
             shape = Sample_type)) +
  geom_jitter(show.legend = F,size = .6, alpha = 1,position = position_jitter(width = .15)) +
  # geom_errorbar(data = unique(immunosuppressed_qcpr_staph[,c("Sample_type", "Assay", "Mean_value_x100_log_10")]),
  #               aes(ymax = Mean_value_x100_log_10, ymin = Mean_value_x100_log_10,x = Sample_type),inherit.aes = F, 
  #               width = .2, linetype = "solid", colour = "black") +
  geom_errorbar(data = unique(subset(immunosuppressed_qcpr_16S[,c("Sample_type", "Assay", "Mean_value")],Assay == "Human_qPCR")),
                aes(ymax = Mean_value, ymin = Mean_value,x = Sample_type),inherit.aes = F, 
                width = .2, linetype = "solid", colour = "black") +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_shape_manual(values = variable_shapes) +
  scale_y_continuous(limits = c(25,41), breaks = seq(25,45,2.5))+
  # coord_cartesian(ylim =  c(0,1000)) +
  labs(subtitle = "Human DNA (ERV3)") +
  ylab("mean CT 40") +
  # ylab(expression(log[10]~"(Genome equivalents per"~mu~"l x 100)")) +
  xlab("Sample type") +
  # theme_minimal_grid() +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 6))
immunosuppressed_human_dna_plot

immunosuppressed_16s_microbial_load_plot <- 
  ggplot(subset(immunosuppressed_qcpr_16S, Assay == "Total 16S"), 
         aes(x = Sample_type,
             y = value_x100_log_10,
             # y = value,
             colour = Sample_type_colour, 
             fill = Sample_type_colour,
             shape = Sample_type)) +
  geom_jitter(show.legend = F,size = .6, alpha = 1,position = position_jitter(width = .15)) +
  geom_errorbar(data = unique(subset(immunosuppressed_qcpr_16S[,c("Sample_type", "Assay", "Mean_value_x100_log_10")],Assay == "Total 16S")),
                aes(ymax = Mean_value_x100_log_10, ymin = Mean_value_x100_log_10,x = Sample_type),inherit.aes = F,
                width = .2, linetype = "solid", colour = "black") +
  # geom_errorbar(data = unique(subset(immunosuppressed_qcpr_16S[,c("Sample_type", "Assay", "Mean_value")],Assay == "Total 16S")),
  #               aes(ymax = Mean_value, ymin = Mean_value,x = Sample_type),inherit.aes = F, 
  #               width = .2, linetype = "solid", colour = "black") +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_shape_manual(values = variable_shapes) +
  # scale_y_continuous(limits = c(-0.1,10100), breaks = seq(0,10000,1000))+
  scale_y_continuous(limits = c(-.001,6), breaks = seq(0,6,.5))+
  # coord_cartesian(ylim =  c(0,1000)) +
  labs(subtitle = "Microbial load (16S qPCR)") +
  # ylab("Genome equivalents per ul") +
  ylab(expression(log[10]~"(Genome equivalents per"~mu~"l x 100)")) +
  xlab("Sample type") +
  # theme_minimal_grid() +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 6))
immunosuppressed_16s_microbial_load_plot

# Create title
title <- ggdraw() + 
  draw_label(
    "Immunosuppressed",
    fontface = 'bold',
    size = 8
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

# Make a grid of plots
grid_plot <- plot_grid(plotlist = list(immunosuppressed_total_dna_plot,
                                       immunosuppressed_human_dna_plot,
                                       immunosuppressed_16s_microbial_load_plot),
                       ncol = 3,nrow=1, rel_widths = c(1,1,1),align = "hv")
grid_plot <- plot_grid(title,grid_plot, rel_heights = c(0.1,1), ncol = 1, nrow=2)


ggsave(filename = "Result_figures/abundance_analysis_plots/immunosuppressed_sample_type_total_human_16S_qPCR.pdf", 
       plot = grid_plot, width = 20, 
       height = 8, units = "cm")


# Immunocompetent
immunocompetent_total_dna_plot <- 
  ggplot(subset(immunocompetent_qcpr_16S, Assay == "Total_DNA"), 
         aes(x = Sample_type,
             # y = value_x100_log_10,
             y = value,
             colour = Sample_type_colour, 
             fill = Sample_type_colour,
             shape = Sample_type)) +
  geom_jitter(show.legend = F,size = .6, alpha = 1,position = position_jitter(width = .15)) +
  # geom_errorbar(data = unique(immunocompetent_qcpr_staph[,c("Sample_type", "Assay", "Mean_value_x100_log_10")]),
  #               aes(ymax = Mean_value_x100_log_10, ymin = Mean_value_x100_log_10,x = Sample_type),inherit.aes = F, 
  #               width = .2, linetype = "solid", colour = "black") +
  geom_errorbar(data = unique(subset(immunocompetent_qcpr_16S[,c("Sample_type", "Assay", "Mean_value")],Assay == "Total_DNA")),
                aes(ymax = Mean_value, ymin = Mean_value,x = Sample_type),inherit.aes = F, 
                width = .2, linetype = "solid", colour = "black") +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_shape_manual(values = variable_shapes) +
  scale_y_continuous(limits = c(-.01,5), breaks = seq(0,5,.5))+
  # coord_cartesian(ylim =  c(0,1000)) +
  labs(subtitle = "Total DNA (Qubit)") +
  ylab("ng per ul") +
  # ylab(expression(log[10]~"(Genome equivalents per"~mu~"l x 100)")) +
  xlab("Sample type (Assay)") +
  # theme_minimal_grid() +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 6))

immunocompetent_human_dna_plot <- 
  ggplot(subset(immunocompetent_qcpr_16S, Assay == "Human_qPCR"), 
         aes(x = Sample_type,
             # y = value_x100_log_10,
             y = value,
             colour = Sample_type_colour, 
             fill = Sample_type_colour,
             shape = Sample_type)) +
  geom_jitter(show.legend = F,size = .6, alpha = 1,position = position_jitter(width = .15)) +
  # geom_errorbar(data = unique(immunocompetent_qcpr_staph[,c("Sample_type", "Assay", "Mean_value_x100_log_10")]),
  #               aes(ymax = Mean_value_x100_log_10, ymin = Mean_value_x100_log_10,x = Sample_type),inherit.aes = F, 
  #               width = .2, linetype = "solid", colour = "black") +
  geom_errorbar(data = unique(subset(immunocompetent_qcpr_16S[,c("Sample_type", "Assay", "Mean_value")],Assay == "Human_qPCR")),
                aes(ymax = Mean_value, ymin = Mean_value,x = Sample_type),inherit.aes = F, 
                width = .2, linetype = "solid", colour = "black") +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_shape_manual(values = variable_shapes) +
  scale_y_continuous(limits = c(25,41), breaks = seq(25,45,2.5))+
  # coord_cartesian(ylim =  c(0,1000)) +
  labs(subtitle = "Human DNA (ERV3)") +
  ylab("mean CT 40") +
  # ylab(expression(log[10]~"(Genome equivalents per"~mu~"l x 100)")) +
  xlab("Sample type") +
  # theme_minimal_grid() +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 6))
immunocompetent_human_dna_plot

immunocompetent_16s_microbial_load_plot <- 
  ggplot(subset(immunocompetent_qcpr_16S, Assay == "Total 16S"), 
         aes(x = Sample_type,
             y = value_x100_log_10,
             # y = value,
             colour = Sample_type_colour, 
             fill = Sample_type_colour,
             shape = Sample_type)) +
  geom_jitter(show.legend = F,size = .6, alpha = 1,position = position_jitter(width = .15)) +
  geom_errorbar(data = unique(subset(immunocompetent_qcpr_16S[,c("Sample_type", "Assay", "Mean_value_x100_log_10")],Assay == "Total 16S")),
                aes(ymax = Mean_value_x100_log_10, ymin = Mean_value_x100_log_10,x = Sample_type),inherit.aes = F,
                width = .2, linetype = "solid", colour = "black") +
  # geom_errorbar(data = unique(subset(immunocompetent_qcpr_16S[,c("Sample_type", "Assay", "Mean_value")],Assay == "Total 16S")),
  #               aes(ymax = Mean_value, ymin = Mean_value,x = Sample_type),inherit.aes = F, 
  #               width = .2, linetype = "solid", colour = "black") +
  scale_colour_identity() + 
  scale_fill_identity() + 
  scale_shape_manual(values = variable_shapes) +
  # scale_y_continuous(limits = c(-0.1,10100), breaks = seq(0,10000,1000))+
  scale_y_continuous(limits = c(-.001,6), breaks = seq(0,6,.5))+
  # coord_cartesian(ylim =  c(0,1000)) +
  labs(subtitle = "Microbial load (16S qPCR)") +
  # ylab("Genome equivalents per ul") +
  ylab(expression(log[10]~"(Genome equivalents per"~mu~"l x 100)")) +
  xlab("Sample type") +
  # theme_minimal_grid() +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 6))
immunocompetent_16s_microbial_load_plot

# Create title
title <- ggdraw() + 
  draw_label(
    "Immunocompetent",
    fontface = 'bold',
    size = 8
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

# Make a grid of plots
grid_plot <- plot_grid(plotlist = list(immunocompetent_total_dna_plot,
                                       immunocompetent_human_dna_plot,
                                       immunocompetent_16s_microbial_load_plot),
                       ncol = 3,nrow=1, rel_widths = c(1,1,1),align = "hv")
grid_plot <- plot_grid(title,grid_plot, rel_heights = c(0.1,1), ncol = 1, nrow=2)


ggsave(filename = "Result_figures/abundance_analysis_plots/immunocompetent_sample_type_total_human_16S_qPCR.pdf", 
       plot = grid_plot, width = 20, 
       height = 8, units = "cm")

