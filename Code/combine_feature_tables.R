library(dplyr)
library(tidyr)
library(reshape2)


# ------------------------------------------------------------------------------------------------------
# Load and process the OTU tables into a single one

# Set the working directory
setwd("/Users/julianzaugg/Desktop/ACE/major_projects/skin_microbiome_16S/")
mydir <- "data/feature_statistics/feature_statistics_skin_single_qc"

myfiles <- list.files(mydir)
my_data_frame <- NULL
for (myfile in myfiles){
  if (is.null(my_data_frame)){
    mydata.df <- read.csv(file = paste0(mydir,"/",myfile), header =T)
    # temp <- melt(mydata.df[names(mydata.df)[!names(mydata.df) %in% c("Frequency", "Confidence","RepSeq")]])
    temp <- melt(mydata.df[names(mydata.df)[!names(mydata.df) %in% c("Frequency", "Confidence")]])
    temp <- temp[temp$value != 0,]
    my_data_frame <- temp
  }else{
    mydata.df <- read.csv(file = paste0(mydir,"/",myfile), header =T)
    # temp <- melt(mydata.df[names(mydata.df)[!names(mydata.df) %in% c("Frequency", "Confidence","RepSeq")]])
    temp <- melt(mydata.df[names(mydata.df)[!names(mydata.df) %in% c("Frequency", "Confidence")]])
    temp <- temp[temp$value != 0,]
    # my_data_frame <- full_join(my_data_frame, new.df, by = "X.OTU.ID")
    my_data_frame <- rbind(my_data_frame, temp)
  }
}
# my_data_frame <- my_data_frame[c("X.OTU.ID", "variable", "value", "Taxon","RepSeq")]

# Requires a lot of memory. May need to be run on server.
my_data_frame_spread <- my_data_frame %>% spread(variable,value,fill = 0)
my_data_frame_spread <- my_data_frame_spread[c(names(my_data_frame_spread)[!names(my_data_frame_spread) %in% c("Taxon", "RepSeq")], "Taxon", "RepSeq")]
names(my_data_frame_spread)[1] <- "OTU.ID"

write.csv(x = my_data_frame_spread, file = "data/feature_statistics/feature_statistics_single_qc_combined.csv", quote = F, row.names = F)

