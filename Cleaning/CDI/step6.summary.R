
# Clear previous data
rm(list = ls())

#load data
library(dplyr)

#--------------------------------------------------------# 
#This is to merge all of the final raw CDI's to create one big dataframe of everything 

B_CDI <- read.csv(file.path('raw-csv', '5.B1_CDI_FINALRAW.csv'), header=TRUE, stringsAsFactors=FALSE)
B2_CDI <- read.csv(file.path('raw-csv','5.B2_CDI_FINALRAW.csv'), header=TRUE, stringsAsFactors=FALSE)
S_CDI<- read.csv(file.path('raw-csv','5.S1_CDI_FINALRAW.csv'), header=TRUE, stringsAsFactors=FALSE)
S2_CDI <- read.csv(file.path('raw-csv','5.S2_CDI_FINALRAW.csv'), header=TRUE, stringsAsFactors=FALSE)

#Merge all parts of summary CDI
merged_CDI_raw <- rbind(B_CDI, B2_CDI, S_CDI, S2_CDI)


#save as the cleaned summary csv
write.csv(merged_CDI_raw,
          file.path('raw-csv', 'Summary', '9.merged_CDI_raw.csv'),
          row.names=FALSE)


