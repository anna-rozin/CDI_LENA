
# Clear previous data
rm(list = ls())

#Load data
final_df <- read.csv(file.path('raw-csv', '5.S2_CDI_FINALRAW.csv'),
                header=TRUE,
                stringsAsFactors=FALSE)

####__________
#Creating a summary df that will be merged with lena

# Define the columns to be removed
columns_to_remove <- c("part_code", "part", "item_code", "response")

# Create a new dataframe with the specified columns removed
summary_df <- final_df[, !(names(final_df) %in% columns_to_remove)]


#save as summary csv
write.csv(summary_df, 
          file.path('raw-csv', 'Summary', '6.S2_CDI_Summary.csv'),
          row.names=FALSE)
