##------------------------------------------------------------##
#This page is to transpose the data

# Load data one by one which has been cleaned 

dat_S1 <- read.csv(file.path('raw-csv1', '1.S1_CDI_nontransposed.csv'),
                header=TRUE,
                stringsAsFactors=FALSE)
dat_S2 <- read.csv(file.path('raw-csv1', '1.S2_CDI_nontransposed.csv'),
                header=TRUE,
                stringsAsFactors=FALSE)
dat_B1 <- read.csv(file.path('raw-csv1', '1.B1_CDI_nontransposed.csv'),
                header=TRUE,
                stringsAsFactors=FALSE)
dat_B2 <- read.csv(file.path('raw-csv1', '1.B2_CDI_nontransposed.csv'),
                header=TRUE,
                stringsAsFactors=FALSE)


# Function to process each dataset
process_data <- function(dat) {
  
### Creating two data frames ###
  
# Identify columns to transpose based on a pattern (starting with "VOC", "COM", or "PROTEM")
cols_to_transpose <- grep("^VOC|^COM|^PROTEM|^GES|^JUE|^ACC", colnames(dat), value = TRUE)

# Keep all columns except those to transpose
cols_to_keep <- setdiff(colnames(dat), cols_to_transpose)

# Create two new data frames: df1 (part to transpose) and df2 (part to keep as is)
df1 <- dat[, c("ID", cols_to_transpose)]
df2 <- dat[, c("ID", cols_to_keep)]

# Print the first few rows of each data frame to verify
head(df1)
head(df2)


### Transposing the data ### ------------

# Load the tidyr package
library(tidyr)

# Transpose df1 while keeping IDs intact
df1_transposed <- pivot_longer(df1, 
                               cols = -ID,  # Exclude the ID column
                               names_to = "item",  # Name of the new column storing original column names
                               values_to = "value"  # Name of the new column storing cell values
)


#### MERGE df1 and df2 -----------------------
#Assuming 'df1_transposed' is your transposed data frame and 'df2' is the other data frame

# Merge df2 with transposed df1 based on the common ID column
final_df <- merge(df2, df1_transposed, by = "ID")

# end
return(final_df)
}

#### apply to each data frame -------
# Apply the function to each data frame
dat_S1_processed <- process_data(dat_S1)
dat_S2_processed <- process_data(dat_S2)
dat_B1_processed <- process_data(dat_B1)
dat_B2_processed <- process_data(dat_B2)

# merge all the transformed data sets
final_df <- rbind(dat_S1_processed, dat_S2_processed, dat_B1_processed, dat_B2_processed)

##------------------------------------------------------------##
#save as transposed csv

write.csv(final_df, 
          file.path('raw-csv1', '2.merged_CDI_transposed.csv'),
          row.names=FALSE)

