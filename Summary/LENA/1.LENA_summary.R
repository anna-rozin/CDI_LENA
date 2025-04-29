### Set working directory and load data

# Clear previous data
rm(list = ls())

lena <- read.csv(file.path('raw-csv1', 'lena_corrected_IDs.csv'),
                header=TRUE,
                stringsAsFactors=FALSE)


###___________________________

# Print all column names in the lena dataframe
print("Column names in lena:")
print(colnames(lena))


library(dplyr)

# Select the desired columns to create summary_lena
summary_lena <- lena[, c("Recording.ID", "ChildKey", "ExternalReferenceID", "Recording_Gender", "Recording_DOB", "RecordingDate",
                         "StartTime", "EndTime","ParticipantID", "CT_COUNT", "AWC_COUNT", "CV_COUNT", "Overlap")]
#save as a summary csv 

write.csv(summary_lena, 
          file.path('raw-csv1', 'Summary', 'lena_Summary.csv'),
          row.names=FALSE)



#------------ checking why the # of pp is so high 
correct <- read.csv(file.path('source','LENA', 'updated_DOB.csv'),
                 header=TRUE,
                 stringsAsFactors=FALSE)


# Find matched ExternalReferenceID values (those that appear in both datasets)
matched_info <- lena %>% 
  filter(ExternalReferenceID %in% correct$ID_lab) %>% 
  distinct(ExternalReferenceID)

# Find unmatched ExternalReferenceID values (those that do not appear in the correct dataset)
unmatched_info <- lena %>% 
  filter(!(ExternalReferenceID %in% correct$ID_lab)) %>% 
  distinct(ExternalReferenceID)

# Print the results
print("Matched IDs:")
print(matched_info)

print("Unmatched IDs:")
print(unmatched_info)









#checking the matches 
unmatched_ids <- lena %>%
  filter(!(ExternalReferenceID %in% correct$ID_lab)) %>%
  distinct(ExternalReferenceID)

print(unmatched_ids)


#checking the non-matches 
unmatched_info <- lena %>%
  filter(!(ExternalReferenceID %in% correct$ID_lab)) %>%
  dplyr::select(ExternalReferenceID, ChildKey, Recording_Gender, RecordingDate) %>%
  distinct()

print(unmatched_info)



# Count the number of unique ExternalReferenceIDs
num_unique_ids <- lena %>%
  summarize(unique_ids = n_distinct(ExternalReferenceID))

# Print the result
print(num_unique_ids)

# Count how many are NA or empty
lena %>%
  filter(is.na(ExternalReferenceID) | ExternalReferenceID == "") %>%
  nrow()
unique_ids <- lena %>%
  distinct(ExternalReferenceID) %>%
  arrange(ExternalReferenceID)

print(unique_ids)


summary_lena %>%
  filter(is.na(ExternalReferenceID) | ExternalReferenceID == "" | trimws(ExternalReferenceID) == "") %>%
  distinct(ChildKey) %>%
  pull(ChildKey)

