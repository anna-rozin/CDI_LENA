
# Clear previous data
rm(list = ls())

# Load data
lena_summary <- read.csv(file.path('raw-csv','Summary','1.lena_Summary.csv'), header=TRUE, stringsAsFactors=FALSE)
CDI_summary <- read.csv(file.path('raw-csv','Summary','5.merged_CDI_final.csv'), header=TRUE, stringsAsFactors=FALSE)

# Perform the merge based on the common identifier, keeping all columns
merged_cdi_lena <- merge(
  CDI_summary, lena_summary, 
  by.x = "ID_lab", by.y = "ExternalReferenceID", 
  all = FALSE
)

#checking 
# Number of unique participants that matched
merged_cdi_lena %>%
  distinct(ID_lab) %>%
  count(name = "matched_participants")

#save as CSV
write.csv(merged_cdi_lena, 
          file.path('raw-csv1', 'Summary', '7.merged_CDIs_summary.csv'),
          row.names=FALSE)








##### This is what I did before and it was not working well for the merge  so use the code above 

# Perform the merge based on the common identifier, keeping all columns
merged_CDI_LENA <- merge(CDI_summary, lena_summary, by.x = "ID_lab", by.y = "ExternalReferenceID", all.x = TRUE, all.y = TRUE)

#check how many matched 
library(dplyr)

merged_CDI_LENA %>%
  filter(!is.na(ID_lab) & !is.na(ExternalReferenceID)) %>%
  distinct(ID_lab) %>%
  count(name = "matched_participants")


# Check the number of rows for each participant after the merge
merged_row_counts <- merged_CDI_LENA%>%
  group_by(ID_lab) %>%
  summarise(n = n())

# View the result
print(merged_row_counts)

###########

#save as CSV
write.csv(merged_CDI_LENA, 
          file.path('raw-csv', 'Summary', '7.merged_CDIs_summary.csv'),
          row.names=FALSE)



#checking ------------------------
num_unique_ids <- CDI_summary %>% 
  summarize(unique_IDs = n_distinct(ID_lab))

# Print the result
print(num_unique_ids)

str(lena_summary$ID_lab)
str(CDI_summary$ID_lab)




