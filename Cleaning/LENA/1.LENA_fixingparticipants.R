# Clear previous data
rm(list = ls())

# Set working directory

# Load data
lena <- read.csv(file.path('source', 'LENA', 'LENAExport_Hourly_20250405_165607.csv'), header=TRUE, stringsAsFactors=FALSE)
incorrect_links <- read.csv(file.path('source', 'LENA', 'LENA.incorrect.csv'), header=TRUE, stringsAsFactors=FALSE)

# Print the column names to ensure they are as expected
print("Column names in lena:")
print(colnames(lena))

print("Column names in incorrect_links:")
print(colnames(incorrect_links))

# Clearing the column ChildKey so that it is clear what the LENA_ID is, to match with the corrected CSV file
# Extract the last 6 digits from the "ChildKey" column
lena$ChildKey <- substr(lena$ChildKey, nchar(lena$ChildKey) - 5, nchar(lena$ChildKey))


######

# Find the row in lena where ChildKey is _ and RecorderSerialNumber is _
matching_row <- lena[lena$ChildKey == 114949 & lena$RecorderSerialNumber == 60052, "ExternalReferenceID"]

# Print the matching ExternalReferenceID
print(matching_row)

#it gave me an output of ID 7056, which is incorrect

# Ensure data types are consistent (convert to character for safe comparison)
lena$ChildKey <- as.character(lena$ChildKey)
lena$Recording.ID <- as.character(lena$Recording.ID)
lena$ExternalReferenceID <- as.character(lena$ExternalReferenceID)
incorrect_links$ParticipantID <- as.character(incorrect_links$ParticipantID)
incorrect_links$Recording.ID <- as.character(incorrect_links$Recording.ID)
incorrect_links$ID.incorrect <- as.character(incorrect_links$ID.incorrect)
incorrect_links$ID.correct <- as.character(incorrect_links$ID.correct)

# Perform the update
for (i in 1:nrow(incorrect_links)) {
  match_index <- which(lena$ChildKey == incorrect_links$ParticipantID[i] & lena$Recording.ID == incorrect_links$Recording.ID[i])
  
  if (length(match_index) > 0) {
    lena$ExternalReferenceID[match_index] <- incorrect_links$ID.correct[i]
    print(paste("Updated ExternalReferenceID for match at row:", match_index))
  } else {
    print(paste("No match found for incorrect_links row:", i))
  }
}


####################### Check if it worked

# Find the row in lena where ChildKey matches 114949 and RecorderSerialNumber matches 60052
matching_row <- lena[lena$ChildKey == 114949 & lena$RecorderSerialNumber == 60052, "ExternalReferenceID"]

# Print the matching ExternalReferenceID
print(matching_row)

#The ID has changed to the corrected one which is 7059


# another check 
matching_row <- lena[lena$ChildKey == 112821 & lena$RecorderSerialNumber == 60045, "ExternalReferenceID"]

# Print the matching ExternalReferenceID
print(matching_row)
#7120 it is correct 

#save as corrected lena csv

write.csv(lena, 
          file.path('raw-csv1', 'lena_corrected_IDs.csv'),
          row.names=FALSE)

