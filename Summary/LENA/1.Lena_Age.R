### Set working directory and load data

# Clear previous data
rm(list = ls())

lena1 <- read.csv(file.path('raw-csv1', 'lena_corrected_IDs.csv'),
                 header=TRUE,
                 stringsAsFactors=FALSE)


###___________________________

# Print all column names in the lena dataframe
print("Column names in lena1:")
print(colnames(lena1))


library(dplyr)

# Select the desired columns to create summary_lena
lena <- lena1[, c("Recording.ID", "ChildKey", "ExternalReferenceID", "Recording_Gender", "Recording_DOB", "RecordingDate",
                         "StartTime", "EndTime","ParticipantID", "CT_COUNT", "AWC_COUNT", "CV_COUNT", "Overlap")]



# 1. Check current date formats 
str(lena$Recording_DOB)
str(lena$RecordingDate)

# Both are mm/dd/yyyy strings, convert like this:
lena$Recording_DOB <- as.Date(lena$Recording_DOB, format = "%m/%d/%Y")
lena$RecordingDate <- as.Date(lena$RecordingDate, format = "%m/%d/%Y")

# Filter out a specific recording
lena <- lena %>%
  filter(!(ExternalReferenceID == '6937B' & RecordingDate == '2023-10-03'))

# Remove rows where ExternalReferenceID is NA or an empty string
lena <- lena %>% filter(!is.na(ExternalReferenceID) & ExternalReferenceID != "")


##### upload correct DOB's to make sure all is well
correct <- read.csv(file.path('source','LENA', 'updated_DOB.csv'),
                    header=TRUE,
                    stringsAsFactors=FALSE)
correct$DoB <- as.Date(correct$DoB, format = "%m/%d/%y")



### checking that Dobs are matching #####------
library(dplyr)

# Unique DoB per participant in correct
correct_unique <- correct %>%
  select(ID_lab, DoB) %>%
  distinct()

# Unique DoB per participant in lena
lena_unique <- lena %>%
  select(ExternalReferenceID, Recording_DOB) %>%
  distinct()

# Join by ID_lab and ExternalReferenceID
dob_compare <- merge(lena_unique,
                     correct_unique,
                     by.x = "ExternalReferenceID",
                     by.y = "ID_lab",
                     all.x = TRUE)

# Create a column to check if DoBs match
dob_compare$DoB_match <- dob_compare$Recording_DOB == dob_compare$DoB

#how many 
table(dob_compare$DoB_match, useNA = "ifany")

#which ones 
mismatches <- dob_compare[dob_compare$DoB_match == FALSE | is.na(dob_compare$DoB_match), ]
mismatches

####------------------
# FIXING DOB's and removing non-existing participants

library(dplyr)

# Step 1: Format DoB in correct
correct <- correct %>%
  mutate(DoB = as.Date(DoB, format = "%m/%d/%y"))

# Step 2: Filter lena to keep only matching IDs, then update DOB
lena <- lena %>%
  filter(ExternalReferenceID %in% correct$ID_lab) %>%           # Keep only matching participants
  left_join(correct %>% select(ID_lab, DoB),                    # Bring in updated DoB
            by = c("ExternalReferenceID" = "ID_lab")) %>%
  mutate(Recording_DOB = DoB) %>%                               # Overwrite Recording_DOB
  select(-DoB)                                                  # Drop temp column

#### ---------------------------------- ################

#### Compute age in months
lena$age_months <- as.numeric(
  difftime(lena$RecordingDate, lena$Recording_DOB, units = "days")
) / 30.44


###### Create Token 
lena <- lena %>%
  mutate(token = case_when(
    age_months < 12              ~ 1,
    age_months >= 12 & age_months < 15 ~ 2,
    age_months >= 15             ~ 3,
    TRUE                         ~ NA_real_
  ))

# how many participants per token?
lena %>%
  distinct(ExternalReferenceID, token) %>%
  count(token)


#save as a summary csv 
write.csv(lena, 
          file.path('raw-csv1', 'Summary', 'lena_Summary.csv'),
          row.names=FALSE)






