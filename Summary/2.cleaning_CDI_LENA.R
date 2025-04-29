# Clear previous data
rm(list = ls())

# Load data
merged_CDI_LENA <- read.csv(file.path('raw-csv1', 'Summary', '7.merged_CDIs_summary.csv'), header=TRUE, stringsAsFactors=FALSE)
updated_DOB<- read.csv(file.path('source', 'LENA', 'updated_DOB.csv'), header=TRUE, stringsAsFactors=FALSE)

# Renaming some columns
names(merged_CDI_LENA)[names(merged_CDI_LENA) == "Recording_Gender"] <- "gender"
names(merged_CDI_LENA)[names(merged_CDI_LENA) == "Recording_DOB"] <- "DOB"
names(merged_CDI_LENA)[names(merged_CDI_LENA) == "RecordingDate"] <- "testing_lena"
names(merged_CDI_LENA)[names(merged_CDI_LENA) == "date"] <- "testing_CDI"
names(merged_CDI_LENA)[names(merged_CDI_LENA) == "lang"] <- "lang_response"


#### fixing DOB in the new sheet 
library(lubridate)
library(dplyr)


updated_DOB <- updated_DOB %>%
  mutate(DOB = mdy(DOB))  # Handles "month/day/year" and 2-digit years correctly

### correcting DOB's in merged_cdi_lena
merged_CDI_LENA <- merged_CDI_LENA %>%
  mutate(DOB = as.Date(DOB)) %>%
  mutate(
    DOB = if_else(
      ID_lab %in% updated_DOB$ID_lab,
      updated_DOB$DOB[match(ID_lab, updated_DOB$ID_lab)],
      DOB
    )
  )



####### Adding age of CDI completion 

# Remove the time component from the date strings
merged_CDI_LENA$testing_CDI <- sub(" .*", "", merged_CDI_LENA$testing_CDI)

# Convert the cleaned date strings to Date objects using ymd function
merged_CDI_LENA$testing_CDI <- ymd(merged_CDI_LENA$testing_CDI)

# Calculate age in days
age_days <- as.numeric(difftime(merged_CDI_LENA$testing_CDI, merged_CDI_LENA$DOB, units = "days"))

# Convert age in days to months
merged_CDI_LENA$age_months <- age_days / 30.4375  # Average number of days in a month

# Rename the new age column to 'age_CDI'
colnames(merged_CDI_LENA)[which(names(merged_CDI_LENA) == "age_months")] <- "age_CDI"

#checking what the ages are so that they all look normal
sort(unique(merged_CDI_LENA$age_CDI))


####### Adding age for lena testing

#change name of date of testing lena 
merged_CDI_LENA <- merged_CDI_LENA %>%
  rename(testing_lena = RecordingDateOnly)


# Convert 'testing_lena' to Date object with correct format
merged_CDI_LENA$testing_lena <- as.Date(merged_CDI_LENA$testing_lena, format = "%Y-%m-%d")

# Calculate age in days for LENA
age_days_lena <- as.numeric(difftime(merged_CDI_LENA$testing_lena, merged_CDI_LENA$DOB, units = "days"))

# Convert age in days to months for LENA
merged_CDI_LENA$age_months_lena <- age_days_lena / 30.4375  # Average number of days in a month

# Rename the new age column to 'age_lena'
colnames(merged_CDI_LENA)[which(names(merged_CDI_LENA) == "age_months_lena")] <- "age_lena"

#checking what the ages are so that they all look normal
sort(unique(merged_CDI_LENA$age_lena))

##### fixing the order of everything 

# Print all column names
print(names(merged_CDI_LENA))


# Specify the new column order
new_order <- c("ID_lab", "ID", "gender", "DOB", "testing_CDI", "age_CDI","token", "completed", "language", 
               "lang_response", "skill_es_comprehend", "skill_es_produce", "skill_eu_comprehend", "skill_eu_produce", 
               "testing_lena", "age_lena", "CT_COUNT_Avg", "AWC_COUNT_Avg", "CV_COUNT_Avg", "Overlap_Avg")

# Reorder columns
merged_CDI_LENA <- merged_CDI_LENA[, new_order]

######## Save for all time points of CDI 

# Remove all rows where "completed" equals 0
merged_CDI_LENA <- merged_CDI_LENA[merged_CDI_LENA$completed != 0, , drop = FALSE]

#### ALL TIME POINTS 1,2,3 -------------------

# Create ALL_CDI: take all time points where completed == 1
ALL_CDI <- merged_CDI_LENA %>%
  filter(completed != 0)


# Remove duplicates, keeping the first entry for each ID_lab
ALL_CDI_unique <- ALL_CDI %>%
  distinct(ID_lab, .keep_all = TRUE)


# Save ALL_CDI as CSV
write.csv(
  ALL_CDI,
  file.path('raw-csv1', 'Summary', '8.CDI_ALL_LENA_T1.csv'),
  row.names = FALSE
)

#### TIME POINT 1 -------------------------

# Create T1_CDI: filter only time point 1 and completed = 1
T1_CDI <- merged_CDI_LENA %>%
  filter(token == 1, completed != 0)

# Remove duplicates, keeping the first entry for each ID_lab
T1_CDI_unique <- T1_CDI %>%
  distinct(ID_lab, .keep_all = TRUE)

# Calculate mean, standard deviation, and number of participants for age_CDI
age_mean <- mean(T1_CDI_unique$age_CDI, na.rm = TRUE)
age_sd <- sd(T1_CDI_unique$age_CDI, na.rm = TRUE)
age_n <- nrow(T1_CDI_unique)

# Print results
cat("Mean Age (CDI):", age_mean, "\n")
cat("Standard Deviation of Age (CDI):", age_sd, "\n")
cat("Number of Participants (CDI):", age_n, "\n")

# Calculate mean, standard deviation, and number of participants for age_lena
age_lena_mean <- mean(T1_CDI_unique$age_lena, na.rm = TRUE)
age_lena_sd <- sd(T1_CDI_unique$age_lena, na.rm = TRUE)
age_lena_n <- nrow(T1_CDI_unique)

# Print results
cat("Mean Age (LENA):", age_lena_mean, "\n")
cat("Standard Deviation of Age (LENA):", age_lena_sd, "\n")
cat("Number of Participants (LENA):", age_lena_n, "\n")

# Save as CSV
write.csv(
  T1_CDI,
  file.path('raw-csv1', 'Summary', '8.CDI_T1_LENA_T1.csv'),
  row.names = FALSE
)


#### TIME POINT 2 -----------------------

# Create T2_CDI: filter only time point 2 and completed = 1
T2_CDI <- merged_CDI_LENA %>%
  filter(token == 2, completed != 0)

# Remove duplicates, keeping the first entry for each ID_lab
T2_CDI_unique <- T2_CDI %>%
  distinct(ID_lab, .keep_all = TRUE)

# Calculate mean, standard deviation, and number of participants for age_CDI (for T2_CDI)
age_mean_T2 <- mean(T2_CDI_unique$age_CDI, na.rm = TRUE)
age_sd_T2 <- sd(T2_CDI_unique$age_CDI, na.rm = TRUE)
age_n_T2 <- nrow(T2_CDI_unique)

# Print results for T2_CDI
cat("Mean Age (T2_CDI):", age_mean_T2, "\n")
cat("Standard Deviation of Age (T2_CDI):", age_sd_T2, "\n")
cat("Number of Participants (T2_CDI):", age_n_T2, "\n")

# **T1_LENA stats calculation (unchanged from previous)**

# Using T1_CDI_unique for age calculations from T1_LENA dataset
age_lena_mean_T1 <- mean(T1_CDI_unique$age_lena, na.rm = TRUE)
age_lena_sd_T1 <- sd(T1_CDI_unique$age_lena, na.rm = TRUE)
age_lena_n_T1 <- nrow(T1_CDI_unique)

# Print results for T1_LENA
cat("Mean Age (LENA):", age_lena_mean_T1, "\n")
cat("Standard Deviation of Age (LENA):", age_lena_sd_T1, "\n")
cat("Number of Participants (LENA):", age_lena_n_T1, "\n")

# Save T2_CDI as CSV
write.csv(
  T2_CDI,
  file.path('raw-csv1', 'Summary', '8.CDI_T2_LENA_T1.csv'),
  row.names = FALSE
)

#### TIME POINT 3 -------------------------

# Create T3_CDI: filter only time point 3 and completed = 1
T3_CDI <- merged_CDI_LENA %>%
  filter(token == 3, completed != 0)

# Remove duplicates, keeping the first entry for each ID_lab
T3_CDI_unique <- T3_CDI %>%
  distinct(ID_lab, .keep_all = TRUE)

# Calculate mean, standard deviation, and number of participants for age_CDI (for T3_CDI)
age_mean_T3 <- mean(T3_CDI_unique$age_CDI, na.rm = TRUE)
age_sd_T3 <- sd(T3_CDI_unique$age_CDI, na.rm = TRUE)
age_n_T3 <- nrow(T3_CDI_unique)

# Print results for T3_CDI
cat("Mean Age (T3_CDI):", age_mean_T3, "\n")
cat("Standard Deviation of Age (T3_CDI):", age_sd_T3, "\n")
cat("Number of Participants (T3_CDI):", age_n_T3, "\n")

# **T1_LENA stats calculation (unchanged from previous)**

# Using T1_CDI_unique for age calculations from T1_LENA dataset
age_lena_mean_T1 <- mean(T1_CDI_unique$age_lena, na.rm = TRUE)
age_lena_sd_T1 <- sd(T1_CDI_unique$age_lena, na.rm = TRUE)
age_lena_n_T1 <- nrow(T1_CDI_unique)

# Print results for T1_LENA
cat("Mean Age (LENA):", age_lena_mean_T1, "\n")
cat("Standard Deviation of Age (LENA):", age_lena_sd_T1, "\n")
cat("Number of Participants (LENA):", age_lena_n_T1, "\n")

# Save T3_CDI as CSV
write.csv(
  T3_CDI,
  file.path('raw-csv1', 'Summary', '8.CDI_T3_LENA_T1.csv'),
  row.names = FALSE
)
