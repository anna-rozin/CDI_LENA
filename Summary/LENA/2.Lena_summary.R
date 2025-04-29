library(dplyr)

# Clear previous data
rm(list = ls())

# Load data 
lena <- read.csv(file.path('raw-csv1', 'Summary', 'lena_Summary.csv'),
                 header=TRUE,
                 stringsAsFactors=FALSE)

# Convert RecordingDate to date format
lena$RecordingDate <- as.Date(lena$RecordingDate, format = "%m/%d/%Y")

# Filter out a specific recording
lena <- lena %>%
  filter(!(ExternalReferenceID == '6937B' & RecordingDate == '2023-10-03'))

# Remove rows where ExternalReferenceID is NA or an empty string
lena <- lena %>% filter(!is.na(ExternalReferenceID) & ExternalReferenceID != "")

# Selecting the columns
selected_data <- dplyr::select(lena, ExternalReferenceID, RecordingDate)
library(dplyr)

# Step 1: Find the first two distinct dates for each participant
first_two_dates <- lena %>%
  group_by(ExternalReferenceID) %>%
  arrange(RecordingDate) %>%
  distinct(RecordingDate, .keep_all = TRUE) %>%
  slice_head(n = 2) %>%
  dplyr::select(ExternalReferenceID, RecordingDate) %>%
  ungroup()  # ungroup to avoid issues later


# Step 2: Filter the original data to keep all rows from the first two dates
lena_first_two_dates <- lena %>%
  semi_join(first_two_dates, by = c("ExternalReferenceID", "RecordingDate")) %>%
  dplyr::select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate, 
         StartTime, EndTime, CT_COUNT, AWC_COUNT, CV_COUNT, Overlap)

# Step 3: Create the "threshold" column based on the "AWC_COUNT" column
lena_first_two_dates <- lena_first_two_dates %>%
  mutate(threshold = ifelse(AWC_COUNT <= 10, 0, ifelse(AWC_COUNT >= 11, 1, NA)))

# Step 4: Summarize the data per participant
summary_df <- lena_first_two_dates %>%
  group_by(ExternalReferenceID) %>%
  summarize(
    Threshold_Sum = sum(threshold, na.rm = TRUE),
    CT_COUNT_Sum = sum(CT_COUNT[threshold == 1], na.rm = TRUE),
    AWC_COUNT_Sum = sum(AWC_COUNT[threshold == 1], na.rm = TRUE),
    CV_COUNT_Sum = sum(CV_COUNT[threshold == 1], na.rm = TRUE),
    Overlap_Sum = sum(Overlap[threshold == 1], na.rm = TRUE)
  ) %>%
  mutate(
    CT_COUNT_Avg = CT_COUNT_Sum / Threshold_Sum,
    AWC_COUNT_Avg = AWC_COUNT_Sum / Threshold_Sum,
    CV_COUNT_Avg = CV_COUNT_Sum / Threshold_Sum,
    Overlap_Avg = Overlap_Sum / Threshold_Sum
  )

# Step 5: Get the earliest recording date per participant
lena_first_two_dates <- lena_first_two_dates %>%
  mutate(RecordingDateOnly = as.Date(RecordingDate)) %>%  # Extract the date part
  group_by(ExternalReferenceID) %>%
  mutate(is_earliest_date = RecordingDateOnly == min(RecordingDateOnly)) %>%
  ungroup()

# Step 6: Merge the summary with the demographic and recording information
final_df <- lena_first_two_dates %>%
  left_join(summary_df, by = "ExternalReferenceID") %>%
  dplyr::select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDateOnly, 
                CT_COUNT, AWC_COUNT, CV_COUNT, Overlap, is_earliest_date, 
                CT_COUNT_Avg, AWC_COUNT_Avg, CV_COUNT_Avg, Overlap_Avg)



### attempt

# Step 1: Filter the data to include only the rows where threshold == 1
filtered_lena <- lena_first_two_dates %>%
  filter(threshold == 1)

# Step 2: Calculate the number of valid hours per day for each participant
hours_per_day_per_participant <- filtered_lena %>%
  group_by(ExternalReferenceID, RecordingDate) %>%
  summarize(Hours_Per_Day = n(), .groups = 'drop')

# Step 3: Calculate the average number of hours per day for each participant
avg_hours_per_participant <- hours_per_day_per_participant %>%
  group_by(ExternalReferenceID) %>%
  summarize(Avg_Hours_Per_Day = mean(Hours_Per_Day, na.rm = TRUE), .groups = 'drop')

# Step 4: Calculate the overall average and standard deviation across all participants
overall_avg_hours <- mean(avg_hours_per_participant$Avg_Hours_Per_Day, na.rm = TRUE)
overall_sd_hours <- sd(avg_hours_per_participant$Avg_Hours_Per_Day, na.rm = TRUE)

# Step 5: Print the results
cat("Overall Mean Hours per Day Across All Participants:", overall_avg_hours, "\n")
cat("Overall Standard Deviation of Hours per Day Across All Participants:", overall_sd_hours, "\n")


# Step 1: Filter the data to include only the rows where threshold == 1
filtered_lena <- lena_first_two_dates %>%
  filter(threshold == 1)

# Step 2: Calculate the total hours per participant
# Since each row represents one hour, the total hours are just the count of these rows
total_hours_per_participant <- filtered_lena %>%
  group_by(ExternalReferenceID) %>%
  summarize(Total_Hours = n())

# Step 3: Calculate the mean and standard deviation of total hours across all participants
mean_hours <- mean(total_hours_per_participant$Total_Hours, na.rm = TRUE)
sd_hours <- sd(total_hours_per_participant$Total_Hours, na.rm = TRUE)

# Step 4: Print the results
cat("Mean Total Hours:", mean_hours, "\n")
cat("Standard Deviation of Total Hours:", sd_hours, "\n")

###
# Step 1: Calculate the number of valid hours per day for each participant
hours_per_day_per_participant <- filtered_lena %>%
  group_by(ExternalReferenceID, RecordingDate) %>%
  summarize(Hours_Per_Day = n(), .groups = 'drop')

# Step 2: Calculate the average number of hours per day across both days for each participant
avg_hours_per_participant <- hours_per_day_per_participant %>%
  group_by(ExternalReferenceID) %>%
  summarize(Avg_Hours_Per_Day = mean(Hours_Per_Day, na.rm = TRUE), .groups = 'drop')

# Step 3: Calculate the overall mean, standard deviation, minimum, and maximum across all participants
overall_stats <- avg_hours_per_participant %>%
  summarize(
    Mean_Hours = mean(Avg_Hours_Per_Day, na.rm = TRUE),
    SD_Hours = sd(Avg_Hours_Per_Day, na.rm = TRUE),
    Min_Hours = min(Avg_Hours_Per_Day, na.rm = TRUE),
    Max_Hours = max(Avg_Hours_Per_Day, na.rm = TRUE),
    .groups = 'drop'
  )

# Step 4: Print the results
cat("Overall Mean Hours per Day Across All Participants:", overall_stats$Mean_Hours, "\n")
cat("Overall Standard Deviation of Hours per Day Across All Participants:", overall_stats$SD_Hours, "\n")
cat("Overall Minimum Hours per Day Across All Participants:", overall_stats$Min_Hours, "\n")
cat("Overall Maximum Hours per Day Across All Participants:", overall_stats$Max_Hours, "\n")

#### attempt 
# Calculate mean, range, and standard deviation for AWC_COUNT_Avg, CT_COUNT_Avg, and CV_COUNT_Avg
# Calculate mean, range, and standard deviation for AWC_COUNT_Avg, CT_COUNT_Avg, and CV_COUNT_Avg
# Calculate mean, median, range, and standard deviation for AWC_COUNT_Avg, CT_COUNT_Avg, and CV_COUNT_Avg
# Calculate mean, median, range, standard deviation, and count of non-missing values
summary_stats <- final_df %>%
  reframe(
    AWC_N = sum(!is.na(AWC_COUNT_Avg)),
    AWC_mean = mean(AWC_COUNT_Avg, na.rm = TRUE),
    AWC_median = median(AWC_COUNT_Avg, na.rm = TRUE),
    AWC_range_min = min(AWC_COUNT_Avg, na.rm = TRUE),
    AWC_range_max = max(AWC_COUNT_Avg, na.rm = TRUE),
    AWC_sd = sd(AWC_COUNT_Avg, na.rm = TRUE),
    
    CT_N = sum(!is.na(CT_COUNT_Avg)),
    CT_mean = mean(CT_COUNT_Avg, na.rm = TRUE),
    CT_median = median(CT_COUNT_Avg, na.rm = TRUE),
    CT_range_min = min(CT_COUNT_Avg, na.rm = TRUE),
    CT_range_max = max(CT_COUNT_Avg, na.rm = TRUE),
    CT_sd = sd(CT_COUNT_Avg, na.rm = TRUE),
    
    CV_N = sum(!is.na(CV_COUNT_Avg)),
    CV_mean = mean(CV_COUNT_Avg, na.rm = TRUE),
    CV_median = median(CV_COUNT_Avg, na.rm = TRUE),
    CV_range_min = min(CV_COUNT_Avg, na.rm = TRUE),
    CV_range_max = max(CV_COUNT_Avg, na.rm = TRUE),
    CV_sd = sd(CV_COUNT_Avg, na.rm = TRUE)
  )




#save as a summary csv 
write.csv(final_df, 
          file.path('raw-csv1', 'Summary', '1.lena_Summary.csv'),
          row.names=FALSE)







##########################################
# load data 
lena <- read.csv(file.path('raw-csv', 'Summary', 'lena_Summary.csv'),
                 header=TRUE,
                 stringsAsFactors=FALSE)


#################### I am getting rid of all time points except for the 1st one 

# Convert RecordingDate to date format
lena$RecordingDate <- as.Date(lena$RecordingDate, format = "%m/%d/%Y")

# Assuming your dataframe is called lena_df
lena <- lena %>%
  filter(!(ExternalReferenceID == '6937B' & RecordingDate == '2023-10-03'))

library(dplyr)


# Step 1: Find the first two distinct dates for each participant
first_two_dates <- lena %>%
  group_by(ExternalReferenceID) %>%
  arrange(RecordingDate) %>%
  distinct(RecordingDate, .keep_all = TRUE) %>%
  slice_head(n = 2) %>%
  select(ExternalReferenceID, RecordingDate)

# Step 2: Filter the original data to keep all rows from the first two dates
lena_first_two_dates <- lena %>%
  semi_join(first_two_dates, by = c("ExternalReferenceID", "RecordingDate")) %>%
  select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate, 
         StartTime, EndTime, CT_COUNT, AWC_COUNT, CV_COUNT, Overlap)


############ Creating Threshold Column 

# Step 3: Create the "threshold" column based on the "AWC_COUNT" column
lena_first_two_dates <- lena_first_two_dates %>%
  mutate(threshold = ifelse(AWC_COUNT <= 10, 0, ifelse(AWC_COUNT >= 11, 1, NA)))


#### creating a new data frame, to calculate average/ hour. Excluding hours in Threshold with 0

# Step 4: Summarize the data per participant
summary_df <- lena_first_two_dates %>%
  group_by(ExternalReferenceID) %>%
  summarize(
    Threshold_Sum = sum(threshold, na.rm = TRUE),
    CT_COUNT_Sum = sum(CT_COUNT[threshold == 1], na.rm = TRUE),
    AWC_COUNT_Sum = sum(AWC_COUNT[threshold == 1], na.rm = TRUE),
    CV_COUNT_Sum = sum(CV_COUNT[threshold == 1], na.rm = TRUE)
  ) %>%
  mutate(
    CT_COUNT_Avg = CT_COUNT_Sum / Threshold_Sum,
    AWC_COUNT_Avg = AWC_COUNT_Sum / Threshold_Sum,
    CV_COUNT_Avg = CV_COUNT_Sum / Threshold_Sum
  )


##### creating new df with only avgs and needed info 

# Step 5: Get the earliest recording date per participant
earliest_dates <- lena_first_two_dates %>%
  group_by(ExternalReferenceID) %>%
  arrange(RecordingDate) %>%
  slice_head(n = 1) %>%
  select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate) %>%
  ungroup()

# Step 6: Merge the summary with the demographic and recording information
final_df <- earliest_dates %>%
  left_join(summary_df, by = "ExternalReferenceID") %>%
  select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate,
         CT_COUNT_Avg, AWC_COUNT_Avg, CV_COUNT_Avg)



#save as a summary csv 
write.csv(final_df, 
          file.path('raw-csv', 'Summary', '1.lena_Summary.csv'),
          row.names=FALSE)




######### MIN/ MAX
library(dplyr)

# Step 1: Calculate Duration_Hours from StartTime and EndTime
lena_first_two_dates <- lena_first_two_dates %>%
  mutate(
    StartTime = as.POSIXct(StartTime, format = "%m/%d/%y %I:%M:%S %p", tz = "Europe/Madrid"),
    EndTime = as.POSIXct(EndTime, format = "%m/%d/%y %I:%M:%S %p", tz = "Europe/Madrid"),
    Duration_Hours = as.numeric(difftime(EndTime, StartTime, units = "hours"))
  )

# Step 2: Group by ExternalReferenceID and summarize the durations
duration_summary <- lena_first_two_dates %>%
  group_by(ExternalReferenceID) %>%
  summarize(
    Total_Duration = sum(Duration_Hours, na.rm = TRUE),
    Average_Duration = mean(Duration_Hours, na.rm = TRUE),
    Min_Duration = min(Duration_Hours, na.rm = TRUE),
    Max_Duration = max(Duration_Hours, na.rm = TRUE)
  )

# Step 3: Calculate overall statistics
overall_average_duration <- mean(duration_summary$Average_Duration)
overall_min_duration <- min(duration_summary$Min_Duration)
overall_max_duration <- max(duration_summary$Max_Duration)

# Print the results
cat("Overall Average duration:", overall_average_duration, "hours\n")
cat("Overall Minimum duration:", overall_min_duration, "hours\n")
cat("Overall Maximum duration:", overall_max_duration, "hours\n")


#### ANOTHER ATTEMPT 

library(dplyr)

# Load data 
lena <- read.csv(file.path('raw-csv', 'Summary', 'lena_Summary.csv'),
                 header=TRUE,
                 stringsAsFactors=FALSE)

# Convert RecordingDate to date format
lena$RecordingDate <- as.Date(lena$RecordingDate, format = "%m/%d/%Y")

# Filter out a specific recording
lena <- lena %>%
  filter(!(ExternalReferenceID == '6937B' & RecordingDate == '2023-10-03'))

# Step 1: Find the first two distinct dates for each participant
first_two_dates <- lena %>%
  group_by(ExternalReferenceID) %>%
  arrange(RecordingDate) %>%
  distinct(RecordingDate, .keep_all = TRUE) %>%
  slice_head(n = 2) %>%
  select(ExternalReferenceID, RecordingDate)

# Step 2: Filter the original data to keep all rows from the first two dates
lena_first_two_dates <- lena %>%
  semi_join(first_two_dates, by = c("ExternalReferenceID", "RecordingDate")) %>%
  select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate, 
         StartTime, EndTime, CT_COUNT, AWC_COUNT, CV_COUNT, Overlap)

# Step 3: Create the "threshold" column based on the "AWC_COUNT" column
lena_first_two_dates <- lena_first_two_dates %>%
  mutate(threshold = ifelse(AWC_COUNT <= 10, 0, ifelse(AWC_COUNT >= 11, 1, NA)))

# Step 4: Summarize the data per participant
summary_df <- lena_first_two_dates %>%
  filter(threshold == 1) %>%
  group_by(ExternalReferenceID) %>%
  mutate(duration_hours = as.numeric(difftime(EndTime, StartTime, units = "hours"))) %>%
  summarize(
    Threshold_Sum = sum(threshold, na.rm = TRUE),
    
    CT_COUNT_Avg_per_hour = mean(CT_COUNT / duration_hours, na.rm = TRUE),
    CT_COUNT_SD_per_hour = sd(CT_COUNT / duration_hours, na.rm = TRUE),
    N_CT = sum(!is.na(CT_COUNT / duration_hours)),
    
    AWC_COUNT_Avg_per_hour = mean(AWC_COUNT / duration_hours, na.rm = TRUE),
    AWC_COUNT_SD_per_hour = sd(AWC_COUNT / duration_hours, na.rm = TRUE),
    N_AWC = sum(!is.na(AWC_COUNT / duration_hours)),
    
    CV_COUNT_Avg_per_hour = mean(CV_COUNT / duration_hours, na.rm = TRUE),
    CV_COUNT_SD_per_hour = sd(CV_COUNT / duration_hours, na.rm = TRUE),
    N_CV = sum(!is.na(CV_COUNT / duration_hours)),
    
    Overlap_Avg_per_hour = mean(Overlap / duration_hours, na.rm = TRUE),
    Overlap_SD_per_hour = sd(Overlap / duration_hours, na.rm = TRUE),
    N_Overlap = sum(!is.na(Overlap / duration_hours))
  )

# Step 5: Get the earliest recording date per participant
earliest_dates <- lena_first_two_dates %>%
  group_by(ExternalReferenceID) %>%
  arrange(RecordingDate) %>%
  slice_head(n = 1) %>%
  select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate) %>%
  ungroup()

# Step 6: Merge the summary with the demographic and recording information
final_df <- earliest_dates %>%
  left_join(summary_df, by = "ExternalReferenceID") %>%
  select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate,
         CT_COUNT_Avg_per_hour, CT_COUNT_SD_per_hour, N_CT,
         AWC_COUNT_Avg_per_hour, AWC_COUNT_SD_per_hour, N_AWC,
         CV_COUNT_Avg_per_hour, CV_COUNT_SD_per_hour, N_CV,
         Overlap_Avg_per_hour, Overlap_SD_per_hour, N_Overlap)

# Print the final dataframe
print(final_df)

