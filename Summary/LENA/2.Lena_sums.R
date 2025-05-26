library(dplyr)

# Clear previous data
rm(list = ls())

# Load data 
lena <- read.csv(file.path('raw-csv1', 'Summary', 'lena_Summary.csv'),
                 header=TRUE,
                 stringsAsFactors=FALSE)

# This is to create Sums of LENA outputs for all tokens

# Create the "threshold" column based on the "AWC_COUNT" column
lena <- lena %>%
  mutate(threshold = case_when(
    is.na(AWC_COUNT)     ~ NA_real_,
    AWC_COUNT <= 10      ~ 0,
    AWC_COUNT > 10       ~ 1
  ))

# Convert RecordingDate to Date format just to make sure
lena <- lena %>%
  mutate(RecordingDate = as.Date(RecordingDate))

# Summary per participant and token
summary_df <- lena %>%
  group_by(ExternalReferenceID, token) %>%
  summarize(
    Threshold_Sum = sum(threshold == 1, na.rm = TRUE),
    CT_COUNT_Sum = sum(CT_COUNT[threshold == 1], na.rm = TRUE),
    AWC_COUNT_Sum = sum(AWC_COUNT[threshold == 1], na.rm = TRUE),
    CV_COUNT_Sum = sum(CV_COUNT[threshold == 1], na.rm = TRUE),
    Overlap_Sum = sum(Overlap[threshold == 1], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    CT_COUNT_Avg = CT_COUNT_Sum / Threshold_Sum,
    AWC_COUNT_Avg = AWC_COUNT_Sum / Threshold_Sum,
    CV_COUNT_Avg = CV_COUNT_Sum / Threshold_Sum,
    Overlap_Avg = Overlap_Sum / Threshold_Sum
  )

# Join summaries back and select relevant columns
final_df <- lena %>%
  left_join(summary_df, by = c("ExternalReferenceID", "token")) %>%
  select(
    ExternalReferenceID, token, age_months, Recording_Gender, Recording_DOB, RecordingDate,
    CT_COUNT, AWC_COUNT, CV_COUNT, Overlap,
    CT_COUNT_Avg, AWC_COUNT_Avg, CV_COUNT_Avg, Overlap_Avg
  )

#save as a summary csv 
write.csv(final_df, 
          file.path('raw-csv1', 'Summary', '1.Lena_Summary.csv'),
          row.names=FALSE)




##### Additional calculation to take a look at 

filtered_lena <- lena %>% 
  filter(threshold == 1)

hours_per_day_per_participant_token <- filtered_lena %>%
  group_by(ExternalReferenceID, token, RecordingDate) %>%
  summarize(Hours_Per_Day = n(), .groups = 'drop')

avg_hours_per_participant_token <- hours_per_day_per_participant_token %>%
  group_by(ExternalReferenceID, token) %>%
  summarize(Avg_Hours_Per_Day = mean(Hours_Per_Day, na.rm = TRUE), .groups = 'drop')

overall_stats_token <- avg_hours_per_participant_token %>%
  group_by(token) %>%
  summarize(
    Mean_Hours = mean(Avg_Hours_Per_Day, na.rm = TRUE),
    SD_Hours = sd(Avg_Hours_Per_Day, na.rm = TRUE),
    Min_Hours = min(Avg_Hours_Per_Day, na.rm = TRUE),
    Max_Hours = max(Avg_Hours_Per_Day, na.rm = TRUE),
    .groups = 'drop'
  )

####### 

total_hours_per_participant_token <- filtered_lena %>%
  group_by(ExternalReferenceID, token) %>%
  summarize(Total_Hours = n(), .groups = 'drop')

mean_sd_total_hours_token <- total_hours_per_participant_token %>%
  group_by(token) %>%
  summarize(
    Mean_Total_Hours = mean(Total_Hours, na.rm = TRUE),
    SD_Total_Hours = sd(Total_Hours, na.rm = TRUE),
    .groups = 'drop'
  )


#####

summary_stats_token <- final_df %>%
  group_by(token) %>%
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
