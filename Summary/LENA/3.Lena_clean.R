
# Clear previous data
rm(list = ls())
library(dplyr)
library(lubridate)

# Load data
lena_summary <- read.csv(file.path('raw-csv1','Summary','1.Lena_Summary.csv'), header=TRUE, stringsAsFactors=FALSE)

# Renaming columns to avoid confusion when merging

# Rename LENA token column
names(lena_summary)[names(lena_summary) == "token"] <- "token_lena"
names(lena_summary)[names(lena_summary) == "Recording_Gender"] <- "gender"
names(lena_summary)[names(lena_summary) == "Recording_DOB"] <- "DOB"
names(lena_summary)[names(lena_summary) == "RecordingDateOnly"] <- "testing_lena"

##### Removing all extra line (only leaving avgs to reduce amount of data)
lena_summary <- lena_summary %>%
  select(-CT_COUNT, -AWC_COUNT, -CV_COUNT, -Overlap) %>%
  distinct(ExternalReferenceID, token_lena, .keep_all = TRUE)

# Save as CSV
write.csv(
  lena_summary,
  file.path('raw-csv1', 'Summary', '1.Lena_Summary_Clean.csv'),
  row.names = FALSE
)
