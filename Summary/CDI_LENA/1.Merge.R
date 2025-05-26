
# Clear previous data
rm(list = ls())
library(dplyr)
library(lubridate)

# Load data
lena <- read.csv(file.path('raw-csv1','Summary','1.Lena_Summary_Clean.csv'), header=TRUE, stringsAsFactors=FALSE)
CDI <- read.csv(file.path('raw-csv1','Summary','1.CDI_Summary_Clean.csv'), header=TRUE, stringsAsFactors=FALSE)

# Checking what the headers are to prepare the merge 

# Print headers (column names) of lena_summary
colnames(lena)

# Print headers (column names) of CDI_scores
colnames(CDI)


# fixing some lena headers.... to make the merge easier 
# Rename columns
lena <- lena %>%
  rename(
    age_lena = age_months,
    testing_lena = RecordingDate
  )

# Remove unnecessary columns
lena <- lena %>%
  select(-gender, -DOB)

#### adding bilingual index
# e.g closer to 0 is monolingual (100/0) and closer to 1 is fully bilingual (50/50)

CDI <- CDI %>%
  mutate(
    L2_p = ifelse(is.na(L2_p) & L1_p == 100, 0, L2_p),  # treat 100% L1 as L2 = 0
    bilingual_index = ifelse(!is.na(L1_p) & !is.na(L2_p) & L1_p != 0, L2_p / L1_p, NA)
  )


######### MERGE -----

# remove a line
CDI <- CDI %>%
  filter(!(ID_lab == "6903B" & token_cdi == 3))

library(dplyr)

# 1. Merge token_lena == 1 with ALL CDI data
lena_t1 <- lena %>% filter(token_lena == 1)
merged_T1 <- merge(CDI, lena_t1,
                   by.x = "ID_lab", by.y = "ExternalReferenceID",
                   all.x = TRUE)

# 2. Merge token_lena == 2 with ALL CDI data
lena_t2 <- lena %>% filter(token_lena == 2)
merged_T2 <- merge(CDI, lena_t2,
                   by.x = "ID_lab", by.y = "ExternalReferenceID",
                   all.x = TRUE)

# 3. Merge token_lena == 3 with CDI tokens 2 and 3 only
lena_t3 <- lena %>% filter(token_lena == 3)
CDI_23 <- CDI %>% filter(token_cdi %in% c(2, 3))
merged_T3 <- merge(CDI_23, lena_t3,
                   by.x = "ID_lab", by.y = "ExternalReferenceID",
                   all.x = TRUE)

# Save all three
write.csv(merged_T1, file.path("raw-csv1", "Summary", "merged_T1.csv"), row.names = FALSE)
write.csv(merged_T2, file.path("raw-csv1", "Summary", "merged_T2.csv"), row.names = FALSE)
write.csv(merged_T3, file.path("raw-csv1", "Summary", "merged_T3.csv"), row.names = FALSE)


############## this is merging everything together 

# duplicating so that it keeps the column
lena$token_lena_copy <- lena$token_lena

### Merging
merged_df <- merge(
  CDI,
  lena,
  by.x = c("ID_lab", "token_cdi"),
  by.y = c("ExternalReferenceID", "token_lena_copy"),
  all = FALSE
)

## Print headers (column names) 
colnames(merged_df)

# Reorder columns
merged_df <- merged_df %>% 
  select(
    ID_lab, Sex, DoB, Mono_Bil, L1, L2, L1_p, L2_p, bilingual_index, token_cdi, age_CDI, 
    L1_comp, L2_comp, L1_prod, L2_prod, Concept_comp, Total_comp, Concept_prod, Total_prod, 
    token_lena, age_lena, CT_COUNT_Avg, AWC_COUNT_Avg, CV_COUNT_Avg, Overlap_Avg
  )


# remove a line
merged_df <- merged_df %>%
  filter(!(ID_lab == "6903B" & token_cdi == 3))

############ save 
# Save CDI_scores as CSV
write.csv(
  merged_df,
  file.path('raw-csv1', 'Summary', 'final.csv'),
  row.names = FALSE
)

exists("CDI")
exists("lena")
names(CDI)
names(lena)


