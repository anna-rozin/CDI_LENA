# Clear previous data
rm(list = ls())


# Load data
CDI_ALL_LENA <- read.csv(file.path('raw-csv1', 'Summary', '8.CDI_ALL_LENA_T1.csv'), header=TRUE, stringsAsFactors=FALSE)

# Load data
CDI_T1_LENA <- read.csv(file.path('raw-csv1', 'Summary', '8.CDI_T1_LENA_T1.csv'), header=TRUE, stringsAsFactors=FALSE)

# Load data
CDI_T2_LENA <- read.csv(file.path('raw-csv1', 'Summary', '8.CDI_T2_LENA_T1.csv'), header=TRUE, stringsAsFactors=FALSE)

# Load data
CDI_T3_LENA <- read.csv(file.path('raw-csv1', 'Summary', '8.CDI_T3_LENA_T1.csv'), header=TRUE, stringsAsFactors=FALSE)

#Load data
updated_pp<- read.csv(file.path('source', 'LENA', 'updated_DOB.csv'), header=TRUE, stringsAsFactors=FALSE)

# check headers
names(CDI_ALL_LENA)
names(updated_pp)

#####

# creating a new data frame which add bilingual status 

# Select only the required columns from CDI_ALL_LENA
counts <- CDI_ALL_LENA %>%
  select(
    ID_lab, gender, DOB, testing_CDI, age_CDI, token, completed, 
    testing_lena, age_lena, CT_COUNT_Avg, AWC_COUNT_Avg, CV_COUNT_Avg, Overlap_Avg
  ) %>%
  # Left join with updated_pp to add the Mono_Bil, L1, L2, L1_p, L2_p columns
  left_join(updated_pp %>% select(ID_lab, Mono_Bil, L1, L2, L1_p, L2_p), by = "ID_lab")

# Check the resulting dataframe
head(counts)

# removing the duplicate dates (per ID) as the CDI counts will be measured another way later #
counts_unique <- counts %>%
  group_by(ID_lab) %>%
  distinct(testing_CDI, .keep_all = TRUE) %>%
  ungroup()

# checking participant count to make sure it looks good #
token_counts_unique <- counts_unique %>%
  count(token)

print(token_counts_unique)

### fixing language status manually for participants who have missing data 

counts_unique <- counts_unique %>%
  mutate(
    L1 = case_when(
      ID_lab == "7540" ~ "Eus",
      ID_lab == "7517" ~ "Cas",  # Change L1 for 7517
      TRUE ~ L1
    ),
    L2 = case_when(
      ID_lab == "7517" ~ "Eus",  # Change L2 for 7517
      TRUE ~ L2
    ),
    Mono_Bil = case_when(
      ID_lab == "6925" ~ "Bil",
      ID_lab == "7006B" ~ "Mono",
      ID_lab == "7517" ~ "Bil",  # Set Mono_Bil to "Bil" for 7517
      TRUE ~ Mono_Bil
    )
  )

# ID_lab 7449 is still missing data (use from second time point?)

##### Now adding scored l1 l2 concept total of comp and prod for t1, t2, and t3 of cdi to the counts_unique

library(dplyr)

# Make sure NAs are treated as 0
CDI_ALL_LENA <- CDI_ALL_LENA %>%
  mutate(across(c(skill_es_comprehend, skill_es_produce, 
                  skill_eu_comprehend, skill_eu_produce), 
                ~ ifelse(is.na(.), 0, .)))

# Create the scores with updated column names
CDI_scores <- CDI_ALL_LENA %>%
  group_by(ID_lab, token) %>%
  summarise(
    # Look up L1 from counts_unique
    L1 = first(counts_unique$L1[counts_unique$ID_lab == first(ID_lab)]),
    
    # L1 scores depending on L1 value
    L1_comp = if_else(
      L1 == "Cas",
      sum(skill_es_comprehend, na.rm = TRUE),
      if_else(L1 == "Eus", sum(skill_eu_comprehend, na.rm = TRUE), NA_real_)
    ),
    L1_prod = if_else(
      L1 == "Cas",
      sum(skill_es_produce, na.rm = TRUE),
      if_else(L1 == "Eus", sum(skill_eu_produce, na.rm = TRUE), NA_real_)
    ),
    
    # L2 scores (always the other language)
    L2_comp = if_else(
      L1 == "Cas",
      sum(skill_eu_comprehend, na.rm = TRUE),
      if_else(L1 == "Eus", sum(skill_es_comprehend, na.rm = TRUE), NA_real_)
    ),
    L2_prod = if_else(
      L1 == "Cas",
      sum(skill_eu_produce, na.rm = TRUE),
      if_else(L1 == "Eus", sum(skill_es_produce, na.rm = TRUE), NA_real_)
    ),
    
    # Total scores
    Total_comp = if_else(
      L1 %in% c("Cas", "Eus"),
      sum(skill_es_comprehend + skill_eu_comprehend, na.rm = TRUE),
      NA_real_
    ),
    Total_prod = if_else(
      L1 %in% c("Cas", "Eus"),
      sum(skill_es_produce + skill_eu_produce, na.rm = TRUE),
      NA_real_
    ),
    
    # Concept scores (subtract double-counted)
    Concept_comp = if_else(
      L1 %in% c("Cas", "Eus"),
      sum(skill_es_comprehend + skill_eu_comprehend, na.rm = TRUE) -
        sum((skill_es_comprehend == 1) & (skill_eu_comprehend == 1)),
      NA_real_
    ),
    Concept_prod = if_else(
      L1 %in% c("Cas", "Eus"),
      sum(skill_es_produce + skill_eu_produce, na.rm = TRUE) -
        sum((skill_es_produce == 1) & (skill_eu_produce == 1)),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  arrange(ID_lab, token)  # Sort nicely by ID and timepoint

########### merge ----------

# Merge CDI_scores and counts_unique based on ID_lab, token, and L1
final_df <- CDI_scores %>%
  left_join(counts_unique, by = c("ID_lab", "token", "L1"))

# Check the result
head(final_df)

#### reorder columns 
names(final_df)

# Rename 'gender' to 'sex'
final_df <- final_df %>% 
  rename(sex = gender)

# Reorder the columns
final_df <- final_df %>% 
  select(
    ID_lab, sex, DOB, Mono_Bil, L1, L2, L1_p, L2_p, 
    token, testing_CDI, age_CDI, completed, 
    L1_comp, L2_comp, L1_prod, L2_prod, 
    Total_comp,Total_prod, Concept_comp, Concept_prod, 
    testing_lena, age_lena, 
    CT_COUNT_Avg, AWC_COUNT_Avg, CV_COUNT_Avg, Overlap_Avg
  )

##### Removing the concept and total scores from participants who are monolingual

# Step 1: Duplicate final_df to preserve the original
final_df_modified <- final_df %>% 
  mutate(
    Total_comp = if_else(Mono_Bil == "Mono", NA_real_, Total_comp),
    Concept_comp = if_else(Mono_Bil == "Mono", NA_real_, Concept_comp),
    Total_prod = if_else(Mono_Bil == "Mono", NA_real_, Total_prod),
    Concept_prod = if_else(Mono_Bil == "Mono", NA_real_, Concept_prod)
  )

# Now, final_df remains unchanged, and final_df_modified has the desired changes.


#### Save as CSV

write.csv(
  final_df_modified,
  file.path('raw-csv1', 'Summary', '8.CDI_counts_LENA.csv'),
  row.names = FALSE
)





