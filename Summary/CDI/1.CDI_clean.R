

# Clear previous data
rm(list = ls())
library(dplyr)
library(lubridate)
library(stringr)



# Load
CDI_summary <- read.csv(file.path('raw-csv1','Summary','5.merged_CDI_final.csv'), header=TRUE, stringsAsFactors=FALSE)
updated_DOB<- read.csv(file.path('source', 'LENA', 'updated_DOB.csv'), header=TRUE, stringsAsFactors=FALSE)


# Rename CDI token column
names(CDI_summary)[names(CDI_summary) == "token"] <- "token_cdi"
names(CDI_summary)[names(CDI_summary) == "date"] <- "testing_CDI"
names(CDI_summary)[names(CDI_summary) == "lang"] <- "lang_response"

# Calculating Age during CDI ----

# Step 1: Merge DoB into CDI_summary
CDI_summary <- CDI_summary %>%
  left_join(updated_DOB, by = "ID_lab")

# Step 2: Convert DoB and testing_CDI to Date format
CDI_summary <- CDI_summary %>%
  mutate(
    DoB = mdy(DoB),
    testing_CDI = ymd(str_sub(testing_CDI, 1, 10))  # Remove time component if present
  )

# Step 3: Calculate age at CDI testing in months
CDI_summary <- CDI_summary %>%
  mutate(age_CDI = as.numeric(difftime(testing_CDI, DoB, units = "days")) / 30.4375)


#### ------ Counting Sums
library(dplyr)

# Replace NAs with 0 for comprehension and production skill columns
CDI_summary <- CDI_summary %>%
  mutate(across(c(skill_es_comprehend, skill_es_produce, skill_eu_comprehend, skill_eu_produce), 
                ~ ifelse(is.na(.), 0, .)))

# Summarize scores by participant and token_cdi

CDI_scores <- CDI_summary %>%
  group_by(ID_lab, token_cdi) %>%
  summarise(
    L1 = first(L1),  # L1 language from your data
    
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
  arrange(ID_lab, token_cdi)


# Removing NA participants
CDI_scores <- CDI_scores %>% 
  filter(
    !(is.na(L1_comp) | L1_comp == "") &
      !(is.na(L1_prod) | L1_prod == "") &
      !(is.na(L2_comp) | L2_comp == "") &
      !(is.na(L2_prod) | L2_prod == "")
  )


##### add remaing necessary columns
CDI_scores <- CDI_scores %>%
  left_join(
    CDI_summary %>%
      select(ID_lab, token_cdi, DoB, Sex, Mono_Bil, L2, L1_p, L2_p, age_CDI, testing_CDI) %>%
      distinct(ID_lab, token_cdi, .keep_all = TRUE),  # Keep one unique row per ID and token
    by = c("ID_lab", "token_cdi")
  )



# re order the columns
names(CDI_scores)

CDI_scores <- CDI_scores %>%
  select(
    ID_lab, Sex, DoB, Mono_Bil, L1, L2, L1_p, L2_p, age_CDI, testing_CDI, token_cdi,
    L1_comp, L2_comp, L1_prod, L2_prod,
    Concept_comp, Total_comp, Concept_prod, Total_prod
  )


# Save CDI_scores as CSV
write.csv(
  CDI_scores,
  file.path('raw-csv1', 'Summary', '1.CDI_Summary_Clean.csv'),
  row.names = FALSE
)
