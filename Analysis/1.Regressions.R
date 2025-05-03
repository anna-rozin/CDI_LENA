# Clear previous data
rm(list = ls())

# Load data
CDI_counts_LENA <- read.csv(file.path('raw-csv1', 'Summary', '8.CDI_counts_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)

#---------------------------------------------------------------#
library(dplyr)
library(tidyr)
library(tidyverse)

# Fixing the % of L1 and L2 so that monolingual 100% in L1 have 0% in L2 instead of N/A
# Ignoring participants who do not have data for L1 and L2 % 
# making a bilingual_index to be 0 to 1 scale
# e.g closer to 0 is monolingual (100/0) and closer to 1 is fully bilingual (50/50)

CDI_counts_LENA <- CDI_counts_LENA %>%
  mutate(
    L2_p = ifelse(is.na(L2_p) & L1_p == 100, 0, L2_p),  # treat 100% L1 as L2 = 0
    bilingual_index = ifelse(!is.na(L1_p) & !is.na(L2_p) & L1_p != 0, L2_p / L1_p, NA)
  )

#now using bilingual_index in regression models as a continuous factor instead of mono_bil 

#----- REGRESSIONS -------# 
# T1 #---------------------

# FILTER DATA FOR TIME POINT 1
cdi_t1 <- CDI_counts_LENA %>% filter(token == 1)
# FILTER FOR BILINGUAL PARTICIPANTS (based on Mono_Bil variable) for L2 measures 
cdi_t1_bil <- cdi_t1 %>% filter(Mono_Bil == "Bil")

### RQ1: Vocabulary & AWC

# L1 Comprehension
model_L1_comp_awc <- lm(L1_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_L1_comp_awc)

# L1 Production
model_L1_prod_awc <- lm(L1_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_L1_prod_awc)

# L2 Comprehension (Bilinguals only)
model_L2_comp_awc <- lm(L2_comp ~ AWC_COUNT_Avg + sex, data = cdi_t1_bil)
summary(model_L2_comp_awc)

# L2 Production (Bilinguals only)
model_L2_prod_awc <- lm(L2_prod ~ AWC_COUNT_Avg + sex, data = cdi_t1_bil)
summary(model_L2_prod_awc)


### RQ2: Vocabulary & CT

# L1 Comprehension
model_L1_comp_ct <- lm(L1_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_L1_comp_ct)

# L1 Production
model_L1_prod_ct <- lm(L1_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_L1_prod_ct)

# L2 Comprehension (Bilinguals only)
model_L2_comp_ct <- lm(L2_comp ~ CT_COUNT_Avg + sex, data = cdi_t1_bil)
summary(model_L2_comp_ct)

# L2 Production (Bilinguals only)
model_L2_prod_ct <- lm(L2_prod ~ CT_COUNT_Avg + sex, data = cdi_t1_bil)
summary(model_L2_prod_ct)


### RQ3: Conceptual & Total Vocabulary — AWC

# Conceptual Comprehension
model_concept_comp_awc <- lm(Concept_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_concept_comp_awc)

# Conceptual Production
model_concept_prod_awc <- lm(Concept_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_concept_prod_awc)

# Total Comprehension
model_total_comp_awc <- lm(Total_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_total_comp_awc)

# Total Production
model_total_prod_awc <- lm(Total_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_total_prod_awc)


### RQ3: Conceptual & Total Vocabulary — CT

# Conceptual Comprehension
model_concept_comp_ct <- lm(Concept_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_concept_comp_ct)

# Conceptual Production
model_concept_prod_ct <- lm(Concept_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_concept_prod_ct)

# Total Comprehension
model_total_comp_ct <- lm(Total_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_total_comp_ct)

# Total Production
model_total_prod_ct <- lm(Total_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t1)
summary(model_total_prod_ct)





#----- REGRESSIONS -------# 
# T2 #---------------------

# FILTER DATA FOR TIME POINT 2
cdi_t2 <- CDI_counts_LENA %>% filter(token == 2)

# FILTER FOR BILINGUAL PARTICIPANTS (based on Mono_Bil variable) for L2 measures 
cdi_t2_bil <- cdi_t2 %>% filter(Mono_Bil == "Bil")

### RQ1: Vocabulary & AWC

# L1 Comprehension ***
model_L1_comp_awc_T2 <- lm(L1_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_L1_comp_awc_T2)

# L1 Production
model_L1_prod_awc_T2 <- lm(L1_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_L1_prod_awc_T2)

# L2 Comprehension (Bilinguals only)
model_L2_comp_awc_T2 <- lm(L2_comp ~ AWC_COUNT_Avg + sex, data = cdi_t2_bil)
summary(model_L2_comp_awc_T2)

# L2 Production (Bilinguals only)
model_L2_prod_awc_T2 <- lm(L2_prod ~ AWC_COUNT_Avg + sex, data = cdi_t2_bil)
summary(model_L2_prod_awc_T2)


### RQ2: Vocabulary & CT

# L1 Comprehension
model_L1_comp_ct_T2 <- lm(L1_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_L1_comp_ct_T2)

# L1 Production
model_L1_prod_ct_T2 <- lm(L1_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_L1_prod_ct_T2)

# L2 Comprehension (Bilinguals only)
model_L2_comp_ct_T2 <- lm(L2_comp ~ CT_COUNT_Avg + sex, data = cdi_t2_bil)
summary(model_L2_comp_ct_T2)

# L2 Production (Bilinguals only)
model_L2_prod_ct_T2 <- lm(L2_prod ~ CT_COUNT_Avg + sex, data = cdi_t2_bil)
summary(model_L2_prod_ct_T2)


### RQ3: Conceptual & Total Vocabulary — AWC

# Conceptual Comprehension *
model_concept_comp_awc_T2 <- lm(Concept_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_concept_comp_awc_T2)

# Conceptual Production
model_concept_prod_awc_T2 <- lm(Concept_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_concept_prod_awc_T2)

# Total Comprehension *
model_total_comp_awc_T2 <- lm(Total_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_total_comp_awc_T2)

# Total Production
model_total_prod_awc_T2 <- lm(Total_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_total_prod_awc_T2)


### RQ3: Conceptual & Total Vocabulary — CT

# Conceptual Comprehension
model_concept_comp_ct_T2 <- lm(Concept_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_concept_comp_ct_T2)

# Conceptual Production
model_concept_prod_ct_T2 <- lm(Concept_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_concept_prod_ct_T2)

# Total Comprehension
model_total_comp_ct_T2 <- lm(Total_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_total_comp_ct_T2)

# Total Production
model_total_prod_ct_T2 <- lm(Total_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t2)
summary(model_total_prod_ct_T2)






#----- REGRESSIONS -------# 
# T3 #---------------------

# FILTER DATA FOR TIME POINT 2
cdi_t3 <- CDI_counts_LENA %>% filter(token == 3)

# FILTER FOR BILINGUAL PARTICIPANTS (based on Mono_Bil variable) for L2 measures 
cdi_t3_bil <- cdi_t3 %>% filter(Mono_Bil == "Bil")


### RQ1: Vocabulary & AWC

# L1 Comprehension
model_L1_comp_awc_T3 <- lm(L1_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_L1_comp_awc_T3)

# L1 Production
model_L1_prod_awc_T3 <- lm(L1_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_L1_prod_awc_T3)

# L2 Comprehension (Bilinguals only)
model_L2_comp_awc_T3 <- lm(L2_comp ~ AWC_COUNT_Avg + sex, data = cdi_t3_bil)
summary(model_L2_comp_awc_T3)

# L2 Production (Bilinguals only)
model_L2_prod_awc_T3 <- lm(L2_prod ~ AWC_COUNT_Avg + sex, data = cdi_t3_bil)
summary(model_L2_prod_awc_T3)


### RQ2: Vocabulary & CT

# L1 Comprehension
model_L1_comp_ct_T3 <- lm(L1_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_L1_comp_ct_T3)

# L1 Production
model_L1_prod_ct_T3 <- lm(L1_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_L1_prod_ct_T3)

# L2 Comprehension (Bilinguals only)
model_L2_comp_ct_T3 <- lm(L2_comp ~ CT_COUNT_Avg + sex, data = cdi_t3_bil)
summary(model_L2_comp_ct_T3)

# L2 Production (Bilinguals only)
model_L2_prod_ct_T3 <- lm(L2_prod ~ CT_COUNT_Avg + sex, data = cdi_t3_bil)
summary(model_L2_prod_ct_T3)


### RQ3: Conceptual & Total Vocabulary — AWC

# Conceptual Comprehension 
model_concept_comp_awc_T3 <- lm(Concept_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_concept_comp_awc_T3)

# Conceptual Production
model_concept_prod_awc_T3 <- lm(Concept_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_concept_prod_awc_T3)

# Total Comprehension
model_total_comp_awc_T3 <- lm(Total_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_total_comp_awc_T3)

# Total Production
model_total_prod_awc_T3 <- lm(Total_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_total_prod_awc_T3)


### RQ3: Conceptual & Total Vocabulary — CT

# Conceptual Comprehension *
model_concept_comp_ct_T3 <- lm(Concept_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_concept_comp_ct_T3)

# Conceptual Production
model_concept_prod_ct_T3 <- lm(Concept_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_concept_prod_ct_T3)

# Total Comprehension
model_total_comp_ct_T3 <- lm(Total_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_total_comp_ct_T3)

# Total Production
model_total_prod_ct_T3 <- lm(Total_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_t3)
summary(model_total_prod_ct_T3)





######### ---------------------------
# Checking how many participants in each Time point # 

# Count participants in T1 with bilingual_index
participants_T1 <- cdi_t1 %>%
  filter(!is.na(bilingual_index)) %>%
  summarise(n_participants = n_distinct(ID_lab))

# Count participants in T2 with bilingual_index
participants_T2 <- cdi_t2 %>%
  filter(!is.na(bilingual_index)) %>%
  summarise(n_participants = n_distinct(ID_lab))

# Count participants in T3 with bilingual_index
participants_T3 <- cdi_t3 %>%
  filter(!is.na(bilingual_index)) %>%
  summarise(n_participants = n_distinct(ID_lab))

# Print the results
participants_T1
participants_T2
participants_T3





####### ------------ GAIN 

# GAIN FOR T2-T1 VOCAB MEASURES # ----------------------------------
# Step 1: Filter for tokens 1 and 2, then reshape the data so T1 and T2 values are in separate columns
cdi_counts_t1_t2 <- CDI_counts_LENA %>%
  filter(token %in% c(1, 2)) %>%  # Keep only T1 and T2 data
  pivot_wider(
    id_cols = ID_lab, 
    names_from = token,  # Separate columns for token 1 and token 2
    values_from = c(L1_comp, L2_comp, L1_prod, L2_prod, Total_comp, Total_prod, Concept_comp, Concept_prod, age_CDI),
    names_glue = "{.value}_T{token}"  # Create names like L1_comp_T1, L1_comp_T2, etc.
  ) %>%
  left_join(
    CDI_counts_LENA %>% 
      select(ID_lab, bilingual_index, Mono_Bil, sex, AWC_COUNT_Avg, CT_COUNT_Avg, Overlap_Avg) %>%
      distinct(), by = "ID_lab"  # Keep bilingual_index and LENA measures
  )

# Step 2: Calculate gain scores by subtracting T1 values from T2 values
cdi_counts_t1_t2 <- cdi_counts_t1_t2 %>%
  mutate(
    gain_L1_comp = L1_comp_T2 - L1_comp_T1,
    gain_L2_comp = L2_comp_T2 - L2_comp_T1,
    gain_L1_prod = L1_prod_T2 - L1_prod_T1,
    gain_L2_prod = L2_prod_T2 - L2_prod_T1,
    gain_total_comp = Total_comp_T2 - Total_comp_T1,
    gain_total_prod = Total_prod_T2 - Total_prod_T1,
    gain_concept_comp = Concept_comp_T2 - Concept_comp_T1,
    gain_concept_prod = Concept_prod_T2 - Concept_prod_T1
  )


#### --- GAIN REGRESSIONS T2- T1 ----

# ADULT WORD COUNT 

# Fit the model for gain_L1_comprehension *
model_gain_L1_comprehension <- lm(gain_L1_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_L1_comprehension)

# Fit the model for gain_L1_production
model_gain_L1_production <- lm(gain_L1_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_L1_production)

# Fit the model for gain_L2_comprehension (only bilinguals)
model_gain_L2_comprehension <- lm(gain_L2_comp ~ AWC_COUNT_Avg + sex, data = cdi_counts_t1_t2 %>% filter(Mono_Bil == "Bil"))
summary(model_gain_L2_comprehension)

# Fit the model for gain_L2_production (only bilinguals)
model_gain_L2_production <- lm(gain_L2_prod ~ AWC_COUNT_Avg + sex, data = cdi_counts_t1_t2 %>% filter(Mono_Bil == "Bil"))
summary(model_gain_L2_production)

# Fit the model for gain_total_comprehension *
model_gain_total_comprehension <- lm(gain_total_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_total_comprehension)

# Fit the model for gain_total_production
model_gain_total_production <- lm(gain_total_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_total_production)

# Fit the model for gain_conceptual_comprehension
model_gain_conceptual_comprehension <- lm(gain_concept_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_conceptual_comprehension)

# Fit the model for gain_conceptual_production
model_gain_conceptual_production <- lm(gain_concept_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_conceptual_production)

### CONVERSATIONAL TURNS 

# Fit the model for gain_L1_comprehension
model_gain_L1_comprehension_ct <- lm(gain_L1_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_L1_comprehension_ct)

# Fit the model for gain_L1_production
model_gain_L1_production_ct <- lm(gain_L1_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_L1_production_ct)

# Fit the model for gain_L2_comprehension (only bilinguals)
model_gain_L2_comprehension_ct <- lm(gain_L2_comp ~ CT_COUNT_Avg + sex, data = cdi_counts_t1_t2 %>% filter(Mono_Bil == "Bil"))
summary(model_gain_L2_comprehension_ct)

# Fit the model for gain_L2_production (only bilinguals)
model_gain_L2_production_ct <- lm(gain_L2_prod ~ CT_COUNT_Avg + sex, data = cdi_counts_t1_t2 %>% filter(Mono_Bil == "Bil"))
summary(model_gain_L2_production_ct)

# Fit the model for gain_total_comprehension
model_gain_total_comprehension_ct <- lm(gain_total_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_total_comprehension_ct)

# Fit the model for gain_total_production
model_gain_total_production_ct <- lm(gain_total_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_total_production_ct)

# Fit the model for gain_conceptual_comprehension
model_gain_conceptual_comprehension_ct <- lm(gain_concept_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_conceptual_comprehension_ct)

# Fit the model for gain_conceptual_production
model_gain_conceptual_production_ct <- lm(gain_concept_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t2)
summary(model_gain_conceptual_production_ct)







#############################
# GAIN FOR T3-T1 VOCAB MEASURES # ----------------------------------

# Step 1: Filter for tokens 1 and 3, then reshape the data so T1 and T3 values are in separate columns
cdi_counts_t1_t3 <- CDI_counts_LENA %>%
  filter(token %in% c(1, 3)) %>%  # Keep only T1 and T3 data
  pivot_wider(
    id_cols = ID_lab, 
    names_from = token,  # Separate columns for token 1 and token 3
    values_from = c(L1_comp, L2_comp, L1_prod, L2_prod, Total_comp, Total_prod, Concept_comp, Concept_prod, age_CDI),
    names_glue = "{.value}_T{token}"  # Create names like L1_comp_T1, L1_comp_T3, etc.
  ) %>%
  left_join(
    CDI_counts_LENA %>% 
      select(ID_lab, bilingual_index, Mono_Bil, sex, AWC_COUNT_Avg, CT_COUNT_Avg, Overlap_Avg) %>%
      distinct(), by = "ID_lab"  # Keep bilingual_index and LENA measures
  )

# Step 2: Calculate gain scores by subtracting T1 values from T3 values
cdi_counts_t1_t3 <- cdi_counts_t1_t3 %>%
  mutate(
    gain_L1_comp = L1_comp_T3 - L1_comp_T1,
    gain_L2_comp = L2_comp_T3 - L2_comp_T1,
    gain_L1_prod = L1_prod_T3 - L1_prod_T1,
    gain_L2_prod = L2_prod_T3 - L2_prod_T1,
    gain_total_comp = Total_comp_T3 - Total_comp_T1,
    gain_total_prod = Total_prod_T3 - Total_prod_T1,
    gain_concept_comp = Concept_comp_T3 - Concept_comp_T1,
    gain_concept_prod = Concept_prod_T3 - Concept_prod_T1
  )

#### --- GAIN REGRESSIONS T3- T1 ----

# ADULT WORD COUNT (AWC)

# Fit the model for gain_L1_comprehension
model_gain_L1_comprehension <- lm(gain_L1_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_L1_comprehension)

# Fit the model for gain_L1_production
model_gain_L1_production <- lm(gain_L1_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_L1_production)

# Fit the model for gain_L2_comprehension (only bilinguals)
model_gain_L2_comprehension <- lm(gain_L2_comp ~ AWC_COUNT_Avg + sex, data = cdi_counts_t1_t3 %>% filter(Mono_Bil == "Bil"))
summary(model_gain_L2_comprehension)

# Fit the model for gain_L2_production (only bilinguals)
model_gain_L2_production <- lm(gain_L2_prod ~ AWC_COUNT_Avg + sex, data = cdi_counts_t1_t3 %>% filter(Mono_Bil == "Bil"))
summary(model_gain_L2_production)

# Fit the model for gain_total_comprehension
model_gain_total_comprehension <- lm(gain_total_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_total_comprehension)

# Fit the model for gain_total_production
model_gain_total_production <- lm(gain_total_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_total_production)

# Fit the model for gain_conceptual_comprehension
model_gain_conceptual_comprehension <- lm(gain_concept_comp ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_conceptual_comprehension)

# Fit the model for gain_conceptual_production
model_gain_conceptual_production <- lm(gain_concept_prod ~ AWC_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_conceptual_production)

### CONVERSATIONAL TURNS (CT)

# Fit the model for gain_L1_comprehension
model_gain_L1_comprehension_ct <- lm(gain_L1_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_L1_comprehension_ct)

# Fit the model for gain_L1_production
model_gain_L1_production_ct <- lm(gain_L1_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_L1_production_ct)

# Fit the model for gain_L2_comprehension (only bilinguals)
model_gain_L2_comprehension_ct <- lm(gain_L2_comp ~ CT_COUNT_Avg + sex, data = cdi_counts_t1_t3 %>% filter(Mono_Bil == "Bil"))
summary(model_gain_L2_comprehension_ct)

# Fit the model for gain_L2_production (only bilinguals)
model_gain_L2_production_ct <- lm(gain_L2_prod ~ CT_COUNT_Avg + sex, data = cdi_counts_t1_t3 %>% filter(Mono_Bil == "Bil"))
summary(model_gain_L2_production_ct)

# Fit the model for gain_total_comprehension
model_gain_total_comprehension_ct <- lm(gain_total_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_total_comprehension_ct)

# Fit the model for gain_total_production
model_gain_total_production_ct <- lm(gain_total_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_total_production_ct)

# Fit the model for gain_conceptual_comprehension
model_gain_conceptual_comprehension_ct <- lm(gain_concept_comp ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_conceptual_comprehension_ct)

# Fit the model for gain_conceptual_production
model_gain_conceptual_production_ct <- lm(gain_concept_prod ~ CT_COUNT_Avg * bilingual_index + sex, data = cdi_counts_t1_t3)
summary(model_gain_conceptual_production_ct)

