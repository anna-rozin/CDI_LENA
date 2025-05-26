####### ------- #########

# Clear previous data
rm(list = ls())

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(interactions)  
library(jtools) 

# Load data
merged_T1 <- read.csv(file.path("raw-csv1", "Summary", "merged_T1.csv"))
merged_T2 <- read.csv(file.path("raw-csv1", "Summary", "merged_T2.csv"))
merged_T3 <- read.csv(file.path("raw-csv1", "Summary", "merged_T3.csv"))


#----------------------------------------------------------------#
# merged_T1 includes lena t1 and cdi t1,2,3
# merged_T2 includes lena t2 and cdi t1,2,3
# merged_T3 includes lena t3 and cdi t2,3
#---------------------------------------------------------------#

###### TIME POINT 1 LENA  ----------------------------------------------------------------------

# ----- AWC COUNT ---- COMPREHENSION ------------------

# L1 Comprehension
T1_AWC_comp_L1 <- lm(L1_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_AWC_comp_L1)

# Concept Comprehension
T1_AWC_comp_Con <- lm(Concept_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_AWC_comp_Con)

# Total Comprehension
T1_AWC_comp_Tot <- lm(Total_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_AWC_comp_Tot)

#### INTERACTION 

# Plotting 
interact_plot(
  model = lm(Concept_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1),
  pred = AWC_COUNT_Avg,
  modx = bilingual_index,
  plot.points = TRUE,
  interval = TRUE
)

# ----- AWC COUNT ---- PRODUCTION ------------------

# L1 Production
T1_AWC_prod_L1 <- lm(L1_prod ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_AWC_prod_L1)

# Concept Production
T1_AWC_prod_Con <- lm(Concept_prod ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_AWC_prod_Con)

# Total Production
T1_AWC_prod_Tot <- lm(Total_prod ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_AWC_prod_Tot)


# ----- CT COUNT ---- COMPREHENSION ------------------

# L1 Comprehension
T1_CT_comp_L1 <- lm(L1_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_CT_comp_L1)

# Concept Comprehension
T1_CT_comp_Con <- lm(Concept_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_CT_comp_Con)

# Total Comprehension
T1_CT_comp_Tot <- lm(Total_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_CT_comp_Tot)


# ----- CT COUNT ---- PRODUCTION ------------------

# L1 Production
T1_CT_prod_L1 <- lm(L1_prod ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_CT_prod_L1)

# Concept Production
T1_CT_prod_Con <- lm(Concept_prod ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_CT_prod_Con)

# Total Production
T1_CT_prod_Tot <- lm(Total_prod ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1)
summary(T1_CT_prod_Tot)





###### TIME POINT 2 LENA  --------------------------------------------------------------------

# ----- AWC COUNT ---- COMPREHENSION ------------------

T2_AWC_comp_L1 <- lm(L1_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_AWC_comp_L1)

T2_AWC_comp_Con <- lm(Concept_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_AWC_comp_Con)

T2_AWC_comp_Tot <- lm(Total_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_AWC_comp_Tot)


# ----- AWC COUNT ---- PRODUCTION ------------------

T2_AWC_prod_L1 <- lm(L1_prod ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_AWC_prod_L1)

T2_AWC_prod_Con <- lm(Concept_prod ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_AWC_prod_Con)

T2_AWC_prod_Tot <- lm(Total_prod ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_AWC_prod_Tot)


# ----- CT COUNT ---- COMPREHENSION ------------------

T2_CT_comp_L1 <- lm(L1_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_CT_comp_L1)

T2_CT_comp_Con <- lm(Concept_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_CT_comp_Con)

T2_CT_comp_Tot <- lm(Total_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_CT_comp_Tot)


# ----- CT COUNT ---- PRODUCTION ------------------

T2_CT_prod_L1 <- lm(L1_prod ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_CT_prod_L1)

T2_CT_prod_Con <- lm(Concept_prod ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_CT_prod_Con)

T2_CT_prod_Tot <- lm(Total_prod ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T2)
summary(T2_CT_prod_Tot)





# ###### TIME POINT 3 LENA  --------------------------------------------------------------------

# ----- AWC COUNT ---- COMPREHENSION ------------------

T3_AWC_comp_L1 <- lm(L1_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_AWC_comp_L1)

T3_AWC_comp_Con <- lm(Concept_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_AWC_comp_Con)

T3_AWC_comp_Tot <- lm(Total_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_AWC_comp_Tot)


# ----- AWC COUNT ---- PRODUCTION ------------------

T3_AWC_prod_L1 <- lm(L1_prod ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_AWC_prod_L1)

T3_AWC_prod_Con <- lm(Concept_prod ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_AWC_prod_Con)

T3_AWC_prod_Tot <- lm(Total_prod ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_AWC_prod_Tot)


# ----- CT COUNT ---- COMPREHENSION ------------------

T3_CT_comp_L1 <- lm(L1_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_CT_comp_L1)

T3_CT_comp_Con <- lm(Concept_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_CT_comp_Con)

T3_CT_comp_Tot <- lm(Total_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_CT_comp_Tot)


# ----- CT COUNT ---- PRODUCTION ------------------

T3_CT_prod_L1 <- lm(L1_prod ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_CT_prod_L1)

T3_CT_prod_Con <- lm(Concept_prod ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_CT_prod_Con)

T3_CT_prod_Tot <- lm(Total_prod ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T3)
summary(T3_CT_prod_Tot)



################ GAINS ----------------------------------------------------------------------------------

#Calculating for lena T1 and CDI T2- T1

# Filter to only include participants with both token_cdi 1 and 2
complete_T1 <- merged_T1 %>%
  group_by(ID_lab) %>%
  filter(all(c(1, 2) %in% token_cdi)) %>%
  ungroup()

# Pivot wider to get CDI scores per token_cdi per participant in separate columns
cdi_wide <- merged_T1 %>%
  select(ID_lab, token_cdi, starts_with("L1_"), starts_with("L2_"), starts_with("Total_"), starts_with("Concept_"), bilingual_index, Sex, AWC_COUNT_Avg, CT_COUNT_Avg) %>%
  pivot_wider(
    id_cols = c(ID_lab, bilingual_index, Sex, AWC_COUNT_Avg, CT_COUNT_Avg),
    names_from = token_cdi,
    values_from = c(L1_comp, L2_comp, L1_prod, L2_prod, Total_comp, Total_prod, Concept_comp, Concept_prod),
    names_glue = "{.value}_T{token_cdi}"
  ) %>%
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


# ADULT WORD COUNT 
# L1 Comprehension gain
m_gain_L1_comp_AWC <- lm(gain_L1_comp ~ AWC_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_L1_comp_AWC)

# Concept Comprehension gain
m_gain_Con_comp_AWC <- lm(gain_concept_comp ~ AWC_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_Con_comp_AWC)

# Total Comprehension gain
m_gain_Tot_comp_AWC <- lm(gain_total_comp ~ AWC_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_Tot_comp_AWC)


# L1 Production gain
m_gain_L1_prod_AWC <- lm(gain_L1_prod ~ AWC_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_L1_prod_AWC)

# Concept Production gain
m_gain_Con_prod_AWC <- lm(gain_concept_prod ~ AWC_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_Con_prod_AWC)

# Total Production gain
m_gain_Tot_prod_AWC <- lm(gain_total_prod ~ AWC_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_Tot_prod_AWC)

# CONVERSATION TURNS ------------------------
# Comprehension --

# L1 Comprehension gain
m_gain_L1_comp_CT <- lm(gain_L1_comp ~ CT_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_L1_comp_CT)

# Concept Comprehension gain
m_gain_Con_comp_CT <- lm(gain_concept_comp ~ CT_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_Con_comp_CT)

# Total Comprehension gain
m_gain_Tot_comp_CT <- lm(gain_total_comp ~ CT_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_Tot_comp_CT)

# production ---

# L1 Production gain
m_gain_L1_prod_CT <- lm(gain_L1_prod ~ CT_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_L1_prod_CT)

# Concept Production gain
m_gain_Con_prod_CT <- lm(gain_concept_prod ~ CT_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_Con_prod_CT)

# Total Production gain
m_gain_Tot_prod_CT <- lm(gain_total_prod ~ CT_COUNT_Avg * bilingual_index + Sex, data = cdi_wide)
summary(m_gain_Tot_prod_CT)





