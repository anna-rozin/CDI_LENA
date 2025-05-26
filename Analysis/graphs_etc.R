# Working on Plots, Graphs, and charts!

# Clear previous data
rm(list = ls())

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(interactions)  
library(jtools) 
library(lme4)
library(lmerTest)

# Load data
all_data <- read.csv(file.path("raw-csv1", "Summary", "final.csv"))

### -----------------------------------------------------------------------------------------------
# Note,
#token_CDI is the token for lena and cdi, sorry should have renamed it to just token for less confusion


### -----------------------------------------------------------------------------------------------
# BILINGUALISM INDEX BAR PLOT

# Keep only one row per unique participant
unique_data <- all_data %>%
  distinct(ID_lab, .keep_all = TRUE)

# Define custom bins
bin_breaks <- c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
unique_data <- unique_data %>%
  mutate(bilingual_bin = cut(bilingual_index, breaks = bin_breaks, include.lowest = TRUE))

# Count number of participants per bin
bin_counts <- unique_data %>%
  group_by(bilingual_bin) %>%
  summarise(count = n())

# Plot
ggplot(bin_counts, aes(x = bilingual_bin, y = count)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(
    title = "Participants Bilingual Range",
    x = "Bilingual Index Range",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)  # This centers the title
  )

# it seems like there are much more monolinguals however really our sample is split 50/50
# since bilingualism on the index is considered more than 0.1

#--------------------------------------------------------------------------------
# Bilingual status pie chart 

# Count unique participants by bilingual status
bilingual_summary <- all_data %>%
  distinct(ID_lab, .keep_all = TRUE) %>%
  count(Mono_Bil)

# Add label column with "n = X"
bilingual_summary <- bilingual_summary %>%
  mutate(label = paste0(Mono_Bil, "\n", "n = ", n))

# Create pie chart
ggplot(bilingual_summary, aes(x = "", y = n, fill = Mono_Bil)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 5) +
  labs(
    title = "Participant Language Status ",
    fill = "Status"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16)
  )

# add this into the chart 
##########-------------------------------------------------------------
# AVERAGE AGES FOR LENA AND CDI

# Prepare CDI data
cdi_data <- all_data %>%
  select(ID_lab, token_cdi, age_CDI) %>%
  rename(token = token_cdi, age_value = age_CDI) %>%
  mutate(age_type = "age_CDI")

# Prepare Lena data
lena_data <- all_data %>%
  select(ID_lab, token_lena, age_lena) %>%
  rename(token = token_lena, age_value = age_lena) %>%
  mutate(age_type = "age_lena")

# Combine both
age_long <- bind_rows(cdi_data, lena_data)

# Summarize mean and SE by token and age_type
age_summary <- age_long %>%
  group_by(token, age_type) %>%
  summarise(
    mean_age = mean(age_value, na.rm = TRUE),
    sd_age = sd(age_value, na.rm = TRUE),
    n = n(),
    se = sd_age / sqrt(n),
    .groups = "drop"
  )

# Convert token to factor for plotting
age_summary$token <- factor(age_summary$token)

# Rename age_type factor levels for nicer legend labels
age_summary$age_type <- factor(age_summary$age_type,
                               levels = c("age_CDI", "age_lena"),
                               labels = c("CDI", "LENA"))

# Plot
ggplot(age_summary, aes(x = token, y = mean_age, color = age_type, group = age_type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_age - se, ymax = mean_age + se), width = 0.2) +
  scale_x_discrete(expand = expansion(add = 0.25)) +  # <-- added to increase x-axis spacing
  labs(
    title = "LENA & CDI Average Ages",
    x = "Time Point",
    y = "Average Months of Age",
    color = "Age Type"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(10, 20, 10, 20),  # top, right, bottom, left
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.spacing = unit(1, "lines")
  )

# change colours

# ----------------------------------------------------------------------------------------
# COMPREHENSION SIZE ACROSS TIME POINTS 

# Select relevant columns
vocab_data <- all_data %>%
  select(ID_lab, token_cdi, L1_comp, Concept_comp, Total_comp) %>%
  filter(!is.na(token_cdi))  # just in case

# Convert token_cdi to factor or numeric (if you want continuous x-axis)
vocab_data$token_cdi <- as.factor(vocab_data$token_cdi)

# Reshape to long format for ggplot
vocab_long <- vocab_data %>%
  pivot_longer(cols = c(L1_comp, Concept_comp, Total_comp),
               names_to = "vocab_type",
               values_to = "vocab_size")

# Summarize mean vocab size by token and vocab_type
vocab_summary <- vocab_long %>%
  group_by(token_cdi, vocab_type) %>%
  summarise(
    mean_vocab = mean(vocab_size, na.rm = TRUE),
    sd_vocab = sd(vocab_size, na.rm = TRUE),
    n = n(),
    se = sd_vocab / sqrt(n),
    .groups = "drop"
  )

# Renaming legend labels
vocab_summary$vocab_type <- factor(vocab_summary$vocab_type,
                                   levels = c("L1_comp", "Concept_comp", "Total_comp"),
                                   labels = c("L1", "Conceptual", "Total"))

# Plot
ggplot(vocab_summary, aes(x = token_cdi, y = mean_vocab, color = vocab_type, group = vocab_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_vocab - se, ymax = mean_vocab + se), width = 0.2) +
  scale_color_manual(values = c(
    "L1" = "green",
    "Conceptual" = "blue",
    "Total" = "red"
  )) +
  labs(
    title = "Comprehension Size",
    x = "Time Point",
    y = "Mean Vocabulary Size",
    color = "Vocabulary Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # center title
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5)
  )

# should I add L2 in the as well ????????
# should i add age with time points for clarity?
# add grey points

# ----------------------------------------------------------------------------------------
# PRODUCTION SIZE ACROSS TIME POINTS 

# Select relevant columns for production
prod_data <- all_data %>%
  select(ID_lab, token_cdi, L1_prod, Concept_prod, Total_prod) %>%
  filter(!is.na(token_cdi))

# Convert token_cdi to factor
prod_data$token_cdi <- as.factor(prod_data$token_cdi)

# Reshape to long format for ggplot
prod_long <- prod_data %>%
  pivot_longer(cols = c(L1_prod, Concept_prod, Total_prod),
               names_to = "prod_type",
               values_to = "prod_size")

# Summarize mean production size by token and prod_type
prod_summary <- prod_long %>%
  group_by(token_cdi, prod_type) %>%
  summarise(
    mean_prod = mean(prod_size, na.rm = TRUE),
    sd_prod = sd(prod_size, na.rm = TRUE),
    n = n(),
    se = sd_prod / sqrt(n),
    .groups = "drop"
  )

# Rename factor levels for nicer legend labels
prod_summary$prod_type <- factor(prod_summary$prod_type,
                                 levels = c("L1_prod", "Concept_prod", "Total_prod"),
                                 labels = c("L1", "Conceptual", "Total"))

# Plot
ggplot(prod_summary, aes(x = token_cdi, y = mean_prod, color = prod_type, group = prod_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_prod - se, ymax = mean_prod + se), width = 0.2) +
  scale_color_manual(values = c(
    "L1" = "green",
    "Conceptual" = "blue",
    "Total" = "red"
  )) +
  labs(
    title = "Production Size across Time Points",
    x = "Time Point",
    y = "Mean Vocabulary Size",
    color = "Vocabulary Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # center title
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5)
  )

#### wow what a jump from 15 months to 18 month ! so cute 
# should I add L2?
# should i add age with time points for clarity?
# add grey points

###------------------------------------------------------------------------------------------
# simple analysis -----------

library(dplyr)
library(lme4)
library(lmerTest)

# L1 --------------------------

# Filter and prepare relevant data
l1_data <- all_data %>%
  select(ID_lab, token_cdi, L1_comp, bilingual_index) %>%
  filter(!is.na(token_cdi), !is.na(L1_comp), !is.na(bilingual_index))

# Convert token to factor
l1_data$token_cdi <- as.factor(l1_data$token_cdi)

# Linear mixed model with bilingual index as a continuous predictor
model_l1 <- lmer(L1_comp ~ token_cdi * bilingual_index + (1 | ID_lab), data = l1_data)
summary(model_l1)

#### As we discussed bilingualism does not play a role in the growth 

# Total -----------------------

# Prepare data
total_data <- all_data %>%
  filter(!is.na(Total_comp), !is.na(token_cdi), !is.na(bilingual_index)) %>%
  select(ID_lab, token_cdi, bilingual_index, Total_comp)

# Run model
library(lmerTest)
model_total <- lmer(Total_comp ~ token_cdi * bilingual_index + (1 | ID_lab), data = total_data)

# Summary
summary(model_total)

##### bilingualism across time interaction is sig ...

# Conceptual ----------------------
# Prepare data
concept_data <- all_data %>%
  filter(!is.na(Concept_comp), !is.na(token_cdi), !is.na(bilingual_index)) %>%
  select(ID_lab, token_cdi, bilingual_index, Concept_comp)

# Run model
model_concept <- lmer(Concept_comp ~ token_cdi * bilingual_index + (1 | ID_lab), data = concept_data)

# Summary
summary(model_concept)

##### bilingualism across time interaction is sig ...



