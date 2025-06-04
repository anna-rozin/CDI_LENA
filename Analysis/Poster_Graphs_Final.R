### POSTER GRAPHS ######
# Clear previous data
rm(list = ls())

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(interactions)  
library(jtools) 

# Load data

# Load data
all_data <- read.csv(file.path("raw-csv1", "Summary", "final.csv"))
merged_T1 <- read.csv(file.path("raw-csv1", "Summary", "merged_T1.csv"))
merged_T2 <- read.csv(file.path("raw-csv1", "Summary", "merged_T2.csv"))
merged_T3 <- read.csv(file.path("raw-csv1", "Summary", "merged_T3.csv"))


# Define the IDs you want to remove
exclude_ids <- c("7343", "7207")

# Filter each dataset and overwrite
all_data <- all_data[!all_data$ID_lab %in% exclude_ids, ]
merged_T1 <- merged_T1[!merged_T1$ID_lab %in% exclude_ids, ]
merged_T2 <- merged_T2[!merged_T2$ID_lab %in% exclude_ids, ]
merged_T3 <- merged_T3[!merged_T3$ID_lab %in% exclude_ids, ]

#

### -----------------------------------------------------------------------------------------------
# BILINGUALISM INDEX BAR PLOT
library(dplyr)
library(ggplot2)

# Define bin breaks and calculate midpoints
bin_breaks <- seq(0, 1, by = 0.1)
bin_mids <- head(bin_breaks, -1) + diff(bin_breaks) / 2

# Bin data manually by midpoint
unique_data <- all_data %>%
  distinct(ID_lab, .keep_all = TRUE) %>%
  mutate(
    bilingual_bin = cut(bilingual_index, breaks = bin_breaks, include.lowest = TRUE),
    bin_mid = bin_mids[as.numeric(cut(bilingual_index, breaks = bin_breaks, include.lowest = TRUE))],
    Mono_Bil = factor(Mono_Bil, levels = c("Mono", "Bil"))
  )

# Count participants per bin midpoint and group
bin_counts <- unique_data %>%
  group_by(bin_mid, Mono_Bil) %>%
  summarise(count = n(), .groups = "drop")

# Plot on a continuous x-axis
ggplot(bin_counts, aes(x = bin_mid, y = count, fill = Mono_Bil)) +
  geom_col(position = "stack", width = 0.09) +
  scale_fill_manual(values = c("Mono" = "lightblue", "Bil" = "steelblue")) +
  scale_x_continuous(
    breaks = bin_breaks,
    limits = c(0, 1)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Participants Bilingual Range",
    x = "Bilingual Index",
    y = "Number of Participants",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )

#----------------------

#--------------------------------------------------------------------------------
# Bilingual status pie chart 
library(dplyr)
library(ggplot2)
# ---------------------------
# Prepare bilingual summary with one row per participant
bilingual_summary <- all_data %>%
  distinct(ID_lab, .keep_all = TRUE) %>%
  count(Mono_Bil) %>%
  mutate(
    Mono_Bil = factor(Mono_Bil, levels = c("Mono", "Bil"))
  ) %>%
  arrange(Mono_Bil)

# Create pie chart without center labels
ggplot(bilingual_summary, aes(x = "", y = n, fill = Mono_Bil)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  # Removed geom_text() here
  scale_fill_manual(values = c("Mono" = "lightblue", "Bil" = "steelblue")) +
  labs(
    title = "Participant Language Status",
    fill = "Status"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16)
  )

#mono n= 19 
# bil n = 17

# ----------------------------------------------------------------------------
# LENA input over time CT and AWC

# Step 1: Prepare individual data in long format
lena_individual <- all_data %>%
  select(ID_lab, token_cdi, AWC_COUNT_Avg, CT_COUNT_Avg) %>%
  pivot_longer(cols = c(AWC_COUNT_Avg, CT_COUNT_Avg),
               names_to = "Measure",
               values_to = "Count") %>%
  mutate(
    Measure = factor(Measure,
                     levels = c("AWC_COUNT_Avg", "CT_COUNT_Avg"),
                     labels = c("Adult Words", "Conversational Turns"))
  )

# Step 2: Summarize mean counts per token
lena_summary <- all_data %>%
  group_by(token_cdi) %>%
  summarise(
    AWC = mean(AWC_COUNT_Avg, na.rm = TRUE),
    CT = mean(CT_COUNT_Avg, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Pivot summary to long format
lena_long <- lena_summary %>%
  pivot_longer(cols = c(AWC, CT),
               names_to = "Measure",
               values_to = "Mean_Count") %>%
  mutate(
    Measure = factor(Measure,
                     levels = c("AWC", "CT"),
                     labels = c("Adult Words", "Conversational Turns"))
  )

# Step 4: Compute max y for expanding y-axis
max_y <- lena_long %>%
  group_by(Measure) %>%
  summarise(max_y = max(Mean_Count, na.rm = TRUE) * 1.2)

lena_long_with_limit <- lena_long %>%
  left_join(max_y, by = "Measure")

# Step 5: Plot individual points and summary
ggplot() +
  # individual points (jitter for visibility)
  geom_jitter(data = lena_individual,
              aes(x = token_cdi, y = Count),
              color = "grey60", alpha = 0.4, width = 0.2, height = 0,
              size = 1.5) +
  # summary points and line
  geom_point(data = lena_long_with_limit,
             aes(x = token_cdi, y = Mean_Count),
             color = "forestgreen", size = 2) +
  geom_line(data = lena_long_with_limit,
            aes(x = token_cdi, y = Mean_Count, group = Measure),
            color = "forestgreen", linewidth = 0.8) +
  geom_blank(data = lena_long_with_limit, aes(x = token_cdi, y = max_y)) +
  facet_wrap(~ Measure, scales = "free_y") +
  scale_x_continuous(
    breaks = unique(lena_long$token_cdi),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  expand_limits(y = 0) +
  labs(
    title = "LENA Input Over Time",
    x = "Time Point",
    y = "Average Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(hjust = 0.5),
    strip.text = element_text(size = 14)
  )

##########-------------------------------------------------------------
# AVERAGE AGES FOR LENA AND CDI
library(dplyr)
library(ggplot2)

# Prepare CDI data
cdi_data <- all_data %>%
  select(ID_lab, token_cdi, age_CDI) %>%
  rename(token = token_cdi, age_value = age_CDI) %>%
  mutate(age_type = "age_CDI")

# Prepare LENA data
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

# Convert token to factor
age_summary$token <- factor(age_summary$token)

# Rename age_type levels for nicer legend labels
age_summary$age_type <- factor(age_summary$age_type,
                               levels = c("age_CDI", "age_lena"),
                               labels = c("CDI", "LENA"))

# Get full integer range of ages for y-axis
all_y_breaks <- floor(min(age_summary$mean_age - age_summary$se, na.rm = TRUE)):
  ceiling(max(age_summary$mean_age + age_summary$se, na.rm = TRUE))

# Plot
ggplot(age_summary, aes(x = token, y = mean_age, color = age_type, group = age_type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_age - se, ymax = mean_age + se), width = 0.2) +
  scale_x_discrete(expand = expansion(add = 0.25)) +
  scale_y_continuous(breaks = all_y_breaks) +
  scale_color_manual(values = c("CDI" = "#9467bd", "LENA" = "#2ca02c")) +  # purple & green
  labs(
    title = "CDI & LENA Average Ages",
    x = "Time Point",
    y = "Average Months of Age",
    color = "Age Type"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(10, 20, 10, 20),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, size = 16),  # <--- Centered title
    panel.spacing = unit(1, "lines")
  )



# ----------------------------------------------------------------------------------------
# COMPREHENSION SIZE ACROSS TIME POINTS 

library(dplyr)
library(ggplot2)
library(tidyr)

# Your existing vocab_data and vocab_summary code (unchanged)
vocab_data <- all_data %>%
  select(ID_lab, token_cdi, L1_comp, Concept_comp, Total_comp) %>%
  filter(!is.na(token_cdi))

vocab_data$token_cdi <- as.factor(vocab_data$token_cdi)

vocab_long <- vocab_data %>%
  pivot_longer(cols = c(L1_comp, Concept_comp, Total_comp),
               names_to = "vocab_type",
               values_to = "vocab_size")

vocab_summary <- vocab_long %>%
  group_by(token_cdi, vocab_type) %>%
  summarise(
    mean_vocab = mean(vocab_size, na.rm = TRUE),
    sd_vocab = sd(vocab_size, na.rm = TRUE),
    n = n(),
    se = sd_vocab / sqrt(n),
    .groups = "drop"
  )

vocab_summary$vocab_type <- factor(vocab_summary$vocab_type,
                                   levels = c("L1_comp", "Concept_comp", "Total_comp"),
                                   labels = c("L1", "Conceptual", "Total"))

# Extract participant-level Conceptual scores for plotting points
conceptual_points <- vocab_data %>%
  select(ID_lab, token_cdi, Concept_comp) %>%
  rename(vocab_size = Concept_comp) %>%
  filter(!is.na(vocab_size)) %>%
  mutate(point_type = "Participant \nConceptual Scores")  # Add for legend

# Filter participant-level Conceptual scores for plotting points (max 800)
conceptual_points_filtered <- conceptual_points %>%
  filter(vocab_size <= 1600)

# Add dummy point_type column to vocab_summary for consistent legend mapping
vocab_summary <- vocab_summary %>%
  mutate(point_type = NA_character_)

# Plot
ggplot() +
  # Summary lines and points
  geom_line(data = vocab_summary, aes(x = token_cdi, y = mean_vocab, color = vocab_type, group = vocab_type), linewidth = 1) +
  geom_point(data = vocab_summary, aes(x = token_cdi, y = mean_vocab, color = vocab_type), size = 3) +
  geom_errorbar(data = vocab_summary, aes(x = token_cdi, ymin = mean_vocab - se, ymax = mean_vocab + se, color = vocab_type), width = 0.2) +
  # Transparent grey points for individuals with legend entry
  geom_point(data = conceptual_points_filtered,
             aes(x = token_cdi, y = vocab_size, color = point_type),
             alpha = 0.3, size = 2) +
  scale_color_manual(
    name = "Vocabulary Type",
    values = c(
      "L1" = "hotpink",
      "Conceptual" = "black",
      "Total" = "purple",
      "Participant \nConceptual Scores" = "grey40"
    ),
    breaks = c("L1", "Conceptual", "Total", "Participant \nConceptual Scores")
  ) +
  coord_cartesian(ylim = c(0, 800)) +
  labs(
    title = "Comprehension Size",
    x = "Time Point",
    y = "Mean Vocabulary Size"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5)
  )

# ----------------------------------------------------------------------------------------
# PRODUCTION SIZE ACROSS TIME POINTS 
# Your existing prod_data and prod_summary code (unchanged)
prod_data <- all_data %>%
  select(ID_lab, token_cdi, L1_prod, Concept_prod, Total_prod) %>%
  filter(!is.na(token_cdi))

prod_data$token_cdi <- as.factor(prod_data$token_cdi)

prod_long <- prod_data %>%
  pivot_longer(cols = c(L1_prod, Concept_prod, Total_prod),
               names_to = "prod_type",
               values_to = "prod_size")

prod_summary <- prod_long %>%
  group_by(token_cdi, prod_type) %>%
  summarise(
    mean_prod = mean(prod_size, na.rm = TRUE),
    sd_prod = sd(prod_size, na.rm = TRUE),
    n = n(),
    se = sd_prod / sqrt(n),
    .groups = "drop"
  )

prod_summary$prod_type <- factor(prod_summary$prod_type,
                                 levels = c("L1_prod", "Concept_prod", "Total_prod"),
                                 labels = c("L1", "Conceptual", "Total"))

# Extract participant-level Conceptual production scores for plotting points
conceptual_prod_points <- prod_data %>%
  select(ID_lab, token_cdi, Concept_prod) %>%
  rename(prod_size = Concept_prod) %>%
  filter(!is.na(prod_size)) %>%
  mutate(point_type = "Participant \nConceptual Scores")  # Add for legend

# Filter participant-level Conceptual production scores for plotting points (max 150)
conceptual_prod_points_filtered <- conceptual_prod_points %>%
  filter(prod_size <= 110)

# Add dummy point_type column to prod_summary for consistent legend mapping
prod_summary <- prod_summary %>%
  mutate(point_type = NA_character_)

# Plot
ggplot() +
  # Summary lines and points
  geom_line(data = prod_summary, aes(x = token_cdi, y = mean_prod, color = prod_type, group = prod_type), linewidth = 1) +
  geom_point(data = prod_summary, aes(x = token_cdi, y = mean_prod, color = prod_type), size = 3) +
  geom_errorbar(data = prod_summary, aes(x = token_cdi, ymin = mean_prod - se, ymax = mean_prod + se, color = prod_type), width = 0.2) +
  # Transparent grey points for individuals with legend entry
  geom_point(data = conceptual_prod_points_filtered,
             aes(x = token_cdi, y = prod_size, color = point_type),
             alpha = 0.3, size = 2) +
  scale_color_manual(
    name = "Vocabulary Type",
    values = c(
      "L1" = "hotpink",
      "Conceptual" = "black",
      "Total" = "purple",
      "Participant \nConceptual Scores" = "grey40"
    ),
    breaks = c("L1", "Conceptual", "Total", "Participant \nConceptual Scores")
  ) +
  coord_cartesian(ylim = c(0, 110)) +
  labs(
    title = "Production Size across Time Points",
    x = "Time Point",
    y = "Mean Vocabulary Size"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5)
  )

# # ----------------------------------------------------------------------------------------

library(interactions)
library(ggplot2)

mean_bi <- mean(merged_T1$bilingual_index, na.rm = TRUE)
sd_bi <- sd(merged_T1$bilingual_index, na.rm = TRUE)
range_bi <- range(merged_T1$bilingual_index, na.rm = TRUE)
modx_vals <- c(mean_bi - sd_bi, mean_bi, mean_bi + sd_bi)
modx_vals_clamped <- pmax(pmin(modx_vals, max(range_bi)), min(range_bi))

# CONCEPT AWC -----------------------------

interact_plot(
  model = lm(Concept_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1),
  pred = AWC_COUNT_Avg,
  modx = bilingual_index,
  modx.values = modx_vals_clamped,
  modx.labels = c("-1 SD", "Mean", "+1 SD"),
  plot.points = TRUE,
  interval = TRUE,
  x.label = "Adult Word Count (AWC)",
  y.label = "Conceptual Comprehension",
  modx.label = "Bilingual Index",
  colors = c("skyblue", "black", "blue"),
  partial.residuals = FALSE
) +
  ylim(NA, 800) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    legend.position = "top"
  ) +
  scale_linetype_manual(values = rep("solid", 3)) +  # All solid lines
  guides(linetype = "none")                         # Hide linetype legend

##############

library(interactions)

sim_slopes(
  model = lm(Concept_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1),
  pred = "AWC_COUNT_Avg",
  modx = "bilingual_index",
  modx.values = modx_vals_clamped
)


# L1 AWC -----------------------------

interact_plot(
  model = lm(L1_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1),
  pred = AWC_COUNT_Avg,
  modx = bilingual_index,
  modx.values = modx_vals_clamped,
  modx.labels = c("-1 SD", "Mean", "+1 SD"),
  plot.points = TRUE,
  interval = TRUE,
  x.label = "Adult Word Count (AWC)",
  y.label = "Dominant Comprehension",
  modx.label = "Bilingual Index",
  colors = c("skyblue", "black", "blue"),
  partial.residuals = FALSE
) +
  ylim(NA, 800) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    legend.position = "top"
  ) +
  scale_linetype_manual(values = rep("solid", 3)) +  # All solid lines
  guides(linetype = "none")                         # Hide linetype legend


##############

library(interactions)

sim_slopes(
  model = lm(L1_comp ~ AWC_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1),
  pred = "AWC_COUNT_Avg",
  modx = "bilingual_index",
  modx.values = modx_vals_clamped
)

# L1 CT -----------------------------

interact_plot(
  model = lm(L1_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1),
  pred = CT_COUNT_Avg,
  modx = bilingual_index,
  modx.values = modx_vals_clamped,
  modx.labels = c("-1 SD", "Mean", "+1 SD"),
  plot.points = TRUE,
  interval = TRUE,
  x.label = "CT",
  y.label = "Dominant Comprehension",
  modx.label = "Bilingual Index",
  colors = c("skyblue", "black", "blue"),
  partial.residuals = FALSE
) +
  ylim(NA, 800) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    legend.position = "top"
  ) +
  scale_linetype_manual(values = rep("solid", 3)) +  # All solid lines
  guides(linetype = "none")                         # Hide linetype legend


library(interactions)

sim_slopes(
  model = lm(L1_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1),
  pred = "CT_COUNT_Avg",
  modx = "bilingual_index",
  modx.values = modx_vals_clamped
)

# CONCEPT CT -----------------------------

interact_plot(
  model = lm(Concept_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1),
  pred = CT_COUNT_Avg,
  modx = bilingual_index,
  modx.values = modx_vals_clamped,
  modx.labels = c("-1 SD", "Mean", "+1 SD"),
  plot.points = TRUE,
  interval = TRUE,
  x.label = "CT",
  y.label = "Concept Comprehension",
  modx.label = "Bilingual Index",
  colors = c("skyblue", "black", "blue"),
  partial.residuals = FALSE
) +
  ylim(NA, 800) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    legend.position = "top"
  ) +
  scale_linetype_manual(values = rep("solid", 3)) +  # All solid lines
  guides(linetype = "none")                         # Hide linetype legend


library(interactions)

sim_slopes(
  model = lm(Concept_comp ~ CT_COUNT_Avg * bilingual_index + Sex + token_cdi, data = merged_T1),
  pred = "CT_COUNT_Avg",
  modx = "bilingual_index",
  modx.values = modx_vals_clamped
)
