
# Clear previous data
rm(list = ls())

#Load the merged cleaned CDI data
final_df <- read.csv(file.path('raw-csv1', '3.merged_CDI_cleaned.responses.csv'),
                     header=TRUE,
                     stringsAsFactors=FALSE)

#-------------------------------------------------#
#This page is to create columns for skills- if children understands or understands and produces for each language
#where 0 is no and 1 is yes 
################

# Define function to convert responses to binary skills for "skill_es_comprehend"
convert_response_to_skill_es_comprehend <- function(response) {
  ifelse(response %in% c("Comprende", "Comprende / Ulertzen du", "Comprende y dice", "Comprende y dice / Ulertu eta esaten du"), 1, 0)
}

# Define function to convert responses to binary skills for "skill_es_produce"
convert_response_to_skill_es_produce <- function(response) {
  ifelse(response %in% c("Comprende y dice", "Comprende y dice / Ulertu eta esaten du"), 1, 0)
}

# Define function to convert responses to binary skills for "skill_eu_comprehend"
convert_response_to_skill_eu_comprehend <- function(response) {
  ifelse(response %in% c("Ulertzen du", "Comprende / Ulertzen du", "Ulertu eta esaten du", "Comprende y dice / Ulertu eta esaten du"), 1, 0)
}

# Define function to convert responses to binary skills for "skill_eu_produce"
convert_response_to_skill_eu_produce <- function(response) {
  ifelse(response %in% c("Ulertu eta esaten du", "Comprende y dice / Ulertu eta esaten du"), 1, 0)
}

# Create binary skill columns based on "response" column
final_df$skill_es_comprehend <- convert_response_to_skill_es_comprehend(final_df$response)
final_df$skill_es_produce <- convert_response_to_skill_es_produce(final_df$response)
final_df$skill_eu_comprehend <- convert_response_to_skill_eu_comprehend(final_df$response)
final_df$skill_eu_produce <- convert_response_to_skill_eu_produce(final_df$response)

# Ensure if produce is 1, comprehend is also 1
final_df$skill_es_comprehend <- ifelse(final_df$skill_es_produce == 1, 1, final_df$skill_es_comprehend)
final_df$skill_eu_comprehend <- ifelse(final_df$skill_eu_produce == 1, 1, final_df$skill_eu_comprehend)



##__________________________
# To make sure that the survey has been fully completed 
# survey completion, 1 means it is completed

# Apply the transformation: 15 becomes 1, all others become 0
final_df$completed <- ifelse(final_df$completed == 15, 1, 0)


#save the merged skills csv

write.csv(final_df, 
          file.path('raw-csv1', '4.merged_CDI_skills.csv'),
          row.names=FALSE)

