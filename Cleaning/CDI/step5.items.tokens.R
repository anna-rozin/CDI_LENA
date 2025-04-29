# Clear previous data
rm(list = ls())

#Load data of merged CDI skills
final_df <- read.csv(file.path('raw-csv1', '4.merged_CDI_skills.csv'),
                     header=TRUE,
                     stringsAsFactors=FALSE)

#-------------------------------------------------------------#
#This page is to fix up and create items and tokens from the CDI 

### adding a new column 'part' where 1 is first part and 2 is second part####
# Extract the digits after ".." and store them in the "part" column
# Extract the digit after ".." and assign to 'part' column
final_df$part <- ifelse(grepl("\\.\\.(\\d)$", final_df$part_code), sub(".*\\.\\.(\\d)$", "\\1", final_df$part_code), NA)

# Remove the "..(digit)" pattern from 'part_code' column
final_df$part_code <- sub("\\.\\.(\\d)$", "", final_df$part_code)

#renaming more columns 
colnames(final_df)[colnames(final_df) == "part_code"] <- "item_code"
colnames(final_df)[colnames(final_df) == "item"] <- "part_code"

# Removing "." from part_code
final_df$part_code <- gsub("\\.", "", final_df$part_code)

# creating the names to put into the variable part ----------------------------

# Define the mapping between short and long versions of categories
part_mapping <- list(
  "VOCPRE0CAS" = "Vocalizaciones prelingüísticas",
  "COMTEM1CAS" = "Comprensión temprana",
  "COMGLOFRA1CAS" = "Comprensión global de frases",
  "PROTEM3CAS" = "Producción temprana",
  "VOC1HYBRID" = "Vocabulario 1 - Interjecciones y sonidos de animales",
  "VOC2BIL" = "Vocabulario 2 - Juegos, rutinas y fórmulas sociales",
  "VOC3BIL" = "Vocabulario 3 - Animales de verdad o de juguete",
  "VOC4BIL" = "Vocabulario 4 - Personas",
  "VOC5BIL" = "Vocabulario 5 - Partes del cuerpo",
  "VOC6BIL" = "Vocabulario 6 - Juguetes",
  "VOC7BIL" = "Vocabulario 7 - Vehículos, de verdad o de juguete",
  "VOC8BIL" = "Vocabulario 8 - Alimentos y bebidas",
  "VOC9BIL" = "Vocabulario 9 - Ropa",
  "VOC10BIL" = "Vocabulario 10 - Objetos y lugares de la casa",
  "VOC11BIL" = "Vocabulario 11 - Objetos y lugares fuera de la casa",
  "VOC12BIL" = "Vocabulario 12 - Acciones",
  "VOC13BIL" = "Vocabulario 13 - Cualidades",
  "VOC14BIL" = "Vocabulario 14 - Tiempo",
  "VOC15BIL" = "Vocabulario 15 - Pronombres y determinantes",
  "VOC16BIL" = "Vocabulario 16 - Preguntas",
  "VOC17BIL" = "Vocabulario 17 - Preposiciones y locativos",
  "VOC18BIL" = "Vocabulario 18 - Cuantificadores y artículos",
  "VOC19BIL" = "Vocabulario 19 - Auxiliares, perífrasis y conectivas",
  "GES2CAS1" = "Gestos",
  "JUE2CAS3" = "Juegos",
  "ACC2CAS3" = "Acciones",
  "JUESIM1CAS" = "Juego simbólico",
  "VOCPREEUS" = "Vocalizaciones prelingüísticas",
  "COMTEM1EUS" = "Comprensión temprana",
  "COMGLOFRA1EUS" = "Comprensión global de frases",
  "PROTEM3EUS" = "Producción temprana",
  "GES2EUS1" = "Gestos",
  "JUE2EUS1" = "Juegos",
  "ACC2EUS3" = "Acciones",
  "JUESIM1EUS" = "Juego simbólico"
)

# Update "part" column based on "part_code"
final_df$part <- part_mapping[final_df$part_code]

### making time-points in "token"-------------

# Update the "token" column by extracting the number after the underscore
final_df$token <- sapply(final_df$token, function(token) {
  # Extract numeric value using regular expression
  numeric_value <- sub(".*_(\\d+).*", "\\1", token)
  return(numeric_value)
})

# Convert the result to character vector
final_df$token <- as.character(final_df$token)



### Creating word part -------------------

#load the word matching data
word_name <- read.csv(file.path('source','CDI', 'word_code_CDI.csv'),
                      header=TRUE,
                      stringsAsFactors=FALSE)


# Merge final_df with word_name for S_word
merged_S_word <- merge(final_df, word_name, by.x = "item_code", by.y = "Bil_code", all.x = TRUE)


# Merge final_df with word_name for B_word
merged_B_word <- merge(final_df, word_name, by.x = "item_code", by.y = "Bil_code", all.x = TRUE)



##---------------------------------------------
# Define the desired order of columns
desired_order <- c("ID", "ID_lab", "date", "completed", "language", "token", "part_code", "part", 
                   "item_code", "BQ_word", "SP_word", "response", "lang", "skill_es_comprehend", "skill_es_produce",
                   "skill_eu_comprehend", "skill_eu_produce")

# Reorder the columns in final_df
merged_B_word <- merged_B_word[, desired_order]

# Convert the "response" column from list to character vector
merged_B_word$response <- sapply(merged_B_word$response, paste, collapse = ", ")

# Convert the "part" column from list to character vector
merged_B_word$part <- sapply(merged_B_word$part, function(x) paste(x, collapse = ", "))


#save as the CSV final, everything as been cleaned --------------

write.csv(merged_B_word, 
          file.path('raw-csv1', 'Summary', '5.merged_CDI_final.csv'),
          row.names=FALSE)
