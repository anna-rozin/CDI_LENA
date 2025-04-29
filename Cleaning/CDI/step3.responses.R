##------------------------------------------------------------##
#This page is to clean the responses of the data

# Load data of the transposed merged CDI
final_df <- read.csv(file.path('raw-csv1', '2.merged_CDI_transposed.csv'),
                     header=TRUE,
                     stringsAsFactors=FALSE)

### cleaning the responses ### 
### This is done in two ways because of the responses 

#load -----
library(dplyr)
library(stringr)

#----- cleaning up the item column -------------

# Clean the 'item' column using sub
final_df$item <- sub("\\.{3}.*", "", final_df$item)

# Print the first few rows of the 'item' column to verify the cleaning
head(final_df$item)

# Example responses
responses <- c(
  "VOCPRE0CAS.L0C1...Por.favor..marque.una.de.las.opciones.siguientes...",
  "COMTEM1CAS.L1C1...Antes.de.que.un.ni√±o.o.una.ni√±a.empiece.a.decir.sus.primeras.palabras..parece.entender.el.lenguaje..Lo.sabemos.porque.responde.a.palabras.y.frases...",
  "VOC10BIL.L12B97..2...CASTELLANO.Si.su.beb√©.no.entiende.ni.dice.alguna.de.las.palabras.de.la.lista..deje.esa.pregunta.en.blanco...",
  "VOC2BIL.L4B1..1...CASTELLANO.Si.su.beb√©.no.entiende.ni.dice.alguna.de.las.palabras.de.la.lista..deje.esa.pregunta.en.blanco...EUSKERA.Zure.haurtxoak.ez.badu.zerrendako.hitzen.bat.ulertzen.edo.esaten..ez.ezazu.galdera.erantzun...a.ba√±arbainatzera.plisti.plasta.egitera..E"
)


# Example data
dat <- data.frame(
  item = c("VOCPRE0CAS.L0C11", "VOCPRE0CAS.L0C6", "COMGLOFRA1CAS.L2C3", "PROTEM3CAS.L3C2", "COMGLOFRA1CAS.L2C32", "VOC2BIL.L4B4..2", "VOC2BIL.L4B19..1")
)


# Extract part codes from 'item' column and remove them (no need)
final_df <- final_df %>%
  mutate(
    part_code = str_extract(item, "L[0-9].*"),  # Extract part code
    item = str_replace(item, "L[0-9].*", "")   # Remove part code from 'item' column
  )


# Rename the 'value' column to 'response'
colnames(final_df)[colnames(final_df) == "value"] <- "response"


#creating a new "lang" column to explain if the answer is for EU or ES words #-------
# Define function to convert responses to languages for "lang" column
convert_response_to_lang <- function(response) {
  ifelse(response %in% c("Comprende", "Comprende y dice"), "es",
         ifelse(response %in% c("Ulertzen du", "Ulertu eta esaten du"), "eu",
                ifelse(response %in% c("Comprende / Ulertzen du", "Comprende y dice / Ulertu eta esaten du"), "eu/es", "")))
}

# Create "lang" column based on "response" column
final_df$lang <- convert_response_to_lang(final_df$response)


#save the merged csv as cleaned.responses-----------

write.csv(final_df, 
          file.path('raw-csv1', '3.merged_CDI_cleaned.responses.csv'),
          row.names=FALSE)


