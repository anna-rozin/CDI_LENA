##------------------------------------------------------------##
### Analysis script for CDI data ###

##load packages
library(readxl) 
library(plyr)
require(dplyr)
require(ggbeeswarm)
require(FSA)
require(ggplot2)
require(reshape)
require(tidyr)
library(magrittr)
library(knitr)
require(lsr)
require(pwr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(lme4)
library(lmerTest)
library(car)
library(MuMIn)
library(sjPlot)
library(sjstats)
library(emmeans) 
library(multcomp)
library(afex)
require(parallel)

##------------------------------------------------------------##

### Set working directory and load data, each part of script is done separately for now

# Clear previous data
rm(list = ls())

# load Spanish data frames --------------------------------------

dat_S1 <- read.csv(file.path('source', 'CDI', 'Spanish', 'S1.csv'),
                header=TRUE,
                stringsAsFactors=FALSE)


dat_S2 <- read.csv(file.path('source', 'CDI', 'Spanish', 'S2.csv'),
                   header=TRUE,
                   stringsAsFactors=FALSE)


# load Basque data frames ----------------------------------------
dat_B1 <- read.csv(file.path('source', 'CDI', 'Basque', 'B1.csv'),
                   header=TRUE,
                   stringsAsFactors=FALSE)


dat_B2 <- read.csv(file.path('source', 'CDI', 'Basque', 'B2.csv'),
                   header=TRUE,
                   stringsAsFactors=FALSE)


# This page is to clean up the data 
##------------------------------------------------------------##
#### for Spanish CDI 

clean_data_S <- function(dat) {
##### Renaming the columns ######
colnames(dat)[colnames(dat) == "startlanguage..Lenguaje.inicial"] <- "language"
colnames(dat)[colnames(dat) == "lastpage..Última.página"] <- "completed"
colnames(dat)[colnames(dat) == "IDENTIFICACION1..ID."] <- "ID_lab"
colnames(dat)[colnames(dat) == "token..Contraseña"] <- "token"
colnames(dat)[colnames(dat) == "id..ID.de.respuesta"] <- "ID"
colnames(dat)[colnames(dat) == "IDENTIFICACION2..Fecha.de.hoy"] <- "date"


##### Removing unnecessary columns #######
dat <- dat[, !colnames(dat) %in% "submitdate..Fecha.de.envío"]

# Identify columns containing "Time" in their names
time_columns <- grep("Time", colnames(dat))
dat <- dat[, -time_columns, drop = FALSE]
dat <- dat[, !colnames(dat) %in% "interviewtime..Tiempo.total"]
dat <- dat[, !colnames(dat) %in% "firstname..Nombre.s"]
dat <- dat[, !colnames(dat) %in% "ID.1"]

return(dat)
}

# Apply function only to S datasets
dat_S1 <- clean_data_S(dat_S1)
dat_S2 <- clean_data_S(dat_S2)

##--------------------------------------------------------------##
###### for basque CDI 

clean_data_B <- function(dat) {
  
##### Renaming the columns ######
colnames(dat)[colnames(dat) == "startlanguage..Start.language"] <- "language"
colnames(dat)[colnames(dat) == "lastpage..Last.page"] <- "completed"
colnames(dat)[colnames(dat) == "ID1..ID."] <- "ID_lab"
colnames(dat)[colnames(dat) == "token..Pasahitza"] <- "token"
colnames(dat)[colnames(dat) == "id..Response.ID"] <- "ID"
colnames(dat)[colnames(dat) == "ID2..Gaurko.data"] <- "date"


##### Removing unnecessary columns #######
dat <- dat[, !colnames(dat) %in% "submitdate..Date.submitted"]
# Identify columns containing "Time" in their names
time_columns <- grep("Time", colnames(dat))
#remove
dat <- dat[, -time_columns, drop = FALSE]
dat <- dat[, !colnames(dat) %in% "interviewtime..Total.time"]
dat <- dat[, !colnames(dat) %in% "firstname..First.name"]

return(dat)
}

# Apply function only to B datasets
dat_B1 <- clean_data_B(dat_B1)
dat_B2 <- clean_data_B(dat_B2)

##--------------------------------------------------------------##
# save the new Spanish and Basque CDI 
# save as raw before we move onto transposing 
# change the saved name so it coordinates the survey part

write.csv(dat_S1, 
          file.path('raw-csv1', '1.S1_CDI_nontransposed.csv'),
          row.names=FALSE)
write.csv(dat_S2, 
          file.path('raw-csv1', '1.S2_CDI_nontransposed.csv'),
          row.names=FALSE)
write.csv(dat_B1, 
          file.path('raw-csv1', '1.B1_CDI_nontransposed.csv'),
          row.names=FALSE)
write.csv(dat_B2, 
          file.path('raw-csv1', '1.B2_CDI_nontransposed.csv'),
          row.names=FALSE)
