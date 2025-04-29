####### This is to merge all of my summary CDIs

#### Load them 

# Clear previous data
rm(list = ls())

B_CDI_summary <- read.csv(file.path('raw-csv','Summary', '6.B_CDI_Summary.csv'), header=TRUE, stringsAsFactors=FALSE)
B2_CDI_summary <- read.csv(file.path('raw-csv','Summary','6.B2_CDI_Summary.csv'), header=TRUE, stringsAsFactors=FALSE)
S_CDI_summary <- read.csv(file.path('raw-csv','Summary','6.S_CDI_Summary.csv'), header=TRUE, stringsAsFactors=FALSE)
S2_CDI_summary <- read.csv(file.path('raw-csv','Summary','6.S2_CDI_Summary.csv'), header=TRUE, stringsAsFactors=FALSE)

#Merge all parts of summary CDI
merged_CDIs <- rbind(B_CDI_summary, B2_CDI_summary, S_CDI_summary, S2_CDI_summary)

#save as CSV
write.csv(merged_CDIs, 
          file.path('raw-csv', 'Summary', 'merged_CDIs_summary.csv'),
          row.names=FALSE)
