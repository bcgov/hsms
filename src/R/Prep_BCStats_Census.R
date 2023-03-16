#Prepare StatsCan Tables for Spatializing
install.packages('readr')
install.packages('dplyr')
install.packages('tibble')

library(readr)
library(dplyr)
library(tibble)

#Read in table
censusTable <- read.csv("W:/Path/to/CensusProfile_Combined.csv")



#Split tables by Topic
censusTableList <- split(censusTable, f = censusTable$Topic)

num <- 1

#Iterate through List of tables
for (i in censusTableList) 
{
 
  elementStr <- names(censusTableList)[num] #use to create name of file
  
  #clean up strings
  elementStr <- gsub(" ", "", elementStr) 
  elementStr <- gsub("[()]", "_", elementStr)
  elementStr <- gsub("-", "_", elementStr)
      
  
  listItem <- censusTableList[[num]] #output dataframe for csv export
  filePath <- paste("W:/Path/to/CensusTables/", elementStr,".csv", sep="")
  
  #remove Topic column
  listItem <- subset(listItem, select = -Topic)
  
  #rotates table
  trItem <- t(listItem)
  trItem <- as.data.frame(trItem)
  
  colnames(trItem) <- trItem[1,] #sets first row values to Field names
  trItem <- trItem[-c(1),] #remove row 1
  trItem <- rownames_to_column(trItem, var = "Municipality")
  
  today <- Sys.Date()
  trItem[, "data_source"] <- "StatsCan"
  trItem[, "_lastupdate"] <- today
  
  #remove "." and replace with " " in City Column
  for (j in 1:nrow(trItem))
  {
    trItem[j,1] <- gsub(".", " ", trItem[j,1], fixed = TRUE)
  }
  
  #writes to csv file
  write.csv(trItem, filePath, row.names = FALSE)
  print(paste("Created File:", elementStr))
  
  num <- 1 + num
}




