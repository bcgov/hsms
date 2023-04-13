
#Prepare StatsCan Tables for Spatializing
install.packages("readr")
install.packages("dplyr")
install.packages("tibble")
install.packages("xlsx")

library(readr)
library(dplyr)
library(tibble)
library(xlsx)


#Read in table
censusTable_a <- read.csv("W:/Path/to/CensusProfile2021_A.csv", header = FALSE)
censusTable_b <- read.csv("W:/Path/to/CensusProfile2021_B.csv", header = FALSE)

outPath <- "W:/Path/to/CensusTables/"

input_table_list <- list(censusTable_a,censusTable_b)
#check if all tables have the same amount of rows

check_row <- nrow(input_table_list[[1]])
run_script <- TRUE

for (table in input_table_list)
{
  rows <- nrow(table)
  if (rows != check_row)
  {
    run_script <- FALSE
  }
}

if(run_script)
{
  #columns that contain no/ unneeded variables
  flag_indexes <- c(3, 5, 7, 9, 11, 13, 14)
  
  count <- 1
  #Clean Tables
  for (input_table in input_table_list)
  {
    #Remove unneeded rows
    input_table <- input_table[-c(1,3),]
    #Remove total_flag columns
    input_table <- input_table[,-flag_indexes]
    #move down muni's, remove row 1, row 1 to colnames
    num_cols <- ncol(input_table)
    
    for (i in 3:num_cols)
    {
      input_table[2,i] <- input_table[1,i]
      muniStr <- strsplit(input_table[2,i],",")
      suppressWarnings(input_table[2,i] <- muniStr[1])
    }
    colnames(input_table) <- input_table[2,]
    input_table <- input_table[-c(1,2),]
    
    num_row <- nrow(input_table)
    
    rInd <- 1
    #remove bottom metadata
    
    while (input_table[rInd,2] != "")
    {
      rInd <- rInd + 1
      
      if (rInd > num_row)
      {
        break
      }
    }
    
    input_table <- input_table[-c(rInd:num_row),]
    
    #combine tables
    if(count == 1)
    {
      censusTable <- cbind(input_table[,c(1,2)])
    }
    censusTable <- cbind(censusTable,input_table[,c(3:num_cols)])
    
    count <- count + 1
  }
  
  #Split tables by Topic
  censusTableList <- split(censusTable, f = censusTable$Topic)
  
  table_num <- length(censusTableList)
  
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
    filePath <- paste(outPath, elementStr,".xlsx", sep="")
    
    #remove Topic column
    listItem <- subset(listItem, select = -Topic)
    
    #rotates table
    trItem <- t(listItem)
    trItem <- as.data.frame(trItem)
    
    colnames(trItem) <- trItem[1,] #sets first row values to Field names
    trItem <- trItem[-c(1),] #remove row 1
    
    #add rownames to a column named Municipality
    newItem <- trItem
    newItem <- cbind(Municipality = rownames(trItem), trItem)
    trItem <- newItem
    
    today <- Sys.Date()
    trItem[, "data_source"] <- "StatsCan"
    trItem[, "_lastupdate"] <- today
    
    #remove "." and replace with " " in City Column
    for (j in 1:nrow(trItem))
    {
      trItem[j,1] <- gsub(".", " ", trItem[j,1], fixed = TRUE)
    }
    
    #writes to csv file
    write.xlsx(trItem, filePath, row.names = FALSE)
    print(paste("Created File", num, "of", table_num, ":", elementStr))
    
    num <- 1 + num
  }
} else
{
  print("Error Tables do not Match")
}