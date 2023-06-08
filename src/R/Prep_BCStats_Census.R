#Prepare Census Profile Tables from Statistics Canada for use in GIS

#Required Packages
#-------------------------
#install.packages("readr")
#install.packages("dplyr")
#install.packages("tibble")
#install.packages("xlsx")

#library(readr)
#library(dplyr)
#library(tibble)
#library(xlsx)
#-------------------------

#input_path: folder path to raw imported csvs from Statistics Canada
#out_path: folder path for output Census tables, folder will be created if it does not exsist 

#Example
#setwd("C:/Documents")
#input_path <- "C:/Documents/Census imports"
#out_path <- "C:/Documents/Census outputs"
#prep_BCStats_Census(input_path, out_path)

prep_BCStats_Census <- function(input_path, out_path)
{
  #checks for required packages
  if (!require("readr"))
  {
    message("Package 'readr' not found. Please install it using install.packages('package1').")
    return(NULL)
  }
  if (!require("dplyr"))
  {
    message("Package 'dplyr' not found. Please install it using install.packages('package1').")
    return(NULL)
  }
  if (!require("tibble"))
  {
    message("Package 'tibble' not found. Please install it using install.packages('package1').")
    return(NULL)
  }
  if (!require("xlsx"))
  {
    message("Package 'xlsx' not found. Please install it using install.packages('package1').")
    return(NULL)
  }
  #links to script containing functions
  tryCatch(
  source("utils.R"),
  
  error = function(e)
  {
    print("utils.R not found, Make sure to set your working directory to the folder containing utils.R")
    stop()
  }
  )
  
  Topic <- NULL
  
  if (!dir.exists(out_path)) {
    dir.create(out_path, recursive = TRUE)
    message("Output folder created at: ", out_path)
  }
  
  # List all CSV files in the folder
  input_files <- list.files(input_path, pattern = ".csv$", full.names = TRUE)
  
  # Convert input files to data frames
  input_table_list <- lapply(input_files, read.csv, header = FALSE)
  
  #check if all tables have the same amount of rows
  check_row <- nrow(input_table_list[[1]])
  
  #check if tables have same amount of rows
  #find a new way to do this
  run_script <- TRUE
  
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
        if(!grepl(",", input_table[2,i]))
        {
          muniStr <- gsub("\\s*\\[Province\\]", "", input_table[2,i])
        } else
        {
          muniStr <- strsplit(input_table[2,i],",")
        }
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
     
      
      censusTable <- cbind(censusTable,input_table[,c(3:num_cols), drop = FALSE])
      
      
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
      fileName <- paste0(elementStr, ".xlsx", sep="")
      filePath <- file.path(out_path, fileName)
      
      #remove Topic column
      listItem <- subset(listItem, select = -Topic)
      
      #add row Date_Range and Tenure
      listItem <- rbind("Total", listItem)
      listItem <- rbind(2021, listItem)
      
      #rotates table
      trItem <- t(listItem)
      trItem <- as.data.frame(trItem)
      
      colnames(trItem) <- trItem[1,] #sets first row values to Field names
      trItem <- trItem[-c(1),] #remove row 1
      colnames(trItem)[1] <- "Date_Range"
      colnames(trItem)[2] <- "Tenure"
      
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
      
      trItem <- fix_stats_vals(trItem)
      
      #create 25-64 column 10 to 17
      #15-19
      if(elementStr == "Agecharacteristics")
      {
        popCol <- trItem[,4, drop=FALSE]
        #create column after col 17
        temp_Table_A <- trItem[,1:32]
        temp_Table_B <- trItem[,33:ncol(trItem)]
        
        temp_Table_A$`15 to 19 years.1` <- 0 #31
        temp_Table_A$`20 to 24 years.1` <- 0 #32
        temp_Table_A$`25 to 64 years.1` <- 0 #33
        temp_Table_A$`65 to 84 years.1` <- 0 #34
  
        trItem <- cbind(temp_Table_A,temp_Table_B)
        
        for (r in 1:nrow(trItem))
        {
          twSum <- 0
          sixSum <- 0
          for (c in 12:19)
          {
            twSum <- twSum + as.numeric(trItem[r, c])
          }
          for (c2 in 21:24)
          {
            sixSum <- sixSum + as.numeric(trItem[r,c2])
          }
          totalVal <- as.numeric(trItem[r, 4])
  
          trItem[r, 33] <- get_dist_val(trItem[r, 10], totalVal)
          trItem[r, 34] <- get_dist_val(trItem[r, 11], totalVal)
          trItem[r, 35] <- get_dist_val(twSum, totalVal)
          trItem[r, 36] <- get_dist_val(sixSum, totalVal)
  
        }
        
      }
      
      if (elementStr == "Householdcharacteristics")
      {
        #add population to householdcharacteristics
        trItem$`Population-Total` <- 0
        
        for (rw in 1:nrow(trItem))
        {
          trItem$`Population-Total`[rw] <-popCol[rw,1]
        }
        
      }
      
      if (elementStr == "Incomeofhouseholdsin2020")
      {
        trItem <- create_income_brackets(trItem, 0, 19999)
        trItem <- create_income_brackets(trItem, 20000, 39999)
        trItem <- create_income_brackets(trItem, 40000, 59999)
        trItem <- create_income_brackets(trItem, 60000, 79999)
        trItem <- create_income_brackets(trItem, 80000, 99999)
      }
      
      trItem$Date_Range <- as.numeric(trItem$Date_Range)
      
      #writes to csv file
      write.xlsx(trItem, filePath, row.names = FALSE)
      print(paste("Created File", num, "of", table_num, ":", elementStr))
      
      num <- 1 + num
    }
  } else
  {
    print("Error Tables do not Match")
  }
  
  print("Process Complete")
}
