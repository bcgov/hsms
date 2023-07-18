#Takes CMHC data from the New Housing Construction (Scss) dataset using the cmhc wrapper api from mountainmath https://mountainmath.github.io/cmhc/index.html
#then converts the table to an annual timeframe and makes it suitable for use in GIS
#New Housing Construction Scss

#Required Packages
#---------------------------
#install.packages("cmhc")
#install.packages("tidyr")
#install.packages("xlsx")
#library(cmhc)
#library(tidyr)
#library(xlsx)
#---------------------------

#tablePath: Path to folder location for output table 
#seriesList: vector of series to query
#dimensionStr: dimension filter for building type
#muniVec: CSD IDs and Names of municipalities in a named character vector (ID = Muni Name)
#filterList: vector of what variables to pull into table

#Example
#setwd("C:/Documents/hsms_Scripts")
#tablePath <- "C:/Documents/CMHC_NH"
#prep_CMHC_New_Construction(tablePath)

prep_CMHC_New_Construction <- function(tablePath, seriesList = NULL, dimensionStr = NULL, muniVec = NULL, filterList = NULL)
{
  if (!require("cmhc"))
  {
    message("Package 'cmhc' not found. Please install it using install.packages('package1').")
    return(NULL)
  }
  if (!require("tidyr"))
  {
    message("Package 'tidyr' not found. Please install it using install.packages('package1').")
    return(NULL)
  }
  if (!require("xlsx"))
  {
    message("Package 'xlsx' not found. Please install it using install.packages('package1').")
    return(NULL)
  }
  
  #-----------------------------------------------------
  #These variables can be altered to look at different datasets within the New housing construction Data Series
  if(is.null(seriesList))
  {
    seriesList <- c("Starts", "Completions", "Under Construction", "Length of Construction", "Absorbed Units", "Share absorbed at completion")
  }
  if(is.null(dimensionStr))
  {
    dimensionStr <- "Dwelling Type"
  }
  if(is.null(muniVec))
  {
    muniVec <- c("59" = "British Columbia", "5909052" = "Abbotsford",
                 "5915011" = "Delta", "5915022" = "Vancouver",
                 "5915043" = "Port Moody", "5915046" = "North Vancouver - District",
                 "5915055" = "West Vancouver", "5917021" = "Saanich",
                 "5917030" = "Oak Bay", "5917034" = "Victoria",
                 "5933042" = "Kamloops")
  }
  if(is.null(filterList))
  {
    filterList <- c("Homeowner", "Rental", "Condo", "Co-op", "All")
  }
  #-----------------------------------------------------
  tryCatch(
    source("utils.R"),
    
    error = function(e)
    {
      print("utils.R not found, Make sure to set your working directory to the folder containing utils.R")
      stop()
    }
  )
  surveyStr <- "Scss" #New Housing Construction dataset
  breakdownStr <- "Historical Time Periods"
  categoryStr <- "New Housing Construction"
  updateStr <- Sys.Date()
  updateStr <- format(updateStr, "%Y-%m-%d")
  muniIndex <- 0
  
  #create Table for Export
  exportTablecols <- c("Classification", "Municipality", "Date_Range", "Single", "Semi_Detached", "Row",
                       "Apartment", "All", "Category", "Intended_Markets", "Data_Source", "_lastupdate")
  combined_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(exportTablecols)))
  colnames(combined_R_CMHC) <- exportTablecols
  
  #Loop Through Municipality
  for (a in muniVec)
  {
    muniIndex <- muniIndex + 1
    muniID <- as.numeric(names(muniVec)[muniIndex])
    muniName <- muniVec[muniIndex]
    
    #iterate through series
    for (b in seriesList)
    {
      seriesStr <- b
      
      #iterate through filters
      for (c in filterList)
      {
        filterStr <- c
        
        printStr <- paste("Downloading Table:", muniName, seriesStr, filterStr)
        print(printStr)
        #Downloads Table from CMHC
        currentTable <- get_cmhc(survey = surveyStr, series = seriesStr, dimension = dimensionStr, breakdown = breakdownStr, geo_uid = muniID, filters = list("dimension-18" = filterStr),frequency = "Annual")
        
        singleValueTot <- 0
        semiValueTot <- 0
        rowValueTot <- 0
        aptValueTot <- 0
        allValueTot <- 0
        
        for (e in 1:nrow(currentTable))
        {
          #get dateString year value
          dateStr <- currentTable$DateString[e]
          dateNum <- as.numeric(dateStr)
          
          if (dateNum >= 2012 & dateNum < 2023)
          {
            currentDwell <- as.character(currentTable$`Dwelling Type`[e])
            currentVal <- currentTable$Value[e]
            currentNA <- FALSE
            
            if (currentDwell == "Single")
            {
              singleValueTot <- currentVal
            } else if (currentDwell == "Semi-Detached")
            {
              semiValueTot <- currentVal
            } else if (currentDwell == "Row")
            {
              rowValueTot <- currentVal
            } else if (currentDwell == "Apartment")
            {
              aptValueTot <- currentVal
            } else if (currentDwell == "All")
            {
              allValueTot <- currentVal
              
              #append values onto the final export table
              newRow <- c(seriesStr,muniName,dateNum,singleValueTot,semiValueTot,rowValueTot,aptValueTot,allValueTot,categoryStr, filterStr, "CMHC", updateStr)
              
              combined_R_CMHC[nrow(combined_R_CMHC) + 1, ] <- newRow
              
              singleValueTot <- 0
              semiValueTot <- 0
              rowValueTot <- 0
              aptValueTot <- 0
              allValueTot <- 0
              
            } 
          }
        }
      }
    }
  }
  combined_R_CMHC$`Year_Tenure` <- "."
  
  for (yt in 1:nrow(combined_R_CMHC))
  {
    add_string <- paste(combined_R_CMHC$Date_Range[yt], combined_R_CMHC$Intended_Markets[yt])
    combined_R_CMHC$`Year_Tenure`[yt] <- add_string
  }
  
  combined_R_CMHC <- fix_stats_vals(combined_R_CMHC)
  
  fileName <- file.path(tablePath,"CMHC_NH.xlsx")
  write.xlsx(combined_R_CMHC, fileName, row.names = FALSE)
  print("Process Complete")
}
