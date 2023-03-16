#New Housing Construction Scss
#Loop through Scss

install.packages("cmhc")
install.packages("tidyr")
library(cmhc)
library(tidyr)

surveyStr <- "Scss"
seriesList <- c("Starts", "Completions", "Under Construction", "Length of Construction", "Absorbed Units", "Share absorbed at completion")
dimensionStr <- "Dwelling Type"
breakdownStr <- "Historical Time Periods"
muniVec <- c("5921007"="Nanaimo", "5917030"="Oak Bay", "5917034"="Victoria", "5915004"="Surrey", "5915022"="Vancouver", "5915025"="Burnaby", "5915034"="Coquitlam", "5915043"="Port Moody", "5915055"="West Vancouver", "5915075"="Maple Ridge")
filterList <- c("Homeowner","Rental","Condo","Co-op","All")
categoryStr <- "New Housing Construction"
updateStr <- Sys.Date()
updateStr <- format(updateStr, "%Y-%m-%d")
muniIndex <- 0

#create Table for Export
exportTablecols <- c("Classification","Municipality","Date_Range","Single","Semi_Detached","Row","Apartment","All","Category","Intended_Markets","Data_Source", "_lastupdate")
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
      currentTable <- get_cmhc(survey = surveyStr, series = seriesStr, dimension = dimensionStr, breakdown = breakdownStr, geo_uid = muniID, filters = list("dimension-18" = filterStr))
      
      singleValueTot <- 0
      semiValueTot <- 0
      rowValueTot <- 0
      aptValueTot <- 0
      allValueTot <- 0
      currentYear <- "Jan 2012"
      
      for (e in 1:nrow(currentTable))
      {
        #get dateString year value
        dateStr <- currentTable$DateString[e]
        dateV <- strsplit(dateStr, split = ' ')
        dateNum <- as.numeric(dateV[[1]][2])
        dateMonth <- dateV[[1]][1]
        
        #if year >= 2012
        if (dateNum >= 2012 & dateNum < 2023)
        {
          #print(paste("DateNum",dateNum,", currentYear",currentYear,", currentVal", currentVal))
          currentDwell <- as.character(currentTable$`Dwelling Type`[e])
          currentVal <- currentTable$Value[e]
          if (currentDwell == "Single")
          {
            singleValueTot <- singleValueTot + currentVal
      
          } else if (currentDwell == "Semi-Detached")
          {
            semiValueTot <- semiValueTot + currentVal
            
          } else if (currentDwell == "Row")
          {
            rowValueTot <- rowValueTot + currentVal
            
          } else if (currentDwell == "Apartment")
          {
            aptValueTot <- aptValueTot + currentVal
            
          } else if (currentDwell == "All")
          {
            allValueTot <- allValueTot + currentVal
            
          }
          
          if (dateMonth == "Dec" & currentDwell == "All")
          {
            
            #append values onto the final export table
            newRow <- c(seriesStr,muniName,dateNum,singleValueTot,semiValueTot,rowValueTot,aptValueTot,allValueTot,categoryStr, filterStr, "CMHC", updateStr)
            
            combined_R_CMHC[nrow(combined_R_CMHC)+1,] <- newRow
            
            singleValueTot <- 0
            semiValueTot <- 0
            rowValueTot <- 0
            aptValueTot <- 0
            allValueTot <- 0
            currentYear <- dateNum
            
          } 
        }
      }
    }
  }
}

fileName <- "W:/Path/to/CMHC_NHC.csv"
write.csv(combined_R_CMHC,fileName,row.names = FALSE)
print("Process Complete")
