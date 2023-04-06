#New Housing Construction Scss
#Loop through Scss

install.packages("cmhc")
install.packages("tidyr")
install.packages("xlsx")
library(cmhc)
library(tidyr)
library(xlsx)

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
            
            combined_R_CMHC[nrow(combined_R_CMHC)+1,] <- newRow
            
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

#convert cols to numeric 3:8
for (ind in 3:8)
{
  for(r_comma in nrow(combined_R_CMHC))
  {
    #removes thousand separator comma's 
    x <- combined_R_CMHC[r_comma,ind]
    x <- gsub(",", "",x)
    combined_R_CMHC[r_comma,ind] <- x
  }
  combined_R_CMHC[,ind] <- as.numeric(combined_R_CMHC[,ind])
}

fileName <- "W:/Path/for/CMHC_NH.xlsx"
write.xlsx(combined_R_CMHC,fileName,row.names = FALSE)
print("Process Complete")
