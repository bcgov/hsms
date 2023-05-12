install.packages("readr")
install.packages("tibble")

library(readr)
library(tibble)

masterTable <- read.csv("W:/path/to/HousingSuitabilitybyTenure.csv", header=FALSE)
mainPath <- "W:/path/to/TenureTables"

#clean table
masterTable <- masterTable[-c(1:8), ]
masterTable <- masterTable[, -c(2, 5)]

num_cols <- ncol(masterTable)

for (i in 5:num_cols)
{
  masterTable[2, i] <- masterTable[1, i]
}
colnames(masterTable) <- masterTable[2, ]
masterTable <- masterTable[-c(1, 2), ]

rowStart <- 1
rowEnd <- 1

#Initialize Final Dataframe
cNames <- c("Municipality", "1 person", "2 persons", "3 persons", "4 persons", "5 or more persons",
            "Number of Persons Per Room", "Tenure", "Number of Rooms and Number of Bedrooms",
            "Data_Source", "_lastupdate")

exportCombined <- data.frame(matrix(nrow = 0, ncol = length(cNames)))
colnames(exportCombined) <- cNames

#fill in category rows
for (x in 1:nrow(masterTable))
{
  if(masterTable[x, 4] == "")
  {
    #Finds last row of data
    last_row <- x-1
    break
  }
  if(masterTable[x,1] != "")
  {
    oneName <- masterTable[x,1]
  }
  if(masterTable[x,2] != "")
  {
    twoName <- masterTable[x,2]
  }
  
  if(masterTable[x,3] != "")
  {
    threeName <- masterTable[x,3]
  }     
  
  oneName <- gsub("[0-9]", "", oneName)
  
  masterTable[x,1] <- oneName
  masterTable[x,2] <- twoName
  masterTable[x,3] <- threeName
}

#trim metadata from bottom of Table
masterTable <- masterTable[1:last_row,]

rowStart <- 1
rowEnd <- 1

#iterate down shelter column
for (a in 2:nrow(masterTable))
{
  if(masterTable[a,1] != masterTable[rowStart,1] | a == nrow(masterTable))
  {
    if(masterTable[a,1] != masterTable[rowStart,1])
    {
      rowEnd <- a-1
    }
    if(a == nrow(masterTable))
    {
      rowEnd <- a
    }
    
    #splits table into section
    shelterTable <- masterTable[rowStart:rowEnd,]
    
    shelterName <- masterTable[rowStart,1]
    
    rowStart <- a
    
    shRowStart <- 1
    
    #iterate down dwelling column
    for (b in 2:nrow(shelterTable))
    {
      
      if(shelterTable[b,2] != shelterTable[shRowStart,2] | b == nrow(shelterTable))
      {
        if(shelterTable[b,2] != shelterTable[shRowStart,2])
        {
          shRowEnd <- b-1
        }
        if(b == nrow(shelterTable))
        {
          shRowEnd <- b
        }
        
        dwellingTable <- shelterTable[shRowStart:shRowEnd,]
        dwellingName <- shelterTable[shRowStart,2]
        
        shRowStart <- b
        
        dwRowStart <- 1
            
        #iterate down core table
        for(d in 2:nrow(dwellingTable))
        {
          
          if (dwellingTable[d,3] != dwellingTable[dwRowStart,3] | d == nrow(dwellingTable))
          {
            if (dwellingTable[d,3] != dwellingTable[dwRowStart,3])
            {
              dwRowEnd <- d-1
            }
            if (d == nrow(dwellingTable))
            {
              dwRowEnd <- d
            }
            
            coTable <- dwellingTable[dwRowStart:dwRowEnd,]
            coName <- dwellingTable[dwRowStart,3]
            coTable <- coTable[, -c(1:3)]
            
            #create Table to export
            exportTable <- t(coTable) #transpose table
            exportTable <- as.data.frame(exportTable)
            colnames(exportTable) <- exportTable[1,]
            exportTable <- exportTable[-1, ]
            exportTable <- rownames_to_column(exportTable, var = "Municipality")
            
            todayStr <- Sys.Date()
            
            #coName, housingname, dwellingname, sheltername
            exportTable[, "Number of Persons Per Room"] <- shelterName
            exportTable[, "Tenure"] <- dwellingName
            exportTable[, "Number of Rooms and Number of Bedrooms"] <- housingName
            exportTable[, "Housing Suitability"] <- coName
            exportTable[, "Data_Source"] <- "StatsCan"
            exportTable[, "_lastupdate"] <- todayStr
            
            #Clean Municipality strings
            for (e in 1:nrow(exportTable))
            {
              muniStr <- exportTable[e,1]
              
              has_two_periods <- grepl("\\..*\\.", muniStr)
              
              if (has_two_periods) 
              {
                muniStr <- gsub("^([^.]+)\\.", "\\1 ", muniStr) #removes the first period and replaces with a space
                muniStr <- gsub("\\..*", "", muniStr) #removes everything after the remaining period 
              } else 
              {
                muniStr <- gsub("\\..*", "", muniStr) #removes everything after the remaining period
              }
              #removes the i**** after the muni string
              muniStr <- gsub(" i.*", "", muniStr)
              exportTable[e,1] <- muniStr
            }
            
            #append table to bottom of combined table
            exportCombined <- rbind(exportCombined, exportTable)
            
            confirmStr = paste("Created table:", coName)
            print(confirmStr)
            
            dwRowStart <- d
          }
        }
      }
    }
  }
}

colnames(exportCombined)[2:6] <- c("One person", "Two persons", "Three persons", "Four persons", "Five or more persons")

fileName <- paste(mainPath,"/","HousingSuitability.csv", sep= "")
write.csv(exportCombined, fileName, row.names = FALSE)
print("Complete")
