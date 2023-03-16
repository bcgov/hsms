install.packages("readr")
install.packages('tibble')

library(readr)
library(tibble)

masterTable <- read.csv("W:/Path/to/HousingSuitabilitybyTenure.csv")
mainPath <- "W:/Path/to/output/folder"

masterTable <- masterTable[,-2]

rowStart <- 1
rowEnd <- 1

#Initialize Final Dataframe
cNames <- c("Municipality", "1 person", "2 persons", "3 persons", "4 persons", "5 or more persons",
            "Number of Persons Per Room", "Tenure", "Number of Rooms and Number of Bedrooms" , "Housing Suitability",
            "Data_Source", "_lastupdate")

exportCombined <- data.frame(matrix(nrow = 0, ncol = length(cNames)))
colnames(exportCombined) <- cNames

#fill in category rows
for (x in 1:nrow(masterTable))
{
  
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
  
  if (masterTable[x,4] != "")
  {
    fourName <- masterTable[x,4]
  }
  
  oneName <- gsub("[0-9]", "", oneName)
  
  masterTable[x,1] <- oneName
  masterTable[x,2] <- twoName
  masterTable[x,3] <- threeName
  masterTable[x,4] <- fourName
}


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
    #create folder
    shelterName <- masterTable[rowStart,1]
    #shelterPath <- paste(mainPath,"/", shelterName, sep = "")
    rowStart <- a
    
    #dir.create(shelterPath)
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
        #dwellingPath <- paste(shelterPath,"/", dwellingName, sep = "")
        shRowStart <- b
        
        #dir.create(dwellingPath)
        dwRowStart <- 1
        #iterate down housing column
        for (c in 2:nrow(dwellingTable))
        {
          
          if(dwellingTable[c,3] != dwellingTable[dwRowStart,3] | c == nrow(dwellingTable))
          {
            if(dwellingTable[c,3] != dwellingTable[dwRowStart,3])
            {
              dwRowEnd <- c-1
            }
            if(c == nrow(dwellingTable))
            {
              dwRowEnd <- c
            }
            
            housingTable <- dwellingTable[dwRowStart:dwRowEnd,]
            housingName <- dwellingTable[dwRowStart,3]
            #housingPath <- paste(dwellingPath,"/", housingName, sep = "")
            dwRowStart <- c
            
            #dir.create(housingPath)
            hoRowStart <- 1
            
            #iterate down core table
            for(d in 2:nrow(housingTable))
            {
              
              if (housingTable[d,4] != housingTable[hoRowStart,4] | d == nrow(housingTable))
              {
                if (housingTable[d,4] != housingTable[hoRowStart,4])
                {
                  hoRowEnd <- d-1
                }
                if (d == nrow(housingTable))
                {
                  hoRowEnd <- d
                }
                
                coTable <- housingTable[hoRowStart:hoRowEnd,]
                coName <- housingTable[hoRowStart,4]
                #coFile <- paste(housingPath,"/",coName,".csv", sep = "")
                coTable <- coTable[, -c(1:4)]
                
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
                  
                  exportTable[e,1] <- muniStr
                }
                
                #append table to bottom of combined table
                exportCombined <- rbind(exportCombined, exportTable)
                
                #export table to folder
                #write.csv(exportTable, coFile, row.names = FALSE)
                
                confirmStr = paste("Created table:", coName)
                print(confirmStr)
                
                hoRowStart <- d
              }
            }
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
