install.packages("readr")
install.packages('tibble')

library(readr)
library(tibble)

masterTable <- read.csv("W:/Path/to/StructuralTypeDwellingbyTenure.csv")
mainPath <- "W:/Path/to/output/folder"

masterTable <- masterTable[,-3]

rowStart <- 1
rowEnd <- 1

#Initialize Final Dataframe
cNames <- c("Municipality", "Owner", "Rental", "Dwelling provided by the local government, First Nation or Indian band",
            "Household Size", "Number of Bedrooms", "Structural Type of Dwelling", "Data_Source", "_lastupdate")

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
  for (y in 5:ncol(masterTable))
  {
    if(masterTable[x,y] == "..." | masterTable[x,y] == "x")
    {
      #replace with 00
      masterTable[x,y] <- -1
    }
  }
  
  threeName <- gsub("[0-9]", "", threeName)
  
  masterTable[x,1] <- oneName
  masterTable[x,2] <- twoName
  masterTable[x,3] <- threeName
}
print("Prepared Table")

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
            
        #iterate down core table
        for(c in 2:nrow(dwellingTable))
        {
          
          if (dwellingTable[c,3] != dwellingTable[dwRowStart,3] | c == nrow(dwellingTable))
          {
            if (dwellingTable[c,3] != dwellingTable[dwRowStart,3])
            {
              dwRowEnd <- c-1
            }
            if (c == nrow(dwellingTable))
            {
              dwRowEnd <- c
            }
            
            coTable <- dwellingTable[dwRowStart:dwRowEnd,]
            coName <- dwellingTable[dwRowStart,3]
            #coFile <- paste(housingPath,"/",coName,".csv", sep = "")
            coTable <- coTable[, -c(1:3)]
            
            #create Table to export
            exportTable <- t(coTable) #transpose table
            exportTable <- as.data.frame(exportTable)
            colnames(exportTable) <- exportTable[1,]
            exportTable <- exportTable[-1, ]
            exportTable <- rownames_to_column(exportTable, var = "Municipality")
            
            todayStr <- Sys.Date()
            
            #coName, housingname, dwellingname, sheltername
            exportTable[, "Household Size"] <- shelterName
            exportTable[, "Number of Bedrooms"] <- dwellingName
            exportTable[, "Structural Type of Dwelling"] <- coName
            exportTable[, "Data_Source"] <- "StatsCan"
            exportTable[, "_lastupdate"] <- todayStr
            
            #Clean Municipality strings
            for (d in 1:nrow(exportTable))
            {
              muniStr <- exportTable[d,1]
              
              has_two_periods <- grepl("\\..*\\.", muniStr)
              
              if (has_two_periods) 
              {
                muniStr <- gsub("^([^.]+)\\.", "\\1 ", muniStr) #removes the first period and replaces with a space
                muniStr <- gsub("\\..*", "", muniStr) #removes everything after the remaining period 
              } else 
              {
                muniStr <- gsub("\\..*", "", muniStr) #removes everything after the remaining period
              }
              
              exportTable[d,1] <- muniStr
            }
            
            #append table to bottom of combined table
            exportCombined <- rbind(exportCombined, exportTable)
            
            #export table to folder
            #write.csv(exportTable, coFile, row.names = FALSE)
            
            confirmStr = paste("Created table:", coName)
            print(confirmStr)
            
            dwRowStart <- c
          }
        }
      }
    }
  }
}

fileName <- paste(mainPath,"/","StructuralType.csv", sep= "")
write.csv(exportCombined, fileName, row.names = FALSE)
print("Complete")