install.packages("readr")
install.packages('tibble')

library(readr)
library(tibble)

#read in Table
masterTable <- read.csv("W:/mtic/vic/rpd/Workarea/ArcGIS_Online/OHCS/Data/Tables/StatsCan/Raw Import/CoreHousingNeeds.csv", header = FALSE)
mainPath <- "W:/mtic/vic/rpd/Workarea/ArcGIS_Online/OHCS/Data/Tables/StatsCan/TenureTables"

#Clean Table
masterTable <- masterTable[-c(1:8),]
masterTable <- masterTable[,-4]
num_cols <- ncol(masterTable)

for (i in 6:num_cols)
{
  masterTable[2,i] <- masterTable[1,i]
}
colnames(masterTable) <- masterTable[2,]
masterTable <- masterTable[-c(1,2),]

#Initialize Final Dataframe
cNames <- c("Municipality","With mortage", "Without Mortgage", "Subsidized housing", "Not subsidized housing", 
            "Dwelling provided by the local government, First Nation or Indian band","Shelter Cost to Income Ratio",
            "Dwelling Condition", "Housing Suitability", "Core Housing Need", "Data_Source", "_lastupdate")

exportCombined <- data.frame(matrix(nrow = 0, ncol = length(cNames)))
colnames(exportCombined) <- cNames
  
#fill in category rows
for (x in 1:nrow(masterTable))
{
  if(masterTable[x,5] == "")
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
  
  if (masterTable[x,4] != "")
  {
    fourName <- masterTable[x,4]
  }
  
  fourName <- gsub("[0-9]", "", fourName)
  
  masterTable[x,1] <- oneName
  masterTable[x,2] <- twoName
  masterTable[x,3] <- threeName
  masterTable[x,4] <- fourName
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
            
            dwRowStart <- c
            
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
            
                coTable <- coTable[, -c(1:4)]
                
                #create Table to export
                exportTable <- t(coTable) #transpose table
                exportTable <- as.data.frame(exportTable)
                colnames(exportTable) <- exportTable[1,]
                exportTable <- exportTable[-1, ]
                exportTable <- rownames_to_column(exportTable, var = "Municipality")
                
                todayStr <- Sys.Date()
                
                #coName, housingname, dwellingname, sheltername
                exportTable[, "Shelter Cost to Income Ratio"] <- shelterName
                exportTable[, "Dwelling Condition"] <- dwellingName
                exportTable[, "Housing Suitability"] <- housingName
                exportTable[, "Core Housing Need"] <- coName
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
                  muniStr <- gsub(" i.*", "", muniStr)
                  exportTable[e,1] <- muniStr
                }
                
                #append table to bottom of combined table
                exportCombined <- rbind(exportCombined, exportTable)
                
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

fileName <- paste(mainPath,"/","CoreHousingNeeds.csv", sep= "")
write.csv(exportCombined, fileName, row.names = FALSE)
print("Complete")


