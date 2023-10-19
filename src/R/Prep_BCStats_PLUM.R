#prepares Census profile tables downloaded from Statistics canada as well as custom census tables from PLUM for use in GIS
#Note that the Prep_BCStats_Census.R table will be run in the script in order to create a fresh set of 2021 census tables

#Required Packages
#--------------------------
#install.packages("readxl")
#install.packages("tibble")
#install.packages("xlsx")

#library(readxl)
#library(tibble)
#library(xlsx)
#--------------------------

#out_path: destination folder for plum tables
#census_path: folder location of census tables
#table_path: location of plum dataset xlsx file
#input_path: location of raw census csv's downloaded from the Census profile website on Statistics Canada

#Example
#setwd("C:/Documents/hsms_Scripts")
#out_path: "C:/Documents/plum tables"
#census_path: "C:/Documents/census tables"
#table_path: "C:/Documents/plumcustomdownload.xlsx"
#input_path: "C:/Documents/censusprofile imports"
#prep_BCStats_PLUM(out_path, census_path, table_path, input_path)

prep_BCStats_PLUM <- function(out_path, census_path, table_path, input_path, lookup_table = NULL, worksheet_list = NULL, muni_inc = NULL)
{
  if (!require("readxl"))
  {
    message("Package 'readxl' not found. Please install it using install.packages('package1').")
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
  
  #creates a fresh set of census tables for plum tables to be added to
  tryCatch(
    source("Prep_BCStats_Census.R"),
    
    error = function(e)
    {
      print("Prep_BCStats_Census.R not found, Make sure to set your working directory to the folder containing Prep_BCStats_Census.R")
      stop()
    }
  )
  prep_BCStats_Census(input_path, census_path)
  
  
  if(is.null(lookup_table))
  {
    lookup_table <- read.csv("https://raw.githubusercontent.com/bcgov/hsms/main/res/PLUM%20and%20StatsCan%20Comparison.csv", header = FALSE)
  }
  #if muni_inc is being input make sure to ensure the worksheets that contain the regions are in worksheet_list
  if(is.null(worksheet_list))
  {
    worksheet_list <- c("RD5917", "RD5909", "RD5915", "RD5933", "PR_BC")
  }
  if(is.null(muni_inc))
  {
    muni_inc <- c("5909052", "5915011", "5915022", "5915043", "5915046", "5915055", "5917021", "5917030", "5917034", "5933042", "59")
  }
  
  colnames_inc <- c("Total", "Owners", "Renters")
  
  lookup_tables_list <- split(lookup_table,lookup_table$V1)
  
  sheet_num <- 1
          
  for (sheet in worksheet_list)
  {
    #sheet <- "RD5915"
    plum_table <- read_excel(table_path, sheet, col_names = FALSE)
  
    plum_table[1:7, 1] <- plum_table[1:7, 2]
    plum_table <- plum_table[, -2]
    
    #delete columns that aren't Total, Owners, Renters
    cols_to_remove <- c()
    
    for (cl in 2:ncol(plum_table))
    {
      current_stat <- plum_table[8, cl]
      current_muni <- plum_table[2, cl]
      
      if (!(current_muni %in% muni_inc) | !(current_stat %in% colnames_inc))
      {
        cols_to_remove <- c(cols_to_remove, cl)
      }
      if (current_muni == "5915046")
      {
        #print(plum_table[3,cl])
        plum_table[3, cl] <- "North Vancouver - District"
      }
    }
    
    if (length(cols_to_remove) > 0)
    {
      plum_table <- plum_table[, -cols_to_remove]
    }
    
    topic_string <- "Remove"
    
    plum_table <- cbind('Census Characteristic' = character(nrow(plum_table)), plum_table)
    plum_table <- cbind(Topic = character(nrow(plum_table)), plum_table)
    
    #find match in lookup tables
    plum_start <- 1
    for (rw in 2:nrow(plum_table))
    {
      current_var <- plum_table[rw, 3]
  
      if(plum_start != 1 && startsWith(current_var, "Total") | startsWith(current_var, "Population"))
      {
        
        plum_end <- rw - 1
        look_start <- 1
        start_found <- FALSE
        atEnd <- FALSE
        total_string <- plum_table[plum_start,3]
        #search only the rows in this group
        for(p_rw in plum_start:plum_end)
        {
          #search for matching total string in col 3 and create range
          match_string <- plum_table[p_rw, 3]
          
          match_count <- 0
          
          for (i in look_start:nrow(lookup_table))
          {
            lookup_string <- as.character(lookup_table[i, 3])
            lookup_string <- trimws(lookup_string)
            if(!start_found)
            {
              
              if(lookup_string == total_string)
              {
                plum_table[p_rw, 1] <- lookup_table[i, 1] #p_rw is the total before?
                plum_table[p_rw, 2] <- lookup_table[i, 2]
                look_start <- i
                start_found <- TRUE
                break
              }
            } else #do this while start has been found until next total string
            {
              if(match_count != 0 && startsWith(lookup_string, "Total") | startsWith(lookup_string, "Population"))#don't check on first time since start_found
              {
                #end found
                atEnd <- TRUE
                break
              } else #this is true while the start has been found and the next total has not been reached
              {
                match_count <- match_count + 1
                #find matches
                if(lookup_string == match_string)
                {
                  plum_table[p_rw, 1] <- lookup_table[i,1]
                  plum_table[p_rw, 2] <- lookup_table[i,2]
                  
                  #change to percentage for "% of" rows
                  perc_string <- as.character(lookup_table[i, 2])
                  if(!is.na(perc_string) && startsWith(perc_string, "%"))
                  {
                    #go through each value and convert to percentage
                    for (cl in 4:ncol(plum_table))
                    {
                      val <- as.numeric(plum_table[p_rw, cl])
          
                      tot_ind <- p_rw - 1
                      tot_val <- as.numeric(plum_table[tot_ind, cl])
                      
                      val <- val/ tot_val * 100
                      val <- format(round(val, 1), nsmall = 1)
                      
                      plum_table[p_rw, cl] <- val
                    }
                  }
                  break
                }
              }
            }
          }
          #if at end
          if (atEnd)
          {
            break
          }
        }
      }
      #mark start of total group
      if(startsWith(current_var, "Total") | startsWith(current_var, "Population"))
      {
        plum_start <- rw
      }
    }
    
    for (rm in 1:nrow(plum_table))
    {
      if(is.na(plum_table[rm,1]) | plum_table[rm,1] == "")
      {
        plum_table[rm, 1] <- "Remove"
      }
    }
    
    plum_table <- plum_table[-c(1,2,4,5,6), ]
    head_plum_table <- plum_table[1:3, ]
    
    plum_table_list <- split(plum_table, plum_table$Topic)
    
    #go through each table and append to equivalent census table
    for (split_table in plum_table_list)
    {
      table_name_str <- split_table[1,1]
      split_table <- rbind(head_plum_table, split_table)
      del_rows <- c()
      print(paste("Starting:", table_name_str))
     
      split_table <- split_table[, -c(1,3)]
      
      tr_split_table <- t(split_table)
      
      tr_split_table[1,1:3] <- c("Municipality", "Date_Range", "Tenure")
      colnames(tr_split_table) <- tr_split_table[1, ]
      tr_split_table <- tr_split_table[-1, ]
      tr_split_table <- as.data.frame(tr_split_table)
      
      today <- Sys.Date()
      tr_split_table[, "data_source"] <- "PLUM"
      tr_split_table[, "_lastupdate"] <- today
      
      tr_split_table[,2] <- as.numeric(tr_split_table[,2])
      
      num_cols <- ncol(tr_split_table)
      check_cols <- num_cols - 5
      
      if (table_name_str == "Age characteristics")
      {
        end_a <- check_cols + 1
        temp_table_A <- tr_split_table[, 1:end_a]
        st_b <- check_cols + 2
        temp_table_B <- tr_split_table[, st_b:num_cols]
        
        temp_table_A[, "Total - Distribution (%) of the population by broad age groups - 100% data"] <- 100
        
        tr_split_table <- cbind(temp_table_A,temp_table_B)
        
        #fix median row not changing to numeric
        for (age_row in 1:nrow(tr_split_table))
        {
          med_val <- tr_split_table$`Median age of the population`[4]
          med_val <- as.numeric(med_val)
          tr_split_table$`Median age of the population`[4] <- med_val
        }
        tr_split_table$`Median age of the population` <- as.numeric(tr_split_table$`Median age of the population`)
        
        for (age_row in 1:nrow(tr_split_table))
        {
          med_val <- tr_split_table$`Average age of the population`[4]
          med_val <- as.numeric(med_val)
          tr_split_table$`Average age of the population`[4] <- med_val
        }
        tr_split_table$`Average age of the population` <- as.numeric(tr_split_table$`Average age of the population`)
        
        #get population column for use in headship
        pop_col <- tr_split_table[,4, drop=FALSE]
      }
      
      #makes stats cols numeric
      for (m in 1:check_cols)
      {
        ind <- m + 3
        check_val <- tr_split_table[1, ind]
        isNum <- !is.na(as.numeric(check_val))
        if (isNum)
        {
          for(r_comma in nrow(tr_split_table))
          {
            #removes thousand separator comma's 
            x <- tr_split_table[r_comma,ind]
            x <- gsub(",", "",x)
            tr_split_table[r_comma,ind] <- x
          }
          
          tr_split_table[,ind] <- as.numeric(tr_split_table[,ind])
        }
      }
      
      if (table_name_str == "Age characteristics")
      {
        tr_split_table <- create_age_dist(tr_split_table, 0, 14)
        tr_split_table <- create_age_dist(tr_split_table, 15, 64)
        tr_split_table <- create_age_dist(tr_split_table, 15, 19)
        tr_split_table <- create_age_dist(tr_split_table, 20, 24)
        tr_split_table <- create_age_dist(tr_split_table, 25, 64)
        tr_split_table <- create_age_dist(tr_split_table, 65, 84)
        tr_split_table <- create_age_dist(tr_split_table, 15, 64)
        tr_split_table <- create_age_dist(tr_split_table, 65)
        tr_split_table <- create_age_dist(tr_split_table, 85)
      }
      
      if (table_name_str == "Income of households in 2020")
      {
        tr_split_table <- create_income_brackets(tr_split_table, 0, 19999)
        tr_split_table <- create_income_brackets(tr_split_table, 20000, 39999)
        tr_split_table <- create_income_brackets(tr_split_table, 40000, 59999)
        tr_split_table <- create_income_brackets(tr_split_table, 60000, 79999)
        tr_split_table <- create_income_brackets(tr_split_table, 80000, 99999)
      }
      
      if (table_name_str == "Household characteristics")#might not be the right spot for this, because the census tables will be join after
        #should census be calculated in that script or here? or transer population here then calc at the end once plum and census have been combined
      {
        
        tr_split_table$`Population-Total` <- 0
        num_row <- nrow(tr_split_table)
        
        for(j in 1:num_row)
        {
          tr_split_table$`Population-Total`[j] <-pop_col[j,1]
        }
        
      }
      
      table_name_str <- gsub(" ", "", table_name_str) 
      table_name_str <- gsub("[()]", "_", table_name_str)
      table_name_str <- gsub("-", "_", table_name_str)
      
      
      if (table_name_str != "Remove")
      {
        print(paste("Saving table:", table_name_str))
        out_file <- file.path(out_path, paste0(table_name_str, "_plum.xlsx"))
        
        #create new xlsx on first run through
        if(sheet_num == 1)
        {
          write.xlsx(tr_split_table, out_file, row.names = FALSE)
        } else # adds new data to exsisting table
        {
          in_plum <- read_excel(out_file)
          colnames(tr_split_table) <- colnames(in_plum)
          tr_split_table <- rbind(in_plum,tr_split_table)
          
          tr_split_table <- as.data.frame(tr_split_table)
          write.xlsx(tr_split_table, out_file, row.names = FALSE)
        }
        
        if(sheet_num == length(worksheet_list))
        {
          #append onto equivalent census table?
          #check each column in tr_split_table and find a match in census_table
          read_file <- file.path(census_path, paste0(table_name_str, ".xlsx"))
          census_table <- read_excel(read_file)
          
          temp_table <- data.frame(matrix(nrow = nrow(tr_split_table), ncol = ncol(census_table)))
          colnames(temp_table) <- colnames(census_table)
          
          for (cp in 1:ncol(tr_split_table))
          {
            plum_field <- colnames(tr_split_table)[cp]
            for (cc in 1:ncol(census_table))
            {
              census_field <- colnames(census_table)[cc]
              if (plum_field == census_field)
              {
                #add col to temp_table at index cc
                temp_table[,cc] <- tr_split_table[,cp]
              }
            }
          }
          census_table <- rbind(census_table, temp_table)
          
          census_table <- as.data.frame(census_table)
          
          if (table_name_str == "Householdcharacteristics")
          {
            #create headship column
            census_table$Headship <- 0
            
            for (k in 1:nrow(census_table))
            {
              households <- as.numeric(census_table[k,8])
              population <- as.numeric(census_table$`Population-Total`[k])
              
              head_val <- households / population
              head_val <- format(round(head_val, 3), nsmall = 3)
              head_val <- as.numeric(head_val)
              
              census_table$Headship[k] <- head_val
              
            }
          }
          
          if (table_name_str == "Householdanddwellingcharacteristics")
          {
            print("Adding percent change")
            #Create Cols for each Per change
            census_table <- insert_col(census_table, 13, "Total Household by Size % Change")
            census_table <- insert_col(census_table, 15, "1 Person % Change")
            census_table <- insert_col(census_table, 17, "2 Persons % Change")
            census_table <- insert_col(census_table, 19, "3 Persons % Change")
            census_table <- insert_col(census_table, 21, "4 Persons % Change")
            census_table <- insert_col(census_table, 23, "5 or More Persons % Change")
            
            #calculate percent change from 2006
            #go row by row
            for (rw in 1:nrow(census_table))
            {
              #get current muni, year, tenure
              curr_muni <- census_table[rw, 1]
              curr_year <- census_table[rw, 2]
              curr_tenure <- census_table[rw, 3]
              
              if (curr_year == 2006)
              {
                for (value in c(14, 16, 18, 20, 22, 24))
                {
                  census_table[rw,value] <- 0
                }
              } else
              {
                #go row by row again and search for muni then 2006 then tenure
                check_start <- 1
                for (ch_rw in check_start:nrow(census_table))
                {
                  ch_muni <- census_table[ch_rw, 1]
                  ch_year <- census_table[ch_rw, 2]
                  ch_tenure <- census_table[ch_rw, 3]
                  
                  if (curr_muni == ch_muni && ch_year == 2006 && curr_tenure == ch_tenure)
                  {
                    
                    for (value in c(13, 15, 17, 19, 21, 23))
                    {
                      ch_val <- value + 1
                      #found 2006 value row to compare
                      final <- census_table[rw, value]
                      initial <- census_table[ch_rw,value]
                      
                      per_val <- (final-initial)/initial * 100
                      census_table[rw,ch_val] <- round(per_val,1)
                    }
                  }
                }
              }
            }
          }
          
          #create year_tenure column
          census_table$`Year_Tenure` <- "."
          
          for (yt in 1:nrow(census_table))
          {
            add_string <- paste(census_table$Date_Range[yt], census_table$Tenure[yt])
            census_table$`Year_Tenure`[yt] <- add_string
          }
          
          print(paste("Adding plum data to", table_name_str))
          census_file <- file.path(census_path, paste0(table_name_str, ".xlsx"))
          write.xlsx(census_table, census_file, row.names = FALSE)
        }
      }
    }
    sheet_num <- sheet_num + 1
  }
  print("Process Complete")
}

