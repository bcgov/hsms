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

prep_BCStats_PLUM <- function(out_path, census_path, table_path, input_path, lookup_table = NULL, worksheet_list = NULL, muni_inc = NULL, inc_reg = FALSE)
{
  #TEMP
  #-----------------
  #out_path <- "W:/mtic/vic/rpd/Workarea/ArcGIS_Online/OHCS/Data/Tables/PLUM/2021/"
  #census_path <- "W:/mtic/vic/rpd/Workarea/ArcGIS_Online/OHCS/Data/Tables/StatsCan/R_Output/CensusTables/" #should be the output of the tables produced in the Prep_BCStats_Census Script
  #table_path <- "W:/mtic/vic/rpd/Workarea/ArcGIS_Online/OHCS/Data/Tables/PLUM/Raw Import/ord-08588-g8n3z6.xlsx"
  #input_path <- "W:/mtic/vic/rpd/Workarea/ArcGIS_Online/OHCS/Data/Tables/StatsCan/Raw Import/CensusImports"
  #inc_reg<- FALSE
  
  #split_sheet_vec <- split(sheet_names, cut(seq_along(sheet_names), 3, labels = FALSE))
  #folder_num <- 1
  #worksheet_list <- c("RD5915")
  #folder_path <- paste0(out_path,"Part ",1,"/")
  #muni_inc <- c("5909052", "5915011", "5915022", "5915043", "5915046", "5915055", "5917021", "5917030", "5917034", "5933042", "59")
  #-----------------
  
  
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
  #prep_BCStats_Census(input_path, census_path)
  all_muni <- FALSE
  
  if(is.null(lookup_table))
  {
    lookup_table <- read.csv("https://raw.githubusercontent.com/bcgov/hsms/main/res/PLUM%20and%20StatsCan%20Comparison.csv", header = FALSE)
  }
  #if muni_inc is being input make sure to ensure the worksheets that contain the regions are in worksheet_list
  if(is.null(worksheet_list))
  {
    worksheet_list <- c("RD5917", "RD5909", "RD5915", "RD5933", "PR_BC")
  }
  if(is.null(muni_inc))#if no muni list supplied, run for all muni's and filter by type
  {
    reg_type <- c("CY","DM","VL","T","TWL","TAL","IM","NL")
    all_muni <- TRUE
  }
  if(inc_reg)
  {
    reg_type <- c(reg_type,"RDA")
  }

  colnames_inc <- c("Total", "Owners", "Renters")
  
  lookup_tables_list <- split(lookup_table,lookup_table$V1)
  
  sheet_num <- 1
          
  for (sheet in worksheet_list)
  {
    print(sheet)
    if (sheet != "Metadata_Definitions" & sheet != "Notes_Footnotes")
    {
    #sheet <- "RD5907"
    plum_table <- read_excel(table_path, sheet, col_names = FALSE)
  
    plum_table[1:6, 1] <- plum_table[1:6, 2]
    plum_table <- plum_table[, -2]
    
    #delete columns that aren't Total, Owners, Renters
    cols_to_remove <- c()
    
    for (cl in 3:ncol(plum_table))
    {
      current_stat <- plum_table[7, cl]
      current_muni <- plum_table[[1, cl]]
      current_type <- plum_table[3, cl]
      #different queries if all muni's or a list of muni's
      if(all_muni)
      {
        if(!(current_type %in% reg_type) | !(current_stat %in% colnames_inc))
        {
          #print(paste(current_muni,":",current_type))
          cols_to_remove <- c(cols_to_remove, cl)
        }
      } else
      {
        if(!(current_muni %in% muni_inc) | !(current_stat %in% colnames_inc))
        {
          cols_to_remove <- c(cols_to_remove, cl)
        }
      }
      
      if (current_muni == "5915046")
      {
        plum_table[2, cl] <- "North Vancouver - District"
      }
      if (current_muni == "5915051")
      {
        plum_table[2, cl] <- "North Vancouver - City"
      }
      if (current_muni == "5915001")
      {
        plum_table[2, cl] <- "Langley - District"
      }
      if (current_muni == "5915002")
      {
        plum_table[2, cl] <- "Langley - City"
      }
      
    }
    #insert_table <- plum_table
    #plum_table <- fix_rd_names(plum_table)
    
    if (length(cols_to_remove) > 0)
    {
      plum_table <- plum_table[, -cols_to_remove]
    }
    #print(ncol(plum_table))
    if (ncol(plum_table) > 2)
    {
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
    
    plum_table <- plum_table[-c(1,3,4,5), ]
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
      }
    }
    sheet_num <- sheet_num + 1
    }
    }
  }
  print("Process Complete")
}

