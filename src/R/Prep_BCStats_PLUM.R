install.packages("readxl", repos = "https://mirror.rcg.sfu.ca/mirror/CRAN/")
install.packages("tibble", repos = "https://mirror.rcg.sfu.ca/mirror/CRAN/")
install.packages("xlsx", repos = "https://mirror.rcg.sfu.ca/mirror/CRAN/")

library(readxl)
library(tibble)
library(xlsx)


#go col by col checking for young in first number, then go through until second number is equal to or over old, then calc dist
#create field in appropriate dist position 
create_age_dist <- function(in_table, young, old = -1)
{
  and_over <- old == -1
  if (and_over) {old <- 99}
  dist_indexes <- c()
  total_ind <- 4
  
  num_cols <- ncol(in_table)
  
  for (cl in 1:num_cols)
  {
    skip_col <- FALSE
    
    col_str <- colnames(in_table)[cl]
    col_vec <- strsplit(col_str, " ")[[1]]
    
    next_col_str <- colnames(in_table)[cl + 1]
    next_col_vec <- strsplit(next_col_str, " ")[[1]]
    
    #get current_young and current_old
    young_str <- col_vec[1]
    old_str <- col_vec[3]
    
    if (!is.na(old_str) && old_str == "Distribution")
    {
      dist_start <- cl
    }
    next_young_str <- next_col_vec[1]
    
    young_isNum <- !is.na(as.numeric(young_str))
    old_isNum <- !is.na(as.numeric(old_str))
    
    #check if column is an age range column
    if (young_isNum & old_isNum)
    {
      young_num <- as.numeric(young_str)
      old_num <- as.numeric(old_str)
      
      next_young_isNum <- !is.na(as.numeric(next_young_str))
      
      if (next_young_isNum)
      {
        next_young_num <- as.numeric(next_young_str)
        
        #set skip_col to true if next young num is less than current old num
        skip_col <- old_num > next_young_num
      }
      
      #check if young_num is >= young and if old_num <= old
      if (!skip_col && (young_num >= young & old_num <= old))
      {
        #add index to dist list
        dist_indexes <- c(dist_indexes, cl)
        #includes 100 and over column
        if (and_over & old_num == 99)
        {
          over_ind <- cl + 1
          dist_indexes <- c(dist_indexes, over_ind)
        }
      }
    }
  }

  #create dist col after dist_start in place before the next cols young num is higher than old_num
  for(dcl in dist_start:num_cols)
  {
    col_str <- colnames(in_table)[dcl]
    col_vec <- strsplit(col_str, " ")[[1]]
    
    young_str <- col_vec[1]
    old_str <- col_vec[3]
    
    young_isNum <- !is.na(as.numeric(young_str))
    
    #check if column is an age range column
    if (young_isNum)
    {
      young_num <- as.numeric(young_str)
      
      if (young < young_num)
      {
        split_ind <- dcl
        break
      }
      
    } else
    {
      split_ind <- dcl
      break
    }
  }
  #place new col before split_ind
  temp_table_A <- in_table[, 1:split_ind]
  st_b <- split_ind + 1
  temp_table_B <- in_table[, st_b:num_cols]
  
  if (!and_over)
  {
    new_col_str <- paste(young, "to", old, "years.1")
  } else
  {
    new_col_str <- paste(young, "years and over.1")
  }
  
  temp_table_A[, new_col_str] <- 0
  
  in_table <- cbind(temp_table_A, temp_table_B)
  
  #calculate dist for every row
  for (rw in 1:nrow(in_table))
  {
    sum <- 0
    total_val <- in_table[rw, total_ind]
    for (d_ind in dist_indexes)
    {
      sum <- sum + in_table[rw, d_ind]
    }
    dist_val <- sum / total_val * 100
    dist_val <- format(round(dist_val, 1), nsmall = 1)
    dist_val <- as.numeric(dist_val)
    
    #insert dist Vals
    dist_ind <- split_ind + 1
    in_table[rw, dist_ind] <- dist_val
    
  }
  return(in_table)
}

#Start of main
#-----------------------------------------------------

out_path <- "W:/PATH/FOR/PLUM/Output Tables/"
census_path <- "W:/path/of/CensusTables/" #should be the output of the tables produced in the Prep_BCStats_Census Script
table_path <- "W:/input/table/cro0172986_bc_cds_csds_its-worksheets_06282019.xlsx"
lookup_table <- read_excel("W:/table/downloaded/from/github/PLUM and StatsCan Comparison.xlsx", col_names = FALSE)#looking to host this on github
worksheet_list <- c("RD5917", "RD5921", "RD5915")

muni_inc <- c("5921007", "5917030", "5917034", "5915004", "5915022", "5915025", "5915034", "5915043", "5915055", "5915075")

colnames_inc <- c("Total", "Owners", "Renters")

lookup_tables_list <- split(lookup_table,lookup_table$...1)

sheet_num <- 1
        
for (sheet in worksheet_list)
{
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
                    
                    val <- val/tot_val * 100
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
    
    
    table_name_str <- gsub(" ", "", table_name_str) 
    table_name_str <- gsub("[()]", "_", table_name_str)
    table_name_str <- gsub("-", "_", table_name_str)
    
    
    if (table_name_str != "Remove")
    {
      print(paste("Saving table:", table_name_str))
      
      out_file <- paste(out_path, table_name_str,"_plum.xlsx", sep="")
      #create new xlsx on first run through
      if(sheet_num == 1)
      {
        write.xlsx(tr_split_table, out_file, row.names = FALSE)
      } else # adds new data to exsisting table
      {
        in_plum <- read_excel(out_file)
        tr_split_table <- rbind(in_plum,tr_split_table)
        
        tr_split_table <- as.data.frame(tr_split_table)
        write.xlsx(tr_split_table, out_file, row.names = FALSE)
      }
      
      if(sheet_num == length(worksheet_list))
      {
        #append onto equivalent census table?
        #check each column in tr_split_table and find a match in census_table
        census_table <- read_excel(paste(census_path, table_name_str, ".xlsx", sep = ""))
        
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
        
        print(paste("Adding plum data to", table_name_str))
        census_file <- paste(census_path, table_name_str,".xlsx", sep="")
        write.xlsx(census_table, census_file, row.names = FALSE)
      }
    }
  }
  sheet_num <- sheet_num + 1
}




