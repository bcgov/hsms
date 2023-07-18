#all required functions for hsms R scripts

#creates income brackets in Incomeofhouseholdsin2020
create_income_brackets <- function(in_table, bottom, top)
{
  brack_indexes <- c()
  
  num_cols <- ncol(in_table)
  inds_found <- FALSE
  
  for (cl in 1:num_cols)
  {
    skip_col <- FALSE
    
    col_str <- colnames(in_table)[cl]
    col_str <- trimws(col_str)
    col_vec <- strsplit(col_str, " ")[[1]]
    
    next_col_str <- colnames(in_table)[cl + 1]
    next_col_vec <- strsplit(next_col_str, " ")[[1]]
    
    #get bottom and top
    bottom_str <- col_vec[1]
    bottom_str <- gsub("[$,]", "", bottom_str)
    
    if(bottom_str == "Under")
    {
      top_str <- col_vec[2]
      bottom_str <- "0"
    } else
    {
      top_str <- col_vec[3]
    }
    top_str <- gsub("[$,]", "", top_str)
    
    next_bottom_str <- next_col_vec[1]
    next_bottom_str <- gsub("[$,]", "", next_bottom_str)
    
    bottom_isNum <- !is.na(as.numeric(bottom_str))
    top_isNum <- !is.na(as.numeric(top_str))
    
    #check if column is an age range column
    if (bottom_isNum & top_isNum)
    {
      bottom_num <- as.numeric(bottom_str)
      top_num <- as.numeric(top_str)
      
      next_bottom_isNum <- !is.na(as.numeric(next_bottom_str))
      
      if (next_bottom_isNum)
      {
        next_bottom_num <- as.numeric(next_bottom_str)
        
        #set skip_col to true if next young num is less than current old num
        skip_col <- next_bottom_num < top_num
      }
      
      if (!inds_found && !skip_col && (bottom_num >= bottom & top_num <= top))
      {
        #add index to dist list
        brack_indexes <- c(brack_indexes, cl)
        #if col matches range of new index
        if (bottom_num == bottom & top_num == top)
        {
          dist_indexes <- cl
          inds_found <- TRUE
        }
        #if top of range is reached
        if (top_num == top)
        {
          inds_found <- TRUE
        }
      }
    }
  }
  #create column at end
  f_bottom <- format(bottom, big.mark = ",")
  f_top <- format(top, big.mark = ",")
  new_col_str <- paste("$", f_bottom, " to $", f_top, sep="")
  
  in_table[, new_col_str] <- 0
  
  #go through each row then each ind and calculate income 
  for (rw in 1:nrow(in_table))
  {
    sum <- 0
    for(n_cl in brack_indexes)
    {
      sum <- sum + in_table[rw, n_cl]
    }
    sum <- as.numeric(sum)
    in_table[rw, new_col_str] <- sum
  }
  return(in_table)
}

#go col by col checking for young in first number, then go through until second number is equal to or over old, then calc dist
#create field in appropriate dist position 
create_age_dist <- function(in_table, young, old = -1)
{
  and_over <- old == -1
  if (and_over) {old <- 99}
  dist_indexes <- c()
  total_ind <- 4
  
  num_cols <- ncol(in_table)
  inds_found <- FALSE
  
  for (cl in 1:num_cols)
  {
    skip_col <- FALSE
    
    col_str <- colnames(in_table)[cl]
    col_str <- trimws(col_str)
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
        skip_col <- next_young_num < old_num
      }
      
      #check if young_num is >= young and if old_num <= old
      if (!inds_found && !skip_col && (young_num >= young & old_num <= old))
      {
        #add index to dist list
        dist_indexes <- c(dist_indexes, cl)
        #includes 100 and over column
        if (and_over & old_num == 99)
        {
          over_ind <- cl + 1
          dist_indexes <- c(dist_indexes, over_ind)
        }
        #if column exactly matches dist range only use the one column
        if (young_num == young & old_num == old)
        {
          dist_indexes <- cl
          inds_found <- TRUE
        }
        if (old_num == old)
        {
          inds_found <- TRUE
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

#a - Excellent, b- Very good, c - Good, d - Fair (Use with Caution)
#replace reliability strings with original scores from cmhc
replace_rel <- function(in_rel)
{
  if(is.na(in_rel))
  {
    return(NA)
    
  } else if (in_rel == "Excellent")
  {
    return("a")
  } else if (in_rel == "Very good")
  {
    return("b")
  } else if (in_rel == "Good")
  {
    return("c")
  } else if (in_rel == "Fair (Use with Caution)")
  {
    return("d")
  } else
  {
    return(NA)
  }
}

#function to take input table from cmhc and put into final table for export
#only from 2012-2022
#in_type is a list of value header strings
#in_table is the input table from get_cmhc()
#out_type is a string of the dimension variable
build_table <- function(in_table, in_master_table, out_type, muni_name)
{
  five_ch <- FALSE
  six_ch <- FALSE
  
  #find in_types
  rep_str <- as.character(in_table[1,4][[1]])
  in_type <- c(rep_str)
  
  for (r in 2:nrow(in_table))
  {
    cur_type <- as.character(in_table[r,4][[1]])
    if (cur_type == rep_str)
    {
      break
    } else
    {
      in_type <- c(in_type, cur_type)
    }
  }
  
  for (e in 1:nrow(in_table))
  {
    
    at_total <- FALSE #when TRUE the new_row is considered complete and can add to table
    
    #get dateString year value
    date_str <- in_table$DateString[e]
    date_vec <- strsplit(date_str, split = ' ')
    date_num <- as.numeric(date_vec[[1]][1])
    date_month <- date_vec[[1]][2]
    
    if (date_num >= 2012 & date_num < 2023)
    {
      #pull Value classification and value
      current_bed_type <- as.character(in_table[[4]][e])
      current_val <- in_table$Value[e]
      
      #get reliability string from the Quality column
      if("Quality" %in% colnames(in_table))
      {
        current_rel <- as.character(in_table$Quality[e])
      } else
      {
        current_rel <- NA
      }
      #apply values to the different Room types defined in in_type list
      if(current_bed_type == in_type[1])
      {
        one_val <- current_val
        one_rel <- replace_rel(current_rel)
        
      } else if(current_bed_type == in_type[2])
      {
        two_val <- current_val
        two_rel <- replace_rel(current_rel)
        
      } else if(current_bed_type == in_type[3])
      {
        three_val <- current_val
        three_rel <- replace_rel(current_rel)
        
      } else if(current_bed_type == in_type[4])
      {
        four_val <- current_val
        four_rel <- replace_rel(current_rel)
        
      }
      #some tables don't have as many room types so these if statements need to be protected
      if (length(in_type) > 4)
      {
        if(current_bed_type == in_type[5])
        {
          five_val <- current_val
          five_rel <- replace_rel(current_rel)
          five_ch <- TRUE
        }
      }
      
      if (length(in_type) > 5)
      {
        if(current_bed_type == in_type[6])
        {
          six_val <- current_val
          six_rel <- replace_rel(current_rel)
          six_ch <- TRUE
        }
      }
      #Final row for each column, see at_total being set to TRUE
      if(current_bed_type == "Total" | current_bed_type == "Units")
      {
        tot_val <- as.numeric(current_val)
        
        if(current_bed_type != "Units")
        {
          tot_rel <- replace_rel(current_rel)
        }
        at_total <- TRUE
        
      }
      #every time we get to Total create new row
      if (at_total)
      {
        #adds row for Structure size table
        if(out_type == "Structure Size")
        {
          if (!five_ch)
          {
            five_val <- NA
            five_rel <- NA
          }
          new_row <- c(series_str, muni_name, date_num, one_val, one_rel,
                       two_val, two_rel, three_val, three_rel, four_val, four_rel, five_val, five_rel, tot_val, tot_rel,
                       "Primary Rental Market", d_filter, "CMHC", update_str)
          
          in_master_table[nrow(in_master_table) + 1,] <- new_row
          
          #adds row for Rent Ranges Table  
        } else if (out_type == "Rent Ranges")
        {
          
          if (!five_ch)
          {
            five_val <- NA
            five_rel <- NA
          }
          
          if (!six_ch)
          {
            six_val <- NA
            six_rel <- NA
          }
          new_row <- c(series_str, muni_name, date_num, one_val, one_rel,
                       two_val, two_rel, three_val, three_rel, four_val, four_rel, five_val, five_rel,
                       six_val, six_rel, tot_val, tot_rel, "Primary Rental Market", d_filter, "CMHC", update_str)
          
          in_master_table[nrow(in_master_table) + 1,] <- new_row
          
          #adds row for summary table
        } else if (out_type == "Summary Statistics")
        {
          if (!five_ch)
          {
            five_val <- NA
            five_rel <- NA
          }
          
          new_row <- c(series_str, muni_name, date_num, one_val, one_rel,
                       two_val, two_rel, three_val, three_rel, four_val, four_rel, five_val, five_rel, tot_val,
                       "Primary Rental Market", d_filter, "CMHC", update_str)
          
          c("Classification", "Municipality", "Date_Range", "Vacancy Rate_Percent", "Reliability_Code_vacrate", "Availability_Rate_Percent",
            "Reliability_Code_avlrate", "Average_Rent", "Reliability_Code_avgrent", "Median_Rent", "Reliability_Code_medrent",
            "Percent_Change", "Reliability_Code_perchg", "Units", "Category", "Row_Apartment", "Data_Source", "_lastupdate")
          
          in_master_table[nrow(in_master_table) + 1, ] <- new_row
          
          #adds row for all other tables 
        } else
        {
          
          new_row <- c(series_str, muni_name, date_num, one_val, one_rel,
                       two_val, two_rel, three_val, three_rel, four_val, four_rel, tot_val, tot_rel,
                       "Primary Rental Market", d_filter, "CMHC", update_str)
          
          in_master_table[nrow(in_master_table) + 1,] <- new_row
          
        }
        at_total <- FALSE
        five_ch <- FALSE
        six_ch <- FALSE
      }
    }
  }
  #return table that will overwrite the table for that type in main
  return(in_master_table)
}

fix_stats_vals <- function(in_table, start_row = 3)
{
  check_cols <- ncol(in_table) - 1
  
  for (ind in start_row:check_cols)
  {
    check_val <- in_table[1, ind]
    isNum <- !is.na(as.numeric(check_val))
    if (isNum)
    {
      for(r_comma in nrow(in_table))
      {
        #removes thousand separator comma's 
        x <- in_table[r_comma,ind]
        x <- gsub(",", "",x)
        in_table[r_comma,ind] <- x
      }
      
      in_table[,ind] <- as.numeric(in_table[,ind])
    }
  }
  return(in_table)
}

#function takes target val and divides by total val, and formats
#args: targetVal, totVal
#returns: rowVal
get_dist_val <- function(targetVal, totVal)
{
  rowVal <- (targetVal / totVal) * 100
  rowVal <- format(round(rowVal, 1), nsmall = 1)
  rowVal <- as.numeric(rowVal)
  
  return(rowVal)
}