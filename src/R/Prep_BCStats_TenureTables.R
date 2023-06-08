#Tool to prepare non Census data tables from Statistics Canada for use in GIS

#Required packages
#---------------------------
#install.packages("readr")
#install.packages('tibble')
#install.packages("xlsx")

#library(readr)
#library(tibble)
#library(xlsx)
#---------------------------

#filepath to raw imported csv from statcan.gc.ca
#table_path: filepath to raw imported csv from statcan.gc.ca
#main_path: #filepath to output folder
#outfile_name: name of output excel file, DO NOT INCLUDE EXTENSION

#Example
#table_path <- "C:/Documents/imported tables"
#main_path <- "C:/Documents/output files"
#outfile_name <- "CoreHousingNeeds"
#prep_BCStats_TenureTables(table_path, main_path, outfile_name)


prep_BCStats_TenureTables <- function(table_path, main_path, outfile_name)
{
  #checks for required packages
  if (!require("readr"))
  {
    message("Package 'readr' not found. Please install it using install.packages('package1').")
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
  
  #Input csv from StatsCan
  master_table <- read.csv(table_path, header = FALSE)
  
  #output folder for excel table
  num_cols <- ncol(master_table)
  num_rows <- nrow(master_table)
  
  #find what rows to remove on top
  for (m_row in 1:num_rows)
  {
    if (master_table[m_row,num_cols] != "")
    {
      top_row <- m_row
      break
    }
  }
  master_table <- master_table[-c(1:top_row - 1),]
  
  #find how many Category Columns there are
  for (m_col in 1:num_cols)
  {
    if (master_table[1, m_col] != "")
    {
      num_cats <-m_col
      break
    }
  }
  
  start_muni_col <- num_cats + 1
  
  #make colnames
  for (ind in start_muni_col:num_cols)
  {
    master_table[2,ind] <- master_table[1,ind]
  }
  colnames(master_table) <- master_table[2,]
  master_table <- master_table[-c(1,2),]
  
  #trim Bottom metadata
  for(trim_row in 1:num_rows)
  {
    if(master_table[trim_row,num_cols] == "")
    {
      last_row <- trim_row - 1
      break
    }
  }
  master_table <- master_table[1:last_row,]
  num_rows <- last_row
  
  check_cols <- num_cats - 1
  count <- 0
  cols_to_delete <- vector(mode = "numeric", length = 0)
  
  #fill in category columns
  for (cat_col in 1:check_cols)
  {
    cat_str <- master_table[1,cat_col]
    cat_str <- gsub("[0-9]\\s*$", "", cat_str)
    master_table[1,cat_col] <- cat_str
    keep_cat <- FALSE
    
    for (cat_row in 1:num_rows)
    {
      if (master_table[cat_row,cat_col] != "" & cat_row != 1)
      {
        cat_str <- master_table[cat_row,cat_col]
        #remove trailing number from cat
        cat_str <- gsub("[0-9]\\s*$", "", cat_str)
        master_table[cat_row,cat_col] <- cat_str
        
        keep_cat <- TRUE
      }
      if (master_table[cat_row,cat_col] == "")
      {
        cat_str <- gsub("[0-9]\\s*$", "", cat_str)
        master_table[cat_row,cat_col] <- cat_str
      }
    }
    if (!keep_cat)
    {
      cols_to_delete <- append(cols_to_delete,cat_col)
    }
  }
  
  master_table <- master_table[,-cols_to_delete]
  num_cats <- num_cats - length(cols_to_delete)
  num_cols <- num_cols - length(cols_to_delete)
  
  #get list of cat column headers
  h_cat_num <- num_cats - 1
  cat_vec <- vector()
  for(cats in 1:h_cat_num)
  {
    current_cat <- colnames(master_table)[cats]
    #clean categories
    current_cat <- gsub(pattern = "\\(.*\\)\\s\\d", replacement = "", x = current_cat)
    current_cat <- trimws(current_cat)
    cat_vec <- append(cat_vec,current_cat)
  }
  
  #create colnames
  #Municipality, repeating set values, cat_columns, data_source, _lastupdate
  c_names <- c("Municipality")
  
  #set 
  rep_str <- master_table[1,num_cats]
  c_names <- append(c_names, rep_str)
  for (row in 2:num_rows)
  {
    if (master_table[row, num_cats] == rep_str)
    {
      break
    }
    set_str <- master_table[row,num_cats]
    c_names <- append(c_names, set_str)
  }
  
  #append cat_vec to c_names
  for(cat_c in 1:h_cat_num)
  {
    c_names <- append(c_names,cat_vec[cat_c])
  }
  c_names <- append(c_names, "data_source")
  c_names <- append(c_names, "_lastupdate")
  
  export_table <- data.frame(matrix(nrow = 0, ncol = length(c_names)))
  colnames(export_table) <- c_names
  
  row_start <- 1
  row_end <- 1
  #iterate down rows making sub tables for each set of values
  for (r in 2:num_rows)
  {
    #checks if set has ended
    if (master_table[r,num_cats] == rep_str)
    {
      row_end <- r - 1
      temp_table <- master_table[row_start:row_end,]
      
      cat_vals <- vector(length = h_cat_num) 
      for(c in 1:length(cat_vals))
      {
        cat_vals[c] <- master_table[row_start,c]
      }
      
      for(ind in 1:h_cat_num)
      {
      temp_table <- temp_table[,-1]
      }
      rownames(temp_table) <- temp_table[,1]
      temp_table <- temp_table[,-1]
      
      #go through each element and remove comma
      for(c_comma in 1:ncol(temp_table))
      {
        for(r_comma in 1:nrow(temp_table))
        {
          #remove comma
          x <- temp_table[r_comma,c_comma] 
          x <- gsub(",", "",x)
          temp_table[r_comma,c_comma] <- x
        }
      }
      
      #create Table to export
      tr_table <- t(temp_table) #transpose table
      tr_table <- as.data.frame(tr_table)
      tr_table <- tr_table[-1, ]
      tr_table <- rownames_to_column(tr_table, var = "Municipality")
      
      todayStr <- Sys.Date()
      v_fill <- length(cat_vals)
      
      for(v in 1:v_fill)
      {
        #c_names for column name cat_vals for column value
        tr_table[,cat_vec[v]] <- cat_vals[v]
      }
      tr_table[,"data_source"] <- "StatsCan"
      tr_table[,"_lastupdate"] <- todayStr
      
      #Clean Municipality strings
      for (e in 1:nrow(tr_table))
      {
        muniStr <- tr_table[e,1]
        
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
        tr_table[e,1] <- muniStr
      }
      #append table to bottom of combined table
      export_table <- rbind(export_table, tr_table)
      row_start <- r
    }
  }
  
  num_vals <- ncol(export_table) - (h_cat_num + 3)
  for(val in 1:num_vals)
  {
    ind <- val + 1
    export_table[, ind] <- as.numeric(export_table[, ind])
  }
  
  file_path <- file.path(main_path,paste0(outfile_name,".xlsx"))
  
  write.xlsx(export_table, file_path, row.names = FALSE)
  
  print("Process Complete")
}
