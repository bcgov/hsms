

prep_Inflation_Rate <- function(table_path, out_path, muni_xlsx, sheet_num = 1, copy_muni)
{
  
  if (!require("xlsx"))
  {
    message("Package 'xlsx' not found. Please install it using install.packages('xlsx').")
    return(NULL)
  }
  if (!require("lubridate"))
  {
    message("Package 'lubridate' not found. Please install it using install.packages('lubridate').")
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
  
  inp_table <- read.csv(table_path)
  
  inp_table <- inp_table[-c(1,2,3,4,5,6,7,9,10),]
  
  rw <- 1
  while (inp_table[rw,2] != "")
  {
    rw <- rw + 1
  }
  
  end <- rw-1
  
  inp_table <- inp_table[1:end,]
  
  col_names <- c("Date_Range","Municipality", "Inflation Rate")
  inf_table <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(inf_table) <- col_names
  
  my_string <- sub(",.*$", "", inp_table[1,5])
  inp_table[1,5] <- my_string
  my_string <- sub(",.*$", "", inp_table[1,4])
  inp_table[1,4] <- my_string
  
  inp_table[1,1] <- inp_table[2,1]
  colnames(inp_table) <- inp_table[1,]
  inp_table <- inp_table[-1,]
  inp_table <- inp_table[,-1]
  
  inp_table <- add_munis(inp_table, muni_xlsx, "British Columbia", sheet_num)
  
  for (x in 1:nrow(inp_table))
  {
    for (y in 2:ncol(inp_table))
    {
      muni <- colnames(inp_table)[y]
      date <- inp_table[x,1]
      val <- inp_table[x,y]
      
      new_row <- c(date, muni, val)
      inf_table <- rbind(inf_table, new_row)
    }
  }
  
  today <- Sys.Date()
  inf_table$data_source <- "StatsCan"
  inf_table[,"_lastupdate"] <- today
  colnames(inf_table)[3] <- "Inflation Rate"
  colnames(inf_table)[2] <- "Municipality"
  colnames(inf_table)[1] <- "Date_Range"
  inf_table[,3] <- as.numeric(inf_table[,3])
  
  Sys.setlocale("LC_TIME", "C")
  
  for(r in 1:nrow(inf_table))
  {
    date_str <- inf_table[r,1]
    date <- my(date_str)
    formatted_date <- format(date, "%Y-%m")
    
    inf_table[r,1] <- formatted_date
  }
  
  muni_names <- c()
  muni_tb <- read.xlsx(muni_xlsx, sheet_num)
  
  for (r in 1:nrow(muni_tb))
  {
    muni_name <- muni_tb[r,1]
    if (!(muni_name %in% current_col_names))
    {
      muni_names <- c(muni_names, muni_name)
    }
  }
  
  #Add is BC Value field
  inf_table$`Only BC Value Available` <- NA
  
  for (rw in 1:nrow(in_table))
  {
    if (inf_table[rw,2] == "British Columbia")
    {
      inf_table$`Only BC Value Available`[rw] <- "YES"
    } else if (inf_table[rw,2] %in% muni_names)
    {
      inf_table$`Only BC Value Available`[rw] <- "YES"
    } else
    {
      inf_table$`Only BC Value Available`[rw] <- "NO"
    }
  }
  
  write.xlsx(inf_table, out_path,row.names = FALSE)
  print(paste("Exported Table to:", out_path))
}

prep_Bank_Rate <- function(table_path, out_path)
{
  if (!require("xlsx"))
  {
    message("Package 'xlsx' not found. Please install it using install.packages('xlsx').")
    return(NULL)
  }
  if (!require("lubridate"))
  {
    message("Package 'lubridate' not found. Please install it using install.packages('lubridate').")
    return(NULL)
  }
  
  inp_table <- read.csv(table_path)
  
  inp_table <- inp_table[-c(1,2,3,4,5,6,7,8,9,10,11),]
  inp_table <- inp_table[,-3]
  
  today <- Sys.Date()
  inp_table$data_source <- "StatsCan"
  inp_table[,"_lastupdate"] <- today
  colnames(inp_table)[2] <- "Rate"
  colnames(inp_table)[1] <- "Date_Range"
  inp_table$Municipality <- "British Columbia"
  inp_table[,2] <- as.numeric(inp_table[,2])
  
  write.xlsx(inp_table, out_path,row.names = FALSE)
  print(paste("Exported Table to:", out_path))
}

prep_Mortgage_5_Year <- function(table_path, out_path)
{
  if (!require("xlsx"))
  {
    message("Package 'xlsx' not found. Please install it using install.packages('xlsx').")
    return(NULL)
  }
  if (!require("lubridate"))
  {
    message("Package 'lubridate' not found. Please install it using install.packages('lubridate').")
    return(NULL)
  }
  
  inp_table <- read.csv(table_path)
  
  inp_table <- inp_table[-c(1,2,3,4,5,6,7,8,9,10),]
  inp_table <- inp_table[,-3]
  
  w <- 1
  while (inp_table[rw,2] != "")
  {
    rw <- rw + 1
  }
  
  end <- rw-2
  
  inp_table <- inp_table[1:end,]
  
  today <- Sys.Date()
  inp_table$data_source <- "StatsCan"
  inp_table[,"_lastupdate"] <- today
  colnames(inp_table)[2] <- "Fixed 5 Year"
  colnames(inp_table)[1] <- "Date_Range"
  inp_table$Municipality <- "British Columbia"
  inp_table[,2] <- as.numeric(inp_table[,2])
  
  Sys.setlocale("LC_TIME", "C")
  for(r in 1:nrow(inp_table))
  {
    date_str <- inp_table[r,1]
    date <- my(date_str)
    formatted_date <- format(date, "%Y-%m")
    
    inp_table[r,1] <- formatted_date
  }
  
  write.xlsx(inp_table, out_path,row.names = FALSE)
  print(paste("Exported Table to:", out_path))
}

prep_Variable_Interest_Rate <- function(table_path, out_path)
{
  if (!require("xlsx"))
  {
    message("Package 'xlsx' not found. Please install it using install.packages('xlsx').")
    return(NULL)
  }
  if (!require("lubridate"))
  {
    message("Package 'lubridate' not found. Please install it using install.packages('lubridate').")
    return(NULL)
  }
  
  inp_table <- read.csv(table_path)
  
  inp_table <- inp_table[-c(1,2,3,4,5,6,7,8,9,10,11),]
  inp_table <- inp_table[,-3]
  
  col_names <- c("Year", "Rate")
  new_table <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(new_table) <- col_names
  
  numr <- nrow(inp_table)
  sum <- 0
  count <- 0
  for(r in 1:numr)
  {
    count <- count + 1
    dateStr <- inp_table[r,1]
    print(dateStr)
    dateStr <- strsplit(dateStr,"-")
    month <- dateStr[[1]][2]
    year <- dateStr[[1]][3]
    year <- as.numeric(year)
    range <- as.numeric(inp_table[r,2])
    sum <- sum + range
    #if next month is different
    nr <- as.numeric(r) + 1
    nextDate <- inp_table[nr,1]
    nextDate <- strsplit(nextDate, "-")
    nextMonth <- nextDate[[1]][2]
    
    if(r == numr || month != nextMonth)
    {
      val <- sum/count
      val <- round(val, digits=2)
      new_row <- c(paste(month,year, sep="-"), val)
      new_table <- rbind(new_table, new_row)
      
      sum <- 0
      count <- 0
    }
  }
  
  Sys.setlocale("LC_TIME", "C")
  for(r in 1:nrow(new_table))
  {
    date_str <- new_table[r,1]
    date <- my(date_str)
    formatted_date <- format(date, "%Y-%m")
    
    new_table[r,1] <- formatted_date
  }
  
  today <- Sys.Date()
  new_table$data_source <- "StatsCan"
  new_table[,"_lastupdate"] <- today
  colnames(new_table)[2] <- "Estimated variable mortgage rate"
  colnames(new_table)[1] <- "Date_Range"
  new_table$Municipality <- "British Columbia"
  new_table[,2] <- as.numeric(new_table[,2])
  
  write.xlsx(new_table, out_path,row.names = FALSE)
  print(paste("Exported Table to:", out_path))
}

prep_Avg_Month_Pay <- function(table_path, out_path, muni_xlsx,value_col)
{
  tryCatch(
    source("utils.R"),
    
    error = function(e)
    {
      print("utils.R not found, Make sure to set your working directory to the folder containing utils.R")
      stop()
    }
  )
  
  inp_table <- read.csv(table_path, header = FALSE)
  inp_table <- t(inp_table)
  
  for (rw in 1:nrow(inp_table))
  {
    date_str <- inp_table[rw,1]
    date_str <- sub("Q", " Q", date_str)
    inp_table[rw,1] <- date_str
  }
  colnames(inp_table) <- inp_table[1,]
  inp_table <- inp_table[-1,]
  
  inp_table <- add_munis(inp_table, muni_xlsx, "British Columbia", 2)
  
  col_names <- c("Date_Range","Municipality", value_col)
  new_table <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(new_table) <- col_names
  
  for (x in 1:nrow(inp_table))
  {
    for (y in 2:ncol(inp_table))
    {
      muni <- colnames(inp_table)[y]
      date <- inp_table[x,1]
      val <- inp_table[x,y]
      
      new_row <- c(date, muni, val)
      new_table <- rbind(new_table, new_row)
    }
  }
  
  for(r_comma in 1:nrow(new_table))
  {
    #removes thousand separator comma's 
    x <- new_table[r_comma,3]
    x <- gsub(",", "",x)
    new_table[r_comma,3] <- x
  }
  new_table[,3] <- as.numeric(new_table[,3])
  
  today <- Sys.Date()
  new_table$data_source <- "StatsCan"
  new_table[,"_lastupdate"] <- today
  colnames(new_table)[3] <- value_col
  colnames(new_table)[2] <- "Municipality"
  colnames(new_table)[1] <- "Date_Range"
  
  
  
  muni_names <- c()
  muni_tb <- read.xlsx(muni_xlsx,sheet_num)
  
  for (r in 1:nrow(muni_tb))
  {
    muni_name <- muni_tb[r,1]
    if (!(muni_name %in% current_col_names))
    {
      muni_names <- c(muni_names, muni_name)
    }
  }
  
  #Add is BC Value field
  new_table$`Used BC Value` <- NA
  
  for (rw in 1:nrow(in_table))
  {
    if (new_table[rw,2] == "British Columbia")
    {
      new_table$`Used BC Value`[rw] <- "YES"
    } else if (new_table[rw,2] %in% muni_names)
    {
      new_table$`Used BC Value`[rw] <- "YES"
    } else
    {
      new_table$`Used BC Value`[rw] <- "NO"
    }
  }
  
  write.xlsx(new_table, out_path,row.names = FALSE)
  print(paste("Exported Table to:", out_path))
}
