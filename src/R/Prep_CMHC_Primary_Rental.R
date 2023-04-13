#Primary Rental Market Rms

install.packages("cmhc")
install.packages("tidyr")
install.packages("xlsx")
library(cmhc)
library(tidyr)
library(xlsx)

tablePath <- "W:/folder/path/for/output/tables"

#Initializing static variables
survey_str <- "Rms"
breakdown_str <- "Historical Time Periods"
dimension_str <- "Bedroom Type"
#--------------------------------------------
#These Variables can be altered to change which datasets will be amalgamated
#List of series to query
series_list <- c("Vacancy Rate", "Average Rent", "Average Rent Change", "Median Rent", "Rental Universe", "Summary Statistics")
#list of filters
vac_dim_list <- c("Bedroom Type", "Structure Size", "Rent Ranges")
#CSD IDs for each municipalities
muni_vec <- c("5921007" = "Nanaimo", "5917030" = "Oak Bay",
              "5917034" = "Victoria", "5915004" = "Surrey",
              "5915022" = "Vancouver", "5915025" = "Burnaby",
              "5915034" = "Coquitlam", "5915043" = "Port Moody",
              "5915055" = "West Vancouver", "5915075" = "Maple Ridge")
d_filter <- "Row / Apartment"
r_filter <- "Total"
season_filter <- "October"
#-------------------------------------------

category_str <- "Primary Rental Market"
update_str <- Sys.Date()
update_str <- format(update_str, "%Y-%m-%d")
muni_index <- 0

#main Table
export_table_cols <- c("Classification", "Municipality", "Date_Range", "Bachelor", "Reliability_Code_Bach", "One_Bedroom", "Reliability_Code_1bed",
                       "Two_Bedroom", "Reliability_Code_2bed", "Three_Bedroom_plus", "Reliability_Code_3bed", "Total", "Reliability_Code_total",
                       "Category", "Row_Apartment", "Data_Source", "_lastupdate")
combined_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(export_table_cols)))
colnames(combined_R_CMHC) <- export_table_cols
bedroom_types <- c("Bachelor", "1 Bedroom", "2 Bedroom", "3 Bedroom +", "Total")


#Range Table
range_table_cols <- c("Classification", "Municipality", "Date_Range", "Range_Less_Than_750", "Reliability_Code_750", "Range_750_999", "Reliability_Code_750_999",
                      "Range_1000_1249","Reliability_Code_1000_1249", "Range_1250_1499","Reliability_Code_1250_1499","Range_1500_plus", "Reliability_Code_1500",
                      "Non_Market_Unknown", "Reliability_Code_nonmarket", "Total", "Reliability_Code_total",
                      "Category", "Row_Apartment", "Data_Source", "_lastupdate")
range_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(range_table_cols)))
colnames(range_R_CMHC) <- range_table_cols
range_types <- c("Less Than $750", "$750 - $999", "$1,000 - $1,249", "$1,250 - $1,499", "$1,500 +", "Non-Market/Unknown", "Total")


#Structure Size Table
structure_table_cols <- c("Classification", "Municipality", "Date_Range", "Units_3_5", "Reliability_Code_3_5", "Units_6_19", "Reliability_Code_6_19",
                          "Units_20_49", "Reliability_Code_20_49", "Units_50_199", "Reliability_Code_50_199", "Units_200_plus", "Reliability_Code_200",
                          "Total", "Reliability_Code_total", "Category", "Row_Apartment", "Data_Source", "_lastupdate")
structure_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(structure_table_cols)))
colnames(structure_R_CMHC) <- structure_table_cols
structure_types <- c("3-5 Units", "6-19 Units", "20-49 Units", "50-199 Units", "Total")


#Summary Table
summary_table_cols <- c("Classification", "Municipality", "Date_Range", "Vacancy Rate_Percent", "Reliability_Code_vacrate", "Availability_Rate_Percent",
                        "Reliability_Code_avlrate", "Average_Rent", "Reliability_Code_avgrent", "Median_Rent", "Reliability_Code_medrent",
                        "Percent_Change", "Reliability_Code_perchg", "Units","Category", "Row_Apartment", "Data_Source", "_lastupdate")
summary_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(summary_table_cols)))
colnames(summary_R_CMHC) <- summary_table_cols
summary_types <- c("Vacancy Rate (%)", "Availability Rate (%)", "Average Rent ($)", "Median Rent ($)", "% Change", "Units")


indexes_list <- list(combined_val_indexes, range_val_indexes, structure_val_indexes, summary_val_indexes)

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

#Fix Data Frames value columns to make them numeric

fix_vals <- function(in_table, in_indexes)
{
  for (ind in in_indexes)
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
  return(in_table)
}


#function to take input table from cmhc and put into final table for export
#only from 2012-2022
#in_type is a list of value header strings
#in_table is the input table from get_cmhc()
#out_type is a string of the dimension variable
build_table <- function(in_type, in_table, in_master_table, out_type)
{
  five_ch <- FALSE
  six_ch <- FALSE
  
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
      
      #if(is.na(current_val))
      #{
      #  current_val <- ""
      #}
      
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

#----Main Function----------------
#Iterate Through Municipality
for(a in muni_vec)
{
  muni_index <- muni_index + 1
  muni_ID <- as.numeric(names(muni_vec)[muni_index])
  muni_name <- muni_vec[muni_index]
  
  #Iterate through Series
  for (series_str in series_list)
  {
    #different path for vacancy rates
    if(series_str == "Vacancy Rate")
    {
      for (vac_dim in vac_dim_list)
      {
        #more filters for rent ranges
        if (vac_dim == "Rent Ranges")
        {
          
          print(paste("Downloading", survey_str, series_str, vac_dim,breakdown_str, muni_ID, d_filter, r_filter))
          #get CMHC
          current_table <- get_cmhc(survey_str, series_str, vac_dim, breakdown_str, "Default", muni_ID,
                                    filters = list("dwelling_type_desc_en" = d_filter, "season" = "October", "bedroom_count_type_desc_en" = r_filter))
          #Build Rent Ranges Table
          range_R_CMHC <- build_table(range_types,current_table, range_R_CMHC, vac_dim)
          
        } else if (vac_dim == "Structure Size")
        {
          print(paste("Downloading", survey_str, series_str, vac_dim,breakdown_str, muni_ID, d_filter))
          current_table <- get_cmhc(survey_str, series_str, vac_dim, breakdown_str, "Default", muni_ID,
                                    filters = list("dwelling_type_desc_en" = d_filter, "season" = "October"))
  
          #build Structure Size table
          structure_R_CMHC <- build_table(structure_types, current_table, structure_R_CMHC, vac_dim)
          
        } else
        {
          print(paste("Downloading", survey_str, series_str, vac_dim, breakdown_str, muni_ID, d_filter))
          current_table <- get_cmhc(survey_str, series_str, vac_dim, breakdown_str, "Default", muni_ID,
                                    filters = list("dwelling_type_desc_en" = d_filter, "season" = "October"))
          #Run table method
          combined_R_CMHC <- build_table(bedroom_types, current_table, combined_R_CMHC, vac_dim)
    
        }
      }
    } else if (series_str == "Summary Statistics")
    {
        print(paste("Downloading", survey_str, series_str,breakdown_str, muni_ID, bed_str, d_filter))
        current_table <- get_cmhc(survey_str, series_str, NA, breakdown_str, "Default", muni_ID,
                                  filters = list("dwelling_type_desc_en" = d_filter, "season" = "October", "bedroom_count_type_desc_en" = r_filter))
        
        #Run Table Method
        summary_R_CMHC <- build_table(summary_types, current_table, summary_R_CMHC, series_str)
    } else
    {
      print(paste("Downloading", survey_str, series_str, dimension_str,breakdown_str, muni_ID, d_filter))
      current_table <- get_cmhc(survey_str, series_str, dimension_str, breakdown_str, "Default", muni_ID,
                                filters = list("dwelling_type_desc_en" = d_filter, "season" = "October"))
      #Run Table method
      combined_R_CMHC <- build_table(bedroom_types, current_table, combined_R_CMHC, "other")
      
    }
  }
}

print("Completed Table Downloads")

combined_val_indexes <- c(3,4,6,8,10,12)
range_val_indexes <- c(3,4,6,8,10,12,14,16)
structure_val_indexes <- c(3,4,6,8,10,12,14)
summary_val_indexes <- c(3,4,6,8,10,12,14)

combined_R_CMHC <- fix_vals(combined_R_CMHC, combined_val_indexes)  
range_R_CMHC <- fix_vals(range_R_CMHC, range_val_indexes)
structure_R_CMHC <- fix_vals(structure_R_CMHC, structure_val_indexes)
summary_R_CMHC <- fix_vals(summary_R_CMHC, summary_val_indexes)

#export 3 tables
bed_file_name = paste(tablePath, "/CMHC_PR.xlsx", sep = "")
range_file_name = paste(tablePath, "/CMHC_PR_Rent_Ranges.xlsx", sep = "")
structure_file_name = paste(tablePath, "/CMHC_PR_Structure_Size.xlsx", sep = "")
summary_file_name = paste(tablePath, "/CMHC_PR_Sum_Stats.xlsx", sep = "")

#export tables
write.xlsx(combined_R_CMHC, bed_file_name, row.names = FALSE)
write.xlsx(range_R_CMHC, range_file_name, row.names = FALSE)
write.xlsx(structure_R_CMHC, structure_file_name, row.names = FALSE)
write.xlsx(summary_R_CMHC, summary_file_name, row.names = FALSE)

print("Exported CSVs")




