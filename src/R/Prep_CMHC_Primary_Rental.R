#Takes CMHC data from the Primary Rental Market (RMS) dataset using the cmhc wrapper api from mountainmath https://mountainmath.github.io/cmhc/index.html
#then converts the table to an annual timeframe and makes it suitable for use in GIS

#Required Packages
#---------------------------
#install.packages("cmhc")
#install.packages("tidyr")
#install.packages("xlsx")
#library(cmhc)
#library(tidyr)
#library(xlsx)
#---------------------------

#tablePath: destination folder path for output tables
#series_list: vector of series to query
#vac_dim_list: vector of filters
#muni_Vec: CSD IDs and Names of municipalities in a named character vector (ID = Muni Name)
#d_filter: dimension filter for building type
#r_filter: filter which statistic to be used (bedroom_count_type_desc_en)
#season_filter: filter which season to pull from

#Example
#setwd("C:/Documents/hsms_Scripts")
#tablePath <- "C:/Documents/CMHC_PR"
#prep_CMHC_Primary_Rental(tablePath)

prep_CMHC_Primary_Rental <- function(tablePath, series_list = NULL, vac_dim_list = NULL, muni_vec = NULL, d_filter = NULL, r_filter = NULL, season_filter = NULL)
{
  if(is.null(series_list))
  {
    series_list <- c("Vacancy Rate", "Average Rent", "Average Rent Change", "Median Rent", "Rental Universe", "Summary Statistics")
  }
  if(is.null(vac_dim_list))
  {
    vac_dim_list <- c("Bedroom Type", "Structure Size", "Rent Ranges")
  }
  if(is.null(muni_vec))
  {
    muni_vec <- c("59" = "British Columbia", "5909052" = "Abbotsford",
                  "5915011" = "Delta", "5915022" = "Vancouver",
                  "5915043" = "Port Moody", "5915046" = "North Vancouver - District",
                  "5915055" = "West Vancouver", "5917021" = "Saanich",
                  "5917030" = "Oak Bay", "5917034" = "Victoria",
                  "5933042" = "Kamloops")
  }
  if(is.null(d_filter))
  {
    d_filter <- "Row / Apartment"
  }
  if(is.null(r_filter))
  {
    r_filter <- "Total"
  }
  if(is.null(season_filter))
  {
    season_filter <- "October"
  }
  
  #Initializing static variables
  survey_str <- "Rms"
  breakdown_str <- "Historical Time Periods"
  dimension_str <- "Bedroom Type"
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
  
  #Range Table
  range_table_cols <- c("Classification", "Municipality", "Date_Range", "Range_Less_Than_750", "Reliability_Code_750", "Range_750_999", "Reliability_Code_750_999",
                        "Range_1000_1249","Reliability_Code_1000_1249", "Range_1250_1499","Reliability_Code_1250_1499","Range_1500_plus", "Reliability_Code_1500",
                        "Non_Market_Unknown", "Reliability_Code_nonmarket", "Total", "Reliability_Code_total",
                        "Category", "Row_Apartment", "Data_Source", "_lastupdate")
  range_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(range_table_cols)))
  colnames(range_R_CMHC) <- range_table_cols
  
  #Structure Size Table
  structure_table_cols <- c("Classification", "Municipality", "Date_Range", "Units_3_5", "Reliability_Code_3_5", "Units_6_19", "Reliability_Code_6_19",
                            "Units_20_49", "Reliability_Code_20_49", "Units_50_199", "Reliability_Code_50_199", "Units_200_plus", "Reliability_Code_200",
                            "Total", "Reliability_Code_total", "Category", "Row_Apartment", "Data_Source", "_lastupdate")
  structure_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(structure_table_cols)))
  colnames(structure_R_CMHC) <- structure_table_cols
  
  #Summary Table
  summary_table_cols <- c("Classification", "Municipality", "Date_Range", "Vacancy Rate_Percent", "Reliability_Code_vacrate", "Availability_Rate_Percent",
                          "Reliability_Code_avlrate", "Average_Rent", "Reliability_Code_avgrent", "Median_Rent", "Reliability_Code_medrent",
                          "Percent_Change", "Reliability_Code_perchg", "Units","Category", "Row_Apartment", "Data_Source", "_lastupdate")
  summary_R_CMHC <- data.frame(matrix(nrow = 0, ncol = length(summary_table_cols)))
  colnames(summary_R_CMHC) <- summary_table_cols
  
  indexes_list <- list(combined_val_indexes, range_val_indexes, structure_val_indexes, summary_val_indexes)
  
  #links to script containing functions
  tryCatch(
    source("utils.R"),
    
    error = function(e)
    {
      print("utils.R not found, Make sure to set your working directory to the folder containing utils.R")
      stop()
    }
  )
  
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
                                      filters = list("dwelling_type_desc_en" = d_filter, "season" = season_filter, "bedroom_count_type_desc_en" = r_filter))
            #Build Rent Ranges Table
            range_R_CMHC <- build_table(current_table, range_R_CMHC, vac_dim, muni_name)
            
          } else if (vac_dim == "Structure Size")
          {
            print(paste("Downloading", survey_str, series_str, vac_dim,breakdown_str, muni_ID, d_filter))
            current_table <- get_cmhc(survey_str, series_str, vac_dim, breakdown_str, "Default", muni_ID,
                                      filters = list("dwelling_type_desc_en" = d_filter, "season" = season_filter))
    
            #build Structure Size table
            structure_R_CMHC <- build_table(current_table, structure_R_CMHC, vac_dim, muni_name)
            
          } else
          {
            print(paste("Downloading", survey_str, series_str, vac_dim, breakdown_str, muni_ID, d_filter))
            current_table <- get_cmhc(survey_str, series_str, vac_dim, breakdown_str, "Default", muni_ID,
                                      filters = list("dwelling_type_desc_en" = d_filter, "season" = season_filter))
            #Run table method
            combined_R_CMHC <- build_table(current_table, combined_R_CMHC, vac_dim, muni_name)
      
          }
        }
      } else if (series_str == "Summary Statistics")
      {
          print(paste("Downloading", survey_str, series_str,breakdown_str, muni_ID, bed_str, d_filter))
          current_table <- get_cmhc(survey_str, series_str, NA, breakdown_str, "Default", muni_ID,
                                    filters = list("dwelling_type_desc_en" = d_filter, "season" = season_filter, "bedroom_count_type_desc_en" = r_filter))
          
          #Run Table Method
          summary_R_CMHC <- build_table(current_table, summary_R_CMHC, series_str, muni_name)
      } else
      {
        print(paste("Downloading", survey_str, series_str, dimension_str,breakdown_str, muni_ID, d_filter))
        current_table <- get_cmhc(survey_str, series_str, dimension_str, breakdown_str, "Default", muni_ID,
                                  filters = list("dwelling_type_desc_en" = d_filter, "season" = season_filter))
        #Run Table method
        combined_R_CMHC <- build_table(current_table, combined_R_CMHC, "other", muni_name)
        
      }
    }
  }
  
  print("Completed Table Downloads")
  
  combined_R_CMHC <- fix_stats_vals(combined_R_CMHC)  
  range_R_CMHC <- fix_stats_vals(range_R_CMHC)
  structure_R_CMHC <- fix_stats_vals(structure_R_CMHC)
  summary_R_CMHC <- fix_stats_vals(summary_R_CMHC)
  
  #export 4 tables
  bed_file_name <- file.path(tablePath, "CMHC_PR.xlsx")
  range_file_name <- file.path(tablePath, "CMHC_PR_Rent_Ranges.xlsx")
  structure_file_name <- file.path(tablePath, "CMHC_PR_Structure_Size.xlsx")
  summary_file_name <- file.path(tablePath, "CMHC_PR_Sum_Stats.xlsx")
  
  #export tables
  write.xlsx(combined_R_CMHC, bed_file_name, row.names = FALSE)
  write.xlsx(range_R_CMHC, range_file_name, row.names = FALSE)
  write.xlsx(structure_R_CMHC, structure_file_name, row.names = FALSE)
  write.xlsx(summary_R_CMHC, summary_file_name, row.names = FALSE)
  
  print("Exported tables")
}



