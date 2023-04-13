# This Document Describes how to use the CMHC R Scripts to download data from CMHC's [Housing Market Information Portal](https://www03.cmhc-schl.gc.ca/hmip-pimh/en#TableMapChart/1/1/Canada).
 

**Note:** This procedure could be used as a reference for using the associated R Scripts with other data sources.
The following scripts uses a wrapper API created by Jens von Bergmann which allows access to CMHC datasets through R.
Information about the API can be found at https://mountainmath.github.io/cmhc/index.html

The CMHC database can be seen as a hierarchical database going in the order:
<br>Census District -> Survey -> Series -> Dimension/Breakdown -> Filter

Any combination of these variables will result in a unique dataset. These R Scripts work by iterating through
defined lists of each of these variables that the user chooses and combining them into a table as long as the statistics
match. For New Housing Construction all tables look at the same statistics (Single, Semi-Detached, Row, Apartment, Total).
Primary Rental Market has a few series that have different statistics which requires seperate output tables to be created.

---
## **New Housing Construction:**
Associated R Script - Prep_CMHC_New_Construction.R

This tool allows you to choose which datasets to download by defining values in a few lists:

**muniVec** - This is a list of all Census Districts and their GEOUIDs, which can be found using [this](https://geosuite.statcan.gc.ca/geosuite/en/index) tool from statistics Canada 
 

**seriesList** - These are equivalent to the links underneath the New Housing Construction dropdown on the CMHC website.
Note that the names used on the website aren't the same as the strings required in R, and can be queried using
list_cmhc_series() tool as described on Jens API website.

**dimensionStr** - This tool is configured to only look at Dwelling Type, which can be found under "By:" once you are in the series
table view. If you require multiple dimensions alterations to the code will be needed.

**breakdownStr** - This tool only supports "Historical Time Periods" breakdown and will output annual statistics

**filterList** - This is equivalent to the intended markets dropdown in the series table view.

---
## **Primary Rental Market:**

Associated R Script - Prep_CMHC_Rental.R

This tool creates 4 excel tables:
CMHC_PR.xlsx - which is the main export table that combines and dataset with the statistics(Bachelor, 1 Bedroom, 2 Bedroom, 3 bedrooms +, and total) along with reliability codes for each

The other 3 tables combine all datasets that include the variables (Rent Ranges, Structure Size, and Summary Statistics), because these variables use different statistics than the rest of the tables in the Rental market survey. 

Because of the variation in statistics this tool is more complex to alter, and more R coding experience may be required to make the changes necessary. 

**muni_vec** - This is a list of all Census Districts and their GEOUIDs, which can be found using [this](https://geosuite.statcan.gc.ca/geosuite/en/index) tool from statistics Canada 

**series_list** - These are equivalent to the links underneath the Primary Rental Market dropdown on the CMHC website. Note that the names used on the website aren't the same as the strings required in R, and can be queried using
list_cmhc_series() tool as described on Jens API website.

**vac_dim_list** - List of filters used in some of the datasets, some of the queries aren't querying all of these filters, and only using dimension_str. These filters can be found under the "By:" section of the series table view

**d_filter/ r_filter/ season_filter** - These are filters that can be selected on the dropdowns above the table in the series table view. 
