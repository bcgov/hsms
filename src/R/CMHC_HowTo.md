# This Document Describes how to use the CMHC R Scripts to download data from CMHC's [Housing Market Information Portal](https://www03.cmhc-schl.gc.ca/hmip-pimh/en#TableMapChart/1/1/Canada).
 

**Note**: This procedure could be used as a reference for using the associated R Scripts with other data sources.
The following scripts uses a wrapper API created by Jens von Bergmann which allows access to CMHC datasets through R.
Information about the API can be found at https://mountainmath.github.io/cmhc/index.html

The CMHC database can be seen as a hierarchical database going in the order:
<br>Census District -> Survey -> Series -> Dimension/Breakdown -> Filter

Any combination of these variables will result in a unique dataset. These R Scripts work by iterating through
defined lists of each of these variables that the user chooses and combining them into a table as long as the statistics
match. For New Housing Construction all tables look at the same statistics (Single, Semi-Detached, Row, Apartment, Total).
Primary Rental Market has a few series that have different statistics which requires seperate output tables to be created.

---
## **New Housing Construction**:
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