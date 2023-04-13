# This Document describes the download and table preperation procedures for the BCStats data source.

**Note**: This procedure could be used as a reference for using the associated R Scripts with other data sources

---
**Census Profile**:<br>
Associated R Script - Prep_BC_Stats_Census.R
Data Link - https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm

This script is designed to split each topic of the Census Profile into a seperate table, with the topics being
the file name of the tables. The tool is designed for GIS data analysis, thus the table is transposed so that each
municipality is a row and each "characteristic" is a column.

The Tool takes the tables as input in the format that they are downloaded from StatsCan. Statistics Canada limits each export to 5 geographies
So the tools allows for multiple tables, given that each table contains exactly the same variables. By Default the tool takes in 
2 tables (censusTable_a and censusTable_b). To add additional tables, you can create a new variable with the file path of the tables, and add that
variable to the input_table_list.

---
**Core Housing Needs, Housing Suitability, Structural Type**:

Associated R Script - Prep_BC_Stats_TenureTables.R <br>
Data Links:<br> 
DataSet | Link                                                             
---|---
Core Housing Needs | https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=9810024701 
Housing Suitability | https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=9810023701 
Structural Type | https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=9810024001 

This script takes the csv as downloaded from Statistics Canada and alters the table for use in GIS Analysis.

When downloading the table from StatsCan, in the "Customize Layout" tab, the tool requires that Geography is set to Column
and all others are set to Row. After you have selected the desired geographies and variables and clicked apply, select the 
download option: CSV download as displayed.

Once the csv is downloaded, use "save as" to save the file to the desired location. This step properly formats the csv file for some strange
reason, **THE SCRIPT WILL NOT WORK WITHOUT COMPLETING THIS STEP**.