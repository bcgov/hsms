# This Document describes the download and table preperation procedures for the BCStats data source.

**Note**: This procedure could be used as a reference for using the associated R Scripts with other data sources

---
**Census Profile**:<br>
Associated R Script - Prep_BC_Stats_Census.R<br>
Data Link - https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm

This script is designed to split each topic of the Census Profile into a seperate table, with the topics being
the file name of the tables. The tool is designed for GIS data analysis, thus the table is transposed so that each
municipality is a row and each "characteristic" is a column.

The Tool takes the tables as input in the format that they are downloaded from StatsCan. Statistics Canada limits each export to 5 geographies
So the tools allows for multiple tables, given that each table contains exactly the same variables. The Tool will take every table provided in the input folder, be sure that only census tables are in the folder.

This script requires you have the utils.R script to run. This script is designed to be run as a function, be sure to look at what the required packages are as well as a description 
of the functions arguments. In order to utilize the util.R script the working directory must be set before running the function using the setwd() function, refer to the example in the script if
needed. 

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

This script is designed to be run as a function, refer to the example at the top of the script for how to call the function and a desctription of the arguments and required packages.

---
**Custom Census table from PLUM**:

Associated R Script - Prep_BCStats_PLUM.R <br>

This script takes a custom census dataset from PLUM and adds the data to the census tables that are created in the prep_BCStats_Census() function.

The script is designed to be run as a function, and the description of arguments and required packages can be found at the top of the script. this script requires that the utils.R and Prep_BCStats_Census.R script are downloaded, and that setwd() is run with the workspace where utils.R as well as Prep_BCStats_Census.R are located. 