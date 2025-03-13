# exabase_visualiser  
exabase_visualise.R is an R script based on shiny package to visualise biodiversity data.  

It was designed to suit my own needs, which are mainly visualizing the data from my collection and from citizen science, and see what is the distribution and the material I have for study and for sequencing through maps and summary statistics. It is particularly useful for people that need to manage a collection of samples for molecular or morphological studies. It is also useful if you want to visualize and explore the data of a certain species or more species from citizen science and those from your own collection, for a paper or project.

## Installation instructions  
Once cloned the repository, the code requires a few R packages to be installed on your machine. If you use Rstudio, just type:  
```
install.packages(c("dplyr","shiny","leaflet","ggplot2","DT"))
```

For how to install R and Rstudio, please see: https://posit.co/download/rstudio-desktop/  

## Input data format  
  
I encourage you to first take a look to the example file (example_data.csv) and try to run the app with this data, to see what the table looks like and how it works. The data table has been produced re-arranging the data from the supplementary_data_s1.xlsx and supplementary_data_s2.xlsx from [Poloni et al. (2023)](https://academic.oup.com/zoolinnean/article/200/3/705/7246614), and contains occurrence data for the genus Stenostoma, together with the data used for molecular analyses.

To avoid problems to the code, I suggest to re-arrange your own data based on the example, but of course it is possible to run the app also with a differently formatted table, with some adjustments. To run with the code provided, the table should at least contain the following fields:
- "genus": taxonomy
- "species": taxonomy
- "nation": nation where the sample has been recorded
- "location": location where the sample has been recorded, usually a toponym (e.g. Lake Garda or Mount Etna) 
- "given_lat": latitude in decimal degrees
- "given_long": longitude in decimal degrees
- "date": to avoid problems in formatting, recurrent with dates, the format is GG_MM_YYYY or, if the day is missing, MM_YYYY
- "source": the source of the occurrence (e.g. GBIF or Museum National d'Histoire Naturelle, Paris)
- "num_m": number of males associated to this observation (only for occurences)
- "num_f": number of females associated to this observation (only for occurences)
- "num_nosex": number of specimens with unknown sex associated to this observation (only for occurences)
- "preservation": how the samples are preserves (only for specimens in collections). For specimens preserved for molecular analyses, the value should be set as "alcol 96"


## Usage  

The GUI (Graphic User Interface) that is produced by the app allows to filter the data, and use it to produce different visualizations:  
- First tab: a map of occurrences, where the size of the dot indicating the occurrence is proportional to the number of specimens in this location  
- Second tab: summarise the phenology of the species/population selected, and summarize the specimens in the database by country and source (e.g. collection or citizen science repository)  
- Third tab: visualize the filtered table  
  
**Importing from SQLite database:** If you want to import from an existing SQLite database, you can easily do that with a few lines of code. I provide a simple example here, importing the table from the table "records" of a database called "database.db":

```
#load two supplementary packages (you will need to install them if they are not already)
library("DBI")
library("RSQLite")

#sets the path of the database file
db_path <- "database.db"
#creates the connection
con <- dbConnect(SQLite(), db_path)

# Query the database
query <- "SELECT
* FROM records;"

#imports the data into a data frame
data <- dbGetQuery(con, query)
# Disconnect from database
dbDisconnect(con)
```

**Importing from Google Sheets** If you want to import from a Google Sheet, I provide an example code here

```
#load googlesheets4 library, of course you will have to install it if is not already
library(googlesheets4)
# Authenticate manually - just run the first time
# If it asks you if you agree to cache OAuth access credentials in the folder ~/Library/Caches/gargle, select "yes"
# it will also open a browser were you will be able to seelct your account
gs4_auth()
#reads the google sheet
data <- read_sheet("sheet_link")
```

## If you encounter some troubles

If you have problems running the code, or you would like something similar for your needs, please contact me by email and I will try to answer as soon as possible.  


## Example of the GUI  

**First tab**
![First_tab_map](https://github.com/user-attachments/assets/24ca691b-fb93-469e-85f2-f3c67d88f48e)

**Second tab**
![Second_tab_summary_stats](https://github.com/user-attachments/assets/063e72f6-895a-4e1f-84e3-f9068e75c186)

**Third tab**
![Third_tab_filtered_table](https://github.com/user-attachments/assets/4c07572a-aae7-4a56-b11f-db2402646919)
