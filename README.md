# exabase_visualiser  
exabase_visualiser is a solution based on R and shiny package to visualise biodiversity data stored in a database structure called exabase: https://github.com/r-poloni/exabase. This database structure is built upon Darwin Core standard and accomodates both individual based records and occurrence based records. 

This app was designed to suit my own needs, which are mainly visualizing the data from natural history collections, from citizen science, and see what the material I have for study and for sequencing through maps and summary statistics. It is particularly useful for people that need to manage a collection of samples for molecular or morphological studies. It is also useful if you want to visualize and explore the data of a certain species or more species from citizen science and those from your own collection, for a paper or project.

## Try it out
A demo version of this app running on the example data (modified for illustrating the app functioning from the data in [Poloni et al. 2023](https://academic.oup.com/zoolinnean/article/200/3/705/7246614?login=false)) is provided with the code and can be accessed here: https://r-poloni.github.io/exabase_visualiser/
The deployment on the github page has been done with the shiny-live package (https://github.com/RamiKrispin/shinylive-r).
Try it out!

## Installation instructions  
Once cloned the repository, you will find the file exabase_visualiser.R that runs the app in the app/ directory. It requires a few R packages to be installed on your machine. If you use Rstudio, just type:  
```
install.packages(c("DBI","RSQLite","dplyr","shiny","leaflet","ggplot2","DT"))
```
To test if the app is working with the test data click on "Run App" in the top right corner of the editor tab. This should open a window with the app in your R session.

For how to install R and Rstudio, please see: https://posit.co/download/rstudio-desktop/  

## Using the app with your data
If you already have a database aligning with the [exabase specs](https://github.com/r-poloni/exabase/blob/main/README.md) you just have to modify a few things:
1) "csv_user.csv" file. It is the file where you store the information specific to your app, namely a) text: the path to the database file ("example_exabase.db" in the example), b) text: the name of the collection of the user for the "Collection" tab visualisation ("coll. Poloni" in the example), c) text: if there is a table like the "Molecular" table containing samples registered in the database individually. Can be "yes" or "no".
2) The app has been made to work offline. In this case, to render the map you will need to download the map tiles (google ones work nicely) in the subfolder www/tiles_google/ of the app/ directory. This can be quite complex and long if you need a high resolution, because you will need to download tens of thousands of tiles. If you use R, one solution is here: https://github.com/lmikolajczak/wms-tiles-downloader
3) The app has the possibility of showing specimen images in the "Filtered table" visualisation under the "image" column. To be displayed, the images must be in the www/images/ subfolder of the app/ directory and their name should match the text in the column "image" of the database table "Records" or "Molecular".


## Usage  

The GUI (Graphic User Interface) that is produced by the app allows to filter the data, and use it to produce different visualizations. The two common components found in all the tabs are
1) **Column selector.** It allows the user to select the coumns he want to use for filtering. Some of them are pre-checked because they are needed by the tab you are using to produce the graphics.  
2) **Filtering text boxes.** These can be used to filter the content of the database. You can either search for an exact match typing directly (e.g. Genus species in the "canonicalName" box to look for a specific species) or using expressions, only available for numeric columns. In the latter case, you need to type exp(your_expression_here). E.g. exp(>10) in the "num_all" column to filter the records with more than 10 individuals.  
<br>
The different tabs in the app are:

- **Map** This is a map of the records, where the size of the dot is proportional to the number of specimens in this location. Clicking on the dot you can visualise the locality name.  
- **Stats by taxon** It is a set of summary graphs that summarise a) the phenology of the taxon selected, b) the altitudinal distribution of observations, c) a table giving the number of specimens of a given taxon available for study per country: "n_all" corresponds to all samples available, "n_dry" to the samples dry-preserved and "n_mol" to the samples available for molecular studies.  
- **Filtered table** Here you can simply visualise the table filtered. The user can choose to visualise the "Records" table or the "Molecular" table.  
- **Collection** This table is a list of taxa (species or subspecies in this case) available in the user collection (ad defined in the "user.csv" file).  
<br>


## If you encounter some troubles

If you have problems running the code, or you would like something similar for your needs, please contact me by email and I will try to answer as soon as possible.  


## Example of the GUI  

**Map**
<img width="1000" height="554" alt="tab1" src="https://github.com/user-attachments/assets/40763761-50cb-4470-86cb-751124c2293f" />


**Stats by species**
<img width="1000" height="599" alt="tab2" src="https://github.com/user-attachments/assets/3e59a26b-56e6-49a7-bdaf-992443003871" />


**Filtered table**
<img width="1000" height="541" alt="tab3" src="https://github.com/user-attachments/assets/cf501024-70a2-41d4-9fc0-294590ccb5d8" />


**Collection**
<img width="1000" height="547" alt="tab4" src="https://github.com/user-attachments/assets/d0940bab-fe6d-4198-8e91-a90fc80a2bc7" />



