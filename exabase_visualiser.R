# Load necessary libraries
library(DBI)
library(RSQLite)
library(dplyr)
library(shiny)
library(leaflet)
library(ggplot2)
library(DT)

#read user modified csv with info
csv_user <- read.csv("csv_user.csv", header = TRUE)
db_path <- as.character(csv_user$db_name)
coll_owner <- as.character(csv_user$coll_owner)
is_individual <- as.character(csv_user$coll_individual)

#### Step 1: Query SQLite Database and Data Manipulation ####
# Connect to SQLite database

con <- dbConnect(SQLite(), db_path)

if (is_individual=="yes")
{
  # Query the database
  query <- "SELECT 
    t1.id,
    t1.usageKey,
    t1.num_m,
    t1.num_f,
    t1.num_nosex,
    t1.num_mol,
    t1.identifiedBy,
    t1.dateIdentified,
    t1.countryCode,
    t1.stateProvince,
    t1.locality,
    t1.decimalLatitude,
    t1.decimalLongitude,
    t1.elevation,
    t1.eventDate,
    t1.recordedBy,
    t1.institutionID,
    t1.image,
    t3.'order',
    t3.family,
    t3.subgenus,
    t3.genus,
    t3.canonicalName,
    t3.authorship
FROM 
    records t1
LEFT JOIN taxonomy t3 ON t3.usageKey = t1.usageKey;"
  data_orig <- dbGetQuery(con, query)
  query <- "SELECT 
    t1.usageKey,
    t1.countryCode,
    t1.stateProvince,
    t1.locality,
    t1.decimalLatitude,
    t1.decimalLongitude,
    t1.eventDate,
    t1.recordedBy,
    t1.institutionID,
    t2.collection_id,
    t2.sex,
    t2.preservation,
    t2.bodypart,
    t2.localisation,
    t2.image,
    t3.'order',
    t3.family,
    t3.genus,
    t3.canonicalName,
    t3.authorship
FROM 
    records t1
LEFT JOIN taxonomy t3 ON t3.usageKey = t1.usageKey
LEFT JOIN molecular t2 ON t2.linking_id = t1.id;"
  data_molecular_orig <- dbGetQuery(con, query)
} else 
{
  # Query the database
  query <- "SELECT 
    t1.id,
    t1.usageKey,
    t1.num_m,
    t1.num_f,
    t1.num_nosex,
    t1.num_mol,
    t1.identifiedBy,
    t1.dateIdentified,
    t1.countryCode,
    t1.stateProvince,
    t1.locality,
    t1.decimalLatitude,
    t1.decimalLongitude,
    t1.elevation,
    t1.eventDate,
    t1.recordedBy,
    t1.institutionID,
    t1.image,
    t3.'order',
    t3.family,
    t3.subgenus,
    t3.genus,
    t3.canonicalName,
    t3.authorship
FROM 
    records t1
LEFT JOIN taxonomy t3 ON t3.usageKey = t1.usageKey;"
  data_orig <- dbGetQuery(con, query)
}

# Disconnect from database
dbDisconnect(con)


#### reformats table ####

#coerce to numeric a series of columns (sqlite import makes them character...)
data_orig <- data_orig %>% mutate_at(c("num_f","num_m","num_nosex","num_mol","decimalLatitude","decimalLongitude"), as.numeric)
data_molecular_orig <- data_molecular_orig[!is.na(data_molecular_orig$preservation),]
data_molecular_orig <- data_molecular_orig %>% mutate_at(c("decimalLatitude","decimalLongitude"), as.numeric)


# sets to 1 the records with no number of specimens for plotting
data_orig$num_nosex <- ifelse((is.na(data_orig$num_f) | data_orig$num_f==0) & (is.na(data_orig$num_m) | data_orig$num_m==0) & (is.na(data_orig$num_nosex) | data_orig$num_nosex==0), 1, data_orig$num_nosex)
#removes NA that bother calculations
data_orig <- data_orig %>% mutate(num_m = ifelse(is.na(num_m), 0, num_m),
                    num_f = ifelse(is.na(num_f), 0, num_f),
                    num_mol = ifelse(is.na(num_mol), 0, num_mol),
                    num_nosex = ifelse(is.na(num_nosex), 0, num_nosex))
#creates a variable num_all for plotting
for (i in 1:nrow(data_orig)) {
  data_orig$num_all[i] <- sum(data_orig$num_f[i], data_orig$num_m[i], data_orig$num_nosex[i], na.rm = TRUE)
  data_orig$num_dry[i] <- (data_orig$num_all[i]-data_orig$num_mol[i])
}


#rearrange columns
data <- data_orig %>% select(id, family, genus, canonicalName, authorship, identifiedBy, dateIdentified, num_m, num_f, num_nosex, num_all, num_dry, num_mol, countryCode,
                             stateProvince,locality,decimalLatitude,decimalLongitude,elevation,eventDate,recordedBy,institutionID,image)

data_molecular <- data_molecular_orig %>% select(collection_id, family, genus, canonicalName, authorship, sex, countryCode,
                                                 stateProvince,locality,decimalLatitude,decimalLongitude,eventDate,recordedBy,institutionID, preservation, bodypart, localisation, image)

# creates a link field
for (i in 1:nrow(data)) {
  if (!is.na(data$image[i]) & nchar(data$image[i]>2))
  {
    data$image[i] <- paste0(
      '<a href="images/', basename(data$image[i]), '" target="_blank">Open File</a>'
    )
  }
}

# creates a link field
for (i in 1:nrow(data_molecular)) {
  if (!is.na(data_molecular$image[i]) & nchar(data_molecular$image[i]>2))
  {
    data_molecular$image[i] <- paste0(
      '<a href="images/', basename(data_molecular$image[i]), '" target="_blank">Open File</a>'
    )
  }
}


#### Step 2: Shiny App ####

# adds path to images for displaying in the app
addResourcePath("images","www/images")

ui <- fluidPage(
  titlePanel(div(column(width = 6, h2("Exabase visualizer")), 
                 column(width = 6, tags$img(src = "logo_exabase.png"), align="right")),
             windowTitle="MyPage"
  ),
  
  fluidRow(
    # Checkboxes for column selection on the left
    column(
      1,
      uiOutput("column_selector")
    ),
    
    # Filters in the middle
    column(
      2,
      uiOutput("filters")
    ),
    
    # Visualisation on the right
    column(
      9,
      fluidRow(
        column(6,
               radioButtons(
                 "viz_choice", "Choose Visualization:",
                 choices = c("Map" = "map", "Stats by taxon" = "stats", "Filtered Table" = "table", "Collection" = "collection"),
                 inline = TRUE
               )
        ),
        column(6,
               conditionalPanel(
                 condition = "input.viz_choice == 'table'",
                 radioButtons(
                   "data_choice", "Choose data source:",
                   choices = c("Records" = "records", "Molecular" = "molecular"),
                   inline = TRUE
                 )
               )
        )
      ),
      br(),
      uiOutput("viz_output")
    )
  )
)

server <- function(input, output, session) {
  
  output$column_selector <- renderUI({
    # Determine which columns should be pre-selected
    mandatory_columns <- switch(input$viz_choice,
                                "map" = c("locality", "decimalLatitude", "decimalLongitude", "num_all"),
                                "stats" = c("countryCode", "eventDate", "elevation","institutionID", "num_all", "num_mol"),
                                "collection" = c("canonicalName", "institutionID", "num_all","num_dry", "num_mol"),
                                character(0)
    )
    
    req(input$viz_choice)
    if (input$viz_choice == "table") req(input$data_choice)  # Only needed when "table" is selected
    
    current_choices <- if (input$viz_choice == "table" && input$data_choice == "molecular") {
      names(data_molecular)
    } else {
      names(data)
    }
    
    checkboxGroupInput(
      "columns",
      "Select Columns:",
      choices = if (input$viz_choice == "table" && input$data_choice == "molecular") {
        names(data_molecular)
      } else {
        names(data)
      },
      selected = unique(c(mandatory_columns))
    )
  })
  
  # Switch dataset based on user selection
  selected_data <- reactive({
    if (input$viz_choice == "table" && input$data_choice == "records") {
      data
    } else if (input$viz_choice == "table" && input$data_choice == "molecular") {
      data_molecular
    } else {
      data
    }
  })
  
  # Reactive dataframe filtered by user selections
  filtered_data <- reactive({
    req(input$columns)
    
    filtered <- selected_data() %>% select(all_of(input$columns))
    
    for (col in input$columns) {
      filter_values <- input[[paste0("filter_", col)]]
      if (!is.null(filter_values) && filter_values != "") {
        
        # Check if filter is in the form exp(...)
        if (grepl("^exp\\(.*\\)$", filter_values)) {
          # Extract expression content
          expr_inside <- sub("^exp\\((.*)\\)$", "\\1", filter_values)
          expr_text <- paste0(".data[['", col, "']] ", expr_inside)
          
          # Parse and evaluate
          expr <- rlang::parse_expr(expr_text)
          filtered <- filtered %>% filter(!!expr)
          
        } else {
          # Otherwise, treat as normal text search
          filter_values <- unlist(strsplit(filter_values, ","))
          filtered <- filtered %>%
            filter(Reduce(`|`, lapply(filter_values, function(val) {
              grepl(val, .data[[col]], ignore.case = TRUE)
            })))
        }
      }
    }
    
    return(filtered)
  })
  
  # Pre-checking columns based on visualization choice
  observe({
    mandatory_columns <- switch(input$viz_choice,
                                "map" = c("locality", "decimalLatitude", "decimalLongitude", "num_all"),
                                "stats" = c("countryCode", "eventDate", "elevation","institutionID", "num_all", "num_mol"),
                                "collection" = c("canonicalName", "institutionID","num_all","num_dry","num_mol"),
                                "table" = character(0),
                                character(0)
    )
    
    updateCheckboxGroupInput(
      session,
      "columns",
      selected = mandatory_columns
    )
  })
  
  # Dynamic UI for filters
  output$filters <- renderUI({
    req(input$columns)
    
    tagList(
      h5("Filters"),
      helpText("Type plain text for exact/partial matching, or exp(your_expression)"),
      
      # Generate one text box per selected column
      lapply(input$columns, function(col) { textInput(paste0("filter_", col), paste("Filter", col, ":")) })
    )
  })
  
  output$map <- renderLeaflet({
    req(input$viz_choice == "map")
    
    df_map <- filtered_data()
    
    required_columns <- c("locality", "decimalLatitude", "decimalLongitude", "num_all")
    if (!all(required_columns %in% colnames(df_map))) {
      showNotification("Map cannot be rendered: Missing required columns.", type = "error")
      return(NULL)
    }
    
    df_map <- df_map %>%
      group_by(locality, decimalLatitude, decimalLongitude) %>%
      summarise(num = sum(num_all, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df_map) == 0) {
      showNotification("No data available for the map visualization after filtering.", type = "error")
      return(NULL)
    }
    
    
    # --- Calculate bbox and zoom ---
    lat_range <- range(df_map$decimalLatitude, na.rm = TRUE)
    lon_range <- range(df_map$decimalLongitude, na.rm = TRUE)
    
    center_lat <- mean(lat_range)
    center_lon <- mean(lon_range)
    
    extent <- max(diff(lat_range), diff(lon_range))
    
    # Very basic heuristic: smaller extent = higher zoom
    get_zoom_level <- function(extent) {
      if (extent > 100) return(2)
      else if (extent > 50) return(3)
      else if (extent > 25) return(4)
      else if (extent > 10) return(5)
      else if (extent > 5) return(6)
      else if (extent > 2) return(7)
      else if (extent > 1) return(8)
      else return(9)
    }
    
    zoom_level <- get_zoom_level(extent)
    
    # --- Create the map ---
    
    if (input$offline_mode) {
      addResourcePath("mytiles", "www/tiles_Google")
      map <- leaflet(df_map) %>%
        addTiles(urlTemplate = "/mytiles/{z}_{x}_{y}.png") %>% 
        setView(lng = center_lon, lat = center_lat, zoom = zoom_level)
    } else {
      map <- leaflet(df_map) %>% setView(lng = center_lon, lat = center_lat, zoom = zoom_level) %>%
        addProviderTiles(providers$OpenStreetMap)
    }
    
    map %>%
      addCircleMarkers(
        lng = ~decimalLongitude, lat = ~decimalLatitude,
        radius = ~ifelse(num < 10, num, 15),
        color = "blue", popup = ~locality
      )
  })
  
  
 # Stats by Species Visualization: Barplot and Data Preparation
 output$barplot <- renderPlot({
  req(input$viz_choice == "stats")  # Ensure "Stats by Species" is selected
  
  # Step 1: Use already filtered data
  df_stats <- filtered_data()  # Get the filtered dataframe from the app logic
  
  # Step 2: Check for required columns
  required_columns <- c("countryCode", "eventDate", "num_all", "num_mol")
  if (!all(required_columns %in% colnames(df_stats))) {
    showNotification("Barplot cannot be rendered: Missing required columns (nation, date, num_mol, num_all).", type = "error")
    return(NULL)
  }
  
  # Step 3: Filter and Aggregate Data
  
  #prepare eventDate field
  for (i in 1:nrow(df_stats)) {
    if (grepl("/",df_stats[i,"eventDate"],fixed=TRUE)) 
    {
      date1 <- unlist(strsplit(df_stats[i,"eventDate"],"/"))[1]
      date2 <- unlist(strsplit(df_stats[i,"eventDate"],"/"))[2]
      if (lengths(regmatches(date1, gregexpr('-', date1)))==1) {df_stats$eventDate[i] <- NA}
      else 
      {
        if (unlist(strsplit(date1,"-"))[2]==unlist(strsplit(date2,"-"))[2])
        {df_stats$eventDate[i] <- unlist(strsplit(date1,"-"))[2]}
        else
        {
          range_dates <- as.integer(diff(range(as.Date(c(date1,date2)))))
          if (range_dates > 20) {df_stats$eventDate[i] <- NA}
          else {df_stats$eventDate[i] <- unlist(strsplit(date1,"-"))[2]}
        }
      }
    }
    else if (lengths(regmatches(df_stats$eventDate[i], gregexpr('-', df_stats$eventDate[i])))==0)
    {
      df_stats$eventDate[i] <- NA
    }
    else {df_stats$eventDate[i] <- unlist(strsplit(df_stats$eventDate[i],"-"))[2]}
  }

  
  df_barplot <- df_stats %>%
    select(eventDate, num_all) %>%  # Keep only relevant columns
    group_by(eventDate) %>%        # Group by date
    summarise(num = sum(num_all, na.rm = TRUE), .groups = "drop")  # Sum num_all into num
  
  # Step 4: Prepare Data for Barplot
  df_barplot <- df_barplot %>%
    group_by(eventDate) %>%  # Group by month
    summarise(abundance = sum(num, na.rm = TRUE), .groups = "drop")  # Sum num into abundance

  
  
  # Step 5: Create the Barplot
  ggplot(df_barplot, aes(x = as.numeric(eventDate), y = abundance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "Month", y = "Abundance") +
    theme(axis.title.x = element_text(size=18), axis.title.y = element_text(size=18),
          axis.text.x = element_text(size=15), axis.text.y = element_text(size=15))
})

  
 output$altitude <- renderPlot({
   req(input$viz_choice == "stats")  # Ensure "Stats by Species" is selected
   
   # Step 1: Use already filtered data
   df_stats <- filtered_data()  # Get the filtered dataframe from the app logic
   
   # Step 2: Check for required columns
   required_columns <- c("countryCode", "eventDate", "elevation", "num_all", "num_mol")
   if (!all(required_columns %in% colnames(df_stats))) {
     showNotification("Barplot cannot be rendered: Missing required columns (countryCode, eventDate, elevation, num_all, num_mol).", type = "error")
     return(NULL)
   }
   
   for (i in 1:nrow(df_stats)) {
     if (grepl("-",df_stats$elevation[i]))
     {
       df_stats$elevation[i] <- mean(as.numeric(unlist(strsplit(df_stats$elevation[i],"-"))[1]),as.numeric(unlist(strsplit(df_stats$elevation[i]),"-"))[2])
     }
     else if (df_stats$elevation[i] == 0) {df_stats$elevation[i] <- NA}
     else {df_stats$elevation[i] <- as.numeric(df_stats$elevation[i])}
   }
   
   
   df_alt <- df_stats %>%
     group_by(elevation) %>%  # Group by altitude
     summarise(abundance = sum(num_all, na.rm = TRUE), .groups = "drop")  # Sum num into abundance
   
   bins <- cut(df_alt$elevation,
               breaks = c(0, 500, 1000, 1500,2000,2500,3000),
               right = FALSE,  # left-inclusive, right-exclusive: [a, b)
               labels = c("0–500", "500–1000", "1000–1500", "1500-2000", "2000-2500", "2500-3000"))
   df_alt <- data.frame(elevation = df_alt$elevation, abundance = df_alt$abundance, bins = bins)
   
   ggplot(df_alt, aes(x = bins, y = abundance)) +
     geom_bar(width=.7, stat='identity', fill = "#E69F00") +
     labs(x = "Elevation", y = "Abundance") +
     theme(axis.title.x = element_text(size=18), axis.title.y = element_text(size=18),
           axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), 
           aspect.ratio = 1/2.8)+
     coord_flip()
 })
 
 output$text_stats <- renderDataTable({
   req(input$viz_choice == "stats") # Ensure we only render when stats is selected
   
   df_coll <- filtered_data() # Fetch filtered data
   
   #makes all non-owner collection == "others" for plotting
   df_coll$institutionID <- ifelse(df_coll$institutionID=="coll. Poloni", "coll. Poloni", "others")
   
   # Group and summarize collection samples and other samples in two columns
   df_coll <- df_coll %>%
     group_by(countryCode, institutionID) %>%
     summarise(
       n_mol = sum(num_mol, na.rm = TRUE),
       n_all = sum(num_all, na.rm = TRUE),
       .groups = "drop"
     )
   
   
   #put the counts in others collection as a separate column
   df_coll$n_dry <- NA
   for (i in 1:nrow(df_coll)) {
     if (df_coll$institutionID[i]=="coll. Poloni")
     {
       df_coll$n_dry[i] <- df_coll$n_all[i]-df_coll$n_mol[i]
     }
     else
     {
       df_coll$n_dry[i] <- 0
     }
   }
   
   
   #merges the non-owner collection and selects only useful columns
   df_coll <- df_coll %>% select(countryCode, n_all, n_dry, n_mol) %>% group_by(countryCode) %>% summarise(
     n_all=sum(n_all), n_dry=sum(n_dry), n_mol=sum(n_mol)
   )
   
   df_coll <- as.data.frame(df_coll) #coerce to df for rendering with datatable
   
   datatable(df_coll, rownames = FALSE, escape = FALSE, options = list(scrollY = "250px", paging=FALSE)) %>%
     formatStyle(
       "n_dry",
       backgroundColor = styleInterval(
         c(0, 3, 8),
         c("#ff6961","#ffb347", "#ffff66", "#77dd77")
       )
     ) %>%
     formatStyle(
       "n_mol",
       backgroundColor = styleInterval(
         c(0, 3, 8),
         c("#ff6961","#ffb347", "#ffff66", "#77dd77")
       )
     )
 })
 
 
 # Filtered table visualization
 output$table <- renderDataTable({
   req(input$viz_choice == "table") # Only render table when selected
   df <- filtered_data()
   
   if (nrow(df) == 0) {
     return(data.frame("No data matches the filters applied."))
   }
   
   datatable(df, rownames = FALSE, escape = FALSE, options = list(scrollY = "500px", paging=FALSE))
 })
 
 
 #add a download for the filtered table
 output$download_filtered <- downloadHandler(
   filename = function() {
     paste("filtered_data_", Sys.Date(), ".csv", sep = "")
   },
   content = function(file) {
     filtered_table <- filtered_data()
     write.csv(filtered_table, file, row.names = FALSE)
   }
 )
  
 # Collection visualization
 output$collection <- renderDataTable({
   req(input$viz_choice == "collection") # Only render collection when selected
   
   coll_tab <- filtered_data()
   coll_tab <- coll_tab[coll_tab$institutionID=="coll. Poloni",]
   
   required_columns <- c("canonicalName", "institutionID","num_all","num_dry","num_mol")
   if (!all(required_columns %in% colnames(coll_tab))) {
     showNotification("Missing required columns (canonicalName, institutionID, num_all, num_dry, num_mol).", type = "error")
     return(NULL)
   }
   
   if (nrow(coll_tab) == 0) {
     return(data.frame("No data matches the filters applied."))
   }
   
   coll_tab <- coll_tab %>% group_by(canonicalName) %>% 
     summarise(num_all=sum(num_all), num_dry=sum(num_dry), num_mol=sum(num_mol))
   
   
   datatable(coll_tab, rownames = FALSE, escape = FALSE, options = list(scrollY = "500px", paging=FALSE)) %>%
     formatStyle(
       "num_dry",
       backgroundColor = styleInterval(
         c(0, 3, 8),
         c("#ff6961","#ffb347", "#ffff66", "#77dd77")
       )
     ) %>%
     formatStyle(
       "num_mol",
       backgroundColor = styleInterval(
         c(0, 3, 8),
         c("#ff6961","#ffb347", "#ffff66", "#77dd77")
       )
     )
 })
 
 
 #add a download for the filtered table
 output$download_collection <- downloadHandler(
   filename = function() {
     paste("collection_data_", Sys.Date(), ".csv", sep = "")
   },
   content = function(file) {
     filtered_collection <- filtered_data()
     write.csv(filtered_collection, file, row.names = FALSE)
   }
 )
  
  
 # Visualization output
 output$viz_output <- renderUI({
   req(input$viz_choice)
   
   if (input$viz_choice == "map") {
     fluidRow(
       column(12,
              checkboxInput("offline_mode", "Use offline map tiles", value = FALSE),
              leafletOutput("map", height = "600px")
       )
     )
   } else if (input$viz_choice == "stats") {
     tagList(
       fluidRow(
         column(6, plotOutput("barplot")),
         column(6, dataTableOutput("text_stats"))
       ),
       fluidRow(
         column(6, plotOutput("altitude"))
       ))
   } else if (input$viz_choice == "collection") {
     tagList(
       fluidRow(
         column(12, downloadButton("download_filtered", "Download CSV", class = "btn-primary"),
                br(),br())
       ),
       dataTableOutput("collection")  # Ensure table is rendered after the button
     )
   } else if (input$viz_choice == "table") {
     req(input$data_choice)
     
     if (input$data_choice == "molecular") {
       tagList(
         fluidRow(
           column(2, downloadButton("download_filtered", "Download CSV", class = "btn-primary"))
         ),
         br(),
         dataTableOutput("table")
       )
     } else {
       tagList(
         fluidRow(
           column(2, downloadButton("download_filtered", "Download CSV", class = "btn-primary"))
         ),
         br(),
         dataTableOutput("table")
       )
     }
   }
 })
}

# Run the Shiny app
shinyApp(ui, server)
