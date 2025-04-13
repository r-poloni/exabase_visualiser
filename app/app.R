# Load necessary libraries
library(dplyr)
library(leaflet)
library(ggplot2)
library(DT)
library(shiny)


#### Step 1: Import csv ####

data <- read.csv("example_data.csv", header=TRUE)

#### Step 2: Filter the data and prepare for visualisation ####

#coerce to numeric a series of columns (sqlite import makes them character...)
cols_num <- c("num_f","num_m","num_nosex","given_long","given_lat")
# data[cols_num] <- sapply(df[cols_num], as.numeric)
data <- data %>% mutate_at(c("num_f","num_m","num_nosex","given_long","given_lat"), as.numeric)

# sets to 1 the records with no number of specimens for plotting
data$num_nosex <- ifelse((is.na(data$num_f) | data$num_f==0) & (is.na(data$num_m) | data$num_m==0) & (is.na(data$num_nosex) | data$num_nosex==0), 1, data$num_nosex)
#creates a variable num_all for plotting
for (i in 1:nrow(data)) {
  data$num_all[i] <- sum(data$num_f[i], data$num_m[i], data$num_nosex[i], na.rm = TRUE)
}

#creates a variable molecular for plotting (0 = no DNA specimen, 1 = yes)
data$molecular <- ifelse(data$preservation=="alcol 96", 1, 0)


#### Run shiny app to visualize ####

# Step 3: Shiny App
ui <- fluidPage(
  titlePanel("Exabase visualizer"),
  
  fluidRow(
    # Checkboxes for column selection on the left
    column(
      1,
      checkboxGroupInput(
        "columns",
        "Select Columns:",
        choices = names(data),
        selected = c("location", "given_lat", "given_long", "num_all", "nation", "date", "molecular")
      )
    ),
    
    # Filters in the middle
    column(
      2,
      uiOutput("filters")
    ),
    
    # Map/visualization on the right
    column(
      9,
      radioButtons(
        "viz_choice", "Choose Visualization:",
        choices = c("Map" = "map", "Stats by Species" = "stats", "Filtered Table" = "table"),
        inline = TRUE
      ),
      uiOutput("viz_output")
    )
  )
)

server <- function(input, output, session) {
  # Reactive dataframe filtered by user selections
  filtered_data <- reactive({
    req(input$columns) # Ensure at least one column is selected
    
    # Start with selected columns
    filtered <- data %>% select(all_of(input$columns))
    
    # Apply dynamic filters
    for (col in input$columns) {
      filter_values <- input[[paste0("filter_", col)]]
      if (!is.null(filter_values) && filter_values != "") {
        # Split comma-separated values into a vector
        filter_values <- unlist(strsplit(filter_values, ","))
        filtered <- filtered %>% filter(.data[[col]] %in% filter_values)
      }
    }
    
    return(filtered)
  })
  
  # Pre-checking columns based on visualization choice
  observe({
    mandatory_columns <- switch(input$viz_choice,
                                "map" = c("locality", "given_lat", "given_long", "num_all"),
                                "stats" = c("nation", "date", "source", "molecular", "num_all"),
                                character(0)
    )
    
    # Update the selected columns, ensuring mandatory ones are checked
    updateCheckboxGroupInput(
      session,
      "columns",
      selected = unique(c(input$columns, mandatory_columns))
    )
  })
  
  # Dynamic UI for filters
  output$filters <- renderUI({
    req(input$columns)
    lapply(input$columns, function(col) {
      textInput(paste0("filter_", col), paste("Filter", col, "(comma-separated values):"))
    })
  })
  
  # Map visualization
  output$map <- renderLeaflet({
    req(input$viz_choice == "map")
    
    df_map <- filtered_data()
    
    # Check if required columns are present
    required_columns <- c("location", "given_lat", "given_long", "num_all")
    if (!all(required_columns %in% colnames(df_map))) {
      showNotification("Map cannot be rendered: Missing required columns (location, given_lat, given_long, num_all).", type = "error")
      return(NULL)
    }
    
    # Group and summarize data for the map
    df_map <- df_map %>%
      group_by(location, given_lat, given_long) %>%
      summarise(num = sum(num_all, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df_map) == 0) {
      showNotification("No data available for the map visualization after filtering.", type = "error")
      return(NULL)
    }
    
    # Add size categories
    df_map$size <- cut(
      df_map$num, breaks = c(-Inf, 5, 10, Inf),
      labels = c("small", "medium", "large")
    )
    
    # Create the map
    leaflet(df_map) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addCircleMarkers(
        lng = ~given_long, lat = ~given_lat,
        radius = ~ifelse(size == "small", 5, ifelse(size == "medium", 10, 15)),
        color = "blue", popup = ~location
      )
  })
  
 # Stats by Species Visualization: Barplot and Data Preparation
 output$barplot <- renderPlot({
  req(input$viz_choice == "stats")  # Ensure "Stats by Species" is selected
  
  # Step 1: Use already filtered data
  df_stats <- filtered_data()  # Get the filtered dataframe from the app logic
  
  # Step 2: Check for required columns
  required_columns <- c("nation", "date", "molecular", "num_all")
  if (!all(required_columns %in% colnames(df_stats))) {
    showNotification("Barplot cannot be rendered: Missing required columns (nation, date, molecular, num_all).", type = "error")
    return(NULL)
  }
  
  # Step 3: Filter and Aggregate Data
  df_stats <- df_stats %>%
    select(nation, date, molecular, num_all) %>%  # Keep only relevant columns
    group_by(nation, date, molecular) %>%        # Group by nation, date, molecular
    summarise(num = sum(num_all, na.rm = TRUE), .groups = "drop") %>%  # Sum num_all into num
    mutate(date = sub("^.*_([0-9]{2})_.*$", "\\1", date))  # Extract the month from the date
  
  # Step 4: Prepare Data for Barplot
  df_barplot <- df_stats %>%
    group_by(date) %>%  # Group by month
    summarise(abundance = sum(num, na.rm = TRUE), .groups = "drop")  # Sum num into abundance
  
  
  # Step 5: Create the Barplot
  ggplot(df_barplot, aes(x = as.numeric(date), y = abundance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "Month", y = "Abundance", title = "Monthly Abundance Barplot") +
    theme_minimal()
})

  
 output$text_stats <- renderPrint({
   req(input$viz_choice == "stats") # Ensure we only render when stats is selected
   
   df_stats <- filtered_data() # Fetch filtered data
   
   #makes all non-Poloni collection == "others" for plotting
   df_stats$source <- ifelse(df_stats$source=="coll. Poloni", "coll. Poloni", "others")

   # Group and summarize molecular samples and other samples in two columns
   df_stats <- df_stats %>%
     group_by(nation, source) %>%
     summarise(
       molecular = sum(molecular, na.rm = TRUE),
       others = sum(num_all, na.rm = TRUE),
       .groups = "drop"
     )


   #put the counts in others collection as a separate column
   df_stats$collection <- NA
   for (i in 1:nrow(df_stats)) {
     if (df_stats$source[i]=="coll. Poloni")
     {
       df_stats$collection[i] <- df_stats$others[i] - df_stats$molecular[i]
       df_stats$others[i] <- 0
     }
     else
     {
       df_stats$collection[i] <- 0
       df_stats$source[i] <- "others"}
   }


   #merges the non-Poloni collection and selects only useful columns
   df_stats <- df_stats %>% select(nation, others, collection, molecular) %>% group_by(nation) %>% summarise(
     others=sum(others), collection=sum(collection), molecular=sum(molecular)
   )

   print(df_stats)
 })

  
  # Filtered table visualization
  output$table <- renderDataTable({
    req(input$viz_choice == "table") # Only render table when selected
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(data.frame("No data matches the filters applied."))
    }
    
    df
  })
  
  # Visualization output
  output$viz_output <- renderUI({
    req(input$viz_choice)
    if (input$viz_choice == "map") {
      leafletOutput("map", height = "600px")
    } else if (input$viz_choice == "stats") {
      fluidRow(
        column(6, plotOutput("barplot")),
        column(6, verbatimTextOutput("text_stats"))
      )
    } else {
      dataTableOutput("table")
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
