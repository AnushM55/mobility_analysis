# --- 1. Load Libraries ---
# Make sure to install them first if you haven't:
# install.packages(c("shiny", "dplyr", "ggplot2", "lubridate", "forecast", "DT", "readxl", "tidyr", "hms", "shinythemes", "cluster", "stats", "scales"))

# Check if packages are installed, install if necessary, and load them
packages <- c("shiny", "dplyr", "ggplot2", "lubridate", "forecast", "DT", "readxl", "tidyr", "hms", "shinythemes", "cluster", "stats", "scales")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE)) # Load libraries

# --- 2. Configuration ---
# <<<--- IMPORTANT: CHANGE THIS TO YOUR ACTUAL FILE PATH --->>>
# Example for Linux/macOS: file_path <- "/home/your_user/data/Bookings.xlsx"
# Example for Windows: file_path <- "C:/Users/your_user/Documents/Data/Bookings.xlsx"
file_path <- "./Bookings.csv" # <<<--- USER: PLEASE VERIFY THIS PATH

# --- 3. Helper Function to Find Columns ---
# This function tries to find the most likely column based on patterns, making the app less sensitive to exact naming.
find_column_name <- function(conceptual_name, patterns, original_colnames, cleaned_colnames_map) {
  found_original <- NULL
  for (pattern in patterns) {
    # Case-insensitive search using grep
    match_indices <- grep(pattern, original_colnames, ignore.case = TRUE)
    if (length(match_indices) > 0) {
      # Prioritize exact matches if multiple grep matches found
      exact_match_indices <- which(tolower(original_colnames) == tolower(pattern))
      if(length(exact_match_indices) > 0) {
        found_original <- original_colnames[exact_match_indices[1]]
      } else {
        # Otherwise, take the first grep match
        found_original <- original_colnames[match_indices[1]]
      }
      break # Stop searching once a match is found
    }
  }
  
  if (!is.null(found_original)) {
    # Return the 'cleaned' name corresponding to the found original name
    cleaned_name <- cleaned_colnames_map[[found_original]]
    if(!is.null(cleaned_name)) {
      message(paste("Found column for '", conceptual_name, "': Original='", found_original, "', Cleaned='", cleaned_name, "'", sep=""))
      return(cleaned_name)
    } else {
      warning(paste("Found original column '", found_original,"' for concept '", conceptual_name,"' but couldn't find its cleaned name in the map.", sep=""))
      return(NULL)
    }
  } else {
    warning(paste("Could not find a column for concept '", conceptual_name, "' using patterns: ", paste(patterns, collapse=", "), sep=""))
    return(NULL)
  }
}


# --- 4. Load and Preprocess Data ---
app_data <- NULL
error_message <- NULL
column_mapping <- list() # To store the dynamically found cleaned column names

tryCatch({
  if (!file.exists(file_path)) {
    stop(paste("File not found at the specified path:", file_path))
  }
  # Explicitly set col_types for Time column to 'text' to avoid default parsing issues
  # Adjust other types if needed (e.g., 'date' for Date, 'numeric' for Value/Distance)
  raw_data <- read.csv(file_path, colClasses = c("character")) # Start with character, override Time below if needed
  
  # Store original and create cleaned names (safer for R variable names)
  original_names <- colnames(raw_data)
  cleaned_names <- make.names(original_names, unique = TRUE)
  
  # Create a mapping from original names to cleaned names
  cleaned_names_map <- setNames(cleaned_names, original_names)
  
  message("--- Column Name Detection ---")
  # Define search patterns for essential columns
  column_mapping$date <- find_column_name("Date", c("Date"), original_names, cleaned_names_map)
  column_mapping$time <- find_column_name("Time", c("Time"), original_names, cleaned_names_map)
  column_mapping$status <- find_column_name("Booking Status", c("Booking Status", "Booking_Status", "Status"), original_names, cleaned_names_map)
  column_mapping$pickup <- find_column_name("Pickup Location", c("Pickup Location", "Pickup_Location", "Pickup"), original_names, cleaned_names_map)
  column_mapping$value <- find_column_name("Booking Value", c("Booking Value", "Booking_Value", "Value", "Fare"), original_names, cleaned_names_map)
  column_mapping$distance <- find_column_name("Ride Distance", c("Ride Distance", "Ride_Distance", "Distance"), original_names, cleaned_names_map)
  # Optional columns
  column_mapping$vehicle <- find_column_name("Vehicle Type", c("Vehicle Type", "Vehicle_Type"), original_names, cleaned_names_map)
  column_mapping$customer_rating <- find_column_name("Customer Rating", c("Customer Rating", "Customer_Rating"), original_names, cleaned_names_map)
  column_mapping$driver_rating <- find_column_name("Driver Rating", c("Driver Rating", "Driver_Ratings", "Driver_Rating"), original_names, cleaned_names_map)
  message("-----------------------------")
  
  # Check if essential columns were found
  essential_found <- !sapply(column_mapping[c("date", "time", "status", "pickup", "value", "distance")], is.null)
  if (!all(essential_found)) {
    missing_concepts <- names(essential_found)[!essential_found]
    stop(paste("Could not automatically detect essential columns for:", paste(missing_concepts, collapse=", "),
               ". Please check Excel headers."))
  }
  
  # Assign cleaned names to the dataframe
  colnames(raw_data) <- cleaned_names
  
  # --- Perform data type conversions and feature engineering ---
  Time_col_sym <- sym(column_mapping$time) # Symbol for the time column

  # --- Robust Time Parsing Function ---
  library(hms)
  parse_time_safe <- function(x) {
    if (is.na(x) || trimws(x) == "") return(as_hms(NA))
    # Numeric Excel time (fraction of day)
    if (!is.na(suppressWarnings(as.numeric(x)))) {
      num <- suppressWarnings(as.numeric(x))
      if (num >= 0 && num < 1) return(hms(seconds = round(num * 86400)))
    }
    # HH:MM:SS or HH:MM
    if (grepl("^\\s*\\d{1,2}:\\d{2}(:\\d{2})?\\s*$", x)) {
      return(tryCatch(as_hms(x), error = function(e) as_hms(NA)))
    }
    # AM/PM
    if (grepl("^\\s*\\d{1,2}:\\d{2}(:\\d{2})?\\s*(AM|PM|am|pm)\\s*$", x) ||
        grepl("^\\s*\\d{1,2}:\\d{2}(:\\d{2})?(AM|PM|am|pm)\\s*$", x)) {
      formats <- c("%I:%M:%S %p", "%I:%M %p", "%I:%M:%S%p", "%I:%M%p")
      for (fmt in formats) {
        time_obj <- tryCatch(strptime(x, format = fmt), error = function(e) NA)
        if (!is.na(time_obj)) return(as_hms(format(time_obj, "%H:%M:%S")))
      }
      return(as_hms(NA))
    }
    # Fallback
    return(as_hms(NA))
  }

processed_data <- raw_data %>%
  # Ensure the identified time column is treated as character initially for robust parsing
  mutate(!!Time_col_sym := as.character(!!Time_col_sym)) %>%
  mutate(
    # Use !!sym() to evaluate the string variable as a column name
    Date = as.Date(!!sym(column_mapping$date)),

    # --- Improved Time Parsing Logic ---
    # Ensure Time_hms is always an hms object
    Time_hms = as_hms(sapply(!!Time_col_sym, parse_time_safe)),
    # --- End of Improved Time Parsing ---

    Hour = ifelse(!is.na(Time_hms), hour(Time_hms), NA_integer_),

    # Determine Status Flags
    Is_Completed = ifelse(tolower(!!sym(column_mapping$status)) == "completed", 1, 0),
    Is_Cancelled = ifelse(tolower(!!sym(column_mapping$status)) == "cancelled", 1, 0),
    # Ensure numeric types
    Booking.Value = suppressWarnings(as.numeric(!!sym(column_mapping$value))),
    Ride.Distance = suppressWarnings(as.numeric(!!sym(column_mapping$distance))),
    # Ensure location is character/factor
    Pickup.Location = as.character(!!sym(column_mapping$pickup))
  ) %>%
  # Add optional columns if found
  { if (!is.null(column_mapping$vehicle)) rename(., Vehicle.Type = !!sym(column_mapping$vehicle)) else . } %>%
  # Clean Customer.Rating and Driver.Rating before coercion
  { if (!is.null(column_mapping$customer_rating)) {
      . <- mutate(., Customer.Rating_raw = !!sym(column_mapping$customer_rating))
      . <- mutate(., Customer.Rating_raw = ifelse(tolower(trimws(Customer.Rating_raw)) == "null", NA, Customer.Rating_raw))
      . <- mutate(., Customer.Rating = suppressWarnings(as.numeric(as.character(Customer.Rating_raw))))
      . <- select(., -Customer.Rating_raw)
      .
    } else . } %>%
  { if (!is.null(column_mapping$driver_rating)) {
      . <- mutate(., Driver.Rating_raw = !!sym(column_mapping$driver_rating))
      . <- mutate(., Driver.Rating_raw = ifelse(tolower(trimws(Driver.Rating_raw)) == "null", NA, Driver.Rating_raw))
      . <- mutate(., Driver.Rating = suppressWarnings(as.numeric(as.character(Driver.Rating_raw))))
      . <- select(., -Driver.Rating_raw)
      .
    } else . } %>%
  { if (!is.null(column_mapping$customer_rating)) {
      . <- mutate(., Customer.Rating_raw = !!sym(column_mapping$customer_rating))
      . <- mutate(., Customer.Rating = suppressWarnings(as.numeric(as.character(Customer.Rating_raw))))
      # Optionally, flag or remove rows with problematic values
      # . <- filter(., is.na(Customer.Rating) | grepl("^[0-9.]+$", Customer.Rating_raw))
      . <- select(., -Customer.Rating_raw)
      .
    } else . } %>%
  # Clean Driver.Rating before coercion
  { if (!is.null(column_mapping$driver_rating)) {
      . <- mutate(., Driver.Rating_raw = !!sym(column_mapping$driver_rating))
      . <- mutate(., Driver.Rating = suppressWarnings(as.numeric(as.character(Driver.Rating_raw))))
      # Optionally, flag or remove rows with problematic values
      # . <- filter(., is.na(Driver.Rating) | grepl("^[0-9.]+$", Driver.Rating_raw))
      . <- select(., -Driver.Rating_raw)
      .
    } else . } %>%
  # Clean up potential issues
  filter(!is.na(Date)) %>%
  mutate(
    # Ensure positive values where logical, replace non-positive with NA
    Ride.Distance = ifelse(!is.na(Ride.Distance) & Ride.Distance <= 0, NA, Ride.Distance),
    Booking.Value = ifelse(!is.na(Booking.Value) & Booking.Value <= 0, NA, Booking.Value),
    # Handle potential NA hours (e.g., from failed time parsing or original NA times)
    # Replace NA Hour with -1 (or another indicator) AFTER checking Time_hms
    Hour = ifelse(is.na(Hour), -1, Hour)
  ) # Removed the select(-any_of("Time_hms")) here so Hour can use it
#  processed_data <- raw_data %>%
#    # Ensure the identified time column is treated as character initially for robust parsing
#    mutate(!!Time_col_sym := as.character(!!Time_col_sym)) %>%
#    mutate(
#      # Use !!sym() to evaluate the string variable as a column name
#      Date = as.Date(!!sym(column_mapping$date)),
#      
#      # --- Improved Time Parsing Logic ---
#      Time_hms = case_when(
#        # Handle NA or empty strings explicitly first
#        is.na(!!Time_col_sym) | trimws(!!Time_col_sym) == "" ~ NA_real_,
#        
#        # Handle numeric Excel times (fraction of a day) - check if it *can* be numeric first
#        # This handles cases where read_excel guessed numeric but it's not 0-1 range
#        !is.na(suppressWarnings(as.numeric(!!Time_col_sym))) & as.numeric(!!Time_col_sym) >= 0 & as.numeric(!!Time_col_sym) < 1 ~
#          hms(seconds = round(as.numeric(!!Time_col_sym) * 86400)),
#        
#        # Handle character times (HH:MM:SS or HH:MM) safely using regex
#        # Regex: ^ optional space, 1-2 digits, :, 2 digits, optional (: + 2 digits), optional space $
#        grepl("^\\s*\\d{1,2}:\\d{2}(:\\d{2})?\\s*$", !!Time_col_sym) ~
#          tryCatch(as_hms(!!Time_col_sym), error = function(e) NA_real_), # Use tryCatch
#        
#        # Handle potential POSIXct/Date objects if read_excel imports them as such (less likely now)
#        # inherits(!!Time_col_sym, "POSIXct") ~ as_hms(!!Time_col_sym), # Already converted to char
#        # inherits(!!Time_col_sym, "Date") ~ hms(seconds=0), # Already converted to char
#        
#        # Default: If none of the above match, result in NA
#        TRUE ~ NA_real_
#      ),
#      # --- End of Improved Time Parsing ---
#      
#      Hour = ifelse(!is.na(Time_hms), hour(Time_hms), NA_integer_),
#      
#      # Determine Status Flags
#      Is_Completed = ifelse(tolower(!!sym(column_mapping$status)) == "completed", 1, 0),
#      Is_Cancelled = ifelse(tolower(!!sym(column_mapping$status)) == "cancelled", 1, 0),
#      # Ensure numeric types
#      Booking.Value = suppressWarnings(as.numeric(!!sym(column_mapping$value))),
#      Ride.Distance = suppressWarnings(as.numeric(!!sym(column_mapping$distance))),
#      # Ensure location is character/factor
#      Pickup.Location = as.character(!!sym(column_mapping$pickup))
#    ) %>%
#    # Add optional columns if found
#    { if (!is.null(column_mapping$vehicle)) rename(., Vehicle.Type = !!sym(column_mapping$vehicle)) else . } %>%
#    { if (!is.null(column_mapping$customer_rating)) mutate(., Customer.Rating = suppressWarnings(as.numeric(!!sym(column_mapping$customer_rating)))) else . } %>%
#    { if (!is.null(column_mapping$driver_rating)) mutate(., Driver.Rating = suppressWarnings(as.numeric(!!sym(column_mapping$driver_rating)))) else . } %>%
#    # Clean up potential issues
#    filter(!is.na(Date)) %>%
#    mutate(
#      # Ensure positive values where logical, replace non-positive with NA
#      Ride.Distance = ifelse(!is.na(Ride.Distance) & Ride.Distance <= 0, NA, Ride.Distance),
#      Booking.Value = ifelse(!is.na(Booking.Value) & Booking.Value <= 0, NA, Booking.Value),
#      # Handle potential NA hours (e.g., from failed time parsing or original NA times)
#      # Replace NA Hour with -1 (or another indicator) AFTER checking Time_hms
#      Hour = ifelse(is.na(Hour), -1, Hour)
#    ) %>%
#    select(-any_of("Time_hms")) # Remove temporary time column
  
  # Final check for key columns needed by the app
  app_cols_check <- c("Date", "Hour", "Pickup.Location", "Booking.Value", "Ride.Distance", "Is_Completed", "Is_Cancelled")
  missing_app_cols <- setdiff(app_cols_check, colnames(processed_data))
  if (length(missing_app_cols) > 0) {
    stop(paste("Error: Columns needed for app missing after processing:", paste(missing_app_cols, collapse=", "),
               "\nIssue in renaming/processing logic or missing source columns."))
  }
  
  # Check for NA coercion warnings (informative)
  if (!is.null(column_mapping$customer_rating) && any(is.na(processed_data$Customer.Rating) & !is.na(raw_data[[column_mapping$customer_rating]]))) {
    warning("NAs introduced by coercion in Customer.Rating. Check original data.", call. = FALSE)
    # List problematic values
    cat("\nProblematic Customer.Rating values (not coerced to numeric):\n")
    print(unique(raw_data[[column_mapping$customer_rating]][is.na(processed_data$Customer.Rating) & !is.na(raw_data[[column_mapping$customer_rating]])]))
  }
  if (!is.null(column_mapping$driver_rating) && any(is.na(processed_data$Driver.Rating) & !is.na(raw_data[[column_mapping$driver_rating]]))) {
    warning("NAs introduced by coercion in Driver.Rating. Check original data.", call. = FALSE)
    # List problematic values
    cat("\nProblematic Driver.Rating values (not coerced to numeric):\n")
    print(unique(raw_data[[column_mapping$driver_rating]][is.na(processed_data$Driver.Rating) & !is.na(raw_data[[column_mapping$driver_rating]])]))
  }
  # Check if many Hours became -1 (indicating time parsing issues)
  if (mean(processed_data$Hour == -1, na.rm = TRUE) > 0.1) { # If > 10% failed
    warning("A significant portion of 'Time' values could not be parsed into hours. Check 'Time' column format in the Excel file.", call. = FALSE)
  }
  
  
  app_data <- processed_data
  
}, error = function(e) {
  # Capture the specific error message related to the problematic step
  error_details <- ifelse(grepl("In argument:", e$message),
                          sub(".*In argument: `([^`]+)`.*", "Problem likely in processing column related to '\\1'", e$message),
                          e$message)
  error_message <<- paste("FATAL Error loading or processing data:", error_details)
  # Print error to console as well for debugging
  print(error_message)
  print(e) # Print full error context
})


# --- 5. Shiny UI ---
ui <- fluidPage(
  theme = shinytheme("spacelab"), # Apply a theme
  titlePanel("Ride-Sharing Analysis & Data Mining Dashboard"),
  
  # Display error message prominently if loading failed
  if (!is.null(error_message)) {
    fluidRow(
      column(12, tags$div(class = "alert alert-danger", role = "alert", HTML(error_message))) # Use HTML to render potential newlines
    )
  } else if (is.null(app_data) || nrow(app_data) == 0) {
    # Handle case where data loaded but is empty
    fluidRow(
      column(12, tags$div(class = "alert alert-warning", role = "alert", "Data loaded successfully, but it appears to be empty or contain no valid records after initial processing."))
    )
  } else {
    # Only show the main layout if data is loaded successfully and is not empty
    sidebarLayout(
      sidebarPanel(
        h4("Filters"),
        dateRangeInput("dateRange", "Date Range:",
                       start = min(app_data$Date, na.rm = TRUE),
                       end   = max(app_data$Date, na.rm = TRUE),
                       min   = min(app_data$Date, na.rm = TRUE),
                       max   = max(app_data$Date, na.rm = TRUE)),
        
        selectInput("locationSelector", "Pickup Location(s):",
                    # Use the dynamically found pickup column name
                    choices = c("All Locations", sort(unique(app_data$Pickup.Location))),
                    selected = "All Locations",
                    multiple = TRUE),
        
        hr(),
        # Conditional Panels for specific tab inputs
        conditionalPanel(
          condition = "input.tabs == 'demandPrediction'",
          h4("Prediction Settings"),
          numericInput("forecastHorizon", "Forecast Horizon (days):", value = 30, min = 7, max = 365),
          selectInput("aggPeriod", "Aggregation Period:", choices = c("Daily", "Weekly", "Monthly"), selected = "Daily")
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'demandPricing'",
          h4("Demand/Pricing Settings"),
          sliderInput("hourRange", "Hour Range:", min = 0, max = 23, value = c(0, 23), step = 1) # Ensure step is 1
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'locationClustering'",
          h4("Clustering Settings"),
          numericInput("numClusters", "Number of Clusters (k):", value = 5, min = 2, max = 15, step = 1),
          checkboxGroupInput("clusterFeatures", "Features for Clustering:",
                             choices = c("Avg. Booking Value" = "Avg_Booking_Value",
                                         "Avg. Ride Distance" = "Avg_Ride_Distance_km",
                                         "Cancellation Rate" = "Cancellation_Rate",
                                         "Total Completed Trips" = "Total_Completed_Trips"
                                         # Add more numeric features from locationSummary if desired
                             ),
                             selected = c("Avg_Booking_Value", "Avg_Ride_Distance_km", "Cancellation_Rate", "Total_Completed_Trips")),
          actionButton("runClustering", "Run Clustering")
        ),
        
        width = 3
      ), # End sidebarPanel
      
      mainPanel(
        tabsetPanel(id = "tabs",
                    # --- Overview/Perfoprmance Tab ---
                    tabPanel("Location Performance", value = "locationPerformance",
                             h4("Location Summary Statistics"),
                             p("Summary metrics for selected locations and date range."),
                             DTOutput("locationSummaryTable"),
                             hr(),
                             fluidRow(
                               column(6,
                                      h4("Booking Value Distribution"),
                                      plotOutput("locationValueDistPlot")
                               ),
                               column(6,
                                      h4("Completed Trips Count"),
                                      plotOutput("locationCompletedTripsPlot")
                               )
                             )
                    ), # End tabPanel locationPerformance
                    
                    # --- Demand/Pricing Insights Tab ---
                    tabPanel("Demand & Pricing Insights", value = "demandPricing",
                             h4("Hourly Patterns by Location"),
                             fluidRow(
                               column(6,
                                      h4("Booking Volume by Hour"),
                                      plotOutput("demandVolumePlot")
                               ),
                               column(6,
                                      h4("Cancellation Rate by Hour"),
                                      plotOutput("cancellationRatePlot")
                               )
                             ),
                             hr(),
                             h4("Average Booking Value by Hour"),
                             plotOutput("avgValuePlot")
                    ), # End tabPanel demandPricing
                    
                    # --- Demand Prediction Tab ---
                    tabPanel("Demand Prediction (Time Series)", value = "demandPrediction",
                             h4("Booking Count Forecast"),
                             p("Predicting future booking counts using ETS/ARIMA models based on historical data."),
                             plotOutput("demandForecastPlot"),
                             hr(),
                             h4("Forecast Model Details"),
                             verbatimTextOutput("forecastSummary")
                    ), # End tabPanel demandPrediction
                    
                    # --- Trip Characteristics Tab ---
                    tabPanel("Trip Characteristics", value = "tripAnalysis",
                             h4("Averages by Pickup Location"),
                             fluidRow(
                               column(6,
                                      h4("Average Ride Distance"),
                                      plotOutput("avgDistancePlot")
                               ),
                               column(6,
                                      h4("Average Booking Value"),
                                      plotOutput("avgBookingValuePlot")
                               )
                             )
                    ), # End tabPanel tripAnalysis
                    
                    # --- Location Clustering Tab (K-Means) ---
                    tabPanel("Location Clustering (K-Means)", value = "locationClustering",
                             h4("Grouping Similar Locations"),
                             p("Using K-Means algorithm to group locations based on selected performance characteristics. Requires clicking 'Run Clustering' after changing settings."),
                             hr(),
                             h4("Cluster Assignments"),
                             DTOutput("clusterResultsTable"),
                             hr(),
                             fluidRow(
                               column(6,
                                      h4("Cluster Visualization (PCA)"),
                                      plotOutput("clusterPlotPCA")
                               ),
                               column(6,
                                      h4("Cluster Profiles (Centroids)"), # Renamed for clarity
                                      DTOutput("clusterProfileTable")
                               )
                             )
                             # Optional: Add Silhouette plot here for validation
                             # plotOutput("clusterSilhouettePlot")
                    ) # End tabPanel locationClustering
                    
        ), # End tabsetPanel
        width = 9
      ) # End mainPanel
    ) # End sidebarLayout
  } # End else (if no error message and data is not empty)
) # End fluidPage


# --- 6. Shiny Server Logic ---
server <- function(input, output, session) {
  
  # Stop the server function if data loading failed or data is empty
  if (!is.null(error_message) || is.null(app_data) || nrow(app_data) == 0) {
    # Optional: Add a message to the UI if the server stops early
    # output$statusMessage <- renderText({"Data loading failed or data is empty. Server not running."})
    return()
  }
  
  # --- Reactive Data Filtering ---
  filtered_data <- reactive({
    # Ensure date range is valid and available
    req(input$dateRange, input$dateRange[1] <= input$dateRange[2])
    
    df <- app_data %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    
    # Filter by Pickup Location if not "All Locations"
    # Ensure input$locationSelector is not NULL or empty before filtering
    if (!is.null(input$locationSelector) && !("All Locations" %in% input$locationSelector)) {
      req(input$locationSelector) # Require it if not "All Locations"
      df <- df %>% filter(Pickup.Location %in% input$locationSelector)
    }
    
    # Basic validation
    validate(
      need(nrow(df) > 0, "No data available for the selected filters. Please adjust date range or location selection.")
    )
    
    return(df)
  })
  
  # --- Location Performance Outputs ---
  
  # Calculate location summary metrics reactively
  location_summary <- reactive({
    # Require filtered_data() to ensure it's valid and non-empty first
    req(filtered_data())
    
    summary_df <- filtered_data() %>%
      group_by(Pickup.Location) %>%
      summarise(
        Total_Booking_Value = sum(Booking.Value, na.rm = TRUE),
        Avg_Booking_Value = mean(Booking.Value, na.rm = TRUE),
        Total_Completed_Trips = sum(Is_Completed, na.rm = TRUE),
        Total_Bookings = n(),
        Total_Cancelled_Trips = sum(Is_Cancelled, na.rm = TRUE),
        Cancellation_Rate = ifelse(Total_Bookings > 0, Total_Cancelled_Trips / Total_Bookings, 0),
        # Avg distance for completed trips only
        Avg_Ride_Distance_km = mean(Ride.Distance[Is_Completed == 1 & !is.na(Ride.Distance)], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      # Clean up potential NaN/Inf resulting from mean(NA, na.rm=T) or division by zero
      mutate(across(where(is.numeric), ~ifelse(is.nan(.x) | is.infinite(.x), NA, .x))) %>% # Replace NaN/Inf with NA first
      mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x))) %>% # Then replace NA with 0 (or choose other imputation)
      arrange(desc(Total_Completed_Trips))
    
    # Basic validation
    validate(
      need(nrow(summary_df) > 0, "Could not calculate summary statistics. Check data for selected filters.")
    )
    return(summary_df)
  })
  
  output$locationSummaryTable <- renderDT({
    req(location_summary()) # Ensure summary is calculated
    datatable(location_summary(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(c('Total_Booking_Value', 'Avg_Booking_Value'), currency = "$", digits = 2) %>% # Adjust currency if needed
      formatPercentage('Cancellation_Rate', digits = 1) %>%
      formatRound(c('Total_Completed_Trips', 'Total_Bookings', 'Total_Cancelled_Trips'), digits = 0) %>%
      formatRound('Avg_Ride_Distance_km', digits = 1)
  })
  
  output$locationValueDistPlot <- renderPlot({
    req(filtered_data()) # Ensure filtered data is available
    df_plot <- filtered_data() %>% filter(!is.na(Booking.Value) & Booking.Value > 0)
    validate(need(nrow(df_plot) > 0, "No positive booking value data to display for the selected filters."))
    
    # Limit number of locations shown for clarity
    # Ensure there's at least one location before trying to count/filter
    if (length(unique(df_plot$Pickup.Location)) > 0) {
      top_locations <- df_plot %>% count(Pickup.Location, sort = TRUE) %>% head(15) %>% pull(Pickup.Location)
      df_plot_top <- df_plot %>% filter(Pickup.Location %in% top_locations)
    } else {
      # Handle case with no locations in filtered data
      validate("No location data available for plotting.")
      return(NULL) # Or return an empty plot object
    }
    
    
    validate(need(nrow(df_plot_top) > 0, "No data for top locations after filtering."))
    
    ggplot(df_plot_top, aes(x = reorder(Pickup.Location, Booking.Value, FUN = median), y = Booking.Value, fill = Pickup.Location)) +
      geom_boxplot(show.legend = FALSE, outlier.shape = NA) + # Hide outliers for cleaner look
      # Adjust y-axis limits based on quantiles of the *filtered* data for the plot
      coord_cartesian(ylim = quantile(df_plot_top$Booking.Value, c(0.05, 0.95), na.rm = TRUE)) +
      labs(title = "Top 15 Locations by Volume", x = "Pickup Location", y = "Booking Value") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::dollar_format()) # Use scales package for formatting
  })
  
  output$locationCompletedTripsPlot <- renderPlot({
    req(filtered_data())
    df_plot <- filtered_data() %>% filter(Is_Completed == 1) %>% count(Pickup.Location, name = "Total_Completed_Trips", sort = TRUE) %>% head(20)
    validate(need(nrow(df_plot) > 0, "No completed trips found for selected filters."))
    
    ggplot(df_plot, aes(x = reorder(Pickup.Location, Total_Completed_Trips), y = Total_Completed_Trips, fill = Pickup.Location)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Top 20 Locations by Completed Trips", x = "Pickup Location", y = "Total Completed Trips") +
      theme_minimal(base_size = 12)
  })
  
  
  # --- Demand/Pricing Insights Outputs ---
  filtered_hourly_data <- reactive({
    req(input$hourRange) # Ensure slider input is available
    req(filtered_data()) # Ensure base filtered data is available
    
    df_hourly <- filtered_data() %>%
      # Filter out the placeholder Hour = -1 used for failed parsing
      filter(Hour >= 0) %>%
      filter(Hour >= input$hourRange[1] & Hour <= input$hourRange[2])
    
    validate(
      need(nrow(df_hourly) > 0, "No data available for the selected hour range (check time parsing and filters).")
    )
    return(df_hourly)
  })
  
  output$demandVolumePlot <- renderPlot({
    # Require the specific reactive data needed for this plot
    req(filtered_hourly_data())
    # Also require input$hourRange to be available for the title
    req(input$hourRange)
    
    df_agg <- filtered_hourly_data() %>%
      count(Hour, name = "Total_Bookings")
    
    # Create title safely after req()
    plot_title <- paste("Hourly Volume (", input$hourRange[1], ":00 - ", input$hourRange[2], ":59)", sep="")
    
    ggplot(df_agg, aes(x = factor(Hour), y = Total_Bookings)) +
      geom_col(fill = "steelblue") +
      labs(title = plot_title,
           x = "Hour of Day", y = "Number of Bookings") +
      theme_minimal(base_size = 12) +
      # Ensure all hours in the range are potentially shown (even if 0 count)
      scale_x_discrete(limits = factor(input$hourRange[1]:input$hourRange[2]), drop = FALSE)
  })
  
  output$cancellationRatePlot <- renderPlot({
    req(filtered_hourly_data(), input$hourRange)
    
    df_agg <- filtered_hourly_data() %>%
      group_by(Hour) %>%
      summarise(Total_Bookings = n(), Total_Cancelled = sum(Is_Cancelled, na.rm=TRUE), .groups = 'drop') %>%
      mutate(Cancellation_Rate = ifelse(Total_Bookings > 5, Total_Cancelled / Total_Bookings, NA)) # Min bookings for stable rate
    
    # Complete the data frame to include all hours in the range for plotting
    all_hours_df <- data.frame(Hour = input$hourRange[1]:input$hourRange[2])
    df_agg_complete <- left_join(all_hours_df, df_agg, by = "Hour")
    
    validate(need(any(!is.na(df_agg_complete$Cancellation_Rate)), "Not enough bookings (>5 per hour) to calculate cancellation rate for any hour in the selected range."))
    
    plot_title <- paste("Hourly Cancellation Rate (", input$hourRange[1], ":00 - ", input$hourRange[2], ":59)", sep="")
    
    ggplot(df_agg_complete, aes(x = factor(Hour), y = Cancellation_Rate)) +
      geom_col(fill = "tomato", na.rm = TRUE) + # Use na.rm=TRUE for geom_col if NAs exist
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = plot_title,
           subtitle = "Requires > 5 bookings per hour",
           x = "Hour of Day", y = "Cancellation Rate") +
      theme_minimal(base_size = 12) +
      scale_x_discrete(limits = factor(input$hourRange[1]:input$hourRange[2]), drop = FALSE)
  })
  
  output$avgValuePlot <- renderPlot({
    req(filtered_hourly_data(), input$hourRange)
    
    df_agg <- filtered_hourly_data() %>%
      filter(!is.na(Booking.Value)) %>% # Ensure we only consider non-NA values
      group_by(Hour) %>%
      summarise(Avg_Booking_Value = mean(Booking.Value, na.rm = TRUE), N = n(), .groups = 'drop') %>%
      filter(N > 5) # Min bookings for stable average
    
    validate(need(nrow(df_agg) > 0, "Not enough data (>5 bookings per hour) to calculate average value for any hour in the selected range."))
    
    # Complete for plotting lines correctly across the range
    all_hours_df <- data.frame(Hour = input$hourRange[1]:input$hourRange[2])
    df_agg_complete <- left_join(all_hours_df, df_agg, by = "Hour")
    
    plot_title <- paste("Hourly Average Booking Value (", input$hourRange[1], ":00 - ", input$hourRange[2], ":59)", sep="")
    
    ggplot(df_agg_complete, aes(x = factor(Hour), y = Avg_Booking_Value)) +
      geom_line(group=1, color="darkgreen", linewidth=1, na.rm = TRUE) + # Use na.rm=TRUE for geom_line
      geom_point(color="darkgreen", size=2, na.rm = TRUE) + # Use na.rm=TRUE for geom_point
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = plot_title,
           subtitle = "Requires > 5 bookings per hour",
           x = "Hour of Day", y = "Average Booking Value") +
      theme_minimal(base_size = 12) +
      scale_x_discrete(limits = factor(input$hourRange[1]:input$hourRange[2]), drop = FALSE)
  })
  
  
  # --- Demand Prediction Outputs ---
  time_series_data <- reactive({
    req(input$aggPeriod)
    req(filtered_data()) # Need base filtered data
    
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available for the selected filters to perform forecasting."))
    
    # Aggregate data based on selected period - COUNTING bookings
    agg_unit <- switch(input$aggPeriod, "Daily" = "day", "Weekly" = "week", "Monthly" = "month")
    df_agg <- df %>%
      mutate(Time_Period = floor_date(Date, agg_unit)) %>%
      group_by(Time_Period) %>%
      summarise(Total_Bookings = n(), .groups = 'drop')
    
    validate(need(nrow(df_agg) > 1, paste("Need at least 2 data points after", input$aggPeriod, "aggregation to create a time series.")))
    
    # Complete the sequence to handle missing periods
    full_seq <- seq(min(df_agg$Time_Period), max(df_agg$Time_Period), by = agg_unit)
    # Ensure Time_Period is Date type before completing
    df_agg <- df_agg %>% mutate(Time_Period = as.Date(Time_Period))
    # Check if full_seq is valid before using complete
    validate(need(length(full_seq) > 0, "Could not generate a valid time sequence for completion."))
    
    df_agg <- df_agg %>%
      tidyr::complete(Time_Period = full_seq, fill = list(Total_Bookings = 0)) # Fill missing periods with 0
    
    # Determine frequency for ts object
    freq <- switch(input$aggPeriod,
                   "Daily" = 7, # Weekly seasonality for daily data
                   "Weekly" = ifelse(nrow(df_agg) > 104, 52, 1), # Annual seasonality if > 2 years, else none
                   "Monthly" = 12) # Annual seasonality for monthly data
    
    # Validate sufficient data length for the chosen frequency
    validate(need(nrow(df_agg) >= 2 * freq,
                  paste("Not enough data for reliable forecasting with the chosen aggregation period.",
                        "Need at least", 2*freq, input$aggPeriod, "periods, but only have", nrow(df_agg), ".")))
    
    # Create ts object
    start_date <- min(df_agg$Time_Period)
    start_param <- switch(input$aggPeriod,
                          "Daily" = c(year(start_date), yday(start_date)),
                          "Weekly" = c(year(start_date), as.numeric(format(start_date, "%U")) + 1), # Week starts from 1
                          "Monthly" = c(year(start_date), month(start_date)))
    
    ts_data <- ts(df_agg$Total_Bookings,
                  start = start_param,
                  frequency = freq)
    
    return(list(ts_data = ts_data, df_agg = df_agg)) # Return both ts object and aggregated data frame
  })
  
  forecast_model <- reactive({
    ts_list <- tryCatch(time_series_data(), error = function(e) {
      warning(paste("Error in time_series_data():", e$message))
      return(NULL)
    })
    validate(need(!is.null(ts_list) && !is.null(ts_list$ts_data), "Time series data preparation failed. Check filters and aggregation period."))
    
    req(input$forecastHorizon)
    
    ts_data <- ts_list$ts_data
    h_periods <- ceiling(input$forecastHorizon / switch(input$aggPeriod, "Daily"=1, "Weekly"=7, "Monthly"=30.44))
    
    fit <- tryCatch(ets(ts_data),
                    error = function(e_ets) {
                      warning(paste("ETS failed:", e_ets$message, "- Trying auto.arima."))
                      tryCatch(auto.arima(ts_data),
                               error = function(e_arima) {
                                 warning(paste("auto.arima also failed:", e_arima$message))
                                 return(NULL)
                               })
                    })
    
    validate(need(!is.null(fit), "Failed to fit ETS or ARIMA model. Data might be too short, too variable, or unsuitable for these models."))
    
    fc <- forecast(fit, h = h_periods)
    return(list(fit = fit, forecast = fc))
  })
  
  output$demandForecastPlot <- renderPlot({
    model_output <- tryCatch(forecast_model(), error = function(e) {
      warning(paste("Error in forecast_model():", e$message))
      return(NULL)
    })
    validate(need(!is.null(model_output) && !is.null(model_output$forecast),
                  "Forecasting failed. Unable to generate plot. Check model details and data suitability."))
    
    p <- ggplot2::autoplot(model_output$forecast) +
      labs(title = paste("Booking Count Forecast (", input$aggPeriod, ")", sep=""),
           subtitle = paste("Model:", model_output$forecast$method),
           x = "Time", y = "Number of Bookings") +
      theme_minimal(base_size = 12)
    
    print(p)
  })
  
  
  output$forecastSummary <- renderPrint({
    model_output <- tryCatch(forecast_model(), error = function(e) NULL)
    validate(need(!is.null(model_output) && !is.null(model_output$fit), "Forecasting failed, cannot show model summary."))
    summary(model_output$fit)
  })
  
  
  # --- Trip Characteristics Outputs ---
  output$avgDistancePlot <- renderPlot({
    req(filtered_data())
    df_plot <- filtered_data() %>%
      filter(Is_Completed == 1 & !is.na(Ride.Distance)) %>%
      group_by(Pickup.Location) %>%
      summarise(Avg_Distance = mean(Ride.Distance, na.rm = TRUE), N = n(), .groups = 'drop') %>%
      filter(N > 5) %>% # Min bookings for stable average
      arrange(desc(Avg_Distance)) %>%
      head(20) # Limit to top 20 for clarity
    
    validate(need(nrow(df_plot) > 0, "Not enough data (>5 completed trips per location) to calculate average distance for top locations."))
    
    ggplot(df_plot, aes(x = reorder(Pickup.Location, Avg_Distance), y = Avg_Distance, fill = Pickup.Location)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      labs(title = "Avg. Ride Distance (Completed Trips)", subtitle="Top 20 Locations (>5 trips)", x = "Pickup Location", y = "Average Distance (km)") +
      theme_minimal(base_size = 12)
  })
  
  output$avgBookingValuePlot <- renderPlot({
    req(filtered_data())
    df_plot <- filtered_data() %>%
      filter(!is.na(Booking.Value)) %>% # Use all non-NA values
      group_by(Pickup.Location) %>%
      summarise(Avg_Value = mean(Booking.Value, na.rm = TRUE), N = n(), .groups = 'drop') %>%
      filter(N > 5) %>% # Min bookings for stable average
      arrange(desc(Avg_Value)) %>%
      head(20) # Limit to top 20
    
    validate(need(nrow(df_plot) > 0, "Not enough data (>5 trips per location) to calculate average booking value for top locations."))
    
    ggplot(df_plot, aes(x = reorder(Pickup.Location, Avg_Value), y = Avg_Value, fill = Pickup.Location)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = "Avg. Booking Value", subtitle="Top 20 Locations (>5 trips)", x = "Pickup Location", y = "Average Booking Value") +
      theme_minimal(base_size = 12)
  })
  
  # --- Location Clustering Outputs ---
  
  clustering_results <- eventReactive(input$runClustering, {
    req(location_summary(), input$numClusters, input$clusterFeatures)
    loc_summary <- location_summary()
    
    features_to_use <- input$clusterFeatures
    available_features <- colnames(loc_summary)
    valid_features <- intersect(features_to_use, available_features)
    
    validate(
      need(length(valid_features) > 0, "No valid features selected or available for clustering."),
      need(length(valid_features) >= 2, "Please select at least two features for clustering visualization.")
    )
    
    cluster_data <- loc_summary %>%
      select(Pickup.Location, all_of(valid_features)) %>%
      mutate(across(all_of(valid_features), ~ifelse(is.na(.x), 0, .x)))
    
    locations <- cluster_data$Pickup.Location
    cluster_data_scaled <- scale(cluster_data[, valid_features])
    
    validate(need(nrow(cluster_data_scaled) >= input$numClusters,
                  paste("Not enough locations (", nrow(cluster_data_scaled), ") to form", input$numClusters, "clusters.")))
    
    set.seed(123)
    kmeans_result <- tryCatch(kmeans(cluster_data_scaled, centers = input$numClusters, nstart = 25),
                              error = function(e) {
                                warning(paste("K-means failed:", e$message))
                                return(NULL)
                              })
    
    validate(need(!is.null(kmeans_result), "K-means clustering algorithm failed. Check data and number of clusters."))
    
    results_df <- loc_summary %>%
      filter(Pickup.Location %in% locations) %>%
      mutate(Cluster = factor(kmeans_result$cluster))
    
    pca_result <- tryCatch(prcomp(cluster_data_scaled, center = TRUE, scale. = FALSE),
                           error = function(e) {
                             warning(paste("PCA failed:", e$message))
                             return(NULL)
                           })
    validate(need(!is.null(pca_result) && ncol(pca_result$x) >= 2, "PCA calculation failed or produced less than 2 components.")) # Check for >=2 PCs
    
    pca_data <- data.frame(pca_result$x[, 1:2])
    colnames(pca_data) <- c("PC1", "PC2")
    pca_data$Cluster <- factor(kmeans_result$cluster)
    pca_data$Location <- locations
    
    cluster_profiles <- results_df %>%
      group_by(Cluster) %>%
      summarise(across(all_of(valid_features), mean, na.rm = TRUE),
                Num_Locations = n(),
                .groups = 'drop')
    
    
    return(list(
      results_table = results_df,
      pca_data = pca_data,
      kmeans_result = kmeans_result,
      cluster_data_scaled = cluster_data_scaled,
      cluster_profiles = cluster_profiles
    ))
  })
  
  output$clusterResultsTable <- renderDT({
    results <- clustering_results()
    req(results$results_table)
    display_table <- results$results_table %>%
      select(Pickup.Location, Cluster, Total_Completed_Trips, Avg_Booking_Value, Avg_Ride_Distance_km, Cancellation_Rate) %>%
      arrange(Cluster, desc(Total_Completed_Trips))
    
    datatable(display_table, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency('Avg_Booking_Value', currency = "$", digits = 2) %>%
      formatPercentage('Cancellation_Rate', digits = 1) %>%
      formatRound('Avg_Ride_Distance_km', digits = 1) %>%
      formatRound('Total_Completed_Trips', digits = 0)
  })
  
  output$clusterPlotPCA <- renderPlot({
    results <- clustering_results()
    req(results$pca_data)
    
    ggplot(results$pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(alpha = 0.7, size = 3) +
      labs(title = "Location Clusters (PCA Visualization)",
           x = "Principal Component 1", y = "Principal Component 2",
           color = "Cluster") +
      theme_minimal(base_size = 12) +
      scale_color_brewer(palette = "Set1")
  })
  
  output$clusterProfileTable <- renderDT({
    results <- clustering_results()
    req(results$cluster_profiles)
    
    datatable(results$cluster_profiles, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(intersect(colnames(results$cluster_profiles), 'Avg_Booking_Value'), currency = "$", digits = 2) %>%
      formatPercentage(intersect(colnames(results$cluster_profiles), 'Cancellation_Rate'), digits = 1) %>%
      formatRound(intersect(colnames(results$cluster_profiles), 'Avg_Ride_Distance_km'), digits = 1) %>%
      formatRound(intersect(colnames(results$cluster_profiles), c('Total_Completed_Trips', 'Num_Locations')), digits = 0)
  })
  
} # End server function


# --- 7. Run the Shiny App ---
shinyApp(ui = ui, server = server)
