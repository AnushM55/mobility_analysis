# --- 1. Load Libraries ---
# Make sure to install them first if you haven't:
# install.packages(c("shiny", "dplyr", "ggplot2", "lubridate", "forecast", "DT", "readxl", "tidyr", "hms", "shinythemes", "cluster", "stats", "scales"))

# Enable Shiny auto-reload for development (works in interactive R sessions)
options(shiny.autoreload = TRUE)

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
  column_mapping$status <- find_column_name("Booking_Status", c("Booking Status", "Booking_Status", "Status"), original_names, cleaned_names_map)
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

# Currency conversion rate
usd_to_inr <- 83

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
    Is_Completed = ifelse(tolower(!!sym(column_mapping$status)) == "success", 1, 0),
    Is_Cancelled = ifelse(tolower(!!sym(column_mapping$status)) != "success", 1, 0),
    # Ensure numeric types and convert Booking.Value from USD to INR
    Booking.Value = suppressWarnings(as.numeric(!!sym(column_mapping$value))) * usd_to_inr,
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
          selectInput("aggPeriod", "Aggregation Period:", choices = c("Daily", "Weekly"), selected = "Daily"),
          selectInput("forecastMethod", "Forecasting Method:",
                      choices = c("Auto (Best)", "Naive", "Mean", "Drift", "Seasonal Naive", "TBATS", "Linear Regression", "Polynomial Regression"),
                      selected = "Auto (Best)")
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'demandPricing'",
          h4("Demand/Pricing Settings"),
          sliderInput("hourRange", "Hour Range:", min = 0, max = 23, value = c(0, 23), step = 1) # Ensure step is 1
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'locationClustering'",
          h4("Clustering Settings"),
          checkboxGroupInput("clusterFeatures", "Features for Clustering:",
                             choices = c(
                               "Avg. Booking Value" = "Avg_Booking_Value",
                               "Avg. Ride Distance" = "Avg_Ride_Distance_km",
                               "Cancellation Rate" = "Cancellation_Rate",
                               "Total Completed Trips" = "Total_Completed_Trips",
                               # Add Avg. Driver Rating if available
                               if ("Driver.Rating" %in% colnames(app_data)) "Avg. Driver Rating" = "Avg_Driver_Rating" else NULL
                             ),
                             selected = c("Avg_Booking_Value", "Avg_Ride_Distance_km", "Cancellation_Rate", "Total_Completed_Trips")),
          numericInput("kmeans_k", "Number of Clusters (k):", value = 3, min = 2, max = 15, step = 1),
          actionButton("runClustering", "Run Clustering"),
          verbatimTextOutput("clusteringDebugInfo", placeholder = TRUE)
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
                             h4("DEBUG: Filtered Hourly Data (first 20 rows)"),
                             DT::dataTableOutput("debugHourlyTable"),
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
                             conditionalPanel(
                               condition = "input.clusteringMethod == 'kmeans'",
                               h4("Cluster Assignments (K-means)"),
                               DTOutput("clusterResultsTable"),
                               hr(),
                               fluidRow(
                                 column(6,
                                        h4("Cluster Visualization (PCA)"),
                                        plotOutput("clusterPlotPCA")
                                 ),
                                 column(6,
                                        h4("Cluster Profiles (Centroids)"),
                                        DTOutput("clusterProfileTable")
                                 )
                               )
                             ),
                             conditionalPanel(
                               condition = "input.clusteringMethod == 'hclust'",
                               h4("Dendrogram (Hierarchical Clustering)"),
                               plotOutput("dendrogramPlot", height = "400px"),
                               hr(),
                               h4("Cluster Assignments (Hierarchical)"),
                               DTOutput("hclustResultsTable")
                             )
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
    
    # Ensure all columns needed for clustering are present (robust)
    needed_cols <- c('Pickup.Location', 'Avg_Booking_Value', 'Avg_Ride_Distance_km', 'Cancellation_Rate', 'Total_Completed_Trips')
    missing_cols <- setdiff(needed_cols, colnames(summary_df))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) summary_df[[col]] <- 0
    }
    summary_df <- summary_df[, unique(c(needed_cols, colnames(summary_df)))]
    
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
  
  output$debugHourlyTable <- DT::renderDataTable({
    head(filtered_hourly_data(), 20)
  })

  output$avgValuePlot <- renderPlot({
    req(filtered_hourly_data(), input$hourRange)
    
    df_agg <- filtered_hourly_data() %>%
      group_by(Hour) %>%
      summarise(Avg_Booking_Value = mean(Booking.Value, na.rm=TRUE), .groups = 'drop')
    
    ggplot(df_agg, aes(x=factor(Hour), y=Avg_Booking_Value)) +
      geom_col(fill="#0073C2FF") +
    
    ggplot(df_agg_complete, aes(x = factor(Hour), y = Cancellation_Rate)) +
      geom_col(fill = "tomato", na.rm = TRUE) + # Use na.rm=TRUE for geom_col if NAs exist
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = plot_title,
           subtitle = "Requires at least 1 booking per hour",
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
      filter(N > 5) %>% # Min bookings for stable average
      arrange(desc(Avg_Booking_Value)) %>%
      head(20) # Limit to top 20
    
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
    agg_unit <- switch(input$aggPeriod, "Daily" = "day", "Weekly" = "week")
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
                   "Weekly" = ifelse(nrow(df_agg) > 104, 52, 1)) # Annual seasonality if > 2 years, else none
    
    # Validate sufficient data length for the chosen frequency
    validate(need(nrow(df_agg) >= 2 * freq,
                  paste("Not enough data for reliable forecasting with the chosen aggregation period.",
                        "Need at least", 2*freq, input$aggPeriod, "periods, but only have", nrow(df_agg), ".")))
    
    # Create ts object
    start_date <- min(df_agg$Time_Period)
    start_param <- switch(input$aggPeriod,
                           "Daily" = c(year(start_date), yday(start_date)),
                           "Weekly" = c(year(start_date), as.numeric(format(start_date, "%U")) + 1)) # Week starts from 1
    
    # Use zoo for better date handling on x-axis
    library(zoo)
    ts_data <- zoo(df_agg$Total_Bookings, order.by = df_agg$Time_Period)
    
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
    h_periods <- ceiling(input$forecastHorizon / switch(input$aggPeriod, "Daily"=1, "Weekly"=7))
    
    # Use user-selected forecasting method
    model_type <- NULL
    fc <- NULL
    fit <- NULL
    method <- input$forecastMethod
    
    if (method == "Naive") {
      fc <- forecast::naive(ts_data, h = h_periods)
      model_type <- "Naive"
      fit <- NULL
    } else if (method == "Mean") {
      fc <- forecast::meanf(ts_data, h = h_periods)
      model_type <- "Mean"
      fit <- NULL
    } else if (method == "Drift") {
      fc <- forecast::rwf(ts_data, h = h_periods, drift = TRUE)
      model_type <- "Drift"
      fit <- NULL
    } else if (method == "Seasonal Naive") {
      fc <- forecast::snaive(ts_data, h = h_periods)
      model_type <- "Seasonal Naive"
      fit <- NULL
    } else if (method == "Linear Regression") {
      x <- 1:length(ts_data)
      y <- as.numeric(ts_data)
      fit <- lm(y ~ x)
      future_x <- (length(ts_data) + 1):(length(ts_data) + h_periods)
      fc_vals <- predict(fit, newdata = data.frame(x = future_x))
      # Generate future dates
      last_date <- as.Date(zoo::index(ts_data)[length(ts_data)])
      by_unit <- ifelse(input$aggPeriod == "Daily", "day", "week")
      future_dates <- seq(from = last_date + 1, by = by_unit, length.out = h_periods)
      fc <- list(
        mean = zoo::zoo(fc_vals, order.by = future_dates),
        lower = matrix(NA, nrow = h_periods, ncol = 2),
        upper = matrix(NA, nrow = h_periods, ncol = 2)
      )
      model_type <- "Linear Regression"
      fit <- fit
    } else if (method == "Polynomial Regression") {
      x <- 1:length(ts_data)
      y <- as.numeric(ts_data)
      fit <- lm(y ~ poly(x, 2))
      future_x <- (length(ts_data) + 1):(length(ts_data) + h_periods)
      fc_vals <- predict(fit, newdata = data.frame(x = future_x))
      last_date <- as.Date(zoo::index(ts_data)[length(ts_data)])
      by_unit <- ifelse(input$aggPeriod == "Daily", "day", "week")
      future_dates <- seq(from = last_date + 1, by = by_unit, length.out = h_periods)
      fc <- list(
        mean = zoo::zoo(fc_vals, order.by = future_dates),
        lower = matrix(NA, nrow = h_periods, ncol = 2),
        upper = matrix(NA, nrow = h_periods, ncol = 2)
      )
      model_type <- "Polynomial Regression (Quadratic)"
      fit <- fit
    } else if (method == "TBATS") {
      if (requireNamespace("forecast", quietly = TRUE) && exists("tbats", where = asNamespace("forecast"))) {
        fit <- forecast::tbats(ts_data)
        fc <- forecast(fit, h = h_periods)
        model_type <- "TBATS"
      } else {
        # Fallback to naive, but ensure output is a zoo object indexed by correct dates
        fc_naive <- forecast::naive(ts_data, h = h_periods)
        last_date <- as.Date(zoo::index(ts_data)[length(ts_data)])
        by_unit <- ifelse(input$aggPeriod == "Daily", "day", "week")
        future_dates <- seq(from = last_date + 1, by = by_unit, length.out = h_periods)
        fc <- list(
          mean = zoo::zoo(as.numeric(fc_naive$mean), order.by = future_dates),
          lower = matrix(NA, nrow = h_periods, ncol = 2),
          upper = matrix(NA, nrow = h_periods, ncol = 2)
        )
        model_type <- "Naive (TBATS not available)"
        fit <- NULL
      }
    } else {
      # Auto (Best)
      if (length(ts_data) < 8) {
        # Fallback to naive, but ensure output is a zoo object indexed by correct dates
        fc_naive <- forecast::naive(ts_data, h = h_periods)
        last_date <- as.Date(zoo::index(ts_data)[length(ts_data)])
        by_unit <- ifelse(input$aggPeriod == "Daily", "day", "week")
        future_dates <- seq(from = last_date + 1, by = by_unit, length.out = h_periods)
        fc <- list(
          mean = zoo::zoo(as.numeric(fc_naive$mean), order.by = future_dates),
          lower = matrix(NA, nrow = h_periods, ncol = 2),
          upper = matrix(NA, nrow = h_periods, ncol = 2)
        )
        model_type <- "Naive (last value)"
        fit <- NULL
      } else {
        # Try STL decomposition + ETS via stlf if enough data
        if (length(ts_data) >= 2*7) { # At least 2 weeks of data
          try({
            fc <- forecast::stlf(ts_data, h = h_periods, method = "ets")
            model_type <- paste0("STL+ETS (", fc$method, ")")
            fit <- attr(fc, "model")
          }, silent = TRUE)
        }
        if (is.null(fc)) {
          # Fallback to ETS/ARIMA
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
          # Check if ARIMA/ETS forecast is flat
          if (all(abs(fc$mean - fc$mean[1]) < 1e-8)) {
            fc <- forecast::naive(ts_data, h = h_periods)
            model_type <- "Naive (last value, fallback from flat ARIMA/ETS)"
            fit <- NULL
          } else if (inherits(fit, "ets")) {
            model_type <- paste0("ETS (", fit$method, ")")
          } else if (inherits(fit, "Arima")) {
            model_type <- paste0("ARIMA (", fit$arma[1], ",", fit$arma[2], ",", fit$arma[3], ")")
          } else {
            model_type <- "Unknown"
          }
        }
      }
    }
    return(list(fit = fit, forecast = fc, ts_data = ts_data, model_type = model_type))
  })
  
  output$demandForecastPlot <- renderPlot({
    model_output <- tryCatch(forecast_model(), error = function(e) {
      warning(paste("Error in forecast_model():", e$message))
      return(NULL)
    })
    validate(need(!is.null(model_output) && !is.null(model_output$forecast),
                  "Forecasting failed. Unable to generate plot. Check model details and data suitability."))
    
    # Plot historical data and forecast together
    hist_df <- data.frame(Date = as.Date(zoo::index(model_output$ts_data)),
                         Bookings = as.numeric(model_output$ts_data))
    # Defensive: Check for empty or invalid forecast mean or date index
    forecast_dates <- tryCatch(as.Date(zoo::index(model_output$forecast$mean)), error = function(e) NA)
    forecast_mean <- tryCatch(as.numeric(model_output$forecast$mean), error = function(e) NA)
    if (length(forecast_dates) == 0 || all(is.na(forecast_dates)) || length(forecast_mean) == 0 || all(is.na(forecast_mean))) {
      validate(need(FALSE, "Forecasting failed: forecast output is empty or invalid. Please check your filters and data availability."))
    }
    forecast_df <- data.frame(Date = forecast_dates,
                              Forecast = forecast_mean,
                              Lo80 = suppressWarnings(as.numeric(model_output$forecast$lower[,1])),
                              Hi80 = suppressWarnings(as.numeric(model_output$forecast$upper[,1])),
                              Lo95 = suppressWarnings(as.numeric(model_output$forecast$lower[,2])),
                              Hi95 = suppressWarnings(as.numeric(model_output$forecast$upper[,2])))
    
    library(ggplot2)
    p <- ggplot() +
      geom_line(data = hist_df, aes(x = Date, y = Bookings), color = 'black') +
      geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = 'blue') +
      geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lo80, ymax = Hi80), fill = 'blue', alpha = 0.2) +
      geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lo95, ymax = Hi95), fill = 'blue', alpha = 0.1) +
      labs(title = paste("Booking Count Forecast (", input$aggPeriod, ")", sep=""),
           subtitle = paste("Model:", model_output$model_type),
           x = "Date", y = "Number of Bookings") +
      theme_minimal(base_size = 12)
    
    # Warn if forecast is flat
    if (sd(forecast_df$Forecast) < 1e-6) {
      p <- p + ggtitle("Warning: Forecast is flat (model predicts constant value). Consider providing more data.")
    }
    print(p)
  })
  
  
  output$forecastSummary <- renderPrint({
    model_output <- tryCatch(forecast_model(), error = function(e) NULL)
    validate(need(!is.null(model_output) && !is.null(model_output$fit), "Forecasting failed, cannot show model summary."))
    summary(model_output$fit)
  })
  
  
  # --- Trip Characteristics Outputs ---
  output$cancellationRatePlot <- renderPlot({
    req(filtered_hourly_data(), input$hourRange)
    
    df_agg <- filtered_hourly_data() %>%
      group_by(Hour) %>%
      summarise(Total_Bookings = n(), Total_Cancelled = sum(Is_Cancelled, na.rm=TRUE), .groups = 'drop') %>%
      mutate(Cancellation_Rate = ifelse(Total_Bookings >= 1, Total_Cancelled / Total_Bookings, NA)) # Show for any hour with at least 1 booking
    
    # Complete the data frame to include all hours in the range for plotting
    all_hours_df <- data.frame(Hour = input$hourRange[1]:input$hourRange[2])
    df_agg_complete <- dplyr::left_join(all_hours_df, df_agg, by = "Hour")
    
    validate(need(any(!is.na(df_agg_complete$Cancellation_Rate)), "Not enough bookings (at least 1 per hour) to calculate cancellation rate for any hour in the selected range."))
    
    plot_title <- paste("Hourly Cancellation Rate (", input$hourRange[1], ":00 - ", input$hourRange[2], ":59)", sep="")
    
    ggplot(df_agg_complete, aes(x = factor(Hour), y = Cancellation_Rate)) +
      geom_col(fill = "tomato", na.rm = TRUE) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title = plot_title,
           subtitle = "Requires at least 1 booking per hour",
           x = "Hour of Day", y = "Cancellation Rate") +
      theme_minimal(base_size = 12) +
      scale_x_discrete(limits = factor(input$hourRange[1]:input$hourRange[2]), drop = FALSE)
  })
  
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
  

clustering_debug <- reactiveVal("Clustering not yet run.") # Initial message
  clustering_results <- eventReactive(input$runClustering, {
    message("--- Clustering event triggered ---")
    message("clustering_results() function called")
    clustering_debug("Starting clustering process...") # Update UI debug
    req(location_summary(), input$clusterFeatures)
    loc_summary <- location_summary()
    message(paste("Location summary rows:", nrow(loc_summary)))
    message("location_summary() is valid")
    message(paste("Number of rows in location_summary():", nrow(loc_summary)))
    features_to_use <- input$clusterFeatures
    available_features <- colnames(loc_summary)
    message(paste("Selected features:", paste(features_to_use, collapse=", ")))
    valid_features <- intersect(features_to_use, available_features)
    message(paste("Initial valid features:", paste(valid_features, collapse=", ")))
    message(paste("Number of valid features:", length(valid_features)))

  # Remove features with all NA or zero variance
  feature_vars <- sapply(loc_summary[, valid_features, drop=FALSE], function(x) var(as.numeric(x), na.rm=TRUE))
  zero_var_features <- names(feature_vars)[feature_vars == 0 | is.na(feature_vars)]
  if (length(zero_var_features) > 0) {
    valid_features <- setdiff(valid_features, zero_var_features)
    msg <- paste("Removed features with zero variance or all NA:", paste(zero_var_features, collapse=", "))
    showNotification(msg, type = 'warning')
    message(msg)
  }
  message(paste("Features after variance check:", paste(valid_features, collapse=", ")))

  if (length(valid_features) < 2) {
    msg <- "At least two features with variation are required for clustering."
    showNotification(msg, type = "error")
    message(msg)
    clustering_debug(msg) # Update UI debug
    return(NULL)
  }

  cluster_data <- loc_summary %>%
    select(Pickup.Location, all_of(valid_features)) %>%
    mutate(across(all_of(valid_features), ~as.numeric(.x)))
  message(paste("Rows in cluster_data before imputation:", nrow(cluster_data)))

  # Impute NA/NaN/Inf in clustering features with median (or 0 if all NA)
  impute_with_median <- function(x) {
    if (all(!is.finite(x))) {
      return(rep(0, length(x)))
    } else {
      med <- median(x[is.finite(x)], na.rm = TRUE)
      x[!is.finite(x)] <- med
      return(x)
    }
  }
  cluster_data <- cluster_data %>% mutate(across(all_of(valid_features), impute_with_median))
  message("Imputation with median applied.")

  # Notify if any imputation was needed and filter
  finite_mask <- apply(cluster_data[, valid_features, drop=FALSE], 1, function(row) all(is.finite(row)))
  num_removed <- sum(!finite_mask)
  if (num_removed > 0) {
     msg <- paste0("Removed ", num_removed, " locations with unresolved NA/NaN/Inf after imputation.")
     showNotification(msg, type = "warning")
     message(msg)
  }
  cluster_data <- cluster_data[finite_mask, ]
  message(paste("Rows in cluster_data after imputation/filtering:", nrow(cluster_data)))

  locations <- cluster_data$Pickup.Location

  # Explicitly coerce to numeric matrix
  cluster_matrix <- tryCatch(
    as.matrix(sapply(cluster_data[, valid_features, drop=FALSE], as.numeric)),
    error = function(e) {
      msg <- paste("Error converting features to numeric matrix:", e$message)
      showNotification(msg, type = 'error')
      message(msg)
      clustering_debug(msg)
      return(NULL)
    }
  )
  if (is.null(cluster_matrix)) return(NULL)
  message("Converted data to numeric matrix.")

  if (any(!is.finite(cluster_matrix))) {
    msg <- 'Still found NA/NaN/Inf in clustering matrix after all cleaning. Aborting.'
    showNotification(msg, type = 'error')
    message(msg)
    clustering_debug(msg)
    return(NULL)
  }

  # --- Scaling ---
  message("Scaling data...")
  cluster_data_scaled <- tryCatch(
    scale(cluster_matrix),
    error = function(e) {
      msg <- paste("Error scaling data:", e$message)
      showNotification(msg, type = 'error')
      message(msg)
      clustering_debug(msg)
      return(NULL)
    }
  )
  if (is.null(cluster_data_scaled)) return(NULL)
  message(paste("Scaling complete. Scaled data dimensions:", paste(dim(cluster_data_scaled), collapse="x")))

  if (nrow(cluster_data_scaled) < 2) {
    msg <- paste0("Not enough locations (", nrow(cluster_data_scaled), ") to cluster after processing.")
    showNotification(msg, type = "error")
    message(msg)
    clustering_debug(msg)
    return(NULL)
  }

  # --- Prepare Debug Info ---
  debug_lines <- reactiveVal(c(
    '--- K-means DEBUG ---',
    paste('Timestamp:', Sys.time()),
    paste('Input k:', input$kmeans_k),
    paste('Selected Features:', paste(input$clusterFeatures, collapse=', ')),
    paste('Valid Features Used:', paste(valid_features, collapse=', ')),
    paste('Number of Locations (Initial):', nrow(loc_summary)),
    paste('Number of Locations (After Filtering/Imputation):', nrow(cluster_data)),
    paste('Number of Locations (Final for Clustering):', nrow(cluster_data_scaled)),
    paste('Number of Features (Final for Clustering):', ncol(cluster_data_scaled)),
    'First 5 rows of scaled data:',
    paste(capture.output(print(head(cluster_data_scaled, 5))), collapse='\n')
  ))

  # --- Run K-means ---
  k <- input$kmeans_k
  if (is.null(k) || !is.numeric(k) || k < 2 || k > nrow(cluster_data_scaled)) {
    msg <- paste("Invalid number of clusters (k =", k, "). Must be between 2 and", nrow(cluster_data_scaled))
    showNotification(msg, type = "error")
    message(msg)
    debug_lines(c(debug_lines(), "ERROR: Invalid k value."))
    clustering_debug(paste(debug_lines(), collapse='\n'))
    return(NULL)
  }
  message(paste("Running kmeans with k =", k))
  set.seed(42) # for reproducibility
  kmeans_result <- tryCatch(
    kmeans(cluster_data_scaled, centers = k, nstart = 25), # Added nstart for stability
    error = function(e) {
      msg <- paste("K-means failed:", e$message)
      showNotification(msg, type = "error")
      message(msg)
      debug_lines(c(debug_lines(), paste("ERROR:", msg)))
      clustering_debug(paste(debug_lines(), collapse='\n'))
      return(NULL)
    }
  )
  if (is.null(kmeans_result)) return(NULL)
  message("K-means finished successfully.")

  cluster_labels <- kmeans_result$cluster
  cluster_labels_factor <- as.character(cluster_labels) # Use character for easier joins/display

  # Update debug info
  cluster_assign_summary <- paste(capture.output(print(table(cluster_labels))), collapse='\n')
  debug_lines(c(debug_lines(),
                'K-means successful.',
                'Cluster assignment summary:',
                cluster_assign_summary))
  clustering_debug(paste(debug_lines(), collapse='\n')) # Update UI debug output

  # --- Prepare Results ---
  message("Preparing results dataframe...")
  # Ensure locations vector matches the rows used in kmeans (cluster_data_scaled)
  results_df <- loc_summary %>%
    filter(Pickup.Location %in% locations) %>% # Filter loc_summary to only include clustered locations
    mutate(KMeans_Cluster = cluster_labels_factor[match(Pickup.Location, locations)]) # Match uses the filtered locations
  message("Results dataframe prepared.")

  # --- PCA for Visualization ---
  message("Running PCA...")
  pca_result <- tryCatch(
    prcomp(cluster_data_scaled, center = TRUE, scale. = FALSE),
    error = function(e) {
      msg <- paste("PCA failed:", e$message)
      showNotification(msg, type = "error")
      message(msg)
      debug_lines(c(debug_lines(), paste("WARNING: PCA failed -", msg)))
      clustering_debug(paste(debug_lines(), collapse='\n'))
      return(NULL) # Allow clustering results even if PCA fails
    }
  )

  pca_data <- NULL
  if (!is.null(pca_result)) {
    if (ncol(pca_result$x) >= 2) {
      pca_data <- data.frame(pca_result$x[, 1:2])
      colnames(pca_data) <- c("PC1", "PC2")
      pca_data$Cluster <- cluster_labels_factor # Use the same factor as results_df
      pca_data$Location <- locations # Use the filtered locations
      message("PCA finished successfully.")
      debug_lines(c(debug_lines(), "PCA successful."))
    } else {
      msg <- "PCA calculation produced less than 2 components. Cannot visualize."
      showNotification(msg, type = "warning")
      message(msg)
      debug_lines(c(debug_lines(), paste("WARNING:", msg)))
    }
  }
  clustering_debug(paste(debug_lines(), collapse='\n')) # Update UI debug again

  # --- Cluster Profiles ---
  message("Calculating cluster profiles...")
  cluster_profiles <- results_df %>%
    group_by(KMeans_Cluster) %>%
    summarise(across(all_of(valid_features), ~mean(.x, na.rm = TRUE)), # Use the actual features used
              Num_Locations = n(), .groups = 'drop')
  message("Cluster profiles calculated.")

  # --- Final Return ---
  message("Clustering process completed.")
  clustering_debug(paste(c(debug_lines(), "--- Clustering Complete ---"), collapse='\n')) # Final UI update
  return(list(
    results_table = results_df, # Contains only successfully clustered locations
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
      select(Pickup.Location, KMeans_Cluster, Total_Completed_Trips, Avg_Booking_Value, Avg_Ride_Distance_km, Cancellation_Rate) %>%
      arrange(KMeans_Cluster, desc(Total_Completed_Trips))
    
    datatable(display_table, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency('Avg_Booking_Value', currency = "\u20B9", digits = 2) %>%
      formatPercentage('Cancellation_Rate', digits = 1) %>%
      formatRound('Avg_Ride_Distance_km', digits = 1) %>%
      formatRound('Total_Completed_Trips', digits = 0)
  })

  output$clusterPlotPCA <- renderPlot({
    results <- clustering_results()
    req(results$pca_data)
    
    ggplot(results$pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(alpha = 0.7, size = 3) +
      labs(title = "Location Clusters (K-means, PCA Visualization)",
           x = "Principal Component 1", y = "Principal Component 2",
           color = "KMeans Cluster") +
      theme_minimal(base_size = 12) +
      scale_color_brewer(palette = "Set1")
  })
  
  output$clusteringDebugInfo <- renderText({
    clustering_debug()
  })

  output$clusterProfileTable <- renderDT({
    results <- clustering_results()
    req(results$cluster_profiles)
    
    datatable(results$cluster_profiles, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(intersect(colnames(results$cluster_profiles), 'Avg_Booking_Value'), currency = "\u20B9", digits = 2) %>%
      formatPercentage(intersect(colnames(results$cluster_profiles), 'Cancellation_Rate'), digits = 1) %>%
      formatRound(intersect(colnames(results$cluster_profiles), 'Avg_Ride_Distance_km'), digits = 1) %>%
      formatRound(intersect(colnames(results$cluster_profiles), c('Total_Completed_Trips', 'Num_Locations')), digits = 0)
  })
  
  observeEvent(input$runClustering, {
    message("Run Clustering button clicked!")
  })
} # End server function


# --- 7. Run the Shiny App ---
shinyApp(ui = ui, server = server)
