# server.R - Baseball Pitch Finder Shiny App Server Logic
library(shiny)
library(dplyr)
library(DT)
library(sabRmetrics)
library(httr)
library(shinyjs)

savant_data_mod <- readRDS("/srv/shiny-server/statcast_final.rds")
# Helper function: clock string to degrees
clock_to_spinaxis <- function(clock_str) {
  parts <- strsplit(clock_str, ":")[[1]]
  hr <- as.numeric(parts[1]) %% 12
  mn <- as.numeric(parts[2])
  deg_clock <- (hr * 30) + (mn / 60) * 30
  spin_axis <- (deg_clock - 180) %% 360
  spin_axis
}

# Function to add pitcher names to dataset (updated for Name column
# Helper function to get display name from Name column or pitcher_id
# Define server logic
function(input, output, session) {
  
  # Reactive values to store results
  values <- reactiveValues(
    results = NULL,
    closest_pitch = NULL,
    search_performed = FALSE
  )
  
  # Check if data exists
  data_available <- reactive({
    exists("savant_data_mod") && is.data.frame(savant_data_mod)
  })
  
  # Search function
  observeEvent(input$search_btn, {
    
    if (!data_available()) {
      values$search_performed <- TRUE
      output$status_message <- renderText({
        "Error: 'savant_data_mod' dataset not found. Please load your data first."
      })
      return()
    }
    
    # Validate tilt input
    tilt_valid <- grepl("^[0-9]{1,2}:[0-9]{2}$", input$tilt_input)
    if (!tilt_valid) {
      output$status_message <- renderText({
        "Error: Tilt must be in format 'H:MM' (e.g., '1:30', '12:00')"
      })
      return()
    }
    
    tryCatch({
      target_spin_axis <- clock_to_spinaxis(input$tilt_input)
            
      # Filter data
      filtered_data <- savant_data_mod %>%
        filter(
          pitch_hand == input$handedness_input,
          tolower(pitch_name) == input$pitch_type_input
        )
      
      if (nrow(filtered_data) == 0) {
        values$search_performed <- TRUE
        output$status_message <- renderText({
          "No pitches found matching the basic filters (handedness, pitch type, RPM)."
        })
        return()
      }
      
      # Find closest pitch
      closest_pitch <- filtered_data %>%
        mutate(
          pfx_x_diff = abs(pfx_x * -12 - input$pfx_x_input),
          pfx_z_diff = abs(pfx_z * 12 - input$pfx_z_input),
          vaa_diff = abs(VAA - input$vaa_input),
          tilt_diff = abs(((spin_axis - target_spin_axis + 180) %% 360) - 180),
	  rpm_diff = 0.65*(abs(release_spin_rate - input$rpm_input)),
          total_diff = pfx_x_diff + pfx_z_diff + vaa_diff + tilt_diff + rpm_diff
        ) %>%
        arrange(total_diff) %>%
        slice(1)
      
      if (nrow(closest_pitch) > 0) {
        values$closest_pitch <- closest_pitch
        values$results <- filtered_data %>%
          mutate(
            pfx_x_diff = abs(pfx_x * -12 - input$pfx_x_input),
            pfx_z_diff = abs(pfx_z * 12 - input$pfx_z_input),
            vaa_diff = abs(VAA - input$vaa_input),
            tilt_diff = abs(((spin_axis - target_spin_axis + 180) %% 360) - 180),
	    rpm_diff = 0.65*(abs(release_spin_rate - input$rpm_input)),
            total_diff = pfx_x_diff + pfx_z_diff + vaa_diff + tilt_diff + rpm_diff
          ) %>%
          arrange(total_diff)|>
	  head(n = 100)
        
        values$search_performed <- TRUE
        output$status_message <- renderText({
          paste("Search completed successfully!", 
                nrow(filtered_data), "pitches matched basic filters.")
        })
      } else {
        values$search_performed <- TRUE
        output$status_message <- renderText({
          "No matching pitches found after applying all filters."
        })
      }
      
    }, error = function(e) {
      values$search_performed <- TRUE
      output$status_message <- renderText({
        paste("Error during search:", e$message)
      })
    })
  })
  
  # Output for results availability
  output$results_available <- reactive({
    !is.null(values$closest_pitch)
  })
  outputOptions(output, "results_available", suspendWhenHidden = FALSE)
  
  # Pitch summary output
  output$pitch_summary <- renderText({
    if (!is.null(values$closest_pitch)) {
      cp <- values$closest_pitch
      sprintf(
        paste(
          "Pitcher: %s",
          "Pitch Name: %s", 
          "RPM: %d",
          "Horizontal Break: %.2f inches (%.2f off target)",
          "Vertical Break: %.2f inches (%.2f off target)",
          "VAA: %.2f° (%.2f off target)",
          "Tilt: %s (%.1f° off spin-axis)",
          sep = "\n"
        ),
        cp$name,
        cp$pitch_name,
        cp$release_spin_rate,
        cp$pfx_x * -12, cp$pfx_x_diff,
        cp$pfx_z * 12, cp$pfx_z_diff,
        cp$VAA, cp$vaa_diff,
        if("tilt" %in% names(cp)) cp$tilt else "N/A", cp$tilt_diff
      )
    }
  })
  
  # Results table
  output$results_table <- DT::renderDataTable({
  if (!is.null(values$results)) {

    # Columns to display
    display_cols <- c("name", "pitcher_id", "pitch_name", "release_spin_rate",
                      "pfx_x", "pfx_z", "VAA", "spin_axis", "total_diff")

    # Only include columns that exist
    available_cols <- intersect(display_cols, names(values$results))

    # Prepare data for display
    results_display <- values$results %>%
      select(all_of(available_cols)) %>%
      mutate(
        pfx_x = round(pfx_x * -12, 2),
        pfx_z = round(pfx_z * 12, 2),
        VAA = round(VAA, 2),
        total_diff = round(total_diff, 2)
      )

    # Rename columns for better display
    names(results_display)[names(results_display) == "name"] <- "Pitcher Name"
    names(results_display)[names(results_display) == "pitcher_id"] <- "Pitcher ID"
    names(results_display)[names(results_display) == "pitch_name"] <- "Pitch"
    names(results_display)[names(results_display) == "release_spin_rate"] <- "RPM"
    names(results_display)[names(results_display) == "pfx_x"] <- "H-Break"
    names(results_display)[names(results_display) == "pfx_z"] <- "V-Break"
    names(results_display)[names(results_display) == "spin_axis"] <- "Spin Axis"
    names(results_display)[names(results_display) == "total_diff"] <- "Difference Score"

    results_display
  }
}, options = list(pageLength = 10,lengthMenu = list(c(10, 25, 50, 100), c("10", "25", "50", "100")), scrollX = TRUE, dom = 'lfrtip'), server = FALSE)
  
  # Video button functionality
  # Video button
  # Video button functionality
  # server.R
observe({
  req(values$closest_pitch)

  if ("play_id" %in% names(values$closest_pitch)) {
    play_id <- values$closest_pitch$play_id[1]

    if (!is.null(play_id) && play_id != "") {
      video_url <- tryCatch({
        sabRmetrics::get_video_url(play_id)
      }, error = function(e) {
        NULL
      })

      if (!is.null(video_url) && video_url != "") {
        # Assign the JS action to the button
        shinyjs::onclick("view_video", shinyjs::runjs(sprintf("window.open('%s');", video_url)))
      } else {
        showNotification("Video not available for this pitch.", type = "warning")
      }
    } else {
      showNotification("No play_id available for this pitch.", type = "warning")
    }
  }
})
}
