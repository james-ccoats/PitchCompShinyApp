# ui.R - Baseball Pitch Finder Shiny App UI
library(shiny)
library(shinydashboard)
library(DT)

# Define UI
dashboardPage(
  dashboardHeader(title = "Baseball Pitch Finder"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pitch Search", tabName = "search", icon = icon("search")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Search tab
      tabItem(tabName = "search",
              fluidRow(
                # Input panel
                box(
                  title = "Search Parameters", status = "primary", solidHeader = TRUE,
                  width = 4,
                  
                  numericInput("pfx_x_input", 
                               "Horizontal Break (inches):", 
                               value = 0, 
                               min = -30, 
                               max = 30, 
                               step = 0.1),
                  
                  numericInput("pfx_z_input", 
                               "Vertical Break (inches):", 
                               value = 0, 
                               min = -30, 
                               max = 30, 
                               step = 0.1),
                  
                  numericInput("rpm_input", 
                               "Minimum RPM:", 
                               value = 2000, 
                               min = 1000, 
                               max = 4000, 
                               step = 50),
                  
                  numericInput("vaa_input", 
                               "Desired VAA (negative = steeper):", 
                               value = -5, 
                               min = -30, 
                               max = 30, 
                               step = 0.1),
                  
                  textInput("tilt_input", 
                            "Desired Tilt (clock format, e.g. '1:30'):", 
                            value = "12:00"),
                  
                  selectInput("pitch_type_input", 
                              "Pitch Type:",
                              choices = c("4-Seam Fastball" = "4-seam fastball",
                                          "Sinker" = "sinker",
                                          "Cutter" = "cutter", 
                                          "Slider" = "slider",
                                          "Sweeper" = "sweeper",
                                          "Curveball" = "curveball",
                                          "Knuckle Curve" = "knuckle curve",
                                          "Changeup" = "changeup",
                                          "Split-Finger" = "split-finger",
                                          "Slurve" = "slurve",
                                          "Knuckleball" = "knuckleball")),
                  
                  radioButtons("handedness_input", 
                               "Pitcher Handedness:",
                               choices = list("Right-handed" = "R", 
                                              "Left-handed" = "L"),
                               selected = "R"),
                  
                  actionButton("search_btn", 
                               "Find Closest Pitch", 
                               class = "btn-primary",
                               style = "width: 100%;")
                ),
                
                # Results panel
                box(
                  title = "Search Results", status = "success", solidHeader = TRUE,
                  width = 8,
                  
                  conditionalPanel(
                    condition = "output.results_available",
                    
                    h4("Closest Pitch Found:"),
                    verbatimTextOutput("pitch_summary"),
                    
                    br(),
                    
                    actionButton("view_video", 
                                 "View Video", 
                                 class = "btn-info",
                                 icon = icon("play")),
                    
                    br(), br(),
                    
                    h4("Detailed Results:"),
                    DT::dataTableOutput("results_table")
                  ),
                  
                  conditionalPanel(
                    condition = "!output.results_available",
                    div(
                      style = "text-align: center; margin-top: 50px;",
                      icon("search", style = "font-size: 48px; color: #ccc;"),
                      h3("Enter search parameters and click 'Find Closest Pitch'", 
                         style = "color: #999;")
                    )
                  )
                )
              ),
              
              # Status messages
              fluidRow(
                box(
                  title = "Status", status = "info", solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("status_message")
                )
              )
      ),
      
      # About tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This App", status = "primary", solidHeader = TRUE,
                  width = 12,
                  
                  h3("Baseball Pitch Finder"),
                  p("This app helps you find baseball pitches that match your desired characteristics from Statcast data."),
                  
                  h4("How to Use:"),
                  tags$ol(
                    tags$li("Enter your desired pitch characteristics in the search panel"),
                    tags$li("Select pitch type and pitcher handedness"),
                    tags$li("Click 'Find Closest Pitch' to search the database"),
                    tags$li("View the results and optionally watch the video")
                  ),
                  
                  h4("Parameters:"),
                  tags$ul(
                    tags$li(strong("Horizontal Break:"), "Side-to-side movement in inches"),
                    tags$li(strong("Vertical Break:"), "Up-down movement in inches"),
                    tags$li(strong("RPM:"), "Minimum spin rate"),
                    tags$li(strong("VAA:"), "Vertical Approach Angle (negative values = steeper)"),
                    tags$li(strong("Tilt:"), "Spin axis as clock position (e.g., '1:30', '12:00')")
                  ),
                  
                  h4("Data Requirements:"),
                  p("This app expects a dataset named 'savant_data_mod' with the following columns:"),
                  tags$ul(
                    tags$li("pitch_hand, pitch_name, release_spin_rate"),
                    tags$li("pfx_x, pfx_z, VAA, spin_axis, tilt"),
                    tags$li("pitcher_id, play_id"),
                    tags$li(strong("Name:"), "Optional column with pitcher names")
                  ),
                  
                  h4("Pitcher Names:"),
                  p("The app will use pitcher names from your 'Name' column if available:"),
                  tags$ul(
                    tags$li(strong("If Name column exists:"), "Shows the pitcher's name"),
                    tags$li(strong("If Name is empty/missing:"), "Shows 'pitcher name not found'"),
                    tags$li(strong("Always shows Pitcher ID:"), "For reference and debugging")
                  )
                )
              )
      )
    )
  )
)