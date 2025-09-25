# ui.R - Baseball Pitch Finder Shiny App UI
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)

# Define UI
dashboardPage(
  dashboardHeader(title = "PitchComp"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pitch Search", tabName = "search", icon = icon("search")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
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
                               "RPM:", 
                               value = 0, 
                               min = 0, 
                               max = 4000, 
                               step = 50),
                  
                  numericInput("vaa_input", 
                               "VAA (Vertical Approach Angle):", 
                               value = -5, 
                               min = -30, 
                               max = 30, 
                               step = 0.1),
                  
                  textInput("tilt_input", 
                            "Tilt (clock format, e.g. '1:30'):", 
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
                  
                  h3("PitchComp"),
                  p("This shiny app helps find baseball pitches that match desired characteristics using Statcast data from the 2024 regular season. It is designed to take the inputs that best characterize the effectiveness of a pitch with metrics commonly available from a Trackman or Rapsodo report."),
                  
                  h4("How to Use:"),
                  tags$ol(
                    tags$li("Enter your desired pitch characteristics in the search panel"),
                    tags$li("Select pitch type and pitcher handedness"),
                    tags$li("Click 'Find Closest Pitch' to search the database"),
                    tags$li("View the results and watch the video")
                  ),
                  
                  h4("Parameters:"),
                  tags$ul(
                    tags$li(strong("Horizontal Break:"), "Horizontal  movement in inches"),
                    tags$li(strong("Vertical Break:"), "Vertical movement in inches"),
                    tags$li(strong("RPM:"), "Minimum spin rate"),
                    tags$li(strong("VAA:"), "Vertical Approach Angle"),
                    tags$li(strong("Tilt:"), "Spin axis as clock position (e.g., '1:30', '12:00')")
                  ),
                  
                  h4("Vertical Approach Angle (VAA)"),
                  p("Vertical Approach Angle describes the downward (or occasionally upward) angle of the baseball as it crosses home plate, measured in degrees relative to the ground."),
                  tags$ul(
                    tags$li("A more negative VAA means the pitch is entering the zone on a steeper downward plane, such as a sharp breaking ball or heavy sinker."),
                    tags$li("A less negative (flatter) VAA means the pitch is approaching more horizontally, which is common for high four-seam fastballs that “ride” through the zone."),
                    tags$li("Hitters track pitches based on expected drop. A flatter VAA can make a fastball play up in velocity and induce more swing and miss at the top of the zone, while a steep VAA can generate ground balls and weak contact."),
		    tags$li("The calculation for VAA can be found on FanGraphs.")
                  ),
                  
                  h4("Tilt"),
                  p("Tilt is a clock-face representation of a pitch’s spin axis,  showing how the seams are oriented as the ball travels."),
                  tags$ul(
                    tags$li("A spin axis of 12:00 is pure backspin (true four-seam fastball)"),
                    tags$li("6:00 is pure topspin (true 12-6 curveball)."),
                    tags$li("Angles such as 1:30 or 10:30 describe diagonal spin that creates combinations of horizontal and vertical break.")
                  )
                )
              )
      )
    )
  )
)
