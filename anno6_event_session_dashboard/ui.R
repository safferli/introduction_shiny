##############
# ANNO 6 EVENT SESSION DASHBOARD - ui.R
##############

##############
# DASHBOARD LAYOUT
##############

dashboard_layout <- list(
  textOutput("invisible"),
  fluidRow(
    valueBoxOutput("usersBox", width = 2),
    valueBoxOutput("timeBox", width = 2),
    valueBoxOutput("avgEventsBox", width = 2),
    valueBoxOutput("avgEventsCorpBox", width = 2),
    valueBoxOutput("totalEventsBox", width = 2),
    valueBoxOutput("successBox", width = 2)
  ),
  fluidRow(
    box(title = "Information", status = "primary", solidHeader = TRUE,
      includeMarkdown(paste0("titlepage.md")),
      width = 12
    )
  )
)


##############
# CHARTS PAGES LAYOUTS
##############

# EVENTS PLAYED AND PLAYTIME
charts1_layout <- list(
  fluidRow(
    column(width = 7,
      box(title = "Number of Times Each Event is Played", status = "primary", solidHeader = TRUE,
          plotOutput("plot11"),
          
          # View Data button
          bsButton("viewdata11", "View Data", style = "primary", icon = icon("table")),
          bsModal("modal11", "Data", "viewdata11", size = "large", dataTableOutput("datatable11")),
          
          width = NULL)
    ),
    column(width = 3,
      box(title = "Inputs", status = "warning", solidHeader = TRUE,
          selectInput("selectdiff11", label = "Corporation Difficulty:", 
                      choices = list("Easy", "Normal", "Hard", "Classic"), 
                      selected = "Easy"),
          width = NULL)
    ),
    column(width = 2)
  ),
  fluidRow(
    box(title = "Time to Completion", status = "primary", solidHeader = TRUE,
        plotOutput("plot12"),
        
        # View Data button
        bsButton("viewdata12", "View Data", style = "primary", icon = icon("table")),
        bsModal("modal12", "Data", "viewdata12", size = "large", dataTableOutput("datatable12")),
        
        width = 7)
  ),
  fluidRow(
    column(width = 7,
      box(title = "Event Sessions Completed per Corporation", status = "primary", solidHeader = TRUE,
          plotOutput("plot14"),
               
          # View Data button
          bsButton("viewdata14", "View Data", style = "primary", icon = icon("table")),
          bsModal("modal14", "Data", "viewdata14", size = "large", dataTableOutput("datatable14")),
               
          width = NULL)
    ),
    column(width = 3,
           box(title = "Inputs", status = "warning", solidHeader = TRUE,
               selectInput("selectdiff14", label = "Corporation Difficulty:", 
                           choices = list("All", "Easy", "Normal", "Hard", "Classic"), 
                           selected = "All"),
               selectInput("selectplot14", label = "Plot Type:",
                           choices = list("Boxplot", "Density", "Violin"),
                           selected = "Boxplot"),
               width = NULL)
    )
  ),
  fluidRow(
    box(title = "Total Events Played", status = "primary", solidHeader = TRUE,
        plotOutput("plot13"),
        
        # View Data button
        bsButton("viewdata13", "View Data", style = "primary", icon = icon("table")),
        bsModal("modal13", "Data", "viewdata13", size = "large", dataTableOutput("datatable13")),
        
        width = 7),
    box(title = "Inputs", status = "warning", solidHeader = TRUE,
        selectInput("selectdiff13", label = "Corporation Difficulty:", 
                    choices = list("Easy", "Normal", "Hard", "Classic"), 
                    selected = "Easy"),
        checkboxInput("checkbox13", label = "Cumulative", value = TRUE),
        width = 3)
  )
)

# RARE MATERIALS AND REWARDS
charts2_layout <-list(
  fluidRow(
    box(title = "Amount of Rare Material Earned", status = "primary", solidHeader = TRUE,
        plotOutput("plot21"),
        
        # View Data button
        bsButton("viewdata21", "View Data", style = "primary", icon = icon("table")),
        bsModal("modal21", "Data", "viewdata21", size = "large", dataTableOutput("datatable21")),
        
        width = 7),
    box(title = "Inputs", status = "warning", solidHeader = TRUE,
        selectInput("selectdiff21", label = "Corporation Difficulty:",
                    choices = list("Easy", "Normal", "Hard", "Classic"),
                    selected = "Easy"),
        selectInput("selectmat21", label = "Rare Material:", 
                    choices = list("Base","Earth","Moon","Polar","All"),
                    selected = "Base"),
        width = 3)
  ),
  fluidRow(
    box(title = "Rare Materials Earned per Event Session", status = "primary", solidHeader = TRUE,
        plotOutput("plot22"),
        
        # View Data button
        bsButton("viewdata22", "View Data", style = "primary", icon = icon("table")),
        bsModal("modal22", "Data", "viewdata22", size = "large", dataTableOutput("datatable22")),
        
        width = 7)
  )
)

# SPECIAL ACTIONS
charts3_layout <- list(
  fluidRow(
    box(title = "Special Actions by Event Heatmap", status = "primary", solidHeader = TRUE,
        plotOutput("plot31"),
        
        # View Data button
        bsButton("viewdata31", "View Data", style = "primary", icon = icon("table")),
        bsModal("modal31", "Data", "viewdata31", size = "large", dataTableOutput("datatable31")),
        
        width = 7),
    box(title = "Inputs", status = "warning", solidHeader = TRUE,
        selectInput("selectdiff31", label = "Corporation Difficulty:",
                    choices = list("Easy", "Normal", "Hard", "Classic"),
                    selected = "Easy"),
        width = 3)
  ),
  fluidRow(
    box(title = "Special Actions by Level Heatmap", status = "primary", solidHeader = TRUE,
        plotOutput("plot32"),
        
        # View Data button
        bsButton("viewdata32", "View Data", style = "primary", icon = icon("table")),
        bsModal("modal32", "Data", "viewdata32", size = "large", dataTableOutput("datatable32")),
        
        width = 7)
  )
)

# MILITARY LEVEL
charts4_layout <- list(
  fluidRow(
    box(title = "Military Level", status = "primary", solidHeader = TRUE,
        plotOutput("plot41"),
        
        # View Data button
        bsButton("viewdata41", "View Data", style = "primary", icon = icon("table")),
        bsModal("modal41", "Data", "viewdata41", size = "large", dataTableOutput("datatable41")),
        
        width = 7),
    box(title = "Inputs", status = "warning", solidHeader = TRUE,
        selectInput("selectdiff41", label = "Corporation Difficulty:",
                    choices = list("Easy", "Normal", "Hard", "Classic"),
                    selected = "Easy"),
        width = 3)
  )
)

##############
# DATA PAGES LAYOUT
##############
# RAW DATA TABLE
data1_layout <- list(
  #dataTableOutput('data_raw')
  div(style = 'overflow-x: scroll', dataTableOutput('data_raw'))
)

# SUMMARY DATA
data2_layout <- list(
  fluidRow(
    box(title = "Input", status = "warning", solidHeader = TRUE,
        selectInput("selectdata", label = "Summary Table:",
                    choices = list("Won Sessions"=1, "Session Times"=2, "Rare Materials"=3,
                                   "Special Actions"=4, "Military Level"=5),
                    selected = 1),
        selectInput("selectdiff_data", label = "Corporation Difficulty:",
                    choices = list("All", "Easy", "Normal", "Hard", "Classic"),
                    selected = "All"),
        downloadButton("download_data", label = "Download Excel", class="btn-info"),
        width = 2),
    box(title = "Raw Tracking Data", status = "primary", solidHeader = TRUE,
        div(style = 'overflow-x: scroll', dataTableOutput('data_summary')), width = 10)
  )
)

# PIVOT TABLE
# data3_layout <- list(
#   fluidRow(
#     box(title = "Pivot Table", status = "primary",
#         div(style = 'overflow-y: scroll', rpivotTableOutput("data_pivot", width = "100%", height = "800px")),
#       #rpivotTableOutput("data_pivot"),
#       width = 10
#     )
#   )
# )

##############
# ANALYSIS LAYOUT
##############
analysis_layout <- list(
  fluidRow(
    box(title = "Histogram", status = "primary", solidHeader = TRUE,
      plotOutput("histogram_plot"),
      textOutput("histogram_text"),
      width = 10
    )
  ),
  fluidRow(
    box(title = "Survival Function", status = "primary", solidHeader = TRUE,
        plotOutput("survival_plot"),
        textOutput("survival_text"),
        width = 10
    )
  ),
  fluidRow(
    box(
      title = "Hazard Rate", status = "primary", solidHeader = TRUE,
      plotOutput("hazard_plot"),
      textOutput("hazard_text"),
      width = 10
    )
  ),
  fluidRow(
    box(
      title = "Curtate Expectation", status = "primary", solidHeader = TRUE,
      plotOutput("curtate_plot"),
      textOutput("curtate_text"),
      width = 10
    )
  )
)


##############
# UI SKELETON
##############
dashboardPage(
  
  ## HEADER
  dashboardHeader(title = "Event Session Dashboard", titleWidth = 280),
  
  ## SIDEBAR
  dashboardSidebar(
    sidebarMenu(
      # Image
      img(src="anno6.jpg", height = 250, width = 250),
      
      # Button to query data
      fluidRow(),
      bsButton("querybutton", "No Data Loaded", style="danger", width = "100%"),
      bsAlert("no_data_popup"),
      
      # Date range input
      dateRangeInput("dates", label = "Date Range:",
                     min = as.Date("2015-09-25"), max = Sys.Date()-1,
                     start = as.Date("2015-11-02"), end = Sys.Date()-1),
      
      # Corporation Level Select input
      sliderInput("level_slider", label = "Corporation Level Range:",
                  min = 1, max = 250, value = c(1, 250)),
      
      # Information tab
      menuItem("Information", tabName = "dashboard", icon = icon("dashboard")),
      
      # Charts tab
      menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o"),
               menuItem("Events Played and Playtime", tabName = "charts1"),
               menuItem("Rare Material Rewards", tabName = "charts2"),
               menuItem("Special Actions", tabName = "charts3"),
               menuItem("Military Level", tabName = "charts4")
      ),
      
      # Data tab
      menuItem("Data", tabName = "data", icon = icon("table"),
               menuItem("Raw Data", tabName = "data1"),
               menuItem("Summary Data", tabName = "data2")
               # menuItem("Pivot Table", tabName = "data3")
      ),
      
      # Analysis
      menuItem("Survival Analysis", tabName = "analysis", icon = icon("align-left"),
               badgeLabel = "new", badgeColor = "green")
    ),
    width = 250
  ),
  
  ## BODY
  dashboardBody(
    tabItems(
      # DASHBOARD
      tabItem(tabName = "dashboard",
              dashboard_layout),
    
      # CHARTS
      tabItem(tabName = "charts1",
              charts1_layout),
      tabItem(tabName = "charts2",
              charts2_layout),
      tabItem(tabName = "charts3",
              charts3_layout),
      tabItem(tabName = "charts4",
              charts4_layout),
    
      # DATA
      tabItem(tabName = "data1",
              data1_layout),
      tabItem(tabName = "data2",
              data2_layout),
#       tabItem(tabName = "data3",
#               data3_layout),
      
      # ANALYSIS
      tabItem(tabName = "analysis",
              analysis_layout)
    
    )
  )
)
