shinyUI(
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("daterange", "Date range:", 
                     start = max(df_raw$date)-7, end = max(df_raw$date), 
                     min = min(df_raw$date), max = max(df_raw$date)),
      hr(), 
      sliderInput("midpoint_heatmap", "Heatmap midpoint value:", min = 0, max = 1, value = 0.5),
      sliderInput("cutoff_heatmap", "Heatmap observations cutoff:", min = 0, max = 1000, value = 0),
      hr(), 
      selectInput('fail_factors', 'FailId factors', list_factors, 
                  selected = setdiff(list_factors, c("mission_name", "hireling_type", "hireling_class", "user_level", "date")), 
                  multiple=TRUE, selectize=TRUE),
      sliderInput("cutoff_failid_n", "FailId observations cutoff:", min = 0, max = 1000, value = 100),
      sliderInput("cutoff_failid_r", "FailId ratio cutoff:", min = 0, max = 1, value = 0.9),
      checkboxInput("ignore_failid", "FailId ignore 0", value = FALSE),
      hr(), submitButton(text = "Run"),
      width = 2
    ),
    mainPanel(
      navbarPage("ACID mission dashboard",
                 tabPanel("Title", includeMarkdown("titlepage.md")),
                 tabPanel("MissionCount", plotOutput("plot_mission_count_by_locobj", height = "900px")),
                 tabPanel("WinRate1", plotOutput("plot_win_rate_by_assassin_class", height = "900px")),
                 tabPanel("Winrate2", plotOutput("plot_win_rate_by_hireling_class", height = "900px")),
                 tabPanel("FailId", dataTableOutput("data_fail")),
                 tabPanel("Pivot", rpivotTableOutput("data_pivot")),
                 tabPanel("RawData", dataTableOutput("data_raw"))
      )
    )
  )
)