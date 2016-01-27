# ui.R

shinyUI(fluidPage(
  titlePanel("A quick shiny visualisation of a lognormal distribution"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Input values"),
      br(),
      
      sliderInput("pt.mu.input", label = ("Chose avg playtime:"),
                  min = 10, max = 100, value = 45),
      br(), 
      
      sliderInput("pt.sd.input", label = ("Chose playtime dispersion:"),
                  min = 0, max = 2.0, value = 0.7, step= 0.1),
      br()
    ),
      
    mainPanel(
      h1("Output"),
      
      ("Playtime follows a log-normal distribution:"),
      a("(wikipedia article)", href="https://en.wikipedia.org/wiki/Log-normal_distribution"),
      
      plotOutput("plot")
    )
  )
))

