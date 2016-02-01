---
title       : Game Analytics
subtitle    : 
author      : Christoph Safferling
job         : Head of Game Analytics
url         : {lib: "."}    # this is important for reveal.js
framework   : revealjs      # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
revealjs: 
    theme: default
--- ds:intro

<!--
2016-02-02
Ubisoft Analytics Summit
-->

<!-- adjust background for "intro" slides -->
<style>
html.intro body {
background:url("./assets/img/ubi-background.jpg");
background-position:center;
background-size: 100%;
} 
</style>


<p style="color: #13DAEC; font-family: 'Lato', sans-serif; font-size: 150%; margin: 0 0 10% 0;">
  R Shiny dashboards <br />
  <span style="font-size: 80%;">a practical introduction</span>
</p>

<p style="font-size: 100%; color: #000000;">
  Christoph Safferling
</p>
<p style="font-size: 80%; margin: 3% 0 0 0; color: #222222;">
  Head of Game Analytics <br/ > 
  Ubisoft Blue Byte
</p>

<!-- fuck CSS... why can't I include this in a custom CSS? -->

<style>
.reveal {color: #231F20;}
.reveal h2, .reveal h3 {color: #222222;}
</style>

--- &vertical

## What is Shiny? 

> A web application framework for R  
> Turn your analyses into interactive web application. 
> No HTML, CSS, or JavaScript knowledge required  

http://shiny.rstudio.com/


***

<img src="assets/img/sailormoon.gif" />

***

<img src="assets/img/shiny.gif" />



---

## What is it good for? 

- dynamic R output
- reporting 
- dashboarding
- provide non-specialist access to data
- [make games](http://deanattali.com/blog/shiny-game-lightsout/)!
- free!


---

## Examples of awesomeness

- DUS Game Analytics dashboards
    - http://10.130.64.74:3838/
- [retention dashboard](http://10.130.64.74:3838/apps/retention_dashboard/)
- [Anno 2205 online features](http://10.130.64.74:3838/apps/anno6_online_features_dashboard/)
- [Anno 2205 quest dashbaord](http://10.130.64.74:3838/apps/anno6_quest_dashboard/)


--- &vertical

## Build your own! 

- what do you need? 
    - R
    - a server 
    - minimal code: `ui.R` and `server.R`
- lognormal distribution: https://safferli.shinyapps.io/Lognormal_distribution/

***

### minimal code

**ui.R**


```r
shinyUI(fluidPage(
))
```

**server.R**


```r
shinyServer(function(input, output) {
})
```

***

### ui.R


```r
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("pt.mu.input"),
      sliderInput("pt.sd.input")
    ),
    mainPanel(
      plotOutput("plot")
    )
    )))
```

### server.R


```r
shinyServer(
  function(input, output) {
    output$plot <- renderPlot({
      
      pt.mu <- input$pt.mu.input
      pt.sd <- input$pt.sd.input
      ggplot(...)  
    }) 
  }
)
```

***

### ui.R


```r
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
```

***

### server.R


```r
library(dplyr)
library(ggplot2)

shinyServer(
  function(input, output) {
    output$plot <- renderPlot({
      
      pt.mu <- input$pt.mu.input
      pt.sd <- input$pt.sd.input
      
      grid <- data.frame(x.data = seq(0, 500, 0.1)) %>% 
        mutate(
          ln.data = dlnorm(x.data, meanlog=log(pt.mu), sdlog=pt.sd)
        )
      
      grid %>% ggplot()+
        geom_line(aes(x=x.data, y=ln.data))+
        geom_vline(xintercept = pt.mu, colour="red", linetype = "longdash")+
        annotate("text", x = (pt.mu+5), y = (max(grid$ln.data)), 
                 label = paste0("average playtime:\n", pt.mu, " minutes"), hjust=0)+
        xlab("playtime in minutes")+ylab("lognormal distribution function")
    }) 
  }
)
```

---

## Shiny dashboard

- https://rstudio.github.io/shinydashboard/
- standardised and simple way to set up dashboards
- used by us for all new dashboards (from Anno 2205 onwards)


---

## Resources

- tutorial: http://shiny.rstudio.com/tutorial/
- Shiny cheatsheet: http://shiny.rstudio.com/articles/cheatsheet.html
- examples: http://shiny.rstudio.com/gallery/
- fancy examples: http://www.rstudio.com/products/shiny/shiny-user-showcase/
- code to all our dashboards: https://gitlab-ncsa.ubisoft.org/bb-analytics/shiny-dashboards 


---

<img src="assets/img/free-roar.gif" />








