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
2016-08-16
Respawn
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
.gif150
{
  width: 150%;
  height: auto;
}
</style>

---

## Ubisoft Blue Byte

- founded in 1988, best known for the *Settlers* and *Anno* brand
- fully acquired by Ubisoft in 2001
- online games since 2010
- latest title: [Anno 2205](http://anno-game.ubi.com/anno-2205/de-de/game-info/anno-2205/index.aspx) 
  (PC)
- next game: [Champions of Anteria](https://www.ubisoft.com/en-CA/game/champions-of-anteria/)


---

## Christoph Safferling

- since 2012: Head of Game Analytics at Blue Byte
- PhD in economics: [Three Essays on the Economics of Online Games](http://kops.uni-konstanz.de/handle/123456789/17259?locale-attribute=en)
- research specialties: personnel economics, incentive theory, industrial organisation
- academic papers available at [repec.org](https://ideas.repec.org/f/psa961.html)
- contact:
    - mail: christoph.safferling@ubisoft.com
    - LinkedIn: https://www.linkedin.com/in/safferling 
    - Twitter: [@safferli](https://twitter.com/safferli) 
    - Github: [@safferli](https://github.com/safferli)
    - this talk: https://github.com/safferli/introduction_shiny


--- &vertical

## Data are treasures?

- not data are treasures, but analysis thereof
    - if you have a manager: pretty trumps content
- many different libraries available
- R is known for being great with stats and graphs

***

## But... I'm creative! I don't need data! 

<!-- if you want to be an Indie developer, make your first games on someone else's budget. You don't want to make all those early mistakes on your own budget! -->

<!-- being an indie doesn't mean you can do what you want -- investor wants something, banks want something, unless you have your own money and you're willing to spend that -->

***

<img src="assets/img/archer-slap.gif" class="gif150" />


--- &vertical

## What is Shiny? 

> A web application framework for R  
> Turn your analyses into interactive web application. 
> No HTML, CSS, or JavaScript knowledge required  

http://shiny.rstudio.com/


***

<img src="assets/img/sailormoon.gif" class="gif150" />

***

<img src="assets/img/shiny.gif" class="gif150" />


---

## What is it good for? 

- dynamic R output
- reporting 
- dashboarding
- provide non-specialist access to data
- [make games](http://deanattali.com/blog/shiny-game-lightsout/)
- free


---

## Examples of awesomeness

- our own dashboards
    - ACID mission dashboard
    - Anno 2205 event sessions
- https://www.rstudio.com/products/shiny/shiny-user-showcase/ 


--- &vertical

## Build your own! 

- lognormal distribution: https://safferli.shinyapps.io/Lognormal_distribution/
- what do you need? 
    - R (https://www.r-project.org/)
    - a server (or use https://www.shinyapps.io/)
    - minimal code: `ui.R` and `server.R`

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


---

<img src="assets/img/free-roar.gif" class="gif150" />








