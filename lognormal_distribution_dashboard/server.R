# server.R

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
          #ln.sample = rlnorm(x.data, meanlog=pt.mu, sdlog=pt.sd)
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

