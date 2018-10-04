library(shiny)
library(ggplot2)
library(cowplot)
source("../temporal_MRT/R/plot_size_distributions.R")
occurrences = read.csv("../data/occurrences.csv")

# ui
ui = fluidPage(
  titlePanel("Site size distributions"), 
  sidebarLayout(
    sidebarPanel(
      selectInput("site", 
                  label = "Choose a site", 
                  choices = c("Portal", "Fray Jorge"), 
                  selected = "Portal")
    ), 
    mainPanel(plotOutput("map"))
  )
)

# server
server = function(input, output){
  output$map = renderPlot({
    data = switch(input$site, 
                  "Portal" = occurrences[occurrences$site == "portal",], 
                  "Fray Jorge" = occurrences[occurrences$site == "frayjorge",])
    plot_size_distributions(site_data = data)
  })
  
}

# run
shinyApp(ui = ui, server = server)