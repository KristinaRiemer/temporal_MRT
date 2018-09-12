library(shiny)
library(ggplot2)
source("helpers.R")
mean_masses = read.csv("data/mean_masses.csv")

# ui
ui = fluidPage(
  titlePanel("Plots of species annual masses by site"), 
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
                  "Portal" = mean_masses[mean_masses$site == "portal",], 
                  "Fray Jorge" = mean_masses[mean_masses$site == "fray_jorge",])
    plot_masses(mass_data = data)
  })
  
}

# run
shinyApp(ui = ui, server = server)