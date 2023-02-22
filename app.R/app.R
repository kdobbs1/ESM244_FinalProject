library(shiny)
library(tidyverse)

ui <- fluidPage(
  navbarPage("My app name",
             tabPanel("Tab 1"),
             tabPanel("Tab 2"),
             tabPanel("Tab 3")
             )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)