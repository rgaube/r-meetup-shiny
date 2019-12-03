#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

source("../solutions.R", local=TRUE)

## Uncomment to break test
# button_labels <- list(replot="Replot it", ok="Plot is fine")

# Run the application 
shinyApp(ui = ex3_ui, server = ex3_server_reactive)
