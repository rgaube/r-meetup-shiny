
#### Setup ####

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)

#### Prepare data and modules ####

stork <- readRDS("stork_data.rds")
source("modules.R", local=TRUE)




#### Define UI ####

routes_page <- fluidPage(birdSelectionInput("birdSelection"),
                         birdFlightUI("birdFlight"))

#### Define server ####

server <- function(input, output, session) {
  bird_selection <- callModule(birdSelection, "birdSelection")
  flight <- callModule(birdFlight, "birdFlight", bird_selection, cluster_icon=cluster_scaled_w_date)
}

#### Define UI skeletion ####


header <- dashboardHeader(
  title = tags$div(span(icon("user-cog")), "Bird migrations"),
  tags$li(a(href = 'https://www.advisori.de',
            img(src = 'https://www.advisori.de/wp-content/themes/advisori/images/logo.svg',
                title = "Company Home", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Migration routes", tabName = "routes", icon = icon("map")),
    menuItem("Summary", tabName = "summary", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "routes", routes_page)
  )
)


ui <- dashboardPage(skin = "black",
                    title = "Bird migrations",
                    header, sidebar, body)


#### Shiny app ####


shinyApp(ui = ui, server = server)

