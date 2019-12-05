

library(shiny)
library(dplyr)


#### Constants ####

page_style <- "background-color:rgb(0,170,112,0.3);"



#### Example: Monte Carlo stock price evolution ####

draw_price_path <- function(mean_return, volatility, 
                            start=100, 
                            duration=1, days_per_period=250,
                            seed=NULL) {
  if (!missing(seed))
    set.seed(seed = seed)
  
  tibble::tibble(day = seq(days_per_period),
                 period = day %% days_per_period,
                 log_return = rnorm(days_per_period*duration, 
                                    mean=log(1+mean_return)/days_per_period, 
                                    sd=volatility/sqrt(days_per_period))) %>%
    mutate(return = exp(log_return),
           value = start*cumprod(return))
}

simulate_paths <- function(iterations, mean_return, volatility) {
  seq(iterations) %>% purrr::map_dfr(~draw_price_path(mean_return, volatility), 
                                     .id="iteration")
}


#### Example: UI ####

ex1_ui <- function() {
  fluidPage(style=page_style,
            fluidRow(h2("Inputs"),
                     sliderInput("mean_return", "Mean return", min=0, max=1, value=0.05),
                     sliderInput("volatility", "Volatility", min=0, max=0.5, value=0.1),
                     sliderInput("iterations", "Iterations", min=1, max=1000, value=100)
            ),
            fluidRow(h2("Outputs"),
                     plotOutput("paths"),
                     plotOutput("distribution"),
                     tableOutput("distribution_summary"))
  )
}

ex2_ui <- function() {
  fluidPage(style=page_style,
            fluidRow(h2("Inputs"),
                     column(4,sliderInput("mean_return", "Mean return", min=0, max=1, value=0.05)),
                     column(4,sliderInput("volatility", "Volatility", min=0, max=0.5, value=0.1)),
                     column(4,sliderInput("iterations", "Iterations", min=1, max=1000, value=100))
            ),
            fluidRow(h2("Outputs"),
                     tabsetPanel(
                       tabPanel("Paths", plotOutput("paths")),
                       tabPanel("Distribution", plotOutput("distribution")),
                       tabPanel("Summary", tableOutput("distribution_summary")))
            )
  )
}

ex3_ui <- function() {
  fluidPage(style=page_style,
            fluidRow(h2("Inputs"),
                     column(4,sliderInput("mean_return", "Mean return", min=0, max=1, value=0.05)),
                     column(4,sliderInput("volatility", "Volatility", min=0, max=0.5, value=0.1)),
                     column(4,sliderInput("iterations", "Iterations", min=1, max=1000, value=100))
            ),
            fluidRow(h2("Outputs"),
                     tabsetPanel(
                       tabPanel("Paths", 
                                actionButton("replot", " ", icon("exclamation-triangle")),
                                plotOutput("paths")),
                       tabPanel("Distribution", plotOutput("distribution")),
                       tabPanel("Summary", tableOutput("distribution_summary")))
            )
  )
}
  

#### Example: Server functions ####

ex1_server_fixed <- function(input, output, session) {
  
  library(ggplot2)
  
  plot_color <- rgb(153,27,86, 80, maxColorValue = 255)
  
  my_theme <- theme_classic() +
    theme(plot.background = element_rect(fill = rgb(0,170,112,0.3*255, maxColorValue = 255)))
  
  mean_return <- 0.05
  volatility <- 0.1
  iterations <- 100
  
  price_paths <- simulate_paths(iterations, mean_return, volatility)
  
  output$paths <- renderPlot({
    price_paths %>% 
      filter(day %% 5 == 1 | day == max(day))  %>%
      ggplot(aes(day, value, group=iteration)) +
      geom_line(color=plot_color) +
      my_theme +
      xlab("Days") +
      ylab("Stock price")
  })
  
  output$distribution <- renderPlot({
    price_paths %>% filter(day == max(day)) %>% 
      ggplot(aes(value)) +
      geom_histogram(fill=plot_color) + 
      theme_classic() +
      theme(plot.background = element_rect(fill = rgb(0,170,112,0.3*255, maxColorValue = 255))) +
      xlab("Stock price") +
      ylab("Count")
  })
  
  output$distribution_summary <- renderTable({
    price_paths %>% 
      filter(day == max(day)) %>%
      pull(value) %>%
      summary() %>% as.list()
    })
}

ex1_server_reactive <- function(input, output, session) {
  
  library(ggplot2)
  
  plot_color <- rgb(153,27,86, 80, maxColorValue = 255)
  
  my_theme <- theme_classic() +
    theme(plot.background = element_rect(fill = rgb(0,170,112,0.3*255, maxColorValue = 255)))
  
  price_paths <- reactive({
    simulate_paths(input$iterations, input$mean_return, input$volatility)})
  
  output$paths <- renderPlot({
    price_paths() %>% 
      ggplot(aes(day, value, group=iteration)) +
      geom_line(color=plot_color) +
      my_theme +
      xlab("Days") +
      ylab("Stock price")
  })
  
  output$distribution <- renderPlot({
    price_paths() %>% 
      filter(day == max(day)) %>%
      ggplot(aes(value)) +
      geom_histogram(fill=plot_color) + 
      my_theme +
      xlab("Stock price") +
      ylab("Count")
  })
  
  output$distribution_summary <- renderTable({
    price_paths() %>% 
      filter(day == max(day)) %>%
      pull(value) %>%
      summary() %>% as.list()
    })
}


button_labels <- list(replot="Replot",
                      ok="Up-to-date")

ex3_server_reactive <- function(input, output, session) {
  
  library(ggplot2)
  
  plot_color <- rgb(153,27,86, 80, maxColorValue = 255)
  
  my_theme <- theme_classic() +
    theme(plot.background = element_rect(fill = rgb(0,170,112,0.3*255, maxColorValue = 255)))
  
  price_paths <- reactive({
    simulate_paths(input$iterations, input$mean_return, input$volatility)})
  
  observeEvent(price_paths(), {
    updateActionButton(session, "replot", 
                       label=button_labels$replot, 
                       icon=shiny::icon("exclamation-triangle"))
  })
  
  observeEvent(input$replot, {updateActionButton(session, "replot", 
                                                 label=button_labels$ok,
                                                 icon=shiny::icon("check-circle"))})
  
  delayed_price_paths_plot <- eventReactive(input$replot, {
    price_paths() %>% 
      ggplot(aes(day, value, group=iteration)) +
      geom_line(color=plot_color) +
      my_theme +
      xlab("Days") +
      ylab("Stock price")
  })
  
  output$paths <- renderPlot({
    delayed_price_paths_plot()
    })
  
  output$distribution <- renderPlot({
    price_paths() %>% 
      filter(day == max(day)) %>%
      ggplot(aes(value)) +
      geom_histogram(fill=plot_color) + 
      my_theme +
      xlab("Stock price") +
      ylab("Count")
  })
  
  output$distribution_summary <- renderTable({
    price_paths() %>% 
      filter(day == max(day)) %>%
      pull(value) %>%
      summary() %>% as.list()
  })
}



#### Example: Modules ####


ex4_moduleInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(h2("Inputs"),
             column(4,sliderInput(ns("mean_return"), "Mean return", min=0, max=1, value=0.05)),
             column(4,sliderInput(ns("volatility"), "Volatility", min=0, max=0.5, value=0.1)),
             column(4,sliderInput(ns("iterations"), "Iterations", min=1, max=1000, value=100))
    )
  )
}

ex4_moduleOutput <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(h2("Outputs"),
             tabsetPanel(
               tabPanel("Paths", 
                        actionButton(ns("replot"), " ", icon("exclamation-triangle")),
                        plotOutput(ns("paths"))),
               tabPanel("Distribution", plotOutput(ns("distribution"))),
               tabPanel("Summary", tableOutput(ns("distribution_summary"))))
    )
  )
}


ex4_calc_paths <- function(input, output, session) {
  reactive({
    simulate_paths(input$iterations, input$mean_return, input$volatility)})
}

ex4_plot <- function(input, output, session,
                     price_paths,
                     plot_color = rgb(153,27,86, 80, maxColorValue = 255),
                     bg_color = rgb(0,170,112,0.3*255, maxColorValue = 255)) {
  
  library(ggplot2)
  
  ns <- session$ns

  my_theme <- theme_classic() +
    theme(plot.background = element_rect(fill = bg_color ))
  
  
  
  observeEvent(price_paths(), {updateActionButton(session, "replot", 
                                                  label=button_labels$replot, 
                                                  icon=shiny::icon("exclamation-triangle"))
  })
  
  observeEvent(input$replot, {updateActionButton(session, "replot", 
                                                 label=button_labels$ok,
                                                 icon=shiny::icon("check-circle"))})
  
  delayed_price_paths_plot <- eventReactive(input$replot, {
    price_paths() %>% 
      ggplot(aes(day, value, group=iteration)) +
      geom_line(color=plot_color) +
      my_theme +
      xlab("Days") +
      ylab("Stock price")
  })
  
  output$paths <- renderPlot({
    delayed_price_paths_plot()
  })
  
  output$distribution <- renderPlot({
    price_paths() %>% 
      filter(day == max(day)) %>%
      ggplot(aes(value)) +
      geom_histogram(fill=plot_color) + 
      my_theme +
      xlab("Stock price") +
      ylab("Count")
  })
  
  output$distribution_summary <- renderTable({
    price_paths() %>% 
      filter(day == max(day)) %>%
      pull(value) %>%
      summary() %>% as.list()
  })
}
