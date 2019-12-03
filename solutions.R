

library(shiny)
library(dplyr)


#### Constants ####

page_style <- "background-color:rgb(0,170,112,0.3);"



#### Example 1: Monte Carlo stock price evolution

draw_price_path <- function(mean_return, volatility, 
                            start=100, 
                            duration=1, days_per_period=250,
                            seed=NULL) {
  set.seed(seed = seed)
  
  tibble::tibble(day = seq(days_per_period),
                 period = day %% days_per_period,
                 log_return = rnorm(days_per_period*duration, 
                                    mean=mean_return/days_per_period, 
                                    sd=volatility/sqrt(days_per_period))) %>%
    mutate(return = exp(log_return),
           value = start*cumprod(return))
}

simulate_paths <- function(iterations, mean_return, volatility) {
  seq(iterations) %>% purrr::map_dfr(~draw_price_path(mean_return, volatility), 
                                     .id="iteration")
}


ex1_ui <- fluidPage(style=page_style,
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

ex2_ui <- fluidPage(style=page_style,
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

ex3_ui <- fluidPage(style=page_style,
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

ex3_server_reactive <- function(input, output, session) {
  
  library(ggplot2)
  
  plot_color <- rgb(153,27,86, 80, maxColorValue = 255)
  
  my_theme <- theme_classic() +
    theme(plot.background = element_rect(fill = rgb(0,170,112,0.3*255, maxColorValue = 255)))
  
  price_paths <- reactive({
    simulate_paths(input$iterations, input$mean_return, input$volatility)})
  
  observeEvent(price_paths(), {
    updateActionButton(session, "replot", label="Replot", icon=shiny::icon("exclamation-triangle"))
  })
  
  observeEvent(input$replot, {updateActionButton(session, "replot", "Up-to-date", icon=shiny::icon("check-circle"))})
  
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

