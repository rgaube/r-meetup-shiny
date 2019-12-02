

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


ex1_server_fixed <- function(input, output, session) {
  
  library(ggplot2)
  
  mean_return <- 0.05
  volatility <- 0.1
  iterations <- 100
  
  price_paths <- simulate_paths(iterations, mean_return, volatility)
  
  output$paths <- renderPlot({
    price_paths %>% 
      ggplot(aes(day, value, group=iteration)) +
      geom_line(color=rgb(0,170,112,0.3*255, maxColorValue = 255)) +
      theme_classic() +
      xlab("Days") +
      ylab("Stock price")
  })
  
  output$distribution <- renderPlot({
    price_paths %>% filter(day == max(day)) %>% 
      ggplot(aes(value)) +
      geom_histogram(fill=rgb(0,170,112,0.3*255, maxColorValue = 255)) + 
      theme_classic() +
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
  
  price_paths <- reactive({
    simulate_paths(input$iterations, input$mean_return, input$volatility)})
  
  output$paths <- renderPlot({
    price_paths() %>% 
      ggplot(aes(day, value, group=iteration)) +
      geom_line(color=rgb(0,170,112,0.3*255, maxColorValue = 255)) +
      theme_classic() +
      xlab("Days") +
      ylab("Stock price")
  })
  
  output$distribution <- renderPlot({
    price_paths() %>% 
      filter(day == max(day)) %>%
      ggplot(aes(value)) +
      geom_histogram(fill=rgb(0,170,112,0.3*255, maxColorValue = 255)) + 
      theme_classic() +
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

