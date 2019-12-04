



#### Bird selection ####

birdSelectionInput <- function(id) {
  ns <- NS(id)
  
  t_day <- lubridate::today()
  
  tagList(
    fluidRow(
      box(
        selectInput(ns("bird_identifier"),
                    label = "Tracked animal",
                    choices = c()),
        sliderInput(ns("flight_date_range"),
                    "Observation period:",
                    min = t_day,
                    max = t_day,
                    value=c(t_day, t_day),
                    
                    timeFormat="%Y-%m-%d"))
      )
  )
}

birdSelection <- function(input, output, session) {
  
  ns <- session$ns
  
  identifier <- stork %>% 
    distinct(individual.local.identifier, tag.local.identifier) %>%
    tibble::deframe()
  
  updateSelectInput(session, "bird_identifier",
                    choices = identifier, 
                    selected = head(identifier, 1))
  
  
  
  bird_data <- reactive({
     stork %>% filter(tag.local.identifier == input$bird_identifier)
    })

  
  observeEvent(input$bird_identifier, {
    if (nrow(bird_data()) > 0) {
      minimum <- as.Date(min(bird_data()$timestamp))-1*lubridate::days()
      maximum <- as.Date(max(bird_data()$timestamp))+1*lubridate::days()
      
      updateSliderInput(session, "flight_date_range",
                        min = minimum,
                        max = maximum,
                        value = c(minimum, maximum))
    }
  })
  
  result <- reactiveValues()
  
  result$bird_flight_data <- reactive({
    bird_data() %>% filter(timestamp >= as.Date(input$flight_date_range[1]),
                           timestamp <= as.Date(input$flight_date_range[2]))
  })
  
  return(result)
}




### Bird flight ####

birdFlightUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(sliderInput(ns("min_flight_speed"), "Minimum flight speed:", 
                      min=0, max=10, value=5, step = 0.1),
          leafletOutput(ns("migrations"))
      )
    )
  )
}



birdFlight <- function(input, output, session, selection, cluster_icon) {

  min_flight_speed <- reactive({
    input$min_flight_speed
  }) %>% debounce(1000)
  
  bird_plot_data <- reactive({

    selection$bird_flight_data() %>%
      mutate(in_flight = ground.speed > input$min_flight_speed,
             start_flight = in_flight & !lag(in_flight),
             stop_flight = !in_flight & lag(in_flight))
  })
  
  
  ####
  output$migrations <- renderLeaflet({
    
    if (nrow(bird_plot_data()) > 0) {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolylines(lng = ~location.long,
                     lat = ~location.lat,
                     #color=~pathColor,
                     data=bird_plot_data() %>% filter(in_flight | stop_flight)) %>%
        addMarkers(lng = ~location.long,
                   lat = ~location.lat,
                   label = ~timestamp,
                   options = markerOptions(timestamp = ~timestamp),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = TRUE,
                                                         zoomToBoundsOnClick = TRUE,
                                                         iconCreateFunction = cluster_icon),
                   #color="yellow",
                   data=bird_plot_data() %>% filter(!in_flight))
    } else {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron)
    }
  })
  
}

cluster_scaled_w_date <- JS("
    function (cluster) {
        var markers = cluster.getAllChildMarkers();
        var startdate = new Date(markers[0].options.timestamp);
        for (i = 0; i < markers.length; i++) {
            date = new Date(markers[i].options.timestamp)
            if (date < startdate) {
                startdate = date
            }
        }
        size = Math.max(40, Math.sqrt(markers.length)/2);
        out = startdate.toLocaleDateString('de-DE', {day: '2-digit', month: '2-digit'});
        return new L.DivIcon({ html: '<div style=\"width:90%;margin-left:5%\"><span>'+out+'</span></div>', 
                                className: 'marker-cluster marker-cluster-medium',
                                iconSize: new L.Point(size, size)});
    }")
