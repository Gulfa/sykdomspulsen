meminfluensaUI <- function(id, label = "Counter", GLOBAL) {
  ns <- NS(id)
  fluidRow(
    column(
      width=2,
      selectInput(ns("influensaCounty"), "Fylke", as.list(GLOBAL$weeklyCounties), selected = GLOBAL$weeklyCounties[1]),
      uiOutput(ns("influensaSeason"))
    ),
    column(
      width=10,
      tabsetPanel(
        tabPanel(
          title="Figur",

          br(),
          div(style='height:80vh;text-align: center',plotOutput(ns("influensa_plot"), height="100%"))
        ),
        tabPanel(
          title="Info",
          br(),
          p("Info")
        )
      )
    )
  )
}

meminfluensaServer <- function(input, output, session, GLOBAL) {
  current_season <- fhi::season(GLOBAL$outbreaksyrwk[1], start_week=30)
  seasons <- pool %>%
    tbl("results_mem") %>%
    filter(tag_outcome == "influensa" &
             source == "data_norsyss" &
             !is.na(rp100_baseline_thresholdu0)) %>%
    distinct(season) %>% arrange(desc(season)) %>%
    collect()
  
  output$influensaSeason <- renderUI(selectInput(session$ns("influensaSeason"), "Sesong", as.list(seasons)$season, selected = current_season))
  
  influensa_data <- reactive({
    req(input$influensaCounty)
    req(input$influensaSeason)
    
    x_location <- input$influensaCounty
    x_season <- input$influensaSeason
    data <- pool %>%
      tbl("results_mem") %>%
      filter(season == x_season &
               source == "data_norsyss" & 
               location_code == x_location &
               tag_outcome == "influensa") %>%
      collect()
    setDT(data)
    data[, granularity_time:="weekly"]
    cat(file=stderr(), x_location)
    data <- sykdomspulsen::calculate_confidence_interval(data, last_weeks=10)
    return(data)
  })

  output$influensa_plot <- renderPlot({
    data <- influensa_data()
    t2 <- sykdomspulsen::get_location_name(input$influensaCounty)
    title<- paste0("Prosent pasienter med influensa i ", t2)
    q <- fhiplot::make_influenza_threshold_chart(data, title)
    return(q)

  })
}
