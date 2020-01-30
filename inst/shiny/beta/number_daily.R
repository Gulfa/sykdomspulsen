number_dailyUI <- function(id, label = "Counter", GLOBAL) {
  ns <- NS(id)
  fluidRow(
    column(
      width=2,
      selectInput(ns("dailyType"), "Sykdom/Symptom", as.list(GLOBAL$dailyTypes), selected = GLOBAL$dailyTypes[1]),
      selectInput(ns("dailyAge"), "Alder", as.list(GLOBAL$dailyAges), selected = "Totalt"),
      selectInput(ns("dailyCounty"), "Fylke", as.list(GLOBAL$weeklyCounties), selected = GLOBAL$weeklyCounties[1])
    ),
    column(
      width=10,
      tabsetPanel(
        tabPanel(
          title="Figur",
          br(),
          div(style='height:60vh;text-align: center',plotOutput(ns("dailyNumberPlot"), height="100%")),
          div(style='height:200px;text-align: center',plotOutput(ns("dailyNumberPlotBrush"), height="100%", brush = brushOpts(ns("dailyNumberBrush"), direction="x", opacity=0.4)))
        ),
        tabPanel(
          title="Info",
          br(),
          p("Grafen viser antall konsultasjoner per dag med en indikasjon om antallet er som forventet eller ikke. Valg av sykdom/symptom, sted og tidsrom gjøres på høyre side. Den svarte streken med rundingene viser antallet faktiske konsultasjoner. Bakgrunnsfargen er laget ut fra beregninger fra de foregående 5 årene. Når den svarte streken ligger i den grønne bakgrunnsfargen er antallet konsultasjoner som forventet og rundingen vises med svart. Når den svarte streken ligger i det gule feltet er antall konsultasjoner høyere enn forventet og fyllet i rundingen blir gult. Dersom den svarte streken ligger i det røde feltet er antall konsultasjoner betydelig høyere enn forventet og fyllet i rundingen blir rødt."),
          p("Se fanen *Om Sykdomspulsen* øverst for mer utfyllende informasjon om dataene og beregninger.")
        )
      )
    )
  )
}

number_dailyServer <- function(input, output, session, GLOBAL) {
  start_date <- GLOBAL$dateMinRestrictedRecent

  dailyPlotBrushData <- reactive({
    req(input$dailyType)
    req(input$dailyCounty)
    req(input$dailyAge)

    x_tag <- input$dailyType
    x_location <- input$dailyCounty
    x_age <- input$dailyAge

    retData <- pool %>%
      tbl("results_qp") %>%
      filter(
        date >= start_date &
          tag_outcome == x_tag &
          source == "data_norsyss" & 
        location_code == x_location &
        granularity_time == "weekly" &
        age == x_age) %>%
      select(date, n, n_baseline_thresholdu0, n_baseline_thresholdu1, n_status) %>%
      collect()
    setDT(retData)
    retData <- retData[retData$date >= GLOBAL$dateMinRestrictedRecent,]

    return(retData)
  })

  dailyPlotData <- reactive({
    req(input$dailyType)
    req(input$dailyCounty)
    req(input$dailyAge)

    x_tag <- input$dailyType
    x_location <- input$dailyCounty
    x_age <- input$dailyAge

    retData <- pool %>%
      tbl("results_qp") %>%
      filter(
        date >= start_date &
          tag_outcome == x_tag &
          source == "data_norsyss" & 
          location_code == x_location &
          granularity_time == "daily" &
          age == x_age) %>%
      select(date, n, n_baseline_thresholdu0, n_baseline_thresholdu1, n_status) %>%
      collect()
    setDT(retData)
    
    retData$top <- max(c(retData$n, retData$n_baseline_thresholdu1), na.rm = T) + 2
    retData$bottom <- 0

    return(retData)
  })

  output$dailyNumberPlotBrush <- renderCachedPlot({
    pd <- dailyPlotBrushData()

    fhiplot::make_line_brush_plot(pd,x="date",dataVal="n",L2="n_baseline_thresholdu0",L3="n_baseline_thresholdu1", GetCols=GetCols)
  }, cacheKeyExpr={list(
    input$dailyType,
    input$dailyCounty,
    input$dailyAge,
    GLOBAL$dateMax
  )})

  output$dailyNumberPlot <- renderCachedPlot({
    pd <- dailyPlotData()

    if(!is.null(input$dailyNumberBrush)){
      pd <- pd[pd$date>=input$dailyNumberBrush$xmin-6 & pd$date<=input$dailyNumberBrush$xmax,]
    }

    t1 <- names(GLOBAL$dailyTypes)[GLOBAL$dailyTypes==input$dailyType]
    t2 <- sykdomspulsen::get_location_name(input$dailyCounty)

    title <- paste0(t1, " i ",t2, " (",input$dailyAge," alder)\n")

    fhiplot::make_line_threshold_plot(pd,x="date",dataVal="n",L1="bottom",L2="n_baseline_thresholdu0",L3="n_baseline_thresholdu1",L4="top",allPoints=FALSE,title=title,xShift=0.5, step=F, GetCols=GetCols, legend_position = "bottom")
  }, cacheKeyExpr={list(
    input$dailyType,
    input$dailyCounty,
    input$dailyAge,
    input$dailyNumberBrush,
    GLOBAL$dateMax
  )})

}
