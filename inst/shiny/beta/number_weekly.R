number_weeklyUI <- function(id, label = "Counter", GLOBAL) {
  ns <- NS(id)
  fluidRow(
    column(
      width=2,
      selectInput(ns("weeklyType"), "Sykdom/Symptom", as.list(GLOBAL$weeklyTypes), selected = GLOBAL$weeklyTypes[1]),
      selectInput(ns("weeklyAge"), "Alder", as.list(GLOBAL$weeklyAges), selected = "Totalt"),
      selectInput(ns("weeklyCounty"), "Fylke", as.list(GLOBAL$weeklyCounties), selected = GLOBAL$weeklyCounties[1]),
      uiOutput(ns("weeklyMunicip"))
    ),
    column(
      width=10,
      tabsetPanel(
        tabPanel(
          title="Figur",
          br(),
          div(style='height:60vh;text-align: center',plotOutput(ns("weeklyNumberPlot"), height="100%")),
          div(style='height:200px;text-align: center',plotOutput(ns("weeklyNumberPlotBrush"), height="100%", brush = brushOpts(ns("weeklyNumberBrush"), direction="x", opacity=0.4)))
        ),
        tabPanel(
          title="Info",
          br(),
          p("Grafen viser antall konsultasjoner per uke med en indikasjon om antallet er som forventet eller ikke. Valg av sykdom/symptom, sted og tidsrom gjøres på venstre side. Den svarte streken med rundingene viser antallet faktiske konsultasjoner. Bakgrunnsfargen er laget ut fra beregninger fra de foregående 5 årene i samme geografiske område. Når den svarte streken ligger i den grønne bakgrunnsfargen er antallet konsultasjoner som forventet og rundingen vises med svart. Når den svarte streken ligger i det gule feltet er antall konsultasjoner høyere enn forventet og fyllet i rundingen blir gult. Dersom den svarte streken ligger i det røde feltet er antall konsultasjoner betydelig høyere enn forventet og fyllet i rundingen blir rødt."),
          p("Se fanen *Om Sykdomspulsen* øverst for mer utfyllende informasjon om dataene og beregninger.")
        )
      )
    )
  )
}

number_weeklyServer <- function(input, output, session, GLOBAL) {
  weeklyMunicipChoices <- reactive({
    if (is.null(input$weeklyCounty))
      return(NULL)
    if (input$weeklyCounty == "Norge") {
      return("Norge")
    } else {
      data <- fd::norway_locations()[county_code==input$weeklyCounty]
      x <- data$municip_code
      names(x) <- data$municip_name

      return(c("Fylke", x))
    }
  })

  output$weeklyMunicip <- renderUI(selectInput(session$ns("weeklyMunicip"), "Kommune", as.list(weeklyMunicipChoices()), selected = weeklyMunicipChoices()[1]))

  ## weekly
  weeklyPlotData <- reactive({
    req(input$weeklyCounty)
    req(input$weeklyMunicip)
    req(input$weeklyType)
    req(input$weeklyAge)
    if (input$weeklyMunicip %in% c("Norge", "Fylke")) {
      x_tbl <- "results_qp"
      x_tag <- input$weeklyType
      x_location <- input$weeklyCounty
      x_age <- input$weeklyAge
    } else {
      x_tbl <- "results_qp"
      x_tag <- input$weeklyType
      x_location <- input$weeklyMunicip
      x_age <- input$weeklyAge
    }
    retData <- pool %>% tbl(x_tbl) %>%
      filter(
        tag_outcome == x_tag &
        location_code== x_location &
        granularity_time=="weekly" &
        source=="data_norsyss" & 
        age == x_age) %>%
      select(date, n, n_baseline_thresholdu0, n_baseline_thresholdu1, n_status,yrwk,n_denominator) %>%
      collect()
    setDT(retData)
    retData[, location_code:=x_location]
    retData[, granularity_time:="weekly"]
    retData[, age:=x_age]
    retData <- sykdomspulsen::calculate_confidence_interval(retData, last_weeks=8)
    
    retData$top <- max(c(retData$n, retData$n_baseline_thresholdu1), na.rm = T) + 2
    retData$bottom <- 0

    return(retData)
  })

  output$weeklyNumberPlotBrush <- renderCachedPlot({
    pd <- weeklyPlotData()
    fhiplot::make_line_brush_plot(pd,x="date",dataVal="n",L2="n_baseline_thresholdu0",L3="n_baseline_thresholdu1", GetCols=GetCols)
  }, cacheKeyExpr={list(
    input$weeklyCounty,
    input$weeklyMunicip,
    input$weeklyType,
    input$weeklyAge,
    GLOBAL$dateMax
  )})

  output$weeklyNumberPlot <- renderCachedPlot({
    pd <- weeklyPlotData()

    if(!is.null(input$weeklyNumberBrush)){
      pd <- pd[pd$date>=input$weeklyNumberBrush$xmin & pd$date<=input$weeklyNumberBrush$xmax,]
    }

    t1 <- names(GLOBAL$weeklyTypes)[GLOBAL$weeklyTypes==input$weeklyType]
    if(input$weeklyMunicip=="Fylke"){
      t2 <- sykdomspulsen::get_location_name(input$weeklyCounty)
    } else {
      t2 <- sykdomspulsen::get_location_name(input$weeklyMunicip)
    }
    title <- paste0(t1, " i ",t2, " (",input$weeklyAge," alder)\n")

    return(fhiplot::make_line_threshold_plot(pd,x="date",dataVal="n",L1="bottom",L2="n_baseline_thresholdu0",L3="n_baseline_thresholdu1",L4="top",title=title, pointShift = -3.5, weekNumbers=TRUE, step=F, GetCols=GetCols, legend_position = "bottom"))

  }, cacheKeyExpr={list(
    input$weeklyCounty,
    input$weeklyMunicip,
    input$weeklyType,
    input$weeklyAge,
    input$weeklyNumberBrush,
    GLOBAL$dateMax
  )}
  )

}


