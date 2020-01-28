barometerUI <- function(id, label = "Counter", GLOBAL) {
  ns <- NS(id)
  fluidRow(
    column(
      width=2,
      selectInput(ns("weeklyBarometerType"), "Sykdom/Symptom", as.list(GLOBAL$weeklyTypes), selected = GLOBAL$weeklyTypes[1]),
      selectInput(ns("weeklyBarometerAge"), "Alder", as.list(GLOBAL$weeklyAges), selected = "Totalt"),
      selectInput(ns("weeklyBarometerCounty"), "Fylke", as.list(GLOBAL$weeklyCounties), selected = GLOBAL$weeklyCounties[1])
    ),
    column(
      width=10,
      tabsetPanel(
        tabPanel(title="Figur",
          br(),
          div(style='height:60vh;text-align: center',plotOutput(ns("weeklyBarometerPlot"), height="100%")),
          div(style='height:200px;text-align: center',plotOutput(ns("weeklyBarometerPlotBrush"), height="100%", brush = brushOpts(ns("weeklyBarometerBrush"), direction="x", opacity=0.4)))
        ),
        tabPanel(
          title="Info",
          br(),
          p("Tabellen viser en oversikt over forekomsten av sykdom/symptom i et valgt tidsrom."),
          p("Valg av tidsrom gjøres på tidslinje nederst på siden. Valg av sykdom/symptom gjøres på venstre side. På venstre side kan man også velge Norge eller et fylke i Norge. Hvis man velger Norge vil hvert fylke få en rad i tabellen. Hvis man velger et fylke vil alle kommunene i valgte fylke få en rad i tabellen."),
          p("Dersom ruten for en gitt uke er farget med grønn farge betyr det at antall konsultasjoner i den gitte kommunen eller fylket er som forventet denne uken. En gul farge en gitt uke betyr at antall konsultasjoner i den gitte kommunen eller fylket er høyere enn forventet denne uken. En rød farge en gitt uke betyr at antall konsultasjoner i den gitte kommunen eller fylket er betydelig høyere enn forventet denne uken. Fargene er laget ut fra beregninger fra de foregående 5 årene i fylke eller kommunen."),
          p("Se fanen *Om Sykdomspulsen* øverst for mer utfyllende informasjon om dataene og beregninger.")
        )
      )
    )
  )
}

barometerServer <- function(input, output, session, GLOBAL) {
  start_date <- GLOBAL$dateMinRestrictedRecent

  weeklyBarometerPlotBrushData <- reactive({
    req(input$weeklyBarometerType)
    req(input$weeklyBarometerCounty)
    req(input$weeklyBarometerAge)

    x_tag <- input$weeklyBarometerType
    x_location <- input$weeklyBarometerCounty
    x_age <- input$weeklyBarometerAge
    retData <- pool %>% tbl("results_qp") %>%
      filter(
        date >= start_date &
          tag_outcome == x_tag &
          source == "data_norsyss" &
        location_code== x_location &
        granularity_time =="weekly" &
        age== x_age
      ) %>% collect()
    setDT(retData)
    return(retData)
  })


  weeklyBarometerPlotData <- reactive({
    req(input$weeklyBarometerCounty)
    req(input$weeklyBarometerType)
    req(input$weeklyBarometerAge)

    x_table <- "results_qp"
    x_tag <- input$weeklyBarometerType
    x_age <- input$weeklyBarometerAge
    x_county <- input$weeklyBarometerCounty

    if(x_county=="norge"){
      retData <- pool %>% tbl(x_table) %>%
        filter(
          date >= start_date &
          tag_outcome == x_tag &
          granularity_time == "weekly" &
          source == "data_norsyss" &
          (granularity_geo == "county" | granularity_geo == "Norge") &
          age==x_age
        ) %>%
        select(date, location_code, n_status) %>%
        collect()
    } else {
      retData <- pool %>% tbl(x_table) %>%
        filter(
          date >= start_date &
          tag_outcome == x_tag &
          age==x_age &
          source == "data_norsyss" &
          granularity_time == "weekly" &
          location_code %in% sykdomspulsen::get_municips_county(x_county)
        ) %>%
        select(date, location_code, n_status) %>%
        collect()
    }
    setDT(retData)
    retData[, location_name:=sykdomspulsen::get_location_name(location_code)]
    return(retData)
  })

  output$weeklyBarometerPlotBrush <- renderCachedPlot({
    pd <- weeklyBarometerPlotBrushData()

    fhiplot::make_line_brush_plot(pd,x="date",dataVal="n",L2="n_baseline_thresholdu0",L3="n_baseline_thresholdu1", GetCols=GetCols)
  }, cacheKeyExpr={list(
    input$weeklyBarometerCounty,
    input$weeklyBarometerType,
    input$weeklyBarometerAge,
    GLOBAL$dateMax
  )})

  output$weeklyBarometerPlot <- renderCachedPlot({
    pd <- weeklyBarometerPlotData()

    if(!is.null(input$weeklyBarometerBrush)){
      pd <- pd[pd$date>=input$weeklyBarometerBrush$xmin & pd$date<=input$weeklyBarometerBrush$xmax,]
    }

    pd <- pd[,c("date","location_name","n_status"),with=F]
    t1 <- names(GLOBAL$weeklyTypes)[GLOBAL$weeklyTypes==input$weeklyBarometerType]
    t2 <- sykdomspulsen::get_location_name(input$weeklyBarometerCounty)
    title <- paste0(t1, " i ",t2, " (",input$weeklyBarometerAge," alder)\n")

    MakeBarometerPlot(pd, title=title, GetCols=GetCols)
  }, cacheKeyExpr={list(
    input$weeklyBarometerCounty,
    input$weeklyBarometerType,
    input$weeklyBarometerAge,
    input$weeklyBarometerBrush,
    GLOBAL$dateMax
  )})
}

MakeBarometerPlot <- function(pd, title, GetCols){
  location_nameOrder <- fd::norway_locations_long()$location_name[fd::norway_locations_long()$location_name %in% unique(pd$location_name)]
  location_nameOrder <- c("1 uke",rev(unique(location_nameOrder))," 1 uke")

  skeleton <- data.table(expand.grid(seq(min(pd$date)-6,max(pd$date),by=1),location_nameOrder,stringsAsFactors = FALSE))
  setnames(skeleton,c("date","location_name"))
  pd <- merge(skeleton,pd,by=c("location_name","date"),all.x=TRUE)
  pd[pd$location_name=="1 uke",]$n_status <- rep(c(rep("White",7),rep("Black",7)),sum(pd$location_name=="1 uke"))[1:sum(pd$location_name=="1 uke")]
  pd[pd$location_name==" 1 uke",]$n_status <- rep(c(rep("White",7),rep("Black",7)),sum(pd$location_name==" 1 uke"))[1:sum(pd$location_name==" 1 uke")]

  pd$printWeek <- ""
  pd$printWeekYear <- ""
  pd[pd$location_name %in% c("1 uke"," 1 uke"),]$printWeek <- format.Date(pd[pd$location_name %in% c("1 uke"," 1 uke"),]$date,"%V")
  pd[pd$location_name %in% c("1 uke"," 1 uke"),]$printWeekYear <- format.Date(pd[pd$location_name %in% c("1 uke"," 1 uke"),]$date,"%V/%G")

  setorder(pd,location_name,date)
  indexErase <- which(c(1:nrow(pd)%%7+1)!=4)
  pd[indexErase,]$printWeek <- ""
  pd[indexErase,]$printWeekYear <- ""

  pd$location_name <- factor(pd$location_name,levels=location_nameOrder)
  setorder(pd,location_name,-date)
  varNames <- "n_status"
  pd$n_status <- zoo::na.locf(pd$n_status)

  includeNormal <- sum(pd$n_status=="Normal")>0
  includeMedium <- sum(pd$n_status=="Medium")>0
  includeHigh <- sum(pd$n_status=="High")>0

  colours <- NULL
  if(includeHigh) colours <- c(colours,GetCols()[1])
  if(includeMedium) colours <- c(colours,GetCols()[2])

  limits <- range(pd$date)
  limitsSize <- max(1,(limits[2] - limits[1])*0.005)
  limits[1] <- limits[1] - limitsSize
  limits[2] <- limits[2] + limitsSize

  q <- ggplot(pd,aes(x=date,y=location_name))
  q <- q + geom_tile(aes(fill = "L1"), alpha = 0.0)
  q <- q + geom_tile(aes(fill = "L2"), alpha = 0.0)
  q <- q + geom_tile(aes(fill = "L3"), alpha = 0.0)
  if(includeHigh) q <- q + geom_tile(aes(fill = "L1"), alpha = 0.6, data=pd[pd$n_status=="High",])
  if(includeMedium) q <- q + geom_tile(aes(fill = "L2"), alpha = 0.6, data=pd[pd$n_status=="Medium",])
  if(includeNormal) q <- q + geom_tile(aes(fill = "L3"), alpha = 0.6, data=pd[pd$n_status=="Normal",])
  q <- q + geom_tile(fill="black", alpha = 0.6, data=pd[pd$n_status=="Black",])
  q <- q + geom_tile(fill="white", alpha = 0.6, data=pd[pd$n_status=="White",])
  q <- q + fhiplot::theme_fhi_basic(legend_position = "bottom")
  breaksDF <- pd[pd$location_name %in% c("1 uke") & pd$n_status %in% c("Black","White") & pd$printWeekYear!="",]
  if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*0.5){
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),2),]
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*1){
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),2),]
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*2){
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),4),]
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*4){
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),8),]
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*10){
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),16),]
  } else {
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),64),]
  }
  breaksDF$printLabel <- breaksDF$printWeekYear
  q <- q + scale_x_date("", breaks = breaksDF$date,  labels = breaksDF$printLabel)
  q <- q + scale_y_discrete("")
  q <- q + scale_fill_manual("",values=GetCols(),labels=c(
    "Betydelig høyere enn forventet",
    "Høyere enn forventet",
    "Forventet"))
  q <- q + coord_cartesian(xlim=limits,expand = FALSE)
  q <- q + labs(title=title)
  q
}
