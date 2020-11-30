riskUI <- function(id, label = "Counter", GLOBAL) {
  ns <- NS(id)
  fluidRow(
    column(
      width=2,
    #  selectInput(ns("weeklyBarometerType"), "Sykdom/Symptom", as.list(GLOBAL$weeklyTypes), selected = GLOBAL$weeklyTypes[1]),
     selectInput(ns("riskAge"), "Alder", as.list(GLOBAL$weeklyAges), selected = "Totalt")
     # selectInput(ns("weeklyBarometerCounty"), "Fylke", as.list(GLOBAL$weeklyCounties), selected = GLOBAL$weeklyCounties[1])
    ),
    column(
      width=10,
      tabsetPanel(
        tabPanel(title="Figur",
                 column(width=5,
                        h3("Risko for utbrudd i Tronderlag"),
                        div(style='height:60vh;text-align: center',plotOutput(ns("risk_chart"), height="100%"))
                        ),
                 column(width=5,
                        h3("Kommuner med høyest risko for utbrudd"),
                        div(style='height:60vh;text-align: center',tableOutput(ns("risk_table"))))
                 
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

riskServer <- function(input, output, session, GLOBAL) {
  start_date <- Sys.Date()

 
  risk_data <- reactive({
    req(input$riskAge)
    x_age <- input$riskAge

    data <- pool %>% tbl("results_pred_oh") %>%
      filter(
#        date >= start_date &
 #         tag_outcome == "gastro" &
  #        source == "data_norsyss"
#        location_code== x_location &
 #       granularity_time =="weekly" &
        age== x_age
      ) %>% collect()
    setDT(data)
    return(data)
  })


  output$risk_table <- renderTable({

    d <- risk_data()
    d[, location_name:=sykdomspulsen::get_location_name(location_code)]
    a <- d[order(p_thresholdu0, decreasing=TRUE)][,.(Kommune=location_name,
                                                          "Predikert antall"=n_est, "Risiko for utburdd(%)"=p_thresholdu0*100)]

    return(head(a))

  })


   # })
  output$risk_chart <- renderPlot({

    base_map <- fhidata::norway_map_municips_b2020
    d <- risk_data()
    map <- d[base_map, on="location_code"]
    map[, cat:="No data"]
    map[p_thresholdu0 < 0.05, cat:="Lav"]
    map[p_thresholdu0 > 0.05 & p_thresholdu0 < 0.15, cat:="Middels"]
    map[p_thresholdu0 > 0.15 & p_thresholdu0 < 0.3, cat:="Hoy"]
    map[p_thresholdu0 > 0.3 , cat:="Veldig Høy"]
    map$cat <- factor(map$cat, levels=c("Veldig høy", "Hoy", "Middels", "Lav", "No data"))
    ggplot(map) + geom_polygon(aes(x=long, y=lat, group=group, fill=cat), color="black") + coord_quickmap() + xlim(8, 15) + ylim(62, 65) + theme_void() + scale_fill_brewer("Risiko for mage-tarm utbrudd", palette= "OrRd", direction = -1)
    
  })
 }

