signalsUI <- function(id, label = "Counter", GLOBAL) {
  ns <- NS(id)
  fluidRow(
    column(
      width=2,
      selectInput(ns("weeklyOutbreakWeek"), "Uker", as.list(GLOBAL$outbreaksyrwk), selected = GLOBAL$outbreaksyrwk[1]),
      selectInput(ns("weeklyOutbreakSort"), "Rangere etter", list("Z verdi"="zscore","Eksess tilfeller"="cases","Navn"="none"), selected = "zscore"),
      checkboxInput(ns("weeklyOutbreakHideEmpty"), "Skjul tomme", TRUE)
    ),
    column(
      width=10,
      tabsetPanel(
        tabPanel(
          title="Fylker",
          tableOutput(ns("table1"))
        ),
        tabPanel(
          title="Kommuner",
          tableOutput(ns("table2"))
        ),
        tabPanel(
          title="Info",
          br(),
          p("Tabellen viser en oversikt over forekomsten av sykdom/symptom i et valgt tidsrom."),
          p("Valg av tidsrom gjøres på bunnefiguren. Valg av sykdom/symptom gjøres på venstre side. På venstre side kan man også velge Norge eller et fylke i Norge. Hvis man velger Norge vil hvert fylke få en rad i tabellen. Hvis man velger et fylke vil alle kommunene få en rad i tabellen."),
          p("Dersom ruten for en gitt uke er farget med grønn farge betyr det at antall konsultasjoner i den gitte kommunen eller fylket er som forventet denne uken. En gul farge en gitt uke betyr at antall konsultasjoner i den gitte kommunen eller fylket er høyere enn forventet denne uken. En rød farge en gitt uke betyr at antall konsultasjoner i den gitte kommunen eller fylket er betydelig høyere enn forventet denne uken. Fargene er laget ut fra beregninger fra de foregående 5 årene."),
          p("Se punktet *Om Sykdomspulsen* på venstre side for mer utfyllende informasjon om dataene og beregninger.")
        )
      )
    )
  )
}

signalsServer <- function(input, output, session, GLOBAL) {
  output$table1 <- renderTable({
    x_wkyr <- input$weeklyOutbreakWeek

    data_county <- pool %>% tbl("spuls_standard_results") %>%
      filter(tag != "influensa" &
               granularity_time == "weekly" &
               granularity_geo == "county" &
               yrwk==x_wkyr) %>% collect()
    data_municipality <- pool %>% tbl("spuls_standard_results") %>%
      filter(tag != "influensa" &
               granularity_time == "weekly" &
               granularity_geo == "municip" &
               yrwk==x_wkyr) %>% collect()
    setDT(data_county)
    setDT(data_municipality)
    data <- GenerateOutbreakListInternal(df=data_county,
                                         dk=data_municipality, saveFiles=NULL)
    data <- data[["df"]]

    if(input$weeklyOutbreakHideEmpty){
      data <- data[data$High!="",]
    }
    if(input$weeklyOutbreakSort=="zscore"){
      setorder(data,-meanZScore)
    } else if(input$weeklyOutbreakSort=="cases"){
      setorder(data,-sumCum)
    }
    if(nrow(data)==0) return(data.frame("Obs"="Ingen utbrudd denne uken"))
    data$yrwk <- NULL
    data$sumCum[is.na(data$sumCum)] <- 0
    data$sumCum <- formatC(data$sumCum,digits=0,format="f")
    data$sumCum[data$sumCum=="0"] <- ""
    setnames(data,c("Sykdom","Alder","Fylke (Z verdi)","Gj. Z Verdi","Eksess tilfeller"))
    data$Sykdom <- factor(data$Sykdom,levels=sykdomspuls::CONFIG$STANDARD$tag)
    levels(data$Sykdom) <- sykdomspuls::CONFIG$STANDARD$namesLong
    data
  },
  striped=TRUE,
  spacing="m",
  align="c",
  width='100%')

  output$table2 <- renderTable({
    x_wkyr <- input$weeklyOutbreakWeek

    data_county <- pool %>% tbl("spuls_standard_results") %>%
      filter(tag != "influensa" &
               granularity_time == "weekly" &
               granularity_geo == "county" &
               yrwk==x_wkyr) %>% collect()
    data_municipality <- pool %>% tbl("spuls_standard_results") %>%
      filter(tag != "influensa" &
               granularity_time == "weekly" &
               granularity_geo == "municip" &
               yrwk==x_wkyr) %>% collect()
    setDT(data_county)
    setDT(data_municipality)
    data <- GenerateOutbreakListInternal(df=data_county,
                                         dk=data_municipality, saveFiles=NULL)
    data <- data[["dk"]]
    if(input$weeklyOutbreakHideEmpty){
      data <- data[data$High!="",]
    }
    if(input$weeklyOutbreakSort=="zscore"){
      setorder(data,-meanZScore)
    } else if(input$weeklyOutbreakSort=="cases"){
      setorder(data,-sumCum)
    }

    if(nrow(data)==0) return(data.frame("Obs"="Ingen utbrudd denne uken"))
    data$yrwk <- NULL
    data$sumCum[is.na(data$sumCum)] <- 0
    data$sumCum <- formatC(data$sumCum,digits=0,format="f")
    data$sumCum[data$sumCum=="0"] <- ""
    setnames(data,c("Sykdom","Alder","Fylke","Kommune (Z verdi)","Gj. Z Verdi","Eksess tilfeller"))
    data$Sykdom <- factor(data$Sykdom,levels=sykdomspuls::CONFIG$STANDARD$tag)
    levels(data$Sykdom) <- sykdomspuls::CONFIG$STANDARD$namesLong
    data
  },
  striped=TRUE,
  spacing="m",
  align="c",
  width='100%')
}

GenerateOutbreakListInternal <- function(df = readRDS(fd::path("results", sprintf("%s/resYearLine.RDS", LatestRawID()))),
                                         dk = readRDS(fd::path("results", sprintf("%s/resYearLineMunicip.RDS", LatestRawID()))),
                                         saveFiles = c(
                                           fd::path("results", sprintf("%s/outbreaks.RDS", LatestRawID()))
                                         ),
                                         useType = FALSE) {
  # variables used in data.table functions in this function
  . <- NULL
  status <- NULL
  wkyr <- NULL
  age <- NULL
  county <- NULL
  location <- NULL
  locationName <- NULL
  zscore <- NULL
  tag <- NULL
  cumE1 <- NULL
  meanZScore <- NULL
  sumCum <- NULL
  sumCumNorge <- NULL
  countyName <- NULL
  # end

  counties <- unique(df[, c("location_code", "location_name"), with = F])
  setnames(counties, c("county_code", "county_name"))

  df <- df[, c("yrwk", "age", "tag", "location_name", "status", "zscore", "cumE1"), with = F]
  dk <- dk[, c("yrwk", "age", "tag", "location_code", "location_name", "status", "county_code", "zscore", "cumE1"), with = F]
  dk <- merge(dk, counties, by = "county_code")

  setorder(df, status, -yrwk, -age)
  setorder(dk, status, -yrwk, -age, county_code, location_code)

  df[, location_name := sprintf("%s (%s)", location_name, formatC(zscore, digits = 2, format = "f"))]
  dk[, location_name := sprintf("%s (%s)", location_name, formatC(zscore, digits = 2, format = "f"))]

  df[status != "High", location_name := ""]
  dk[status != "High", location_name := ""]

  df[, status := NULL]
  dk[, status := NULL]

  dk[, location_code := NULL]


  df1 <- df[, lapply(
    .SD, paste0,
    collapse = ", "
  ), by = .(
    yrwk,
    age,
    tag
  )]
  df1[, zscore := NULL]
  df1[, cumE1 := NULL]

  df2 <- df[location_name != "", .(
    meanZScore = mean(zscore),
    sumCum = sum(cumE1)
  ), by = .(
    yrwk,
    age,
    tag
  )]
  df3 <- df[stringr::str_detect(location_name, "Norge"), .(
    sumCumNorge = sum(cumE1)
  ), by = .(
    yrwk,
    age,
    tag
  )]
  df <- merge(df1, df2, by = c("yrwk", "age", "tag"), all.x = T)
  df <- merge(df, df3, by = c("yrwk", "age", "tag"), all.x = T)
  df[is.na(meanZScore), meanZScore := 0]
  df[is.na(sumCum), sumCum := 0]
  df[is.na(sumCumNorge), sumCumNorge := 0]
  df[stringr::str_detect(location_name, "Norge"), sumCum := sumCumNorge]
  df[, sumCumNorge := NULL]
  df[, meanZScore := formatC(meanZScore, digits = 2, format = "f")]
  df[meanZScore == "0.00", meanZScore := ""]
  df[, sumCum := round(sumCum)]
  df[sumCum == 0, sumCum := NA]

  dk1 <- dk[, lapply(
    .SD, paste0,
    collapse = ", "
  ), by = .(
    yrwk,
    age,
    tag,
    county_code,
    county_name
  )]
  dk1[, zscore := NULL]
  dk1[, cumE1 := NULL]

  dk2 <- dk[location_name != "", .(
    meanZScore = mean(zscore),
    sumCum = sum(cumE1)
  ), by = .(
    yrwk,
    age,
    tag,
    county_code
  )]
  dk <- merge(dk1, dk2, by = c("yrwk", "age", "tag", "county_code"), all.x = T)
  dk[is.na(meanZScore), meanZScore := 0]
  dk[is.na(sumCum), sumCum := 0]
  dk[, meanZScore := formatC(meanZScore, digits = 2, format = "f")]
  dk[meanZScore == "0.00", meanZScore := ""]
  dk[, sumCum := round(sumCum)]
  dk[sumCum == 0, sumCum := NA]

  df[, location_name := gsub(", , ", "", location_name)]
  df[, location_name := gsub(", $", "", location_name)]
  df[, location_name := gsub("^, ", "", location_name)]
  setorder(df, tag, -yrwk, -age)
  setnames(df, "location_name", "High")

  df[, age := factor(age, levels = c("Totalt", "0-4", "5-14", "15-19", "20-29", "30-64", "65+"))]
  setorder(df, tag, -yrwk, age)
  setcolorder(df, c("tag", "yrwk", "age", "High", "meanZScore", "sumCum"))

  dk[, location_name := gsub(", , ", "", location_name)]
  dk[, location_name := gsub(", $", "", location_name)]
  dk[, location_name := gsub("^, ", "", location_name)]
  dk[, age := factor(age, levels = c("Totalt", "0-4", "5-14", "15-19", "20-29", "30-64", "65+"))]
  setorder(dk, tag, -yrwk, age, county_code)
  dk[, county_code := NULL]
  setcolorder(dk, c("tag", "yrwk", "age", "county_name", "location_name", "meanZScore", "sumCum"))
  setnames(dk, "location_name", "High")

  if (useType) {
    setnames(df, "tag", "type")
    setnames(dk, "tag", "type")
  }

  outbreaks <- list(df = df, dk = dk)
  if (!is.null(saveFiles)) {
    SaveRDS(outbreaks, saveFiles)
  }

  return(outbreaks)
}

