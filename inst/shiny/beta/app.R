## app.R ##
library(shiny)
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)


shinyOptions(cache = diskCache("/tmp/", max_size = 50e6))

source("global.R")
source("barometer.R")
source("signals.R")
source("number_weekly.R")
source("number_daily.R")
source("meminfluensa.R")

ui <- navbarPage("Sykdomspulsen",
  tabPanel("Nyheter", icon = icon("newspaper"),
    fluidRow(
      column(2),
      column(8,
        wellPanel(
          strong("2019-09-20 / Ny nettside"),
          p("Vi har byttet ut den gamle nettsiden med en ny for å gi dere en raskere opplevelse og for å være mer i tråd med kommunikasjonsavdelingens anbefalinger om grafer. Håper dere liker det!")
        ),
        wellPanel(
          strong("2019-09-11 / MeM terskler til influensa tilsatt"),
          p("Nå kan du se på MeM terskler til influensa under fanen 'MeM-Influensa'.")
        ),
        wellPanel(
          strong("2016-08-01 / Økning i aldersgruppen 15-19 år"),
          p("Fra august 2016 er det en økning i antall konsultasjoner i aldersgruppen 15-19 år grunnet behov for sykemelding ved fravær i den videregående skole.")
        ),
        wellPanel(
          strong("2015-01-01 / Lansering av Sykdomspulsen"),
          p("Velkommen til Sykdomspulsen!")
        )
      )
    )
  ),
  tabPanel("Oversikt (ukentlig)", icon = icon("th"),
    barometerUI("barometer", "Counter", GLOBAL=GLOBAL)
  ),
  tabPanel("Signaler", icon = icon("exclamation-triangle"),
    signalsUI("signals", "Counter", GLOBAL=GLOBAL)
  ),
  tabPanel("Antall (ukentlig)", icon = icon("chart-line"),
    number_weeklyUI("number_weekly", "Counter", GLOBAL=GLOBAL)
  ),
  tabPanel("Antall (daglig)", icon = icon("chart-line"),
    number_dailyUI("number_daily", "Counter", GLOBAL=GLOBAL)
  ),
  tabPanel("MeM-Influensa",icon = icon("snowflake"),
    meminfluensaUI("meminfluensa", "Counter #1", GLOBAL=GLOBAL)
  )
)

server <- function(input, output) {
  callModule(barometerServer, "barometer", GLOBAL=GLOBAL)
  callModule(signalsServer, "signals", GLOBAL=GLOBAL)
  callModule(number_weeklyServer, "number_weekly", GLOBAL=GLOBAL)
  callModule(number_dailyServer, "number_daily", GLOBAL=GLOBAL)
  callModule(meminfluensaServer, "meminfluensa", GLOBAL=GLOBAL)
}

shinyApp(ui, server)

#  shiny::runApp('inst/shiny/beta', port = 4989, host = "0.0.0.0")
