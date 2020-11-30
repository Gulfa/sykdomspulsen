## app.R ##
library(shiny)
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)


shinyOptions(cache = diskCache("/tmp/", max_size = 50e6))

source("global.R")
source("risk.R")
#source("barometer.R")
#source("signals.R")
#source("number_weekly.R")
#source("number_daily.R")
#source("meminfluensa.R")
ui <- navbarPage("Sykdomspulsen En-Helse",
  tabPanel("Nyheter", icon = icon("newspaper"),
           fluidRow(
             column(2),
             column(8,
                    wellPanel(
                      strong("2019-09-20 / Ny nettside"),
                      p("Ny En-Helse portal")
                    )
                    )
           )
  ),
  tabPanel("Risiko", icon = icon("th"),
     riskUI("risk", "Counter", GLOBAL=GLOBAL)
  )
  ## tabPanel("Signaler", icon = icon("exclamation-triangle"),
  ##   signalsUI("signals", "Counter", GLOBAL=GLOBAL)
  ## ),
  ## tabPanel("Antall (ukentlig)", icon = icon("chart-line"),
  ##   number_weeklyUI("number_weekly", "Counter", GLOBAL=GLOBAL)
  ## ),
  ## tabPanel("Antall (daglig)", icon = icon("chart-line"),
  ##   number_dailyUI("number_daily", "Counter", GLOBAL=GLOBAL)
  ## ),
  ## tabPanel("MeM-Influensa",icon = icon("snowflake"),
  ##   meminfluensaUI("meminfluensa", "Counter #1", GLOBAL=GLOBAL)
  ## )
  )



server <- function(input, output) {
  callModule(riskServer, "risk", GLOBAL=GLOBAL)
#  callModule(signalsServer, "signals", GLOBAL=GLOBAL)
#  callModule(number_weeklyServer, "number_weekly", GLOBAL=GLOBAL)
#  callModule(number_dailyServer, "number_daily", GLOBAL=GLOBAL)
#  callModule(meminfluensaServer, "meminfluensa", GLOBAL=GLOBAL)
}

shinyApp(ui, server)

#  shiny::runApp('inst/shiny/beta', port = 4989, host = "0.0.0.0")
