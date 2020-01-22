
#' get_NorSySS_data
#'
#' Get and clean from file
#'
#'
#' @import data.table
#'
#' @export
data_pre_norsyss <- function(data, argset, schema){
  sykdomspuls_aggregate(
    date_from = argset$date_from,
    date_to = format(Sys.time(), "%Y-%m-%d"),
    folder = path("input", "norsyss"),
    ages = c(
      "0-4" = "0-4",
      "5-14" = "5-9",
      "5-14" = "10-14",
      "15-19" = "15-19",
      "20-29" = "20-29",
      "30-64" = "30-39",
      "30-64" = "40-49",
      "30-64" = "50-59",
      "30-64" = "60-64",
      "65-69" = "65+",
      "70-79" = "65+",
      "80+" = "65+"
    ))
  get_n_doctors(path("input", "norsyss"))
  return(TRUE)
}



takstkoder <- list(
  "11ad" = "Legekontakt",
  "11ak" = "Legekontakt",
  "1ad" = "Telefonkontakt",
  "1ak" = "Telefonkontakt",
  "1bd" = "Telefonkontakt",
  "1bk" = "Telefonkontakt",
  "1g" = "Telefonkontakt",
  "1h" = "Telefonkontakt",
  "2ad" = "Legekontakt",
  "2ae" = "Telefonkontakt",
  "2ak" = "Legekontakt",
  "2fk" = "Legekontakt"
)


# NAV Kommune nummer til FREG
# Some municip numbers received by KUHR do not match the expected
# numbers from folkeregistret. This table translates between them

# Any other municip numbers not in config for sykdomspulsen will be set to 9999
#Bydels number also exist for these codes (see docoumentation)
nav_to_freg = list(
  "312" = 301,
  "313" = 301,
  "314" = 301,
  "315" = 301,
  "316" = 301,
  "318" = 301,
  "319" = 301,
  "321" = 301,
  "326" = 301,
  "327" = 301,
  "328" = 301,
  "330" = 301,
  "331" = 301,
  "334" = 301,
  "335" = 301,
  "1161" = 1103,
  "1162" = 1103,
  "1164" = 1103,
  "1165" = 1103,
  "1202" = 1201,
  "1203" = 1201,
  "1204" = 1201,
  "1205" = 1201,
  "1206" = 1201,
  "1208" = 1201,
  "1209" = 1201,
  "1210" = 1201,
  "1603" = 301,
  "1604" = 1601,
  "1605" = 1601,
  "1607" = 1601
)




sykdomspuls_aggregate_format_raw_data <- function(d, configs) {
  d[, influensa := 0]
  d[Diagnose %in% "R80", influensa := 1]

  d[, gastro := 0]
  d[Diagnose %in% c("D11", "D70", "D73"), gastro := 1]

  d[, respiratory := 0]
  d[Diagnose %in% c("R05", "R74", "R78", "R83"), respiratory := 1]

  d[, respiratoryexternal := 0]
  d[Diagnose %in% c("R05", "R74", "R78", "R83"), respiratoryexternal := 1]

  d[, respiratoryinternal := 0]
  d[Diagnose %in% c("R05", "R74", "R83"), respiratoryinternal := 1]

  d[, lungebetennelse := 0]
  d[Diagnose %in% "R81", lungebetennelse := 1]

  d[, bronkitt := 0]
  d[Diagnose %in% "R78", bronkitt := 1]

  d[, skabb := 0]
  d[Diagnose %in% "S72", skabb := 1]

  ####
  d[, emerg1 := 0]
  d[Diagnose %in% "R80", emerg1 := 1]

  d[, emerg2 := 0]
  d[Diagnose %in% "R80", emerg2 := 1]

  d[, emerg3 := 0]
  d[Diagnose %in% "R80", emerg3 := 1]

  d[, emerg4 := 0]
  d[Diagnose %in% "R80", emerg4 := 1]

  d[, emerg5 := 0]
  d[Diagnose %in% "R80", emerg5 := 1]


  ### Praksis

  d[Praksis == "Fastl\u00F8nnet", Praksis := "Fastlege"]
  d[Praksis == "kommunal legevakt", Praksis := "Legevakt"]


  d[, Kontaktype := "Ukjent"]
  ### Kontaktkode
  for (takstkode in names(takstkoder)) {
    d[ Takst == takstkode, Kontaktype := takstkoder[takstkode]]
  }

  dups <- d[, .(n_diff = length(unique(Kontaktype))), by = .(Id)]
  d <- d[ !(Id %in% dups[n_diff >= 2, Id] & Kontaktype == "Telefonkontakt")]

  d[, age := "Ukjent"]
  d[PasientAlder == "0-4", age := "0-4"]
  d[PasientAlder == "5-9", age := "5-14"]
  d[PasientAlder == "0-9", age := "5-14"]
  d[PasientAlder == "10-14", age := "5-14"]
  d[PasientAlder == "10-19", age := "15-19"]
  d[PasientAlder == "15-19", age := "15-19"]
  d[PasientAlder == "20-29", age := "20-29"]
  d[PasientAlder == "30-39", age := "30-64"]
  d[PasientAlder == "40-49", age := "30-64"]
  d[PasientAlder == "50-59", age := "30-64"]
  d[PasientAlder == "60-64", age := "30-64"]
  d[PasientAlder == "65-69", age := "65+"]
  d[PasientAlder == "60-69", age := "65+"]
  d[PasientAlder == "70-79", age := "65+"]
  d[PasientAlder == "80+", age := "65+"]



  #Fixing behandler kommune nummer
  for(old in names(nav_to_freg)){

    d[as.character(BehandlerKommune) == old, BehandlerKommune:=nav_to_freg[old]]

  }

  

  # Collapsing it down to 1 row per consultation
  d <- d[, .(
    influensa = sum(influensa),
    gastro = sum(gastro),
    respiratory = sum(respiratory),
    respiratoryexternal = sum(respiratoryexternal),
    respiratoryinternal = sum(respiratoryinternal),
    lungebetennelse = sum(lungebetennelse),
    bronkitt = sum(bronkitt),
    skabb = sum(skabb),
    emerg1 = sum(emerg1),
    emerg2 = sum(emerg2),
    emerg3 = sum(emerg3),
    emerg4 = sum(emerg4),
    emerg5 = sum(emerg5)
  ),
  by = .(
    Id,
    BehandlerKommune,
    age,
    Konsultasjonsdato,
    Praksis,
    Kontaktype
  )
  ]

  # Collapsing it down to 1 row per kommune/age/day
  d <- d[, .(
    influensa = sum(influensa),
    gastro = sum(gastro),
    respiratory = sum(respiratory),
    respiratoryexternal = sum(respiratoryexternal),
    respiratoryinternal = sum(respiratoryinternal),
    lungebetennelse = sum(lungebetennelse),
    bronkitt = sum(bronkitt),
    skabb = sum(skabb),
    emerg1 = sum(emerg1),
    emerg2 = sum(emerg2),
    emerg3 = sum(emerg3),
    emerg4 = sum(emerg4),
    emerg5 = sum(emerg5),
    consult = .N
  ),
  by = .(
    BehandlerKommune,
    age,
    Konsultasjonsdato,
    Praksis,
    Kontaktype
  )
  ]

  d[, municip := paste0("municip", formatC(BehandlerKommune, width = 4, flag = 0))]
  d[, BehandlerKommune := NULL]
  setnames(d, "Konsultasjonsdato", "date")

  return(d)
}

#' sykdomspuls_aggregate
#'
#' A function to extract aggregated sykdomspulsen data
#' @param date_from a
#' @param date_to a
#' @param folder a
#' @param ages a
#' @param overwrite_file a
#' @param ... a
#' @import data.table
#' @export
sykdomspuls_aggregate <- function(
                                  date_from = "2018-01-01",
                                  date_to = lubridate::today(),
                                  folder = "/mount/work/projects/",
                                  ages = c(
                                    "0-4" = "0-4",
                                    "5-14" = "5-9",
                                    "5-14" = "10-14",
                                    "15-19" = "15-19",
                                    "20-29" = "20-29",
                                    "30-64" = "30-39",
                                    "30-64" = "40-49",
                                    "30-64" = "50-59",
                                    "30-64" = "60-64",
                                    "65-69" = "65+",
                                    "70-79" = "65+",
                                    "80+" = "65+"
                                  ),
                                  overwrite_file = FALSE,
                                  ...) {
  file_name <- paste0("partially_formatted_", format(Sys.time(), "%Y_%m_%d"), ".txt")
  file_temp <- fs::path(fhi::temp_dir(), file_name)
  file_permanent <- fs::path(folder, file_name)

  if (overwrite_file == FALSE) {
    if (file.exists(file_permanent)) {
      x <- fread(file_permanent)
      max_date <- as.Date(max(x$date, na.rm = T))
      # as long as last date in the file is within 2 days of the requested date
      if (abs(as.numeric(difftime(date_to, max_date, units = "days"))) <= 2) {
        fd::msg("file already exists! exiting...", slack = T)
        return()
      }
    }
  }

  db <- RODBC::odbcDriverConnect("driver={ODBC Driver 17 for SQL Server};server=dm-prod;database=SykdomspulsenAnalyse; trusted_connection=yes")

  # calculate dates
  datesToExtract <- data.table(from = seq(as.Date(date_from), by = "month", length.out = 300), to = seq(as.Date(date_from), by = "month", length.out = 301)[-1] - 1)
  # Remove future dates
  datesToExtract <- datesToExtract[from <= date_to]

  # predefine storage of results
  pb <- fhi::txt_progress_bar(min = 1, max = nrow(datesToExtract))
  for (i in 1:nrow(datesToExtract)) {
    command <- paste0(
      "select Id,Diagnose,PasientAlder,PasientKommune,BehandlerKommune,Konsultasjonsdato,Takst,Praksis from Konsultasjon join KonsultasjonDiagnose on Id=KonsultasjonId join KonsultasjonTakst on Id=KonsultasjonTakst.KonsultasjonId where Konsultasjonsdato >='",
      datesToExtract[i]$from,
      "' AND Konsultasjonsdato <= '",
      datesToExtract[i]$to,
      "'"
    )
    d <- RODBC::sqlQuery(db, command)
    d <- data.table(d)
    d <- sykdomspuls_aggregate_format_raw_data(d)
    if (i == 1) {
      utils::write.table(d, file_temp, sep = "\t", row.names = FALSE, col.names = TRUE, append = FALSE)
    } else {
      utils::write.table(d, file_temp, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  system(glue::glue("mv {file_temp} {file_permanent}"))
}

#' get_n_doctors
#'
#' A function to extract the number of doctors per week

#' @param folder a
#' @import data.table
#' @export
get_n_doctors <- function(folder) {
  db <- RODBC::odbcDriverConnect("driver={ODBC Driver 17 for SQL Server};server=dm-prod;database=SykdomspulsenAnalyse; trusted_connection=yes")
  res <- RODBC::sqlQuery(db, 'select count(distinct(Behandler_Id)) as behandlere, DATEPART("ISO_WEEK", Konsultasjonsdato) as week ,DATEPART("YEAR", Konsultasjonsdato) as year from Konsultasjon group by DATEPART("ISO_WEEK", Konsultasjonsdato) ,DATEPART("YEAR", Konsultasjonsdato)')
  setDT(res)

  file_permanent <- fs::path(folder, "behandlere.txt")

  fwrite(res[order(year, week)], file_permanent)
  close(db)
}
