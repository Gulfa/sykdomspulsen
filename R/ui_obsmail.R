#' ui_obsmail
#'
#' @export
ui_obsmail<- function(data, argset, schema) {
  max_date <- argset$today
  yrwks <- fhi::isoyearweek(max_date - c(0, 7, 14, 21))
  tag_relevant <- argset$tags

  results <- schema$input$dplyr_tbl() %>%
    dplyr::filter(granularity_time == "weekly") %>%
    dplyr::filter(yrwk %in% !!yrwks) %>%
    dplyr::filter(tag_outcome %in% tag_relevant) %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  print(results)
  results[, tag :=tag_outcome]
  results[, to_keep := FALSE]
  results[n_status != "Normal", to_keep := TRUE]
  results[, to_keep := as.logical(max(to_keep)), by = .(tag, location_code, age)]
  results <- results[to_keep == TRUE]
  results[, to_keep := NULL]

  setorder(results, tag, location_code, age, yrwk)
  print(results)
  results[, week_id := 1:.N, by = .(tag, location_code, age)]

  alerts <- sykdomspuls_obs_get_emails()
  setDT(alerts)
  emails <- unique(alerts$email)

  email_subject <- glue::glue("OBS varsel fra Sykdomspulsen {yrwks[1]}")

  if (!fd::config$is_production) {
    if (!"test@rwhite.no" %in% unique(alerts$email)) stop("THIS IS NOT A TEST EMAIL DATASET")
  }

  email_text_top <- glue::glue(
    "<br><b>OBS varsel fra Sykdomspulsen oppdatert 11/10/2019.</b> ",
    "Mer informasjon om Sykdomspulsen og OBS varselet finner du under tabellene.<br><br><br>"
  )

  email_text_bottom <- glue::glue(
    "<b>Dette er et OBS varsel fra Sykdomspulsen.</b><br><br>",

    "OBS varselet inneb\u00E6rer at alle dere som deltar i pilotprosjektet ",
    "<b>Sykdomspulsen til kommunehelsetjenesten</b> f\u00E5r et varsel p\u00E5",
    "e-post dersom deres kommune eller et fylke har flere konsultasjoner enn ",
    "forventet av henholdsvis mage-tarminfeksjoner eller luftveisinfeksjoner en av de siste 4 ukene.<br><br>",

    "Tabellen under viser informasjon om stedet der det er mer enn forventet ",
    "antall meldte tilfeller og aldersgruppe, antallet tilfeller flere enn normalt ",
    "og en verdi som viser hvor ekstremt signalet er (z-score). ",
    "Hvis z-scoret er mellom 2 og 4 er antallet konsultasjoner h{fhi::nb$oe}yere enn ",
    "forventet og man vil se at det ligger i gul sone p\u00E5 Sykdomspulsen websiden. ",
    "Dersom z-scoret er over 4 er antallet konsultasjoner betydelig h\u00F8yere ",
    "enn forventet og man vil se at det ligger i r\u00F8d sone p\u00E5 Sykdomspulsen websiden.<br><br>",

    "I tabellen over er det en link til stedet der du kan se OBS varselet i Sykdomspulsen. ",
    "Denne virker ikke dersom den \u00E5pnes i Internet explorer. Dersom du har problemer ",
    "med linken kan du h\u00F8yreklikke p\u00E5 koblingen og kopiere den for deretter ",
    "\u00E5 lime den inn i for eksempel Google chrome eller en annen nettleser. ",
    "Du kan ogs\u00E5 logge deg inn p\u00E5 Sykdomspulsen p\u00E5 vanlig m\u00E5te ",
    "(<a href='http://sykdomspulsen.fhi.no/lege123/'>http://sykdomspulsen.fhi.no/lege123/</a>) ",
    "og selv finne aktuell kommune eller fylke.<br><br>",

    "Varselet er en informasjon om at det kan v\u00E6re noe som b\u00F8r f\u00F8lges ",
    "opp i din kommune eller i et fylke. Det anbefales \u00E5 g\u00E5 inn i Sykdomspulsen ",
    "websiden og sjekke det ut. Varselet beh\u00F8ver ikke \u00E5 bety noe alvorlig.<br><br>",

    "Nederst i denne mailen viser vi hvilke(n) kommune(r) du f\u00E5r varsel for. ",
    "Alle f\u00E5r varsel for alle fylker og hele Norge. Dersom det ikke st\u00E5r noen ",
    "kommune i tabellen mangler vi det for deg og vi ber deg kontakte oss for \u00E5 ",
    "f\u00E5 satt opp riktig kommune(r).<br><br>",

    "Sykdomspulsen kan i noen tilfeller generere et OBS varsel selv om det bare er en ",
    "eller to konsultasjoner for et symptom/sykdom. Dette sees som oftest i sm\u00E5 ",
    "kommuner der det vanligvis ikke er mange konsultasjoner. For ikke \u00E5 bli forstyrret ",
    "av slike signaler har vi n\u00E5 lagt inn en nedre grense for gult signal p\u00E5 to ",
    "konsultasjoner og en nedre grense for r\u00F8dt signal p\u00E5 tre konsultasjoner.<br><br>",

    "Ta kontakt med oss om du har sp\u00F8rsm\u00E5l eller om det er noe som er uklart ",
    "p\u00E5 sykdomspulsen@fhi.no.<br><br>",

    "Send oss ogs\u00E5 en tilbakemelding dersom du \u00F8nsker varsel for andre kommuner ",
    "eller fylker.<br><br>",

    "Vi \u00F8nsker ogs\u00E5 tilbakemelding p\u00E5 om dette varselet er nyttig for ",
    "dere eller ikke.<br><br>",

    "<b> NB! Oppdatering av Sykdomspulsen vil n\u00E5 skje p\u00E5 onsdager istedenfor ",
    "tirsdager. H\u00E5per dette ikke vil for\u00E5rsake noen ulemper for dere.</b> <br><br>",

    "Hilsen:<br><br>",
    "Sykdomspulsen ved Folkehelseinstituttet<br>",
    "v/Gry M Gr\u00F8neng (prosjektleder) og Richard White (statistiker og webansvarlig)<br><br>"
  )

  alerts[, output := sprintf("<tr> <td>%s</td> </tr>", location)]

  setorder(results, -zscore)
  results[, tag_pretty := tag]
  RAWmisc::RecodeDT(results, switch = sykdomspuls::CONFIG$tagsWithLong, var = "tag_pretty", oldOnLeft = FALSE)
  results[, link := sprintf("<a href='http://sykdomspulsen.fhi.no/lege123/#/ukentlig/%s/%s/%s/%s'>%s</a>", county_code, location_code, tag, age, location_name)]
  results[is.na(county_code), link := sprintf("<a href='http://sykdomspulsen.fhi.no/lege123/#/ukentlig/%s/%s/%s/%s'>%s</a>", location_code, location_code, tag, age, location_name)]

  for (em in emails) {
    a <- alerts[email %in% em]


    r <- vector("list", length = nrow(a))
    for (i in 1:nrow(a)) {
      # first get all of the data
      temp <- results[stringr::str_detect(location_code, a$location[i])]
      temp[, to_keep := FALSE]
      temp[status %in% a$statuses[[i]], to_keep := TRUE]
      temp[, to_keep := as.logical(max(to_keep)), by = .(tag, location_code, age)]
      temp <- temp[to_keep == TRUE]
      r[[i]] <- temp
    }
    r <- rbindlist(r)

    email_text <- email_text_top

    email_text <- paste0(email_text, "<hr width='60%' size='5px' noshade><br>")

    # include outbreaks
    for (tag in sykdomspuls::CONFIG$SYNDROMES[alertExternal == T]$tag) {
      email_text <- paste0(email_text, EmailExternalGenerateTable(results = r, xtag = tag))
    }

    email_text <- paste0(email_text, "<hr width='60%' size='5px' noshade><br>")

    # include registered places
    tab <- huxtable::huxtable("Geografisk omr\u00E5de" = a$location) %>%
      huxtable::add_colnames() %>%
      fhiplot::huxtable_theme_fhi_basic()
    huxtable::escape_contents(tab)[1, 1] <- FALSE
    tab <- huxtable::to_html(tab)

    email_text <- paste0(email_text, "<br>Du er registrert for \u00E5 motta varsel om utbrudd i:<br>", tab, "<br>")

    email_text <- paste0(email_text, "<hr width='60%' size='5px' noshade><br><br>")

    # add in bottom text
    email_text <- paste0(email_text, email_text_bottom)

    fd::mailgun(
      subject = email_subject,
      html = email_text,
      to = em,
      is_final = actions[["sykdomspuls_obs"]]$is_final()
    )

    Sys.sleep(1)
  }

  return(0)
}


EmailExternalGenerateTable <- function(results, xtag) {
  r_long <- results[tag == xtag]
  setorder(r_long, tag, -zscore)

  if (nrow(r_long) == 0) {
    return(sprintf("<br><b>%s:</b> <span style='color:red;text-decoration:underline;'>Ingen utbrudd registrert</span><br><br>", sykdomspuls::CONFIG$SYNDROMES[tag == xtag]$namesLong))
  }

  r_long[, excessp := ceiling(pmax(0, n - threshold2))]
  r_long[, zscorep := fhiplot::format_nor(zscore, 1)]

  r_wide <- dcast.data.table(
    r_long,
    tag_pretty + link + age ~ week_id,
    value.var = c("n", "excessp", "threshold2", "zscore", "zscorep", "status")
  )
  setorder(r_wide, -zscore_4)

  yrwks <- unique(r_long[, c("week_id", "yrwk")])
  setorder(yrwks, week_id)

  tab <- huxtable::huxtable(
    Syndrom = r_wide$tag_pretty,
    "Geografisk omr\u00E5de" = r_wide$link,
    Alder = r_wide$age,
    `n_1` = r_wide$n_1,
    `n_2` = r_wide$n_2,
    `n_3` = r_wide$n_3,
    `n_4` = r_wide$n_4,
    `excess_1` = r_wide$excessp_1,
    `excess_2` = r_wide$excessp_2,
    `excess_3` = r_wide$excessp_3,
    `excess_4` = r_wide$excessp_4,
    `zscore_1` = r_wide$zscorep_1,
    `zscore_2` = r_wide$zscorep_2,
    `zscore_3` = r_wide$zscorep_3,
    `zscore_4` = r_wide$zscorep_4
  ) %>%
    huxtable::add_colnames() %>%
    fhiplot::huxtable_theme_fhi_basic()

  # coloring in
  for (i in 1:4) {
    z <- glue::glue("status_{i}")
    column_to_color <- c(3, 7, 11) + i
    index_low <- which(r_wide[[z]] == "Normal") + 1
    index_med <- which(r_wide[[z]] == "Medium") + 1
    index_hig <- which(r_wide[[z]] == "High") + 1

    if (length(index_low) > 0) huxtable::background_color(tab)[index_low, column_to_color] <- fhiplot::warning_color[["low"]]
    if (length(index_med) > 0) huxtable::background_color(tab)[index_med, column_to_color] <- fhiplot::warning_color[["med"]]
    if (length(index_hig) > 0) huxtable::background_color(tab)[index_hig, column_to_color] <- fhiplot::warning_color[["hig"]]
  }

  tab[1, ] <- c(
    "Syndrom",
    "Geografisk omr\u00E5de",
    "Alder",
    yrwks$yrwk,
    yrwks$yrwk,
    yrwks$yrwk
  )

  tab <- huxtable::add_rows(tab, tab[1, ], after = 0)

  huxtable::escape_contents(tab)[, 2] <- FALSE

  tab[1, 1:3] <- " "

  tab <- huxtable::merge_cells(tab, 1, 4:7)
  tab[1, 4] <- "Meldte tilfeller"

  tab <- huxtable::merge_cells(tab, 1, 8:11)
  tab[1, 8] <- "Flere enn normalt<sup>1</sup>"

  tab <- huxtable::merge_cells(tab, 1, 12:15)
  tab[1, 12] <- "Z-verdi<sup>3</sup>"

  huxtable::left_border(tab)[, c(4, 8, 12)] <- 5
  huxtable::left_border_style(tab)[, c(4, 8, 12)] <- "double"

  huxtable::align(tab) <- "center"

  nr0 <- nrow(tab) + 1
  tab <- huxtable::add_footnote(tab, glue::glue(
    "<sup>1</sup>Differansen mellom antall registrete og {fhi::nb$oe}vre grense for normalt antall<sup>2</sup><br>",
    "<sup>2</sup>95% prediksjonsintervall<br>",
    "<sup>3</sup>Z-verdi: antall ganger standardaviket verdien er fra forventet antall konsultasjoner<br>",
    "<sup>3</sup>Z-verdi mellom 2 og 4 og flere enn 2,5 meldte tilfeller indikerer at det er et h{fhi::nb$oe}yere antall meldte tilfeller enn normalt (vist som gul)<br>",
    "<sup>3</sup>Z-verdi >= 4 og flere enn 3 meldte tilfeller indikerer at det er et betydlig h{fhi::nb$oe}yere antall meldte tilfeller enn normalt (vist som r{fhi::nb$oe}d)<br>",
  ), border = 0)
  nr1 <- nrow(tab)

  huxtable::escape_contents(tab)[1, c(8, 12)] <- F
  huxtable::escape_contents(tab)[nr0:nr1, ] <- F

  return(huxtable::to_html(tab))
}


AlertsEmailConverter <- function(emails) {
  setDT(emails)
  emails[, statuses := vector("list", length = .N)]
  emails[, statuses := rep(list(c("High", "Medium")), .N)]
  emails[level == "high", statuses := rep(list(c("High")), .N)]

  return(emails)
}


sykdomspuls_obs_get_emails <- function() {
  if (config$is_production & actions[["sykdomspuls_obs"]]$is_final()) {
    retval <- readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert.xlsx"))
  } else {
    retval <- readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert_test.xlsx"))
  }

  retval <- AlertsEmailConverter(retval)

  return(retval)
}
