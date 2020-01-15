#' ui_mem_plots
#'
#' @export
ui_mem_plots <- function(data, argset, schema){
  outputs <- list(
    charts = create_plots,
    norway_sheet = create_norway_sheet,
    county_sheet = create_county_sheet,
    region_sheet = create_region_sheet,
    n_doctors_sheet = create_n_doctors_sheet
  )
  folder <- results_folder(glue::glue("{argset$folder_name}"), argset$today)
  argset[["folder"]] <- folder
  fs::dir_create(folder)
  for (output in argset$outputs) {
    outputs[[output]](argset, argset$today)
  }
  create_latest_folder(glue::glue("{argset$folder_name}"),argset$today )

}

#' create norway sheet
#'
#' @param conf A mem model configuration object
#' @param date extract date
#'
create_norway_sheet <- function(conf, date) {
  current_season <- tbl("spuls_mem_results") %>%
    dplyr::summarize(season = max(season, na.rm = T)) %>%
    dplyr::collect()
  current_season <- current_season$season

  x_tag <- conf$tag
  data <- tbl("spuls_mem_results") %>%
    dplyr::filter(season == current_season & tag == x_tag) %>%
    dplyr::collect()
  setDT(data)
  ili_out <- tbl("spuls_mem_results") %>%
    dplyr::filter(location_code == "norge", tag == x_tag) %>%
    dplyr::select(year,
      year_week = yrwk, week, season, percent_ili = rate,
      season_week = x,
      ili_consultations = n, total_consultations = denominator, status
    ) %>%
    dplyr::collect()
  readr::write_csv(ili_out, glue::glue("{conf$folder}/ili_data.csv"))
}

#' create county sheet
#'
#' @param conf A mem model configuration object
#' @param date extract date
#'
create_county_sheet <- function(conf, date) {
  current_season <- tbl("spuls_mem_results") %>%
    dplyr::summarize(season = max(season, na.rm = T)) %>%
    dplyr::collect()
  current_season <- current_season$season

  x_tag <- conf$tag
  data <- tbl("spuls_mem_results") %>%
    dplyr::filter(season == current_season & tag == x_tag) %>%
    dplyr::collect()
  setDT(data)

  # Norway overview sheet

  out_data <- data %>%
    dplyr::mutate(
      rate = round(rate, 2),
      loc_name = get_location_name(location_code)
    ) %>%
    dplyr::select(yrwk, week, loc_name, rate, n, denominator)
  setDT(out_data)

  overview <- dcast(out_data, yrwk + week ~ loc_name, value.var = c("rate", "n", "denominator"))
  col_names <- names(overview)
  col_names <- gsub("rate_([A-\u00D8a-\u00F80-9-]*)$", "\\1 % ILI", col_names)
  col_names <- gsub("n_([A-\u00D8a-\u00F80-9-]*)$", "\\1 ILI konsultasjoner", col_names)
  col_names <- gsub("denominator_([A-\u00D8a-\u00F80-9-]*)$", "\\1 Totalt konsultasjoner", col_names)
  col_names <- gsub("yrwk$", "\u00C5r-Uke", col_names)
  col_names <- gsub("week$", "Uke", col_names)
  names(overview) <- col_names
  setcolorder(overview, col_names[order(col_names)])
  wb <- xlsx::createWorkbook(type = "xlsx")
  sheet_rate <- xlsx::createSheet(wb, sheetName = "Andel ILI")
  sheet_consult <- xlsx::createSheet(wb, sheetName = "Konsultasjoner")
  sheet_info <- xlsx::createSheet(wb, sheetName = "Info")
  rate_df <- overview %>% dplyr::select("\u00C5r-Uke", "Uke", dplyr::ends_with("% ILI"))
  consult_df <- overview %>% dplyr::select("\u00C5r-Uke", "Uke", dplyr::ends_with("konsultasjoner"))

  xlsx::addDataFrame(rate_df,
    sheet_rate,
    row.names = FALSE
  )
  # xlsx::autoSizeColumn(sheet_rate, colIndex = 1:ncol(rate_df))
  xlsx::addDataFrame(consult_df,
    sheet_consult,
    row.names = FALSE
  )
  # xlsx::autoSizeColumn(sheet_consult, colIndex = 1:ncol(consult_df))
  info <- data.frame(
    Syndrom = conf$tag,
    ICPC2 = paste(conf$icpc2, sep = ","),
    Konktattype = paste(conf$contactType, sep = ","),
    Oppdatert = date
  )
  xlsx::addDataFrame(info,
    sheet_info,
    row.names = FALSE
  )
  # xlsx::autoSizeColumn(sheet_info, colIndex = 1:ncol(info))
  xlsx::saveWorkbook(wb, glue::glue("{conf$folder}/fylke.xlsx"))
}

#' create MEM region sheet
#'
#' @param conf A mem model configuration object
#' @param date extract date
#'
create_region_sheet <- function(conf, date) {
  current_season <- tbl("spuls_mem_results") %>%
    dplyr::summarize(season = max(season, na.rm = T)) %>%
    dplyr::collect()
  current_season <- current_season$season

  x_tag <- conf$tag
  data <- tbl("spuls_mem_results") %>%
    dplyr::filter(season == current_season & tag == x_tag) %>%
    dplyr::collect()
  setDT(data)

  norway_locations <- norway_locations()[, .(region_name = min(region_name)), by = .(county_code)]
  out_data <- data[norway_locations, on = c("location_code" = "county_code")]
  out_data <- out_data[, .(n = sum(n), denominator = sum(denominator)), by = .(region_name, yrwk, week)]
  total <- out_data[, .(n = sum(n), denominator = sum(denominator)), by = .(yrwk, week)]
  total[, region_name := "Norge"]
  out_data <- rbindlist(list(out_data, total), use.names = TRUE)
  out_data <- out_data[, rate := round(n / denominator, 4)]

  overview <- dcast(out_data, yrwk + week ~ region_name, value.var = c("rate", "n", "denominator"))
  col_names <- names(overview)
  col_names <- gsub("rate_([A-\u00D8a-\u00F80-9 \\s -]*)$", "\\1 ILI", col_names)
  col_names <- gsub("n_([A-\u00D8a-\u00F80-9 \\s -]*)$", "\\1 ILI konsultasjoner", col_names)
  col_names <- gsub("denominator_([A-\u00D8a-\u00F80-9 \\s -]*)$", "\\1 Totalt konsultasjoner", col_names)
  col_names <- gsub("yrwk$", "\u00C5r-Uke", col_names)
  col_names <- gsub("week$", "Uke", col_names)
  names(overview) <- col_names
  setcolorder(overview, col_names[order(col_names)])


  wb <- xlsx::createWorkbook(type = "xlsx")
  sheet_rate <- xlsx::createSheet(wb, sheetName = "Andel ILI")
  sheet_consult <- xlsx::createSheet(wb, sheetName = "Konsultasjoner")
  sheet_info <- xlsx::createSheet(wb, sheetName = "Info")
  rate_df <- overview %>% dplyr::select("\u00C5r-Uke", "Uke", "\u00D8st ILI", "S\u00F8r ILI", "Vest ILI", "Midt-Norge ILI", "Nord-Norge ILI", "Norge ILI")
  consult_df <- overview %>% dplyr::select(
    "\u00C5r-Uke", "Uke", dplyr::ends_with("konsultasjoner"),
    -"Norge ILI konsultasjoner", -"Norge Totalt konsultasjoner", "Norge ILI konsultasjoner", "Norge Totalt konsultasjoner"
  )

  s <- xlsx::CellStyle(wb, dataFormat = xlsx::DataFormat("#,##0.0 %"))
  xlsx::addDataFrame(rate_df,
    sheet_rate,
    row.names = FALSE,
    colStyle = list(
      "3" = s,
      "4" = s,
      "5" = s,
      "6" = s,
      "7" = s,
      "8" = s
    )
  )

  # xlsx::autoSizeColumn(sheet_rate, colIndex = 1:ncol(rate_df))
  xlsx::addDataFrame(consult_df,
    sheet_consult,
    row.names = FALSE
  )
  # xlsx::autoSizeColumn(sheet_consult, colIndex = 1:ncol(consult_df))
  info <- data.frame(
    Syndrom = conf$tag,
    ICPC2 = paste(conf$icpc2, sep = ","),
    Konktattype = paste(conf$contactType, sep = ","),
    Oppdatert = date
  )
  xlsx::addDataFrame(info,
    sheet_info,
    row.names = FALSE
  )
  regions <- norway_locations()[, .(Fylke = min(county_name), Region = min(region_name)), by = .(county_code)]

  xlsx::addDataFrame(regions[, .(Fylke, Region)],
    sheet_info,
    row.names = FALSE,
    startRow = 4,
  )
  xlsx::autoSizeColumn(sheet_info, colIndex = 1:ncol(info))
  xlsx::saveWorkbook(wb, glue::glue("{conf$folder}/regioner.xlsx"))
}


#' create MEm sheet with doctors
#'
#' @param conf A mem model configuration object
#' @param date extract date
#'
create_n_doctors_sheet <- function(conf, date) {
  current_season <- tbl("spuls_mem_results") %>%
    dplyr::summarize(season = max(season, na.rm = T)) %>%
    dplyr::collect()
  current_season <- current_season$season
  x_tag <- conf$tag
  data <- tbl("spuls_mem_results") %>%
    dplyr::filter(season == current_season & tag == x_tag & location_code == "norge") %>%
    dplyr::collect()
  setDT(data)

  doctors <- fread(path("data_raw", "behandlere.txt", package = "sykdomspuls"))

  doctors[, yrwk := paste(year, stringr::str_pad(week, 2, pad = "0"), sep = "-")]

  doctors[, yrwk := paste(year, stringr::str_pad(week, 2, pad = "0"), sep = "-")]
  doctors[, season := fhi::season(yrwk)]
  prev_year <- as.integer(strsplit(current_season, split = "/")[[1]][1])
  prev_season <- glue::glue("{prev_year - 1}/{prev_year}")

  mean_doctors <- mean(doctors[season == prev_season & (week >= 40 | week <= 20), behandlere])


  doctors[, "Andel_behandlere" := behandlere / mean_doctors * 100]
  overview <- dcast(data, yrwk + week ~ age, value.var = c("rate", "n", "denominator"))
  overview <- overview[doctors[, .(yrwk, behandlere, Andel_behandlere)],
    on = c("yrwk" = "yrwk"), nomatch = 0
  ]

  col_names <- names(overview)
  col_names <- gsub("rate_([0-9 + -]*)$", "\\1 % ILI", col_names)
  col_names <- gsub("n_([0-9 + -]*)$", "\\1 ILI konsultasjoner", col_names)
  col_names <- gsub("denominator_([0-9 + -]*)$", "\\1 Totalt konsultasjoner", col_names)
  col_names <- gsub("yrwk$", "\u00C5r-Uke", col_names)
  col_names <- gsub("week$", "Uke", col_names)
  col_names <- gsub("Andel_behandlere", "% Behandlere", col_names)
  names(overview) <- col_names
  setcolorder(overview, col_names[c(1, 2, 3, 5, 4, 6, 7, 9, 8, 10, 11, 13, 12, 14, 15, 16)])
  wb <- xlsx::createWorkbook(type = "xlsx")
  sheet_1 <- xlsx::createSheet(wb, sheetName = "Influensa")
  sheet_info <- xlsx::createSheet(wb, sheetName = "Info")
  s <- xlsx::CellStyle(wb, dataFormat = xlsx::DataFormat("#,##0.0"))

  xlsx::addDataFrame(overview,
    sheet_1,
    row.names = FALSE,
    colStyle = list(
      "3" = s,
      "4" = s,
      "5" = s,
      "6" = s,
      "16" = s
    )
  )
  # xlsx::autoSizeColumn(sheet_consult, colIndex = 1:ncol(consult_df))
  info <- data.frame(
    Syndrom = conf$tag,
    ICPC2 = paste(conf$icpc2, sep = ","),
    Konktattype = paste(conf$contactType[[1]], sep = ",", collapse = ""),
    Oppdatert = date
  )
  xlsx::addDataFrame(info,
    sheet_info,
    row.names = FALSE
  )
  xlsx::autoSizeColumn(sheet_info, colIndex = 1:ncol(info))
  xlsx::saveWorkbook(wb, glue::glue("{conf$folder}/behandlere.xlsx"))
}


#' create MEM season plots
#'
#' @param conf A mem model configuration object
#' @param date extract date
#'
create_plots <- function(conf, date) {
  current_season <- tbl("spuls_mem_results") %>%
    dplyr::summarize(season = max(season, na.rm = T)) %>%
    dplyr::collect()
  current_season <- current_season$season

  x_tag <- conf$tag
  data <- tbl("spuls_mem_results") %>%
    dplyr::filter(season == current_season & tag == x_tag) %>%
    dplyr::collect()
  setDT(data)
  for (loc in unique(data[, location_code])) {
    data_location <- data[location_code == loc]

    chart <- fhiplot::make_influenza_threshold_chart(data_location, "",
      weeks = c(40, 20),
      color_palette = "influensa", legend_control = "text"
    )

    filename <- fs::path(conf$folder, glue::glue("{get_location_name(loc)}.png"))
    ggsave(filename, chart, height = 7, width = 9)
  }

  latest_week <- max(data[, x])
  weeks <- unique(data[, c("x", "week", "yrwk")])
  setorder(weeks, x)


  data[, status := as.character(NA)]
  data[is.na(status) & rate <= low, status := "Sv\u00E6rt lavt"]
  data[is.na(status) & rate <= medium, status := "Lavt"]
  data[is.na(status) & rate <= high, status := "Middels"]
  data[is.na(status) & rate <= very_high, status := "H\u00F8yt"]
  data[is.na(status) & rate > very_high, status := "Sv\u00E6rt h\u00F8yt"]
  data[, status := factor(status, levels = c(
    "Sv\u00E6rt lavt",
    "Lavt",
    "Middels",
    "H\u00F8yt",
    "Sv\u00E6rt h\u00F8yt"
  ))]
  for (i in 1:nrow(weeks)) {
    counties <- norway_map_counties()
    # print(counties)
    xyrwk <- weeks$yrwk[i]
    plot_data <- counties[data[yrwk == xyrwk], on = .(location_code = location_code), nomatch = 0]
    # print(plot_data)
    label_positions <- norway_map_counties_label_positions()

    ## label_positions <- data.frame(
    ##   location_code = c(
    ##     "county01", "county02", "county03", "county04",
    ##     "county05", "county06", "county07", "county08",
    ##     "county09", "county10", "county11", "county12",
    ##     "county14", "county15", "county18", "county19",
    ##     "county20", "county50"
    ##   ),
    ##   long = c(
    ##     11.266137, 11.2, 10.72028, 11.5, 9.248258, 9.3, 10.0, 8.496352,
    ##     8.45, 7.2, 6.1, 6.5, 6.415354, 7.8, 14.8, 19.244275, 24.7, 11
    ##   ),

    ##   lat = c(
    ##     59.33375, 60.03851, 59.98, 61.26886, 61.25501, 60.3, 59.32481, 59.47989,
    ##     58.6, 58.4, 58.7, 60.25533, 61.6, 62.5, 66.5, 68.9, 69.6, 63
    ##   )
    ## )
    cnames_whole_country <- plot_data[, .(rate, location_code)][label_positions, on = "location_code"]

    cnames_whole_country$rate <- format(round(cnames_whole_country$rate, 1), nsmall = 1)

    cnames_country <- cnames_whole_country[ !(location_code %in% c("county02", "county03"))]
    cnames_osl_ak <- cnames_whole_country[location_code %in% c("county02", "county03")]
    week_string <- gsub("([0-9]*)-([0-9]*)$", "\\2 \\1", xyrwk)
    map_plot <- ggplot() +
      geom_polygon(
        data = plot_data, aes(x = long, y = lat, group = group, fill = status),
        color = "#808080", size = 0.1
      ) +
      theme_void() +

      scale_fill_manual("Niv\u00E5",
        breaks = c(
          "Sv\u00E6rt lavt",
          "Lavt",
          "Middels",
          "H\u00F8yt",
          "Sv\u00E6rt h\u00F8yt"
        ),
        values = c(
          "Sv\u00E6rt lavt" = "#8DCFE4",
          "Lavt" = "#43B3CE",
          "Middels" = "#5793A7",
          "H\u00F8yt" = "#276B81",
          "Sv\u00E6rt h\u00F8yt" = "#00586E"
        ),
        drop = FALSE
      ) +
      geom_text(data = cnames_country, aes(long, lat, label = rate), size = 2.3) +
      geom_text(
        data = data.frame(
          txt = c(glue::glue("Uke {week_string}")),
          lat = c(70), long = c(6)
        ),
        aes(long, lat, label = txt), size = 6
      ) +
      geom_text(
        data = data.frame(
          txt = c(glue::glue('Oppdatert {strftime(as.Date(conf$today), format="%d.%m.%Y")}')),
          lat = c(58), long = c(20)
        ),
        aes(long, lat, label = txt), size = 3
      ) +
      coord_map(projection = "conic", par = 55, xlim = c(4.5, 31))
    legend <- cowplot::get_legend(map_plot)

    if (config$border == 2019) {
      insert_title <- "Oslo og Akershus"
    } else {
      insert_title <- "Oslo"
    }

    insert <- ggplot() +
      geom_polygon(
        data = plot_data[location_code %in% c("county03", "county02")],
        aes(x = long, y = lat, group = group, fill = status),
        color = "#808080", size = 0.1
      ) +
      theme_void() +
      scale_fill_manual("Niv\u00E5", values = c(
        "Sv\u00E6rt lavt" = "#8DCFE4",
        "Lavt" = "#43B3CE",
        "Middels" = "#5793A7",
        "H\u00F8yt" = "#276B81",
        "Sv\u00E6rt h\u00F8yt" = "#00586E"
      ), drop = FALSE) +
      geom_text(data = cnames_osl_ak, aes(long, lat, label = rate), size = 2.3) +
      theme(legend.position = "none") +
      ggtitle(insert_title) +
      theme(plot.title = element_text(size = 8, )) +
      coord_map(projection = "conic", par = 55)


    filename <- fs::path(conf$folder, glue::glue("map_week_{xyrwk}.png"))
    filename_legend <- fs::path(conf$folder, glue::glue("map_week_{xyrwk}_legend.png"))
    grDevices::png(filename, width = 7, height = 6, units = "in", res = 800)
    grid::grid.newpage()
    vpb_ <- grid::viewport(width = 1.2, height = 1, x = 0.5, y = 0.5, clip = TRUE) # the larger map
    vpa_ <- grid::viewport(width = 0.3, height = 0.3, x = 0.6, y = 0.3)
    print(map_plot + theme(legend.position = "none"), vp = vpb_)
    print(insert, vp = vpa_)
    grDevices::dev.off()
    image <- magick::image_read(filename)
    image <- magick::image_crop(image, "3760x4800+680+0")
    magick::image_write(image, filename, format = "png")
    ggsave(filename_legend, ggpubr::as_ggplot(legend), height = 3, width = 3)
  }
}


