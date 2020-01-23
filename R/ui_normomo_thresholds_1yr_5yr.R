#' ui_normomo_thresholds_1yr_5yr
#'
#' @export
ui_normomo_thresholds_1yr_5yr <- function(data, argset, schema) {
  # tm_update_plans("ui_normomo_thresholds_1yr_5yr")
  # data <- tm_get_data("ui_normomo_thresholds_1yr_5yr", index_plan=2)
  # argset <- tm_get_argset("ui_normomo_thresholds_1yr_5yr", index_plan=1, index_argset = 1)
  # schema <- tm_get_schema("ui_normomo_thresholds_1yr_5yr")

  if(argset$age=="totalt"){
    title1 <- glue::glue("Totalt antall d{fhi::nb$oe}de per uke siste {fhi::nb$aa}r")
    title2 <- glue::glue("Totalt antall d{fhi::nb$oe}de per uke siste 5 {fhi::nb$aa}r")
  } else {
    title1 <- glue::glue("Antall ({argset$age} {fhi::nb$aa}r) d{fhi::nb$oe}de per uke siste {fhi::nb$aa}r")
    title2 <- glue::glue("Antall ({argset$age} {fhi::nb$aa}r) d{fhi::nb$oe}de per uke siste 5 {fhi::nb$aa}r")
  }

  # data
  pd <- copy(data$data)

  # folder
  folder <- glue::glue(
    argset$folder,
    today = argset$today
  )
  fs::dir_create(path("output",folder))

  # caption
  caption <- glue::glue('Sist oppdatert: {strftime(argset$today, format = "%d/%m/%Y")}')

  # graph1
  q <- normomo_graph_thresholds_1yr_5yr(
    pd = pd,
    title1 = title1,
    title2 = title2,
    includeRealDeaths = FALSE,
    caption = caption
  )
  filename <- glue::glue(
    argset$filename,
    tag = "excl_reported",
    location_code = argset$location_code,
    age = argset$age,
    yrwk_minus_1 = fhi::isoyearweek(argset$today-7)
  )
  fhiplot::save_a4(
    q,
    filename = path("output", folder, filename)
  )

  # graph2
  q <- normomo_graph_thresholds_1yr_5yr(
    pd = pd,
    title1 = title1,
    title2 = title2,
    includeRealDeaths = TRUE,
    caption = caption
  )
  filename <- glue::glue(
    argset$filename,
    tag = "incl_reported",
    location_code = argset$location_code,
    age = argset$age,
    yrwk_minus_1 = fhi::isoyearweek(argset$today-7)
  )
  fhiplot::save_a4(
    q,
    filename = path("output", folder, filename)
  )

}

normomo_graph_thresholds_1yr_5yr <- function(
  pd,
  title1,
  title2,
  includeRealDeaths = FALSE,
  caption = "") {

  pd <- copy(pd)
  setorder(pd,yrwk)
  pd[,rownum:=1:.N]
  pd1 <- pd[rownum >= max(rownum) - 52]
  pd2 <- pd[rownum >= max(rownum) - 52 * 5 + 1]

  pd1[, titlex := title1]
  pd2[, titlex := title2]

  pd1[, type := "top"]
  pd2[, type := "bottom"]

  pd <- rbind(pd1, pd2)
  pd[, titlex := factor(titlex, levels = c(title1, title2))]

  pd[, ymax := max(ncor_est, ncor_baseline_thresholdu1)]
  pd[, unstableEstimates := "Stable"]
  pd[forecast==TRUE, unstableEstimates := "Unstable"]

  pd[, rownum_split := rownum]
  pd[type == "bottom", rownum_split := rownum * 10L]

  breaks <- unique(pd[, c("yrwk", "rownum"), with = FALSE])
  breaksTop <- breaks[seq(1, 53, 4)]
  breaksTop[, label := yrwk]

  breaks <- unique(pd[, c("rownum", "year"), with = FALSE])
  setorder(breaks, rownum)
  breaks[, year2 := shift(year)]
  breaksBottom <- stats::na.omit(breaks[breaks$year != breaks$year2, ])
  breaksBottom$label <- paste0(breaksBottom$year,"-01")
  breaksBottom[, rownum := rownum * 10L]

  breaks <- rbind(breaksTop[, c("rownum", "label")], breaksBottom[, c("rownum", "label")])

  filllabels1 <- c("Prediksjonsintervall", "Betydelig forh\u00F8yet", "Forh\u00F8yet", "Normalt", "")
  shapelabels <- c("Forel\u00F8pig")
  colourlabels <- c("Korrigert for forsinkelse", "Rapporterte d\u00F8dsfall")
  ylabel <- "Antall d\u00F8de per uke"

  q <- ggplot(pd, aes(x = rownum_split))
  q <- q + geom_ribbon(aes(ymin = -Inf, ymax = ncor_baseline_thresholdl0, fill = "5lower"))
  q <- q + geom_ribbon(aes(ymin = ncor_baseline_thresholdl0, ymax = ncor_baseline_thresholdu0, fill = "4expected"))
  q <- q + geom_ribbon(aes(ymin = ncor_baseline_thresholdu0, ymax = ncor_baseline_thresholdu1, fill = "3high"))
  q <- q + geom_ribbon(aes(ymin = ncor_baseline_thresholdu1, ymax = Inf, fill = "2veryhigh"))
  q <- q + geom_ribbon(data = pd[forecast==TRUE & type == "top"], mapping = aes(ymin = ncor_thresholdl0, ymax = ncor_thresholdu0, fill = "1predinterval"), alpha = 0.5)
  if (includeRealDeaths) q <- q + geom_line(data = pd[type == "top"], mapping = aes(y = n_obs, colour = "Rapporterte"), lwd = 0.5)
  q <- q + geom_line(aes(y = ncor_est, colour = "Korrigert"), lwd = 0.5)
  q <- q + geom_point(data = pd[forecast == TRUE], aes(y = ncor_est, shape = "Usikkert"), size = 2)
  q <- q + facet_wrap(~titlex, scales = "free", ncol = 1)
  # q <- q + labs(title=title)
  q <- q + scale_x_continuous("", breaks = breaks$rownum, labels = breaks$label, expand = expand_scale(mult = c(0, 0.01)))
  q <- q + scale_y_continuous(ylabel)
  q <- q + scale_fill_manual("",
                             values = c(
                               "1predinterval" = fhiplot::base_color[[1]],
                               "2veryhigh" = fhiplot::warning_color[["hig"]],
                               "3high" = fhiplot::warning_color[["med"]],
                               "4expected" = fhiplot::warning_color[["low"]],
                               "5lower" = "white"
                             ),
                             labels = filllabels1
  )
  q <- q + scale_shape_manual("",
                              values = c("Usikkert" = 16),
                              labels = shapelabels
  )
  q <- q + scale_colour_manual("",
                               values = c("Korrigert" = "black", "Rapporterte" = "red"),
                               labels = colourlabels
  )
  q <- q + labs(caption = caption)
  q <- q + fhiplot::theme_fhi_lines(base_size = 18)
  q <- q + fhiplot::set_x_axis_vertical()
  # q <- q + theme(panel.grid.major = element_line(colour = "white"),
  #               panel.grid.minor = element_line(colour = "white", size = 0.25))
  q <- q + guides(fill = guide_legend(title.position = "top", reverse = F, order = 1, ncol = 1))
  q <- q + guides(colour = guide_legend(title.position = "top", reverse = F, order = 2, ncol = 1))
  q <- q + guides(shape = guide_legend(title.position = "top", reverse = F, order = 3, ncol = 1))
  if (!is.null(title1)) {
    q <- q + theme(legend.position = "right")
  } else {
    q <- q + theme(legend.position = "bottom")
  }
  # q <- SMAOFormatGGPlot(q, legendPos="right", xAngle=90,ncol=1,legendBorder=TRUE)
  # q <- format_plot(q,2,2,stripes=TRUE, xangle=90)
  return(q)
}

normomo_graphs_deaths <- function(
  runName = "norge",
  data,
  folder) {
  storedData <- list()
  if (runName == "norge") {
    runList <- c("Total", "0to4", "5to14", "15to64", "65P")
  } else {
    runList <- "Total"
  }
  for (i in runList) {
    if (i == "Total") {
      title1 <- "Totalt antall d\u00F8de per uke siste \u00E5r"
      title1a <- "Totalt antall d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Totalt antall d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Totalt antall d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av totalt antall d\u00F8de per uke siste"
    } else if (i == "0to4") {
      title1 <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r"
      title1a <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (0-4 \u00E5r) per uke"
    } else if (i == "5to14") {
      title1 <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (5-14 \u00E5r) per uke"
    } else if (i == "15to64") {
      title1 <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (15-64 \u00E5r) per uke"
    } else if (i == "65P") {
      title1 <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (65+ \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (65+ \u00E5r) per uke"
    }

    q <- GraphTogether(
      data = data[age == i],
      title1 = title1,
      title2 = title2,
      includeRealDeaths = FALSE,
      caption = paste("Sist oppdatert: ", strftime(fd::get_rundate()[package == "normomo"]$date_extraction, format = "%d/%m/%Y"), sep = "")
    )
    fhiplot::save_a4(q, filename = paste0(folder, "/excl_reported_", runName, "-", i, "-", normomo_yrwk(), ".png"))

    q <- GraphTogether(
      data = data[age == i],
      title1 = title1,
      title2 = title2,
      includeRealDeaths = TRUE,
      caption = paste("Sist oppdatert: ", strftime(fd::get_rundate()[package == "normomo"]$date_extraction, format = "%d/%m/%Y"), sep = "")
    )
    fhiplot::save_a4(q, filename = paste0(folder, "/incl_reported_", runName, "-", i, "-", normomo_yrwk(), ".png"))
  }
}


