#'ui_norsyss_pdf
#'
#' @export
ui_norsyss_pdf <- function(data, argset, schema) {
  fs::dir_create(sykdomspulspdf_folder("rmarkdown", argset$today))
  fs::dir_create(sykdomspulspdf_folder("pdf", argset$today))
  fs::dir_create(sykdomspulspdf_folder("svg", argset$today))

  locs <- unique(fd::norway_locations()[, c("county_code", "county_name")])

  table <- schema$input$dplyr_tbl()
  
  for (tag in argset$tags) {
    fd::msg(glue::glue("sykdomspulspdf {tag}"), slack = T)
    # setup
    files <- c("monthly_report.Rmd", "monthly_reportALL.Rmd")

    for (f in files) {
      file.copy(
        from = system.file("extdata", "norsyss_pdf", f, package = "sykdomspulsen"),
        to = fs::path(sykdomspulspdf_folder("rmarkdown", argset$today), f),
        overwrite = !config$is_production
      )
    }

    file_before <- glue::glue("child_{tag}.Rmd")
    files_after <- glue::glue("{locs$county_code}_child_{tag}.Rmd")
    for (i in seq_along(files_after)) {
      if (file.exists(system.file("extdata","norsyss_pdf", files_after[i], package = "sykdomspulsen"))) {
        file.copy(
          from = system.file("extdata", "norsyss_pdf",files_after[i], package = "sykdomspulsen"),
          to = fs::path(sykdomspulspdf_folder("rmarkdown", argset$today), files_after[i]),
          overwrite = !config$is_production
        )
      } else {
        file.copy(
          from = system.file("extdata", "norsyss_pdf", file_before, package = "sykdomspulsen"),
          to = fs::path(sykdomspulspdf_folder("rmarkdown", argset$today), files_after[i]),
          overwrite = !config$is_production
        )
      }
    }

    fhi::sykdompulspdf_resources_copy(sykdomspulspdf_folder("rmarkdown", argset$today))

    # graphs
    q <- sykdomspulspdf_plot_total(table, location_code = "norge", x_tag = tag)
    ggsave(
      filename = sykdomspulspdf_folder("svg",  argset$today, glue::glue("{tag} Norge alle alder {argset$today}.svg")),
      plot = q,
      width = 7,
      height = 4,
      units = "in"
    )

    q <- sykdomspulspdf_plot_ages(table, location_code = "norge", x_tag = tag)
    ggsave(
      filename = sykdomspulspdf_folder("svg", argset$today, glue::glue("{tag} Norge aldersfordelt {argset$today}.svg")),
      plot = q,
      width = 7,
      height = 4,
      units = "in"
    )

    # pdfs

    for (i in 1:nrow(locs)) {
      input <- fs::path(sykdomspulspdf_folder("rmarkdown", argset$today), "monthly_report.Rmd")
      output_dir <- sykdomspulspdf_folder("pdf", argset$today)
      output_file <- glue::glue("{tag}_{locs$county_code[i]}_monthly_report.pdf")
      output_file_renamed <- glue::glue("{locs$county_name[i]}_{tag}.pdf")
      rmarkdown::render(
        input = fs::path(sykdomspulspdf_folder("rmarkdown", argset$today), "monthly_report.Rmd"),
        output_dir = output_dir,
        output_file = output_file,
        params = list(
          tag = tag,
          location_code = locs$county_code[i]
        ),
        envir = new.env(),
        quiet = TRUE
      )
      # rename
      fs::file_move(
        fs::path(output_dir, output_file),
        fs::path(output_dir, output_file_renamed)
      )
    }

    rmarkdown::render(
      input = fs::path(sykdomspulspdf_folder("rmarkdown", argset$today), "monthly_reportALL.Rmd"),
      output_file = glue::glue("{tag}_ALL_monthly_report.pdf"),
      output_dir = sykdomspulspdf_folder("pdf", argset$today),
      params = list(
        tag = tag
      ),
      envir = new.env(),
      quiet = TRUE
    )
  }

  create_latest_folder("norsyss_pdfs", argset$today)
}


sykdomspulspdf_plot_total <- function(table, location_code, x_tag) {
  data_long <- table %>%
    dplyr::filter(granularity_time == "weekly") %>%
    dplyr::filter(location_code == !!location_code) %>%
    dplyr::filter(tag_outcome == !!x_tag) %>%
    dplyr::filter(age == "Totalt") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  # remove last 3 weeks
  yrwks <- rev(unique(data_long$yrwk))[-c(1:3)]
  data_long <- data_long[yrwk %in% yrwks]
  data_long[, location_name:=get_location_name(location_code)]
  data_long[, season := fhi::season(yrwk)]

  seasons <- rev(unique(data_long$season))[1:5]
  labs <- unique(data_long[, c("week")])
  labs <- labs[as.numeric(week) %in% seq(2, 52, 2)]

  data_long <- data_long[season %in% seasons]
  yrange <- max(data_long$n)

  title <- glue::glue(
    "{syndrome}, {location}, alle aldersgrupper",
    syndrome = config$def$long_names[x_tag],
    location = data_long$location_name[1]
  )

  q <- ggplot(data_long, aes(x = x, y = n, color = season))
  q <- q + geom_vline(xintercept = fhi::x(51), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(2), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(11), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(17), lty = 2)
  q <- q + annotate("label", x = (fhi::x(51) + fhi::x(2)) / 2, y = yrange * 0.03, label = "Jul/Nytt\u00E5r", size = 3)
  q <- q + annotate("label", x = fhi::x(14), y = yrange * 0.03, label = "P\u00E5ske", size = 3)
  q <- q + geom_line(lwd = 0.5)
  q <- q + geom_line(data = data_long[season == max(season)], lwd = 1.5)
  q <- q + fhiplot::theme_fhi_basic(10)
  q <- q + fhiplot::scale_color_fhi("", palette = "combination", direction = -1)
  q <- q + guides(color = guide_legend(reverse = FALSE))
  q <- q + expand_limits(y = 0)
  q <- q + scale_y_continuous("Antall konsultasjoner", expand = expand_scale(mult = c(0, 0.1)))
  q <- q + scale_x_continuous(
    "Ukenummer",
    expand = expand_scale(mult = c(0, 0)),
    breaks = labs$x,
    labels = labs$week
  )
  q <- q + labs(title = title)
  q
}


sykdomspulspdf_folder <- function(folder, date, further = NULL) {
  if (is.null(further)) {
    retval <- path("output", "norsyss_pdfs", date, folder)
  } else {
    retval <- path("output", "norsyss_pdfs", date, folder, further)
  }
  return(retval)
}

sykdomspulspdf_plot_ages <- function(table, location_code, x_tag) {
  data_long <- table %>%
    dplyr::filter(granularity_time == "weekly") %>%
    dplyr::filter(location_code == !!location_code) %>%
    dplyr::filter(tag_outcome == !!x_tag) %>%
    dplyr::filter(age != "Totalt") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  # remove last 3 weeks
  yrwks <- rev(unique(data_long$yrwk))[-c(1:3)]
  data_long <- data_long[yrwk %in% yrwks]
  data_long[, location_name:=get_location_name(location_code)]
  data_long[, season := fhi::season(yrwk)]
  data_long[, age := car::recode(
    age,
    glue::glue(
      "c('5-14','15-19')='5-19';",
      "c('20-29','30-64')='20-64'"
    )
  )]
  data_long <- data_long[, .(
    n = sum(n)
  ), keyby = .(
    age,
    season,
    x,
    week,
    location_name
  )]
  data_long[, age := factor(
    age,
    levels = c("0-4", "5-19", "20-64", "65+"),
    labels = c(
      glue::glue("0-4 {fhi::nb$aa}r"),
      glue::glue("5-19 {fhi::nb$aa}r"),
      glue::glue("20-64 {fhi::nb$aa}r"),
      glue::glue("65+ {fhi::nb$aa}r")
    )
  )]

  seasons <- rev(unique(data_long$season))[1:5]
  labs <- unique(data_long[, c("week", "x")])
  labs <- labs[as.numeric(week) %in% seq(2, 52, 4)]

  data_long <- data_long[season %in% seasons]
  yrange <- max(data_long$n)

  title <- glue::glue(
    "{syndrome}, {location}, aldersfordelt",
    syndrome = config$def$long_names[x_tag],
    location = data_long$location_name[1]
  )

  q <- ggplot(data_long, aes(x = x, y = n, color = season))
  q <- q + geom_vline(xintercept = fhi::x(51), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(2), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(11), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(17), lty = 2)
  q <- q + geom_line(lwd = 0.5)
  q <- q + geom_line(data = data_long[season == max(season)], lwd = 1.5)
  q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "all", scales = "free")
  q <- q + fhiplot::theme_fhi_basic(10)
  q <- q + fhiplot::scale_color_fhi("", palette = "combination", direction = -1)
  q <- q + guides(color = FALSE)
  q <- q + expand_limits(y = 0)
  q <- q + scale_y_continuous("Antall konsultasjoner", expand = expand_scale(mult = c(0, 0.1)))
  q <- q + scale_x_continuous(
    "Ukenummer",
    expand = expand_scale(mult = c(0, 0)),
    breaks = labs$x,
    labels = labs$week
  )
  q <- q + labs(title = title)
  q
}
