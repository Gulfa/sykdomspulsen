#' ui_alert_pdf
#'
#' @export 
ui_alert_pdf <- function(data, argset, schema) {
  
  fd::msg("Sykdomspuls: creating alerts pdf", slack = T)
  max_date_q <- schema$input$dplyr_tbl() %>%
    dplyr::summarise(m=max(date)) %>% dplyr::collect()
  max_yrwk <- fhi::isoyearweek(max_date_q$m)
  tags <- argset$tags
  d <- schema$input$dplyr_tbl() %>%
    dplyr::filter(granularity_time == "weekly") %>%
    dplyr::filter(granularity_geo == "municip") %>%
    dplyr::filter(yrwk == !!max_yrwk) %>%
    dplyr::filter(n_status == "High") %>%
    dplyr::filter(tag_outcome %in% tags) %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  if (nrow(d) == 0) {
    return()
  }
  d[, location_name:=get_location_name(location_code)]
  d <- unique(d[, c("tag_outcome", "location_code", "location_name")])
  d[, name_short:=config[["def"]]$short_names[tag_outcome]]
  d[, name_long:=config[["def"]]$long_names[tag_outcome]]
  d[, output_file := glue::glue(
    "{name_short}_{location_name}.pdf",
    name_short = name_short,
    location_name = location_name
  )]

  fs::dir_create(path("output","norsyss", argset$today))
  d[, output_dir := path("output","norsyss",argset$today)]
  d[, attachment := fs::path(output_dir, output_file)]
  for (i in 1:nrow(d)) {
    Sys.sleep(1)

    input <- system.file("extdata", "alert.Rmd", package = "sykdomspulsen")
    output_file <- d$output_file[i]
    output_dir <- d$output_dir[i]
    x_location_code <- d$location_code[i]
    x_tag <- d$tag_outcome[i]
    x_name_long <- d$name_long[i]
    rmarkdown::render(
      input = input,
      output_file = output_file,
      output_dir = output_dir,
      params = list(
        location_code=x_location_code,
        tag=x_tag,
        name_long=x_name_long
      ),
      envir=new.env()
    )
  }
  tab <- huxtable::hux(
    Syndrom = d$name_long,
    Kommunenavn = d$location_name,
    Kommunenummer = d$location_code,
      file = d$output_file
  ) %>%
    huxtable::add_colnames() %>%
    huxtable::theme_basic() %>%
    huxtable::to_html()
  html <- glue::glue(
    "Please find attached sykdomspuls alert pdfs.<br>",
      "These are the municipalities with at least one z-score above 4<br><br>",
      "{tab}"
    )
    
  attachments <- d$attachment
  if (length(attachments) > 10) attachments <- attachments[1:10]


  fd::mailgun(
    subject = "Sykdomspuls alert pdfs",
    html = html,
    bcc = fd::e_emails(
      "sykdomspuls_utbrudd",
      is_final = is_final()
    ),
    attachments = attachments,
    is_final = is_final()
  )
}

