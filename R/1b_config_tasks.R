set_tasks <- function() {
  config$tasks <- TaskManager$new()

  ##############
  #### data ####
  config$tasks$add_task(
    task_from_config(
      list(
        name = "data_normomo",
        type = "data",
        action = "data_normomo",
        schema = list(output = config$schema$datar_normomo)
      )
    )
  )

  config$tasks$add_task(
    task_from_config(
      list(
        name = "data_msis",
        type = "data",
        action = "data_msis",
        schema = list(output = config$schema$data_msis),
        args = list(
          start_year = 2008,
          end_year = 2019
        )
      )
    )
  )

  config$tasks$add_task(
    task_from_config(
      list(
        name = "data_norsyss",
        type = "data",
        action = "data_norsyss",
        schema = list(output = config$schema$data_norsyss),
        args = list(
          syndromes = rbind(
            data.table(
              tag = "gastro",
              syndrome = "gastro",
              contactType = list(c("Legekontakt", "Telefonkontakt"))
            ),
            data.table(
              tag = "influensa",
              syndrome = "influensa",
              contactType = list("Legekontakt")
            ),
            data.table(
              tag = "influensa_all",
              syndrome = "influensa_all",
              contactType = list(c("Legekontakt", "Telefonkontakt"))
            )
          )
        )
      )
    )
  )

  ##################
  #### analysis ####
  config$tasks$add_task(
    Task$new(
      name = "analysis_normomo",
      type = "analysis",
      plans = analysis_normomo_plans(),
      schema = c("output" = config$schema$results_normomo),
      cores = min(6, parallel::detectCores()),
      chunk_size = 1
    )
  )

  config$tasks$add_task(
    task_from_config(
      conf = list(
        name = "analysis_norsyss_qp_gastro",
        db_table = "data_norsyss",
        type = "analysis",
        dependencies = c("data_norsyss"),
        action = "analysis_qp",
        filter = "tag_outcome=='gastro'",
        for_each = list("location_code" = "all", "age" = "all", "sex" = "Totalt"),
        schema = list(
          output = config$schema$results_qp,
          output_limits = config$schema$results_mem_limits
        ),
        args = list(
          tag = "gastro",
          train_length = 5,
          years = c(2018, 2019),
          weeklyDenominatorFunction = sum,
          denominator = "consult_without_influenza",
          granularity_time = "weekly"
        )
      )
    )
  )

  config$tasks$add_task(
    task_from_config(
      list(
        name = "analysis_norsyss_mem_influensa",
        db_table = "data_norsyss",
        type = "analysis",
        dependencies = c("data_norsyss"),
        action = "analysis_mem",
        filter = "(granularity_geo=='county' | granularity_geo=='norge') & tag_outcome=='influensa'",
        for_each = list("location_code" = "all"),
        schema = list(
          output = config$schema$results_mem,
          output_limits = config$schema$results_mem_limits
        ),
        args = list(
          age = jsonlite::toJSON(list("Totalt" = c("Totalt"))),
          tag = "influensa",
          weeklyDenominatorFunction = "sum",
          multiplicative_factor = 100,
          denominator = "consult_with_influenza"
        )
      )
    )
  )

  config$tasks$add_task(
    task_from_config(
      list(
        name = "analysis_norsyss_mem_influensa_all",
        db_table = "data_norsyss",
        type = "analysis",
        dependencies = c("data_norsyss"),
        action = "analysis_mem",
        filter = "(granularity_geo=='county' | granularity_geo=='norge') & tag_outcome=='influensa_all'",
        for_each = list("location_code" = "all"),
        schema = list(
          output = config$schema$results_mem,
          output_limits = config$schema$results_mem_limits
        ),
        args = list(
          age = jsonlite::toJSON(list(
            "0-4" = c("0-4"), "5-14" = c("5-14"),
            "15-64" = c("15-19", "20-29", "30-64"), "65+" = c("65+")
          )),
          tag = "influensa",
          weeklyDenominatorFunction = "sum",
          multiplicative_factor = 100,
          denominator = "consult_with_influenza"
        )
      )
    )
  )

  config$tasks$add_task(
    task_from_config(
      list(
        name = "analysis_analysis_msis",
        type = "analysis",
        db_table = "data_msis",
        action = "analysis_simple",
        dependencies = c("data_msis"),
        schema = list(output = config$schema$results_simple),
        for_each = list("location_code" = "all", "tag_outcome" = c("Kikoste", "Campylobacteriose")),
        args = list(
          group_by = "month",
          past_years = 5
        )
      )
    )
  )

  ############
  #### ui ####
  config$tasks$add_task(
    task_from_config(
      list(
        name = "ui_threshold_plot_msis",
        type = "ui",
        action = "ui_create_threshold_plot",
        db_table = "results_simple",
        schema = NULL,
        for_each = list("location_code" = "all", "tag_outcome" = c("Kikoste", "Campylobacteriose")),
        dependencies = c("norsyss_mem_influensa"),
        args = list(
          filename = "{location_code}.png",
          folder = " {tag_outcome}/{today}"
        ),
        filter = "year > 2010 & source == 'data_msis'"
      )
    )
  )

  config$tasks$add_task(
    task_from_config(
      list(
        name = "ui_norsyss_mem_influensa",
        type = "ui",
        action = "ui_mem_plots",
        db_table = "results_mem",
        schema = NULL,
        for_each = list(tag_outcome = c("influensa_all")),
        dependencies = c("norsyss_mem_influensa_all"),
        args = list(
          tag = "influensa",
          icpc2 = "R60",
          contactType = "Legekontakt, Telefonkontakt",
          folder_name = "mem_influensa",
          outputs = c("n_doctors_sheet")
        ),
        filter = "source=='data_norsyss'"
      )
    )
  )

  config$tasks$add_task(
    task_from_config(
      list(
        name = "ui_norsyss_mem_influensa_all",
        type = "ui",
        action = "ui_mem_plots",
        db_table = "results_mem",
        schema = NULL,
        for_each = list(tag_outcome = c("influensa")),
        dependencies = c("simple_analysis_msis"),
        args = list(
          tag = "influensa",
          icpc2 = "R60",
          contactType = "Legekontakt",
          folder_name = "mem_influensa",
          outputs = c("charts", "county_sheet", "region_sheet", "norway_sheet")
        ),
        filter = "source=='data_norsyss'"
      )
    )
  )
}