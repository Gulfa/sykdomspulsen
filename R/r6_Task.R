#' task_from_config
#' @export
task_from_config <- function(conf) {
  name <- conf$name
  plans <- list()
  schema <- conf$schema
  task <- NULL
  if (conf$type == "data") {
    plan <- plnr::Plan$new()
    arguments <- list(fn = get(conf$action), name = name)
    if ("args" %in% names(conf)) {
      arguments <- c(arguments, conf$args)
    }
    do.call(plan$add_analysis, arguments)

    task <- Task$new(
      name = name,
      type = conf$type,
      plans = list(plan),
      schema = schema
    )
  } else if (conf$type %in% c("analysis", "ui")) {
    task <- Task$new(
      name = name,
      type = conf$type,
      plans = plans,
      schema = schema
    )

    task$update_plans_fn <- function() {
      table_name <- conf$db_table
      x_plans <- list()

      filters <- list()
      for (t in names(conf$for_each)) {
        if (conf$for_each[t] == "all") {
          filter <- get_list(conf, "filter", default = "")
          table <- tbl(table_name)
          if (filter != "") {
            table <- table %>% dplyr::filter(!!!rlang::parse_exprs(filter))
          }
          options <- table %>%
            dplyr::distinct(!!as.symbol(t)) %>%
            dplyr::collect() %>%
            dplyr::pull(!!as.symbol(t))
        } else {
          options <- conf$for_each[[t]]
        }
        filters[[t]] <- options
      }

      filters <- do.call(tidyr::crossing, filters)
      for (i in 1:nrow(filters)) {
        current_plan <- plnr::Plan$new()
        fs <- c()
        arguments <- list(
          fn = get(conf$action), name = glue::glue("{name}{i}"),
          source_table = table_name,
          today = Sys.Date()
        )
        for (n in names(filters)) {
          arguments[n] <- filters[i, n]
          fs <- c(fs, glue::glue("{n}=='{filters[i,n]}'"))
        }
        extra_filter <- get_list(conf, "filter", default = "")

        filter <- paste(fs, collapse = " & ")
        if (extra_filter != "") {
          filter <- paste(filter, extra_filter, sep = " & ")
        }
        current_plan$add_data(name = "data", fn = function() {
          if (is.na(filter)) {
            d <- tbl(table_name) %>%
              dplyr::collect() %>%
              latin1_to_utf8()
          } else {
            d <- tbl(table_name) %>%
              dplyr::filter(!!!rlang::parse_exprs(filter)) %>%
              dplyr::collect() %>%
              latin1_to_utf8()
          }
          return(d)
        })

        if ("args" %in% names(conf)) {
          arguments <- c(arguments, conf$args)
        }
        do.call(current_plan$add_analysis, arguments)
        x_plans[[i]] <- current_plan
      }
      return(x_plans)
    }
  }
  return(task)
}
#' Task
#'
#' @import R6
#' @export
Task <- R6::R6Class(
  "Task",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    type = NULL,
    plans = list(),
    schema = list(),
    parallel = FALSE,
    name = NULL,
    update_plans_fn = NULL,
    initialize = function(name, type, plans, schema, parallel = FALSE) {
      self$name <- name
      self$type <- type
      self$plans <- plans
      self$schema <- schema
      self$parallel <- parallel
    },
    update_plans = function() {
      if (!is.null(self$update_plans_fn)) {
        self$plans <- self$update_plans_fn()
      }
    },
    num_argsets = function() {
      retval <- 0
      for (i in seq_along(plans)) {
        retval <- retval + plans[[i]]$len()
      }
      return(retval)
    },
    run = function(log = TRUE) {
      # task <- config$tasks$task_get("analysis_normomo")
      message(glue::glue("task: {self$name}"))
      if (log == FALSE | can_run()) {
        self$update_plans()

        message(glue::glue("Running task {self$name} with {self$num_argsets()} argsets"))

        pb <- progress::progress_bar$new(
          format = "[:bar] :current/:total (:percent) in :elapsed, eta: :eta",
          total = self$num_argsets()
        )
        pb$tick(0)
        for (i in seq_along(plans)) {
          print(i)
          plans[[i]]$set_pb(pb)
          plans[[i]]$run_all(schema = schema)
        }
        if (log) {
          update_rundate(
            package = self$name,
            date_results = lubridate::today(),
            date_extraction = lubridate::today(),
            date_run = lubridate::today()
          )
        }
      } else {
        print(glue::glue("Not running {self$name}"))
      }
    },
    can_run = function() {
      return(TRUE)
    }
  )
)
