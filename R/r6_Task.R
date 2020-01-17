#' task_from_config
#'
#' @export
task_from_config <- function(conf) {
  name <- conf$name
  plans <- list()
  schema <- conf$schema
  cores <- get_list(conf, "cores", 1)
  chunk_size <- get_list(conf, "chunk_size", 1)
  task <- NULL
  if (conf$type == "data") {
    plan <- plnr::Plan$new()
    arguments <- list(fn = get(conf$action), name = name,
                      today=Sys.Date())
    if ("args" %in% names(conf)) {
      arguments <- c(arguments, conf$args)
    }
    do.call(plan$add_analysis, arguments)

    task <- Task$new(
      name = name,
      type = conf$type,
      plans = list(plan),
      schema = schema,
      cores = cores,
      chunk_size = chunk_size

    )
  } else if (conf$type %in% c("analysis", "ui")) {
    task <- Task$new(
      name = name,
      type = conf$type,
      plans = plans,
      schema = schema,
      cores = cores,
      chunk_size = chunk_size
      
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
#' @import foreach
#' @export
Task <- R6::R6Class(
  "Task",
  portable = FALSE,
  cloneable = TRUE,
  public = list(
    type = NULL,
    plans = list(),
    schema = list(),
    cores = 1,
    chunk_size = 100,
    name = NULL,
    update_plans_fn = NULL,
    initialize = function(name, type, plans, schema, cores = 1, chunk_size = 100) {
      self$name <- name
      self$type <- type
      self$plans <- plans
      self$schema <- schema
      self$cores <- cores
      self$chunk_size <- chunk_size
    },
    update_plans = function() {
      if (!is.null(self$update_plans_fn)) {
        message(glue::glue("Updating plans..."))
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
    run = function(log = TRUE, cores = self$cores) {
      # task <- config$tasks$task_get("analysis_normomo")
      message(glue::glue("task: {self$name}"))
      if (log == FALSE | can_run()) {
        self$update_plans()

        # progressr::with_progress({
        #   pb <- progressr::progressor(steps = self$num_argsets())
        #   for (i in seq_along(plans)) {
        #     if(!interactive()) print(i)
        #     plans[[i]]$set_progress(pb)
        #     plans[[i]]$run_all(schema = schema)
        #   }
        # })

        message(glue::glue("Running task={self$name} with plans={length(self$plans)} and argsets={self$num_argsets()}"))

        if(cores != 1){
          doFuture::registerDoFuture()
          if(length(self$plans)==1){
            # parallelize the inner loop
            future::plan(list(
              future::sequential,
              future::multisession,
              workers = cores,
              earlySignal = TRUE
              ))

            parallel <- "plans=sequential, argset=multisession"
          } else {
            # parallelize the outer loop
            future::plan(future::multisession, workers = cores, earlySignal = TRUE)

            parallel <- "plans=multisession, argset=sequential"
          }
        } else {
          parallel <- "plans=sequential, argset=sequential"
        }

        message(glue::glue("{parallel} with cores={self$cores} and chunk_size={self$chunk_size}"))

        progressr::with_progress({
          pb <- progressr::progressor(steps = self$num_argsets())
          y <- foreach(x = self$plans, .options.future = list(chunk.size = self$chunk_size)) %dopar% {
            if(cores != 1) data.table::setDTthreads(1)

            for(s in schema) s$db_connect()
            x$set_progress(pb)
            #x$run_all(schema = schema, chunk_size = self$chunk_size)
            x$run_all(schema = schema)
            for(s in schema) s$db_disconnect()
          }
        })

        future::plan(future::sequential)
        data.table::setDTthreads()

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
