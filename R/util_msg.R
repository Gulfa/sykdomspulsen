#' Enhanced Messaging
#' @param txt Text
#' @param type msg, warn, err
#' @param syscalls_depth The number of syscalls included in the message. Set to 0 to disable.
#' @param newLine Should there be a new line at the start of the message?
#' @param slack Should this also be posted to slack?
#' @export msg
msg <- function(txt, type = "msg", syscalls_depth = 0, newLine = FALSE, slack = FALSE) {
  if (slack & config$is_production) slack(glue::glue("{Sys.time()}: {txt}"))

  # make warnings print immediately
  op <- options("warn")
  on.exit(options(op))
  options(warn = 1)

  if (syscalls_depth < 0) stop("syscalls_depth cannot be less than zero")
  if (!type %in% c("msg", "warn", "err")) stop(sprintf("%s not msg, warn, err", type))

  startOfLine <- ""
  if (newLine) startOfLine <- "\r\n"

  fn <- switch(type,
    msg = base::message,
    warn = base::warning,
    err = base::stop
  )

  depth <- sys.nframe() - 1
  x <- sys.calls()
  if (depth >= 1 & syscalls_depth > 0) {
    depthSeq <- depth:1
    if (length(depthSeq) > syscalls_depth) depthSeq <- depthSeq[1:syscalls_depth]
    depthSeq <- rev(depthSeq)
    for (i in depthSeq) {
      base::message(startOfLine, "           ", depth - i + 1, "/", depth, ": ", deparse(x[[i]]))
    }
  }

  if (type == "msg") {
    if (config$is_initialized) {
      fn(sprintf("%s%s/%s/%s %s\r", startOfLine, Sys.time(), config$name_computer, config$package, txt))
    } else {
      fn(sprintf("%s%s %s\r", startOfLine, Sys.time(), txt))
    }
  } else {
    if (config$is_initialized) {
      fn(sprintf("%s%s/%s/%s %s\r", startOfLine, Sys.time(), config$name_computer, config$package, txt), call. = F)
    } else {
      fn(sprintf("%s%s %s\r", startOfLine, Sys.time(), txt), call. = F)
    }
  }
}
