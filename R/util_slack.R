s_webhook <- function() {
  Sys.getenv("SLACK_WEBHOOK", "X")
}

#' slack
#' @param txt a
#' @export
slack <- function(txt) {
  httr::POST(
    url = s_webhook(),
    encode = "json",
    body = list(
      text = txt
    )
  )
}
