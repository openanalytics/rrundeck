
RundeckJob <- function(rundeck, id) {
  
  structure(
      list(rundeck = rundeck,
          id = id),
      class = c("RundeckJob", "list"))
  
}

#' @export
RUNDECK_LOG_LEVELS <- c("INFO", "DEBUG","VERBOSE","WARN","ERROR")


#' @importFrom httr modify_url
#' @export
print.RundeckJob <- function(x, ...) {
  
  cat(sprintf("<rundeck job with url: %s>\n",
          modify_url(x$rundeck$host, path = c("job", "show", x$id))))
  
}

#' @importFrom httr modify_url
#' @export
browse.RundeckJob <- function(x, ...) {
  
  browseURL(modify_url(x$rundeck$host, path = c("job", "show", x$id)), ...)
  
}

#' Run Rundeck Job
#' @param options named list 
#' @param follow follow the output and print to console
#' @return object of class \code{RundeckExecution}
#' @importFrom httr content
#' @export
run <- function(job,
    options = list(),
    follow = FALSE,
    logLevel = RUNDECK_LOG_LEVELS,
    asUser = NULL,
    filter = NULL,
    runAtTime = NULL,
    ...) {
  
  #  if (job$rundeck$version < 18) stop("Unsupported API version (< 18).")
  
  requireAPIVersion(job$rundeck, min = 18)
  
  match.arg(logLevel, choices = RUNDECK_LOG_LEVELS)
  
  content <- c(
      list("loglevel" = logLevel, options = options),
      if (!is.null(asUser)) list("asUser" = asUser),
      if (!is.null(filter)) list("filter" = filter),
      if (!is.null(runAtTime)) list("runAtTime" = runAtTime)
  )

  response <- rundeckPOST(job$rundeck,
      path = c("job", job$id, "executions"),
      body = content,
      ...)
  
  exec <- RundeckExecution(job$rundeck, id = content(response)$id)
  
  message(sprintf("Execution [%s] for job [%s] scheduled.", exec$id, job$id))
  
  if (follow) getExecutionLog(exec, follow = follow) else exec
  
}
