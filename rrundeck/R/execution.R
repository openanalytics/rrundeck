
RundeckExecution <- function(rundeck, id) {
  
  structure(
      list(rundeck = rundeck,
          id = id),
      class = c("RundeckExecution", "list"))
  
}

#' @importFrom httr modify_url
#' @export
print.RundeckExecution <- function(x, ...) {
  
  cat(sprintf("<rundeck execution with url: %s>\n",
          modify_url(x$rundeck$host, path = c("execution", "show", x$id))))
  
}

#' @importFrom httr modify_url
#' @export
browse.RundeckExecution <- function(x, ...) {
  
  browseURL(modify_url(x$rundeck$host, path = c("execution", "show", x$id)), ...)
  
}

#' @export
summary.RundeckExecution <- function(x, ...) {
  
  info <- rundeckGET(x$rundeck, path = c("execution", x$id))
  
  cat(sprintf("Execution:\t%s\n", info$id))
  cat(sprintf("Job:\t\t%s [%s]\n", info$job$name, info$job$id))
  cat(sprintf("Status:\t\t%s\n", info$status))
  
  cat("\n")
  
}

#' Retrieve Rundeck Execution Output Log
#' @param exec execution
#' @param follow make repeated requests to follow the output of an execution
#' @param verbose print every log entry received and a summary at the end
#' @param raw return content as returned by API instead of a formatted log
#' @return all received log entries as a single \code{character()} (invisible)
#' @export
getExecutionLog <- function(
    exec,
    follow = TRUE,
    verbose = follow,
    offset = 0L,
    maxLines = 100L,
    raw = FALSE,
    ...) {
  
  output <- rundeckGET(exec$rundeck, path = c("execution", exec$id, "output"),
      query = list(offset = 0, maxlines = maxLines))
  
  if (raw) return(output)
  
  formatLogEntry <- function(entry) {
    fmtEntry <- sprintf("%s | %.10s | %s", entry$time, entry$node, entry$log)
    if (verbose) cat(fmtEntry, "\n")
    fmtEntry
  }
  
  fmtEntries <- sapply(output$entries, formatLogEntry)
   
  if (follow) {
    while (!output$execCompleted) {
      output <- rundeckGET(exec$rundeck, path = c("execution", exec$id, "output"),
          query = list(offset = output$offset, maxlines = maxLines))
      
      fmtEntries <- c(fmtEntries, sapply(output$entries, formatLogEntry))
    }
  }
  
  if (verbose) {
    cat(sprintf("Duration:\t%f ms\n", output$execDuration))
    cat(sprintf("Status:\t\t%s\n", output$execState))
  }
  
  invisible(paste(collapse = "\n", fmtEntries))
  
}
