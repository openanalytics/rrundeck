
RundeckProject <- function(rundeck, name) {
  
  structure(
      list(rundeck = rundeck,
          name = name),
      class = c("RundeckProject", "list"))
  
}

#' @importFrom httr modify_url
#' @export
print.RundeckProject <- function(x, ...) {
  
  cat(sprintf("<rundeck project with url: %s>\n",
          modify_url(x$rundeck$host, path = c("project", x$name))))
  
}

#' @importFrom httr modify_url
#' @export
browse.RundeckJob <- function(x, ...) {
  
  browseURL(modify_url(x$rundeck$host, path = c("project", x$name)), ...)
  
}

#' @export
#' @seealso https://docs.rundeck.com/docs/api/#listing-jobs
listJobs.RundeckProject <- function(
    project,
    idList = NULL,
    groupPath = NULL,
    jobFilter = NULL,
    jobExactFilter = NULL,
    groupPathExact = NULL,
    scheduledFilter = NULL,
    serverNodeUUIDFilter = NULL,
    ...) {
  
  requireAPIVersion(project$rundeck, min = 14)
  
  results <- c("name", "description", "id")
  
  do.call(rbind,
      lapply(rundeckGET(project$rundeck,
              path = c("project", project$name, "jobs"),
              query = c(
                  if (!is.null(idList)) list(idlist = idList),
                  if (!is.null(groupPath)) list(groupPath = groupPath),
                  if (!is.null(jobFilter)) list(jobFilter = jobFilter),
                  if (!is.null(jobExactFilter)) list(jobExactFilter = jobExactFilter),
                  if (!is.null(groupPathExact)) list(groupPathExact = groupPathExact),
                  if (!is.null(scheduledFilter)) list(scheduledFilter = ifelse(scheduledFilter, "true", "false")),
                  if (!is.null(serverNodeUUIDFilter)) list(serverNodeUUIDFilter = serverNodeUUIDFilter)
              ),
              ...),
          function(x) as.data.frame(x[results], stringsAsFactors = FALSE)))
  
}

#' @export
getJob.RundeckProject <- function(project, id = NULL, name = NULL, ...) {
  
  matches <- listJobs(project, jobExactFilter = name, idList = id)$id
  
  if (length(matches) == 0)
    stop("No matching jobs found.")
  
  if (length(matches) > 1)
    warning("More than one matching job found: returning the first one.")
  
  RundeckJob(project$rundeck, matches[1])
  
}

#' @export
hasJob.RundeckProject <- function(project, id, ...) {
  
  rundeckHEAD(project$rundeck, c("project", project$name, "job", id))$status == 200
  
}
