
#' Connect to a Rundeck server
#' @param url TODO
#' @param version TODO
#' @param user TODO
#' @param token TODO
#' @import httr
#' @export
rundeck <- function(url = "http://localhost", version = 31, user = NULL,
    token = NULL, contextPath = c()) {
  
  if (!any(parse_url(url)$scheme == c("https", "http"))) {
    warning("Unsupported scheme")
  }
  
  if (is.null(user) || is.null(token)) stop("missing credentials")
  
  structure(
      list(host = url,
          user = user,
          contextPath = contextPath,
          version = version,
          token = token),
      class = c("Rundeck", "list"))
  
}

requireAPIVersion <- function(rundeck, min) {
  if (rundeck$version < min) {
    stop(sprintf("Unsupported API version: at minimum version %s is required", min))
  }
}

#' @param ... further arguments; not used
#' @export
print.Rundeck <- function(x, ...) {
  
  cat(sprintf("<rundeck server with url: %s>\n", x$host))
  
}

#' Rundeck GET API Request
#' @description Execute a GET API request against Rundeck
#' @param rundeck see \code{\link{rundeck}}
#' @param path TODO
#' @importFrom httr GET stop_for_status content
#' @export
rundeckGET <- function(rundeck, path, ...) {
  
  url = modify_url(rundeck$host, 
      path = c(rundeck$contextPath, "api", rundeck$version, path))
  
  response <- GET(
      url,
      add_headers("X-Rundeck-Auth-Token" = rundeck$token),
      accept_json(),
      ...)
  
  stop_for_status(response)
  
  content(response, type = "application/json", as = "parsed")
  
}

#' Rundeck POST API Request
#' @description Execute a POST API request against Rundeck
#' @param rundeck see \code{\link{rundeck}}
#' @param path TODO
#' @param body TODO
#' @importFrom httr POST stop_for_status content_type_json
#' @importFrom jsonlite toJSON
#' @export
rundeckPOST <- function(rundeck, path, body, ...) {
  
  json <- toJSON(body, auto_unbox = TRUE)
  
  url <- modify_url(rundeck$host, 
      path = c(rundeck$contextPath, "api", rundeck$version, path))
  
  response <- POST(
      url = url,
      body = json,
      content_type_json(),
      add_headers("X-Rundeck-Auth-Token" = rundeck$token),
      ...)
  
  stop_for_status(response)
  
  invisible(response)
  
}

#' Rundeck HEAD API Request
#' @description Execute a HEAD API request against rundeck
#' @param rundeck see \code{\link{rundeck}}
#' @param path TOOD
#' @param ... further arguments to \code{\link[httr]{HEAD}}
#' @export
rundeckHEAD <- function(rundeck, path, ...) {
  
  url <- modify_url(rundeck$host, 
      path = c(rundeck$contextPath, "api", rundeck$version, path))
  
  response <- HEAD(
      url = url,
      add_headers("X-Rundeck-Auth-Token" = rundeck$token),
      ...)
  
  warn_for_status(response)
  
  list(
      headers = headers(response),
      status = status_code(response)
  )

}



#' Rundeck - Listing Projects
#' @description List the existing projects on the server.
#' @param rundeck object of class \code{Rundeck}; see \code{\link{rundeck}}
#' @return vector of projects
#' @export
listProjects <- function(rundeck, ...) {
  
  do.call(rbind,
      lapply(rundeckGET(rundeck, "projects", ...),
          function(x) as.data.frame(x[c("name", "description", "label")],
                stringsAsFactors = FALSE)))
  
}

#' Rundeck - Project Creation
#' @description Create a new project.
#' @param rundeck object of class \code{Rundeck}; see \code{\link{rundeck}}
#' @param name project name
#' @param config project definition
#' @importFrom jsonlite unbox
#' @return nothing
#' @export
createProject <- function(
    rundeck,
    name,
    config = c(),
    ...) {
  
  rundeckPOST(rundeck,
      path = c(rundeck$contextPath, "projects"),
      body = list(name = unbox(name), config = config),
      ...)
  
  RundeckProject(rundeck, name)
  
}

#' Rundeck - Get A Project
#' @param rundeck object of class \code{Rundeck}; see \code{\link{rundeck}}
#' @param name project name
#' @export
getProject <- function(rundeck, name, ...) {
  
  if (!(name %in% listProjects(rundeck, ...)$name))
    stop(sprintf("Project does not exist: %s", name))
  
  RundeckProject(rundeck, name)
  
}

