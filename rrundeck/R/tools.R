
mbapply <- function(X, FUN) {
  ifNotNULL(X, FUN(X))
}

ifNotNULL <- function(a, b) {
  if (!is.null(a)) b else NULL
}
