#' This function takes a two-column matrix, generates factors and encodeds it
#' into an integer matrix with an extra attribute: `labels`
#' @noRd
encode_mat <- function(x) {

  x      <- as.factor(c(x[, 1L], x[, 2L]))
  labels <- levels(x)

  x <- matrix(as.integer(x), ncol=2, byrow = FALSE,
              dimnames = list(NULL, c("ego", "alter")))
  attr(x, "labels") <- labels

  x

}

#' This function takes a set of vectors and creates a unique id using all
#' by pasting it.
#' @noRd
makegroups <- function(dat, group.id, sep) {
  apply(dat[, group.id, drop=FALSE], 1, paste0, collapse=sep)
}

#' This function coerces a set of variables in a data frame into character
#' so that when that is turned into a factor, information is not loss.
#' @noRd
set_as_char <- function(dat, vars) {

  dat <- as.character(match.call()$dat)
  env <- parent.frame()

  for (v in vars)
    if (!is.character(env[[dat]][[v]]))
      env[[dat]][[v]] <- as.character(env[[dat]][[v]])
}
