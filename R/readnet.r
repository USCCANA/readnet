#' This function takes a two-column matrix, generates factors and encodeds it
#' into an integer matrix with an extra attribute: `labels`
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
makegroups <- function(dat, group.id, sep) {
  apply(dat[, group.id, drop=FALSE], 1, paste0, collapse=sep)
}

#' This function coerces a set of variables in a data frame into character
#' so that when that is turned into a factor, information is not loss.
set_as_char <- function(dat, vars) {

  dat <- as.character(match.call()$dat)
  env <- parent.frame()

  for (v in vars)
    if (!is.character(env[[dat]][[v]]))
      env[[dat]][[v]] <- as.character(env[[dat]][[v]])
}

#' Creates an edgelist (or a list of edgelist) from survey data
#'
#' The function grabs ego ids and its nominations and creates an edgelist
#' in which the labels (ids) are recoded such that ids go from 1 to n.
#'
#' @param dat `data.frame`.
#' @param ego.id Character scalar.
#' @param alter.id Character vector.
#' @param group.id Character vector.
#' @param time Character vector.
#' @param group.sep Character scalar.
#' @param time.sep Character scalar.
#'
#' @return If no `group.id` or `time` are provided, an integer matrix with
#' two named columns (_ego_ and _alter_), and an attribute "`labels`". Otherwise
#' will return a nested list in which the first level is given by `group.id`,
#' and the second level by `time` (if provided).
#'
#' @examples
#'
#' 1+1
#' @export
survey_to_edgelist <- function(
  dat,
  ego.id,
  alter.id,
  group.id  = NULL,
  time      = NULL,
  group.sep = "-",
  time.sep  = "-"
  ) {

  # Setting nasty option that I don't want to use!
  old_strasf <- getOption("stringsAsFactors")
  on.exit(options(stringsAsFactors = old_strasf))
  options(stringsAsFactors = FALSE)

  # Making sure everything is character
  set_as_char(dat, c(ego.id, alter.id, group.id, time))

  # Checking if group was specified, then call it recursively
  if (length(group.id)) {

    # Group tags
    group.id <- makegroups(dat, group.id, group.sep)

    # Calling the function recursively
    return(
      lapply(
        split.data.frame(dat, group.id),
        survey_to_edgelist,
        ego.id = ego.id, alter.id = alter.id, group.id = NULL, time = time)
      )
  }

  # Creates some containers
  n          <- nrow(dat)
  k          <- length(alter.id)
  ans        <- vector("list", k)
  names(ans) <- alter.id

  # Pastes the data
  for (id in alter.id) {
    ans[[id]] <- cbind(dat[[ego.id]], dat[[id]])
  }

  ans <- encode_mat(do.call(rbind, ans))
  labels <- attr(ans, "labels")

  # If time was provided
  if (length(time)) {
    time <- makegroups(dat, time, time.sep)
    ans  <- split.data.frame(ans, time)
    ans  <- lapply(ans, function(a)
      structure(a[complete.cases(a), ,drop=FALSE], labels=labels)
      )
  } else {
    ans <- structure(ans[complete.cases(ans), ,drop=FALSE], labels=labels)
  }

  ans

}
