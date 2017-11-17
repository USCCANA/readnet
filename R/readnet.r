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

#' Creates an edgelist (or a list of edgelist) from survey data
#'
#' The function grabs ego ids and its nominations and creates an edgelist
#' in which the labels (ids) are recoded such that ids go from 1 to n.
#'
#' @param dat A `data.frame` with the data to be read in.
#' @param ego.id,alter.id Character scalars. Names of the variables in `dat` that
#' correspond to the ids of ego and alter. It does not need to be numeric
#' (see details).
#' @param group.id Character vector.
#' @param time Character vector.
#' @param group.sep Character scalar.
#' @param time.sep Character scalar.
#'
#' @details
#' By default, `ego.id` and `alter.id` are transform to factor variables, so the
#' user doesn't need to worry about giving individuals in the survey a special
#' coding
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
    dat  <- split.data.frame(dat, time)

    # Mapping the function
    ans  <- Map(function(e, d) {

      structure(
        list(
          edgelist = e[stats::complete.cases(e),,drop=FALSE],
          labels   = labels,
          data     = d[match(labels, d[[ego.id]]), , drop=FALSE]
        ), class = c("rn_edgelist")
        )
    }, e = ans, d = dat)

  } else {

    ans <- structure(
      list(
        edgelist = ans[stats::complete.cases(ans),,drop=FALSE],
        labels   = labels,
        data     = dat[match(labels, dat[[ego.id]]), , drop=FALSE]
      ), class = c("rn_edgelist")
    )

  }

  # Returning including the data
  ans
}

#' Turn a
#' @param x An object of class `rn_edgelist` or a list of that.
#' @param ... Further arguments passed to [graph_from_data_frame](igraph:graph_from_data_frame)
#' @export
rn_edgelist_to_igraph <- function(x, ...)
  UseMethod("rn_edgelist_to_igraph")

#' @export
rn_edgelist_to_igraph.list <- function(x, ...) {
  lapply(x, rn_edgelist_to_igraph, ...)
}

#' @export
rn_edgelist_to_igraph.rn_edgelist <- function(x, ...) {

  # Creating the vertex matrix
  vertices <- cbind(
    data.frame(name = x$labels),
    x$data
  )

  # Relabeling the edgelist...
  edgelist   <- x$edgelist
  edgelist[] <- x$labels[edgelist[]]

  # Putting all together
  igraph::graph_from_data_frame(edgelist, vertices = vertices, ...)

}
