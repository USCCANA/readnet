#' Function to coerce an object to class `edgelist`
#' @noRd
new_rn_edgelist <- function(
  edgelist,
  labels,
  data,
  graph.attrs = NULL,
  ego.id = NULL,
  checks = c("edgelist", "labels", "data")
) {

  # Automatic coercion
  edgelist   <- unname(as.matrix(edgelist))
  edgelist[] <- as.integer(edgelist[])
  colnames(edgelist) <- c("ego", "alter")

  labels     <- unname(as.character(unlist(labels, recursive = TRUE)))

  data       <- as.data.frame(data)

  n <- length(labels)

  if ("labels" %in% checks)
    if (any(is.na(labels)))
      stop("Some `labels` are missing.", call. = FALSE)

  if ("data" %in% checks)
    if (nrow(data) != n)
      stop("The number of observations in `data`, ", nrow(data), ", does not",
           "  coincide with the number of labels extracted from the edgelist, ",
           n, ". Yo probably have duplicated observations in `data`.", call. = FALSE)


  if ("edgelist" %in% checks) {

    # Complete cases
    test <- which(!complete.cases(edgelist))
    if (length(test))
      stop("Some elements of the `edgelist` are missing.", call. = FALSE)

    # Pointing to the labels
    test <- range(edgelist)
    if (test[1] < 0 | test[2] > n)
      stop("Some elements of the `edgelist` have id values out of range (1 or n).",
           call. = FALSE)

  }

  # If to be sorted
  if (!length(ego.id))
    data <- data[match(labels, data[[ego.id]]), , drop=FALSE]


  # Creating the object
  structure(
    list(
      edgelist    = edgelist,
      labels      = labels,
      data        = data,
      graph.attrs = graph.attrs
    ),
    class = "rn_edgelist"
  )

}

#' Creates an edgelist (or a list of edgelist) from survey data
#'
#' The function grabs ego ids and its nominations and creates an edgelist
#' in which the labels (ids) are recoded such that ids go from 1 to n.
#'
#' @param data A `data.frame` with the data to be read in.
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
#' @aliases rn_edgelist
survey_to_edgelist <- function(
  data,
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
  set_as_char(data, c(ego.id, alter.id, group.id, time))

  # Checking if group was specified, then call it recursively
  if (length(group.id)) {

    # Group tags
    group.id <- makegroups(data, group.id, group.sep)

    # Calling the function recursively
    return(
      lapply(
        split.data.frame(data, group.id),
        survey_to_edgelist,
        ego.id = ego.id, alter.id = alter.id, group.id = NULL, time = time)
      )
  }

  # Creates some containers
  n          <- nrow(data)
  k          <- length(alter.id)
  ans        <- vector("list", k)
  names(ans) <- alter.id

  # Pastes the data
  for (id in alter.id) {
    ans[[id]] <- cbind(data[[ego.id]], data[[id]])
  }

  ans <- encode_mat(do.call(rbind, ans))
  labels <- attr(ans, "labels")

  # If time was provided
  if (length(time)) {
    time <- makegroups(data, time, time.sep)
    ans  <- split.data.frame(ans, time)
    data  <- split.data.frame(data, time)

    # Mapping the function
    ans  <- Map(function(e, d) {

      new_rn_edgelist(
        edgelist = e[stats::complete.cases(e),,drop=FALSE],
        labels   = labels,
        data     = d,
        ego.id   = ego.id,
        checks   = "data"
      )

    }, e = ans, d = data)

  } else {

    ans <- new_rn_edgelist(
      edgelist = ans[stats::complete.cases(ans),,drop=FALSE],
      labels   = labels,
      data     = data,
      ego.id   = ego.id,
      checks   = "data"
    )

  }

  # Returning including the data
  ans
}

#' Coerce objects `rn_edgelist` to `igraph`
#' @param x An object of class `rn_edgelist` or a list of that.
#' @param ... Further arguments passed to [`graph_from_data_frame`][igraph::graph_from_data_frame()]
#' from the \pkg{igraph} R package.
#' @export
as_igraph <- function(x, ...)
  UseMethod("as_igraph")

#' @export
as_igraph.list <- function(x, ...) {
  lapply(x, as_igraph, ...)
}

#' @export
as_igraph.rn_edgelist <- function(x, ...) {

  # Creating the vertex matrix
  vertices <- cbind(
    data.frame(name = x$labels),
    x$data
  )

  # Relabeling the edgelist...
  edgelist   <- x$edgelist
  edgelist[] <- x$labels[edgelist[]]

  # Putting all together
  ans <- igraph::graph_from_data_frame(edgelist, vertices = vertices, ...)

  for (a in names(x$graph.attr))
    igraph::graph_attr(ans, a) <- x$graph.attr[[a]]

  ans

}

#' Coerce different graph objects to `rn_edgelist`
#' @param x An object to be coerced to [rn_edgelist]
#' @export
as_rn_edgelist <- function(x) UseMethod("as_rn_edgelist")

#' @export
as_rn_edgelist.list <- function(x)
  lapply(x, as_rn_edgelist)

#' @export
as_rn_edgelist.igraph <- function(x) {

  labels <- igraph::vertex_attr(x, "name")
  if (!length(labels))
    labels <- as.character(1L:igraph::vcount(x))

  new_rn_edgelist(
    edgelist = igraph::as_edgelist(x, names = FALSE),
    labels   = labels,
    data     = igraph::as_data_frame(x, "vertices"),
    graph.attrs = igraph::graph_attr(x),
    ego.id   = "name"
  )


}
