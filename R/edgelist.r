#' Basic checks of the data passed.
#' @param data A data.frame
#' @noRd
check_data <- function(data, idvar, timevar = NULL) {

  # Is it a data.frame
  if (!is.data.frame(data))
    stop("`data` must be an object of class `data.frame`.", call. = FALSE)

  # Is it unique? --------------------------------------------------------------
  if (length(timevar)) {

    if (!length(data[[timevar]]))
      stop("The variable '", timevar, "' does not exists in `data`.", call. = FALSE)

    split(data[[timevar]], check_data, idvar = idvar)
  }

  if (!length(data[[idvar]]))
    stop("The variable '", idvar, "' does not exists in `data`.", call. = FALSE)

  if (any(duplicated(data[[idvar]])))
    stop("The panel has duplicated data.", call. = FALSE)

}

edgelist_to_adjmat <- function(
  edgelist,
  data     = NULL,
  ego.id   = NULL,
  group.id = NULL,
  timevar  = NULL
) {

  # Setting nasty option that I don't want to use!
  old_strasf <- getOption("stringsAsFactors")
  on.exit(options(stringsAsFactors = old_strasf))
  options(stringsAsFactors = FALSE)

  # Checks
  if (!did_i_call_myself("edgelist_to_adjmat")) {
    check_data(data, idvar, timevar)

    # Coercing into character
    set_as_char(data, c(idvar, timevar, ego.id, group.id, timevar))

    nodes <- data[[idvar]]
    nodes <- data.frame(id = 1L:length(node), label = nodes)

    # Are all the fellas of `edgelist` in `data`?
    test <- setdiff(as.vector(edgelist), nodes[["label"]])
    if (length(test))
      stop("The following nodes do not show in `data`:\n",
           paste(unique(test), collapse = ", "), ".", call. = FALSE)
  } else {
    env <- parent.frame()
    nodes <- env[["nodes"]]
  }

  if (length(timevar)) {
    Map(function(e, d) {
      edgelist_to_adjmat(e, d, ego.id = egp.id, group.id = group.id)
    },
    e = split.data.frame(data, data[[timevar]]),
    d = split.data.frame(data, data[[timevar]])
      ,
      edgelist_to_adjmat,

    )
  }

  # Recoding edgelist
  edgelist[, 1] <- nodes[match(edgelist[, 1], nodes[, 2]), 1]
  edgelist[, 2] <- nodes[match(edgelist[, 2], nodes[, 2]), 1]

  # Creating the adjmat
  adjmat <- matrix()


}

#' Returns `TRUE` if the function `fn` was called by itself (recursion)
#' @noRd
did_i_call_myself <- function(fn) {
  isrecur <- as.character(sys.call(-1)[[1]]) == fn
  if (length(isrecur) && isrecur)
    return(TRUE)
  else (FALSE)
}


