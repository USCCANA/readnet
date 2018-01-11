
#' Creates an adjacency matrix from an edgelist with vertex attributes
#'
#' The only difference with [igraph::graph_from_data_frame] is that
#' this returns an 0/1 adjacency matrix and sorts whatever attributes were
#' passed via `data` accordingly to the network coding.
#'
#' @param edgelist A data.frame. Only the first two columns will be used.
#' @param data A data.frame with vertex attributes.
#' @param data.idvar Character scalar. Name of the variable that holds the
#' vertex id (label). By default it uses the first column.
#'
#' @return A list with the following
#' \item{adjmat}{A matrix of \eqn{n\times n}.}
#' \item{data}{A sorted version of `data` according to the network coding.}
#'
#' @examples
#' # Simple example
#' net <- c("a", "b", "a", "c", "b", "c", "c", "b", "c", "a")
#' net <- matrix(net, byrow = TRUE, ncol=2)
#'
#' dat <- data.frame(name = c("d", "a", "c", "b"), age = c(4, 1, 3, 2))
#' ans0 <- edgelist_to_adjmat_w_attributes(net, dat)
#' @name edgelist_to_adjmat_w_attributes
NULL

#' Basically, this function replicates a dataframe so it
#' @noRd
time_check_and_fill <- function() {

  # We will be working with the data of the parent function
  env <- parent.frame()

  # Finding time ---------------------------------------------------------------
  times.e <- if (length(env$edgelist.timevar)) {
    unique(env[["edgelist"]][[env$edgelist.timevar]])
  } else {
    NULL
  }

  times.d <- if (length(env$data.timevar)) {
    unique(env[["data"]][[env$data.timevar]])
  } else {
    NULL
  }
  times <- sort(unique(c(times.e, times.d)))

  # If defined All times should be present -------------------------------------

  # In the edgelist
  test <- times[which(!(times %in% times.e))]
  if (length(times.e) && length(test))
    stop("Some time periods in `data` are not present in `edgelist`: ",
         paste(test, collapse=", "), ".")

  # In the data
  test <- times[which(!(times %in% times.d))]
  if (length(times.d) && length(test))
    stop("Some time periods in `edgelist` are not present in `data`: ",
         paste(test, collapse=", "), ".")

  # If no time is specified for one, then fill the gaps ------------------------

  # For the edgelist
  if (!length(env[["edgelist.timevar"]])) {
    env[["edgelist"]] <- do.call(
      rbind,
      lapply(times, function(i) cbind(env[["edgelist"]], time = i))
      )

    env[["edgelist.timevar"]] <- "time"
  }

  # For the data
  if (!length(env[["data.timevar"]])) {
    env[["data"]] <- do.call(
      rbind,
      lapply(times, function(i) cbind(env[["data"]], time = i))
    )

    env[["data.timevar"]] <- "time"
  }

}

#' @rdname edgelist_to_adjmat_w_attributes
#' @export
edgelist_to_adjmat_w_attributes <- function(
  edgelist,
  data,
  data.idvar       = colnames(data)[1],
  ids.to.keep      = NULL,
  data.timevar     = NULL,
  edgelist.timevar = NULL
) {

  # Here goes checks
  # 1. Both are data.frames
  # 2. edgelist has two columns
  # 3. attributes must be unique!

  edgelist <- as.data.frame(edgelist)
  data     <- as.data.frame(data)

  # Filtering ids
  if (length(ids.to.keep)) {

    edgelist <- edgelist[
      (edgelist[[1]] %in% ids.to.keep) & (edgelist[[2]] %in% ids.to.keep),
      ]

    data <- data[data[[data.idvar]] %in% ids.to.keep, ]

  }

  # Apply recursively if timevar are specified
  if (length(edgelist.timevar) | length(data.timevar)) {

    # Finding time
    times <- NULL
    time_check_and_fill()

    ans <- Map(
      function(e, d) {
        edgelist_to_adjmat_w_attributes(
          edgelist    = e,
          data        = d,
          data.idvar  = data.idvar
          )
        },
      e = split.data.frame(edgelist, edgelist[[edgelist.timevar]]),
      d = split.data.frame(data, data[[data.timevar]])
      )

    names(ans) <- times

    return(ans)


  }

  # Creating dictionary
  charedgelist <- as.vector(unlist(edgelist[,1:2]))
  dict <- sort(unique(c(
    charedgelist,
    as.character(data[[data.idvar]])
    )))

  n <- length(dict)

  # There should not be any duplicated data
  if (any(duplicated(data[[data.idvar]])))
    stop("`data` has duplicated observations.", call. = FALSE)

  # This all should match
  if (n != nrow(data))
    stop("Missing ids in `data`. There are ", nrow(data),
         " observations in `data` and ", n, " identified ids. ",
         "The observations that are missing in `data` are: '",
         paste(setdiff(dict, data[[data.idvar]]), collapse="', '"), "'.",
         call. = FALSE)

  # Encoding edgelist
  new_edgelist <- matrix(0L, nrow=nrow(edgelist), ncol=2)
  new_edgelist[] <- (1:n)[match(charedgelist, dict)]

  # Encoding data (sorting)
  data <- data[order(match(data[[data.idvar]], dict)),]

  # Returning

  adjmat <- matrix(0, nrow=n, ncol=n, dimnames = list(dict, dict))
  adjmat[new_edgelist] <- 1L
  list(
    adjmat = adjmat,
    data   = data
  )

}
