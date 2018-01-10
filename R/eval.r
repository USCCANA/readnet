library(magrittr)

new_graph <- function(.data) {
  env <- new.env()
  env[["DF"]] <- .data
  env
}

getval <- function(x) {
  as.character(substitute(x, parent.frame()))
}

set_variable <- function(.data, var, name) {

  # Adds the variable to the list... if it exists
  .data[[name]] <- c(
    .data[[name]],
    var
  )

  # Checks whether the variable exists
  if (!length(.data$DF[[.data[[name]]]]))
    stop("The variable `", .data[[name]], "` does not exists.", call. = FALSE)

  .data
}

set_group <- function(.data, var) {
  set_variable(.data, getval(var), "group")
}

set_time <- function(.data, var) {
  set_variable(.data, getval(var), "time")
}

get_edgelist <- function(.data, ego, ...) {

  set_variable(.data, ego, "ego")

  for (alter in list(...)) {
    set_variable(.data, getval(var), "alter")
  }

  .data
}

set_edgelist <- function(.data, edgelist) {

}

# dat0 <- data.frame(
#   ego    = c("a", "b", "c"),
#   alter1 = c("b", NA, NA),
#   alter2 = NA,
#   time   = 0
# )
#
# dat1 <- data.frame(
#   ego    = c("a", "b", "c"),
#   alter1 = c("b", "a", NA),
#   alter2 = c("c", NA, NA),
#   time   = 1
# )
#
# dat <- rbind(dat0, dat1)
#
# ans <- dat %>%
#   new_graph %>%
#   set_group(time) %>%
#   get_edgelist(ego, alter1, alter2)
