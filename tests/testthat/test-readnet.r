context("readnet.r")

test_that("Basic test", {

  dat0 <- data.frame(
    ego    = c("a", "b", "c"),
    alter1 = c("b", NA, NA),
    alter2 = NA,
    time   = 0
  )

  dat1 <- data.frame(
    ego    = c("a", "b", "c"),
    alter1 = c("b", "a", NA),
    alter2 = c("c", NA, NA),
    time   = 1
    )


  ans0 <- structure(list(
    `0` = structure(
      list(
        edgelist = structure(1:2, .Dim = 1:2, .Dimnames = list(NULL, c("ego", "alter"))),
        labels = c("a", "b", "c"),
        data = structure(
          list(
            ego = c("a", "b", "c"),
            alter1 = c("b", NA, NA),
            alter2 = c(NA_character_,
                       NA_character_, NA_character_),
            time = c("0", "0", "0")
          ),
          .Names = c("ego",
                     "alter1", "alter2", "time"),
          row.names = c(NA, 3L),
          class = "data.frame"
        ),
        graph.attrs=NULL
      ),
      class = "rn_edgelist"
    ),
    `1` = structure(
      list(
        edgelist = structure(
          c(1L, 2L, 1L, 2L, 1L, 3L),
          .Dim = c(3L,
                   2L),
          .Dimnames = list(NULL, c("ego", "alter"))
        ),
        labels = c("a",
                   "b", "c"),
        data = structure(
          list(
            ego = c("a", "b", "c"),
            alter1 = c("b", "a", NA),
            alter2 = c("c", NA, NA),
            time = c("1",
                     "1", "1")
          ),
          .Names = c("ego", "alter1", "alter2", "time"),
          row.names = 4:6,
          class = "data.frame"
        ),
        graph.attrs = NULL
      ),
      class = "rn_edgelist"
    )
  ), .Names = c("0", "1"))

  ans <- survey_to_edgelist(rbind(dat0, dat1), "ego", c("alter1", "alter2"), time = "time")
  expect_equal(ans, ans0)
})

test_that("igraph", {

  set.seed(12)
  ans0 <- igraph::barabasi.game(10)
  igraph::vertex_attr(ans0, "name") <- as.character(1L:10L)
  igraph::V(ans0)$x <- runif(10)

  ans1 <- as_igraph(as_rn_edgelist(ans0))

  expect_equal(igraph::as_adj(ans0), igraph::as_adj(ans1))
  expect_equal(igraph::graph_attr(ans0), igraph::graph_attr(ans1))
  expect_equal(igraph::edge_attr(ans0), igraph::edge_attr(ans1))
  expect_equal(igraph::vertex_attr(ans0), igraph::vertex_attr(ans1))

})
