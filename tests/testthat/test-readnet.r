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

  ans0 <- structure(
    list(
      `0` = structure(
        1:2, .Dim = 1:2, .Dimnames = list(NULL, c("ego", "alter")), labels = c("a", "b", "c")
        ),
      `1` = structure(
        c(1L, 2L, 1L, 2L, 1L, 3L), .Dim = c(3L, 2L), .Dimnames = list(NULL, c("ego", "alter")),
        labels = c("a", "b", "c"))
      ),
    .Names = c("0", "1")
    )

  ans <- survey_to_edgelist(rbind(dat0, dat1), "ego", c("alter1", "alter2"), time = "time")
  expect_equal(ans, ans0)
})
