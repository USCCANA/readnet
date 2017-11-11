readnet
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
The goal of readnet is to provide tools for preprocessing network data (a companion to igraph, statnet, netdiffuseR, etc.)

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r

library(readnet)

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

dat <- rbind(dat0, dat1)
dat
#>   ego alter1 alter2 time
#> 1   a      b   <NA>    0
#> 2   b   <NA>   <NA>    0
#> 3   c   <NA>   <NA>    0
#> 4   a      b      c    1
#> 5   b      a   <NA>    1
#> 6   c   <NA>   <NA>    1

survey_to_edgelist(dat, "ego", c("alter1", "alter2"), time = "time")
#> $`0`
#>      ego alter
#> [1,]   1     2
#> attr(,"labels")
#> [1] "a" "b" "c"
#> 
#> $`1`
#>      ego alter
#> [1,]   1     2
#> [2,]   2     1
#> [3,]   1     3
#> attr(,"labels")
#> [1] "a" "b" "c"
```
