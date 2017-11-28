readnet
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
The goal of readnet is to provide tools for preprocessing network data (a companion to igraph, statnet, netdiffuseR, etc.)

Some of the features:

-   Automatically handle coding for nodes, time and groups.

-   If either `group.id` or `time` are specified, the function makes recursive calls, returning nested edgelists with the following structure:

        object
        --- group1:
          --- edgelist in time1
          --- ...
          --- edgelist in time n
        ...
        --- group n:
          --- edgelist in time1
          --- ...
          --- edgelist in time n

-   Includes functions to coerce the resulting edgelists into igraph objects and viceversa.

This project is on development and not published on CRAN yet.

Installation
------------

You can get `readnet` using devtools:

``` r
devtools::install_github("USCCANA/readnet")
```

Example
-------

This is a basic example which shows you how to solve a common problem. Suppose that you have a dataset that looks something like this:

``` r
dat
#>   ego alter1 alter2 time
#> 1   a      b   <NA>    0
#> 2   b   <NA>   <NA>    0
#> 3   c   <NA>   <NA>    0
#> 4   a      b      c    1
#> 5   b      a   <NA>    1
#> 6   c   <NA>   <NA>    1
```

This is, a survey with network nominations, and in this case, including a time variable. We can use the function `survey_to_edgelist` to process this data

``` r
library(readnet)
(ans <- survey_to_edgelist(dat, "ego", c("alter1", "alter2"), time = "time"))
#> $`0`
#> $edgelist
#>      ego alter
#> [1,]   1     2
#> 
#> $labels
#> [1] "a" "b" "c"
#> 
#> $data
#>   ego alter1 alter2 time
#> 1   a      b   <NA>    0
#> 2   b   <NA>   <NA>    0
#> 3   c   <NA>   <NA>    0
#> 
#> $graph.attrs
#> NULL
#> 
#> attr(,"class")
#> [1] "rn_edgelist"
#> 
#> $`1`
#> $edgelist
#>      ego alter
#> [1,]   1     2
#> [2,]   2     1
#> [3,]   1     3
#> 
#> $labels
#> [1] "a" "b" "c"
#> 
#> $data
#>   ego alter1 alter2 time
#> 4   a      b      c    1
#> 5   b      a   <NA>    1
#> 6   c   <NA>   <NA>    1
#> 
#> $graph.attrs
#> NULL
#> 
#> attr(,"class")
#> [1] "rn_edgelist"
```

The previous returned a list with elements of class `rn_edgelist`. Each element contains an edgelist, a vector of labels, and a dataframe with data. We can use the function `as_igraph` to turn this into an igraph object

``` r
as_igraph(ans)
#> $`0`
#> IGRAPH c8b4753 DN-- 3 1 -- 
#> + attr: name (v/c), ego (v/c), alter1 (v/c), alter2 (v/c), time
#> | (v/c)
#> + edge from c8b4753 (vertex names):
#> [1] a->b
#> 
#> $`1`
#> IGRAPH 9775947 DN-- 3 3 -- 
#> + attr: name (v/c), ego (v/c), alter1 (v/c), alter2 (v/c), time
#> | (v/c)
#> + edges from 9775947 (vertex names):
#> [1] a->b b->a a->c
```

and as a `rn_edgelist` back

``` r
as_rn_edgelist(as_igraph(ans))
#> $`0`
#> $edgelist
#>      ego alter
#> [1,]   1     2
#> 
#> $labels
#> [1] "a" "b" "c"
#> 
#> $data
#>   name ego alter1 alter2 time
#> a    a   a      b   <NA>    0
#> b    b   b   <NA>   <NA>    0
#> c    c   c   <NA>   <NA>    0
#> 
#> $graph.attrs
#> named list()
#> 
#> attr(,"class")
#> [1] "rn_edgelist"
#> 
#> $`1`
#> $edgelist
#>      ego alter
#> [1,]   1     2
#> [2,]   2     1
#> [3,]   1     3
#> 
#> $labels
#> [1] "a" "b" "c"
#> 
#> $data
#>   name ego alter1 alter2 time
#> a    a   a      b      c    1
#> b    b   b      a   <NA>    1
#> c    c   c   <NA>   <NA>    1
#> 
#> $graph.attrs
#> named list()
#> 
#> attr(,"class")
#> [1] "rn_edgelist"
```
