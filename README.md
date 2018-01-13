
<!-- README.md is generated from README.Rmd. Please edit that file -->
readnet
=======

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

Survey to Edgelist
------------------

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
#> IGRAPH 8103e75 DN-- 3 1 -- 
#> + attr: name (v/c), ego (v/c), alter1 (v/c), alter2 (v/c), time
#> | (v/c)
#> + edge from 8103e75 (vertex names):
#> [1] a->b
#> 
#> $`1`
#> IGRAPH d3b3db4 DN-- 3 3 -- 
#> + attr: name (v/c), ego (v/c), alter1 (v/c), alter2 (v/c), time
#> | (v/c)
#> + edges from d3b3db4 (vertex names):
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

Setting up data for Siena (or alike :))
---------------------------------------

Here is an example that I built for [Prof. Kayla de la Haye](http://profiles.sc-ctsi.org/kayla.delahaye/). The idea here is that we have an edgelist with (finite) timestamps, a dataset with node attributes, and a list of ids that we would like to include in the data. The final output looks like lists of adjacency matrices of *n* × *n* (so we coded the edgelist as 1 through `n`) and `data.frame` of the attributes matching the positions of nodes in the edgelist (you may be able to do something similar with igraph, but the key difference here is that this function allows you to include time and returns the attribute dataset sorted accordignly):

``` r
library(readnet)
library(dplyr)

edgelist <- readxl::read_excel("playground/kayla/GROW_Edgelist clean for George.xlsx")
dat      <- readr::read_tsv("playground/kayla/GROW Attribute Data for George.csv")
ids2work <- readxl::read_excel("playground/kayla/GROW_Edgelist clean for George.xlsx", "NetMembersN400")


# Catched an error:
# Error: Missing ids in `data`. There are 610 observations in `data` and 613
# identified ids. The observations that are missing in `data` are: '812398',
# '812660', '831086'.

# The same error shows up when I run it filtering the data:
#
#  Error: Missing ids in `data`. There are 394 observations in `data` and 397
# identified ids. The observations that are missing in `data` are: '812398',
# '812660', '831086'. So
#
# So I'll just drop it!

dids <- unique(dat$study_id)
edgelist <- filter(edgelist, (Sender %in% dids) & (Receiver %in% dids))

ans <- edgelist_to_adjmat_w_attributes(
  edgelist         = edgelist,
  data             = dat,
  data.idvar       = "study_id",
  edgelist.timevar = "wave",
  ids.to.keep      = ids2work$study_id
  )
```
