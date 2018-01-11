rm(list = ls())

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
