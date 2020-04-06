# library(srhoads)

if(DOTHISSTUFF <- F){
  RFs <- readLines("R/functions.R")
  RFs2 <- RFs %>% stringr::str_extract_all(., ".*::")
  RFs3 <- RFs2 %>% unlist() %>% unique() %>%
    stringr::word(., -1) %>% unique() %>%
    gsub("::", "", .) %>%
    gsub(".*[[:punct:]]", "", .) %>% unique()
  
  DESCRs <- readLines("DESCRIPTION") %>% setNames(names(.) <- .) %>% as.list()
  STARTLINE <- DESCRs %>% grep("Imports:", .) + 1
  ENDLINE <- DESCRs %>% grep("License:", .) - 1
  (PkgsAlreadyInDESCR <- DESCRs[STARTLINE:ENDLINE] %>% names() %>% gsub("[^[:alnum:]]", "", .))
  
  setdiff(RFs3, tidyverse::tidyverse_packages()) %>%
    setdiff(., PkgsAlreadyInDESCR) %>%
    paste0(., ",\n") %>%
    catn()
}

# devtools,
# qdapRegex,
# readxl,
# feather,
# tools,
# stringi,
# plyr,
# quanteda,
# text2vec,
# glmnet,
# DT,
# janitor,
# glue,
# ggmap






##-----------------------------------------------------------------------##
# WHAT HAPPENS WHEN I INSTALL MY PACKAGE NOW THAT I HAVE DEPENDENCIES ADDED
##-----------------------------------------------------------------------##

# >   devtools::install_github('srhoads/srhoads')
# Downloading GitHub repo srhoads/srhoads@master
# These packages have more recent versions available.
# Which would you like to update?
#   
# 1: All                                      
# 2: CRAN packages only                       
# 3: None                                     
# 4: tidyverse (1.2.1     -> 1.3.0    ) [CRAN]
# 5: quanteda  (1.5.1     -> 1.5.2    ) [CRAN]
# 6: glmnet    (2.0-18    -> 3.0-1    ) [CRAN]
# 7: DT        (0.9       -> 0.10     ) [CRAN]
# 8: ggmap     (2.6.1     -> 3.0.0    ) [CRAN]
# 9: haven     (2.1.1     -> 2.2.0    ) [CRAN]
# 10: hms       (0.5.1     -> 0.5.2    ) [CRAN]
# 11: covr      (3.3.2     -> 3.4.0    ) [CRAN]
# 12: roxygen2  (6.1.1     -> 7.0.2    ) [CRAN]
# 13: rversions (2.0.0     -> 2.0.1    ) [CRAN]
# 14: testthat  (2.2.1     -> 2.3.1    ) [CRAN]
# 15: network   (1.15      -> 1.16.0   ) [CRAN]
# 16: RSpectra  (0.15-0    -> 0.16-0   ) [CRAN]
# 17: R6        (2.4.0     -> 2.4.1    ) [CRAN]
# 18: jpeg      (0.1-8     -> 0.1-8.1  ) [CRAN]
# 19: scales    (1.0.0     -> 1.1.0    ) [CRAN]
# 20: farver    (1.1.0     -> 2.0.1    ) [CRAN]
# 21: curl      (4.2       -> 4.3      ) [CRAN]
# 22: rmarkdown (1.16      -> 1.18     ) [CRAN]
# 23: knitr     (1.25      -> 1.26     ) [CRAN]
# 24: tinytex   (0.16      -> 0.17     ) [CRAN]
# 25: xfun      (0.10      -> 0.11     ) [CRAN]
# 26: selectr   (0.4-1     -> 0.4-2    ) [CRAN]
# 27: RcppEigen (0.3.3.5.0 -> 0.3.3.7.0) [CRAN]
# 
# Enter one or more numbers, or an empty line to skip updates:
# 1
# 
# Installing 25 packages: tidyverse, quanteda, glmnet, DT, ggmap, haven, hms, covr, roxygen2, rversions, testthat, network, RSpectra, R6, shape, jpeg, scales, farver, curl, rmarkdown, knitr, tinytex, xfun, selectr, RcppEigen
# Installing packages into ‘/Users/srhoads/Library/R/3.6/library’
# (as ‘lib’ is unspecified)
# 
# There are binary versions available but the source versions are later:
#   binary source needs_compilation
# roxygen2   7.0.1  7.0.2              TRUE
# rversions  2.0.0  2.0.1             FALSE
# curl         4.2    4.3              TRUE
# 
# Do you want to install from sources the packages which need compilation? (Yes/no/cancel) n
#
# ...........
# ...........
#
# The downloaded binary packages are in
# /var/folders/wf/g7fd247s63zgv2t032n2kfc40000gn/T//RtmpOYa3iP/downloaded_packages
# installing the source package ‘rversions’
# 
# trying URL 'https://cran.rstudio.com/src/contrib/rversions_2.0.1.tar.gz'
# Content type 'application/x-gzip' length 41476 bytes (40 KB)
# ==================================================
#   downloaded 40 KB
# 
# * installing *source* package ‘rversions’ ...
# ** package ‘rversions’ successfully unpacked and MD5 sums checked
# ** using staged installation
# ** R
# ** inst
# ** byte-compile and prepare package for lazy loading
# ** help
# *** installing help indices
# *** copying figures
# ** building package indices
# ** testing if installed package can be loaded from temporary location
# ** testing if installed package can be loaded from final location
# ** testing if installed package keeps a record of temporary installation path
# * DONE (rversions)
# 
# The downloaded source packages are in
# ‘/private/var/folders/wf/g7fd247s63zgv2t032n2kfc40000gn/T/RtmpOYa3iP/downloaded_packages’
# ✔  checking for file ‘/private/var/folders/wf/g7fd247s63zgv2t032n2kfc40000gn/T/RtmpOYa3iP/remotes323a713af5ef/srhoads-srhoads-604ab60/DESCRIPTION’ ...
# ─  preparing ‘srhoads’: (337ms)
# ✔  checking DESCRIPTION meta-information ...
# ─  checking for LF line-endings in source and make files and shell scripts
# ─  checking for empty or unneeded directories
# ─  building ‘srhoads_0.0.0.9000.tar.gz’
#
# Installing package into ‘/Users/srhoads/Library/R/3.6/library’
# (as ‘lib’ is unspecified)
# * installing *source* package ‘srhoads’ ...
# ** using staged installation
# ** R
# ** byte-compile and prepare package for lazy loading
# tidyverse dependency imported
# devtools dependency imported
# [1] "yey u loaded sam's fxns!"
# 
# devtools,
# qdapRegex,
# feather,
# tools,
# stringi,
# plyr,
# quanteda,
# text2vec,
# glmnet,
# DT,
# janitor,
# glue,
# ggmap,
# 
# tidyverse dependency imported
# devtools dependency imported
# [1] "yey u loaded sam's fxns!"
# [1] "yey u loaded sam's fxns!"
# ** help
# *** installing help indices
# ** building package indices
# ** testing if installed package can be loaded from temporary location
# ** testing if installed package can be loaded from final location
# ** testing if installed package keeps a record of temporary installation path
# * DONE (srhoads)
# > 




