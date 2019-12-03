RFs <- readLines("R/functions.R")
RFs2 <- RFs %>% stringr::str_extract_all(., ".*::")
RFs3 <- RFs2 %>% unlist() %>% unique() %>%
  word(., -1) %>% unique() %>% 
  gsub("::", "", .) %>% 
  gsub(".*[[:punct:]]", "", .) %>% unique()
setdiff(RFs3, tidyverse_packages()) %>%
  paste0(., ",\n") %>%
  catn()
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