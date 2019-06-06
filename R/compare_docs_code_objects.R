# similar sorta doc in Colgate directory--called consolidate_0xxxxxxx.R
## FUNCTIONS.R ##########################################################################################################
rm(list = ls())
#goals.R
source("R/functions.R")
obj <- allobj <- as.list(.GlobalEnv)

#-------------
pkg("datapasta")

df_paste_ <- function(df){
  vector_paste((substitute(df)))
  df_paste(df)
}

df_paste_2 <- function(df){
  vector_paste(c("'____'", substitute(df), "'____'"))
  df_paste(df)
}

df_paste_lod <- function(lod){
  lapply(seq_along(lod), 
         function(y, n, i) {vector_paste(n[[i]]); df_paste(y[[i]])}, 
         y=lod, n=names(lod))
}

returnname <- function(z){
  mean.x<-mean(z$x)
  nm <-deparse(substitute(z))
  return(nm)
}

lsetdiff <- function(l1, l2) l1[!(l1 %in% l2)] # same as identical() i believe

lapply2 <- function(l, fxn) lapply(l, function(ll) lapply(ll, fxn))

filter_list <- filter_l <- function(l, is){
  l_is <- lapply(l, is)
  l[l_is==T]
}

collapse_obj_tostr <- function(x) x %>% 
  lapply(., function(x) x %>% unlist() %>% 
           capture.output() %>% paste0(., collapse="    \n    "))

#-------------

objnames <- obj %>% names()
objstr <- obj %>% collapse_obj_tostr()

# nonfxns <- lapply(obj, function(x) !is.function(x))
obj_nonfxns <- obj %>% filter_l(., function(x) !is.function(x))
obj_dfs <- obj %>% filter_l(., is.data.frame)


onedf <- obj_dfs[[1]]
# df_paste_2(onedf)

# df_paste_lod(obj_dfs)



# lapply(seq_along(obj_dfs), 
#        function(y, n, i) {vector_paste(n[[i]]); df_paste(y[[i]])}, 
#        y=obj_dfs, n=names(obj_dfs))
# 


# names(obj_nonfxns)
# obj_nonfxns %>% lapply(., df_paste)
# obj_dfs %>% lapply(., function(x) {vector_paste((returnname(x))); df_paste(x)})
# 
# for(i in 1:length(obj_dfs)){
#   {vector_paste(returnname(obj_dfs[i])); df_paste(obj_dfs[i])}
# }
# 
# for(i in 1:length(obj_dfs)){
#   {vector_paste(names(obj_dfs[1])); df_paste(obj_dfs[i])}
# }

# 
# what1 <- obj_dfs %>% lapply(., function(x) names(names(x[1])))
# what1 <- obj_dfs %>% lapply(., function(x) names(x))
# 
# (what1 <- obj_dfs %>% lapply(., function(x) names(x)))
# (what1 <- obj_dfs %>% lapply(., function(x) names(x) %>% names() %>% names() %>% unlist() %>% as.character() %>% names()))
# 
# (what1 <- obj_dfs %>% lapply(., function(x) names(x)[[1]]))
# (what1 <- obj_dfs %>% lapply(., function(x) {
#   fu <- names(x)[[1]]
#   fu[[1]] <- fu
#   fu
#   returnname(x)
# }))


# x <- list(a=11,b=12,c=13) # Changed to list to address concerns in commments
# lapply(seq_along(x), function(y, n, i) { paste(n[[i]], y[[i]]) }, y=x, n=names(x))

# lapply(seq_along(obj_dfs), function(y, n, i) paste(n[[i]]), y=obj_dfs, n=names(obj_dfs))


# obj_dfs %>% lapply(., function(x) {vector_paste((returnname(x))); df_paste(x)})

# lapply(seq_along(obj_dfs), 
#        function(y, n, i) {vector_paste(n[[i]]); df_paste(y[[i]])}, 
#        y=obj_dfs, n=names(obj_dfs))
# 
# 




#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' df_paste_()
#' df_paste_ <- function(df){
#'   vector_paste((substitute(df)))
#'   df_paste(df)
#' }
#' 
#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' df_paste_2()
#' df_paste_2 <- function(df){
#'   vector_paste(c("'____'", substitute(df), "'____'"))
#'   df_paste(df)
#' }
#' 
#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' df_paste_lod()
#' df_paste_lod <- function(lod){
#'   lapply(seq_along(lod), 
#'          function(y, n, i) {vector_paste(n[[i]]); df_paste(y[[i]])}, 
#'          y=lod, n=names(lod))
#' }

