% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{DT_NAs_red_background}
\alias{DT_NAs_red_background}
\title{#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' fillr(df, ID='ID')
fillr <- function(df, ID='ID'){
  cat("fillr()\n")
  df <- na_if_(df)
  df[['var']] <- df[[ID]]
  df %<>% dplyr::group_by(var) %>%
    na_if_() %>%
    tidyr::fill(-var) %>%
    na_if_() %>%
    tidyr::fill(-var, .direction='up') %>%
    na_if_() %>%
    dplyr::distinct()
  # df %>% select(-matches('^var$'))
  df <- df[, -ncol(df)]
  print(paste0('fillr: ', ID))
  df
  # dplyr::distinct(df)
}
#' Samantha Rhoads's function fillr2
#' @export
#' @examples
#' fillr2(df, ID='ID')
fillr2 <- function(df, ID='ID'){
  DATE_COLUMN_NAMES <- df %>% select_if(is.Date.class) %>% names()
  df %<>% mutate_at(vars(one_of(DATE_COLUMN_NAMES)), as.character)
  df <- na_if_(df)
  df[['var']] <- df[[ID]]
  df %<>% dplyr::group_by(var) %>%
    tidyr::fill(-var) %>%
    tidyr::fill(-var, .direction='up')
  df <- df[, -ncol(df)]
  cat(paste0('fillr3: ', ID))
  df %<>% mutate_at(vars(one_of(DATE_COLUMN_NAMES)), lubridate::date)
  df
}
Srhoads wrote this to allow you to...}
\usage{
DT_NAs_red_background(DTdatatable)
}
\description{
#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' fillr(df, ID='ID')
fillr <- function(df, ID='ID'){
  cat("fillr()\n")
  df <- na_if_(df)
  df[['var']] <- df[[ID]]
  df %<>% dplyr::group_by(var) %>%
    na_if_() %>%
    tidyr::fill(-var) %>%
    na_if_() %>%
    tidyr::fill(-var, .direction='up') %>%
    na_if_() %>%
    dplyr::distinct()
  # df %>% select(-matches('^var$'))
  df <- df[, -ncol(df)]
  print(paste0('fillr: ', ID))
  df
  # dplyr::distinct(df)
}
#' Samantha Rhoads's function fillr2
#' @export
#' @examples
#' fillr2(df, ID='ID')
fillr2 <- function(df, ID='ID'){
  DATE_COLUMN_NAMES <- df %>% select_if(is.Date.class) %>% names()
  df %<>% mutate_at(vars(one_of(DATE_COLUMN_NAMES)), as.character)
  df <- na_if_(df)
  df[['var']] <- df[[ID]]
  df %<>% dplyr::group_by(var) %>%
    tidyr::fill(-var) %>%
    tidyr::fill(-var, .direction='up')
  df <- df[, -ncol(df)]
  cat(paste0('fillr3: ', ID))
  df %<>% mutate_at(vars(one_of(DATE_COLUMN_NAMES)), lubridate::date)
  df
}
Srhoads wrote this to allow you to...
}
\examples{
DT_NAs_red_background(DTdatatable)
}
