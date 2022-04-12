#' FRED
#'
#' @param Dataset The data set want to use in FRED
#' @param start.date The start date of the data set
#' @param end.date The end date of the data set
#'
#' @return
#' @export
#'
#' @examples
#' fred("FEDFUNDS", "1954-01-01", "2021-08-01")
fred <- function(Dataset = character(), start.date, end.date){
    dat <- suppressWarnings(tidyquant::tq_get(Dataset, get = "economic.data", from = as.Date(start.date), to  = as.Date(end.date))[, -1])
    names(dat)[ncol(dat)] <- Dataset
    dat
}

read_stata <- function(path){
    stata(paste("use", path, data.out = TRUE))
}

