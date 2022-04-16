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
fred <- function(Dataset = character(),
                 start.date,
                 end.date){
    quantmod::getSymbols(Dataset, src = "FRED", auto.assign = F, return.class = "data.frame") %>%
        dplyr::mutate(date = as.Date(rownames(.))) %>%
        dplyr::relocate(date) %>%
        dplyr::filter(date >= lubridate::as_date(start.date) & date <= lubridate::as_date(end.date)) %>%
        tibble()
    # Quandl::Quandl(paste0("FRED/", Dataset)) %>%
    #     dplyr::mutate(date = as.Date(Date)) %>%
    #     dplyr::select(-Date) %>%
    #     dplyr::filter(date >= lubridate::as_date(start.date) & date <= lubridate::as_date(end.date)) %>%
    #     tibble()
}



#' Read Stata dta file from given path or website
#'
#' @param path The path of dta file
#'
#' @return Return is a data.frame object
#' @export
#'
#' @examples read_stata("hiway")
#' Load (Minnesota Highway Data, 1973)
#'
#' read_stata("https://www.stata-press.com/data/r17/union")
#'
#' read_stata("data.dta")
read_stata <- function(path){
    stata(paste0('use "', path, '"'), data.out = TRUE)
}

