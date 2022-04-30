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
        dplyr::mutate(date = lubridate::as_date(rownames(.))) %>%
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
    invisible(capture.output(stata(paste0('use "', path, '"'), data.out = TRUE)))
}


#' Download file
#'
#' @param link the url
#' @param dfile the destination file
#'
#' @return
#' @export
#'
#' @examples
download_file <- function(link, dfile) {
    UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0"
    tryCatch({
        r <- httr::GET(link,
                       httr::add_headers(`Connection` = "keep-alive", `User-Agent` = UA),
                       httr::write_disk(dfile, overwrite=TRUE)
        )
        if(httr::status_code(r)==200){
            return(TRUE)
        }else{
            return(FALSE)
        }
        return(TRUE)
    }, error = function(e) {
        return(FALSE)
    })
}
