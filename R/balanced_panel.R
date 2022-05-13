#' Make panel data balanced
#'
#' @param df an object of class `data.frame`, `tibble`
#' @param type character, one of `"fill_NA"`,
#'     `"drop_individuals"`, or `"drop_times"`, see
#'     **Details**,
#' @param index only relevant for `data.frame` interface; if
#'     `NULL`, the first two columns of the data.frame are
#'     assumed to be the index variables; if not `NULL`, both
#'     dimensions ('individual', 'time') need to be specified by
#'     `index` as character of length 2 for data frame.
#' @param individual_var default is NULL; time invariant variables in the dataset, when balancing dataset they would keep the same by `"updown"` search.
#' @param time_seq default is NULL; window for balancing panel data set.
#'
#' @return
#' @export
#'
#' @examples
#' data("EmplUK", package = "plm")
#' EmplUK %>%
#' select(year, firm) %>%
#' filter(firm %in% c(1:10)) %>%
#' table()
#'
#' balance_panel(EmplUK, type = "fill_NA", individual_var = c("sector"))


balance_panel <- function(df,
                          type = c("fill_NA", "drop_individuals", "drop_times"),
                          index = NULL,
                          individual_var = NULL,
                          time_seq = NULL){
    type <- match.arg(type)
    # Step1: identify the index
    if (!is.null(index) && length(index) != 2L){ # index less than time and id
        stop("if argument 'index' is not NULL, 'index' needs to specify
             'individual' and 'time' dimension")
    }
    # default is the first two columns as index
    index <- if(is.null(index)) names(df)[1:2] else index

    # check variables does not be contained by names(df) and coincide with index
    if(!is.null(individual_var)){
        if(!identical(setdiff(individual_var, names(df)), character(0))) stop("'individual_var' should be contained in names(data.frame)")
        if(any(individual_var %in% index)) {
            individual_var <- setdiff(individual_var, index)
            warning("there are 'individual_var' coincided with 'index'")
        }
    }

    # identify time sequence
    time_seq <- if(!is.null(time_seq)) time_seq else df %>%
        select(index[2]) %>%
        unique %>%
        pull()


    switch(type,
           "fill_NA" = {
               result <- df %>%
                   group_by_at(index[1]) %>%
                   fill_(if(is.null(individual_var)) NULL else individual_var, .direction = "updown") %>%
                   group_by_at(c(index[1], individual_var)) %>%
                   expand(time_seq) %>%
                   rename_with(~index[2], time_seq) %>%
                   {left_join(., df, by = names(.))} %>%
                   ungroup()
           },
           "drop_individuals" = {
               result <- df %>%
                   # filter_(paste0(index[2], " %in% time_seq")) %>%
                   filter(.[[index[2]]] %in% time_seq) %>%
                   group_by_at(index[1]) %>%
                   fill_(if(is.null(individual_var)) NULL else individual_var, .direction = "updown") %>%
                   filter_at(union(index, individual_var),all_vars(!is.na(.))) %>%
                   ungroup()
           },
           "drop_times" = {
               time_new <- Reduce(intersect,
                                  split(df[, index[2]], df[, index[1]])) %>%
                   intersect(time_seq)
               result <- df %>%
                   filter(.[[index[2]]] %in% time_new) %>%
                   tibble()
           })
    return(result)
}
