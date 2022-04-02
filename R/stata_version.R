#' Stata Version
#'
#' @return the version of stata # from version 10.0
#' @export
#'
#' @examples
stata_version <- function(){
    OS <- Sys.info()["sysname"]
    OS.type <- .Platform$OS.type
    SRC <- "version"
    stata.path <-  find_stata(message = FALSE)

    if (OS %in% "Windows") {
        winRStataLog <- "RStata.log"
        on.exit(unlink(winRStataLog))
    }
    doFile <- "RStata.do"
    logFile <- "RStata.log"
    on.exit(unlink(doFile), add = TRUE)
    on.exit(unlink(logFile), add = TRUE)

    ## -------------------------
    ## Creating the .do file ...
    ## -------------------------

    ## External .do script 'support': KIS
    if (file.exists(SRC[1L]))
        SRC <- readLines(SRC[1L])

    ## put a placeholder around the part of interest, in order to find
    ## it easily (when removing overhead/setup code for each run)
    cut_me_here <- 'RSTATA: cut me here'
    cut_me_comment <- paste0('/*', cut_me_here, '*/')

    ## capture noisily and set cut points
    SRC <- c(
        'capture noisily {',
        cut_me_comment,
        SRC,
        cut_me_comment,
        '} /* end capture noisily */')

    ## set more off just to be sure nothing will freeze (hopefully :) )
    SRC <- c('set more off', SRC)

    ## adding this command to the end simplify life if user make changes but
    ## doesn't want a data.frame back
    SRC <- c(SRC, "exit, clear STATA")

    ## -------------
    ## Stata command
    ## -------------

    ## With Windows version, /e is almost always needed (if Stata is
    ## installed with GUI)
    stataCmd <- paste(stata.path,
                      if (OS %in% "Windows") "/e" else "",
                      "do",
                      doFile)

    ## ---
    ## IPC
    ## ---
    ## setup the .do file
    con <- file(doFile, "w")
    writeLines(SRC, con)
    close(con)

    ## execute Stata
    rdl <- pipe(stataCmd, "r")
    stataLog <- readLines(rdl)
    close(rdl)

    if (OS %in% "Windows") stataLog <- readLines(winRStataLog)
    ## postprocess log, keeping only the output of interest (between rows
    ## having /* RSTATA: cut me here */
    cutpoints <- grep(cut_me_here, stataLog)
    stataLog <- stataLog[seq.int(cutpoints[1] + 1, cutpoints[2] - 1)]
    as.numeric(substring(capture.output(cat(stataLog, sep = "\n"))[2], first = 9, last = 10))
}
