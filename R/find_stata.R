#' Find Stata
#'
#' @description This function is from Doug Hemken, Statamarkdown package
#'
#' @param message
#'
#' @return The path of STATA
#' @export
#'
#' @examples
find_stata <- function(message=TRUE) {
    stataexe <- ""
    if (.Platform$OS.type == "windows"){
        #  stataexe <- NULL
        for (d in c("C:/Program Files","C:/Program Files (x86)")) {
            if (stataexe=="" & dir.exists(d)) {
                for (v in seq(17,11,-1)) {
                    dv <- paste(d,paste0("Stata",v), sep="/")
                    if (dir.exists(dv)) {
                        for (f in c("Stata", "StataIC", "StataSE", "StataMP", "StataBE",
                                    "Stata-64", "StataIC-64", "StataSE-64", "StataMP-64", "StataBE-64")) {
                            dvf <- paste(paste(dv, f, sep="/"), "exe", sep=".")
                            if (file.exists(dvf)) {
                                stataexe <- substr(dvf, 1, nchar(dvf)-4)
                                if (message) message("Stata found at ", stataexe)
                            }
                            if (stataexe != "") break
                        }
                    }
                    if (stataexe != "") break
                }
            }
            if (stataexe != "") break
        }
    } else if (Sys.info()["sysname"]=="Darwin") {
        #    stataexe <- NULL
        dv <- "/Applications/Stata"
        if (dir.exists(dv)) {
            for (f in c("Stata", "StataSE", "StataMP", "StataIC", "StataBE")) {
                exc <- ifelse(f == "Stata", 'stata', paste0("stata-", tolower(strsplit(f, "Stata")[[1]][2]), ""))
                dvf <- paste(paste(paste(dv, f, sep="/"), "app", sep="."), "Contents/MacOS", exc, sep="/")
                if (file.exists(dvf)) {
                    stataexe <- dvf
                    if (message) message("Stata found at ", stataexe)
                    break
                }
                if (stataexe != "") break
            }
        }
    } else if (.Platform$OS.type == "unix") {
        #      stataexe <- NULL
        stataexe <- system2("which", "stata", stdout=TRUE)
        if (message) message("Stata found at ", stataexe)
    } else {
        message("Unrecognized operating system.")
    }
    if (stataexe!="") {
        knitr::opts_chunk$set(engine.path=list(stata=stataexe))
    } else {
        packageStartupMessage("No Stata executable found.")
    }
    return(stataexe)
}
