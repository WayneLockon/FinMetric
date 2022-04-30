.onLoad <- function (libname, pkgname) {
    if (!requireNamespace("utils")) stop("Requires utils package.")
}
.onAttach <- function (libname, pkgname) {
    options("RStata.StataPath" = find_stata(message = FALSE))
    options("RStata.StataVersion" = stata_version())
    library(lmtest)
    suppressMessages(library(tidyverse))
    library(sandwich)
    suppressPackageStartupMessages(library(stargazer))
    suppressWarnings(suppressPackageStartupMessages(library(plm)))
}

