# deprecated.R
#' Deprecated functions of plm
#'
#' `dynformula`, `pht`, `plm.data`, and `pvcovHC` are
#' deprecated functions which could be removed from \pkg{plm} in a near future.
#'
#' `dynformula` was used to construct a dynamic formula which was the
#' first argument of `pgmm`. `pgmm` uses now multi-part formulas.
#'
#' `pht` estimates the Hausman-Taylor model, which can now be estimated
#' using the more general `plm` function.
#'
#' `plm.data` is replaced by `pdata.frame`.
#'
#' `pvcovHC` is replaced by `vcovHC`.
#'
#' `detect_lin_dep` was renamed to `detect.lindep`.
#'
#' @name plm-deprecated
#' @aliases detect_lin_dep
#' @param formula a formula,
#' @param lag.form a list containing the lag structure of each variable in the
#' formula,
#' @param diff.form a vector (or a list) of logical values indicating whether
#' variables should be differenced,
#' @param log.form a vector (or a list) of logical values indicating whether
#' variables should be in logarithms.
#' @param object,x an object of class `"plm"`,
#' @param data a `data.frame`,
#' @param \dots further arguments.
#' @param indexes a vector (of length one or two) indicating the (individual
#' and time) indexes (see Details);
#' @param lhs see Formula
#' @param rhs see Formula
#' @param model see plm
#' @param effect see plm
#' @param theta the parameter of transformation for the random effect model
#' @param cstcovar.rm remove the constant columns or not
#'
NULL

#' @rdname plm-deprecated
#' @export
pvcovHC <- function(x, ...){
    .Deprecated(new = "pvcovHC", msg = "'pvcovHC' is deprecated, use 'vcovHC' instead for same functionality",
                old = "vcovHC")
    UseMethod("vcovHC")
}


# plm.data() is deprecated since February 2017. Need to keep it in package
# for backward compatibility of users' code out there and packages, especially
# for package 'systemfit' (systemfit supports pdata.frame since 2017-03-09 but
# plm.data can be used there as well).
#
# While plm.data() was a 'full function' once, it now uses
# pdata.frame() and re-works the properties of the "plm.dim" object
# original created by the 'full' plm.data() function. The 'full'
# plm.data() function is kept non-exported as plm.data_depr_orig due
# to reference and testing (see tests/test_plm.data.R)

#' @rdname plm-deprecated
#' @export
plm.data <- function(x, indexes = NULL) {

    .Deprecated(new = "pdata.frame", msg = "use of 'plm.data' is discouraged, better use 'pdata.frame' instead",
                old = "plm.data")

    # the class "plm.dim" (which plm.data creates) deviates from class "pdata.frame":
    #    * always contains the indexes (in first two columns (id, time))
    #    * does not have fancy rownames
    #    * always coerces strings to factors
    #    * does not have index attribute
    #    * leaves in constant columns (albeit the 'full' implementation printed a msg about dropping those ...)
    #
    #  -> call pdata.frame accordingly and adjust afterwards
    orig_col_order <- colnames(x)

    x <- pdata.frame(x, index              = indexes,
                     drop.index         = FALSE,
                     row.names          = FALSE,
                     stringsAsFactors   = TRUE,
                     replace.non.finite = TRUE,
                     drop.NA.series     = TRUE,
                     drop.const.series  = FALSE)

    # determine position and names of index vars in pdata.frame
    pos_indexes <- pos.index(x)
    names_indexes <- names(pos_indexes) # cannot take from arg 'indexes' as it could be only the index for id

    # the class "plm.dim" does not have the index attribute -> remove
    attr(x, "index") <- NULL
    # remove class 'pdata.frame' to prevent any dispatching of special methods on object x
    class(x) <- setdiff(class(x), "pdata.frame")

    # class "plm.dim" always has indexes in first two columns (id, time)
    # while "pdata.frame" leaves the index variables at it's place (if not dropped at all with drop.index = T)
    x <- x[ , c(names_indexes, setdiff(orig_col_order, names_indexes))]

    # set class
    class(x) <- c("plm.dim", "data.frame")
    return(x)
}

### pht

lev2var <- function(x, ...){
    # takes a data.frame and returns a vector of variable names, the
    # names of the vector being the names of the effect

    is.fact <- sapply(x, is.factor)
    if (sum(is.fact) > 0L){
        not.fact <- names(x)[!is.fact]
        names(not.fact) <- not.fact
        x <- x[is.fact]
        wl <- lapply(x,levels)
        # nl is the number of levels for each factor
        nl <- sapply(wl,length)
        # nf is a vector of length equal to the total number of levels
        # containing the name of the factor
        nf <- rep(names(nl),nl)
        result <- unlist(wl)
        names(result) <- nf
        result <- paste(names(result), result, sep = "")
        names(nf) <- result
        c(nf, not.fact)
    }
    else{
        z <- names(x)
        names(z) <- z
        z
    }
}


#' Hausman--Taylor Estimator for Panel Data
#'
#' The Hausman--Taylor estimator is an instrumental variable estimator without
#' external instruments (function deprecated).
#'
#' `pht` estimates panels models using the Hausman--Taylor estimator,
#' Amemiya--MaCurdy estimator, or Breusch--Mizon--Schmidt estimator, depending
#' on the argument `model`. The model is specified as a two--part formula,
#' the second part containing the exogenous variables.
#'
#' @aliases pht
#' @param formula a symbolic description for the model to be
#'     estimated,
#' @param object,x an object of class `"plm"`,
#' @param data a `data.frame`,
#' @param subset see [lm()] for `"plm"`, a character or
#'     numeric vector indicating a subset of the table of coefficient
#'     to be printed for `"print.summary.plm"`,
#' @param na.action see [lm()],
#' @param model one of `"ht"` for Hausman--Taylor, `"am"`
#'     for Amemiya--MaCurdy and `"bms"` for
#'     Breusch--Mizon--Schmidt,
#' @param index the indexes,
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param \dots further arguments.
#' @return An object of class `c("pht", "plm", "panelmodel")`.
#'
#' A `"pht"` object contains the same elements as `plm`
#' object, with a further argument called `varlist` which
#' describes the typology of the variables. It has `summary` and
#' `print.summary` methods.
#'
#' @note The function `pht` is deprecated. Please use function `plm`
#'     to estimate Taylor--Hausman models like this with a three-part
#'     formula as shown in the example:\cr `plm(<formula>,
#'     random.method = "ht", model = "random", inst.method =
#'     "baltagi")`. The Amemiya--MaCurdy estimator and the
#'     Breusch--Mizon--Schmidt estimator is computed likewise with
#'     `plm`.
#' @export
#' @author Yves Croissant
#' @references
#'
#' \insertCite{AMEM:MACU:86}{plm}
#'
#' \insertCite{BALT:13}{plm}
#'
#' \insertCite{BREU:MIZO:SCHM:89}{plm}
#'
#' \insertCite{HAUS:TAYL:81}{plm}
#'
#' @keywords regression
#' @examples
#'
#' ## replicates Baltagi (2005, 2013), table 7.4; Baltagi (2021), table 7.5
#' ## preferred way with plm()
#' data("Wages", package = "plm")
#' ht <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) +
#'               bluecol + ind + union + sex + black + ed |
#'               bluecol + south + smsa + ind + sex + black |
#'               wks + married + union + exp + I(exp ^ 2),
#'           data = Wages, index = 595,
#'           random.method = "ht", model = "random", inst.method = "baltagi")
#' summary(ht)
#'
#' am <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) +
#'               bluecol + ind + union + sex + black + ed |
#'               bluecol + south + smsa + ind + sex + black |
#'               wks + married + union + exp + I(exp ^ 2),
#'           data = Wages, index = 595,
#'           random.method = "ht", model = "random", inst.method = "am")
#' summary(am)
#'
#' ## deprecated way with pht() for HT
#' #ht <- pht(lwage ~ wks + south + smsa + married + exp + I(exp^2) +
#' #          bluecol + ind + union + sex + black + ed |
#' #          sex + black + bluecol + south + smsa + ind,
#' #          data = Wages, model = "ht", index = 595)
#' #summary(ht)
#' # deprecated way with pht() for AM
#' #am <- pht(lwage ~ wks + south + smsa + married + exp + I(exp^2) +
#' #          bluecol + ind + union + sex + black + ed |
#' #          sex + black + bluecol + south + smsa + ind,
#' #          data = Wages, model = "am", index = 595)
#' #summary(am)
#'
#'
pht <- function(formula, data, subset, na.action, model = c("ht", "am", "bms"), index = NULL, ...){

    .Deprecated(old = "pht",
                msg = paste0("uses of 'pht()' and 'plm(., model = \"ht\")' are discouraged, ",
                             "better use 'plm(., model = \"random\", random.method = \"ht\", ",
                             "inst.method = \"baltagi\"/\"am\"/\"bms\")' for Hausman-Taylor, ",
                             "Amemiya-MaCurdy, and Breusch-Mizon-Schmidt estimator"))

    cl <- match.call(expand.dots = TRUE)
    mf <- match.call()

    model <- match.arg(model)
    # compute the model.frame using plm with model = NA
    mf[[1L]] <- as.name("plm")
    mf$model <- NA
    data <- eval(mf, parent.frame())
    # estimate the within model without instrument and extract the fixed
    # effects
    formula <- Formula(formula)
    if (length(formula)[2L] == 1L) stop("a list of exogenous variables should be provided")
    mf$model = "within"
    mf$formula <- formula(formula, rhs = 1)
    within <- eval(mf, parent.frame())
    fixef <- fixef(within)
    id <- index(data, "id")
    time <- index(data, "time")
    pdim <- pdim(data)
    balanced <- pdim$balanced
    T <- pdim$nT$T
    n <- pdim$nT$n
    N <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    # get the typology of the variables
    X <- model.matrix(data, rhs = 1, model = "within", cstcovar.rm = "all")
    W <- model.matrix(data, rhs = 2, model = "within", cstcovar.rm = "all")
    exo.all <- colnames(W)
    all.all <- colnames(X)
    edo.all <- all.all[!(all.all %in% exo.all)]
    all.cst <- attr(X, "constant")
    exo.cst <- attr(W, "constant")
    if("(Intercept)" %in% all.cst) all.cst <- setdiff(all.cst, "(Intercept)")
    if("(Intercept)" %in% exo.cst) exo.cst <- setdiff(exo.cst, "(Intercept)")
    exo.var <- exo.all[!(exo.all %in% exo.cst)]
    edo.cst <- all.cst[!(all.cst %in% exo.cst)]
    edo.var <- edo.all[!(edo.all %in% edo.cst)]

    if (length(edo.cst) > length(exo.var)){
        stop(" The number of endogenous time-invariant variables is greater
           than the number of exogenous time varying variables\n")
    }

    X <- model.matrix(data, model = "pooling", rhs = 1, lhs = 1)
    XV <- if(length(exo.var) > 0L) X[ , exo.var, drop = FALSE] else NULL
    NV <- if(length(edo.var) > 0L) X[ , edo.var, drop = FALSE] else NULL
    XC <- if(length(exo.cst) > 0L) X[ , exo.cst, drop = FALSE] else NULL
    NC <- if(length(edo.cst) > 0L) X[ , edo.cst, drop = FALSE] else NULL
    zo <- if(length(all.cst) != 0L)
        twosls(fixef[as.character(id)], cbind(XC, NC), cbind(XC, XV), TRUE)
    else lm(fixef ~ 1)

    sigma2 <- list()
    sigma2$one <- 0
    sigma2$idios <- deviance(within)/ (N - n)
    sigma2$one <- deviance(zo) / n
    if(balanced){
        sigma2$id <- (sigma2$one - sigma2$idios)/ T
        theta <- 1 - sqrt(sigma2$idios / sigma2$one)
    }
    else{
        # for unbalanced data, the harmonic mean of the Ti's is used ; why ??
        barT <- n / sum(1 / Ti)
        sigma2$id <- (sigma2$one - sigma2$idios) / barT
        theta <- 1 - sqrt(sigma2$idios / (sigma2$idios + Ti * sigma2$id))
        theta <- theta[as.character(id)]
    }

    estec <- structure(list(sigma2 = sigma2, theta = theta),
                       class = "ercomp",
                       balanced = balanced,
                       effect = "individual")

    y <- pmodel.response(data, model = "random", effect = "individual", theta = theta)
    X <- model.matrix(data, model = "random", effect = "individual", theta = theta)
    within.inst <- model.matrix(data, model = "within")

    if (model == "ht"){
        between.inst <- model.matrix(data, model = "Between",
                                     rhs = 2)[ , exo.var, drop = FALSE]
        W <- cbind(within.inst, XC, between.inst)
    }
    if (model == "am"){
        Vx <- model.matrix(data, model = "pooling",
                           rhs = 2)[ , exo.var, drop = FALSE]
        if (balanced){
            # Plus rapide mais pas robuste au non cylindre
            Vxstar <- Reduce("cbind",
                             lapply(seq_len(ncol(Vx)),
                                    function(x)
                                        matrix(Vx[ , x], ncol = T, byrow = TRUE)[rep(1:n, each = T), ]))
        }
        else{
            Xs <- lapply(seq_len(ncol(Vx)), function(x)
                structure(Vx[, x], index = index(data), class = c("pseries", class(Vx[, x]))))
            Vx2 <- Reduce("cbind", lapply(Xs, as.matrix))
            Vxstar <- Vx2[rep(1:n, times = Ti), ]
            Vxstar[is.na(Vxstar)] <- 0
        }
        W <- cbind(within.inst, XC, Vxstar)
    }
    if (model == "bms"){
        between.inst <- model.matrix(data, model = "Between",
                                     rhs = 2)[ , exo.var, drop = FALSE]
        Vx <- within.inst
        if (balanced){
            # Plus rapide mais pas robuste au non cylindre
            Vxstar <- Reduce("cbind",
                             lapply(seq_len(ncol(Vx)),
                                    function(x)
                                        matrix(Vx[ , x], ncol = T, byrow = TRUE)[rep(1:n, each = T), ]))
        }
        else{
            Xs <- lapply(seq_len(ncol(Vx)), function(x)
                structure(Vx[, x], index = index(data), class = c("pseries", class(Vx[, x]))))
            Vx2 <- Reduce("cbind", lapply(Xs, as.matrix))
            Vxstar <- Vx2[rep(1:n, times = Ti), ]
            Vxstar[is.na(Vxstar)] <- 0
        }
        W <- cbind(within.inst, XC, between.inst, Vxstar)
    }

    result <- twosls(y, X, W)
    K <- length(data)
    ve <- lev2var(data)
    varlist <- list(xv = unique(ve[exo.var]),
                    nv = unique(ve[edo.var]),
                    xc = unique(ve[exo.cst[exo.cst != "(Intercept)"]]),
                    nc = unique(ve[edo.cst])
    )
    varlist <- lapply(varlist, function(x){ names(x) <- NULL; x})
    result <- list(coefficients = coef(result),
                   vcov         = vcov(result),
                   residuals    = resid(result),
                   df.residual  = df.residual(result),
                   formula      = formula,
                   model        = data,
                   varlist      = varlist,
                   ercomp       = estec,
                   call         = cl,
                   args         = list(model = "ht", ht.method = model))
    names(result$coefficients) <- colnames(result$vcov) <-
        rownames(result$vcov) <- colnames(X)
    class(result) <- c("pht", "plm", "panelmodel")
    result
}

#' @rdname pht
#' @export
summary.pht <- function(object, ...){
    object$fstatistic <- pwaldtest(object, test = "Chisq")
    # construct the table of coefficients
    std.err <- sqrt(diag(vcov(object)))
    b <- coefficients(object)
    z <- b/std.err
    p <- 2*pnorm(abs(z), lower.tail = FALSE)
    object$coefficients <- cbind("Estimate"   = b,
                                 "Std. Error" = std.err,
                                 "z-value"    = z,
                                 "Pr(>|z|)"   = p)
    class(object) <- c("summary.pht", "pht", "plm", "panelmodel")
    object
}

#' @rdname pht
#' @export
print.summary.pht <- function(x, digits = max(3, getOption("digits") - 2),
                              width = getOption("width"), subset = NULL, ...){
    formula <- formula(x)
    has.instruments <- (length(formula)[2L] >= 2L)
    effect <- describe(x, "effect")
    model <- describe(x, "model")
    ht.method <- describe(x, "ht.method")
    cat(paste(effect.plm.list[effect]," ", sep = ""))
    cat(paste(model.plm.list[model]," Model", sep = ""), "\n")
    cat(paste("(", ht.method.list[ht.method], ")", sep = ""), "\n")

    cat("\nCall:\n")
    print(x$call)

    #    cat("\nTime-Varying Variables: ")
    names.xv <- paste(x$varlist$xv, collapse=", ")
    names.nv <- paste(x$varlist$nv, collapse=", ")
    names.xc <- paste(x$varlist$xc, collapse=", ")
    names.nc <- paste(x$varlist$nc, collapse=", ")
    cat(paste("\nT.V. exo  : ", names.xv,"\n", sep = ""))
    cat(paste("T.V. endo : ",   names.nv,"\n", sep = ""))
    #    cat("Time-Invariant Variables: ")
    cat(paste("T.I. exo  : ", names.xc, "\n", sep= ""))
    cat(paste("T.I. endo : ", names.nc, "\n", sep= ""))
    cat("\n")
    pdim <- pdim(x)
    print(pdim)
    cat("\nEffects:\n")
    print(x$ercomp)
    cat("\nResiduals:\n")
    save.digits <- unlist(options(digits = digits))
    on.exit(options(digits = save.digits))
    print(sumres(x))

    cat("\nCoefficients:\n")
    if (is.null(subset)) printCoefmat(coef(x), digits = digits)
    else printCoefmat(coef(x)[subset, , drop = FALSE], digits = digits)
    cat("\n")
    cat(paste("Total Sum of Squares:    ", signif(tss(x), digits),     "\n", sep = ""))
    cat(paste("Residual Sum of Squares: ", signif(deviance(x),digits), "\n", sep = ""))
    #  cat(paste("Multiple R-Squared:      ",signif(x$rsq,digits),"\n",sep=""))
    fstat <- x$fstatistic
    if (names(fstat$statistic) == "F"){
        cat(paste("F-statistic: ",signif(fstat$statistic),
                  " on ",fstat$parameter["df1"]," and ",fstat$parameter["df2"],
                  " DF, p-value: ",format.pval(fstat$p.value,digits=digits),"\n",sep=""))
    }
    else{
        cat(paste("Chisq: ", signif(fstat$statistic),
                  " on ", fstat$parameter,
                  " DF, p-value: ", format.pval(fstat$p.value,digits=digits), "\n", sep=""))

    }
    invisible(x)
}

## dynformula

sumres <- function(x){
    sr <- summary(unclass(resid(x)))
    srm <- sr["Mean"]
    if (abs(srm) < 1e-10){
        sr <- sr[c(1:3, 5:6)]
    }
    sr
}


create.list <- function(alist, K, has.int, has.resp, endog, exo, default){
    # if alist is NULL, create a list of 0
    if (is.null(alist)) alist <- rep(list(default), K+has.resp)
    # if alist is not a list, coerce it
    if (!is.list(alist)) alist <- list(alist)

    if (!is.null(names(alist))){
        # case where (at least) some elements are named
        nam <- names(alist) # vector of names of elements
        oalist <- alist  # copy of the alist provided
        notnullname <- nam[nam != ""]
        if (any (nam == "")){
            # case where one element is unnamed, and therefore is the default
            unnamed <- which(nam == "")
            if (length(unnamed) > 1L) stop("Only one unnamed element is admitted")
            default <- alist[[unnamed]]
        }
        else{
            # case where there are no unnamed elements, the default is 0
            default <- default
        }
        alist <- rep(list(default), K+has.resp)
        names(alist) <- c(endog, exo)
        alist[notnullname] <- oalist[notnullname]
    }
    else{
        # case where there are no names, in this case the relevant length is
        # whether 1 or K+1
        if (length(alist) == 1L) alist <- rep(alist, c(K+has.resp))
        else if (!length(alist) %in% c(K+has.resp)) stop("irrelevant length for alist")
    }
    names(alist) <- c(endog,exo)
    alist
}

write.lags <- function(name, lags, diff){
    lags <- switch(length(lags),
                   "1" = c(0, lags),
                   "2" = sort(lags),
                   stop("lags should be of length 1 or 2\n")
    )
    lag.string <- ifelse(diff, "diff", "lag")
    chlag <- c()
    if (lags[2L] != 0L){
        lags <- lags[1L]:lags[2L]
        for (i in lags){
            if (i == 0L){
                if (diff) chlag <- c(chlag, paste("diff(",name,")")) else chlag <- c(chlag,name)
            }
            else{
                ichar <- paste(i)
                chlag <- c(chlag, paste(lag.string,"(",name,",",i,")",sep=""))
            }
        }
        ret <- paste(chlag, collapse="+")
    }
    else{
        if (diff) chlag <- paste("diff(",name,")") else chlag <- name
        ret <- chlag
    }
    ret
}



#' @rdname plm-deprecated
#' @export
dynformula <- function(formula, lag.form = NULL, diff.form = NULL, log.form = NULL) {

    .Deprecated(msg = "use of 'dynformula()' is deprecated, use a multi-part formula instead",
                old = "dynformula")

    # for backward compatibility, accept a list argument and coerce it
    # to a vector
    if (!is.null(diff.form) && !is.list(diff.form)) diff.form <- as.list(diff.form)
    if (!is.null(log.form) && !is.list(log.form)) log.form <- as.list(log.form)

    # exo / endog are the names of the variable
    # has.int has.resp  TRUE if the formula has an intercept and a response
    # K is the number of exogenous variables
    exo <- attr(terms(formula), "term.labels")
    has.int <- attr(terms(formula), "intercept") == 1
    if(length(formula) == 3L){
        endog <- deparse(formula[[2L]])
        has.resp <- TRUE
    }
    else{
        endog <- NULL
        has.resp <- FALSE
    }
    K <- length(exo)

    # use the create.list function to create the lists with the relevant
    # default values
    lag.form <- create.list(lag.form, K, has.int, has.resp, endog, exo, 0)
    diff.form <- unlist(create.list(diff.form, K, has.int, has.resp, endog, exo, FALSE))
    log.form  <- unlist(create.list(log.form,  K, has.int, has.resp, endog, exo, FALSE))

    structure(formula, class = c("dynformula", "formula"), lag = lag.form,
              diff = diff.form, log = log.form, var = c(endog,exo))
}

#' @rdname plm-deprecated
#' @export
formula.dynformula <- function(x, ...){
    log.form <- attr(x, "log")
    lag.form <- attr(x, "lag")
    diff.form <- attr(x, "diff")
    has.resp <- length(x) == 3L
    exo <- attr(x, "var")
    if (has.resp){
        endog <- exo[1L]
        exo <- exo[-1L]
    }
    has.int <- attr(terms(x), "intercept") == 1
    chexo <- c()
    if (has.resp){
        if (log.form[1L])  endog <- paste("log(",  endog, ")", sep = "")
        if (diff.form[1L]) endog <- paste("diff(", endog, ")", sep = "")
        if (  length(lag.form[[1L]]) == 1L && lag.form[[1L]] != 0L) lag.form[[1L]] <- c(1, lag.form[[1L]])
        if (!(length(lag.form[[1L]]) == 1L && lag.form[[1L]] == 0L))
            chexo <- c(chexo, write.lags(endog, lag.form[[1L]], diff.form[1L]))
    }
    for (i in exo){
        lag.formi <- lag.form[[i]]
        diff.formi <- diff.form[i]
        if (log.form[[i]]) i <- paste("log(",i,")", sep = "")
        chexo <- c(chexo, write.lags(i, lag.formi, diff.formi))
    }
    chexo <- paste(chexo, collapse = "+")
    formod <- if(has.resp) { as.formula(paste(endog, "~", chexo, sep = "")) }
    else { as.formula(paste("~", chexo, sep = "")) }
    if (!has.int) formod <- update(formod, . ~ . -1)
    formod
}

#' @rdname plm-deprecated
#' @export
print.dynformula <- function(x, ...){
    print(formula(x), ...)
}

#' @rdname plm-deprecated
#' @export
pFormula <- function(object) {
    .Deprecated(msg = paste0("class 'pFormula' is deprecated, simply use class",
                             "'Formula'. 'pFormula' will be removed very soon!"),
                old = "pFormula", new = "Formula")
    stopifnot(inherits(object, "formula"))
    if (!inherits(object, "Formula")){
        object <- Formula(object)
    }
    class(object) <- unique(c("pFormula", class(object)))
    object
}

#' @rdname plm-deprecated
#' @export
as.Formula.pFormula <- function(x, ...){
    class(x) <- setdiff(class(x), "pFormula")
    x
}


## pFormula stuff, usefull for cquad

#' @rdname plm-deprecated
#' @export
as.Formula.pFormula <- function(x, ...){
    class(x) <- setdiff(class(x), "pFormula")
    x
}

#' @rdname plm-deprecated
#' @export
model.frame.pFormula <- function(formula, data, ..., lhs = NULL, rhs = NULL){
    if (is.null(rhs)) rhs <- 1:(length(formula)[2L])
    if (is.null(lhs)) lhs <- if(length(formula)[1L] > 0L) 1 else 0
    index <- attr(data, "index")
    mf <- model.frame(as.Formula(formula), as.data.frame(data), ..., rhs = rhs)
    index <- index[as.numeric(rownames(mf)), ]
    index <- droplevels(index)
    class(index) <- c("pindex", "data.frame")
    structure(mf,
              index = index,
              class = c("pdata.frame", class(mf)))
}


#' @rdname plm-deprecated
#' @export
model.matrix.pFormula <- function(object, data,
                                  model = c("pooling", "within", "Between", "Sum",
                                            "between", "mean", "random", "fd"),
                                  effect = c("individual", "time", "twoways", "nested"),
                                  rhs = 1,
                                  theta = NULL,
                                  cstcovar.rm = NULL,
                                  ...){
    model <- match.arg(model)
    effect <- match.arg(effect)
    formula <- object
    has.intercept <- has.intercept(formula, rhs = rhs)
    # relevant defaults for cstcovar.rm
    if (is.null(cstcovar.rm)) cstcovar.rm <- ifelse(model == "within", "intercept", "none")
    balanced <- is.pbalanced(data)
    # check if inputted data is a model.frame, if not convert it to
    # model.frame (important for NA handling of the original data when
    # model.matrix.pFormula is called directly) As there is no own
    # class for a model.frame, check if the 'terms' attribute is
    # present (this mimics what lm does to detect a model.frame)
    if (is.null(attr(data, "terms")))
        data <- model.frame.pFormula(pFormula(formula), data)
    # this goes to Formula::model.matrix.Formula:
    X <- model.matrix(as.Formula(formula), rhs = rhs, data = data, ...)
    # check for infinite or NA values and exit if there are some
    if(any(! is.finite(X))) stop(paste("model matrix or response contains non-finite",
                                       "values (NA/NaN/Inf/-Inf)"))
    X.assi <- attr(X, "assign")
    X.contr <- attr(X, "contrasts")
    X.contr <- X.contr[ ! sapply(X.contr, is.null) ]
    index <- index(data)
    checkNA.index(index) # check for NAs in model.frame's index and error if any
    attr(X, "index") <- index
    if (effect == "twoways" && model %in% c("between", "fd"))
        stop("twoways effect only relevant for within, random and pooling models")
    if (model == "within")  X <- Within(X, effect)
    if (model == "Sum")     X <- Sum(X, effect)
    if (model == "Between") X <- Between(X, effect)
    if (model == "between") X <- between(X, effect)
    if (model == "mean")    X <- Mean(X)
    if (model == "fd")      X <- pdiff(X, effect = "individual",
                                       has.intercept = has.intercept)
    if (model == "random"){
        if (is.null(theta)) stop("a theta argument should be provided")
        if (effect %in% c("time", "individual")) X <- X - theta * Between(X, effect)
        if (effect == "nested") X <- X - theta$id * Between(X, "individual") -
                theta$gp * Between(X, "group")
        if (effect == "twoways" && balanced)
            X <- X - theta$id * Between(X, "individual") -
                theta$time * Between(X, "time") + theta$total * Mean(X)
    }

    if (cstcovar.rm == "intercept"){
        posintercept <- match("(Intercept)", colnames(X))
        if (! is.na(posintercept)) X <- X[ , - posintercept, drop = FALSE]
    }
    if (cstcovar.rm %in% c("covariates", "all")){
        cols <- apply(X, 2, is.constant)
        cstcol <- names(cols)[cols]
        posintercept <- match("(Intercept)", cstcol)
        cstintercept <- if(is.na(posintercept)) FALSE else TRUE
        zeroint <- if(cstintercept &&
                      max(X[, posintercept]) < sqrt(.Machine$double.eps))
            TRUE else FALSE
        if (length(cstcol) > 0L){
            if ((cstcovar.rm == "covariates" || !zeroint) && cstintercept) cstcol <- cstcol[- posintercept]
            if (length(cstcol) > 0L){
                X <- X[, - match(cstcol, colnames(X)), drop = FALSE]
                attr(X, "constant") <- cstcol
            }
        }
    }
    structure(X, assign = X.assi, contrasts = X.contr, index = index)
}



# detect_lin_dep_alias.R
# functions to aid in detecting linear dependent columns in the (transformed)
# model matrix or estimated plm models:
#  * detect.lindep
#  * alias (the latter is a wrapper around alias.lm)
#
# doc file provides an extensive example how linear dependence can arise after
# the data transformation, e. g., for within transformation

### detect.lindep.matrix, .data.frame, .plm




#' Functions to detect linear dependence
#'
#' Little helper functions to aid users to detect linear dependent columns in a
#' two-dimensional data structure, especially in a (transformed) model matrix -
#' typically useful in interactive mode during model building phase.
#'
#'
#' Linear dependence of columns/variables is (usually) readily avoided when
#' building one's model.  However, linear dependence is sometimes not obvious
#' and harder to detect for less experienced applied statisticians. The so
#' called "dummy variable trap" is a common and probably the best--known
#' fallacy of this kind (see e. g. Wooldridge (2016), sec. 7-2.). When building
#' linear models with `lm` or `plm`'s `pooling` model, linear
#' dependence in one's model is easily detected, at times post hoc.
#'
#' However, linear dependence might also occur after some transformations of
#' the data, albeit it is not present in the untransformed data. The within
#' transformation (also called fixed effect transformation) used in the
#' `"within"` model can result in such linear dependence and this is
#' harder to come to mind when building a model. See **Examples** for two
#' examples of linear dependent columns after the within transformation: ex. 1)
#' the transformed variables have the opposite sign of one another; ex. 2) the
#' transformed variables are identical.
#'
#' During `plm`'s model estimation, linear dependent columns and their
#' corresponding coefficients in the resulting object are silently dropped,
#' while the corresponding model frame and model matrix still contain the
#' affected columns.  The plm object contains an element `aliased` which
#' indicates any such aliased coefficients by a named logical.
#'
#' Both functions, `detect.lindep` and `alias`, help to
#' detect linear dependence and accomplish almost the same:
#' `detect.lindep` is a stand alone implementation while
#' `alias` is a wrapper around
#' [stats::alias.lm()], extending the `alias`
#' generic to classes `"plm"` and `"pdata.frame"`.
#' `alias` hinges on the availability of the package
#' \CRANpkg{MASS} on the system. Not all arguments of `alias.lm`
#' are supported.  Output of `alias` is more informative as it
#' gives the linear combination of dependent columns (after data
#' transformations, i. e., after (quasi)-demeaning) while
#' `detect.lindep` only gives columns involved in the linear
#' dependence in a simple format (thus being more suited for automatic
#' post--processing of the information).
#'
#' @aliases detect.lindep
#' @param object for `detect.lindep`: an object which should be checked
#' for linear dependence (of class `"matrix"`, `"data.frame"`, or
#' `"plm"`); for `alias`: either an estimated model of class
#' `"plm"` or a `"pdata.frame"`. Usually, one wants to input a model
#' matrix here or check an already estimated plm model,
#' @param suppressPrint for `detect.lindep` only: logical indicating
#' whether a message shall be printed; defaults to printing the message, i. e.,
#' to `suppressPrint = FALSE`,
#' @param model (see `plm`),
#' @param effect (see `plm`),
#' @param \dots further arguments.
#' @return For `detect.lindep`: A named numeric vector containing column
#' numbers of the linear dependent columns in the object after data
#' transformation, if any are present. `NULL` if no linear dependent
#' columns are detected.
#'
#' For `alias`: return value of [stats::alias.lm()] run on the
#' (quasi-)demeaned model, i. e., the information outputted applies to
#' the transformed model matrix, not the original data.
#' @note function `detect.lindep` was called `detect_lin_dep`
#'     initially but renamed for naming consistency later.
#' @export
#' @author Kevin Tappe
#' @seealso [stats::alias()], [stats::model.matrix()] and especially
#'     `plm`'s [model.matrix()] for (transformed) model matrices,
#'     plm's [model.frame()].
#' @references
#'
#' \insertRef{WOOL:13}{plm}
#'
#' @keywords manip array
#' @examples
#'
#' ### Example 1 ###
#' # prepare the data
#' data("Cigar" , package = "plm")
#' Cigar[ , "fact1"] <- c(0,1)
#' Cigar[ , "fact2"] <- c(1,0)
#' Cigar.p <- pdata.frame(Cigar)
#'
#' # setup a formula and a model frame
#' form <- price ~ 0 + cpi + fact1 + fact2
#' mf <- model.frame(Cigar.p, form)
#' # no linear dependence in the pooling model's model matrix
#' # (with intercept in the formula, there would be linear depedence)
#' detect.lindep(model.matrix(mf, model = "pooling"))
#' # linear dependence present in the FE transformed model matrix
#' modmat_FE <- model.matrix(mf, model = "within")
#' detect.lindep(modmat_FE)
#' mod_FE <- plm(form, data = Cigar.p, model = "within")
#' detect.lindep(mod_FE)
#' alias(mod_FE) # => fact1 == -1*fact2
#' plm(form, data = mf, model = "within")$aliased # "fact2" indicated as aliased
#'
#' # look at the data: after FE transformation fact1 == -1*fact2
#' head(modmat_FE)
#' all.equal(modmat_FE[ , "fact1"], -1*modmat_FE[ , "fact2"])
#'
#' ### Example 2 ###
#' # Setup the data:
#' # Assume CEOs stay with the firms of the Grunfeld data
#' # for the firm's entire lifetime and assume some fictional
#' # data about CEO tenure and age in year 1935 (first observation
#' # in the data set) to be at 1 to 10 years and 38 to 55 years, respectively.
#' # => CEO tenure and CEO age increase by same value (+1 year per year).
#' data("Grunfeld", package = "plm")
#' set.seed(42)
#' # add fictional data
#' Grunfeld$CEOtenure <- c(replicate(10, seq(from=s<-sample(1:10,  1), to=s+19, by=1)))
#' Grunfeld$CEOage    <- c(replicate(10, seq(from=s<-sample(38:65, 1), to=s+19, by=1)))
#'
#' # look at the data
#' head(Grunfeld, 50)
#'
#' form <- inv ~ value + capital + CEOtenure + CEOage
#' mf <- model.frame(pdata.frame(Grunfeld), form)
#' # no linear dependent columns in original data/pooling model
#' modmat_pool <- model.matrix(mf, model="pooling")
#' detect.lindep(modmat_pool)
#' mod_pool <- plm(form, data = Grunfeld, model = "pooling")
#' alias(mod_pool)
#'
#' # CEOtenure and CEOage are linear dependent after FE transformation
#' # (demeaning per individual)
#' modmat_FE <- model.matrix(mf, model="within")
#' detect.lindep(modmat_FE)
#' mod_FE <- plm(form, data = Grunfeld, model = "within")
#' detect.lindep(mod_FE)
#' alias(mod_FE)
#'
#' # look at the transformed data: after FE transformation CEOtenure == 1*CEOage
#' head(modmat_FE, 50)
#' all.equal(modmat_FE[ , "CEOtenure"], modmat_FE[ , "CEOage"])
#'
detect.lindep <- function(object, ...) {
    UseMethod("detect.lindep")
}

#' @rdname detect.lindep
#' @method detect.lindep matrix
#' @export
detect.lindep.matrix <- function(object, suppressPrint = FALSE, ...) {
    if (!inherits(object, "matrix")) {
        stop("Input 'object' must be a matrix. Presumably, one wants a model matrix
         generated by some 'model.matrix' function.")}

    # do rank reduction to detect lin. dep. columns
    rank_rec <- sapply(1:ncol(object), function(col) qr(object[ , -col])$rank)

    if (diff(range(rank_rec)) == 0) {
        num <- NULL # return NULL if there is no linear dep.
    } else {
        num <- which(rank_rec == max(rank_rec))
        names(num) <- colnames(object)[num]
    }

    if(!suppressPrint) {
        if(is.null(num)) {
            print("No linear dependent column(s) detected.")
        } else {
            print(paste0("Suspicious column number(s): ", paste(num,        collapse = ", ")))
            print(paste0("Suspicious column name(s):   ", paste(names(num), collapse = ", ")))
        }
        return(invisible(num))
    }
    return(num)
}

#' @rdname detect.lindep
#' @method detect.lindep data.frame
#' @export
detect.lindep.data.frame <- function(object, suppressPrint = FALSE, ...) {
    if (!inherits(object, "data.frame")) {
        stop("Input 'object' must be a data.frame")}

    return(detect.lindep.matrix(as.matrix(object), suppressPrint = suppressPrint, ...))
}

#' @rdname detect.lindep
#' @method detect.lindep plm
#' @export
detect.lindep.plm <- function(object, suppressPrint = FALSE, ...) {
    if (!inherits(object, "plm")) {
        stop("Input 'object' must be of class \"plm\"")}

    return(detect.lindep.matrix(model.matrix(object), suppressPrint = suppressPrint, ...))
}


### alias.plm, alias.pFormula
# This is just a wrapper function to allow to apply the generic stats::alias on
# plm objects and pFormulas with the _transformed data_ (the transformed model.matrix).
# NB: arguments 'model' and 'effect' are not treated here.


#' @rdname detect.lindep
#' @export
alias.plm <- function(object, ...) {
    dots <- list(...)
    if (!is.null(dots$inst.method)) stop("alias.plm/alias.pFormula: IV not supported")
    if (length(formula(object))[2] == 2) stop("alias.plm/alias.pFormula: IV not supported")

    # catch unsupported alias.lm args and convert
    if (!is.null(dots[["partial"]])) {
        if (dots[["partial"]]) {
            dots[["partial"]] <- FALSE
            warning("alias.plm/alias.pFormula: arg partial = TRUE not supported, changed to FALSE")
        }
    }
    if (!is.null(dots[["partial.pattern"]])) {
        if (dots[["partial.pattern"]]) {
            dots[["partial.pattern"]] <- FALSE
            warning("alias.plm/alias.pFormula: arg partial.pattern = TRUE not supported, changed to FALSE")
        }
    }

    X <- model.matrix(object)
    y <- pmodel.response(object)

    lm.fit.obj <- lm.fit(X, y)
    class(lm.fit.obj) <- "lm"
    lm.fit.obj$terms <- deparse(object$formula)

    ## use lm.fit rather than lm():
    ## could estimate lm model with lm(), but takes more resources and
    ## need to remove extra classes "formula" for lm to prevent warning
    # form <- object$formula
    # form <- setdiff(class(form), c("pFormula", "Formula"))
    # Xdf <- as.data.frame(X)
    # ydf <- as.data.frame(y)
    # names(ydf) <- names(object$model)[1]
    # data <- cbind(ydf, Xdf)
    # lmobj <- lm(form, data = data)
    # return(stats::alias(lmobj))

    return(stats::alias(lm.fit.obj, ... = dots))
}

## alias.pFormula <- function(object, data,
##                            model = c("pooling", "within", "Between", "between",
##                                      "mean", "random", "fd"),
##                            effect = c("individual", "time", "twoways"),
##                            ...) {
##   dots <- list(...)
##   if (!is.null(dots$inst.method)) stop("alias.plm/alias.pFormula: IV not supported")
##   model <- match.arg(model)
##   effect <- match.arg(effect)
##   formula <- object

##   # check if object is already pFormula, try to convert if not
##   if (!inherits(formula, "pFormula")) formula <- pFormula(formula)

##   # check if data is already a model frame, convert to if not
##   if (is.null(attr(data, "terms"))) {
##     data <- model.frame.pFormula(pFormula(formula), data)
##   }

##   plmobj <- plm(formula, data = data, model = model, effect = effect, ...)
## #  print(summary(plmobj))
##   return(alias(plmobj, ...))
## }


#' @rdname detect.lindep
#' @export
alias.pdata.frame <- function(object,
                              model = c("pooling", "within", "Between", "between",
                                        "mean", "random", "fd"),
                              effect = c("individual", "time", "twoways"),
                              ...) {
    dots <- list(...)
    if (!is.null(dots$inst.method)) stop("alias.plm/alias.pFormula: IV not supported")
    model <- match.arg(model)
    effect <- match.arg(effect)
    # check if data is already a model frame, if not exit
    if (is.null(attr(object, "terms")))
        stop("the argument must be a model.frame")
    formula <- attr(object, "formula")
    plmobj <- plm(formula, data = object, model = model, effect = effect, ...)
    return(alias(plmobj, ...))
}

# est_cce.R
## Common Correlated Effects Pooled/MG estimators
## ref. Holly, Pesaran and Yamagata JoE 158 (2010)
## (also Kapetanios, Pesaran and Yamagata JoE 2010)
## CCEP and CCEMG together in the same SW framework
## based on generalized FEs

## this version 6: includes both defactored (cce) and raw (standard) residuals,
## leaving to a special residuals.pcce method the choice of which to retrieve

## NB the effect of including a trend is exactly the same as for
## including as.numeric(<timeindex>) in the model specification
## If the panel is unbalanced, though, then for some i the trend becomes
## (3,4,5, ...) instead of (1,2,3, ...); the difference is absorbed by
## the individual intercept, and *the group intercept* changes.

## needed for standalone operation:
#plm <- plm:::plm
#pdim <- plm:::pdim

#model.matrix.plm <- plm:::model.matrix.plm
#pmodel.response.plm <- plm:::pmodel.response.plm

#tss <- plm:::tss


#' Common Correlated Effects estimators
#'
#' Common Correlated Effects Mean Groups (CCEMG) and Pooled (CCEP)
#' estimators for panel data with common factors (balanced or
#' unbalanced)
#'
#' `pcce` is a function for the estimation of linear panel models by
#' the Common Correlated Effects Mean Groups or Pooled estimator,
#' consistent under the hypothesis of unobserved common factors and
#' idiosyncratic factor loadings. The CCE estimator works by
#' augmenting the model by cross-sectional averages of the dependent
#' variable and regressors in order to account for the common factors,
#' and adding individual intercepts and possibly trends.
#'
#' @aliases pcce
#' @param formula a symbolic description of the model to be estimated,
#' @param object,x an object of class `"pcce"`,
#' @param data a `data.frame`,
#' @param subset see `lm`,
#' @param na.action see `lm`,
#' @param model one of `"mg"`, `"p"`, selects Mean Groups vs. Pooled
#'     CCE model,
#' @param index the indexes, see [pdata.frame()],
#' @param trend logical specifying whether an individual-specific
#'     trend has to be included,
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param type one of `"defactored"` or `"standard"`,
#' @param vcov a variance-covariance matrix furnished by the user or a function to calculate one,
#' @param \dots further arguments.
#' @return An object of class `c("pcce", "panelmodel")` containing:
#'     \item{coefficients}{the vector of coefficients,}
#'     \item{residuals}{the vector of (defactored) residuals,}
#'     \item{stdres}{the vector of (raw) residuals,}
#'     \item{tr.model}{the transformed data after projection on H,}
#'     \item{fitted.values}{the vector of fitted values,}
#'     \item{vcov}{the covariance matrix of the coefficients,}
#'     \item{df.residual}{degrees of freedom of the residuals,}
#'     \item{model}{a data.frame containing the variables used for the
#'     estimation,}
#'     \item{call}{the call,}
#'     \item{indcoef}{the matrix of individual coefficients from
#'     separate time series regressions,}
#'     \item{r.squared}{numeric, the R squared.}
#' @export
#' @importFrom MASS ginv
#' @author Giovanni Millo
#' @references
#'
#' \insertRef{kappesyam11}{plm}
#'
#' @keywords regression
#' @examples
#'
#' data("Produc", package = "plm")
#' ccepmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="p")
#' summary(ccepmod)
#' summary(ccepmod, vcov = vcovHC) # use argument vcov for robust std. errors
#'
#' ccemgmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="mg")
#' summary(ccemgmod)
#'
pcce <- function (formula, data, subset, na.action,
                  model = c("mg", "p"),
                  #residuals = c("defactored", "standard"),
                  index = NULL, trend = FALSE, ...) {

    ## Create a Formula object if necessary (from plm)
    if (!inherits(formula, "Formula")) formula <- as.Formula(formula)

    ## same as pggls but for effect, fixed at "individual" for compatibility
    ## ind for id, tind for time, k for K, coefnam for coef.names
    effect <- "individual"

    ## record call etc.
    model <- match.arg(model)
    model.name <- paste("cce", model, sep="")
    data.name <- paste(deparse(substitute(data)))
    cl <- match.call()
    plm.model <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "effect",
                 "model", "index"), names(plm.model), 0)
    plm.model <- plm.model[c(1L, m)]
    plm.model[[1L]] <- as.name("plm")
    ## change the 'model' in call
    plm.model$model <- "pooling"
    ## evaluates the call, modified with model = "pooling", inside the
    ## parent frame resulting in the pooling model on formula, data
    plm.model <- eval(plm.model, parent.frame())
    mf <- model.frame(plm.model)
    index <- unclass(attr(mf, "index")) # unclass for speed
    ind  <- index[[1L]] ## individual index
    tind <- index[[2L]] ## time index
    ## set dimension variables
    pdim <- pdim(plm.model)
    balanced <- pdim$balanced
    nt <- pdim$Tint$nt
    Ti <- pdim$Tint$Ti
    T. <- pdim$nT$T
    n <- pdim$nT$n
    N <- pdim$nT$N
    ## set index names
    time.names <- pdim$panel.names$time.names
    id.names   <- pdim$panel.names$id.names
    coef.names <- names(coef(plm.model))
    ## number of coefficients
    k <- length(coef.names)

    ## model data
    X <- model.matrix(plm.model)
    y <- model.response(mf)

    ## det. *minimum* group numerosity
    t <- min(Ti) # ==  min(tapply(X[ , 1], ind, length))

    ## check min. t numerosity
    ## NB it is also possible to allow estimation if there *is* one group
    ## with t large enough and average on coefficients removing NAs
    ## Here we choose the explicit way: let estimation fail if we lose df
    ## but a warning would do...
    if(t < (k+1)) stop("Insufficient number of time periods")

    ## one regression for each group i in 1..n
    ## and retrieve coefficients putting them into a matrix
    ## (might be unbalanced => t1 != t2 but we don't care as long
    ## as min(t) > k+1)

    ## subtract intercept from parms number and names
    has.int <- attr(terms(plm.model), "intercept")
    if(has.int) {
        k <- k - 1
        coef.names <- coef.names[-1L]
    }

    ## "pre-allocate" coefficients matrix for the n models
    tcoef <- matrix(NA_real_, nrow = k, ncol = n)

    ## pre-allocate residuals lists for individual regressions
    ## (lists allow for unbalanced panels)
    cceres <- vector("list", n)
    stdres <- vector("list", n)

    ## CCE by-group estimation

    ## must put the intercept into the group-invariant part!!
    ## so first drop it from X
    if(has.int) {
        X <- X[ , -1L, drop = FALSE]
    }

    ## group-invariant part, goes in Hhat
    ## between-periods transformation (take means over groups for each t)
    Xm <- Between(X, effect = tind, na.rm = TRUE)
    ym <- as.numeric(Between(y, effect = "time", na.rm = TRUE))

    Hhat <- if(has.int) cbind(ym, Xm, 1L) else cbind(ym, Xm)

    ## prepare XMX, XMy arrays
    XMX <- array(data = NA_real_, dim = c(k, k, n))
    XMy <- array(data = NA_real_, dim = c(k, 1L, n))

    ## hence calc. beta_i anyway because of vcov

    ## for each x-sect. i=1..n estimate (over t) the CCE for every TS
    ## as in KPY, eq. 15
    unind <- unique(ind)
    for(i in 1:n) {
        tX <- X[ind == unind[i], , drop = FALSE]
        ty <- y[ind == unind[i]]
        tHhat <- Hhat[ind == unind[i], , drop = FALSE]

        ## if 'trend' then augment the xs-invariant component
        if(trend) tHhat <- cbind(tHhat, 1:(dim(tHhat)[[1L]]))

        ## NB tHat, tMhat should be i-invariant
        tMhat <- diag(1, length(ty)) -
            tHhat %*% solve(crossprod(tHhat), t(tHhat))

        CP.tXtMhat <- crossprod(tX, tMhat)
        tXMX <- tcrossprod(CP.tXtMhat, t(tX))
        tXMy <- tcrossprod(CP.tXtMhat, t(ty))

        ## XMX_i, XMy_i
        XMX[ , , i] <- tXMX
        XMy[ , , i] <- tXMy

        ## single CCE coefficients
        tb <- ginv(tXMX) %*% tXMy  #solve(tXMX, tXMy)
        ## USED A GENERALIZED INVERSE HERE BECAUSE OF PBs WITH ECM SPECS
        ## Notice remark in Pesaran (2006, p.977, between (27) and (28))
        ## that XMX.i is invariant to the choice of a g-inverse for H'H
        tcoef[ , i] <- tb

        ## cce (defactored) residuals as M_i(y_i - X_i * bCCEMG_i)
        tytXtb <- ty - tcrossprod(tX, t(tb))
        cceres[[i]] <- tcrossprod(tMhat, t(tytXtb))
        ## std. (raw) residuals as y_i - X_i * bCCEMG_i - a_i
        ta <- mean(ty - tX)
        stdres[[i]] <- tytXtb - ta
    }

    ## module for making transformed data My, MX for vcovHC use
    ## (NB M is symmetric)
    ## Some redundancy because this might be moved to model.matrix.pcce

    ## initialize
    tX1 <- X[ind == unind[1L], , drop = FALSE]
    ty1 <- y[ind == unind[1L]]
    tHhat1 <- Hhat[ind == unind[1L], , drop = FALSE]

    ## if 'trend' then augment the xs-invariant component
    if(trend) tHhat1 <- cbind(tHhat1, 1:(dim(tHhat)[[1L]]))

    ## NB tHat, tMhat should be i-invariant (but beware of unbalanced)
    tMhat1 <- diag(1, length(ty1)) -
        tHhat1 %*% solve(crossprod(tHhat1), t(tHhat1))
    MX <- crossprod(tMhat1, tX1)
    My <- crossprod(tMhat1, ty1)
    for(i in 2:n) {
        tX <- X[ind == unind[i], , drop = FALSE]
        ty <- y[ind == unind[i]]
        tHhat <- Hhat[ind == unind[i], , drop = FALSE]

        ## if 'trend' then augment the xs-invariant component
        if(trend) tHhat <- cbind(tHhat, 1:(dim(tHhat)[[1L]]))

        ## NB tHat, tMhat should be i-invariant
        tMhat <- diag(1, length(ty)) -
            tHhat %*% solve(crossprod(tHhat), t(tHhat))
        tMX <- crossprod(tMhat, tX)
        tMy <- crossprod(tMhat, ty)

        MX <- rbind(MX, tMX)
        My <- c(My, tMy)
    }

    ## checks
    ## MX <<- MX
    ## My <<- My

    ## ALT:
    ## MXa <<- kronecker(diag(n), tMhat1) %*% X
    ## Mya <<- kronecker(diag(n), tMhat1) %*% y
    ## very same result, less efficient

    ## end data module

    ## CCEMG coefs are averages across individual regressions
    ## (here: coefs of xs-variants only!)
    coefmg <- rowMeans(tcoef) # was: apply(tcoef, 1, mean)

    ## make matrix of cross-products of demeaned individual coefficients
    Rmat <- array(data = NA_real_, dim = c(k, k, n))

    ## make b_i - b_CCEMG
    demcoef <- tcoef - coefmg # coefmg gets recycled n times by column

    ## calc. coef and vcov according to model
    switch(model,
           "mg" = {
               ## assign beta CCEMG
               coef <- coefmg
               for(i in 1:n) Rmat[ , , i] <- outer(demcoef[ , i], demcoef[ , i])
               vcov <- 1/(n*(n-1)) * rowSums(Rmat, dims = 2L) # == 1/(n*(n-1)) * apply(Rmat, 1:2, sum), but rowSums(., dims = 2L)-construct is way faster
           },

           "p" = {
               ## calc beta_CCEP
               sXMX <- rowSums(XMX, dims = 2L) # == apply(XMX, 1:2, sum), but rowSums(., dims = 2L)-construct is way faster
               sXMy <- rowSums(XMy, dims = 2L) # == apply(XMy, 1:2, sum), but rowSums(., dims = 2L)-construct is way faster
               coef <- solve(sXMX, sXMy)

               ## calc CCEP covariance:
               psi.star <- 1/N * sXMX

               for(i in 1:n) Rmat[ , , i] <- XMX[ , , i] %*%
                   outer(demcoef[ , i], demcoef[ , i]) %*% XMX[ , , i]
               ## summing over the n-dimension of the array we get the
               ## covariance matrix of coefs
               R.star <- 1/(n-1) * rowSums(Rmat, dims = 2L) * 1/(t^2) # rowSums(Rmat, dims = 2L) faster than == apply(Rmat, 1:2, sum)

               Sigmap.star <- solve(psi.star, R.star) %*% solve(psi.star)
               vcov <- Sigmap.star/n

               ## calc CCEP residuals both defactored and raw
               for(i in 1:n) {
                   ## must redo all this because needs b_CCEP, which is
                   ## not known at by-groups step
                   tX <- X[ind == unind[i], , drop = FALSE]
                   ty <- y[ind == unind[i]]
                   tHhat <- Hhat[ind == unind[i], , drop = FALSE]

                   ## if 'trend' then augment the xs-invariant component
                   if(trend) tHhat <- cbind(tHhat, 1:(dim(tHhat)[[1L]]))

                   ## NB tHat, tMhat should be i-invariant (but for the
                   ## group size if unbalanced)
                   tMhat <- diag(1, length(ty)) -
                       tHhat %*% solve(crossprod(tHhat), t(tHhat))

                   ## cce residuals as M_i(y_i - X_i * bCCEP)
                   tytXcoef <- ty - tcrossprod(tX, t(coef))
                   cceres[[i]] <- tcrossprod(tMhat, t(tytXcoef))
                   ## std. (raw) residuals as y_i - X_i * bCCEMG_i - a_i
                   ta <- mean(ty - tX)
                   stdres[[i]] <- tytXcoef - ta
               }
           })

    ## calc. measures of fit according to model type
    switch(model,
           "mg" = {
               ## R2 as in HPY 2010: sigma2ccemg = average (over n) of variances
               ## of defactored residuals
               ## (for unbalanced panels, each variance is correctly normalized
               ## by group dimension T.i)
               ##
               ## If balanced, would simply be
               ## sum(unlist(cceres)^2)/(n*(T.-2*k-2))

               ## pre-allocate list for individual CCEMG residual variances
               sigma2cce.i <- vector("list", n)
               ## average variance of defactored residuals sigma2ccemg as in
               ## Holly, Pesaran and Yamagata, (3.14)
               for(i in 1:n) {
                   sigma2cce.i[[i]] <- crossprod(cceres[[i]])*
                       1/(length(cceres[[i]])-2*k-2)
               }
               sigma2cce <- 1/n*sum(unlist(sigma2cce.i, use.names = FALSE))
           },

           "p" = {
               ## variance of defactored residuals sigma2ccep as in Holly,
               ## Pesaran and Yamagata, (3.15)
               sigma2cce <- 1/(n*(T.-k-2)-k)*
                   sum(vapply(cceres, crossprod, FUN.VALUE = 0.0, USE.NAMES = FALSE))
               ## is the same as sum(unlist(cceres)^2)
           })

    ## calc. overall R2, CCEMG or CCEP depending on 'model'
    sigma2.i <- vector("list", n)
    for(i in 1:n) {
        ty <- y[ind == unind[i]]
        sigma2.i[[i]] <- as.numeric(crossprod((ty-mean(ty))))/(length(ty)-1)
    }
    sigma2y <- mean(unlist(sigma2.i, use.names = FALSE))
    r2cce <- 1 - sigma2cce/sigma2y

    ## allow outputting different types of residuals
    stdres    <- unlist(stdres)
    residuals <- unlist(cceres)

    ## add transformed data (for now a simple list)
    tr.model <- list(y = My, X = MX)
    ## so that if the model is ccepmod,
    ## > lm(ccepmod$tr.model[["y"]] ~ ccepmod$tr.model[["X"]]-1)
    ## reproduces the model results

    ## Final model object:
    ## code as in pggls, differences:
    ## - here there is no 'sigma'
    ## - there are two types of residuals
    ## - transformed data My, MX are included for vcovHC usage
    df.residual <- nrow(X) - ncol(X)
    fitted.values <- y - residuals
    coef <- as.numeric(coef)
    names(coef) <- rownames(vcov) <- colnames(vcov) <- coef.names
    dimnames(tcoef) <- list(coef.names, id.names)
    pmodel <- list(model.name = model.name)
    pccemod <- list(coefficients  = coef,
                    residuals     = residuals,
                    stdres        = stdres,
                    tr.model      = tr.model,
                    fitted.values = fitted.values,
                    vcov          = vcov,
                    df.residual   = df.residual,
                    model         = mf,
                    indcoef       = tcoef,
                    r.squared     = r2cce,
                    #cceres   = as.vector(cceres),
                    #ccemgres = as.vector(ccemgres),
                    formula       = formula,
                    call          = cl)
    pccemod <- structure(pccemod, pdim = pdim, pmodel = pmodel)
    class(pccemod) <- c("pcce", "panelmodel")
    pccemod
}

#' @rdname pcce
#' @export
summary.pcce <- function(object, vcov = NULL, ...){
    vcov_arg <- vcov
    std.err <- if (!is.null(vcov_arg)) {
        if (is.matrix(vcov_arg))   rvcov <- vcov_arg
        if (is.function(vcov_arg)) rvcov <- vcov_arg(object)
        sqrt(diag(rvcov))
    } else {
        sqrt(diag(stats::vcov(object)))
    }
    b <- object$coefficients
    z <- b/std.err
    p <- 2*pnorm(abs(z), lower.tail = FALSE)
    CoefTable <- cbind(b, std.err, z, p)
    colnames(CoefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
    object$CoefTable <- CoefTable
    y <- object$model[[1L]]
    object$tss <- tss(y)
    object$ssr <- as.numeric(crossprod(residuals(object)))
    object$rsqr <- object$r.squared #1-object$ssr/object$tss
    ## add some info to summary.pcce object
    # robust vcov (next to "normal" vcov)
    if (!is.null(vcov_arg)) {
        object$rvcov <- rvcov
        rvcov.name <- paste0(deparse(substitute(vcov)))
        attr(object$rvcov, which = "rvcov.name") <- rvcov.name
    }
    class(object) <- c("summary.pcce")
    return(object)
}

#' @rdname pcce
#' @export
print.summary.pcce <- function(x, digits = max(3, getOption("digits") - 2), width = getOption("width"), ...){
    pmodel <- attr(x, "pmodel")
    pdim   <- attr(x, "pdim")
    cat("Common Correlated Effects ")
    cat(paste(model.pcce.list[pmodel$model.name], "\n", sep = ""))
    if (!is.null(x$rvcov)) {
        cat("\nNote: Coefficient variance-covariance matrix supplied: ", attr(x$rvcov, which = "rvcov.name"), "\n", sep = "")
    }
    cat("\nCall:\n")
    print(x$call)
    cat("\n")
    print(pdim)
    cat("\nResiduals:\n")
    print(sumres(x)) # was until rev. 1178: print(summary(unlist(residuals(x))))
    cat("\nCoefficients:\n")
    printCoefmat(x$CoefTable, digits = digits)
    cat(paste("Total Sum of Squares: ",    signif(x$tss,  digits), "\n", sep=""))
    cat(paste("Residual Sum of Squares: ", signif(x$ssr,  digits), "\n", sep=""))
    cat(paste("HPY R-squared: ",           signif(x$rsqr, digits), "\n", sep=""))
    invisible(x)
}

#' @rdname pcce
#' @export
residuals.pcce <- function(object,
                           type = c("defactored", "standard"),
                           ...) {
    ## special resid() method for pcce: allows to extract either
    ## defactored residuals (default) or raw residuals
    defres <- pres(object)
    switch(match.arg(type),
           "standard" = {
               ## add panel features and names from 'defres'
               residuals <- add_pseries_features(object$stdres, index(defres))
               names(residuals) <- names(defres)
           },
           "defactored" = { residuals <- defres }
    )
    return(residuals)
}

#' @rdname pcce
#' @export
model.matrix.pcce <- function(object, ...) {
    object$tr.model$X
}

#' @rdname pcce
#' @export
pmodel.response.pcce <- function(object, ...) {
    object$tr.model$y
}

# est_ggls.R
# #' General FGLS Estimators
#'
#' General FGLS estimators for panel data (balanced or unbalanced)
#'
#'
#' `pggls` is a function for the estimation of linear panel models by
#' general feasible generalized least squares, either with or without
#' fixed effects. General FGLS is based on a two-step estimation
#' process: first a model is estimated by OLS (`model = "pooling"`),
#' fixed effects (`model = "within"`) or first differences
#' (`model = "fd"`), then its residuals are used to estimate an error
#' covariance matrix for use in a feasible-GLS analysis. This framework allows
#' the error covariance structure inside every group
#' (if `effect = "individual"`, else symmetric) of observations to be fully
#' unrestricted and is therefore robust against any type of intragroup
#' heteroskedasticity and serial correlation. Conversely, this
#' structure is assumed identical across groups and thus general FGLS
#' estimation is inefficient under groupwise heteroskedasticity. Note
#' also that this method requires estimation of \eqn{T(T+1)/2}
#' variance parameters, thus efficiency requires N >> T
#' (if `effect = "individual"`, else the opposite).
#'
#' If `model = "within"` (the default) then a FEGLS (fixed effects GLS, see
#' Wooldridge, Ch. 10.5) is estimated; if `model = "fd"` a FDGLS
#' (first-difference GLS). Setting `model = "pooling"` produces an unrestricted
#' FGLS model (see ibid.) (`model = "random"` does the same, but using this value
#' is deprecated and included only for retro--compatibility reasons).
#'
#' @aliases pggls
#' @param formula a symbolic description of the model to be estimated,
#' @param object,x an object of class `pggls`,
#' @param data a `data.frame`,
#' @param subset see [lm()],
#' @param na.action see [lm()],
#' @param effect the effects introduced in the model, one of
#'     `"individual"` or `"time"`,
#' @param model one of `"within"`, `"pooling"`, `"fd"`,
#' @param index the indexes, see [pdata.frame()],
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param \dots further arguments.
#' @return An object of class `c("pggls","panelmodel")` containing:
#'     \item{coefficients}{the vector of coefficients,}
#'     \item{residuals}{the vector of residuals,}
#'     \item{fitted.values}{the vector of fitted values,}
#'     \item{vcov}{the covariance matrix of the coefficients,}
#'     \item{df.residual}{degrees of freedom of the residuals,}
#'     \item{model}{a data.frame containing the variables used for the
#'     estimation,}
#'     \item{call}{the call,}
#'     \item{sigma}{the estimated intragroup (or cross-sectional, if
#'     `effect = "time"`) covariance of errors,}
#' @export
#' @importFrom bdsmatrix bdsmatrix
#' @author Giovanni Millo
#' @references
#'
#' \insertRef{IM:SEUN:SCHM:WOOL:99}{plm}
#'
#' \insertRef{KIEF:80}{plm}
#'
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#'
#' @keywords regression
#' @examples
#'
#' data("Produc", package = "plm")
#' zz_wi <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'                data = Produc, model = "within")
#' summary(zz_wi)
#'
#' zz_pool <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'                  data = Produc, model = "pooling")
#' summary(zz_pool)
#'
#' zz_fd <- pggls(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'                data = Produc, model = "fd")
#' summary(zz_fd)
#'
#'
pggls <- function(formula, data, subset, na.action,
                  effect = c("individual", "time"),
                  model = c("within", "pooling", "fd"),
                  index = NULL, ...)
{
    # check and match the arguments
    effect <- match.arg(effect)

    if(length(model) == 1L && model == "random") {
        msg.random <- paste0("pggls(): argument 'model = \"random\"' is deprecated, ",
                             " changed to 'model = \"pooling\"' for estimation ",
                             " of unrestricted FGLS model")
        warning(msg.random, call. = FALSE)
        model <- "pooling"
    }

    model.name <- match.arg(model)

    data.name <- paste(deparse(substitute(data)))
    cl <- match.call()
    plm.model <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "effect", "model", "index"), names(plm.model), 0)
    plm.model <- plm.model[c(1L, m)]
    plm.model[[1L]] <- as.name("plm")
    plm.model$model <- model.name
    plm.model <- eval(plm.model, parent.frame())

    mf <- model.frame(plm.model)
    index <- attr(mf, "index")
    pdim <- pdim(plm.model)
    balanced   <- pdim$balanced
    time.names <- pdim$panel.names$time.names
    id.names   <- pdim$panel.names$id.names
    coef.names <- names(coef(plm.model))
    K <- length(coef.names)

    if (model.name == "fd") {
        ## eliminate first year in indices
        nt <- pdim$Tint$nt[-1L]
        Ti <- pdim$Tint$Ti - 1
        T <- pdim$nT$T - 1
        n <- pdim$nT$n
        N <- pdim$nT$N - pdim$Tint$nt[1L]
        time.names <- pdim$panel.names$time.names[-1L]
        tind <- as.numeric(index[ , 2L])
        sel <- (tind - c(-1, tind[-length(tind)])) == 1
        index <- index[sel, ]
        id <- index[[1L]]
        time <- factor(index[[2L]], levels = attr(index[ , 2L], "levels")[-1L])
    } else {
        nt <- pdim$Tint$nt
        Ti <- pdim$Tint$Ti
        T <- pdim$nT$T
        n <- pdim$nT$n
        N <- pdim$nT$N

        id <- index[[1L]]
        time <- index[[2L]]
    }

    if (effect == "time") {
        cond <- time
        other <- id
        ncond <- T
        nother <- n
        cond.names <- time.names
        other.names <- id.names
        groupsdim <- nt
    }
    else {
        cond <- id
        other <- time
        ncond <- n
        nother <- T
        cond.names <- id.names
        other.names <- time.names
        groupsdim <- Ti
    }
    myord <- order(cond, other)
    X <- model.matrix(plm.model)[myord, , drop = FALSE]
    commonpars <- intersect(coef.names, colnames(X))
    X <- X[ , commonpars, drop = FALSE]
    y <- pmodel.response(plm.model)[myord]
    resid <- lm.fit(X, y)$residuals

    cond <- cond[myord]
    other <- other[myord]
    drop1 <- FALSE
    if (drop1 && model.name %in% c("within", "fd")) {
        ## drop one time period (e.g., first as we do here)
        ## (see Wooldridge (2002) 10.5, eq. 10.61)/Wooldridge (2010),10.5.5, eq.10.61)
        ## this is needed according to Wooldridge (2002), p.277 / Wooldridge (2010), p. 312
        ## but is not totally robust to unbalancedness, dummies etc.
        ##
        ## The function turns out to work irrespective of dropping
        ## one time period or not! Absolutely the same results!
        ## This is thx to solve.bdsmatrix() using a generalized
        ## inverse, which in this case where rank=T-1 is equivalent
        ## to discarding one year (N columns)
        ## -> as noted by Wooldridge
        ##
        ## The 'if' parameterization is just for debugging.

        numeric.t <- as.numeric(other)
        t1 <- which(numeric.t != min(numeric.t))
        X0 <- X
        y0 <- y
        X <- X[t1, ]
        y <- y[t1]
        resid <- lm.fit(X, y)$residuals
        #resid[t1]
        cond <- cond[t1]
        other <- other[t1]
        nother <- nother - 1
        other.names <- other.names[-1L]
    }
    tres <- array(NA_real_, dim = c(nother, nother, ncond),
                  dimnames = list(other.names, other.names, cond.names))
    lcnd <- levels(cond)
    if (balanced) {
        for (i in 1:ncond) {
            ut <- resid[cond == lcnd[i]]
            tres[ , , i] <- ut %o% ut
        }
        subOmega <- rowMeans(tres, dims = 2L) # == apply(tres, 1:2, mean) but faster
        omega <- bdsmatrix::bdsmatrix(rep(nother, ncond), rep(subOmega, ncond))
    } else {
        lti <- list()
        for (i in 1:ncond) {
            cond.i <- cond == lcnd[i]
            ut <- resid[cond.i]
            names(ut) <- lti[[i]] <- other[cond.i]
            out <- ut %o% ut
            tres[names(ut), names(ut), i] <- out
        }
        subOmega <- rowMeans(tres, dims = 2L, na.rm = TRUE) # == apply(tres, 1:2, mean, na.rm = TRUE) but faster
        list.cov.blocks <- list()
        for (i in 1:ncond) {
            list.cov.blocks[[i]] <- subOmega[lti[[i]], lti[[i]]]
        }
        omega <- bdsmatrix::bdsmatrix(groupsdim, unlist(list.cov.blocks, use.names = FALSE))
    }
    A <- crossprod(X, solve(omega, X))
    B <- crossprod(X, solve(omega, y))
    vcov <- solve(A)
    coef <- as.numeric(solve(A, B))
    if (drop1 && model == "within") {
        X <- X0
        y <- y0
    }
    residuals <- y - as.numeric(tcrossprod(coef, X))
    df.residual <- nrow(X) - ncol(X)
    fitted.values <- y - residuals
    names(coef) <- rownames(vcov) <- colnames(vcov) <- coef.names
    pmodel <- list(model.name = model.name, effect.name = effect)
    fullGLS <- list(coefficients  = coef,
                    residuals     = residuals,
                    fitted.values = fitted.values,
                    vcov          = vcov,
                    df.residual   = df.residual,
                    model         = mf,
                    sigma         = subOmega,
                    call          = cl,
                    formula       = plm.model$formula)

    fullGLS <- structure(fullGLS, pdim = pdim, pmodel = pmodel)
    class(fullGLS) <- c("pggls", "panelmodel")
    fullGLS
}

#' @rdname pggls
#' @export
summary.pggls <- function(object,...){
    std.err <- sqrt(diag(object$vcov))
    b <- object$coefficients
    z <- b/std.err
    p <- 2*pnorm(abs(z), lower.tail = FALSE)
    CoefTable <- cbind(b, std.err, z, p)
    colnames(CoefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
    object$CoefTable <- CoefTable
    y <- object$model[[1L]]
    object$tss <- tss(y)
    object$ssr <- as.numeric(crossprod(residuals(object)))
    object$rsqr <- 1-object$ssr/object$tss
    class(object) <- c("summary.pggls")
    return(object)
}

#' @rdname pggls
#' @export
print.summary.pggls <- function(x, digits = max(3, getOption("digits") - 2), width = getOption("width"), ...){
    pmodel <- attr(x, "pmodel")
    pdim   <- attr(x, "pdim")
    cat(paste(effect.pggls.list[pmodel$effect.name], " ",  sep = ""))
    cat(paste(model.pggls.list[ pmodel$model.name],  "\n", sep = ""))
    cat("\nCall:\n")
    print(x$call)
    cat("\n")
    print(pdim)
    cat("\nResiduals:\n")
    print(sumres(x)) # was until rev. 1176:  print(summary(unlist(residuals(x))))
    cat("\nCoefficients:\n")
    printCoefmat(x$CoefTable, digits = digits)
    cat(paste("Total Sum of Squares: ",    signif(x$tss,  digits), "\n", sep=""))
    cat(paste("Residual Sum of Squares: ", signif(x$ssr,  digits), "\n", sep=""))
    cat(paste("Multiple R-squared: ",      signif(x$rsqr, digits), "\n", sep=""))
    invisible(x)
}

#' @rdname pggls
#' @export
residuals.pggls <- function(object, ...) {
    return(pres(object))
}

# est_gmm.R
# #' Generalized Method of Moments (GMM) Estimation for Panel Data
#'
#' Generalized method of moments estimation for static or dynamic
#' models with panel data.
#'
#'
#' `pgmm` estimates a model for panel data with a generalized method
#' of moments (GMM) estimator. The description of the model to
#' estimate is provided with a multi--part formula which is (or which
#' is coerced to) a `Formula` object. The first right--hand side part
#' describes the covariates. The second one, which is mandatory,
#' describes the GMM instruments. The third one, which is optional,
#' describes the 'normal' instruments. By default, all the variables
#' of the model which are not used as GMM instruments are used as
#' normal instruments with the same lag structure as the one specified
#' in the model.
#'
#' `y~lag(y, 1:2)+lag(x1, 0:1)+lag(x2, 0:2) | lag(y, 2:99)` is similar to
#'
#' `y~lag(y, 1:2)+lag(x1, 0:1)+lag(x2, 0:2) | lag(y, 2:99) | lag(x1,
#' 0:1)+lag(x2, 0:2)`
#'
#' and indicates that all lags from 2 of `y` are used
#' as GMM instruments.
#'
#' `transformation` indicates how the model should be transformed for
#' the estimation. `"d"` gives the "difference GMM" model
#' \insertCite{@see @AREL:BOND:91}{plm}, `"ld"` the "system GMM" model
#' \insertCite{@see @BLUN:BOND:98}{plm}.
#'
#' `pgmm` is an attempt to adapt GMM estimators available within the
#' DPD library for GAUSS \insertCite{@see @AREL:BOND:98}{plm} and Ox
#' \insertCite{@see @DOOR:AREL:BOND:12}{plm} and within the xtabond2
#' library for Stata \insertCite{@see @ROOD:09}{plm}.
#'
#' @aliases pgmm
#' @param formula a symbolic description for the model to be
#'     estimated. The preferred interface is now to indicate a
#'     multi--part formula, the first two parts describing the
#'     covariates and the GMM instruments and, if any, the third part
#'     the 'normal' instruments,
#' @param object,x an object of class `"pgmm"`,
#' @param data a `data.frame` (neither factors nor character vectors
#'     will be accepted in `data.frame`),
#' @param subset see [lm()],
#' @param na.action see [lm()],
#' @param effect the effects introduced in the model, one of
#'     `"twoways"` (the default) or `"individual"`,
#' @param model one of `"onestep"` (the default) or `"twosteps"`,
#' @param collapse if `TRUE`, the GMM instruments are collapsed (default is
#'                 `FALSE`),
#' @param lost.ts the number of lost time series: if `NULL`, this is
#'     automatically computed. Otherwise, it can be defined by the
#'     user as a numeric vector of length 1 or 2. The first element is
#'     the number of lost time series in the model in difference, the
#'     second one in the model in level. If the second element is
#'     missing, it is set to the first one minus one,
#' @param transformation the kind of transformation to apply to the
#'     model: either `"d"` (the default value) for the
#'     "difference GMM" model or `"ld"` for the "system GMM" model,
#' @param fsm the matrix for the one step estimator: one of `"I"`
#'     (identity matrix) or `"G"` (\eqn{=D'D} where \eqn{D} is the
#'     first--difference operator) if `transformation="d"`, one of
#'     `"GI"` or `"full"` if `transformation="ld"`,
# TODO: fms = NULL (default)/"full"/"GI" not explained; arg fsm is not evaluated at all
#' @param index the indexes,
#' @param \dots further arguments.
#' @param robust for pgmm's summary method: if `TRUE` (default), robust inference
#'               is performed in the summary,
#' @param time.dummies for pgmm's summary method: if `TRUE`, the estimated
#'     coefficients of time dummies are present in the table of coefficients;
#'     default is `FALSE`, thus time dummies are dropped in summary's coefficient
#'     table (argument is only meaningful if there are time dummies in the model,
#'     i.e., only for `effect = "twoways"`),
#' @param digits digits,
#' @param width the maximum length of the lines in the print output.

#' @return An object of class `c("pgmm","panelmodel")`, which has the
#'     following elements:
#'
#' \item{coefficients}{the vector (or the list for fixed effects) of
#'                     coefficients,}
#' \item{residuals}{the list of residuals for each individual,}
#' \item{vcov}{the covariance matrix of the coefficients,}
#' \item{fitted.values}{the vector of fitted values,}
#' \item{df.residual}{degrees of freedom of the residuals,}
#' \item{model}{a list containing the variables used for the
#'              estimation for each individual,}
#' \item{W}{a list containing the instruments for each individual (a matrix per
#'          list element) (two lists in case of system GMM,}
# TODO: not correct W does not contain two lists for system GMM
#' \item{A1}{the weighting matrix for the one--step estimator,}
#' \item{A2}{the weighting matrix for the two--steps estimator,}
#' \item{call}{the call.}
#'
#' In addition, it has attribute `"pdim"` which contains the pdim object for
#' model.
#'
#' It has `print`, `summary` and `print.summary` methods.
#' @author Yves Croissant
#' @export
#' @importFrom MASS ginv
#' @seealso
#'
#' [sargan()] for the Hansen--Sargan test and [mtest()] for
#' Arellano--Bond's test of serial correlation.  [dynformula()] for
#' dynamic formulas (deprecated).
#' @references
#'
#' \insertAllCited{}
#'
#' @keywords regression
#' @examples
#'
#' data("EmplUK", package = "plm")
#'
#' ## Arellano and Bond (1991), table 4 col. b
#' z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
#'            + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
#'             data = EmplUK, effect = "twoways", model = "twosteps")
#' summary(z1, robust = FALSE)
#'
#' ## Blundell and Bond (1998) table 4 (cf. DPD for OX p. 12 col. 4)
#' z2 <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
#'            lag(log(capital), 0:1) | lag(log(emp), 2:99) +
#'            lag(log(wage), 2:99) + lag(log(capital), 2:99),
#'            data = EmplUK, effect = "twoways", model = "onestep",
#'            transformation = "ld")
#' summary(z2, robust = TRUE)
#'
#' \dontrun{
#' ## Same with the old formula or dynformula interface
#' ## Arellano and Bond (1991), table 4, col. b
#' z1 <- pgmm(log(emp) ~ log(wage) + log(capital) + log(output),
#'             lag.form = list(2,1,0,1), data = EmplUK,
#'             effect = "twoways", model = "twosteps",
#'             gmm.inst = ~log(emp), lag.gmm = list(c(2,99)))
#' summary(z1, robust = FALSE)
#'
#' ## Blundell and Bond (1998) table 4 (cf DPD for OX p. 12 col. 4)
#' z2 <- pgmm(dynformula(log(emp) ~ log(wage) + log(capital), list(1,1,1)),
#'             data = EmplUK, effect = "twoways", model = "onestep",
#'             gmm.inst = ~log(emp) + log(wage) + log(capital),
#'             lag.gmm = c(2,99), transformation = "ld")
#' summary(z2, robust = TRUE)
#' }
#'
pgmm <- function(formula, data, subset, na.action,
                 effect = c("twoways", "individual"),
                 model = c("onestep", "twosteps"),
                 collapse = FALSE, # TODO: collapse does not seem to be assumed a locigal in the code below but rahter a character vector
                 lost.ts = NULL,
                 transformation = c("d", "ld"),
                 fsm = NULL, # TODO: argument 'fsm' is not evaluated,
                 index = NULL, ...) {

    # yX : response / covariates, W : gmm instruments, Z : normal
    # instruments, V : time dummies

    #  cl <- match.call(expand.dots = FALSE)
    cl <- match.call(expand.dots = TRUE)
    effect <- match.arg(effect)
    model <- match.arg(model)
    transformation <- match.arg(transformation)
    namesV <- NULL

    #################################################################
    ##### 1. Backward compatibility with the old formula / dynformula
    ##### interface
    #################################################################

    if (inherits(formula, "dynformula") || length(Formula(formula))[2L] == 1L){
        if (!inherits(formula, "dynformula")){
            formula <- match.call(expand.dots = TRUE)
            m <- match(c("formula", "lag.form", "diff.form", "log.form"),names(formula),0)
            formula <- formula[c(1L, m)]
            formula[[1L]] <- as.name("dynformula")
            formula <- cl$formula <- eval(formula, parent.frame())
        }
        response.name <- paste(deparse(formula[[2L]]))
        main.lags <- attr(formula, "lag")
        if (length(main.lags[[1L]]) == 1L && main.lags[[1L]] > 1L)
            main.lags[[1L]] <- c(1L, main.lags[[1L]])
        main.lags[2:length(main.lags)] <- lapply(main.lags[2:length(main.lags)],
                                                 function(x){
                                                     if (length(x) == 1L && x != 0) x <- c(0, x)
                                                     x
                                                 })
        main.form <- dynterms2formula(main.lags, response.name)
        dots <- list(...)
        gmm.inst <- dots$gmm.inst
        lag.gmm <- dots$lag.gmm
        instruments <- dots$instruments
        gmm.form <- dynformula(gmm.inst, lag.form = lag.gmm)
        gmm.lags <- attr(gmm.form, "lag")
        gmm.lags <- lapply(gmm.lags, function(x) min(x):max(x))
        gmm.form <- dynterms2formula(gmm.lags)
        formula <- as.Formula(main.form, gmm.form)
    }

    #################################################################
    ##### 2. Extract the response/covariates, the gmm instruments and
    ##### the "normal" instruments, as a named list containing the lag
    ##### structure
    #################################################################

    x <- formula
    if (!inherits(x, "Formula")) x <- Formula(formula)
    # gmm instruments : named list with the lags, names being the variables
    gmm.form <- formula(x, rhs = 2, lhs = 0)
    gmm.lags <- dynterms(gmm.form)

    cardW <- length(gmm.lags)
    if (is.null(names(collapse))){
        if (length(collapse) == 1L){
            collapse <- as.vector(rep(collapse, cardW), mode = "list")
        }
        else{
            if (length(collapse) != cardW) stop("the 'collapse' vector has a wrong length")
        }
        names(collapse) <- names(gmm.lags)
    }
    else{
        if (any(! (names(collapse) %in% names(gmm.lags)))) stop("unknown names in the 'collapse' vector")
        else{
            bcollapse <- as.vector(rep(FALSE, cardW), mode = "list")
            names(bcollapse) <- names(gmm.lags)
            bcollapse[names(collapse)] <- collapse
            collapse <- bcollapse
        }
    }

    # covariates : named list with the lags, names being the variables
    main.form <- formula(x, rhs = 1, lhs = 1)
    main.lags <- dynterms(main.form)

    # Three possibilities for 'normal' instruments :
    # 1. the third part of the formula describes them
    # 2. all variables not used as gmm are normal instruments
    # 3. all variables are gmm instruments and therefore, there are no
    #    normal instruments except maybe time dummies

    # the third part of the formula (if any) deals with the 'normal' instruments
    if (length(x)[2L] == 3L){
        normal.instruments <- TRUE
        inst.form <- formula(x, rhs = 3, lhs = 0)
        # the . - x1 + x2 syntax is allowed, in this case update with the first part
        inst.form <- update(main.form, inst.form)
        inst.form <- formula(Formula(inst.form), lhs = 0)
        inst.lags <- dynterms(inst.form)
    }
    else{
        # the default 'normal' instruments is the subset of covariates
        # which are not used as gmm instruments
        iv <- names(main.lags)[! names(main.lags) %in% names(gmm.lags)]
        inst.lags <- main.lags[iv]
        # generate the formula for 'normal' instruments
        if (length(inst.lags) > 0L){
            normal.instruments <- TRUE
            inst.form <- dynterms2formula(inst.lags)
        }
        else{
            # the case where there are no normal instruments : set inst.form
            # and inst.lags to NULL
            normal.instruments <- FALSE
            inst.form <- NULL
            inst.lags <- NULL
        }
    }

    #################################################################
    ##### 3. How many time series are lost
    #################################################################

    if (!is.null(lost.ts)){
        if (!is.numeric(lost.ts)) stop("argument 'lost.ts' should be numeric")
        lost.ts <- as.numeric(lost.ts)
        if (!(length(lost.ts) %in% c(1L, 2L))) stop("argument 'lost.ts' should be of length 1 or 2")
        TL1 <- lost.ts[1L]
        TL2 <- if(length(lost.ts) == 1L) { TL1 - 1 } else lost.ts[2L]
    }
    else{
        # How many time series are lost? May be the maximum number of lags
        # of any covariates + 1 because of first - differencing or the
        # largest minimum lag for any gmm or normal instruments
        # min or max to select the number of lost time series?
        gmm.minlag  <- min(unlist(gmm.lags, use.names = FALSE))                                  # was (==): min(sapply(gmm.lags, min))
        inst.maxlag <- if (!is.null(inst.lags)) max(unlist(inst.lags, use.names = FALSE)) else 0 # was (==): max(sapply(inst.lags, max)) else 0
        main.maxlag <- max(unlist(main.lags, use.names = FALSE))                                 # was (==): max(sapply(main.lags, max))
        TL1 <- max(main.maxlag + 1, inst.maxlag + 1, gmm.minlag)
        TL2 <- max(main.maxlag,     inst.maxlag,     gmm.minlag - 1)
        # if TL2 = 0 (no lags), one observation is lost anyway because of
        # the differentiation of the lag instruments
        TL1 <- max(main.maxlag + 1, gmm.minlag)       ## TODO: TL1, TL2 calc. twice and prev. result overwritten!?!
        TL2 <- max(main.maxlag,     gmm.minlag - 1)
    }

    #################################################################
    ##### 4. Compute the model frame which contains the
    ##### response/covariates, the gmm instruments and the 'normal'
    ##### instruments without the lags
    #################################################################

    gmm.form <- as.formula(paste("~", paste(names(gmm.lags), collapse = "+")))
    if (!is.null(inst.form))  Form <- as.Formula(main.form, gmm.form, inst.form)
    else Form <- as.Formula(main.form, gmm.form)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "index"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("plm")
    mf$model <- NA
    mf$formula <- Form
    mf$na.action <- "na.pass"
    mf$subset <- NULL
    data <- eval(mf, parent.frame())
    index <- index(data)
    pdim <- pdim(data)
    N <- pdim$nT$n
    T <- pdim$nT$T
    balanced <- pdim$balanced

    # if the data is unbalanced, "balance" the data
    if (!balanced){
        un.id <- sort(unique(index(data, "id")))
        un.time <- sort(unique(index(data, "time")))
        rownames(data) <- paste(index(data, "id"), index(data, "time"), sep = ".")
        allRows <- as.character(t(outer(un.id, un.time, paste, sep = ".")))
        data <- data[allRows, ]
        rownames(data) <- allRows
        index <- data.frame(id = rep(un.id, each = length(un.time)),
                            time = rep(un.time, length(un.id)),
                            row.names = rownames(data))
        class(index) <- c("pindex", "data.frame")
        attr(data, "index") <- index
    }

    #################################################################
    ##### 5. Get the response/covariates matrix yX, the gmm instruments
    ##### matrix W and the normal instruments matrix inst, split by
    ##### individuals
    #################################################################

    attr(data, "formula") <- formula(main.form)
    yX <- extract.data(data)
    names.coef <- colnames(yX[[1L]])[-1L]
    if (normal.instruments){
        attr(data, "formula") <- inst.form
        Z <- extract.data(data)
    }
    else Z <- NULL
    attr(data, "formula") <- gmm.form
    W <- extract.data(data, as.matrix = FALSE)

    #################################################################
    ##### 6. Create the matrix of response/covariates, gmm instruments
    ##### and normal instruments for the diff model
    #################################################################
    # create the matrix of gmm instruments for every individual
    W1 <- lapply(W,
                 function(x){
                     u <- mapply(makegmm, x, gmm.lags, TL1, collapse, SIMPLIFY = FALSE)
                     u <- matrix(unlist(u), nrow = nrow(u[[1L]]))
                     u
                 }
    )

    # differentiate the matrix of response/covariates (and of normal
    # instruments if any) and remove T1 - 1 time series (xd is already
    # differenced)
    yX1 <- lapply(yX,
                  function(x){
                      xd <- diff(x)
                      xd <- xd[- c(1:(TL1 - 1)), , drop = FALSE]
                      xd
                  }
    )
    if (normal.instruments){
        Z1 <- lapply(Z,
                     function(x){
                         xd <- diff(x)
                         xd <- xd[- c(1:(TL1 - 1)), , drop = FALSE]
                         xd
                     }
        )
    }

    #################################################################
    ##### 7. In case of system gmm, create the matrix of
    ##### response/covariates, gmm instruments and normal instruments
    ##### for the level model and merge with the diff model
    #################################################################

    if (transformation == "ld"){
        W2 <- lapply(W,
                     function(x){
                         u <- mapply(makeW2, x, collapse, SIMPLIFY = FALSE)
                         # the matrix of instruments in difference has T - 2
                         # rows if one time series is lost (there are no gmm
                         # instruments for t = 2 but there is a moment
                         # condition with the intercept. In this case, a row
                         # of 0 should be added. Otherwise, the number of
                         # rows is just T - TL2
                         nrow.ud <- if(TL2 == 1L) { T - 2 } else { T - TL2 }
                         u <- matrix(unlist(u), nrow = nrow.ud)
                         if (TL2 == 1) u <- rbind(0, u)
                         u
                     }
        )
        # remove the relevant number of time series for data in level
        yX2 <- lapply(yX,
                      function(x){
                          x <- x[- c(0:TL2), , drop = FALSE]
                          x
                      }
        )
        if (normal.instruments){
            Z2 <- lapply(Z, function(x){x <- x[- c(0:TL2), , drop = FALSE]; x})
        }
    }

    #################################################################
    ##### 8. Add time dummies if effect = "twoways"
    #################################################################

    if (effect == "twoways"){
        namesV <- levels(index(data, which = "time"))
        if (transformation == "d"){
            V1 <- td.model.diff <- diff(diag(1, T - TL1 + 1))[, -1]
            namesV <- namesV[- c(0:(TL1))]
        }
        else{
            td <- cbind(1, rbind(0, diag(1, T - 1)))
            # remove as many columns and rows as there are lost time series
            # in level (the difference of position between rows and columns
            # is due to the fact that the first column of td is the
            # intercept and should be kept anyway
            V2 <- td[- c(1:TL2), - c(2:(2 + TL2 - 1))]
            V1 <- diff(V2)
            namesV <- c("(Intercept)", namesV[- c(0:TL2 + 1)])
        }
        for (i in 1:N){
            yX1[[i]] <- cbind(yX1[[i]], V1)
            if (transformation == "d"){
                W1[[i]] <- cbind(W1[[i]], V1)
            }
            else{
                W2[[i]] <- cbind(W2[[i]], V2)
                yX2[[i]] <- cbind(yX2[[i]], V2)
            }
        }
    }
    # A QAD fix for the bug in mtest for ld model without time.dummies
    if (effect == "individual" && transformation == "ld"){
        namesV <- levels(index(data, which = "time"))
        namesV <- c("(Intercept)", namesV[-c(0:TL2 + 1)])
    }

    #################################################################
    ##### 9. In case of unbalanced data, replace NA's by 0 and overwrite
    ##### rows for missing time series with 0
    #################################################################

    for (i in 1:N){
        narows <- apply(yX1[[i]], 1, function(z) anyNA(z))
        yX1[[i]][narows, ] <- 0
        W1[[i]][is.na(W1[[i]])] <- 0
        W1[[i]][narows, ] <- 0
        if (normal.instruments){
            Z1[[i]][is.na(Z1[[i]])] <- 0
            Z1[[i]][narows, ] <- 0
        }
        if (transformation == "ld"){
            narows <- apply(yX2[[i]], 1, function(z) anyNA(z))
            yX2[[i]][narows, ] <- 0
            W2[[i]][is.na(W2[[i]])] <- 0
            W2[[i]][narows, ] <- 0
            if (normal.instruments){
                Z2[[i]][is.na(Z2[[i]])] <- 0
                Z2[[i]][narows, ] <- 0
            }
        }
    }

    #################################################################
    ##### 10. In case of sys gmm, bdiag or rbind the diff and level
    ##### matrices
    #################################################################

    if (transformation == "ld"){
        for (i in 1:N){
            W1[[i]] <- bdiag(W1[[i]], W2[[i]])
            yX1[[i]] <- rbind(yX1[[i]], yX2[[i]])
            if (normal.instruments) Z1[[i]] <- bdiag(Z1[[i]], Z2[[i]])
        }
    }
    if (normal.instruments){
        for (i in 1:N) W1[[i]] <- cbind(W1[[i]], Z1[[i]])
    }


    #################################################################
    ##### 11. Compute the estimator
    #################################################################

    W <- W1
    yX <- yX1

    # Compute the first step matrices
    if (transformation == "d")  A1 <- tcrossprod(diff(diag(1, T - TL1 + 1)))
    if (transformation == "ld") A1 <- FSM(T - TL2, "full")  # TODO: always uses "full" but man page tells otherwise

    # compute the estimator

    ## WX <- mapply(function(x, y) crossprod(x, y), W, yX, SIMPLIFY = FALSE)
    ## WX <- Reduce("+", WX)
    ## zerolines <- which(apply(WX, 1, function(z) sum(abs(z))) == 0)
    ## for (i in 1:N) W[[i]] <- W[[i]][, - zerolines]

    WX <- mapply(function(x, y) crossprod(x, y), W, yX, SIMPLIFY = FALSE)
    Wy <- lapply(WX, function(x) x[ ,  1L])
    WX <- lapply(WX, function(x) x[ , -1L, drop = FALSE])
    A1 <- lapply(W, function(x) crossprod(t(crossprod(x, A1)), x))
    A1 <- Reduce("+", A1)
    minevA1 <- min(eigen(A1)$values)
    eps <- 1E-9
    A1 <- if(minevA1 < eps){
        warning("the first-step matrix is singular, a general inverse is used")
        ginv(A1)
    }
    else solve(A1)
    A1 <- A1 * length(W)

    WX <- Reduce("+", WX)
    Wy <- Reduce("+", Wy)
    t.CP.WX.A1 <- t(crossprod(WX, A1))
    B1 <- solve(crossprod(WX, t.CP.WX.A1))
    Y1 <- crossprod(t.CP.WX.A1, Wy)
    coefficients <- as.numeric(crossprod(B1, Y1))
    if (effect == "twoways") names.coef <- c(names.coef, namesV)
    names(coefficients) <- names.coef

    residuals <- lapply(yX,
                        function(x)
                            as.vector(x[ , 1L] - crossprod(t(x[ , -1L, drop = FALSE]), coefficients)))
    outresid <- lapply(residuals, function(x) outer(x, x))

    A2 <- mapply(function(x, y) crossprod(t(crossprod(x, y)), x), W, outresid, SIMPLIFY = FALSE)
    A2 <- Reduce("+", A2)
    minevA2 <- min(eigen(A2)$values)
    A2 <- if (minevA2 < eps) {
        warning("the second-step matrix is singular, a general inverse is used")
        ginv(A2)
    }
    else solve(A2)

    if (model == "twosteps") {
        coef1s <- coefficients
        t.CP.WX.A2 <- t(crossprod(WX, A2))
        Y2 <- crossprod(t.CP.WX.A2, Wy)
        B2 <- solve(crossprod(WX, t.CP.WX.A2))
        coefficients <- as.numeric(crossprod(B2, Y2))
        names(coefficients) <- names.coef

        # calc. residuals with coefs from 2nd step
        residuals <- lapply(yX,
                            function(x){
                                nz <- rownames(x)
                                z <- as.vector(x[ , 1L] - crossprod(t(x[ , -1L, drop = FALSE]), coefficients))
                                names(z) <- nz
                                z})
        vcov <- B2
    }
    else vcov <- B1
    rownames(vcov) <- colnames(vcov) <- names.coef

    # TODO: yX does not contain the original data (but first-diff-ed data) -> fitted.values not what you would expect
    fitted.values <- mapply(function(x, y) x[ , 1L] - y, yX, residuals)
    # fitted.values <- data[ , 1L] - unlist(residuals) # in 'data' is original data, but obs lost due to diff-ing are not dropped -> format incompatible

    if(model == "twosteps") coefficients <- list(coef1s, coefficients)

    args <- list(model          = model,
                 effect         = effect,
                 transformation = transformation,
                 #    collapse       = collapse, # TODO: this would give a list of instruments, not the logical collapse as arg input
                 namest         = namesV)

    result <- list(coefficients  = coefficients,
                   residuals     = residuals, # is a list (but documentation said for a long time 'vector'), mtest() and sargan() expect a list
                   vcov          = vcov,
                   fitted.values = fitted.values,
                   #       df.residual   = df.residual,     # TODO: df.residual is not defined here, hence the function 'df.residual' is attached by this
                   model         = yX,
                   W             = W,
                   A1            = A1,
                   A2            = A2,
                   call          = cl,
                   args          = args)

    result <- structure(result,
                        class = c("pgmm", "panelmodel"),
                        pdim = pdim)
    result
}

dynterms <- function(x){
    trms.lab <- attr(terms(x), "term.labels")
    result <- getvar(trms.lab)
    nv <- names(result)
    dn <- names(table(nv))[table(nv) > 1]
    un <- names(table(nv))[table(nv) == 1]
    resu <- result[un]
    for (i in dn){
        v <- sort(unique(unlist(result[nv == i])))
        names(v) <- NULL
        resu[[i]] <- v
    }
    resu
}

getvar <- function(x){
    x <- as.list(x)
    result <- lapply(x, function(y){
        deb <- as.numeric(gregexpr("lag\\(", y)[[1L]])
        if (deb == -1){
            lags <- 0
            avar <- y
        }
        else{
            #      inspar <- substr(y, deb + 2, nchar(y) - 1)
            inspar <- substr(y, deb + 4, nchar(y) - 1)
            coma <- as.numeric(gregexpr(",", inspar)[[1L]][1L])
            if (coma == -1){
                endvar <- nchar(inspar)
                lags <- 1
            }
            else{
                endvar <- coma - 1
                lags <- substr(inspar, coma + 1, nchar(inspar))
                lags <- eval(parse(text = lags))
            }
            avar <- substr(inspar, 1, endvar)
        }
        list(avar, lags)
    }
    )
    nres   <- sapply(result, function(x) x[[1L]])
    result <- lapply(result, function(x) x[[2L]])
    names(result) <- nres
    result
}

dynterms2formula <- function(x, response.name = NULL){
    result <- character(0)
    for (i in 1:length(x)){
        theinst <- x[[i]]
        # if the first element is zero, write the variable without lag and
        # drop the 0 from the vector
        if (theinst[1L] == 0){
            at <- names(x)[i]
            theinst <- theinst[-1L]
        }
        else{
            at <- character(0)
        }
        # if there are still some lags, write them
        if (length(theinst) > 0L){
            if (length(theinst) > 1L){
                at <- c(at, paste("lag(", names(x)[i], ",c(",
                                  paste(theinst, collapse = ","), "))", sep =""))
            }
            else{
                at <- c(at, paste("lag(", names(x)[i], ",", theinst, ")", sep =""))
            }
        }
        result <- c(result, at)
    }
    if (is.null(response.name)) as.formula(paste("~", paste(result, collapse = "+")))
    else as.formula(paste(response.name, "~", paste(result, collapse = "+")))
}

extract.data <- function(data, as.matrix = TRUE){
    # the previous version is *very* slow because :
    # 1. split works wrong on pdata.frame
    # 2. model.matrix is lapplied !
    form <- attr(data, "formula")
    trms <- terms(form)
    has.response <- attr(trms, 'response') == 1
    has.intercept <- attr(trms, 'intercept') == 1
    if (has.intercept == 1){
        # Formula is unable to update formulas with no lhs
        form <- Formula(update(formula(form), ~ . -1))
        #    form <- update(form, ~. -1)
    }
    index <- attr(data, "index")

    X <- model.matrix(form, data)
    if (has.response){
        X <- cbind(data[[1L]], X)
        colnames(X)[1L] <- deparse(trms[[2L]])
    }
    data <- split(as.data.frame(X), index[[1L]])
    time <- split(index[[2L]], index[[1L]])
    data <- mapply(
        function(x, y){
            rownames(x) <- y
            if (as.matrix) x <- as.matrix(x)
            x
        }
        , data, time, SIMPLIFY = FALSE)
    data
}

G <- function(t){
    G <- matrix(0, t, t)
    for (i in 1:(t-1)){
        G[i,   i]   <-  2
        G[i,   i+1] <- -1
        G[i+1, i]   <- -1
    }
    G[t, t] <- 2
    G
}

FD <- function(t){
    FD <- Id(t)[-1L, ]
    for (i in 1:(t-1)){
        FD[i, i] <- -1
    }
    FD
}

Id <- function(t){
    diag(1, t)
}

FSM <- function(t, fsm){
    switch(fsm,
           "I" = Id(t),
           "G" = G(t),
           "GI" = bdiag(G(t-1), Id(t)),
           "full" = rbind(cbind(G(t-1), FD(t)), cbind(t(FD(t)), Id(t)))
    )
}

makegmm <- function(x, g, TL1, collapse = FALSE){
    T <- length(x)
    rg <- range(g)
    z <- as.list((TL1 + 1):T)
    x <- lapply(z, function(y) x[max(1, y - rg[2L]):(y - rg[1L])])
    if (collapse) {
        x <- lapply(x, rev)
        m <- matrix(0, T - TL1, min(T - rg[1L], rg[2L]+1-rg[1L]))
        for (y in 1:length(x)){ m[y, 1:length(x[[y]])] <- x[[y]]}
        result <- m
    }
    else {
        lx <- vapply(x, length, FUN.VALUE = 0.0)
        n <- length(x)
        lxc <- cumsum(lx)
        before <- c(0, lxc[-n])
        after <- lxc[n] - lx - before
        result <- t(mapply(function(x, y, z)
            c(rep(0, y), x, rep(0, z)),
            x, before, after, SIMPLIFY = TRUE))
    }
    result
}


makeW2 <-function (x, collapse = FALSE){
    if(collapse) { diff(x[-c(length(x))]) }
    else {    diag(diff(x[-c(length(x))])) }
}

#' @rdname pgmm
#' @export
coef.pgmm <- function(object,...){
    model <- describe(object, "model")
    if(model == "onestep") object$coefficients
    else                   object$coefficients[[2L]]
}

#' @rdname pgmm
#' @export
summary.pgmm <- function(object, robust = TRUE, time.dummies = FALSE, ...) {
    model <- describe(object, "model")
    effect <- describe(object, "effect")
    transformation <- describe(object, "transformation")
    vv <- if(robust) vcovHC(object) else vcov(object)
    K <- if(model == "onestep") length(object$coefficients)
    else                   length(object$coefficients[[2L]])
    object$sargan <- sargan(object, "twosteps")
    object$m1 <- mtest(object, order = 1, vcov = vv)
    # TODO: catch case when order = 2 is not feasible due to too few data
    object$m2 <- mtest(object, order = 2, vcov = vv)
    object$wald.coef <- pwaldtest(object, param = "coef", vcov = vv)
    if(effect == "twoways") object$wald.td <- pwaldtest(object, param = "time", vcov = vv)
    Kt <- length(object$args$namest)
    rowsel <- if(!time.dummies && effect == "twoways") -c((K - Kt + 1):K)
    else 1:K
    std.err <- sqrt(diag(vv))
    b <- coef(object)
    z <- b / std.err
    p <- 2 * pnorm(abs(z), lower.tail = FALSE)
    coefficients <- cbind(b, std.err, z, p)
    colnames(coefficients) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
    object$coefficients <- coefficients[rowsel, , drop = FALSE]
    class(object) <- "summary.pgmm"
    object
}

#' Arellano--Bond Test of Serial Correlation
#'
#' Test of serial correlation for models estimated by GMM
#'
#' The Arellano--Bond test is a test of correlation based on the residuals of
#' the estimation. By default, the computation is done with the standard
#' covariance matrix of the coefficients.  A robust estimator of this
#' covariance matrix can be supplied with the `vcov` argument.
#'
#' @param object an object of class `"pgmm"`,
#' @param order integer: the order of the serial correlation,
#' @param vcov a matrix of covariance for the coefficients or a function to
#' compute it,
#' @param \dots further arguments (currently unused).
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant
#' @seealso [pgmm()]
#' @references
#'
#' \insertCite{AREL:BOND:91}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("EmplUK", package = "plm")
#' ar <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) +
#'            lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
#'            data = EmplUK, effect = "twoways", model = "twosteps")
#' mtest(ar, order = 1L)
#' mtest(ar, order = 2L, vcov = vcovHC)
#'
mtest <- function(object, ...) {
    UseMethod("mtest")
}

#' @rdname mtest
#' @export
mtest.pgmm <- function(object, order = 1L, vcov = NULL, ...) {
    if (!inherits(object, "pgmm")) stop("argument 'object' needs to be class 'pgmm'")
    myvcov <- vcov
    if (is.null(vcov)) vv <- vcov(object)
    else if (is.function(vcov)) vv <- myvcov(object)
    else vv <- myvcov
    model <- describe(object, "model")
    transformation <- describe(object, "transformation")
    Kt <- length(object$args$namest)

    switch(transformation,
           "d" = {
               resid <- object$residuals
               residl <- lapply(resid,
                                function(x)
                                    c(rep(0, order), x[1:(length(x) - order)]))
           },
           "ld" = {
               resid <- lapply(object$residuals,
                               function(x)
                                   c(x[-c(Kt:(2 * Kt + 1))], rep(0, Kt)))
               residl <- lapply(object$residuals,
                                function(x)
                                    c(rep(0, order), x[1:(Kt - order - 1)], rep(0, Kt)))
           })

    X <- lapply(object$model, function(x) x[ , -1L, drop = FALSE])
    W <- object$W
    A <- if(model == "onestep") object$A1 else object$A2
    EVE <- Reduce("+",
                  mapply(function(x, y) t(y) %*% x %*% t(x) %*% y, resid, residl, SIMPLIFY = FALSE))
    EX <- Reduce("+", mapply(crossprod, residl, X, SIMPLIFY = FALSE))
    XZ <- Reduce("+", mapply(crossprod, W,      X, SIMPLIFY = FALSE))
    ZVE <- Reduce("+",
                  mapply(function(x, y, z) t(x) %*% y %*% t(y) %*% z, W, resid, residl, SIMPLIFY = FALSE))

    denom <- EVE - 2 * EX %*% vcov(object) %*% t(XZ) %*% A %*% ZVE + EX %*% vv %*% t(EX)
    num <- Reduce("+", mapply(crossprod, resid, residl, SIMPLIFY = FALSE))
    stat <- num / sqrt(denom)
    names(stat) <- "normal"
    if(!is.null(vcov)) vcov <- paste0(", vcov: ", deparse(substitute(vcov)))
    method <- paste0("Arellano-Bond autocorrelation test of degree ", order, vcov)
    pval <- 2 * pnorm(abs(stat), lower.tail = FALSE)
    mtest <- list(statistic   = stat,
                  p.value     = pval,
                  alternative = "autocorrelation present",
                  method      = method,
                  data.name   = data.name(object))
    class(mtest) <- "htest"
    mtest
}


#' @rdname pgmm
#' @export
print.summary.pgmm <- function(x, digits = max(3, getOption("digits") - 2),
                               width = getOption("width"),
                               ...) {
    model <- describe(x, "model")
    transformation <- describe(x, "transformation")
    effect <- describe(x, "effect")
    pdim <- attr(x, "pdim")
    formula <- x$call$formula
    model.text <- paste(effect.pgmm.list[effect], model.pgmm.list[model],
                        model.pgmm.transformation.list[transformation], sep = " ")
    cat(paste(model.text, "\n"))
    ## TODO: add info about collapse argument in printed output

    cat("\nCall:\n")
    print(x$call)
    cat("\n")
    print(pdim)
    ntot <- sum(unlist(x$residuals, use.names = FALSE) != 0)
    ninst <- dim(x$W[[1L]])[2L]
    cat("\nNumber of Observations Used:", ntot, sep = " ")
    #  cat("\nNumber of Instruments Used:  ", ninst, "\n", sep ="") # TODO: more checks, then activate printing
    cat("\nResiduals:\n")
    print(summary(unlist(residuals(x), use.names = FALSE)))
    cat("\nCoefficients:\n")
    printCoefmat(x$coefficients, digits = digits)

    cat("\nSargan test: ", names(x$sargan$statistic),
        "(", x$sargan$parameter, ") = ", x$sargan$statistic,
        " (p-value = ", format.pval(x$sargan$p.value,digits=digits), ")\n", sep = "")
    cat("Autocorrelation test (1): ", names(x$m1$statistic),
        " = ", x$m1$statistic,
        " (p-value = ", format.pval(x$m1$p.value, digits = digits), ")\n", sep = "")
    cat("Autocorrelation test (2): ", names(x$m2$statistic),
        " = ", x$m2$statistic,
        " (p-value = ", format.pval(x$m2$p.value,digits=digits), ")\n", sep = "")
    cat("Wald test for coefficients: ", names(x$wald.coef$statistic),
        "(",x$wald.coef$parameter,") = ", x$wald.coef$statistic,
        " (p-value = ", format.pval(x$wald.coef$p.value, digits = digits), ")\n", sep = "")

    if(effect == "twoways") {
        cat("Wald test for time dummies: ", names(x$wald.td$statistic),
            "(", x$wald.td$parameter, ") = ", x$wald.td$statistic,
            " (p-value = ", format.pval(x$wald.td$p.value, digits = digits), ")\n", sep = "")
    }
    invisible(x)
}


#' Hansen--Sargan Test of Overidentifying Restrictions
#'
#' A test of overidentifying restrictions for models estimated by GMM.
#'
#' The Hansen--Sargan test ("J test") calculates the quadratic form of the moment
#' restrictions that is minimized while computing the GMM estimator. It follows
#' asymptotically a chi-square distribution with number of degrees of freedom
#' equal to the difference between the number of moment conditions and the
#' number of coefficients.
#'
#' @param object an object of class `"pgmm"`,
#' @param weights the weighting matrix to be used for the computation of the
#' test.
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant
#' @seealso [pgmm()]
#' @references
#'
#' \insertCite{HANS:82}{plm}
#'
#' \insertCite{SARG:58}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("EmplUK", package = "plm")
#' ar <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) +
#'            lag(log(capital), 0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
#'            data = EmplUK, effect = "twoways", model = "twosteps")
#' sargan(ar)
#'
sargan <- function(object, weights = c("twosteps", "onestep")) {
    if (!inherits(object, "pgmm")) stop("argument 'object' needs to be class 'pgmm'")
    weights <- match.arg(weights)
    model <- describe(object, "model")
    Ktot <- if(model == "onestep") length(object$coefficients)
    else                   length(object$coefficients[[2L]])
    z <- as.numeric(Reduce("+", mapply(crossprod, object$W, object$residuals, SIMPLIFY = FALSE)))
    p <- ncol(object$W[[1L]])
    A <- if(weights == "onestep") object$A1 else object$A2
    stat <- as.numeric(tcrossprod(z, crossprod(z, A)))
    parameter <- p - Ktot
    names(parameter) <- "df"
    names(stat) <- "chisq"
    method <- "Sargan test"
    pval <- pchisq(stat, df = parameter, lower.tail = FALSE)
    sargan <- list(statistic = stat,
                   p.value   = pval,
                   parameter = parameter,
                   method    = method,
                   alternative = "overidentifying restrictions not valid",
                   data.name = data.name(object))
    class(sargan) <- "htest"
    sargan
}

# est_ldv.R
# #' Panel estimators for limited dependent variables
#'
#' Fixed and random effects estimators for truncated or censored
#' limited dependent variable
#'
#' `pldv` computes two kinds of models: a LSQ/LAD estimator for the
#' first-difference model (`model = "fd"`) and a maximum likelihood estimator
#' with an assumed normal distribution for the individual effects
#' (`model = "random"` or `"pooling"`).
#'
#' For maximum-likelihood estimations, `pldv` uses internally function
#' [maxLik::maxLik()] (from package \CRANpkg{maxLik}).
#'
#' @aliases pldv
#' @param formula a symbolic description for the model to be
#'     estimated,
#' @param data a `data.frame`,
#' @param subset see `lm`,
#' @param weights see `lm`,
#' @param na.action see `lm`,
#' @param model one of `"fd"`, `"random"`, or `"pooling"`,
#' @param index the indexes, see [pdata.frame()],
#' @param R the number of points for the gaussian quadrature,
#' @param start a vector of starting values,
#' @param lower the lower bound for the censored/truncated dependent
#'     variable,
#' @param upper the upper bound for the censored/truncated dependent
#'     variable,
#' @param objfun the objective function for the fixed effect model (`model = "fd"`,
#'     irrelevant for other values of the `model` argument ):
#'     one of `"lsq"` for least squares (minimise sum of squares of the residuals)
#'     and `"lad"` for least absolute deviations (minimise sum of absolute values
#'     of the residuals),
#' @param sample `"cens"` for a censored (tobit-like) sample,
#'     `"trunc"` for a truncated sample,
#' @param \dots further arguments.
#' @return For `model = "fd"`, an object of class `c("plm", "panelmodel")`, for
#'  `model = "random"` and `model = "pooling"` an object of class `c("maxLik", "maxim")`.
#'
#' @export
#' @importFrom maxLik maxLik
#' @author Yves Croissant
#' @references
#'
#' \insertRef{HONO:92}{plm}
#'
#' @keywords regression
#' @examples
#' ## as these examples take a bit of time, do not run them automatically
#' \dontrun{
#' data("Donors", package = "pder")
#' library("plm")
#' pDonors <- pdata.frame(Donors, index = "id")
#'
#' # replicate Landry/Lange/List/Price/Rupp (2010), online appendix, table 5a, models A and B
#' modA <- pldv(donation ~ treatment +  prcontr, data = pDonors,
#'             model = "random", method = "bfgs")
#' summary(modA)
#' modB <- pldv(donation ~ treatment * prcontr - prcontr, data = pDonors,
#'             model = "random", method = "bfgs")
#' summary(modB)
#' }
#'
#
# TODO: check if argument method = "bfgs" is needed in example (and why)
#   -> seems strange as it is no direct argument of pldv

pldv <- function(formula, data, subset, weights, na.action,
                 model = c("fd", "random", "pooling"), index = NULL,
                 R = 20, start = NULL, lower = 0, upper = +Inf,
                 objfun = c("lsq", "lad"), sample = c("cens", "trunc"), ...){

    ## Due to the eval() construct with maxLik::maxLik we import maxLik::maxLik
    ## and re-export it via NAMESPACE as plm::maxLik with a minimal documentation
    ## pointing to the original documentation.
    ## This way, we can keep the flexibility of eval() [evaluate in parent frame]
    ## and can lessen the dependency burden by placing pkg maxLik in 'Imports'
    ## rather than 'Depends' in DESCRIPTION.

    # use the plm interface to compute the model.frame
    sample <- match.arg(sample)
    model <- match.arg(model)
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf <- cl
    m <- match(c("formula", "data", "subset", "weights", "na.action", "index"), names(mf), 0)
    mf <- mf[c(1L, m)]
    mf$model <- NA
    mf[[1L]] <- as.name("plm")
    mf <- eval(mf, parent.frame())
    formula <- attr(mf, "formula")

    # extract the relevant arguments for maxLik
    maxl <- cl
    m <- match(c("print.level", "ftol", "tol", "reltol",
                 "gradtol", "steptol", "lambdatol", "qrtol",
                 "iterlim", "fixed", "activePar", "method"), names(maxl), 0)
    maxl <- maxl[c(1L, m)]
    maxl[[1L]] <- as.name("maxLik")

    # The within model -> Bo Honore (1992)
    if (model == "fd"){
        objfun <- match.arg(objfun)
        # create a data.frame containing y_t and y_{t-1}
        y <- as.character(formula[[2L]])
        y <- mf[[y]]
        ly <- c(NA, y[1:(length(y) - 1)])
        id <- as.integer(index(mf, "id"))
        lid <- c(NA, id[1:(nrow(mf) - 1)])
        keep <- id == lid
        keep[1L] <- FALSE
        Y <- data.frame(y, ly)
        Y <- Y[keep, ]
        yt <- Y$y
        ytm1 <- Y$ly
        # create the matrix of first differenced covariates
        X <- model.matrix(mf, model = "fd")
        start <- rep(.1, ncol(X))
        names(start) <- colnames(X)
        if (sample == "trunc"){
            if (objfun == "lad") fm <- function(x) abs(x)
            if (objfun == "lsq") fm <- function(x) x ^ 2
            psi <- function(a1, a2, b){
                fm( (a2 <= b) * a1 +
                        (b > - a1 & b < a2) * (a2 - a1 - b) +
                        (a1 <= - b) * a2
                )
            }
        }
        if (sample == "cens"){
            if (objfun == "lad"){
                psi <- function(a1, a2, b){
                    (a1 <= pmax(0, - b) & a2 <= pmax(0, b)) * 0 +
                        (! (a1 <= pmax(0, - b) & a2 <= pmax(0, b)) ) * abs(a2 - a1 - b)
                }
            }
            if (objfun == "lsq"){
                psi <- function(a1, a2, b){
                    (a2 <= b) * (a1 ^ 2 - 2 * a1 * (a2 - b)) +
                        (b > - a1 & b < a2) * (a2 - a1 - b) ^ 2 +
                        (a1 <= - b) * (a2 ^ 2 - 2 * a2 * (b + a1))
                }
            }
        }
        BO <- function(param){
            bdx <- as.numeric(X %*% param)
            lnl <- - psi(ytm1, yt, bdx)
            selobs <- (bdx > - ytm1 & bdx < yt)
            if (objfun == "lsq" && sample == "cens"){
                attr(lnl, "gradient") <- -
                    ( (ytm1 > - bdx & yt > bdx) * (- 2 * (yt - ytm1 - bdx)) +
                          (ytm1 > - bdx & yt < bdx) * (  2 * ytm1) +
                          (ytm1 < - bdx & yt > bdx) * (- 2 * yt) ) * X
                attr(lnl, "hessian") <-  - crossprod( (ytm1 > - bdx & yt > bdx) * X)
            }
            lnl
        }
        maxl[c("logLik", "start")] <- list(BO, start)
        result <- eval(maxl, parent.frame())
        if (objfun == "lsq" && sample == "cens"){
            bdx <- as.numeric((crossprod(t(X), coef(result))))
            V4 <- yt ^ 2 * (bdx <= - ytm1) + ytm1 ^ 2 * (yt <= bdx) +
                (yt - ytm1 - bdx) ^ 2 * (bdx > - ytm1 & bdx < yt)
            V4 <- crossprod(X, V4 * X) / length(V4)
            T4 <- crossprod((bdx > - ytm1 & bdx < yt) * X, X) / length(V4)
            solve_T4 <- solve(T4)
            vcov <- solve_T4 %*% V4 %*% solve_T4
            result$vcov <- V4
        }
        if (is.null(result$vcov)) result$vcov <- solve(- result$hessian)
        resid <- yt - as.numeric(crossprod(t(X), coef(result)))
        result <- list(coefficients = coef(result),
                       vcov         = result$vcov,
                       formula      = formula,
                       model        = mf,
                       df.residual  = nrow(X) - ncol(X),
                       residuals    = resid,
                       args         = list(model = "fd", effect = "individual"),
                       call         = cl)
        class(result) <- c("plm", "panelmodel")
    }
    else{ # model != "fd" => cases model = "random" / "pooling"

        # old pglm stuff for the pooling and the random model, with
        # update to allow upper and lower bonds
        X <- model.matrix(mf, rhs = 1, model = "pooling", effect = "individual")

        if (ncol(X) == 0L) stop("empty model")
        y <- pmodel.response(mf, model = "pooling", effect = "individual")
        id <- attr(mf, "index")[[1L]]

        # The following is the only instance of statmod::gauss.quad, so check for
        # the package's availability. (We placed 'statmod' in 'Suggests' rather
        # than 'Imports' so that it is not an absolutely required dependency.)
        ## Procedure for pkg check for pkg in 'Suggests' as recommended in
        ## Wickham, R packages (http://r-pkgs.had.co.nz/description.html).
        if (!requireNamespace("statmod", quietly = TRUE)) {
            stop(paste("Function 'gauss.quad' from package 'statmod' needed for this function to work.",
                       "Please install it, e.g., with 'install.packages(\"statmod\")"),
                 call. = FALSE)
        }
        # compute the nodes and the weights for the gaussian quadrature
        rn <- statmod::gauss.quad(R, kind = 'hermite')
        # compute the starting values
        ls <- length(start)
        if (model == "pooling"){
            K <- ncol(X)
            if (! ls %in% c(0, K + 1)) stop("irrelevant length for the start vector")
            if (ls == 0L){
                m <- match(c("formula", "data", "subset", "na.action"), names(cl), 0)
                lmcl <- cl[c(1,m)]
                lmcl[[1L]] <- as.name("lm")
                lmcl <- eval(lmcl, parent.frame()) # eval stats::lm()
                sig2 <- deviance(lmcl) / df.residual(lmcl)
                sigma <- sqrt(sig2)
                start <- c(coef(lmcl), sd.nu = sigma)
            }
        }
        else{ # case model != "pooling" and != "fd" => model ="random"
            if (ls <= 1L){
                startcl <- cl
                startcl$model <- "pooling"
                startcl$method <- "bfgs"
                pglmest <- eval(startcl, parent.frame()) # eval pldv() with updated args
                thestart <- coef(pglmest)
                if (ls == 1L){
                    start <- c(thestart, start)
                }
                else{
                    # case ls = 0
                    resid <- y -  as.numeric(tcrossprod(X, t(coef(pglmest)[1:ncol(X)])))
                    eta <- tapply(resid, id, mean)[as.character(id)]
                    nu <- resid - eta
                    start <- c(thestart[1:ncol(X)], sd.nu = sd(nu), sd.eta = sd(eta))
                }
            }
        }
        # call to maxLik with the relevant arguments
        argschar <- function(args){
            paste(as.character(names(args)), as.character(args),
                  sep= "=", collapse= ",")
        }
        args <- list(param = "start",
                     y = "y", X = "X", id = "id", model = "model",
                     rn = "rn", lower = lower, upper = upper)
        thefunc <- paste("function(start) lnl.tobit", "(", argschar(args), ")", sep = "")
        maxl$logLik <- eval(parse(text = thefunc))
        maxl$start <- start
        result <- eval(maxl, parent.frame())
        result[c('call', 'args', 'model')] <- list(cl, args, data)
    } # end cases model = "random" / "pooling"
    result
}


lnl.tobit <- function(param, y, X, id, lower = 0, upper = +Inf, model = "pooling", rn = NULL){
    compute.gradient <- TRUE
    compute.hessian <- FALSE
    mills <- function(x) exp(dnorm(x, log = TRUE) - pnorm(x, log.p = TRUE))
    O <- length(y)
    K <- ncol(X)
    beta <- param[1L:K]
    sigma <- param[K + 1L]
    Xb <- as.numeric(crossprod(t(X), beta))
    YLO <- (y == lower)
    YUT <- (y > lower) & (y < upper)
    YUP <- y == upper
    if (model == "random"){
        R <- length(rn$nodes)
        seta <- param[K + 2L]
    }
    else seta <- 0

    f <- function(i = NA){
        result <- numeric(length = length(y))
        z <- if(is.na(i)) 0 else rn$nodes[i]
        e <- (y - Xb - sqrt(2) * seta * z) / sigma
        result[YLO] <- pnorm(  e[YLO], log.p = TRUE)
        result[YUT] <- dnorm(  e[YUT], log = TRUE) - log(sigma)
        result[YUP] <- pnorm(- e[YUP], log.p = TRUE)
        result
    }

    g <- function(i = NA){
        z <- if(is.na(i)) 0 else rn$nodes[i]
        e <- (y - Xb - sqrt(2) * seta * z) / sigma
        mz <-  mills(e)
        mmz <- mills(- e)
        gradi <- matrix(0, nrow = nrow(X), ncol = ncol(X) + 1L)
        gradi[YLO, 1L:K]   <- - mz[YLO] * X[YLO, , drop = FALSE]
        gradi[YLO, K + 1L] <- -  e[YLO] * mz[YLO]
        gradi[YUT, 1L:K]   <-    e[YUT] * X[YUT, , drop = FALSE]
        gradi[YUT, K + 1L] <- - (1 - e[YUT] ^ 2)
        gradi[YUP, 1L:K]   <-  mmz[YUP] *  X[YUP, , drop = FALSE]
        gradi[YUP, K + 1L] <-    e[YUP] * mmz[YUP]
        if (! is.na(i)){
            gradi <- cbind(gradi, NA)
            gradi[YLO, K + 2L] <- - mz[YLO] * sqrt(2) * z
            gradi[YUT, K + 2L] <-    e[YUT] * sqrt(2) * z
            gradi[YUP, K + 2L] < - mmz[YUP] * sqrt(2) * z
        }
        gradi / sigma
    }

    h <- function(i = NA, pwnt = NULL){
        if (is.na(i)){
            z <- 0
            seta <- 0
            pw <- 1
        }
        else{
            z <- rn$nodes[i]
            pw <- pwnt[[i]]
        }
        e <- (y - Xb - sqrt(2) * seta * z) / sigma
        mz <-  mills(e)
        mmz <- mills(- e)
        hbb <- hbs <- hss <- numeric(length = nrow(X)) # pre-allocate
        hbb[YLO] <- - (e[YLO] + mz[YLO]) * mz[YLO]
        hbs[YLO] <-          mz[YLO] * (1 - (e[YLO] + mz[YLO]) * e[YLO])
        hss[YLO] <- e[YLO] * mz[YLO] * (2 - (e[YLO] + mz[YLO]) * e[YLO])
        hbb[YUT] <- - 1
        hbs[YUT] <- - 2 * e[YUT]
        hss[YUT] <- (1 - 3 * e[YUT] ^ 2)
        hbb[YUP] <- - (- e[YUP] + mmz[YUP]) * mmz[YUP]
        hbs[YUP] <-          - mmz[YUP] * (1 + (mmz[YUP] - e[YUP]) * e[YUP])
        hss[YUP] <- - e[YUP] * mmz[YUP] * (2 + (mmz[YUP] - e[YUP]) * e[YUP])
        hbb <- crossprod(hbb * X * pw, X)
        hbs <- apply(hbs * X * pw, 2, sum) # TODO: can use colSums -> faster
        hss <- sum(hss * pw)
        H <- rbind(cbind(hbb, hbs), c(hbs, hss))
        if (! is.na(i)){
            hba <- hsa <- haa <- numeric(length = nrow(X))
            hba[YLO] <- - (e[YLO] + mz[YLO]) * mz[YLO] * sqrt(2) * z
            hsa[YLO] <-   mz[YLO] * sqrt(2) * z * (1 - (e[YLO] + mz[YLO]) * e[YLO])
            haa[YLO] <- - (e[YLO] + mz[YLO]) * mz[YLO] * 2 * z ^ 2
            hba[YUT] <- - sqrt(2) * z
            hsa[YUT] <- - 2 * sqrt(2) * z * e[YUT]
            haa[YUT] <- - 2 * z ^ 2
            hba[YUP] <- - (- e[YUP] + mmz[YUP]) * mmz[YUP] * sqrt(2) * z
            hsa[YUP] <- - mmz[YUP] * sqrt(2) * z * (1 + (- e[YUP] + mmz[YUP]) * e[YUP])
            haa[YUP] <- - (- e[YUP] + mmz[YUP]) * mmz[YUP] * 2 * z ^ 2
            hba <- apply(hba * X * pw, 2, sum) # TODO: can use colSums -> faster
            haa <- sum(haa * pw)
            hsa <- sum(hsa * pw)
            H <- rbind(cbind(H, c(hba, hsa)), c(hba, hsa, haa))
        }
        H / sigma ^ 2
    }

    if (model == "pooling"){
        lnL <- sum(f(i = NA))
        if (compute.gradient) attr(lnL, "gradient") <- g(i = NA)
        if (compute.hessian)  attr(lnL, "hessian")  <- h(i = NA)
    }
    if (model == "random"){
        lnPntr <- lapply(1:R, function(i)  f(i = i))
        lnPnr <- lapply(lnPntr, function(x){
            result <- tapply(x, id, sum)
            ids <- names(result)
            result <- as.numeric(result)
            names(result) <- ids
            result
        }
        )
        lnPn <- lapply(1:R, function(i) rn$weights[i] * exp(lnPnr[[i]]))
        lnPn <- log(Reduce("+", lnPn)) - 0.5 * log(pi)
        lnL <- sum(lnPn)
        if (compute.gradient || compute.hessian){
            glnPnr  <- lapply(1:R, function(i) g(i = i))
            pwn     <- lapply(1:R, function(i) exp(lnPnr[[i]] - lnPn))
            pwnt    <- lapply(1:R, function(i) pwn[[i]][as.character(id)])
            glnPnr2 <- lapply(1:R, function(i) rn$weights[i] * pwnt[[i]]  * glnPnr[[i]])
            gradi <- Reduce("+", glnPnr2) / sqrt(pi)
            attr(lnL, "gradient") <- gradi
        }
        if (compute.hessian){
            hlnPnr <- lapply(1:R, function(i) h(i = i, pwnt = pwnt))
            daub <- lapply(1:R, function(i) apply(glnPnr[[i]], 2, tapply, id, sum) * pwn[[i]] * rn$weights[i])
            daub <- Reduce("+", daub) / sqrt(pi)
            DD1 <- - crossprod(daub)
            DD2 <- lapply(1:R, function(i) rn$weights[i] * hlnPnr[[i]])
            DD2 <- Reduce("+", DD2) / sqrt(pi)
            DD3 <- lapply(1:R, function(i) rn$weights[i] * crossprod(sqrt(pwn[[i]]) * apply(glnPnr[[i]], 2, tapply, id, sum)))
            DD3 <- Reduce("+", DD3) / sqrt(pi)
            H <- (DD1 + DD2 + DD3)
            attr(lnL, "hessian") <- H
        }
    }
    lnL
}

# est_mg.R
# ## Mean Group estimator
## ref. Coakley, Fuertes and Smith 2004
##
## This version 10:
##   added R2 = 1-var(resid)/var(y) as a measure of fit
## from version 9:
##   fixed residuals
##   output matrix of individual coefficients as 'indcoef' aptly named

## NB the effect of including a trend is exactly the same as for
## including as.numeric(<timeindex>) in the model specification
## Yet it is cleaner unless some automatic treatment of group invariant
## variates is added for the CCE case (where else any group invariant
## becomes perfectly collinear with the ybar, Xbar and gives NAs in coefs.
## Moreover, if the panel is unbalanced then for some i the trend becomes
## (3,4,5, ...) instead of (1,2,3, ...); the difference is absorbed by
## the individual intercept, and *the group intercept* changes.

## TODO: see last point above: treatment of invariants

## TODO: see how to estimate the intercept in cmg, dmg

## TODO: manage models without intercept in cmg, dmg

## TODO: output single coefs (see how the structure of pvcm is)

## needed for standalone operation:
#plm <- plm:::plm
#pdim <- plm:::pdim

#model.matrix.plm <- plm:::model.matrix.plm
#pmodel.response <- plm:::pmodel.response.plm




#' Mean Groups (MG), Demeaned MG and CCE MG estimators
#'
#' Mean Groups (MG), Demeaned MG (DMG) and Common Correlated Effects
#' MG (CCEMG) estimators for heterogeneous panel models, possibly with
#' common factors (CCEMG)
#'
#' `pmg` is a function for the estimation of linear panel models with
#' heterogeneous coefficients by various Mean Groups estimators. Setting
#' argument `model = "mg"` specifies the standard Mean Groups estimator, based on the
#' average of individual time series regressions. If `model = "dmg"`
#' the data are demeaned cross-sectionally, which is believed to
#' reduce the influence of common factors (and is akin to what is done
#' in homogeneous panels when `model = "within"` and `effect = "time"`).
#' Lastly, if `model = "cmg"` the CCEMG estimator is
#' employed which is consistent under the hypothesis of
#' unobserved common factors and idiosyncratic factor loadings; it
#' works by augmenting the model by cross-sectional averages of the
#' dependent variable and regressors in order to account for the
#' common factors, and adding individual intercepts and possibly
#' trends.
#'
#' @aliases pmg
#' @param formula a symbolic description of the model to be estimated,
#' @param object,x an object of class `pmg`,
#' @param data a `data.frame`,
#' @param subset see [lm()],
#' @param na.action see [lm()],
#' @param model one of `"mg"`, `"cmg"`, or `"dmg"`,
#' @param index the indexes, see [pdata.frame()],
#' @param trend logical specifying whether an individual-specific
#'     trend has to be included,
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param \dots further arguments.
#'
#' @return An object of class `c("pmg", "panelmodel")` containing:
#'     \item{coefficients}{the vector of coefficients,}
#'     \item{residuals}{the vector of residuals,}
#'     \item{fitted.values}{the vector of fitted values,}
#'     \item{vcov}{the covariance matrix of the coefficients,}
#'     \item{df.residual}{degrees of freedom of the residuals,}
#'     \item{model}{a data.frame containing the variables used for the
#'                  estimation,}
#'     \item{r.squared}{numeric, the R squared,}
#'     \item{call}{the call,}
#'     \item{indcoef}{the matrix of individual coefficients from
#'                    separate time series regressions.}
#' @export
#' @author Giovanni Millo
#' @references
#'
#' \insertRef{PESA:06}{plm}
#'
#' @keywords regression
#' @examples
#' data("Produc", package = "plm")
#' ## Mean Groups estimator
#' mgmod <- pmg(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)
#' summary(mgmod)
#'
#' ## demeaned Mean Groups
#' dmgmod <- pmg(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'              data = Produc, model = "dmg")
#' summary(dmgmod)
#'
#' ## Common Correlated Effects Mean Groups
#' ccemgmod <- pmg(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'                 data = Produc, model = "cmg")
#' summary(ccemgmod)
pmg <- function(formula, data, subset, na.action,
                model = c("mg", "cmg", "dmg"), index = NULL,
                trend = FALSE, ...) {

    ## same as pggls but for effect, fixed at "individual" for compatibility
    ## ind for id, tind for time, k for K, coefnam for coef.names
    effect <- "individual"

    ## record call etc.
    model <- match.arg(model)
    model.name <- model
    data.name <- paste(deparse(substitute(data)))
    cl <- match.call()
    plm.model <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "effect",
                 "model", "index"), names(plm.model), 0)
    plm.model <- plm.model[c(1L, m)]
    plm.model[[1L]] <- as.name("plm")
    ## change the 'model' in call
    plm.model$model <- "pooling"
    ## evaluates the call, modified with model = "pooling", inside the
    ## parent frame resulting in the pooling model on formula, data
    plm.model <- eval(plm.model, parent.frame())
    mf <- model.frame(plm.model)
    index <- unclass(attr(mf, "index")) # unclass for speed
    ind  <- index[[1L]] ## individual index
    tind <- index[[2L]] ## time index
    ## set dimension variables
    pdim <- pdim(plm.model)
    balanced <- pdim$balanced
    nt <- pdim$Tint$nt
    Ti <- pdim$Tint$Ti
    T. <- pdim$nT$T
    n <- pdim$nT$n
    N <- pdim$nT$N
    ## set index names
    time.names <- pdim$panel.names$time.names
    id.names   <- pdim$panel.names$id.names
    coef.names <- names(coef(plm.model))
    ## number of coefficients
    k <- length(coef.names)

    ## model data
    X <- model.matrix(plm.model)
    y <- model.response(mf)


    ## det. *minimum* group numerosity
    t <- min(Ti) # == min(tapply(X[ , 1], ind, length))

    ## check min. t numerosity
    ## NB it is also possible to allow estimation if there *is* one group
    ## with t large enough and average on coefficients removing NAs
    ## Here we choose the explicit way: let estimation fail if we lose df
    ## but a warning would do...
    if(t < (k+1)) stop("Insufficient number of time periods")

    ## one regression for each group i in 1..n
    ## and retrieve coefficients putting them into a matrix
    ## (might be unbalanced => t1!=t2 but we don't care as long
    ## as min(t)>k+1)

    ## "pre-allocate" coefficients matrix for the n models
    kt <- if (trend) 1L else 0L
    tcoef <- matrix(data = NA_real_, nrow = k+kt, ncol = n)
    tres <- vector("list", n)

    switch(model,
           "mg" = {
               ## for each x-sect. i = 1..n
               unind <- unique(ind)
               for(i in 1:n) {
                   tX <- X[ind == unind[i], ]
                   ty <- y[ind == unind[i]]
                   if(trend) tX <- cbind(tX, 1:(dim(tX)[[1L]]))
                   tfit <- lm.fit(tX, ty)
                   tcoef[ , i] <- tfit$coefficients
                   tres[[i]]   <- tfit$residuals
               }
               ## 'trend' always comes last
               if(trend) coef.names <- c(coef.names, "trend")
               ## adjust k
               k <- length(coef.names)
           },

           "cmg" = {
               ## between-periods transformation (take means over groups for each t)
               Xm <- Between(X, effect = "time", na.rm = TRUE)
               ym <- as.numeric(Between(y, effect = "time", na.rm = TRUE))

               augX <- cbind(X, ym, Xm[ , -1L, drop = FALSE])

               ## allow for extended coef vector
               tcoef0 <- matrix(data = NA_real_, nrow = 2*k+kt, ncol = n)

               ## for each x-sect. i = 1..n estimate (over t) an augmented model
               ## y_it = alpha_i + beta_i*X_it + c1_i*my_t + c2_i*mX_t + err_it
               unind <- unique(ind)
               for(i in 1:n) {
                   taugX <- augX[ind == unind[i], ] # TODO: check if this kind of extractions need drop = FALSE for corner cases
                   ty    <-    y[ind == unind[i]]
                   if(trend) taugX <- cbind(taugX, 1:(dim(taugX)[[1L]]))
                   tfit <- lm.fit(taugX, ty)
                   tcoef0[ , i] <- tfit$coefficients
                   tres[[i]]    <- tfit$residuals
               }
               tcoef     <- tcoef0[1:k, ] # TODO: this line seems superfluous as tcoef is overwritten a few lines below again
               tcoef.bar <- tcoef0[-(1:k), ]

               coef.names.bar <- c("y.bar", paste(coef.names[-1L], ".bar", sep=""))

               ## 'trend' always comes last
               if(trend) coef.names.bar <- c(coef.names.bar, "trend")

               ## output complete coefs
               tcoef <- tcoef0
               coef.names <- c(coef.names, coef.names.bar)
               ## adjust k
               k <- length(coef.names)

               ## TODO: adjust model formula etc. (else breaks waldtest, update, ...)
           },

           "dmg" = {
               ##  time-demean
               demX <- Within(X, effect = "time", na.rm = TRUE)
               demX[ , 1L] <- 1 # put back intercept lost by within transformation
               demy <- as.numeric(Within(y, effect = "time", na.rm = TRUE))

               ## for each x-sect. i=1..n estimate (over t) a demeaned model
               ## (y_it-my_t) = alpha_i + beta_i*(X_it-mX_t) + err_it
               unind <- unique(ind)
               for (i in 1:n) {
                   tdemX <- demX[ind == unind[i], ]
                   tdemy <- demy[ind == unind[i]]
                   if(trend) tdemX <- cbind(tdemX, 1:(dim(tdemX)[[1L]]))
                   tfit <- lm.fit(tdemX, tdemy)
                   tcoef[ , i] <- tfit$coefficients
                   tres[[i]]   <- tfit$residuals
               }
               ## 'trend' always comes last
               if(trend) coef.names <- c(coef.names, "trend")
               ## adjust k
               k <- length(coef.names)
           })

    ## coefs are averages across individual regressions
    coef <- rowMeans(tcoef) # == apply(tcoef, 1, mean)

    ## make matrix of cross-products of demeaned individual coefficients
    coefmat <- array(data = NA_real_, dim = c(k, k, n))
    demcoef <- tcoef - coef # gets recycled n times by column

    for (i in 1:n) coefmat[ , , i] <- outer(demcoef[ , i], demcoef[ , i])
    ## summing over the n-dimension of the array we get the
    ## covariance matrix of coefs
    vcov <- rowSums(coefmat, dims = 2L) / (n*(n-1)) # == apply(coefmat, 1:2, sum) / (n*(n-1)) but rowSums(., dims = 2L)-construct is way faster

    ######### na.omit = T in apply was the big problem!!

    ## code as in pggls, only difference is here there is no 'sigma'
    residuals <- unlist(tres)
    ##was: as.vector(y) - as.vector(crossprod(t(X), coef[1:(dim(X)[[2]])]))
    df.residual <- nrow(X) - ncol(X)
    fitted.values <- y - residuals

    ## R2 as 1-var(res)/var(y);
    ## originally (HPY 3.14) adjusted by *(T.-1)/(T.-2*k0-2)
    ## but here k has expanded to include ybar, Xbar, (trend)
    r2 <- 1-var(residuals)/var(y)*(T.-1)/(T.-k-1)

    names(coef) <- rownames(vcov) <- colnames(vcov) <- coef.names
    dimnames(tcoef) <- list(coef.names, id.names)
    pmodel <- list(model.name = model.name)
    mgmod <- list(coefficients  = coef,
                  residuals     = residuals,
                  fitted.values = fitted.values,
                  vcov          = vcov,
                  df.residual   = df.residual,
                  r.squared     = r2,
                  model         = mf,
                  indcoef       = tcoef,
                  formula       = formula,
                  call          = cl)
    mgmod <- structure(mgmod, pdim = pdim, pmodel = pmodel)
    class(mgmod) <- c("pmg", "panelmodel")
    mgmod
}

#' @rdname pmg
#' @export
summary.pmg <- function(object, ...){
    std.err <- sqrt(diag(object$vcov))
    b <- object$coefficients
    z <- b/std.err
    p <- 2*pnorm(abs(z), lower.tail = FALSE)
    CoefTable <- cbind(b, std.err, z, p)
    colnames(CoefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
    object$CoefTable <- CoefTable
    y <- object$model[[1L]]
    object$tss <- tss(y)
    object$ssr <- as.numeric(crossprod(residuals(object)))
    object$rsqr <- 1-object$ssr/object$tss
    class(object) <- c("summary.pmg")
    return(object)
}

#' @rdname pmg
#' @export
print.summary.pmg <- function(x, digits = max(3, getOption("digits") - 2),
                              width = getOption("width"), ...){
    pmodel <- attr(x, "pmodel")
    pdim   <- attr(x, "pdim")
    cat(paste(model.pmg.list[pmodel$model.name], "\n", sep=""))
    cat("\nCall:\n")
    print(x$call)
    cat("\n")
    print(pdim)
    cat("\nResiduals:\n")
    print(sumres(x)) # was until rev. 1178: print(summary(unlist(residuals(x))))
    cat("\nCoefficients:\n")
    printCoefmat(x$CoefTable, digits = digits)
    cat(paste("Total Sum of Squares: ",    signif(x$tss,  digits),  "\n", sep=""))
    cat(paste("Residual Sum of Squares: ", signif(x$ssr,  digits),  "\n", sep=""))
    cat(paste("Multiple R-squared: ",      signif(x$rsqr, digits), "\n", sep=""))
    invisible(x)
}

#' @rdname pmg
#' @export
residuals.pmg <- function(object, ...) {
    return(pres(object))
}

# est_pi.R#
# #' Angrist and Newey's version of Chamberlain test for fixed effects
#'
#' Angrist and Newey's version of the Chamberlain test
#'
#' Angrist and Newey's test is based on the results of the artifactual
#' regression of the within residuals on the covariates for all the
#' periods.
#'
#' @aliases aneweytest
#' @param formula a symbolic description for the model to be estimated,
#' @param data a `data.frame`,
#' @param subset see [lm()],
#' @param na.action see [lm()],
#' @param index the indexes,
#' @param \dots further arguments.
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant
#' @references
#' \insertRef{ANGR:NEWE:91}{plm}
#'
#' @seealso [piest()] for Chamberlain's test
#' @keywords htest
#' @examples
#'
#' data("RiceFarms", package = "plm")
#' aneweytest(log(goutput) ~ log(seed) + log(totlabor) + log(size), RiceFarms, index = "id")
#'
aneweytest <- function(formula, data, subset, na.action, index = NULL,  ...){
    # NB: code fails for unbalanced data -> is Angrist and Newey's test only for balanced data?
    #     unbalanced case is currently caught and a message is printed

    mf <- match.call()
    # compute the model.frame using plm with model = NA
    mf[[1L]] <- as.name("plm")
    mf$model <- NA
    data <- eval(mf, parent.frame())
    # estimate the within model without instrument and extract the fixed
    # effects
    formula <- as.Formula(formula)
    mf$formula <- formula(formula, rhs = 1)
    index <- index(data)
    id <- index[[1L]]
    time <- index[[2L]]
    periods <- unique(time)
    pdim <- pdim(data)
    T <- pdim$nT$T
    n <- pdim$nT$n
    N <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    balanced <- pdim$balanced

    if(!balanced) stop("'aneweytest' not implemented for unbalanced data")

    ht <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action",
                 "effect", "model", "inst.method", "restict.matrix",
                 "restrict.rhs", "index"), names(ht), 0)
    ht <- ht[c(1L, m)]
    ht[[1L]] <- as.name("plm")
    ht$model <- "within"
    ht$effect <- "individual"
    ht <- eval(ht, parent.frame())

    .resid <- split(resid(ht), time)

    # extract the covariates (no intercept), and isolate time-invariant covariates
    X <- model.matrix(data, model = "pooling", rhs = 1, lhs = 1)[ , -1, drop = FALSE]
    cst <- attr(model.matrix(data, model = "within", rhs = 1, lhs = 1), "constant")

    # get constant columns and remove the intercept
    if (length(cst) > 0L) cst <- cst[- match("(Intercept)", cst)]
    if (length(cst) > 0L){
        vr <- colnames(X)[!(colnames(X) %in% cst)]
        Z <- X[ , cst, drop = FALSE]
        X <- X[ , vr,  drop = FALSE]
        Kz <- ncol(Z)
        namesZ <- colnames(Z)
    } else {
        Z <- NULL
        Kz <- 0
        namesZ <- NULL
    }

    Kx <- ncol(X)

    # time-demean and split by period:
    attr(X, "index") <- index
    X <- Within(X, effect ="time")
    X <- lapply(as.list(periods), function(x) X[time == x, , drop = FALSE])
    # put column names for split matrices in X:
    for (i in 1:(length(periods))){
        colnames(X[[i]]) <- paste(colnames(X[[i]]), periods[i], sep = ".")
    }

    if (!is.null(Z)){
        Z <- Z[time == periods[1], , drop = FALSE]
        Z <- t(t(Z) - .colMeans(Z, nrow(Z), ncol(Z))) # TODO: could use Within() framework
    }

    XX <- cbind(Reduce("cbind", X), Z)

    # compute the unconstrained estimates
    # NA-freeness guaranteed by model frame construction, so can use lm.fit
    # (non-collinearity is not catered for but code errors anywayif collinearity
    # is present a bit later)
    #   was:   LMS <- lapply(.resid, function(x) lm(x ~ XX - 1))
    LMS <- lapply(.resid, function(x) lm.fit(XX, x))

    YTOT <- vapply(.resid, function(x) crossprod(x),           FUN.VALUE = 0.0, USE.NAMES = FALSE)
    DEV  <- vapply(LMS,    function(x) crossprod(x$residuals), FUN.VALUE = 0.0, USE.NAMES = FALSE)

    stat <- c("chisq" = sum(1 - DEV / YTOT) * (n - ncol(XX)))
    df <- c("df" = (T ^ 2 - T - 1) * Kx)
    aneweytest <- structure(list(statistic   = stat,
                                 parameter   = df,
                                 method      = "Angrist and Newey's test of within model",
                                 p.value     = pchisq(stat, df = df, lower.tail = FALSE),
                                 alternative = "within specification does not apply",
                                 data.name   = paste(deparse(formula))),
                            class = "htest")
    aneweytest
}



#' Chamberlain estimator and test for fixed effects
#'
#' General estimator useful for testing the within specification
#'
#' The Chamberlain method consists in using the covariates of all the
#' periods as regressors. It allows to test the within specification.
#'
#' @aliases piest
#' @param formula a symbolic description for the model to be estimated,
#' @param object,x an object of class `"piest"` and of class `"summary.piest"`
#'                  for the print method of summary for piest objects,
#' @param data a `data.frame`,
#' @param subset see [lm()],
#' @param na.action see [lm()],
#' @param index the indexes,
#' @param robust logical, if `FALSE`, the error is assumed to be spherical,
#' if `TRUE`, a robust estimation of the covariance matrix is computed,
#' @param \dots further arguments.
#' @return An object of class `"piest"`.
#' @export
#' @author Yves Croissant
#' @references
#'
#' \insertRef{CHAM:82}{plm}
#'
#' @seealso [aneweytest()]
#' @keywords htest
#' @examples
#'
#' data("RiceFarms", package = "plm")
#' pirice <- piest(log(goutput) ~ log(seed) + log(totlabor) + log(size), RiceFarms, index = "id")
#' summary(pirice)
#'
piest <- function(formula, data, subset, na.action, index = NULL, robust = TRUE,  ...){
    # NB: code fails for unbalanced data -> is Chamberlain's test only for balanced data?
    #     unbalanced case is currently caught and a message is printed
    cl <- match.call(expand.dots = TRUE)
    mf <- match.call()
    # compute the model.frame using plm with model = NA
    mf[[1L]] <- as.name("plm")
    mf$model <- NA
    data <- eval(mf, parent.frame())
    # estimate the within model without instrument and extract the fixed
    # effects
    formula <- as.Formula(formula)
    mf$formula <- formula(formula, rhs = 1)
    index <- index(data)
    id   <- index[[1L]]
    time <- index[[2L]]
    pdim <- pdim(data)
    balanced <- pdim$balanced
    T  <- pdim$nT$T
    n  <- pdim$nT$n
    N  <- pdim$nT$N
    Ti <- pdim$Tint$Ti

    if(!balanced) stop("'piest' not implemented for unbalanced data")

    # extract the response, time-demean and split by period
    y <- pmodel.response(data, model = "pooling", effect = "individual")
    Y <- Within(y, "time")
    Y <- split(Y, time)

    # extract the covariates, and isolate time-invariant covariates
    X <- model.matrix(data, model = "pooling", rhs = 1, lhs = 1)[ , -1, drop = FALSE]
    cst <- attr(model.matrix(data, model = "within", rhs = 1, lhs = 1), "constant")

    # get constant columns and remove the intercept
    if (length(cst) > 0L) cst <- cst[- match("(Intercept)", cst)]
    if (length(cst) > 0L){
        vr <- colnames(X)[!(colnames(X) %in% cst)]
        Z <- X[ , cst, drop = FALSE]
        X <- X[ , vr, drop = FALSE]
        Kz <- ncol(Z)
        namesZ <- colnames(Z)
    } else {
        Z <- NULL
        Kz <- 0
        namesZ <- NULL
    }

    Kx <- ncol(X)
    namesX <- colnames(X)

    # time-demean X and split by period:
    attr(X, "index") <- index
    X <- Within(X, effect ="time")
    periods <- unique(time)
    X <- lapply(as.list(periods), function(x) X[time == x, , drop = FALSE])
    # put columnnames for split matrices in X:
    for (i in 1:(length(periods))){
        colnames(X[[i]]) <- paste(colnames(X[[i]]), periods[i], sep = ".")
    }

    if (!is.null(Z)){
        Z <- Z[time == periods[1L], , drop = FALSE]
        Z <- t(t(Z) - .colMeans(Z, nrow(Z), ncol(Z))) # TODO: can use Within() framework
    }

    XX <- cbind(Reduce("cbind", X), Z)

    # compute the unconstrained estimates
    # NA-freeness guaranteed by model frame construction, so can use lm.fit
    # (non-collinearity is not cared for but code error if collinearity is
    # present anyway a bit later)
    #   was:   LMS <- lapply(Y, function(x) lm(x ~ XX - 1))
    LMS <- lapply(Y, function(x) lm.fit(XX, x))

    # compute the empirical covariance of the covariates
    Sxxm1 <- solve(crossprod(XX) / n)
    # compute the residuals matrix
    .resid <- sapply(LMS, resid)
    # extract the pi vector of unconstrained estimates
    pi <- unlist(lapply(LMS, coef), use.names = FALSE)

    if(robust) {
        Omega <- lapply(seq_len(n),
                        function(i)
                            tcrossprod(.resid[i, ]) %x%
                            (Sxxm1 %*% tcrossprod(XX[i, ]) %*% Sxxm1))
        Omega <- Reduce("+", Omega) / n
    } else {
        Omega <- (crossprod(.resid) / n) %x% Sxxm1
    }

    # construct the matrix of linear restrictions R | R x theta = pi
    R <- matrix(0, T * (T * Kx + Kz), (T + 1) * Kx + Kz)
    for (i in 1:Kx){
        R[ ((1:T) - 1) * (Kx * T + Kz) + (Kx * (1:T - 1)) + i , i] <- 1
    }
    if (Kz > 0){
        for (i in 1:Kz){
            R[ (Kx * T) + (1:T - 1) * (Kx * T + Kz) + i, Kx + i] <- 1
        }
    }
    for (i in 1:(Kx * T)){
        R[((1:T) - 1) * (Kx * T + Kz) + i , Kx + Kz + i] <- 1
    }

    solve_Omega <- solve(Omega)
    A <- solve(t(R) %*% solve_Omega %*% R)
    .coef <- as.numeric(A %*% t(R) %*% solve_Omega %*% as.numeric(pi))
    #  .coef <- as.numeric(solve(t(R) %*% R) %*% t(R) %*% as.numeric(pi))
    namescoef <- if(Kz > 0)  c(namesX, namesZ, colnames(XX)[- c(ncol(XX) - 0:(Kz-1))])
    else     c(namesX, namesZ, colnames(XX))
    names(.coef) <- rownames(A) <- colnames(A) <- namescoef
    resb <- as.numeric(R %*% .coef) - as.numeric(pi)
    piconst <- matrix(R %*% .coef, ncol = T)
    OOmega <- Omega                                       ## TODO: OOmega is never used
    .resid <- matrix(unlist(Y, use.names = FALSE), ncol = length(Y)) - XX %*% piconst

    if(TRUE){                                             ## TODO: this is always TRUE...!
        if(robust) {                                      ## and Omega is calc. again, with a
            ## new .resid input but with same lapply-construct
            Omega <- lapply(seq_len(n),
                            function(i)
                                tcrossprod(.resid[i, ]) %x%
                                (Sxxm1 %*% tcrossprod(XX[i, ]) %*% Sxxm1))
            Omega <- Reduce("+", Omega) / n
        } else {
            Omega <- (crossprod(.resid) / n) %x% Sxxm1
        }
    }

    A <- solve(t(R) %*% solve(Omega) %*% R)
    stat <- c("chisq" = n * resb %*% solve(Omega) %*% resb)
    df <- c("df" = Kx * (T ^ 2 - T - 1))    ## TODO: df is overwritten in next line...?!
    df <- c("df" = length(pi) - length(.coef))

    pitest <- list(statistic   = stat,
                   parameter   = df,
                   method      = "Chamberlain's pi test",
                   p.value     = pchisq(stat, df = df, lower.tail = FALSE),
                   alternative = "within specification does not apply",
                   data.name   = paste(deparse(formula))
    )

    structure(list(coefficients = .coef,
                   pi           = pi,
                   daub         = resb,
                   vcov         = A / n,
                   formula      = formula,
                   R            = R,
                   model        = data,
                   pitest       = structure(pitest, class = "htest"),
                   Omega        = Omega,
                   moments      = resb,
                   call         = cl),
              class = c("piest", "panelmodel"))
}

#' @rdname piest
#' @export
print.piest <- function(x, ...) print(x$pitest, ...)

#' @rdname piest
#' @export
summary.piest <- function(object,...){
    # construct the table of coefficients
    std.err <- sqrt(diag(vcov(object)))
    b <- coefficients(object)
    z <- b / std.err
    p <- 2 * pnorm(abs(z), lower.tail = FALSE)
    object$coefficients <- cbind("Estimate"   = b,
                                 "Std. Error" = std.err,
                                 "z-value"    = z,
                                 "Pr(>|z|)"   = p)
    class(object) <- c("summary.piest", "piest", "panelmodel")
    object
}

#' @rdname piest
#' @param digits number of digits for printed output,
#' @param width the maximum length of the lines in the printed output,
#' @export
print.summary.piest <- function(x, digits = max(3, getOption("digits") - 2),
                                width = getOption("width"), subset = NULL, ...){
    if(is.null(subset)) printCoefmat(coef(x), digits = digits, ...)
    else printCoefmat(coef(x)[subset, , drop = FALSE], digits = digits, ...)
    print(x$pitest, ...)
    invisible(x)
}

# est_plm.R#
starX <- function(formula, data, model, rhs = 1, effect){
    # non-exported, used for IV estimations "am" and "bms"
    # produces a column per time period with the (transformed) data
    # NB: function is not symmetric in individual and time effect
    apdim <- pdim(data)
    amatrix <- model.matrix(data, model, effect, rhs)
    T <- apdim$nT$T # was (same): length(unique(index(data, 2L)))
    N <- apdim$nT$n # was (same): length(unique(index(data, 1L)))
    if (apdim$balanced){
        result <- Reduce("cbind",
                         lapply(seq_len(ncol(amatrix)),
                                function(x)
                                    matrix(amatrix[ , x],
                                           ncol = T, byrow = TRUE)[rep(1:N, each = T), ]))
    }
    else{ # unbalanced
        Ti <- apdim$Tint$Ti
        result <- lapply(seq_len(ncol(amatrix)), function(x)
            structure(amatrix[ , x], index = index(data),
                      class = c("pseries", class(amatrix[ , x]))))
        result <- Reduce("cbind", lapply(result, as.matrix))
        result <- result[rep(1:N, times = Ti), ]
        result[is.na(result)] <- 0
    }
    result
}


# Regards plm man page: some elements not listed here...: "assign", "contrast",
# etc... \item{na.action}{if relevant, information about handling of
# NAs by the  model.frame function,}
# NB: na.action is currently not included as it is not supported


#' Panel Data Estimators
#'
#' Linear models for panel data estimated using the `lm` function on
#' transformed data.
#'
#' `plm` is a general function for the estimation of linear panel
#' models.  It supports the following estimation methods: pooled OLS
#' (`model = "pooling"`), fixed effects (`"within"`), random effects
#' (`"random"`), first--differences (`"fd"`), and between
#' (`"between"`). It supports unbalanced panels and two--way effects
#' (although not with all methods).
#'
#' For random effects models, four estimators of the transformation
#' parameter are available by setting `random.method` to one of
#' `"swar"` \insertCite{SWAM:AROR:72}{plm} (default), `"amemiya"`
#' \insertCite{AMEM:71}{plm}, `"walhus"`
#' \insertCite{WALL:HUSS:69}{plm}, or `"nerlove"`
#' \insertCite{NERLO:71}{plm} (see below for Hausman-Taylor instrumental
#' variable case).
#'
#' For first--difference models, the intercept is maintained (which
#' from a specification viewpoint amounts to allowing for a trend in
#' the levels model). The user can exclude it from the estimated
#' specification the usual way by adding `"-1"` to the model formula.
#'
#' Instrumental variables estimation is obtained using two--part
#' formulas, the second part indicating the instrumental variables
#' used. This can be a complete list of instrumental variables or an
#' update of the first part. If, for example, the model is `y ~ x1 +
#' x2 + x3`, with `x1` and `x2` endogenous and `z1` and `z2` external
#' instruments, the model can be estimated with:
#'
#' \itemize{
#' \item `formula = y~x1+x2+x3 | x3+z1+z2`,
#' \item `formula = y~x1+x2+x3 | . -x1-x2+z1+z2`.
#' }
#'
#' If an instrument variable estimation is requested, argument
#' `inst.method` selects the instrument variable transformation
#' method:
#'
#' - `"bvk"` (default) for \insertCite{BALE:VARA:87;textual}{plm},
#' - `"baltagi"` for \insertCite{BALT:81;textual}{plm},
#' - `"am"` for \insertCite{AMEM:MACU:86;textual}{plm},
#' - `"bms"` for \insertCite{BREU:MIZO:SCHM:89;textual}{plm}.
#'
#' The Hausman--Taylor estimator \insertCite{HAUS:TAYL:81}{plm} is
#' computed with arguments `random.method = "ht"`, `model = "random"`,
#' `inst.method = "baltagi"` (the other way with only `model = "ht"`
#' is deprecated).
#'
#' See also the vignettes for introductions to model estimations (and more) with
#' examples.
#'
#' @aliases plm
#' @param formula a symbolic description for the model to be
#'     estimated,
#' @param x,object an object of class `"plm"`,
#' @param data a `data.frame`,
#' @param subset see [stats::lm()],
#' @param weights see [stats::lm()],
#' @param na.action see [stats::lm()]; currently, not fully
#'     supported,
#' @param effect the effects introduced in the model, one of
#'     `"individual"`, `"time"`, `"twoways"`, or
#'     `"nested"`,
#' @param model one of `"pooling"`, `"within"`,
#'     `"between"`, `"random"` `"fd"`, or `"ht"`,
#' @param random.method method of estimation for the variance
#'     components in the random effects model, one of `"swar"`
#'     (default), `"amemiya"`, `"walhus"`, `"nerlove"`; for
#'     Hausman-Taylor estimation set to `"ht"` (see Details and Examples),
#' @param random.models an alternative to the previous argument, the
#'     models used to compute the variance components estimations are
#'     indicated,
#' @param random.dfcor a numeric vector of length 2 indicating which
#'     degree of freedom should be used,
#' @param inst.method the instrumental variable transformation: one of
#'     `"bvk"`, `"baltagi"`, `"am"`, or `"bms"` (see also Details),
#' @param index the indexes,
#' @param restrict.matrix a matrix which defines linear restrictions
#'     on the coefficients,
#' @param restrict.rhs the right hand side vector of the linear
#'     restrictions on the coefficients,
#' @param digits number of digits for printed output,
#' @param width the maximum length of the lines in the printed output,
#' @param dx the half--length of the individual lines for the plot
#'     method (relative to x range),
#' @param N the number of individual to plot,
#' @param seed the seed which will lead to individual selection,
#' @param within if `TRUE`, the within model is plotted,
#' @param pooling if `TRUE`, the pooling model is plotted,
#' @param between if `TRUE`, the between model is plotted,
#' @param random if `TRUE`, the random effect model is plotted,
#' @param formula. a new formula for the update method,
#' @param evaluate a boolean for the update method, if `TRUE` the
#'     updated model is returned, if `FALSE` the call is returned,
#' @param newdata the new data set for the `predict` method,
#' @param \dots further arguments.
#'
#' @return An object of class `"plm"`.
#'
#'
#' A `"plm"` object has the following elements :
#'
#' \item{coefficients}{the vector of coefficients,}
#' \item{vcov}{the variance--covariance matrix of the coefficients,}
#' \item{residuals}{the vector of residuals (these are the residuals
#' of the (quasi-)demeaned model),}
#' \item{weights}{(only for weighted estimations) weights as
#' specified,}
#' \item{df.residual}{degrees of freedom of the residuals,}
#' \item{formula}{an object of class `"Formula"` describing the model,}
#' \item{model}{the model frame as a `"pdata.frame"` containing the
#' variables used for estimation: the response is in first column followed by
#' the other variables, the individual and time indexes are in the 'index'
#' attribute of `model`,}
#' \item{ercomp}{an object of class `"ercomp"` providing the
#' estimation of the components of the errors (for random effects
#' models only),}
#' \item{aliased}{named logical vector indicating any aliased
#' coefficients which are silently dropped by `plm` due to
#' linearly dependent terms (see also [detect.lindep()]),}
#' \item{call}{the call.}
#'
#'
#' It has `print`, `summary` and `print.summary` methods. The
#' `summary` method creates an object of class `"summary.plm"` that
#' extends the object it is run on with information about (inter alia) F
#' statistic and (adjusted) R-squared of model, standard errors, t--values, and
#' p--values of coefficients, (if supplied) the furnished vcov, see
#' [summary.plm()] for further details.
#' @import Formula
#' @importFrom stats alias approx as.formula coef coefficients cor delete.response
#' @importFrom stats deviance df.residual dnorm fitted formula lm lm.fit model.frame
#' @importFrom stats model.matrix model.response model.weights na.omit pchisq pf
#' @importFrom stats pnorm printCoefmat pt qnorm reshape resid residuals sd terms
#' @importFrom stats update var vcov
#' @importFrom grDevices heat.colors rainbow
#' @importFrom graphics abline axis barplot legend lines plot points
#' @export
#' @author Yves Croissant
#' @seealso [summary.plm()] for further details about the associated
#' summary method and the "summary.plm" object both of which provide some model
#' tests and tests of coefficients.  [fixef()] to compute the fixed
#' effects for "within" models (=fixed effects models).
#' @references
#'
#' \insertRef{AMEM:71}{plm}
#'
#' \insertRef{AMEM:MACU:86}{plm}
#'
#' \insertRef{BALE:VARA:87}{plm}
#'
#' \insertRef{BALT:81}{plm}
#'
#' \insertRef{BALT:SONG:JUNG:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BREU:MIZO:SCHM:89}{plm}
#'
#' \insertRef{HAUS:TAYL:81}{plm}
#'
#' \insertRef{NERLO:71}{plm}
#'
#' \insertRef{SWAM:AROR:72}{plm}
#'
#' \insertRef{WALL:HUSS:69}{plm}
#'
#' @keywords regression
#' @examples
#'
#' data("Produc", package = "plm")
#' zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'           data = Produc, index = c("state","year"))
#' summary(zz)
#'
#' # replicates some results from Baltagi (2013), table 3.1
#' data("Grunfeld", package = "plm")
#' p <- plm(inv ~ value + capital,
#'          data = Grunfeld, model = "pooling")
#'
#' wi <- plm(inv ~ value + capital,
#'           data = Grunfeld, model = "within", effect = "twoways")
#'
#' swar <- plm(inv ~ value + capital,
#'             data = Grunfeld, model = "random", effect = "twoways")
#'
#' amemiya <- plm(inv ~ value + capital,
#'                data = Grunfeld, model = "random", random.method = "amemiya",
#'                effect = "twoways")
#'
#' walhus <- plm(inv ~ value + capital,
#'               data = Grunfeld, model = "random", random.method = "walhus",
#'               effect = "twoways")
#'
#' # summary and summary with a furnished vcov (passed as matrix,
#' # as function, and as function with additional argument)
#' summary(wi)
#' summary(wi, vcov = vcovHC(wi))
#' summary(wi, vcov = vcovHC)
#' summary(wi, vcov = function(x) vcovHC(x, method = "white2"))
#'
#'
#' ## nested random effect model
#' # replicate Baltagi/Song/Jung (2001), p. 378 (table 6), columns SA, WH
#' # == Baltagi (2013), pp. 204-205
#' data("Produc", package = "plm")
#' pProduc <- pdata.frame(Produc, index = c("state", "year", "region"))
#' form <- log(gsp) ~ log(pc) + log(emp) + log(hwy) + log(water) + log(util) + unemp
#' summary(plm(form, data = pProduc, model = "random", effect = "nested"))
#' summary(plm(form, data = pProduc, model = "random", effect = "nested",
#'             random.method = "walhus"))
#'
#' ## Instrumental variable estimations
#' # replicate Baltagi (2013/2021), p. 133/162, table 7.1
#' data("Crime", package = "plm")
#' FE2SLS <- plm(lcrmrte ~ lprbarr + lpolpc + lprbconv + lprbpris + lavgsen +
#'                 ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed +
#'                 lwsta + lwloc + lpctymle + lpctmin + region + smsa + factor(year)
#'               | . - lprbarr - lpolpc + ltaxpc + lmix,
#'               data = Crime, model = "within")
#' G2SLS <- update(FE2SLS, model = "random", inst.method = "bvk")
#' EC2SLS <- update(G2SLS, model = "random", inst.method = "baltagi")
#'
#' ## Hausman-Taylor estimator and Amemiya-MaCurdy estimator
#' # replicate Baltagi (2005, 2013), table 7.4; Baltagi (2021), table 7.5
#' data("Wages", package = "plm")
#' ht <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) +
#'               bluecol + ind + union + sex + black + ed |
#'               bluecol + south + smsa + ind + sex + black |
#'               wks + married + union + exp + I(exp ^ 2),
#'           data = Wages, index = 595,
#'           random.method = "ht", model = "random", inst.method = "baltagi")
#' summary(ht)
#'
#' am <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) +
#'               bluecol + ind + union + sex + black + ed |
#'               bluecol + south + smsa + ind + sex + black |
#'               wks + married + union + exp + I(exp ^ 2),
#'           data = Wages, index = 595,
#'           random.method = "ht", model = "random", inst.method = "am")
#' summary(am)
#'
plm <- function(formula, data, subset, weights, na.action,
                effect = c("individual", "time", "twoways", "nested"),
                model = c("within", "random", "ht", "between", "pooling", "fd"),
                random.method = NULL,
                random.models = NULL,
                random.dfcor = NULL,
                inst.method = c("bvk", "baltagi", "am", "bms"),
                restrict.matrix = NULL,
                restrict.rhs = NULL,
                index = NULL,
                ...){

    if (is.list(formula)){
        # if the first argument is a list (of formulas), then call plmlist and early exit
        plmlist <- match.call(expand.dots = FALSE)
        plmlist[[1L]] <- as.name("plm.list")
        # eval in nframe and not the usual parent.frame(), relevant?
        nframe <- length(sys.calls())
        plmlist <- eval(plmlist, sys.frame(which = nframe))
        return(plmlist)
    }

    if ((! is.null(restrict.matrix) || ! is.null(restrict.rhs)) && ! is.list(formula)) {
        stop(paste0("arguments 'restrict.matrix' and 'restrict.rhs' cannot yet be used ",
                    "for single equations"))
    }
    dots <- list(...)

    # match and check the effect and model arguments
    effect <- match.arg(effect)
    inst.method <- match.arg(inst.method)

    # note that model can be NA, in this case the model.frame is returned
    if (! anyNA(model)) model <- match.arg(model)
    if (! anyNA(model) && effect == "nested" && model != "random") {
        # input check for nested RE model
        stop(paste0("effect = \"nested\" only valid for model = \"random\", but input is model = \"",
                    model, "\"."))
    }

    if (! anyNA(model) && model == "fd") {
        # input checks for FD model: give informative error messages as
        # described in footnote in vignette
        if (effect == "time") stop(paste("effect = \"time\" for first-difference model",
                                         "meaningless because cross-sections do not",
                                         "generally have a natural ordering"))
        if (effect == "twoways") stop(paste("effect = \"twoways\" is not defined",
                                            "for first-difference models"))
    }

    # Deprecated section

    # model = "ht" in plm() and pht() are no longer maintained, but working
    # -> call pht() and early exit
    if (! anyNA(model) && model == "ht"){
        ht <- match.call(expand.dots = FALSE)
        m <- match(c("formula", "data", "subset", "na.action", "index"), names(ht), 0)
        ht <- ht[c(1L, m)]
        ht[[1L]] <- as.name("pht")
        ht <- eval(ht, parent.frame())
        return(ht)
    }

    # check whether data and formula are pdata.frame and Formula and if not
    # coerce them
    orig_rownames <- row.names(data)

    if (! inherits(data, "pdata.frame")) data <- pdata.frame(data, index)
    if (! inherits(formula, "Formula")) formula <- as.Formula(formula)

    # in case of 2-part formula, check whether the second part should
    # be updated, e.g., y ~ x1 + x2 + x3 | . - x2 + z becomes
    # y ~ x1 + x2 + x3 | x1 + x3 + z
    # use length(formula)[2] because the length is now a vector of length 2
    #    if (length(formula)[2] == 2) formula <- expand.formula(formula)

    # eval the model.frame
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("data", "formula", "subset", "weights", "na.action"), names(mf), 0)
    mf <- mf[c(1L, m)]
    names(mf)[2:3] <- c("formula", "data")
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    # use the Formula and pdata.frame which were created if necessary (and not
    # the original formula / data)
    mf$formula <- data
    mf$data <- formula
    data <- eval(mf, parent.frame())

    # preserve original row.names for data [also fancy rownames]; so functions
    # like pmodel.response(), model.frame(), model.matrix(), residuals() return
    # the original row.names eval(mf, parent.frame()) returns row.names as
    # character vector containing the "row_number" with incomplete observations
    # dropped
    row.names(data) <- orig_rownames[as.numeric(row.names(data))]

    # return the model.frame (via early exit) if model = NA, else estimate model
    if (is.na(model)){
        attr(data, "formula") <- formula
        return(data)
    }

    # note that the model.frame has as attributes the Formula and the index
    # data.frame
    args <- list(model = model, effect = effect,
                 random.method = random.method,
                 random.models = random.models,
                 random.dfcor = random.dfcor,
                 inst.method = inst.method)
    result <- plm.fit(data, model, effect, random.method,
                      random.models, random.dfcor, inst.method)
    result$call <- cl
    result$args <- args
    result
}

plm.fit <- function(data, model, effect, random.method,
                    random.models, random.dfcor, inst.method){
    formula <- attr(data, "formula")
    # check for 0 cases like in stats::lm.fit (e.g., due to NA dropping)
    if (nrow(data) == 0L) stop("0 (non-NA) cases")

    # if a random effect model is estimated, compute the error components
    if (model == "random"){
        is.balanced <- is.pbalanced(data)
        estec <- ercomp(data, effect, method = random.method,
                        models = random.models, dfcor = random.dfcor)
        sigma2 <- estec$sigma2
        theta <- estec$theta
        if (length(formula)[2L] > 1L && effect == "twoways")
            stop(paste("Instrumental variable random effect estimation",
                       "not implemented for two-ways panels"))
    }
    else theta <- NULL

    # For all models except the unbalanced twoways random model, the
    # estimator is obtained as a linear regression on transformed data
    if (! (model == "random" && effect == "twoways" && ! is.balanced)){
        # extract the model.matrix and the model.response actually, this can be
        # done by providing model.matrix and pmodel.response's methods
        # to pdata.frames
        X <- model.matrix(data, rhs = 1, model = model,
                          effect = effect, theta = theta, cstcovar.rm = "all")
        y <- pmodel.response(data, model = model,
                             effect = effect, theta = theta)
        if (ncol(X) == 0L) stop("empty model")

        w <- model.weights(data)
        if (! is.null(w)){
            if (! is.numeric(w)) stop("'weights' must be a numeric vector")
            if (any(w < 0 | is.na(w))) stop("missing or negative weights not allowed")
            X <- X * sqrt(w)
            y <- y * sqrt(w)
        }
        else w <- 1

        # IV case: extract the matrix of instruments if necessary
        # (means here that we have a multi-parts formula)
        if (length(formula)[2L] > 1L) {

            if(!is.null(model.weights(data)) || any(w != 1))
                stop("argument 'weights' not yet implemented for instrumental variable models")

            if ( ! (model == "random" && inst.method != "bvk")) {
                #  FD/FE/BE IV and RE "bvk" IV estimator
                if (length(formula)[2L] == 2L) {
                    W <- model.matrix(data, rhs = 2,
                                      model = model, effect = effect,
                                      theta = theta, cstcovar.rm = "all")
                }
                else {
                    W <- model.matrix(data, rhs = c(2, 3),
                                      model = model, effect = effect,
                                      theta = theta, cstcovar.rm = "all")
                }
            }

            if (model == "random" && inst.method != "bvk") {
                # IV estimators RE "baltagi", "am", and "bms"
                X <- X / sqrt(sigma2["idios"])
                y <- y / sqrt(sigma2["idios"])
                W1 <- model.matrix(data, rhs = 2,
                                   model = "within", effect = effect,
                                   theta = theta, cstcovar.rm = "all")
                B1 <- model.matrix(data, rhs = 2,
                                   model = "Between", effect = effect,
                                   theta = theta, cstcovar.rm = "all")

                if (inst.method %in% c("am", "bms"))
                    StarW1 <- starX(formula, data, rhs = 2, model = "within", effect = effect)

                if (length(formula)[2L] == 3L) {
                    # eval. 3rd part of formula, if present
                    W2 <- model.matrix(data, rhs = 3,
                                       model = "within", effect = effect,
                                       theta = theta, cstcovar.rm = "all")

                    if (inst.method == "bms")
                        StarW2 <- starX(formula, data, rhs = 3, model = "within", effect = effect)
                }
                else W2 <- StarW2 <- NULL

                # TODO: here, some weighting is done but prevented earlier by stop()?!
                #       also: RE bvk/BE/FE IV do not have weighting code.
                if (inst.method == "baltagi") W <- sqrt(w) * cbind(W1, W2, B1)
                if (inst.method == "am")      W <- sqrt(w) * cbind(W1, W2, B1, StarW1)
                if (inst.method == "bms")     W <- sqrt(w) * cbind(W1, W2, B1, StarW1, StarW2)
            }

            if (ncol(W) < ncol(X)) stop("insufficient number of instruments")
        } # END all IV cases
        else W <- NULL # no instruments (no IV case)

        result <- mylm(y, X, W)
        df <- df.residual(result)
        vcov <- result$vcov
        aliased <- result$aliased

        # in case of a within estimation, correct the degrees of freedom
        if (model == "within"){
            pdim <- pdim(data)
            card.fixef <- switch(effect,
                                 "individual" = pdim$nT$n,
                                 "time"       = pdim$nT$T,
                                 "twoways"    = pdim$nT$n + pdim$nT$T - 1
            )
            df <- df.residual(result) - card.fixef
            vcov <- result$vcov * df.residual(result) / df
        }

        result <- list(coefficients = coef(result),
                       vcov         = vcov,
                       residuals    = resid(result),
                       weights      = w,
                       df.residual  = df,
                       formula      = formula,
                       model        = data)

        if (is.null(model.weights(data))) result$weights <- NULL
        if (model == "random") result$ercomp <- estec
    }
    else {
        # random twoways unbalanced:
        pdim <- pdim(data)
        TS <- pdim$nT$T
        theta <- estec$theta$id
        phi2mu <- estec$sigma2["time"] / estec$sigma2["idios"]
        Dmu <- model.matrix( ~ unclass(index(data))[[2L]] - 1)
        attr(Dmu, "index") <- index(data)
        Dmu <- Dmu - theta * Between(Dmu, "individual")
        X <- model.matrix(data, rhs = 1, model = "random",
                          effect = "individual", theta = theta)
        y <- pmodel.response(data, model = "random",
                             effect = "individual", theta = theta)
        P <- solve(diag(TS) + phi2mu * crossprod(Dmu))
        phi2mu.CPXDmu.P <- phi2mu * crossprod(X, Dmu) %*% P
        XPX <- crossprod(X)    - phi2mu.CPXDmu.P %*% crossprod(Dmu, X)
        XPy <- crossprod(X, y) - phi2mu.CPXDmu.P %*% crossprod(Dmu, y)
        gamma <- solve(XPX, XPy)[ , , drop = TRUE]

        # residuals 'e' are not the residuals of a quasi-demeaned
        # model but of the 'outer' model
        e <- pmodel.response(data, model = "pooling", effect = effect) -
            as.numeric(model.matrix(data, rhs = 1, model = "pooling") %*% gamma)

        result <- list(coefficients = gamma,
                       vcov         = solve(XPX),
                       formula      = formula,
                       model        = data,
                       ercomp       = estec,
                       df.residual  = nrow(X) - ncol(X),
                       residuals    = e)

        # derive 'aliased' information (this is based on the assumption that
        # estimation fails anyway if singularities are present).
        aliased <- is.na(gamma)
    }
    result$assign <- attr(X, "assign")
    result$contrasts <- attr(X, "contrasts")
    result$args <- list(model = model, effect = effect)
    result$aliased <- aliased
    class(result) <- c("plm", "panelmodel")
    result
}

tss <- function(x, ...){
    UseMethod("tss")
}

tss.default <- function(x){
    # always gives centered TSS (= demeaned TSS)
    var(x) * (length(x) - 1)
}

tss.plm <- function(x, model = NULL){
    if(is.null(model)) model <- describe(x, "model")
    effect <- describe(x, "effect")
    if(model == "ht") model <- "pooling"
    theta <- if(model == "random") x$ercomp$theta else NULL
    tss(pmodel.response(x, model = model, effect = effect, theta = theta))
}

#' R squared and adjusted R squared for panel models
#'
#' This function computes R squared or adjusted R squared for plm objects. It
#' allows to define on which transformation of the data the (adjusted) R
#' squared is to be computed and which method for calculation is used.
#'
#'
#' @param object an object of class `"plm"`,
#' @param model on which transformation of the data the R-squared is to be
#' computed. If `NULL`, the transformation used to estimate the model is
#' also used for the computation of R squared,
#' @param type indicates method which is used to compute R squared. One of\cr
#' `"rss"` (residual sum of squares),\cr `"ess"` (explained sum of
#' squares), or\cr `"cor"` (coefficient of correlation between the fitted
#' values and the response),
#' @param dfcor if `TRUE`, the adjusted R squared is computed.
#' @return A numerical value. The R squared or adjusted R squared of the model
#' estimated on the transformed data, e. g., for the within model the so called
#' "within R squared".
#' @seealso [plm()] for estimation of various models;
#' [summary.plm()] which makes use of `r.squared`.
#' @keywords htest
#' @export
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' p <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
#' r.squared(p)
#' r.squared(p, dfcor = TRUE)
#'
r.squared <- function(object, model = NULL,
                      type = c("cor", "rss", "ess"), dfcor = FALSE){
    ## TODO: does not handle non-intercept models correctly
    ##       see below r.squared_no_intercept
    if (is.null(model)) model <- describe(object, "model")
    effect <- describe(object, "effect")
    type <- match.arg(type)
    if (type == "cor"){
        y <- pmodel.response(object, model = model, effect = effect)
        haty <- fitted(object, model = model, effect = effect)
        R2 <- cor(y, haty)^2
    }
    if (type == "rss"){
        R2 <- 1 - deviance(object, model = model) / tss(object, model = model)
    }
    if (type == "ess"){
        haty <- fitted(object, model = model)
        mhaty <- mean(haty)
        ess <- as.numeric(crossprod((haty - mhaty)))
        R2 <- ess / tss(object, model = model)
    }
    ### adj. R2 Still wrong for models without intercept, e.g., pooling models
    # (but could be correct for within models, see comment below in function r.squared_no_intercept)
    if (dfcor) R2 <- 1 - (1 - R2) * (length(resid(object)) - 1) / df.residual(object)
    R2
}

## first try at r.squared adapted to be suitable for non-intercept models
r.squared_no_intercept <- function(object, model = NULL,
                                   type = c("rss", "ess", "cor"), dfcor = FALSE){
    if(is.null(model)) model <- describe(object, "model")
    effect <- describe(object, "effect")
    type <- match.arg(type)
    ## TODO: check what is sane for IV and what for within
    # [1L] as has.intercept returns > 1 boolean for IV models # TODO: to check if this is sane
    has.int <- if(model != "within") has.intercept(object)[1L] else FALSE

    if (type == "rss"){
        # approach: 1 - RSS / TSS
        R2 <- if(has.int) {
            1 - deviance(object, model = model) / tss(object, model = model)
        } else {
            # use non-centered (= non-demeaned) TSS
            1 - deviance(object, model = model) / as.numeric(crossprod(pmodel.response(object, model = model)))
        }
    }

    if(type == "ess"){
        # approach: ESS / TSS
        haty <- fitted(object, model = model)
        R2 <- if(has.int) {
            mhaty <- mean(haty)
            ess <- as.numeric(crossprod(haty - mhaty))
            tss <- tss(object, model = model)
            ess / tss
        }
        else {
            # use non-centered (=non-demeaned) ESS and non-centered TSS
            ess <- as.numeric(crossprod(haty))
            tss <- as.numeric(crossprod(pmodel.response(object, model = model)))
            ess / tss
        }
    }

    if(type == "cor"){
        # approach: squared-correlation(dependent variable, predicted value), only for models with intercept
        if(!has.int) warning("for models without intercept, type = \"cor\" may not be sane") # TODO: tbd if warning is good

        # TODO: Check should this be for "cor" the original variable? This makes a difference for (at least) RE models!
        #       and on the fitted values which are not given by fitted() for RE models
        #      y <- pmodel.response(object, model = model, effect = effect)
        #      haty <- fitted(object, model = model, effect = effect)
        y <- pmodel.response(object, model = "pooling")
        haty <- fitted_exp.plm(object)
        R2 <- cor(y, haty)^2
    }

    # this takes care of the intercept
    # Still unclear, how the adjustment for within models should look like,
    # i.e., subtract 1 for intercept or not
    if(dfcor) R2 <- 1 - (1 - R2) * (length(resid(object)) - has.int) / df.residual(object)

    return(R2)
}



# describe function: extract characteristics of plm model
describe <- function(x,
                     what = c("model", "effect", "random.method",
                              "inst.method", "transformation", "ht.method")){
    what <- match.arg(what)
    cl <- x$args
    switch(what,
           "model"          = if(!is.null(cl$model))
               cl$model else  "within",
           "effect"         = if(!is.null(cl$effect))
               cl$effect else "individual",
           "random.method"  = if(!is.null(cl$random.method))
               cl$random.method else "swar",
           "inst.method"    = if(!is.null(cl$inst.method))
               cl$inst.method else "bvk",
           "transformation" = if(!is.null(cl$transformation))
               cl$transformation else "d",
           "ht.method"      = if(!is.null(cl$ht.method))
               cl$ht.method else "ht"
    )
}

# plm.list <- function(formula, data, subset, na.action,
effect = c("individual", "time", "twoways"),
model = c("within", "random", "ht", "between", "pooling", "fd"),
random.method = NULL, #c("swar", "walhus", "amemiya", "nerlove", "ht"),
inst.method = c("bvk", "baltagi"),
restrict.matrix = NULL,
restrict.rhs = NULL,
index = NULL,
...){
    sysplm <- match.call(expand.dots = FALSE)
    if (!inherits(data, "pdata.frame")){
        odataname <- substitute(data)
        data <- pdata.frame(data, index)
        sysplm$data <- data
    }

    names.eq <- names(formula)
    # run plm for each equation of the list, store the results in a
    # list
    plm.models <- function(sysplm, amodel, ...){
        formulas <- sysplm$formula
        L <- length(formulas) - 1
        models <- vector(mode = "list", length = L)
        for (l in 2:(L+1)){
            aformula <- formulas[[l]]
            if (is.name(aformula)) aformula <- eval(aformula, parent.frame())
            else aformula <- as.formula(formulas[[l]])
            sysplm$formula <- aformula
            sysplm[[1L]] <- as.name("plm")
            sysplm$model <- amodel
            # a new pb, plm on every equation fails because of the restrict.matrix argument
            sysplm$restrict.matrix <- NULL
            models[[l-1]] <- eval(sysplm, parent.frame())
        }
        models
    }

    # Extract the model matrix and the response and transform them in
    # order to get iid errors using a furnished matrix of covariance of
    # the raw errors
    BIG <- function(X, y, W, Omega){
        S <- chol(Omega)
        N <- length(y[[1L]])
        if (!is.null(W)) BIGW <- c()
        BIGX <- c()
        BIGy <- c()
        L <- nrow(S)
        for (l in 1:L){
            rowBIGy <- rep(0, N)
            rowBIGX <- c()
            if (!is.null(W)) rowBIGW <- c()
            for (m in 1:L){
                rowBIGX <- cbind(rowBIGX, t(solve(S))[l, m] * X[[m]])
                if (!is.null(W)) rowBIGW <- cbind(rowBIGW, t(S)[l, m] * W[[m]])
                rowBIGy <- rowBIGy + t(solve(S))[l, m] * y[[m]]
            }
            BIGX <- rbind(BIGX, rowBIGX)
            if (!is.null(W)) BIGW <- rbind(BIGW, rowBIGW)
            BIGy <- c(BIGy, rowBIGy)
        }
        if (!is.null(W)) return(structure(list(X = BIGX, y = BIGy, W = BIGW), class = "BIG"))
        else return(structure(list(X = BIGX, y = BIGy), class = "BIG"))
    }

    # take a list of unconstrained models and a restriction matrix and
    # return a list containing the coefficients, the vcov and the
    # residuals of the constrained model ; qad version which deals with
    # lists of plm models or with models fitted by mylm (which have X, y
    # and W slots)
    systemlm <- function(object, restrict.matrix, restrict.rhs){
        if (inherits(object, "list")){
            Ucoef <- Reduce("c", lapply(object, coef))
            Uvcov <- Reduce("bdiag", lapply(object, vcov))
            X <- Reduce("bdiag", lapply(object, model.matrix))
            y <- Reduce("c", lapply(object, pmodel.response))
        }
        else{
            Ucoef <- coef(object)
            Uvcov <- vcov(object)
            X <- object$X
            y <- object$y
        }
        if (!is.null(restrict.matrix)){
            R <- restrict.matrix
            if (is.null(restrict.rhs)) restrict.rhs <- rep(0, nrow(restrict.matrix))
            XpXm1 <- solve(crossprod(X))
            Q <- XpXm1 %*% t(R) %*% solve(R %*% XpXm1 %*% t(R))
            Ccoef <- as.numeric(Ucoef - Q %*% (R %*% Ucoef - restrict.rhs))
            names(Ccoef) <- names(Ucoef)
            Cvcov <- Uvcov - Q %*% R %*% Uvcov
            Cresid <- y - X %*% Ccoef
            structure(list(coefficients = Ccoef, vcov = Cvcov, residuals = Cresid), class = "basiclm")
        }
        else{
            .resid <- Reduce("c", lapply(object, resid))
            structure(list(coefficents = Ucoef, vcov = Uvcov, residuals = .resid), class = "basiclm")
        }
    }
    models <- plm.models(sysplm, amodel = model, random.method = "kinla") #TODO NB: "kinla" does not seem to be supported anymore...
    L <- length(models)
    sys <- systemlm(models, restrict.matrix = restrict.matrix, restrict.rhs = restrict.rhs)
    Instruments <- sapply(models, function(x) length(formula(x))[2L]) > 1L

    # Get the residuals and compute the consistent estimation of the
    # covariance matrix of the residuals : Note that if there are
    # restrictions, the "restricted" residuals are used ; for random
    # effect models, two covariance matrices must be computed
    if (model == "random"){
        resid.pooling <- Reduce("cbind", lapply(models, function(x) resid(x, model = "pooling")))
        id <- index(models[[1L]])[[1L]]
        pdim <- pdim(models[[1L]])
        T <- pdim$nT$T
        N <- pdim$nT$n
        .fixef <- apply(resid.pooling, 2, tapply, id, mean)
        resid.within <- resid.pooling - .fixef[as.character(id),]
        Omega.nu <- crossprod(resid.within)/(N * (T - 1))
        Omega.eta <- crossprod(.fixef) / (N - 1)
        colnames(Omega.nu) <- rownames(Omega.nu) <- colnames(Omega.eta) <- rownames(Omega.eta) <- names.eq
        Omega.1 <- Omega.nu + T * Omega.eta
        Omega <- list(id = Omega.eta, idios = Omega.nu)
        phi <- 1 - sqrt(diag(Omega.nu)/diag(Omega.1))
        XW <- lapply(models, function(x) model.matrix(x, model = "within"))
        intercepts <- lapply(models, has.intercept)
        XB <- lapply(models, function(x) model.matrix(x, model = "Between"))
        yW <- lapply(models, function(x) pmodel.response(x, model = "within"))
        yB <- lapply(models, function(x) pmodel.response(x, model = "Between"))
        if (Instruments[1L]){
            WW <- lapply(models,
                         function(x){
                             if (length(formula(x))[2L] == 3L) rhss = c(2, 3) else rhss = 2
                             model.matrix(model.frame(x), rhs = rhss, model = "within")
                         }
            )
            WB <- lapply(models, function(x) model.matrix(model.frame(x), rhs = 2, model = "Between"))
        }
        else WW <- WB <- NULL
        coefnames <- lapply(XB, colnames)
        BIGW <- BIG(XW, yW, WW, Omega.nu)
        BIGB <- BIG(XB, yB, WB, Omega.1)
        y <- BIGW$y + BIGB$y
        X <- BIGB$X
        # Attention, pb lorsque noms de colonnes duppliques !!
        #    X[, colnames(BIGW$X)] <- X[, colnames(BIGW$X)] + BIGW$X
        # version provisoire : emplacement des constantes
        intercepts <- c(1, cumsum(sapply(XB, ncol))[-length(XB)]+1)
        X[, - intercepts] <- X[, - intercepts] + BIGW$X
        m <- mylm(y, X, cbind(BIGW$W, BIGB$W))
    }
    else{
        .resid <- matrix(sys$residuals, ncol = length(models))
        Omega <- crossprod(.resid) / nrow(.resid)
        colnames(Omega) <- rownames(Omega) <- names.eq
        X <- lapply(models, model.matrix)
        y <- lapply(models, pmodel.response)
        if (Instruments[1L])
            W <- lapply(models,
                        function(x){
                            if (length(formula(x))[2L] == 3L) rhss = c(2, 3) else rhss = 2
                            model.matrix(model.frame(x), rhs = rhss)
                        }
            )
        else W <- NULL
        coefnames <- lapply(X, colnames)
        BIGT <- BIG(X, y, W, Omega)
        X <- BIGT$X
        m <- with(BIGT, mylm(y, X, W))
    }
    if (!is.null(restrict.matrix)){
        m <- systemlm(m, restrict.matrix = restrict.matrix, restrict.rhs = restrict.rhs)
    }
    m$model <- data
    m$coefnames <- coefnames
    m$df.residual <- length(resid(m)) - length(coef(m))
    m$vcovsys <- Omega
    m$formula <- formula
    sysplm$data <- odataname
    m$call <- sysplm
    args <- list(model = model, effect = effect, random.method = random.method)
    m$args <- args
    class(m) <- c("plm.list", "plm", "panelmodel", "lm")
    return(m)
}

#' @rdname summary.plm
#' @export
summary.plm.list <- function(object, ...){
    class(object) <- setdiff(class(object), "plm.list")
    formulas <- eval(object$call$formula)
    eqnames <- names(formulas)
    L <- length(object$coefnames)
    Ks <- c(0, cumsum(sapply(object$coefnames, length)))
    models <- vector(mode = "list", length = L)
    if (is.null(object$vcov)){
        coefTable <- coef(summary(object))
    }
    else{
        std.err <- sqrt(diag(object$vcov))
        b <- coefficients(object)
        z <- b / std.err
        p <- 2 * pt(abs(z), df = object$df.residual, lower.tail = FALSE)
        coefTable <- cbind("Estimate"   = b,
                           "Std. Error" = std.err,
                           "t-value"    = z,
                           "Pr(>|t|)"   = p)
    }
    for (l in 1:L){
        models[[l]] <- coefTable[(Ks[l] + 1):Ks[l + 1] , ]
    }
    names(models) <- eqnames
    object$models <- models
    object$coefficients <- coefTable
    class(object) <- c("summary.plm.list", class(object))
    object
}


#' @rdname summary.plm
#' @export
coef.summary.plm.list <- function(object, eq = NULL, ...){
    if (is.null(eq)) object$coefficients
    else object$models[[eq]]
}

#' @rdname summary.plm
#' @export
print.summary.plm.list <- function(x, digits = max(3, getOption("digits") - 2),
                                   width = getOption("width"), ...){
    effect <- describe(x, "effect")
    model <- describe(x, "model")
    cat(paste(effect.plm.list[effect]," ",sep=""))
    cat(paste(model.plm.list[model]," Model",sep=""))
    if (model=="random"){
        ercomp <- describe(x, "random.method")
        cat(paste(" \n   (",
                  random.method.list[ercomp],
                  "'s transformation)\n",
                  sep=""))
    }
    else{
        cat("\n")
    }
    cat("Call:\n")
    print(x$call)
    cat("\n")
    print(pdim(x))
    cat("\nEffects:\n\n")
    cat("  Estimated standard deviations of the error\n")
    if (model == "random"){
        sd <- rbind(id = sqrt(diag(x$vcovsys$id)),
                    idios = sqrt(diag(x$vcovsys$idios)))
        print(sd, digits = digits)
        cat("\n")
        cat("  Estimated correlation matrix of the individual effects\n")
        corid <- x$vcovsys$id / tcrossprod(sd[1L, ])
        corid[upper.tri(corid)] <- NA
        print(corid, digits = digits, na.print = ".")
        cat("\n")
        cat("  Estimated correlation matrix of the idiosyncratic effects\n")
        coridios <- x$vcovsys$idios / tcrossprod(sd[2L, ])
        coridios[upper.tri(coridios)] <- NA
        print(coridios, digits = digits, na.print = ".")
    }
    else{
        sd <- sqrt(diag(x$vcovsys))
        print(sd, digits = digits)
        cat("\n")
        cat("\nEstimated correlation matrix of the errors\n")
        corer <- x$vcovsys / tcrossprod(sd)
        corer[upper.tri(corer)] <- NA
        print(corer, digits = digits, na.print = ".")
        cat("\n")
    }
    for (l in 1:length(x$models)){
        cat(paste("\n - ", names(x$models)[l], "\n", sep = ""))
        printCoefmat(x$models[[l]], digits = digits)
    }
    invisible(x)
}

#' @rdname plm
#' @export
print.plm.list <- function(x, digits = max(3, getOption("digits") - 2), width = getOption("width"),...){
    cat("\nModel Formulas:\n")
    for (l in 1:length(formula(x))){
        cat(paste(names(formula(x))[l], "  : ", deparse(formula(x)[[l]]), "\n", sep = ""))
    }
    cat("\nCoefficients:\n")
    print(coef(x),digits = digits)
    cat("\n")
    invisible(x)
}
# est_vcm.R#
#' Variable Coefficients Models for Panel Data
#'
#' Estimators for random and fixed effects models with variable coefficients.
#'
#' `pvcm` estimates variable coefficients models. Individual or time
#' effects are introduced, respectively, if `effect = "individual"`
#' (default) or `effect = "time"`.
#'
#' Coefficients are assumed to be fixed if `model = "within"`, i.e., separate
#' pooled OLS models are estimated per individual (`effect = "individual"`)
#' or per time period (`effect = "time"`). Coefficients are assumed to be
#' random if `model = "random"` and the model by
#' \insertCite{SWAM:70;textual}{plm} is estimated. It is a generalized least
#' squares model which uses the results of the previous model.
#'
#' @aliases pvcm
#' @param formula a symbolic description for the model to be estimated,
#' @param object,x an object of class `"pvcm"`,
#' @param data a `data.frame`,
#' @param subset see `lm`,
#' @param na.action see `lm`,
#' @param effect the effects introduced in the model: one of
#' `"individual"`, `"time"`,
#' @param model one of `"within"`, `"random"`,
#' @param index the indexes, see [pdata.frame()],
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param \dots further arguments.
#' @return An object of class `c("pvcm", "panelmodel")`, which has the
#' following elements:
#'
#' \item{coefficients}{the vector (or the data frame for fixed
#' effects) of coefficients,}
#'
#' \item{residuals}{the vector of
#' residuals,}
#'
#' \item{fitted.values}{the vector of fitted values,}
#'
#' \item{vcov}{the covariance matrix of the coefficients (a list for
#' fixed effects model (`model = "within"`)),}
#'
#' \item{df.residual}{degrees of freedom of the residuals,}
#'
#' \item{model}{a data frame containing the variables used for the
#' estimation,}
#'
#' \item{call}{the call,} \item{Delta}{the estimation of the
#' covariance matrix of the coefficients (random effect models only),}
#'
#' \item{std.error}{a data frame containing standard errors for all
#' coefficients for each individual (within models only).}
#'
#' `pvcm` objects have `print`, `summary` and `print.summary` methods.
#'
#' @export
#' @author Yves Croissant
#' @references
#'
#' \insertRef{SWAM:70}{plm}
#'
#' @keywords regression
#' @examples
#'
#' data("Produc", package = "plm")
#' zw <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "within")
#' zr <- pvcm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "random")
#'
#' ## replicate Greene (2012), p. 419, table 11.14
#' summary(pvcm(log(gsp) ~ log(pc) + log(hwy) + log(water) + log(util) + log(emp) + unemp,
#'              data = Produc, model = "random"))
#'
#' \dontrun{
#' # replicate Swamy (1970), p. 166, table 5.2
#' data(Grunfeld, package = "AER") # 11 firm Grunfeld data needed from package AER
#' gw <- pvcm(invest ~ value + capital, data = Grunfeld, index = c("firm", "year"))
#' }
#'
#'
pvcm <- function(formula, data, subset ,na.action, effect = c("individual", "time"),
                 model = c("within", "random"), index = NULL, ...){

    effect <- match.arg(effect)
    model.name <- match.arg(model)
    data.name <- paste(deparse(substitute(data)))

    cl <- match.call(expand.dots = TRUE)
    mf <- match.call()
    mf[[1L]] <- as.name("plm")
    mf$model <- NA
    data <- eval(mf, parent.frame())
    result <- switch(model.name,
                     "within" = pvcm.within(formula, data, effect),
                     "random" = pvcm.random(formula, data, effect)
    )
    class(result) <- c("pvcm", "panelmodel")
    result$call <- cl
    result$args <- list(model = model, effect = effect)
    result
}

pvcm.within <- function(formula, data, effect){
    index <- attr(data, "index")
    id <- index[[1L]]
    time <- index[[2L]]
    pdim <- pdim(data)

    if (effect == "time"){
        cond <- time
        other <- id
        card.cond <- pdim$nT$T
    }
    else{
        cond <- id
        other <- time
        card.cond <- pdim$nT$n
    }
    ml <- split(data, cond)
    nr <- vapply(ml, function(x) dim(x)[1L] > 0, FUN.VALUE = TRUE) # == sapply(ml, function(x) dim(x)[1L]) > 0
    ml <- ml[nr]
    attr(ml, "index") <- index
    ols <- lapply(ml,
                  function(x){
                      X <- model.matrix(x)
                      if (nrow(X) <= ncol(X)) stop("insufficient number of observations")
                      y <- pmodel.response(x)
                      r <- lm(y ~ X - 1, model = FALSE)
                      nc <- colnames(model.frame(r)$X)
                      names(r$coefficients) <- nc
                      r
                  })
    # extract coefficients:
    coef <- matrix(unlist(lapply(ols, coef)), nrow = length(ols), byrow = TRUE) # was: as.data.frame(t(sapply(ols, coef)))...
    dimnames(coef)[1:2] <- list(names(ols), names(coef(ols[[1L]])))             # ... but that code errored with intercept-only model
    coef <- as.data.frame(coef)

    # extract residuals and make pseries:
    residuals <- unlist(lapply(ols, residuals))
    residuals <- add_pseries_features(residuals, index)

    # extract standard errors:
    vcov <- lapply(ols, vcov)
    std <- matrix(unlist(lapply(vcov, function(x) sqrt(diag(x)))), nrow = length(ols), byrow = TRUE) # was: as.data.frame(t(sapply(vcov, function(x) sqrt(diag(x)))))
    dimnames(std)[1:2] <- list(names(vcov), colnames(vcov[[1L]]))                                    # ... but this code errored with intercept-only model
    std <- as.data.frame(std)

    ssr <- as.numeric(crossprod(residuals))
    y <- unlist(lapply(ml, function(x) x[ , 1L]))
    fitted.values <- y - residuals
    tss <- tss(y)
    df.resid <- pdim$nT$N - card.cond * ncol(coef)
    nopool <- list(coefficients  = coef,
                   residuals     = residuals,
                   fitted.values = fitted.values,
                   vcov          = vcov,
                   df.residual   = df.resid,
                   model         = data,
                   std.error     = std)
    nopool
}


pvcm.random <- function(formula, data, effect){

    interc <- has.intercept(formula)
    index <- index(data)
    id <- index[[1L]]
    time <- index[[2L]]
    pdim <- pdim(data)
    N <- nrow(data)
    if (effect == "time"){
        cond <- time
        other <- id
        card.cond <- pdim$nT$T
    }
    else{
        cond <- id
        other <- time
        card.cond <- pdim$nT$n
    }

    ml <- split(data, cond)
    nr <- vapply(ml, function(x) dim(x)[1L] > 0, FUN.VALUE = TRUE) # == sapply(ml, function(x) dim(x)[1L]) > 0
    ml <- ml[nr]
    attr(ml, "index") <- index
    ols <- lapply(ml,
                  function(x){
                      X <- model.matrix(formula, x)
                      if (nrow(X) <= ncol(X)) stop("insufficient number of observations")
                      y <- pmodel.response(x)
                      r <- lm(y ~ X - 1, model = FALSE)
                      nc <- colnames(model.frame(r)$X)
                      names(r$coefficients) <- nc
                      r
                  })

    # matrix of coefficients
    coefm <- matrix(unlist(lapply(ols, coef)), nrow = length(ols), byrow = TRUE)
    dimnames(coefm)[1:2] <- list(names(ols), names(coef(ols[[1]])))

    # number of covariates
    K <- ncol(coefm) - has.intercept(formula)
    # check for NA coefficients
    coefna <- is.na(coefm)
    # list of model matrices
    X <- lapply(ols, model.matrix)
    # same without the covariates with NA coefficients
    Xna <- lapply(seq_len(nrow(coefm)), function(i) X[[i]][ , !coefna[i, ]]) # TODO: Xna is used nowhere!?
    # list of model responses
    y <- lapply(ols, function(x) model.response(model.frame(x)))
    # compute a list of XpX^-1 matrices, with 0 for lines/columns with
    # NA coefficients
    xpxm1 <- lapply(seq_len(card.cond), function(i){
        z <- matrix(0, ncol(coefm), ncol(coefm),
                    dimnames = list(colnames(coefm), colnames(coefm)))
        z[!coefna[i, ], !coefna[i, ]] <- solve(crossprod(X[[i]][!coefna[i, ], !coefna[i, ]]))
        z
    })

    # compute the mean of the parameters
    coefb <- colMeans(coefm, na.rm = TRUE)
    # insert the mean values in place of NA coefficients (if any)
    if(any(coefna)) coefm <- apply(coefm, 2, function(x){x[is.na(x)] <- mean(x, na.rm = TRUE); x})
    # D1: compute the first part of the variance matrix
    coef.mb <- t(coefm) - coefb
    D1 <- tcrossprod(coef.mb, coef.mb / (card.cond - 1)) # TODO: this fails if only 1 individual, catch this corner case w/ informative error msg?
    # D2: compute the second part of the variance matrix
    sigi <- vapply(ols, function(x) deviance(x) / df.residual(x), FUN.VALUE = 0.0)
    D2 <- Reduce("+", lapply(seq_len(card.cond),
                             function(i) sigi[i] * xpxm1[[i]])) / card.cond
    # if D1-D2 semi-definite positive, use it, otherwise use D1
    eig <- prod(eigen(D1 - D2)$values >= 0)
    Delta <- if(eig) { D1 - D2 } else  D1

    # compute the Omega matrix for each individual
    Omegan <- lapply(seq_len(card.cond), function(i) sigi[i] * diag(nrow(X[[i]])) + X[[i]] %*% Delta %*% t(X[[i]]))
    # compute X'Omega X and X'Omega y for each individual
    XyOmXy <- lapply(seq_len(card.cond), function(i){
        Xn <- X[[i]][ , !coefna[i, ]] ## TODO: check if drop = FALSE needed (also in other extractions)
        yn <- y[[i]]
        # pre-allocate matrices
        XnXn <- matrix(0, ncol(coefm), ncol(coefm), dimnames = list(colnames(coefm), colnames(coefm)))
        Xnyn <- matrix(0, ncol(coefm), 1L,          dimnames = list(colnames(coefm), "y"))
        solve_Omegan_i <- solve(Omegan[[i]])
        CP.tXn.solve_Omegan_i <- crossprod(Xn, solve_Omegan_i)
        XnXn[!coefna[i, ], !coefna[i, ]] <- CP.tXn.solve_Omegan_i %*% Xn # == t(Xn) %*% solve(Omegan[[i]]) %*% Xn
        Xnyn[!coefna[i, ], ]             <- CP.tXn.solve_Omegan_i %*% yn # == t(Xn) %*% solve(Omegan[[i]]) %*% yn
        list("XnXn" = XnXn, "Xnyn" = Xnyn)
    })
    # Compute coefficients
    # extract and reduce XnXn (pos 1 in list's element) and Xnyn (pos 2)
    # position-wise extraction is faster than name-based extraction
    XpXm1 <-    solve(Reduce("+", vapply(XyOmXy, "[", 1L, FUN.VALUE = list(length(XyOmXy)))))
    beta <- XpXm1 %*% Reduce("+", vapply(XyOmXy, "[", 2L, FUN.VALUE = list(length(XyOmXy))))

    beta.names <- rownames(beta)
    beta <- as.numeric(beta)
    names(beta) <- beta.names

    weightsn <- lapply(seq_len(card.cond),
                       function(i){
                           # YC2019/30/08
                           #old
                           #                           vcovn <- vcov(ols[[i]])
                           #                           Deltan <- Delta[! coefna[i,], ! coefna[i,]]
                           #                           wn <- solve(vcovn + Deltan)
                           #new
                           vcovn <- vcov(ols[[i]])
                           wn <- solve((vcovn + Delta)[!coefna[i, ], !coefna[i, ]])
                           z <- matrix(0, nrow = ncol(coefm), ncol = ncol(coefm),
                                       dimnames = list(colnames(coefm), colnames(coefm)))
                           z[!coefna[i, ], !coefna[i, ]] <- wn
                           z
                       }
    )
    V <- solve(Reduce("+", weightsn))
    weightsn <- lapply(weightsn, function(x) V %*% x)
    ## TODO: should "Beta" be called "beta"?
    Beta <- Reduce("+", lapply(seq_len(card.cond), function(i) weightsn[[i]] %*% coefm[i, ]))
    Beta.names <- rownames(Beta)
    Beta <- as.numeric(Beta)
    names(Beta) <- Beta.names
    XpXm1 <- V

    y <- pmodel.response(data)
    X <- model.matrix(data)
    fit <- as.numeric(tcrossprod(beta, X))
    res <- y - fit
    df.resid <- N - ncol(coefm)

    list(coefficients  = beta,
         residuals     = res,
         fitted.values = fit,
         vcov          = XpXm1,
         df.residual   = df.resid,
         model         = data,
         Delta         = Delta)
}


#' @rdname pvcm
#' @export
summary.pvcm <- function(object, ...) {
    model <- describe(object, "model")
    if (model == "random") {

        coef_wo_int <- object$coefficients[!(names(coef(object)) %in% "(Intercept)")]
        int.only <- !length(coef_wo_int)
        object$waldstatistic <- if(!int.only) pwaldtest(object) else NULL
        std.err <- sqrt(diag(vcov(object)))
        b <- object$coefficients
        z <- b / std.err
        p <- 2 * pnorm(abs(z), lower.tail = FALSE)
        coef <- cbind(b, std.err, z, p)
        colnames(coef) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
        object$coefficients <- coef
    }
    object$ssr <- deviance(object)
    object$tss <- tss(unlist(model.frame(object)))
    object$rsqr <- 1 - object$ssr / object$tss
    class(object) <- c("summary.pvcm", "pvcm")
    return(object)
}

#' @rdname pvcm
#' @export
print.summary.pvcm <- function(x, digits = max(3, getOption("digits") - 2),
                               width = getOption("width"), ...) {
    effect <- describe(x, "effect")
    formula <- formula(x)
    model <- describe(x, "model")
    cat(paste(effect.pvcm.list[effect], " ", sep = ""))
    cat(paste(model.pvcm.list[model], "\n", sep = ""))
    cat("\nCall:\n")
    print(x$call)
    cat("\n")
    print(pdim(model.frame(x)))
    cat("\nResiduals:\n")
    print(sumres(x))
    if (model == "random") {
        cat("\nEstimated mean of the coefficients:\n")
        printCoefmat(x$coefficients, digits = digits)
        cat("\nEstimated variance of the coefficients:\n")
        print(x$Delta, digits = digits)
    }
    if (model == "within") {
        cat("\nCoefficients:\n")
        print(summary(x$coefficients))
    }
    cat("\n")
    cat(paste0("Total Sum of Squares: ",    signif(x$tss, digits), "\n"))
    cat(paste0("Residual Sum of Squares: ", signif(x$ssr, digits), "\n"))
    cat(paste0("Multiple R-Squared: ",      signif(x$rsqr, digits), "\n"))
    if (model == "random" && !is.null(waldstat <- x$waldstatistic)) {
        cat(paste0("Chisq: ", signif(waldstat$statistic), " on ",
                   waldstat$parameter, " DF, p-value: ",
                   format.pval(waldstat$p.value, digits = digits), "\n"))
    }
    invisible(x)
}
# experimental.R#
residuals_overall_exp.plm <- function(x, ...) { #### experimental, non-exported function
    # residuals_overall.plm: gives the residuals of the "overall"/outer model for all types of plm models.
    # In the future, this could be integrated with residuals.plm by some argument, e.g., overall = FALSE (default).
    # see also test file tests/test_residuals_overall_fitted_exp.R

    # no na.action eval yet

    model <- describe(x, "model")

    if (model == "ht") stop("model \"ht\" not (yet?) supported")

    # for all effects of within models: residuals of (quasi-)demeaned (inner) model
    # are also the residuals of the "overall" model
    if (model == "random") {
        # get untransformed data to calculate overall residuals
        X <- model.matrix(x, model = "pooling")
        y <- pmodel.response(x, model = "pooling")
        # take care of any aliased coefficients:
        # they are not in x$coefficients but assoc. variables are still present in model.matrix
        if (any(x$aliased, na.rm = TRUE)) { # na.rm = TRUE because currently, RE tw unbalanced models set aliased differently
            X <- X[ , !x$aliased, drop = FALSE]
        }

        est <- as.numeric(tcrossprod(coef(x), X))
        res <- y - est
        names(res) <- rownames(X)

        # make residuals a pseries
        res <- structure(res, index = index(x), class = c("pseries", class(res)))

    } else { # all plm models except random (and also except ht)
        res <- residuals(x)
    }
    return(res)
}

residuals_overall_e_exp <- function(object) { ### experimental non-exported function
    ## residuals of "overall" RE model minus random effects (=e_it)
    ## e.g.: two-way model: residual_overall_it = random_component_individual_i + random_component_time_t + e_it
    model <- describe(object, "model")
    if (model != "random") stop("only for random effect models")
    obj.eff <- describe(object, "effect")
    res_ov <- residuals_overall_exp.plm(object)
    if (obj.eff == "twoways") {
        res_ov_e <- res_ov - ranef(object, "individual")[index(object, "id")] - ranef(object, "time")[index(object, "time")]
    } else {
        res_ov_e <- res_ov - ranef(object)[index(object, if(obj.eff == "individual") "id" else "time")]
    }
    names(res_ov_e) <- names(res_ov)
    return(res_ov_e)
}

fitted_exp.plm <- function(x, ...) { #### experimental, non-exported function
    # fitted_exp.plm: gives the fitted values of all types of plm models by subtracting the overall
    #                 residuals from the untransformed response variable; does not have
    #                 a model argument so it is not as versatile as 'fitted.plm' below.
    # see also test file tests/test_residuals_overall_fitted_exp.R
    model <- describe(x, "model")
    res <- residuals_overall_exp.plm(x)

    # For "between" and "fd" models, the number of fitted values is not equal to the
    # number of original observations. Thus, model.frame cannot be used but rather
    # pmodel.response because it has the right length. However, pmodel.response
    # shall not be used for the other models because we want the untransformed data.
    y <- if (model %in% c("between", "fd")) pmodel.response(x) else model.frame(x)[ , 1L]
    return(y - res)
}



# check_propagation_correct_class: helper function
# Function checks if the class and storage mode (type) of an object match
# and corrects its class attribute if not
#
# A mismatch can occur if a pseries of lower class and type logical or integer
# are propagated to higher type by an arithmetic operation as R's arithmetic
# operations do not change the first value of class attribute for
# c("pseries", "logical/integer"). However, using groupGenerics as wrapper around
# pseries objects, this does not happen anymore.
# E.g.,
#  x <- c(1L, 2L, 3L)
#  x + 1.5
# results in class propagation from class "integer" to "numeric"
# but not if x is of class c("myclass", "integer")
check_propagation_correct_class <- function(x) {
    # x: a pseries object (usually)
    if (any((pos <- inherits(x, c("logical" ,"integer", "numeric"), which = TRUE)) > 0)) {
        pos <- pos[pos > 0] # non-matches in inherits(..., which = TRUE) results in 0
        switch(typeof(x),
               "double"  = { attr(x, "class")[pos] <- "numeric" },
               "integer" = { attr(x, "class")[pos] <- "integer" },
               "complex" = { attr(x, "class")[pos] <- "complex" }
        )
    }
    return(x)
}
# groupGenerics_pseries.R#
## groupGenerics for operations on pseries
## see ?groupGeneric
## see tests/test_groupGenerics_pseries.R for examples
##
## implemented wrappers for groups Ops, Math, Complex
##
## group generic for Summary (all, any, sum, prod, min, max, range) not needed
## as functions in this group do not change the data type
##
## groupGenerics need to be registered in NAMESPACE
##
## groupGenerics are used to allow automatic propagation to higher/lower data type
## when operations are performed on pseries,
## e.g., class c("pseries", "integer") -> c("pseries", "numeric") when a function
## takes an integer as input and outputs a numeric. Without the group generics,
## the class of the results would stay as c("pseries", "integer") while the values
## themselves are numerics. The associated test file demonstrates the behaviour,
## see tests/test_groupGenerics_pseries.R


## helper functions: remove_pseries_features and add_pseries_features
remove_pseries_features <- function(x) {

    # debug:
    #  if (!is.pseries(x)) warning("removing pseries features now but object was not a proper pseries before")

    attr(x, "index") <- NULL
    # unclass is simpler and faster than previously (up to and incl. rev. 1307) used
    # combination of check_propagation_correct_class() and class() <- setdiff(class(<.>), "pseries")
    # unclass handles propagation and keeps names but coerces factor to integer
    x <- if(!is.factor(x)) unclass(x) else { class(x) <- setdiff(class(x), "pseries"); x }
    x
}

add_pseries_features <- function(x, index) {
    # debug:
    #  if (is.null(index)) warning("'index' is null")

    attr(x, "index") <- index
    class(x) <- unique(c("pseries", class(x)))
    return(x)
}

#' @export
Ops.pseries <- function(e1, e2) {
    #  print("Ops.pseries executed!") # debug output

    miss_e2 <- missing(e2)
    e1_pseries <- e2_pseries <- FALSE
    # either one or both could be pseries
    if(inherits(e1, "pseries")) {
        e1_pseries <- TRUE
        index_e1 <- attr(e1, "index")
        e1 <- remove_pseries_features(e1)
    }

    if(!miss_e2 && inherits(e2, "pseries")) {
        e2_pseries <- TRUE
        index_e2 <- attr(e2, "index")
        e2 <- remove_pseries_features(e2)
    }

    res <- if(!miss_e2) get(.Generic)(e1, e2) else get(.Generic)(e1)

    # result could be, e.g., matrix. So check if adding back pseries features
    # makes sense (e.g., do not create something of class c("pseries", "matrix")).
    # Need is.atomic because is.vector is too strict, however need to sort out
    # some other data types
    add_back_pseries <- if(is.atomic(res) && !is.matrix(res) && !is.pairlist(res)) TRUE else FALSE
    if(add_back_pseries) {
        if(miss_e2 && e1_pseries)      relevant_index <- index_e1
        if( e1_pseries && !e2_pseries) relevant_index <- index_e1
        if(!e1_pseries &&  e2_pseries) relevant_index <- index_e2
        if( e1_pseries &&  e2_pseries) {
            # decide on index for result:
            # if objects vary in length: shorter object is recycled by R
            #  -> must take index of non-recycled object (= longer pseries)
            #
            # Also, base R uses the names of the first operand -> additional justification
            # to assign index_e1 in case of same number of rows
            relevant_index <- if(nrow(index_e1) >= nrow(index_e2)) index_e1 else index_e2

            # do not warn anymore (since rev. 1181)
            #    if ((nrow(index_e1) == nrow(index_e2)) && !isTRUE(all.equal(index_e1, index_e2)))
            #      warning("indexes of pseries have same length but not same content: result was assigned first operand's index")
        }
        res <- add_pseries_features(res, relevant_index)
    }

    return(res)
}

#' @export
Math.pseries <- function(x, ...) {
    #  print("Math.pseries executed!") # debug output

    index <- attr(x, "index")
    x <- remove_pseries_features(x)

    x <- get(.Generic)(x, ...)
    x <- add_pseries_features(x, index)
    return(x)
}

#' @export
Complex.pseries <- function(z) {
    #  print("Complex.pseries executed!") # debug output

    index <- attr(z, "index")
    z <- remove_pseries_features(z)

    z <- get(.Generic)(z)
    z <- add_pseries_features(z, index)
    return(z)
}
# is.pconsecutive_pbalanced.R#
########### is.pconsecutive ##############
# little helper function to determine if the time periods of an object are consecutive per id.
# By consecutive we mean "consecutive in the numbers", i.e., is.pconsecutive takes the numerical
# value of the time variable into account: t, t+1, t+2, ... where t is an integer
#
# For this, we need as.numeric(as.character(time_var)) where as.character is a crucial part!
# Equivalent but more efficient is as.numeric(levels(id_timevar))[as.integer(id_timevar)]
# (see R FAQ 7.10 for coercing factors to numeric]
# and the coerction of time_var in this manner needs to be meaningful numbers.
#
# see also in separate file make.pconsecutive.R:
#   * make.pconsecutive
#   * make.pbalanced


#' Check if time periods are consecutive
#'
#' This function checks for each individual if its associated time periods are
#' consecutive (no "gaps" in time dimension per individual)
#'
#' (p)data.frame, pseries and estimated panelmodel objects can be tested if
#' their time periods are consecutive per individual.  For evaluation of
#' consecutiveness, the time dimension is interpreted to be numeric, and the
#' data are tested for being a regularly spaced sequence with distance 1
#' between the time periods for each individual (for each individual the time
#' dimension can be interpreted as sequence t, t+1, t+2, \ldots{} where t is an
#' integer). As such, the "numerical content" of the time index variable is
#' considered for consecutiveness, not the "physical position" of the various
#' observations for an individuals in the (p)data.frame/pseries (it is not
#' about "neighbouring" rows). If the object to be evaluated is a pseries or a
#' pdata.frame, the time index is coerced from factor via as.character to
#' numeric, i.e., the series
#' `as.numeric(as.character(index(<pseries/pdata.frame>)[[2]]))]` is
#' evaluated for gaps.
#'
#' The default method also works for argument `x` being an arbitrary
#' vector (see **Examples**), provided one can supply arguments `id`
#' and `time`, which need to ordered as stacked time series. As only
#' `id` and `time` are really necessary for the default method to
#' evaluate the consecutiveness, `x = NULL` is also possible. However, if
#' the vector `x` is also supplied, additional input checking for equality
#' of the lengths of `x`, `id` and `time` is performed, which is
#' safer.
#'
#' For the data.frame interface, the data is ordered in the appropriate way
#' (stacked time series) before the consecutiveness is evaluated. For the
#' pdata.frame and pseries interface, ordering is not performed because both
#' data types are already ordered in the appropriate way when created.
#'
#' Note: Only the presence of the time period itself in the object is tested,
#' not if there are any other variables.  `NA` values in individual index
#' are not examined but silently dropped - In this case, it is not clear which
#' individual is meant by id value `NA`, thus no statement about
#' consecutiveness of time periods for those "`NA`-individuals" is
#' possible.
#'
#' @name is.pconsecutive
#' @aliases is.pconsecutive
#' @param x usually, an object of class `pdata.frame`,
#'     `data.frame`, `pseries`, or an estimated
#'     `panelmodel`; for the default method `x` can also be
#'     an arbitrary vector or `NULL`, see **Details**,
#' @param na.rm.tindex logical indicating whether any `NA` values
#'     in the time index are removed before consecutiveness is
#'     evaluated (defaults to `FALSE`),
#' @param index only relevant for `data.frame` interface; if
#'     `NULL`, the first two columns of the data.frame are
#'     assumed to be the index variables; if not `NULL`, both
#'     dimensions ('individual', 'time') need to be specified by
#'     `index` for `is.pconsecutive` on data frames, for
#'     further details see [pdata.frame()],
#' @param id,time only relevant for default method: vectors specifying
#'     the id and time dimensions, i. e., a sequence of individual and
#'     time identifiers, each as stacked time series,
#' @param \dots further arguments.
#' @return A named `logical` vector (names are those of the
#'     individuals). The i-th element of the returned vector
#'     corresponds to the i-th individual. The values of the i-th
#'     element can be: \item{TRUE}{if the i-th individual has
#'     consecutive time periods,} \item{FALSE}{if the i-th
#'     individual has non-consecutive time periods,}
#'     \item{"NA"}{if there are any NA values in time index of
#'     the i-th the individual; see also argument `na.rm.tindex`
#'     to remove those.}
#' @export
#' @author Kevin Tappe
#' @seealso [make.pconsecutive()] to make data consecutive
#'     (and, as an option, balanced at the same time) and
#'     [make.pbalanced()] to make data balanced.\cr
#'     [pdim()] to check the dimensions of a 'pdata.frame'
#'     (and other objects), [pvar()] to check for individual
#'     and time variation of a 'pdata.frame' (and other objects),
#'     [lag()] for lagged (and leading) values of a
#'     'pseries' object.\cr
#'
#' [pseries()], [data.frame()], [pdata.frame()],
#' for class 'panelmodel' see [plm()] and [pgmm()].
#' @keywords attribute
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' is.pconsecutive(Grunfeld)
#' is.pconsecutive(Grunfeld, index=c("firm", "year"))
#'
#' # delete 2nd row (2nd time period for first individual)
#' # -> non consecutive
#' Grunfeld_missing_period <- Grunfeld[-2, ]
#' is.pconsecutive(Grunfeld_missing_period)
#' all(is.pconsecutive(Grunfeld_missing_period)) # FALSE
#'
#' # delete rows 1 and 2 (1st and 2nd time period for first individual)
#' # -> consecutive
#' Grunfeld_missing_period_other <- Grunfeld[-c(1,2), ]
#' is.pconsecutive(Grunfeld_missing_period_other) # all TRUE
#'
#' # delete year 1937 (3rd period) for _all_ individuals
#' Grunfeld_wo_1937 <- Grunfeld[Grunfeld$year != 1937, ]
#' is.pconsecutive(Grunfeld_wo_1937) # all FALSE
#'
#' # pdata.frame interface
#' pGrunfeld <- pdata.frame(Grunfeld)
#' pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
#' is.pconsecutive(pGrunfeld) # all TRUE
#' is.pconsecutive(pGrunfeld_missing_period) # first FALSE, others TRUE
#'
#'
#' # panelmodel interface (first, estimate some models)
#' mod_pGrunfeld <- plm(inv ~ value + capital, data = Grunfeld)
#' mod_pGrunfeld_missing_period <- plm(inv ~ value + capital, data = Grunfeld_missing_period)
#'
#' is.pconsecutive(mod_pGrunfeld)
#' is.pconsecutive(mod_pGrunfeld_missing_period)
#'
#' nobs(mod_pGrunfeld) # 200
#' nobs(mod_pGrunfeld_missing_period) # 199
#'
#'
#' # pseries interface
#' pinv <- pGrunfeld$inv
#' pinv_missing_period <- pGrunfeld_missing_period$inv
#'
#' is.pconsecutive(pinv)
#' is.pconsecutive(pinv_missing_period)
#'
#' # default method for arbitrary vectors or NULL
#' inv <- Grunfeld$inv
#' inv_missing_period <- Grunfeld_missing_period$inv
#' is.pconsecutive(inv, id = Grunfeld$firm, time = Grunfeld$year)
#' is.pconsecutive(inv_missing_period, id = Grunfeld_missing_period$firm,
#'                                     time = Grunfeld_missing_period$year)
#'
#' # (not run) demonstrate mismatch lengths of x, id, time
#' # is.pconsecutive(x = inv_missing_period, id = Grunfeld$firm, time = Grunfeld$year)
#'
#' # only id and time are needed for evaluation
#' is.pconsecutive(NULL, id = Grunfeld$firm, time = Grunfeld$year)
#'
is.pconsecutive <- function(x, ...){
    UseMethod("is.pconsecutive")
}

#' @rdname is.pconsecutive
#' @export
is.pconsecutive.default <- function(x, id, time, na.rm.tindex = FALSE, ...) {
    # argument 'x' just used for input check (if it is not NULL and is atomic)

    # input checks
    if(length(id) != length(time))
        stop(paste0("arguments 'id' and 'time' must have same length: length(id): ", length(id), ", length(time) ", length(time)))

    if(!is.null(x) && is.atomic(x)) { # is.atomic was once is.vector, but is.vector is too strict as a factor is not a vector
        if(!(length(x) == length(id) && length(x) == length(time) && length(id) == length(time)))
            stop(paste0("arguments 'x', 'id', 'time' must have same length: length(x): ",
                        length(x), ", length(id): ", length(id), ", length(time): ", length(time)))
    }

    # NB: 'time' is assumed to be organised as stacked time series (sorted for each individual)
    #     (successive blocks of individuals, each block being a time series for the respective individual))
    #
    #   'time' is in the correct order if is.pconsecutive.default is called by
    #   is.pconsecutive.pdata.frame or is.pconsecutive.pseries as a pdata.frame (which is sorted) was constructed
    #   in the first place; for data.frame interface the ordering is done in the respective function

    if(na.rm.tindex) {
        NA_tindex <- is.na(time)
        time <- time[!NA_tindex]
        id <- id[!NA_tindex]
    }

    # if time var is factor (as is TRUE for pdata.frames, pseries):
    # need to convert to numeric, do this by coering to character first (otherwise wrong results!)
    #  see R FAQ 7.10 for coercing factors to numeric:
    #      as.numeric(levels(factor_var))[as.integer(factor_var)]   is more efficient than
    #      as.numeric(as.character(factor_var))
    if(!is.numeric(time) && is.factor(time)) time <- as.numeric(levels(time))[as.integer(time)]

    list_id_timevar <- split(time, id, drop = TRUE)

    res <- vapply(list_id_timevar, function(id_timevar) { if(anyNA(id_timevar)) {
        NA # return NA if NA found in the time periods for individual
    } else {
        begin <- id_timevar[1L]
        end   <- id_timevar[length(id_timevar)]

        # compare to length(original id_timevar) to find out if times are consecutive
        (end - begin + 1L) == length(id_timevar)

        # Alternative way of checking:
        # consecutive time periods from begin to end (if id_timevar were consecutive)
        # consecutive <- seq(from = begin, to = end, by = 1)
        # length(consecutive) == length(id_timevar)
    }
    }, FUN.VALUE = TRUE)

    return(res)
}

#' @rdname is.pconsecutive
#' @export
is.pconsecutive.data.frame <- function(x, index = NULL, na.rm.tindex = FALSE, ...){
    if (!is.null(index) && length(index) != 2L)
        stop("if argument 'index' is not NULL, 'index' needs to specify
         'individual' and 'time' dimension for is.pconsecutive to work on a data.frame")

    # if index not provided, assume first two columns to be the index vars
    index_orig_names <- if(is.null(index)) names(x)[1:2] else index

    id   <- x[ , index_orig_names[1L]]
    time <- x[ , index_orig_names[2L]]

    # order as stacked time series (by id and time) first, otherwise default method does not work correctly!
    ord <- order(id, time)
    x_ordered    <- x[ord, ]
    id_ordered   <- id[ord]
    time_ordered <- time[ord]

    #  if (!identical(x, x_ordered))
    #    print("Note: for test of consecutiveness of time periods, the data.frame was ordered by index variables (id, time)")

    return(is.pconsecutive.default(x_ordered, id_ordered, time_ordered, na.rm.tindex = na.rm.tindex, ...))
}

#' @rdname is.pconsecutive
#' @export
is.pconsecutive.pseries <- function(x, na.rm.tindex = FALSE, ...){
    index <- unclass(attr(x, "index")) # unclass for speed
    return(is.pconsecutive.default(x, index[[1L]], index[[2L]], na.rm.tindex = na.rm.tindex, ...))
}


#' @rdname is.pconsecutive
#' @export
is.pconsecutive.pdata.frame <- function(x, na.rm.tindex = FALSE, ...){
    index <- unclass(attr(x, "index")) # unclass for speed
    return(is.pconsecutive.default(x, index[[1L]], index[[2L]], na.rm.tindex = na.rm.tindex, ...))
}

#' @rdname is.pconsecutive
#' @export
is.pconsecutive.panelmodel <- function(x, na.rm.tindex = FALSE, ...){
    index <- unclass(attr(x$model, "index")) # unclass for speed
    # can determine solely based on indexes:
    return(is.pconsecutive.default(NULL, index[[1L]], index[[2L]], na.rm.tindex = na.rm.tindex, ...))
}


########### is.pbalanced ##############
### for convenience and to be faster than pdim() for the purpose
### of the determination of balancedness only, because it avoids
### pdim()'s calculations which are unnecessary for balancedness.
###
### copied (and adapted) methods and code from pdim.*
### (only relevant parts to determine balancedness)


#' Check if data are balanced
#'
#' This function checks if the data are balanced, i.e., if each individual has
#' the same time periods
#'
#' Balanced data are data for which each individual has the same time periods.
#' The returned values of the `is.pbalanced(object)` methods are identical
#' to `pdim(object)$balanced`.  `is.pbalanced` is provided as a short
#' cut and is faster than `pdim(object)$balanced` because it avoids those
#' computations performed by `pdim` which are unnecessary to determine the
#' balancedness of the data.
#'
#' @aliases is.pbalanced
#' @param x an object of class `pdata.frame`, `data.frame`,
#'     `pseries`, `panelmodel`, or `pgmm`,
#' @param y (only in default method) the time index variable (2nd index
#' variable),
#' @param index only relevant for `data.frame` interface; if
#'     `NULL`, the first two columns of the data.frame are
#'     assumed to be the index variables; if not `NULL`, both
#'     dimensions ('individual', 'time') need to be specified by
#'     `index` as character of length 2 for data frames, for
#'     further details see [pdata.frame()],
#' @param \dots further arguments.
#' @return A logical indicating whether the data associated with
#'     object `x` are balanced (`TRUE`) or not
#'     (`FALSE`).
#' @seealso [punbalancedness()] for two measures of
#'     unbalancedness, [make.pbalanced()] to make data
#'     balanced; [is.pconsecutive()] to check if data are
#'     consecutive; [make.pconsecutive()] to make data
#'     consecutive (and, optionally, also balanced).\cr
#'     [pdim()] to check the dimensions of a 'pdata.frame'
#'     (and other objects), [pvar()] to check for individual
#'     and time variation of a 'pdata.frame' (and other objects),
#'     [pseries()], [data.frame()],
#'     [pdata.frame()].
#' @export
#' @keywords attribute
#' @examples
#'
#' # take balanced data and make it unbalanced
#' # by deletion of 2nd row (2nd time period for first individual)
#' data("Grunfeld", package = "plm")
#' Grunfeld_missing_period <- Grunfeld[-2, ]
#' is.pbalanced(Grunfeld_missing_period)     # check if balanced: FALSE
#' pdim(Grunfeld_missing_period)$balanced    # same
#'
#' # pdata.frame interface
#' pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
#' is.pbalanced(Grunfeld_missing_period)
#'
#' # pseries interface
#' is.pbalanced(pGrunfeld_missing_period$inv)
#'
is.pbalanced <- function(x, ...) {
    UseMethod("is.pbalanced")
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.default <- function(x, y, ...) {
    if (length(x) != length(y)) stop("The length of the two inputs differs\n")
    x <- x[drop = TRUE] # drop unused factor levels so that table
    y <- y[drop = TRUE] # gives only needed combinations
    z <- table(x, y)
    balanced <- if(any(v <- as.vector(z) == 0L)) FALSE else TRUE
    if (any(v > 1L)) warning("duplicate couples (id-time)\n")
    return(balanced)
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.data.frame <- function(x, index = NULL, ...) {
    x <- pdata.frame(x, index)
    index <- unclass(attr(x, "index")) # unclass for speed
    return(is.pbalanced(index[[1L]], index[[2L]]))
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.pdata.frame <- function(x, ...) {
    index <- unclass(attr(x, "index")) # unclass for speed
    return(is.pbalanced(index[[1L]], index[[2L]]))
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.pseries <- function(x, ...) {
    index <- unclass(attr(x, "index")) # unclass for speed
    return(is.pbalanced(index[[1L]], index[[2L]]))
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.pggls <- function(x, ...) {
    # pggls is also class panelmodel, but take advantage of its pdim attribute
    return(attr(x, "pdim")$balanced)
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.pcce <- function(x, ...) {
    # pcce is also class panelmodel, but take advantage of its pdim attribute
    return(attr(x, "pdim")$balanced)
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.pmg <- function(x, ...) {
    # pmg is also class panelmodel, but take advantage of its pdim attribute
    return(attr(x, "pdim")$balanced)
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.pgmm <- function(x, ...) {
    # pgmm is also class panelmodel, but take advantage of its pdim attribute
    return(attr(x, "pdim")$balanced)
}

#' @rdname is.pbalanced
#' @export
is.pbalanced.panelmodel <- function(x, ...) {
    x <- model.frame(x)
    return(is.pbalanced(x))
}
# make.pconsecutive_pbalanced.R#
### This file:
### make.pconsecutive.*
### make.pbalanced.*
###
### is.pconsecutive.* is in separate file is.pconsecutive.R

#  consecutive: "consecutive in the numbers": t, t+1, t+2, ... where t is an integer,
#                i.e., the time index var is interpreted as a numerical
#

## in the future, maybe make.pconsective could gain an additional argument 'fill' for the filled value (currently NA)
##      if so, check other packages (data.table, dplyr, tidyr, ...) what the argument is called there
##      arg would need to be a (named) list (for (p)data.frame methods) because columns of
##      (p)data.frames are of arbitrary classes


#' Make data consecutive (and, optionally, also balanced)
#'
#' This function makes the data consecutive for each individual (no "gaps" in
#' time dimension per individual) and, optionally, also balanced
#'
#' (p)data.frame and pseries objects are made consecutive, meaning their time
#' periods are made consecutive per individual.  For consecutiveness, the time
#' dimension is interpreted to be numeric, and the data are extended to a
#' regularly spaced sequence with distance 1 between the time periods for each
#' individual (for each individual the time dimension become a sequence t, t+1,
#' t+2, \ldots{}, where t is an integer). Non--index variables are filled with
#' `NA` for the inserted elements (rows for (p)data.frames, vector
#' elements for pseries).
#'
#' With argument `balanced = TRUE`, additionally to be made consecutive,
#' the data also can be made a balanced panel/pseries.  Note: This means
#' consecutive AND balanced; balancedness does not imply consecutiveness. In
#' the result, each individual will have the same time periods in their time
#' dimension by taking the min and max of the time index variable over all
#' individuals (w/o `NA` values) and inserting the missing time periods.
#' Looking at the number of rows of the resulting (pdata.frame) (elements for
#' pseries), this results in `nrow(make.pconsecutive(<.>, balanced = FALSE))` <=
#' `nrow(make.pconsecutive(<.>, balanced = TRUE))`. For making the data only
#' balanced, i.e., not demanding consecutiveness at the same time, use
#' [make.pbalanced()] (see **Examples** for a comparison)).
#'
#' Note: rows of (p)data.frames (elements for pseries) with `NA` values in
#' individual or time index are not examined but silently dropped before the
#' data are made consecutive. In this case, it is not clear which individual or
#' time period is meant by the missing value(s). Especially, this means: If
#' there are `NA` values in the first/last position of the original time
#' periods for an individual, which usually depicts the beginning and ending of
#' the time series for that individual, the beginning/end of the resulting time
#' series is taken to be the min and max (w/o `NA` values) of the original
#' time series for that individual, see also **Examples**. Thus, one might
#' want to check if there are any `NA` values in the index variables
#' before applying `make.pconsecutive`, and especially check for `NA` values
#' in the first and last position for each individual in original data and, if
#' so, maybe set those to some meaningful begin/end value for the time series.
#'
#' @aliases make.pconsecutive
#' @param x an object of class `pdata.frame`, `data.frame`,
#'     or `pseries`,
#' @param balanced logical, indicating whether the data should
#'     _additionally_ be made balanced (default: FALSE),
#' @param index only relevant for `data.frame` interface; if
#'     `NULL`, the first two columns of the data.frame are
#'     assumed to be the index variables; if not `NULL`, both
#'     dimensions ('individual', 'time') need to be specified by
#'     `index` as character of length 2 for data frames, for
#'     further details see [pdata.frame()],
#' @param \dots further arguments.
#' @return An object of the same class as the input `x`, i.e., a
#'     pdata.frame, data.frame or a pseries which is made
#'     time--consecutive based on the index variables. The returned
#'     data are sorted as a stacked time series.
#' @export
#' @author Kevin Tappe
#' @seealso [is.pconsecutive()] to check if data are
#'     consecutive; [make.pbalanced()] to make data only
#'     balanced (not consecutive).\cr [punbalancedness()]
#'     for two measures of unbalancedness, [pdim()] to check
#'     the dimensions of a 'pdata.frame' (and other objects),
#'     [pvar()] to check for individual and time variation
#'     of a 'pdata.frame' (and other objects), [lag()] for
#'     lagged (and leading) values of a 'pseries' object.\cr
#'     [pseries()], [data.frame()],
#'     [pdata.frame()].
#' @keywords attribute
#' @examples
#'
#' # take data and make it non-consecutive
#' # by deletion of 2nd row (2nd time period for first individual)
#' data("Grunfeld", package = "plm")
#' nrow(Grunfeld)                             # 200 rows
#' Grunfeld_missing_period <- Grunfeld[-2, ]
#' is.pconsecutive(Grunfeld_missing_period)   # check for consecutiveness
#' make.pconsecutive(Grunfeld_missing_period) # make it consecutiveness
#'
#'
#' # argument balanced:
#' # First, make data non-consecutive and unbalanced
#' # by deletion of 2nd time period (year 1936) for all individuals
#' # and more time periods for first individual only
#' Grunfeld_unbalanced <- Grunfeld[Grunfeld$year != 1936, ]
#' Grunfeld_unbalanced <- Grunfeld_unbalanced[-c(1,4), ]
#' all(is.pconsecutive(Grunfeld_unbalanced)) # FALSE
#' pdim(Grunfeld_unbalanced)$balanced        # FALSE
#'
#' g_consec_bal <- make.pconsecutive(Grunfeld_unbalanced, balanced = TRUE)
#' all(is.pconsecutive(g_consec_bal)) # TRUE
#' pdim(g_consec_bal)$balanced        # TRUE
#' nrow(g_consec_bal)                 # 200 rows
#' head(g_consec_bal)                 # 1st individual: years 1935, 1936, 1939 are NA
#'
#' g_consec <- make.pconsecutive(Grunfeld_unbalanced) # default: balanced = FALSE
#' all(is.pconsecutive(g_consec)) # TRUE
#' pdim(g_consec)$balanced        # FALSE
#' nrow(g_consec)                 # 198 rows
#' head(g_consec)                 # 1st individual: years 1935, 1936 dropped, 1939 is NA
#'
#'
#' # NA in 1st, 3rd time period (years 1935, 1937) for first individual
#' Grunfeld_NA <- Grunfeld
#' Grunfeld_NA[c(1, 3), "year"] <- NA
#' g_NA <- make.pconsecutive(Grunfeld_NA)
#' head(g_NA)        # 1936 is begin for 1st individual, 1937: NA for non-index vars
#' nrow(g_NA)        # 199, year 1935 from original data is dropped
#'
#'
#' # pdata.frame interface
#' pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
#' make.pconsecutive(Grunfeld_missing_period)
#'
#'
#' # pseries interface
#' make.pconsecutive(pGrunfeld_missing_period$inv)
#'
#'
#' # comparison to make.pbalanced (makes the data only balanced, not consecutive)
#' g_bal <- make.pbalanced(Grunfeld_unbalanced)
#' all(is.pconsecutive(g_bal)) # FALSE
#' pdim(g_bal)$balanced        # TRUE
#' nrow(g_bal) # 190 rows
#'
make.pconsecutive <- function(x, ...){
    UseMethod("make.pconsecutive")
}

# no export needed
make.pconsecutive.indexes <- function(x, index, balanced = FALSE, ...) {
    # make.pconsecutive.indexes: helper function, not exported
    # returns list with 3 elements:
    #   1 "consec_index":        consecutive data.frame to serve as the new index data.frame in other functions,
    #   2 "NArows_former_index": information about dropped lines (logical vector with length of original data)
    #   3 "has_fancy_rownames":  logical whether fancy row.names were used in original data (can only be TRUE for pdata.frame or pseries)

    if (inherits(x, "pdata.frame") || inherits(x, "pseries")) {
        pdataframe_or_pseries <- TRUE
        index_orig <- attr(x, which = "index")
        id_orig    <- index_orig[[1L]] # can leave as factor if it is a factor
        times_orig <- index_orig[[2L]]
        if (!is.numeric(times_orig) && is.factor(times_orig)) times_orig <- as.numeric(levels(times_orig))[as.integer(times_orig)]
        # time var needs to be numeric [as.character needed here!]
        # [R FAQ 7.10 for coercing factors to numeric
        # as.numeric(levels(factor_var))[as.integer(factor_var)] is more efficient than as.numeric(as.character(factor_var))

        # check if fancy rownames are used (to restore them later)
        if (inherits(x, "pseries")) {
            has_fancy_rownames <- isTRUE(all.equal(names(x), fancy.row.names(index_orig)))
            rownames_mode <- mode(attr(x, "names"))
            rownames_typeof <- typeof(attr(x, "names"))
        } else {
            # pdata.frame
            has_fancy_rownames <- isTRUE(all.equal(row.names(x), fancy.row.names(index_orig)))
            rownames_mode <- mode(attr(x, "row.names"))
            rownames_typeof <- typeof(attr(attr(x, "index"), "row.names")) # here we want the typeof of the index

        }
    }
    if (inherits(x, "data.frame") && !inherits(x, "pdata.frame")) {
        # x is a data.frame, but no pdata.frame
        pdataframe_or_pseries <- FALSE
        has_fancy_rownames    <- FALSE
        index_orig <- x[ , index]
        id_orig    <- index_orig[[1L]]
        times_orig <- index_orig[[2L]]
        id_orig_typeof    <- typeof(id_orig)
        times_orig_typeof <- typeof(times_orig)
        rownames_mode <- mode(attr(x, "row.names"))
        rownames_typeof <- typeof(attr(x, "row.names"))

    }

    df_index <- data.frame(id = id_orig, times = times_orig)

    # remove any rows with NA in id or time variable as it is impossible to
    # infer their values, thus: drop them
    is_NA <- is.na(id_orig) | is.na(times_orig)
    df_index <- df_index[!is_NA, ]

    n_id_orig <- length(unique(id_orig))

    if (!balanced) {
        min_values <- by(df_index[ , "times"], df_index[ , "id"], min)
        max_values <- by(df_index[ , "times"], df_index[ , "id"], max)

        times_filled_list <- sapply(seq_len(n_id_orig), function(i) {
            seq(from = min_values[i], to = max_values[i], by = 1)
        }, simplify = FALSE, USE.NAMES = FALSE)

    } else {
        min_value <- min(df_index[, "times"])
        max_value <- max(df_index[, "times"])

        times_filled_list <- sapply(seq_len(n_id_orig), function(i) {
            seq(from = min_value, to = max_value, by = 1)
        }, simplify = FALSE, USE.NAMES = FALSE)
    }

    times_filled_vector <- unlist(times_filled_list, use.names = FALSE)
    id_times <- lengths(times_filled_list, use.names = FALSE)

    id_filled_vector <- unlist(mapply(rep, unique(id_orig), id_times, SIMPLIFY = FALSE), use.names = FALSE)
    # SIMPLIFY = FALSE => always return list

    df_index_filled <- data.frame(id = id_filled_vector, times = times_filled_vector)
    names(df_index_filled)[1:2] <- names(index_orig)[1:2] # set original index names


    if (pdataframe_or_pseries) {
        df_index_filled[ , 1L] <- as.factor(df_index_filled[ , 1L])
        df_index_filled[ , 2L] <- as.factor(df_index_filled[ , 2L])
        class(df_index_filled) <- c("pindex", class(df_index_filled))
    } else {
        if (typeof(df_index_filled[ , 1L]) != id_orig_typeof)    { mode(df_index_filled[ , 1L]) <- id_orig_typeof    }
        if (typeof(df_index_filled[ , 2L]) != times_orig_typeof) { mode(df_index_filled[ , 2L]) <- times_orig_typeof }
    }

    # restore mode of row.names attribute
    # [was changed by above code due to some simplification by R's standard behaviour]
    mode(attr(df_index_filled, "row.names")) <- rownames_typeof

    res <- list(consec_index         = df_index_filled,
                NArows_former_index  = is_NA,
                has_fancy_rownames   = has_fancy_rownames)

    return(res)
} ### END: make.pconsecutive.indexes


#' @rdname make.pconsecutive
#' @export
make.pconsecutive.data.frame <- function(x, balanced = FALSE, index = NULL, ...){
    # if not NULL, index is must be character of length 2
    if (!is.null(index) && length(index) != 2L)
        stop("if argument 'index' is not NULL, 'index' needs to specify
         'individual' and 'time' dimension for make.pconsecutive to work on a data.frame")

    # assume first two columns to be the index vars
    index_orig_names <- if(is.null(index)) names(x)[1:2] else index

    list_ret_make_index <- make.pconsecutive.indexes(x, index_orig_names, balanced = balanced, ...)

    index_df_filled    <- list_ret_make_index[["consec_index"]]
    NArows_old_index   <- list_ret_make_index[["NArows_former_index"]]
    has_fancy_rownames <- list_ret_make_index[["has_fancy_rownames"]]

    # silently drop rows with NA in either individual or time variable of original index
    x <- x[!NArows_old_index, ]

    index_df_filled_plus_x <- merge(index_df_filled, x, by.x = names(index_df_filled)[1:2],
                                    by.y = index_orig_names,
                                    all.x = TRUE)

    # restore mode of row.names attribute [was changed by above code due to some simplification as R's standard behaviour]
    mode(attr(index_df_filled_plus_x, "row.names")) <- typeof(attr(index_df_filled, "row.names"))

    # restore original order of columns, esp. place index vars at original position
    index_df_filled_plus_x <- index_df_filled_plus_x[ , names(x)]

    return(index_df_filled_plus_x)
} ### END: make.pconsecutive.data.frame

#' @rdname make.pconsecutive
#' @export
make.pconsecutive.pdata.frame <- function(x, balanced = FALSE, ...){
    orig_column_names <- names(x)

    list_ret_make_index <- make.pconsecutive.indexes(x, balanced = balanced, ...)
    index_df_filled    <- list_ret_make_index[["consec_index"]]
    NArows_old_index   <- list_ret_make_index[["NArows_former_index"]]
    has_fancy_rownames <- list_ret_make_index[["has_fancy_rownames"]]

    # silently drop rows with NA in either individual or time variable of original index
    # do dropping only if there is any NA row, because calling the subsetting slightly changes the pdata.frame
    if (any(NArows_old_index)) x <- x[!NArows_old_index, ]

    # if index not as vars in pdata.frame: pad index vars in columns 1,2 to enable merging
    # determine position of index vars is c(NA, NA) if index vars are not columns in x
    pos_indexvars <- pos.index(x)
    index_orig_names <- names(pos_indexvars)
    if (anyNA(pos_indexvars)) {
        index_orig <- attr(x, "index")
        x <- cbind(index_orig, x)
    }

    x_df_filled <- merge(index_df_filled, x, by = index_orig_names, all.x = TRUE)
    # merge produces a pdata.frame with 'pseries' in columns (if [.pseries is active])
    # -> remove pseries features from columns
    x_df_filled <- lapply(x_df_filled, remove_pseries_features)

    # make pdata.frame (index vars are already in columns 1,2)
    x_pdf_filled <- pdata.frame(x_df_filled, row.names = has_fancy_rownames)

    # save order of attributes to restore order later
    # attrib_names_before <- names(attributes(x_pdf_filled))

    # restore original order of columns:
    # this also places index vars at original position or drops them if they were not in original pdata.frame
    # (do only if order of columns differs or index is not in pdata.frame to avoid adding extra attributes by subsetting)
    if (!isTRUE(all.equal(orig_column_names, names(x_pdf_filled)))) x_pdf_filled <- x_pdf_filled[ , orig_column_names]

    # restore mode of row.names attribute [was changed by above code due to some simplification as R's standard behaviour]
    mode(attr(attr(x_pdf_filled, "index"), "row.names")) <- typeof(attr(index_df_filled, "row.names"))

    # reorder attributes: subsetting with R's [.data.frame changes order
    # order of attribute shall be assumed to be a set rather than having an order, see do not reorder (see ?attributes)
    ##  attributes(x_pdf_filled) <- attributes(x_pdf_filled)[attrib_names_before]

    return(x_pdf_filled)
} ### END: make.pconsecutive.pdata.frame

#' @rdname make.pconsecutive
#' @export
make.pconsecutive.pseries <- function(x, balanced = FALSE, ...) {
    is_p <- is.pconsecutive(x)
    is_bal <- is.pbalanced(x)
    make_balanced <- balanced == TRUE && !is_bal # consecutive AND balancedness requested but data not balanced
    #  -> independent of the consecutiveness, we need to treat the balancedness

    if (anyNA(is_p) || !all(is_p) || make_balanced) {

        list_ret_make_index <- make.pconsecutive.indexes(x, balanced = balanced, ...)
        df_index_filled    <- list_ret_make_index[["consec_index"]]
        NArows_old_index   <- list_ret_make_index[["NArows_former_index"]]
        has_fancy_rownames <- list_ret_make_index[["has_fancy_rownames"]]

        df_old_index <- attr(x, "index")
        class(df_old_index) <- "data.frame"

        # strip x to its pure form (no index, no class pseries)
        df_old_index$x <- remove_pseries_features(x)

        # silently drop entries with NA in either individual or time variable of original index
        df_old_index <- df_old_index[!NArows_old_index, ]

        df_index_filled_plus_x <- merge(df_index_filled, df_old_index, by.x = names(df_index_filled)[1:2],
                                        by.y = names(df_old_index)[1:2],
                                        all.x = TRUE)

        pdf_index_filled_plus_x <- pdata.frame(df_index_filled_plus_x,
                                               drop.index = FALSE,
                                               row.names = has_fancy_rownames)

        x <- pdf_index_filled_plus_x$x
    }
    return(x)
}



############# make.pbalanced #############
## make.pbalanced.* methods make the input balanced (but not consecutive).
## It does so by either
## balance.type = "fill": filling in only those missing time periods are
##                        introduced that are present for at least one individual
##                        (union of time periods)
##
## balance.type = "shared.times": remove all observations with time periods
##                                not shared among all individuals
##                                (keep intersect of time periods)
##
##                "shared.individuals": drop individuals which don't have all time periods
##                                      (symmetric to "shared.times")



#' Make data balanced
#'
#' This function makes the data balanced, i.e., each individual has the same
#' time periods, by filling in or dropping observations
#'
#' (p)data.frame and pseries objects are made balanced, meaning each
#' individual has the same time periods.  Depending on the value of
#' `balance.type`, the balancing is done in different ways:
#' \itemize{ \item `balance.type = "fill"` (default): The union
#' of available time periods over all individuals is taken (w/o
#' `NA` values).  Missing time periods for an individual are
#' identified and corresponding rows (elements for pseries) are
#' inserted and filled with `NA` for the non--index variables
#' (elements for a pseries).  This means, only time periods present
#' for at least one individual are inserted, if missing.
#'
#' \item `balance.type = "shared.times"`: The intersect of available time
#' periods over all individuals is taken (w/o `NA` values).  Thus, time
#' periods not available for all individuals are discarded, i. e., only time
#' periods shared by all individuals are left in the result).
#'
#' \item `balance.type = "shared.individuals"`: All available time periods
#' are kept and those individuals are dropped for which not all time periods
#' are available, i. e., only individuals shared by all time periods are left
#' in the result (symmetric to `"shared.times"`).  }
#'
#' The data are not necessarily made consecutive (regular time series
#' with distance 1), because balancedness does not imply
#' consecutiveness. For making the data consecutive, use
#' [make.pconsecutive()] (and, optionally, set argument
#' `balanced = TRUE` to make consecutive and balanced, see also
#' **Examples** for a comparison of the two functions.
#'
#' Note: Rows of (p)data.frames (elements for pseries) with `NA`
#' values in individual or time index are not examined but silently
#' dropped before the data are made balanced. In this case, it cannot
#' be inferred which individual or time period is meant by the missing
#' value(s) (see also **Examples**).  Especially, this means:
#' `NA` values in the first/last position of the original time
#' periods for an individual are dropped, which are usually meant to
#' depict the beginning and ending of the time series for that
#' individual.  Thus, one might want to check if there are any
#' `NA` values in the index variables before applying
#' `make.pbalanced`, and especially check for `NA` values in the
#' first and last position for each individual in original data and,
#' if so, maybe set those to some meaningful begin/end value for the
#' time series.
#'
#' @aliases make.pbalanced
#' @param x an object of class `pdata.frame`, `data.frame`,
#'     or `pseries`;
#' @param balance.type character, one of `"fill"`,
#'     `"shared.times"`, or `"shared.individuals"`, see
#'     **Details**,
#' @param index only relevant for `data.frame` interface; if
#'     `NULL`, the first two columns of the data.frame are
#'     assumed to be the index variables; if not `NULL`, both
#'     dimensions ('individual', 'time') need to be specified by
#'     `index` as character of length 2 for data frames, for
#'     further details see [pdata.frame()],
#' @param \dots further arguments.
#' @return An object of the same class as the input `x`, i.e., a
#'     pdata.frame, data.frame or a pseries which is made balanced
#'     based on the index variables. The returned data are sorted as a
#'     stacked time series.
#' @export
#' @author Kevin Tappe
#' @seealso [is.pbalanced()] to check if data are balanced;
#'     [is.pconsecutive()] to check if data are consecutive;
#'     [make.pconsecutive()] to make data consecutive (and,
#'     optionally, also balanced).\cr [punbalancedness()]
#'     for two measures of unbalancedness, [pdim()] to check
#'     the dimensions of a 'pdata.frame' (and other objects),
#'     [pvar()] to check for individual and time variation
#'     of a 'pdata.frame' (and other objects), [lag()] for
#'     lagging (and leading) values of a 'pseries' object.\cr
#'     [pseries()], [data.frame()],
#'     [pdata.frame()].
#' @keywords attribute
#' @examples
#'
#' # take data and make it unbalanced
#' # by deletion of 2nd row (2nd time period for first individual)
#' data("Grunfeld", package = "plm")
#' nrow(Grunfeld)                            # 200 rows
#' Grunfeld_missing_period <- Grunfeld[-2, ]
#' pdim(Grunfeld_missing_period)$balanced    # check if balanced: FALSE
#' make.pbalanced(Grunfeld_missing_period)   # make it balanced (by filling)
#' make.pbalanced(Grunfeld_missing_period, balance.type = "shared.times") # (shared periods)
#' nrow(make.pbalanced(Grunfeld_missing_period))
#' nrow(make.pbalanced(Grunfeld_missing_period, balance.type = "shared.times"))
#'
#' # more complex data:
#' # First, make data unbalanced (and non-consecutive)
#' # by deletion of 2nd time period (year 1936) for all individuals
#' # and more time periods for first individual only
#' Grunfeld_unbalanced <- Grunfeld[Grunfeld$year != 1936, ]
#' Grunfeld_unbalanced <- Grunfeld_unbalanced[-c(1,4), ]
#' pdim(Grunfeld_unbalanced)$balanced        # FALSE
#' all(is.pconsecutive(Grunfeld_unbalanced)) # FALSE
#'
#' g_bal <- make.pbalanced(Grunfeld_unbalanced)
#' pdim(g_bal)$balanced        # TRUE
#' unique(g_bal$year)          # all years but 1936
#' nrow(g_bal)                 # 190 rows
#' head(g_bal)                 # 1st individual: years 1935, 1939 are NA
#'
#' # NA in 1st, 3rd time period (years 1935, 1937) for first individual
#' Grunfeld_NA <- Grunfeld
#' Grunfeld_NA[c(1, 3), "year"] <- NA
#' g_bal_NA <- make.pbalanced(Grunfeld_NA)
#' head(g_bal_NA)        # years 1935, 1937: NA for non-index vars
#' nrow(g_bal_NA)        # 200
#'
#' # pdata.frame interface
#' pGrunfeld_missing_period <- pdata.frame(Grunfeld_missing_period)
#' make.pbalanced(Grunfeld_missing_period)
#'
#' # pseries interface
#' make.pbalanced(pGrunfeld_missing_period$inv)
#'
#' # comparison to make.pconsecutive
#' g_consec <- make.pconsecutive(Grunfeld_unbalanced)
#' all(is.pconsecutive(g_consec)) # TRUE
#' pdim(g_consec)$balanced        # FALSE
#' head(g_consec, 22)             # 1st individual:   no years 1935/6; 1939 is NA;
#'                                # other indviduals: years 1935-1954, 1936 is NA
#' nrow(g_consec)                 # 198 rows
#'
#' g_consec_bal <- make.pconsecutive(Grunfeld_unbalanced, balanced = TRUE)
#' all(is.pconsecutive(g_consec_bal)) # TRUE
#' pdim(g_consec_bal)$balanced        # TRUE
#' head(g_consec_bal)                 # year 1936 is NA for all individuals
#' nrow(g_consec_bal)                 # 200 rows
#'
#' head(g_bal)                        # no year 1936 at all
#' nrow(g_bal)                        # 190 rows
#'
make.pbalanced <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), ...) {
    UseMethod("make.pbalanced")
}


#' @rdname make.pbalanced
#' @export
make.pbalanced.pdata.frame <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), ...) {

    balance.type <- match.arg(balance.type)
    index <- attr(x, "index")

    switch(balance.type,
           "fill" = {
               x_consec_bal <- make.pconsecutive(x, balanced = TRUE)

               # delete time periods that were not present for any individual, but introduced by
               # making data consecutive
               # result: no time periods are added that are not present for at least one individual
               times_present_orig <- attr(x_consec_bal, "index")[[2L]] %in% unique(index[[2L]])
               result <- x_consec_bal[times_present_orig, ]

               # drop not present factor levels (some new levels were introduced by making data consecutive first):
               # drop from index
               index_result <- attr(result, "index")
               index_result[[2L]] <- droplevels(index_result[[2L]])
               attr(result, "index") <- index_result

               # drop from time column (if time index column present in pdata.frame)
               pos_indexvars <- pos.index(result) # position of index vars is c(NA, NA) if index vars are not present as columns
               index_orig_names <- names(pos_indexvars)
               if (!anyNA(pos_indexvars)) {
                   result[ , pos_indexvars[2L]] <- droplevels(result[ , pos_indexvars[2L]])
               }
           },
           "shared.times" = {
               keep <- intersect_index(index, "time")
               result <- x[keep, ]
           },
           "shared.individuals" = {
               keep <- intersect_index(index, "individual")
               result <- x[keep, ]
           })
    return(result)
} ## END make.pbalanced.pdata.frame


#' @rdname make.pbalanced
#' @export
make.pbalanced.pseries <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), ...) {

    balance.type <- match.arg(balance.type)
    index <- attr(x, "index")

    switch(balance.type,
           "fill" = {
               x_consec_bal <- make.pconsecutive(x, balanced = TRUE)

               # delete time periods that were not present for any individual, but introduced by
               # making data consecutive
               # result: no time periods are added that are not present for at least one individual
               x_consec_bal_index <- attr(x_consec_bal, "index")
               times_present_orig <- x_consec_bal_index[[2L]] %in% unique(index[[2L]])
               result <- x_consec_bal[times_present_orig] # this drops the pseries features (index, class "pseries")
               # because there is no function "[.pseries]" (as of 2016-05-14)

               # drop introduced extra periods also from index
               x_consec_bal_index <- x_consec_bal_index[times_present_orig, ]
               # re-attach index and restore original class(es)
               attr(result, "index") <- x_consec_bal_index
               attr(result, "class") <- attr(x, "class")
           },

           "shared.times" = {
               keep <- intersect_index(index, "time")
               result <- x[keep]
               # restore 'pseries' features
               # (no subsetting method for pseries in the package (yet),
               #  usual vector subsetting removes the pseries features)
               attr(result, "index") <- index[keep, ]
               class(result) <- unique(c("pseries", class(result)))
           },

           "shared.individuals" = {
               keep <- intersect_index(index, "individual")
               result <- x[keep]
               # restore 'pseries' features
               # (no subsetting method for pseries in the package (yet),
               #  usual vector subsetting removes the pseries features)
               attr(result, "index") <- index[keep, ]
               class(result) <- unique(c("pseries", class(result)))
           })
    return(result)
} ## END make.pbalanced.pseries


#' @rdname make.pbalanced
#' @export
make.pbalanced.data.frame <- function(x, balance.type = c("fill", "shared.times", "shared.individuals"), index = NULL, ...) {
    # NB: for data.frame interface: the data is also sorted as stack time series

    balance.type <- match.arg(balance.type)

    ## identify index of data.frame
    # if not NULL, index is must be character of length 2
    if (!is.null(index) && length(index) != 2L)
        stop("if argument 'index' is not NULL, 'index' needs to specify
             'individual' and 'time' dimension for make.pconsecutive to work on a data.frame")

    # assume first two columns to be the index vars
    if (is.null(index)) index_orig_names <- names(x)[1:2]
    else index_orig_names <- index

    index_df <- x[ , index_orig_names]

    switch(balance.type,
           "fill" = {
               x_consec_bal <- make.pconsecutive(x, index = index_orig_names, balanced = TRUE)

               # delete time periods that were not present for any individual, but introduced by
               # making data consecutive
               # result: no time periods are added that are not present for at least one individual
               times_present_orig <- x_consec_bal[ , index_orig_names[2L]] %in% unique(index_df[[2L]])
               result <- x_consec_bal[times_present_orig , ]},

           "shared.times" = {
               keep <- intersect_index(index_df, "time")
               result <- x[keep, ]},

           "shared.individuals" = {
               keep <- intersect_index(index_df, "individual")
               result <- x[keep, ]
           })
    return(result)
} ## END make.pbalanced.data.frame



# helper function: returns logical vector which rows/entries to keep
#                  when balance.type = "shared.times" or "shared.individuals"
#                  (intersect of all time periods or individuals)
intersect_index <- function(index, by) {
    # intersect() is defined on vectors (not factors)
    #  -> convert respective index to character before
    unclass(index) # unclass for speed
    switch(by,
           "time" = {
               id <- index[[1L]]
               time <- as.character(index[[2L]])
           },
           "individual" = {
               id <- index[[2L]]
               time <- as.character(index[[1L]])
           })

    times_by_ids <- split(time, id)
    common_times <- Reduce(intersect, times_by_ids)
    keep_entries <- time %in% common_times
    return(keep_entries)
}

# plm-package.R#
#' Functions exported from other packages
#'
#' These functions are imported from other packages and re-exported by
#' \pkg{plm} to enable smooth use within \pkg{plm}.  Please follow the
#' links to view the function's original documentation.
#' @name re-export_functions
#' @keywords internal
NULL

#' @rdname re-export_functions
#' @name maxLik
#' @importFrom maxLik maxLik
#' @export
NULL


#' plm package: linear models for panel data
#'
#' plm is a package for R which intends to make the estimation of linear panel
#' models straightforward. plm provides functions to estimate a wide variety of
#' models and to make (robust) inference.
#'
#' For a gentle and comprehensive introduction to the package, please see the
#' package's vignette.
#'
#' The main functions to estimate models are:
#'
#' - `plm`: panel data estimators using `lm` on transformed data,
#' - `pvcm`: variable coefficients models
#' - `pgmm`: generalized method of moments (GMM) estimation for panel
#' data,
#' - `pggls`: estimation of general feasible generalized least squares models,
#' - `pmg`: mean groups (MG), demeaned MG and common correlated effects
#' (CCEMG) estimators,
#' - `pcce`: estimators for common correlated effects mean groups (CCEMG) and
#' pooled (CCEP) for panel data with common factors,
#' - `pldv`: panel estimators for limited dependent variables.
#'
#' Next to the model estimation functions, the package offers several
#' functions for statistical tests related to panel data/models.
#'
#' Multiple functions for (robust) variance--covariance matrices are
#' at hand as well.
#'
#' The package also provides data sets to demonstrate functions and to
#' replicate some text book/paper results.  Use
#' `data(package="plm")` to view a list of available data sets in
#' the package.
#'
#' @name plm-package
#' @docType package
#' @keywords package
#' @examples
#'
#' data("Produc", package = "plm")
#' zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'           data = Produc, index = c("state","year"))
#' summary(zz)
#'
#' # replicates some results from Baltagi (2013), table 3.1
#' data("Grunfeld", package = "plm")
#' p <- plm(inv ~ value + capital,
#'          data = Grunfeld, model="pooling")
#'
#' wi <- plm(inv ~ value + capital,
#'           data = Grunfeld, model="within", effect = "twoways")
#'
#' swar <- plm(inv ~ value + capital,
#'             data = Grunfeld, model="random", effect = "twoways")
#'
#' amemiya <- plm(inv ~ value + capital,
#'                data = Grunfeld, model = "random", random.method = "amemiya",
#'                effect = "twoways")
#'
#' walhus <- plm(inv ~ value + capital,
#'               data = Grunfeld, model = "random", random.method = "walhus",
#'               effect = "twoways")
#'
NULL


#' Cigarette Consumption
#'
#' a panel of 46 observations from 1963 to 1992
#'
#' *total number of observations* : 1380
#'
#' *observation* : regional
#'
#' *country* : United States
#'
#'
#' @name Cigar
#' @docType data
#' @format
#'
#' A data frame containing :
#' \describe{
#' \item{state}{state abbreviation}
#' \item{year}{the year}
#' \item{price}{price per pack of cigarettes}
#' \item{pop}{population}
#' \item{pop16}{population above the age of 16}
#' \item{cpi}{consumer price index (1983=100)}
#' \item{ndi}{per capita disposable income}
#' \item{sales}{cigarette sales in packs per capita}
#' \item{pimin}{minimum price in adjoining states per pack of cigarettes}
#' }
#'
#' @references
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:LEVI:92}{plm}
#'
#' \insertRef{BALT:GRIF:XION:00}{plm}
#'
#' @source
#'
#' Online complements to Baltagi (2001):
#'
#' \url{https://www.wiley.com/legacy/wileychi/baltagi/}
#'
#' Online complements to Baltagi (2013):
#'
#' \url{https://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @importFrom Rdpack reprompt
#' @keywords datasets
NULL

#' Crime in North Carolina
#'
#' a panel of 90 observational units (counties) from 1981 to 1987
#'
#' *total number of observations* : 630
#'
#' *observation* : regional
#'
#' *country* : United States
#'
#' The variables l* (lcrmrte, lprbarr, ...) contain the pre-computed logarithms
#' of the base variables as found in the original data set. Note that these
#' values slightly differ from what R's log() function yields for the base
#' variables.  In order to reproduce examples from the literature, the
#' pre-computed logs need to be used, otherwise the results differ slightly.
#'
#' @name Crime
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{county}{county identifier}
#' \item{year}{year from 1981 to 1987}
#' \item{crmrte}{crimes committed per person}
#' \item{prbarr}{'probability' of arrest}
#' \item{prbconv}{'probability' of conviction}
#' \item{prbpris}{'probability' of prison sentence}
#' \item{avgsen}{average sentence, days}
#' \item{polpc}{police per capita}
#' \item{density}{people per square mile}
#' \item{taxpc}{tax revenue per capita}
#' \item{region}{factor. One of 'other', 'west' or 'central'.}
#' \item{smsa}{factor. (Also called "urban".) Does the individual reside in a SMSA (standard metropolitan statistical area)?}
#' \item{pctmin}{percentage minority in 1980}
#' \item{wcon}{weekly wage in construction}
#' \item{wtuc}{weekly wage in transportation, utilities, communications}
#' \item{wtrd}{weekly wage in wholesale and retail trade}
#' \item{wfir}{weekly wage in finance, insurance and real estate}
#' \item{wser}{weekly wage in service industry}
#' \item{wmfg}{weekly wage in manufacturing}
#' \item{wfed}{weekly wage in federal government}
#' \item{wsta}{weekly wage in state government}
#' \item{wloc}{weekly wage in local government}
#' \item{mix}{offence mix: face-to-face/other}
#' \item{pctymle}{percentage of young males (between ages 15 to 24)}
#' \item{lcrmrte}{log of crimes committed per person}
#' \item{lprbarr}{log of 'probability' of arrest}
#' \item{lprbconv}{log of 'probability' of conviction}
#' \item{lprbpris}{log of 'probability' of prison sentence}
#' \item{lavgsen}{log of average sentence, days}
#' \item{lpolpc}{log of police per capita}
#' \item{ldensity}{log of people per square mile}
#' \item{ltaxpc}{log of tax revenue per capita}
#' \item{lpctmin}{log of percentage minority in 1980}
#' \item{lwcon}{log of weekly wage in construction}
#' \item{lwtuc}{log of weekly wage in transportation, utilities, communications}
#' \item{lwtrd}{log of weekly wage in wholesale and retail trade}
#' \item{lwfir}{log of weekly wage in finance, insurance and real estate}
#' \item{lwser}{log of weekly wage in service industry}
#' \item{lwmfg}{log of weekly wage in manufacturing}
#' \item{lwfed}{log of weekly wage in federal government}
#' \item{lwsta}{log of weekly wage in state government}
#' \item{lwloc}{log of weekly wage in local government}
#' \item{lmix}{log of offence mix: face-to-face/other}
#' \item{lpctymle}{log of percentage of young males (between ages 15 to 24)}}
#'
#' @references
#'
#' \insertRef{CORN:TRUM:94}{plm}
#'
#' \insertRef{BALT:06}{plm}
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' @source
#'
#' Journal of Applied Econometrics Data Archive (complements Baltagi
#' (2006)):
#'
#' \url{http://qed.econ.queensu.ca/jae/2006-v21.4/baltagi/}
#'
#' Online complements to Baltagi (2001):
#'
#' \url{https://www.wiley.com/legacy/wileychi/baltagi/}
#'
#' Online complements to Baltagi (2013):
#'
#' \url{https://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#'
#' See also Journal of Applied Econometrics data archive entry for
#' Baltagi (2006) at
#' \url{http://qed.econ.queensu.ca/jae/2006-v21.4/baltagi/}.
#'
#' @keywords datasets
NULL

#' Employment and Wages in the United Kingdom
#'
#' An unbalanced panel of 140 observations from 1976 to 1984
#'
#' *total number of observations* : 1031
#'
#' *observation* : firms
#'
#' *country* : United Kingdom
#'
#'
#' @name EmplUK
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{firm}{firm index}
#' \item{year}{year}
#' \item{sector}{the sector of activity}
#' \item{emp}{employment}
#' \item{wage}{wages}
#' \item{capital}{capital}
#' \item{output}{output}
#' }
#' @source
#' \insertRef{AREL:BOND:91}{plm}
#' @keywords datasets
NULL

#' Gasoline Consumption
#'
#' A panel of 18 observations from 1960 to 1978
#'
#' *total number of observations* : 342
#'
#' *observation* : country
#'
#' *country* : OECD
#'
#'
#' @name Gasoline
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{country}{a factor with 18 levels}
#' \item{year}{the year}
#' \item{lgaspcar}{logarithm of motor gasoline consumption per car}
#' \item{lincomep}{logarithm of real per-capita income}
#' \item{lrpmg}{logarithm of real motor gasoline price}
#' \item{lcarpcap}{logarithm of the stock of cars per capita}
#' }
#' @references
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:GRIF:83}{plm}
#'
#' @source
#'
#' Online complements to Baltagi (2001):
#'
#' \url{https://www.wiley.com/legacy/wileychi/baltagi/}
#'
#' Online complements to Baltagi (2013):
#'
#' \url{https://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @keywords datasets
NULL

#' Grunfeld's Investment Data
#'
#' A balanced panel of 10 observational units (firms) from 1935 to 1954
#'
#' *total number of observations* : 200
#'
#' *observation* : production units
#'
#' *country* : United States
#'
#'
#' @name Grunfeld
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{firm}{observation}
#' \item{year}{date}
#' \item{inv}{gross Investment}
#' \item{value}{value of the firm}
#' \item{capital}{stock of plant and equipment} }
#'
#' @note The Grunfeld data as provided in package `plm` is the
#'     same data as used in Baltagi (2001), see **Examples** below.
#'
#' NB:\cr Various versions of the Grunfeld data circulate
#' online. Also, various text books (and also varying among editions)
#' and papers use different subsets of the original Grunfeld data,
#' some of which contain errors in a few data points compared to the
#' original data used by Grunfeld (1958) in his PhD thesis. See
#' Kleiber/Zeileis (2010) and its accompanying website for a
#' comparison of various Grunfeld data sets in use.
#'
#' @seealso For the complete Grunfeld data (11 firms), see
#' [AER::Grunfeld], in the `AER` package.
#'
#' @references
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{GRUN:58}{plm}
#'
#' \insertRef{KLEI:ZEIL:10}{plm}
#'
#'  website accompanying the paper with various variants of the
#' Grunfeld data:
#' \url{https://www.zeileis.org/grunfeld/}.
## \url{https://eeecon.uibk.ac.at/~zeileis/grunfeld/}.
## \url{http://statmath.wu-wien.ac.at/~zeileis/grunfeld/}.
#'
#' @source Online complements to Baltagi (2001):
#'
#' \url{https://www.wiley.com/legacy/wileychi/baltagi/}
#'
#' \url{https://www.wiley.com/legacy/wileychi/baltagi/supp/Grunfeld.fil}
#'
#' Online complements to Baltagi (2013):
#'
#' \url{https://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @keywords datasets
#' @examples
#'
#' \dontrun{
#' # Compare plm's Grunfeld data to Baltagi's (2001) Grunfeld data:
#'   data("Grunfeld", package="plm")
#'   Grunfeld_baltagi2001 <- read.csv("http://www.wiley.com/legacy/wileychi/
#'     baltagi/supp/Grunfeld.fil", sep="", header = FALSE)
#'   library(compare)
#'   compare::compare(Grunfeld, Grunfeld_baltagi2001, allowAll = T) # same data set
#'   }
#'
NULL


#' Hedonic Prices of Census Tracts in the Boston Area
#'
#' A cross-section
#'
#' *number of observations* : 506
#'
#' *observation* : regional
#'
#' *country* : United States
#'
#'
#' @name Hedonic
#' @docType data
#' @format A dataframe containing:
#' \describe{
#' \item{mv}{median value of owner--occupied homes}
#' \item{crim}{crime rate}
#' \item{zn}{proportion of 25,000 square feet residential lots}
#' \item{indus}{proportion of no--retail business acres}
#' \item{chas}{is the tract bounds the Charles River?}
#' \item{nox}{annual average nitrogen oxide concentration in parts per hundred million}
#' \item{rm}{average number of rooms}
#' \item{age}{proportion of owner units built prior to 1940}
#' \item{dis}{weighted distances to five employment centers in the Boston area}
#' \item{rad}{index of accessibility to radial highways}
#' \item{tax}{full value property tax rate ($/$10,000)}
#' \item{ptratio}{pupil/teacher ratio}
#' \item{blacks}{proportion of blacks in the population}
#' \item{lstat}{proportion of population that is lower status}
#' \item{townid}{town identifier} }
#'
#' @references
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BESL:KUH:WELS:80}{plm}
#'
#' \insertRef{HARR:RUBI:78}{plm}

#' @source Online complements to Baltagi (2001):
#'
#' \url{https://www.wiley.com/legacy/wileychi/baltagi/}
#'
#' Online complements to Baltagi (2013):
#'
#' \url{https://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @keywords datasets
NULL

#' Wages and Hours Worked
#'
#' A panel of 532 observations from 1979 to 1988
#'
#' *number of observations* : 5320
#'
#'
#' @name LaborSupply
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{lnhr}{log of annual hours worked}
#' \item{lnwg}{log of hourly wage}
#' \item{kids}{number of children}
#' \item{age}{age}
#' \item{disab}{bad health}
#' \item{id}{id}
#' \item{year}{year}
#' }
#'
#' @references
#'
#' \insertRef{CAME:TRIV:05}{plm}
#'
#' \insertRef{ZILI:97}{plm}
#'
#' @source Online complements to Ziliak (1997).
#'
#' Journal of Business Economics and Statistics web site:
#' \url{https://amstat.tandfonline.com/loi/ubes20/}.
#'
#' @keywords datasets
NULL



#' Wages and Education of Young Males
#'
#' A panel of 545 observations from 1980 to 1987
#'
#' *total number of observations* : 4360
#'
#' *observation* : individuals
#'
#' *country* : United States
#'
#'
#' @name Males
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{nr}{identifier}
#' \item{year}{year}
#' \item{school}{years of schooling}
#' \item{exper}{years of experience (computed as `age-6-school`)}
#' \item{union}{wage set by collective bargaining?}
#' \item{ethn}{a factor with levels `black, hisp, other`}
#' \item{married}{married?}
#' \item{health}{health problem?}
#' \item{wage}{log of hourly wage}
#' \item{industry}{a factor with 12 levels}
#' \item{occupation}{a factor with 9 levels}
#' \item{residence}{a factor with levels `rural_area, north_east, northern_central, south`}
#' }
#'
#' @references
#'
#' \insertRef{VELL:VERB:98}{plm}
#'
#' \insertRef{VERB:04}{plm}
#'
#' @source Journal of Applied Econometrics data archive
#' \url{http://qed.econ.queensu.ca/jae/1998-v13.2/vella-verbeek/}.
#'
#' @keywords datasets
NULL


#' Purchasing Power Parity and other parity relationships
#'
#' A panel of 104 quarterly observations from 1973Q1 to 1998Q4
#'
#' *total number of observations* : 1768
#'
#' *observation* : country
#'
#' *country* : OECD
#'
#'
#' @name Parity
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{country}{country codes: a factor with 17 levels}
#' \item{time}{the quarter index, 1973Q1-1998Q4}
#' \item{ls}{log spot exchange rate vs. USD}
#' \item{lp}{log price level}
#' \item{is}{short term interest rate}
#' \item{il}{long term interest rate}
#' \item{ld}{log price differential vs. USA}
#' \item{uis}{U.S. short term interest rate}
#' \item{uil}{U.S. long term interest rate} }
#'
#' @references
#'
#' \insertRef{COAK:FUER:SMIT:06}{plm}
#'
#' \insertRef{DRIS:KRAA:98}{plm}
#'
#' @source
#'
#' \insertRef{COAK:FUER:SMIT:06}{plm}

#' @keywords datasets
NULL


#' US States Production
#'
#' A panel of 48 observations from 1970 to 1986
#'
#' *total number of observations* : 816
#'
#' *observation* : regional
#'
#' *country* : United States
#'
#'
#' @name Produc
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{state}{the state}
#' \item{year}{the year}
#' \item{region}{the region}
#' \item{pcap}{public capital stock}
#' \item{hwy}{highway and streets}
#' \item{water}{water and sewer facilities}
#' \item{util}{other public buildings and structures}
#' \item{pc}{private capital stock}
#' \item{gsp}{gross state product}
#' \item{emp}{labor input measured by the employment in non--agricultural payrolls}
#' \item{unemp}{state unemployment rate} }
#'
#' @references
#'
#' \insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:PINN:95}{plm}
#'
#' \insertRef{MUNN:90}{plm}
#'
#' @source Online complements to Baltagi (2001):
#'
#' \url{https://www.wiley.com/legacy/wileychi/baltagi/}
#'
#' Online complements to Baltagi (2013):
#'
#' \url{https://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @keywords datasets
NULL



#' Production of Rice in Indonesia
#'
#' a panel of 171 observations
#'
#' *number of observations* : 1026
#'
#' *observation* : farms
#'
#' *country* : Indonesia
#'
#'
#' @name RiceFarms
#' @docType data
#' @format A dataframe containing :
#' \describe{
#' \item{id}{the farm identifier}
#'
#' \item{size}{the total area cultivated with rice, measured in hectares}
#'
#' \item{status}{land status, on of `'owner'` (non sharecroppers,
#' owner operators or leaseholders or both), `'share'`
#' (sharecroppers), `'mixed'` (mixed of the two previous status)}
#'
#' \item{varieties}{one of `'trad'` (traditional varieties),
#' `'high'` (high yielding varieties) and `'mixed'` (mixed
#' varieties)}
#'
#' \item{bimas}{bIMAS is an intensification program; one of
#' `'no'` (non-bimas farmer), `'yes'` (bimas farmer) or
#' `'mixed'` (part but not all of farmer's land was registered to
#' be in the bimas program)}
#'
#' \item{seed}{seed in kilogram}
#'
#' \item{urea}{urea in kilogram}
#'
#' \item{phosphate}{phosphate in kilogram}
#'
#' \item{pesticide}{pesticide cost in Rupiah}
#'
#' \item{pseed}{price of seed in Rupiah per kg}
#'
#' \item{purea}{price of urea in Rupiah per kg}
#'
#' \item{pphosph}{price of phosphate in Rupiah per kg}
#'
#' \item{hiredlabor}{hired labor in hours}
#'
#' \item{famlabor}{family labor in hours}
#'
#' \item{totlabor}{total labor (excluding harvest labor)}
#'
#' \item{wage}{labor wage in Rupiah per hour}
#'
#' \item{goutput}{gross output of rice in kg}
#'
#' \item{noutput}{net output, gross output minus harvesting cost (paid
#' in terms of rice)}
#'
#' \item{price}{price of rough rice in Rupiah per kg}
#'
#' \item{region}{one of `'wargabinangun'`, `'langan'`,
#' `'gunungwangi'`, `'malausma'`, `'sukaambit'`,
#' `'ciwangi'`}
#'
#' }
#'
#' @source
#'
#' \insertRef{FENG:HORR:12}{plm}

#' @keywords datasets
NULL

#' Employment and Wages in Spain
#'
#' A panel of 738 observations from 1983 to 1990
#'
#' *total number of observations*: 5904
#'
#' *observation*: firms
#'
#' *country*: Spain
#'
#'
#' @name Snmesp
#' @docType data
#' @format A data frame containing:
#'
#' \describe{
#' \item{firm}{firm index}
#' \item{year}{year}
#' \item{n}{log of employment}
#' \item{w}{log of wages}
#' \item{y}{log of real output}
#' \item{i}{log of intermediate inputs}
#' \item{k}{log of real capital stock}
#' \item{f}{real cash flow} }
#'
#' @references
#'
#' \insertRef{ALON:AREL:99}{plm}

#' @source Journal of Business Economics and Statistics data archive:
#'
#' \url{https://amstat.tandfonline.com/loi/ubes20/}.
#'
#' @keywords datasets
NULL

#' The Penn World Table, v. 5
#'
#' A panel of 125 observations from 1960 to 1985
#'
#' *total number of observations* : 3250
#'
#' *observation* : country
#'
#' *country* : World
#'
#'
#' @name SumHes
#' @docType data
#' @format A data frame containing :
#' \describe{
#' \item{year}{the year}
#' \item{country}{the country name (factor)}
#' \item{opec}{OPEC member?}
#' \item{com}{communist regime? }
#' \item{pop}{country's population (in thousands)}
#' \item{gdp}{real GDP per capita (in 1985 US dollars)}
#' \item{sr}{saving rate (in percent)}}
#'
#' @references
#'
#' \insertRef{HAYA:00}{plm}
#'
#' \insertRef{SUMM:HEST:91}{plm}
#'
#' @source Online supplements to Hayashi (2000).
#'
#' \url{http://fhayashi.fc2web.com/datasets.htm}
#'
#' @keywords datasets
NULL

#' Panel Data of Individual Wages
#'
#' A panel of 595 individuals from 1976 to 1982, taken from the Panel Study of
#' Income Dynamics (PSID).\cr\cr The data are organized as a stacked time
#' series/balanced panel, see **Examples** on how to convert to a
#' `pdata.frame`.
#'
#' *total number of observations* : 4165
#'
#' *observation* : individuals
#'
#' *country* : United States
#'
#'
#' @name Wages
#' @docType data
#' @format A data frame containing:
#' \describe{
#' \item{exp}{years of full-time work experience.}
#' \item{wks}{weeks  worked.}
#' \item{bluecol}{blue collar?}
#' \item{ind}{works in a manufacturing industry?}
#' \item{south}{resides in the south?}
#' \item{smsa}{resides in a standard metropolitan statistical area?}
#' \item{married}{married?}
#' \item{sex}{a factor with levels `"male"` and `"female"`}
#' \item{union}{individual's wage set by a union contract?}
#' \item{ed}{years of education.}
#' \item{black}{is the individual black?}
#' \item{lwage}{logarithm of wage.} }
#'
#' @references
#'
#'\insertRef{BALT:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{CORN:RUPE:88}{plm}
#'
#' @source Online complements to Baltagi (2001):
#'
#' \url{https://www.wiley.com/legacy/wileychi/baltagi/}
#'
#' Online complements to Baltagi (2013):
#'
#' \url{https://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @keywords datasets
#' @examples
#'
#' # data set 'Wages' is organized as a stacked time series/balanced panel
#' data("Wages", package = "plm")
#' Wag <- pdata.frame(Wages, index=595)
#'
NULL

# test_cd.R#
############## Pesaran's CD test and Breusch/Pagan LM Test (also scaled) ###############

## Pesaran's CD test for cross-sectional dependence in panel data models
## (and Breusch and Pagan's LM and scaled LM)
## ref. Pesaran, General diagnostic tests..., CESifo WP 1229, 2004

## In case K+1>T the group-specific model is not estimable;
## as in Greene 11.7.2, formula (11.23) we use the group-specific residuals
## of a consistent estimator. This may be pooled OLS, RE, FE. Here the
## default is set to FE.

## Note that the test can be performed on the results of plm objects with
## any kind of effects: having "time" effects means checking for
## xs-dependence *after* introducing time dummies.

## In principle, the test can be performed on the results of *any*
## panelmodel object. Some issues remain regarding standardization of
## model output: some missing pieces are, e.g., the 'model$indexes'
## in ggls. ''fd'' models are also not compatible because of indexes
## keeping the original timespan, while data lose the first period.

## production version, generic and based on plm

## version 11: added test = "bcsclm"
##
## version 10:
## substantial optimization for speed, now fast (few seconds) on N=3000
## all methods pass on a pseries to pcdres()

## make toy example
#dati <- data.frame(ind=rep(1:7, 4), time=rep(1:4, each=7), x=rnorm(28),
#                   group=rep(c(1,1,2,2,2,3,3), 4))
#pdati <- pdata.frame(dati)

#' Tests of cross-section dependence for panel models
#'
#' Pesaran's CD or Breusch--Pagan's LM (local or global) tests for cross
#' sectional dependence in panel models
#'
#' These tests are originally meant to use the residuals of separate
#' estimation of one time--series regression for each cross-sectional
#' unit in order to check for cross--sectional dependence (`model = NULL`).
#' If a different model specification (`model = "within"`, `"random"`,
#' \ldots{}) is assumed consistent, one can resort to its residuals for
#' testing (which is common, e.g., when the time dimension's length is
#' insufficient for estimating the heterogeneous model).
#'
#' If the time
#' dimension is insufficient and `model = NULL`, the function defaults
#' to estimation of a `within` model and issues a warning. The main
#' argument of this function may be either a model of class
#' `panelmodel` or a `formula` and `data frame`; in the second case,
#' unless `model` is set to `NULL`, all usual parameters relative to
#' the estimation of a `plm` model may be passed on. The test is
#' compatible with any consistent `panelmodel` for the data at hand,
#' with any specification of `effect` (except for `test = "bcsclm"` which
#' requires a within model with either individual or two-ways effect).
#' E.g., specifying  `effect = "time"` or `effect = "twoways"` allows
#' to test for residual cross-sectional dependence after the introduction
#' of time fixed effects to account for common shocks.
#'
#' A **local** version of either test can be computed by supplying a
#' proximity matrix (elements coercible to `logical`) with argument
#' `w` which provides information on whether any pair of individuals
#' are neighbours or not. If `w` is supplied, only neighbouring pairs
#' will be used in computing the test; else, `w` will default to
#' `NULL` and all observations will be used. The matrix need not be
#' binary, so commonly used "row--standardized" matrices can be
#' employed as well. `nb` objects from \CRANpkg{spdep} must instead be
#' transformed into matrices by \CRANpkg{spdep}'s function `nb2mat`
#' before using.
#'
#' The methods implemented are suitable also for unbalanced panels.
#'
#' Pesaran's CD test (`test="cd"`), Breusch and Pagan's LM test
#' (`test="lm"`), and its scaled version (`test="sclm"`) are all
#' described in \insertCite{PESA:04;textual}{plm} (and complemented by
#' Pesaran (2005)). The bias-corrected scaled test (`test="bcsclm"`)
#' is due to \insertCite{BALT:FENG:KAO:12}{plm} and only valid for
#' within models including the individual effect (it's unbalanced
#' version uses max(Tij) for T) in the bias-correction term).
#' \insertCite{BREU:PAGA:80;textual}{plm} is the original source for
#' the LM test.
#'
#' The test on a `pseries` is the same as a test on a pooled
#' regression model of that variable on a constant, i.e.,
#' `pcdtest(some_pseries)` is equivalent to `pcdtest(plm(some_var ~ 1,
#' data = some_pdata.frame, model = "pooling")` and also equivalent to
#' `pcdtest(some_var ~ 1, data = some_data)`, where `some_var` is
#' the variable name in the data which corresponds to `some_pseries`.
#'
#' @aliases pcdtest
#' @param x an object of class `formula`, `panelmodel`, or `pseries`
#'     (depending on the respective interface) describing the model to
#'     be tested,
#' @param data a `data.frame`,
#' @param index an optional numerical index, if `NULL`, the first two
#'     columns of the data.frame provided in argument `data` are
#'     assumed to be the index variables; for further details see
#'     [pdata.frame()],
#' @param model an optional character string indicating which type of
#'     model to estimate; if left to `NULL`, the original
#'     heterogeneous specification of Pesaran is used,
#' @param test the type of test statistic to be returned. One of
#'     \itemize{ \item `"cd"` for Pesaran's CD statistic, \item `"lm"`
#'     for Breusch and Pagan's original LM statistic, \item `"sclm"`
#'     for the scaled version of Breusch and Pagan's LM statistic,
#'     \item `"bcsclm"` for the bias-corrected scaled version of
#'     Breusch and Pagan's LM statistic, \item `"rho"` for the average
#'     correlation coefficient, \item `"absrho"` for the average
#'     absolute correlation coefficient,}
#' @param w either `NULL` (default) for the global tests or -- for the
#'     local versions of the statistics -- a `n x n` `matrix`
#'     describing proximity between individuals, with \eqn{w_ij = a}
#'     where \eqn{a} is any number such that `as.logical(a)==TRUE`, if
#'     \eqn{i,j} are neighbours, \eqn{0} or any number \eqn{b} such
#'     that `as.logical(b)==FALSE` elsewhere. Only the lower
#'     triangular part (without diagonal) of `w` after coercing by
#'     `as.logical()` is evaluated for neighbouring information (but
#'     `w` can be symmetric). See also **Details** and
#'     **Examples**,
#' @param \dots further arguments to be passed on for model estimation to `plm`,
#'    such as `effect` or `random.method`.
#' @return An object of class `"htest"`.
#' @export
#' @references
#'
#' \insertRef{BALT:FENG:KAO:12}{plm}
#'
#' \insertRef{BREU:PAGA:80}{plm}
#'
#' \insertRef{PESA:04}{plm}
#'
#' \insertRef{PESA:15}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' ## test on heterogeneous model (separate time series regressions)
#' pcdtest(inv ~ value + capital, data = Grunfeld,
#'         index = c("firm", "year"))
#'
#' ## test on two-way fixed effects homogeneous model
#' pcdtest(inv ~ value + capital, data = Grunfeld, model = "within",
#'         effect = "twoways", index = c("firm", "year"))
#'
#' ## test on panelmodel object
#' g <- plm(inv ~ value + capital, data = Grunfeld, index = c("firm", "year"))
#' pcdtest(g)
#'
#' ## scaled LM test
#' pcdtest(g, test = "sclm")
#'
#' ## test on pseries
#' pGrunfeld <- pdata.frame(Grunfeld)
#' pcdtest(pGrunfeld$value)
#'
#' ## local test
#' ## define neighbours for individual 2: 1, 3, 4, 5 in lower triangular matrix
#' w <- matrix(0, ncol= 10, nrow=10)
#' w[2,1] <- w[3,2] <- w[4,2] <- w[5,2] <- 1
#' pcdtest(g, w = w)
#'
pcdtest <- function(x, ...)
{
    UseMethod("pcdtest")
}

## this formula method here only for adding "rho" and "absrho"
## arguments

#' @rdname pcdtest
#' @export
pcdtest.formula <- function(x, data, index = NULL, model = NULL,
                            test = c("cd", "sclm", "bcsclm", "lm", "rho", "absrho"),
                            w = NULL, ...) {
    #data <- pdata.frame(data, index = index)
    test <- match.arg(test)
    if(test == "bcsclm" && (is.null(model) || model != "within"))
        stop("for test = 'bcsclm', set argument model = 'within'")

    # evaluate formula in parent frame
    cl <- match.call(expand.dots = TRUE)
    cl$model  <- if(test != "bcsclm") "pooling" else "within"
    if(test == "bcsclm") {
        # check args model and effect for test = "bcsclm"
        if(is.null(cl$effect)) cl$effect <- "individual" # make default within model is individual within
        eff <- isTRUE(cl$effect == "individual" || cl$effect == "twoways")
        if(model != "within" || !eff) stop("for test = 'bcsclm', requirement is model = \"within\" and effect = \"individual\" or \"twoways\"")
    }
    names(cl)[2L] <- "formula"
    m <- match(plm.arg, names(cl), 0L)
    cl <- cl[c(1L, m)]
    cl[[1L]] <- as.name("plm")
    mymod <- eval(cl, parent.frame()) # mymod is either "pooling" or "within" (the latter iff for test = "bcsclm")

    hetero.spec <- if(is.null(model)) TRUE else FALSE

    if(hetero.spec && min(pdim(mymod)$Tint$Ti) < length(mymod$coefficients)+1) {
        warning("Insufficient number of observations in time to estimate heterogeneous model: using within residuals",
                call. = FALSE)
        hetero.spec <- FALSE
        model <- "within"
    }

    ind0 <- attr(model.frame(mymod), "index")
    tind <- as.numeric(ind0[[2L]])
    ind <- as.numeric(ind0[[1L]])

    if(hetero.spec) {
        ## estimate individual normal regressions one by one
        ## (original heterogeneous specification of Pesaran)
        X <- model.matrix(mymod)
        y <- model.response(model.frame(mymod))
        unind <- unique(ind)
        n <- length(unind)
        ti.res   <- vector("list", n)
        ind.res  <- vector("list", n)
        tind.res <- vector("list", n)
        for (i in 1:n) {
            tX <- X[ind == unind[i], , drop = FALSE]
            ty <- y[ind == unind[i]]
            res.i <- lm.fit(tX, ty)$residuals
            ti.res[[i]] <- res.i
            names(ti.res[[i]]) <- tind[ind == unind[i]]
            ind.res[[i]] <- rep(i, length(res.i))
            tind.res[[i]] <- tind[ind == unind[i]]
        }
        ## make pseries of (all) residuals
        resdata <- data.frame(ee   = unlist(ti.res,   use.names = FALSE),
                              ind  = unlist(ind.res,  use.names = FALSE),
                              tind = unlist(tind.res, use.names = FALSE))
        pee <- pdata.frame(resdata, index = c("ind", "tind"))
        tres <- pee$ee
    } else {
        # else case is one of:
        # a) insufficient number of observations for heterogen. spec. or
        # b) model specified when function was called (incl. case test = "bcsclm")
        if(test != "bcsclm") {
            # Estimate the model specified originally in function call or due to
            # forced model switch to within model by insufficient number of
            # observations for heterogen. spec.
            # (for test = "bcsclm" it is ensured that a within model was already
            # estimated -> no need to estimate again a within model)
            cl$model <- model
            mymod <- eval(cl, parent.frame())
        }

        tres <- resid(mymod)
        unind <- unique(ind)
        n <- length(unind)
        t <- min(pdim(mymod)$Tint$Ti)
        nT <- length(ind)
        k <- length(mymod$coefficients)
    }

    return(pcdres(tres = tres, n = n, w = w,
                  form = paste(deparse(x)),
                  test = test))
}


## panelmodel method: just fetch resid (as a pseries) and hand over to pcdres

#' @rdname pcdtest
#' @export
pcdtest.panelmodel <- function(x, test = c("cd", "sclm", "bcsclm", "lm", "rho", "absrho"),
                               w = NULL, ...) {

    test <- match.arg(test)
    model <- describe(x, "model")
    effect <- describe(x, "effect")
    eff <- (effect == "individual" || effect == "twoways")
    if (test == "bcsclm")
        if (model != "within" || !eff) stop("for test = 'bcsclm', model x must be a within individual or twoways model")

    tres <- resid(x)
    index <- attr(model.frame(x), "index")
    #tind <- as.numeric(index[[2L]])
    ind <- as.numeric(index[[1L]])
    unind <- unique(ind)
    n <- length(unind)
    #t <- pdim(x)$Tint$Ti
    #nT <- length(ind)
    #k <- length(x$coefficients)
    return(pcdres(tres = tres, n = n, w = w,
                  form = paste(deparse(x$formula)),
                  test = test))
}

#' @rdname pcdtest
#' @export
pcdtest.pseries <- function(x, test = c("cd", "sclm", "bcsclm", "lm", "rho", "absrho"),
                            w = NULL, ...) {

    ## calculates local or global CD test on a pseries 'x' just as it
    ## would on model residuals
    ## important difference here: a pseries _can_ have NAs

    # input check
    if (!inherits(x, "pseries")) stop("input 'x' needs to be of class \"pseries\"")
    form <- paste(deparse(substitute(x)))

    pos.na <- is.na(x)
    if (any(pos.na)) {
        x <- subset_pseries(x, !pos.na) # TODO: use [.pseries (pseries subsetting) once implemented
        warning("NA values encountered in input and removed")
        if (length(x) == 0L) stop("input is empty after removal of NA values")
    }

    ## get indices
    tind <- as.numeric(attr(x, "index")[[2L]])
    ind <- as.numeric(attr(x, "index")[[1L]])

    ## det. number of groups and df
    unind <- unique(ind)
    n <- length(unind)

    tres <- x

    ## "pre-allocate" an empty list of length n
    #tres <- vector("list", n)

    ## use model residuals, group by group
    ## list of n:
    ## t_i residuals for each x-sect. 1..n
    #for(i in 1:n) {
    #          # remove NAs
    #          xnonna <- !is.na(x[ind==unind[i]])
    #          tres[[i]] <- x[ind==unind[i]][xnonna]
    #          ## name resids after the time index
    #          names(tres[[i]]) <- tind[ind==unind[i]][xnonna]
    #          }

    return(pcdres(tres = tres, n = n, w = w,
                  form = form,
                  test = match.arg(test)))
}

pcdres <- function(tres, n, w, form, test) {
    # 'form' is a character describing the formula (not a formula object!)
    # and goes into htest_object$data.name

    ## Take model residuals as pseries, and calc. test
    ## (from here on, what's needed for rho_ij is ok)

    ## this function is the modulus calculating the test,
    ## to be called from pcdtest.formula,
    ## pcdtest.panelmodel or pcdtest.pseries

    ## now (since v10) tres is the pseries of model residuals

    ## calc matrix of all possible pairwise corr.
    ## coeffs. (200x speedup from using cor())
    wideres <- t(preshape(tres, na.rm = FALSE))
    rho <- cor(wideres, use = "pairwise.complete.obs")

    ## find length of intersecting pairs
    ## fast method, times down 200x
    data.res <- data.frame(time = attr(tres, "index")[[2L]],
                           indiv = attr(tres, "index")[[1L]])
    ## tabulate which obs in time for each ind are !na
    presence.tab <- table(data.res)
    ## calculate t.ij
    t.ij <- crossprod(presence.tab)

    # input check
    if (!is.null(w)) {
        dims.w <- dim(w)
        if(dims.w[1L] != n || dims.w[2L] != n)
            stop(paste0("matrix 'w' describing proximity of individuals has wrong dimensions: ",
                        "should be ", n, " x ", n, " (no. of individuals) but is ", dims.w[1L], " x ", dims.w[2L]))
    }


    ## begin features for local test ####################
    ## higher orders omitted for now, use wlag() explicitly

    ## if global test, set all elements in w to 1
    if(is.null(w)) {
        w <- matrix(1, ncol = n, nrow = n)
        dep <- ""
    } else { dep <- "local" }

    ## make (binary) selector matrix based on the contiguity matrix w
    ## and extracting elements corresponding to ones in the lower triangle
    ## excluding the diagonal

    ## transform in logicals (0=FALSE, else=TRUE: no need to worry
    ## about row-std. matrices)
    selector.mat <- matrix(as.logical(w), ncol = n)

    ## some sanity checks for 'w' (not perfect sanity, but helps)
    if (sum(selector.mat[lower.tri(selector.mat, diag = FALSE)]) == 0) {
        stop(paste0("no neighbouring individuals defined in proximity matrix 'w'; ",
                    "only lower triangular part of 'w' (w/o diagonal) is evaluated"))
    } else {
        if (sum(selector.mat[upper.tri(selector.mat, diag = FALSE)]) != 0) {
            if (!isSymmetric((unname(selector.mat)))) { # unname needed to ignore rownames and colnames
                stop(paste0("proximity matrix 'w' is ambiguous: upper and lower triangular part ",
                            "define different neighbours (it is sufficient to provide information ",
                            "about neighbours only in the lower triangluar part of 'w'"))
            }
        }
    }

    ## if no intersection or only 1 shared period of e_it and e_jt
    ## => exclude from calculation and issue a warning.
    ## In general, length(m.ij) gives the number of shared periods by indiviudals i, j
    ## Thus, non intersecting pairs are indicated by length(m.ij) == 0 (t.ij[i,j] == 0)
    no.one.intersect <- (t.ij <= 1)
    if (any(no.one.intersect, na.rm = TRUE)) {
        # t.ij is a lower triangular matrix: do not divide by 2 to get the number of non-intersecting pairs!
        number.of.non.one.intersecting.pairs <- sum(no.one.intersect, na.rm = TRUE)
        number.of.total.pairs <- (n*(n-1))/2
        share.on.one.intersect.pairs <- number.of.non.one.intersecting.pairs / number.of.total.pairs * 100
        warning(paste("Some pairs of individuals (",
                      signif(share.on.one.intersect.pairs, digits = 2),
                      " percent) do not have any or just one time period in common and have been omitted from calculation", sep=""))
        selector.mat[no.one.intersect] <- FALSE
    }

    ## set upper tri and diagonal to FALSE
    selector.mat[upper.tri(selector.mat, diag = TRUE)] <- FALSE

    ## number of elements in selector.mat
    ## elem.num = 2*(N*(N-1)) in Pesaran (2004), formulae (6), (7), (31), ...
    elem.num <- sum(selector.mat)

    ## end features for local test ######################

    ## Breusch-Pagan or Pesaran statistic for cross-sectional dependence,
    ## robust vs. unbalanced panels:

    switch(test,
           lm = {
               CDstat        <- sum((t.ij*rho^2)[selector.mat])
               pCD           <- pchisq(CDstat, df = elem.num, lower.tail = FALSE)
               names(CDstat) <- "chisq"
               parm          <- elem.num
               names(parm)   <- "df"
               testname      <- "Breusch-Pagan LM test"
           },
           sclm = {
               CDstat        <- sqrt(1/(2*elem.num))*sum((t.ij*rho^2-1)[selector.mat])
               pCD           <- 2*pnorm(abs(CDstat), lower.tail = FALSE)
               names(CDstat) <- "z"
               parm          <- NULL
               testname      <- "Scaled LM test"
           },
           bcsclm = {
               # Baltagi/Feng/Kao (2012), formula (11)
               # (unbalanced case as sclm + in bias correction as EViews: max(T_ij) instead of T)
               CDstat        <- sqrt(1/(2*elem.num))*sum((t.ij*rho^2-1)[selector.mat]) - (n/(2*(max(t.ij)-1)))
               pCD           <- 2*pnorm(abs(CDstat), lower.tail = FALSE)
               names(CDstat) <- "z"
               parm          <- NULL
               testname      <- "Bias-corrected Scaled LM test"
           },
           cd = {
               # (Pesaran (2004), formula (31))
               CDstat        <- sqrt(1/elem.num)*sum((sqrt(t.ij)*rho)[selector.mat])
               pCD           <- 2*pnorm(abs(CDstat), lower.tail = FALSE)
               names(CDstat) <- "z"
               parm          <- NULL
               testname      <- "Pesaran CD test"
           },
           rho = {
               CDstat        <- sum(rho[selector.mat])/elem.num
               pCD           <- NULL
               names(CDstat) <- "rho"
               parm          <- NULL
               testname      <- "Average correlation coefficient"
           },
           absrho = {
               CDstat        <- sum(abs(rho)[selector.mat])/elem.num
               pCD           <- NULL
               names(CDstat) <- "|rho|"
               parm          <- NULL
               testname      <- "Average absolute correlation coefficient"
           })

    ##(insert usual htest features)
    RVAL <- list(statistic = CDstat,
                 parameter = parm,
                 method    = paste(testname, "for", dep,
                                   "cross-sectional dependence in panels"),
                 alternative = "cross-sectional dependence",
                 p.value     = pCD,
                 data.name   = form)
    class(RVAL) <- "htest"
    return(RVAL)
}

preshape <- function(x, na.rm = TRUE, ...) {
    ## reshapes pseries,
    ## e.g., of residuals from a panelmodel,
    ## in wide form
    inames <- names(attr(x, "index"))
    mres <- reshape(cbind(as.vector(x), attr(x, "index")),
                    direction = "wide",
                    timevar = inames[2L],
                    idvar = inames[1L])
    ## drop ind in first column
    mres <- mres[ , -1L, drop = FALSE]
    ## reorder columns (may be scrambled depending on first
    ## available obs in unbalanced panels)
    mres <- mres[ , order(dimnames(mres)[[2L]])]
    ## if requested, drop columns (time periods) with NAs
    if(na.rm) {
        na.cols <- vapply(mres, FUN = anyNA, FUN.VALUE = TRUE, USE.NAMES = FALSE)
        if(sum(na.cols) > 0L) mres <- mres[ , !na.cols]
    }
    return(mres)
}




#' Cross--sectional correlation matrix
#'
#' Computes the cross--sectional correlation matrix
#'
#'
#' @param x an object of class `pseries`
#' @param grouping grouping variable,
#' @param groupnames a character vector of group names,
#' @param value to complete,
#' @param \dots further arguments.
#' @return A matrix with average correlation coefficients within a group
#' (diagonal) and between groups (off-diagonal).
#' @export
#' @keywords htest
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' pGrunfeld <- pdata.frame(Grunfeld)
#' grp <- c(rep(1, 100), rep(2, 50), rep(3, 50)) # make 3 groups
#' cortab(pGrunfeld$value, grouping = grp, groupnames = c("A", "B", "C"))
#'
cortab <- function(x, grouping, groupnames = NULL,
                   value = "statistic", ...) {
    ## makes matrix containing within (diagonal) and between (off-diagonal)
    ## correlation
    ## needs a pseries and a groupings vector of **same length**

    ## would use a better naming, and also passing a char or factor as
    ## grouping index

    ## x must be a pseries
    if(!inherits(x, "pseries")) stop("First argument must be a pseries")
    if(length(x) != length(grouping)) stop("arguments 'x' and 'grouping' must have same length")

    fullind <- as.numeric(attr(x, "index")[ , 1L])
    ids <- unique(fullind)
    n <- length(ids)
    regs <- 1:length(unique(grouping))

    if(!(is.numeric(grouping))) grouping <- as.numeric(as.factor(grouping))

    idnames <- as.character(ids)
    if(is.null(groupnames)) {
        groupnames <- as.character(unique(grouping))
    }

    ## make matrices of between-regions correlations
    ## (includes within correlation on diagonal)
    ## for each pair of regions (nb: no duplicates, e.g., 3.1 but not 1.3)

    ## make w<1.n>:
    for(h in 1:length(regs)) {
        for(k in 1:h) {
            statew <- matrix(0, ncol = n, nrow = n)
            ## make statew for cor. between h and k
            for(i in 1:n) {
                ## get first region (all values equal, so take first one)
                ireg <- grouping[fullind == ids[i]][1L]
                if(ireg == h) {
                    for(j in 1:n) {
                        jreg <- grouping[fullind == ids[j]][1L]
                        if(jreg == k) statew[i, j] <- 1
                    }
                }
            }
            if(h!=k) statew <- statew + t(statew)
            ## just for debugging reasons:
            dimnames(statew) <- list(idnames, idnames)
            ## eliminate self.correlation of states if i=j
            diag(statew) <- 0
            ## not needed: pcdtest seems to do this by construction
            eval(parse(text=paste("w", h, ".", k, " <- statew", sep="")))
        }
    }

    ## notice: without the line
    ## '' if(i!=j) statew <- statew + t(statew) ''
    ## all wn.n matrices would have values only on one half (upper
    ## or lower triangle)

    ## make generic table of regions' within and between correlation
    ## argument: a pseries
    #YC regnames is undefined, so is myw
    tab.g <- function(x, regs, regnames, test="rho", value) {
        myw <- 0
        tabg <- matrix(NA, ncol=length(regs), nrow=length(regs))
        for(i in 1:length(regs)) {
            for(j in 1:i) {
                ## take appropriate w matrix
                eval(parse(text = paste("myw<-w", i, ".", j, sep = "")))
                tabg[i, j] <- pcdtest(x, test = "rho", w = myw)[[value]]
            }
        }
        dimnames(tabg) <- list(groupnames, groupnames)
        return(tabg)
    }
    regnames <- ""
    mytab <- tab.g(x, regs = regs, regnames = regnames, test = "rho", value = value)
    return(mytab)
}


# test_cips.R#
## taken from pmg to estimate CIPS test statistic as "average of t's"
## since version 4: added type warning, and output single CADF
## regressions as well, use func gettvalue for speed.  estimation loop
## for single TS models is now lm(formula, data) with 'data' properly
## subsetted; this allows for decent output of individual mods.

## needed for standalone operation:
#plm <- plm:::plm
#pdim <- plm:::pdim

#model.matrix.plm <- plm:::model.matrix.plm
#pmodel.response <- plm:::pmodel.response.plm

## Reference is
## Pesaran, M.H. (2007) A simple panel unit root test in the presence of
## cross-section dependence, Journal of Applied Econometrics, 22(2), pp. 265-312




#' Cross-sectionally Augmented IPS Test for Unit Roots in Panel Models
#'
#' Cross-sectionally augmented Im, Pesaran and Shin (IPS) test for
#' unit roots in panel models.
#'
#' Pesaran's \insertCite{pes07}{plm} cross-sectionally augmented version of
#' the IPS unit root test \insertCite{IM:PESAR:SHIN:03}{plm} (H0: `pseries`
#' has a unit root) is a so-called second-generation panel unit root test: it
#' is in fact robust against cross-sectional dependence, provided that the default
#' `model="cmg"` is calculated. Else one can obtain the standard
#' (`model="mg"`) or cross-sectionally demeaned (`model="dmg"`)
#' versions of the IPS test.
#'
#' Argument `type` controls how the test is executed:
#' - `"none"`: no intercept, no trend (Case I in \insertCite{pes07}{plm}),
#' - `"drift"`: with intercept, no trend (Case II),
#' - `"trend"` (default): with intercept, with trend (Case III).
#'
#' @param x an object of class `"pseries"`,
#' @param lags integer, lag order for Dickey-Fuller augmentation,
#' @param type one of `"trend"` (default), `"drift"`, `"none"`,
#' @param model one of `"cmg"` (default), `"mg"`, `"dmg"`,
#' @param truncated logical, specifying whether to calculate the
#'     truncated version of the test (default: `FALSE`),
#' @param \dots further arguments passed to `critvals.cips`
#' (non-exported function).
#' @return An object of class `"htest"`.
#' @author Giovanni Millo
#' @export
#' @seealso [purtest()], [phansitest()]
#' @references
#'
#' \insertAllCited{}
#'
#' @aliases cipstest
#' @keywords htest
#' @examples
#'
#' data("Produc", package = "plm")
#' Produc <- pdata.frame(Produc, index=c("state", "year"))
#' ## check whether the gross state product (gsp) is trend-stationary
#' cipstest(Produc$gsp, type = "trend")
#'
cipstest <- function (x, lags = 2, type = c("trend", "drift", "none"),
                      model = c("cmg", "mg", "dmg"), truncated = FALSE, ...) {

    ## type = c("trend", "drift", "none") corresponds to Case III, II, I
    ## in Pesaran (2007), respectively.

    ## input checks
    if(!inherits(x, "pseries")) stop("Argument 'x' has to be a pseries")
    if(!is.numeric(lags)) stop("Argument 'lags' has to be an integer") # but accept numeric as well
    if(round(lags) != lags) stop("Argument 'lags' has to be an integer")
    # TODO: does 'lags' always need to be >= 1? if so, check for this, too

    dati <- pmerge(diff(x), lag(x))
    dati <- pmerge(dati, diff(lag(x)))
    ## minimal column names
    indexnames <- c("ind", "tind")
    dimnames(dati)[[2L]][1:2] <- indexnames
    clnames <- c("de", "le", "d1e")
    dimnames(dati)[[2L]][3:5] <- clnames
    ## add lags if lags > 1
    if(lags > 1L) {
        for(i in 2:lags) {
            dati <- pmerge(dati, diff(lag(x, i)))
            clnames <- c(clnames, paste("d", i, "e", sep = ""))
        }
    }

    dimnames(dati)[[2]][3:(lags+4)] <- clnames

    deterministic <- switch(match.arg(type),
                            "trend" = {"+as.numeric(tind)"},
                            "drift" = {""},
                            "none"  = {"-1"})

    ## make formula
    adffm <- as.formula(paste("de~le+",
                              paste(clnames[3:(lags+2)], collapse = "+"),
                              deterministic, sep = ""))

    ## estimate preliminary pooling plm, to take care of all diffs
    ## and lags in a 'panel' way (would be lost in single TS regr.s)
    pmod <- plm(adffm, data = dati, model = "pooling")
    ## this as in pmg()
    index <- attr(model.frame(pmod), "index")
    ind  <- index[[1L]] ## individual index
    tind <- index[[2L]] ## time index
    ## set dimension variables
    pdim <- pdim(pmod)
    balanced <- pdim$balanced
    nt <- pdim$Tint$nt
    Ti <- pdim$Tint$Ti
    T. <- pdim$nT$T
    n <- pdim$nT$n
    N <- pdim$nT$N
    ## set index names
    time.names <- pdim$panel.names$time.names
    id.names   <- pdim$panel.names$id.names
    coef.names <- names(coef(pmod))
    ## number of coefficients
    k <- length(coef.names)

    ## CIPS test needs an ADF regression with k lags
    ## so fm <- has to be like diff(e) ~ lag(e)+diff(lag(e)) etc.

    ## model data, remove index and pseries attributes
    X <- model.matrix(pmod)
    attr(X, "index") <- NULL
    y <- as.numeric(model.response(model.frame(pmod)))

    ## det. *minimum* group numerosity
    t <- min(Ti) # == min(tapply(X[,1], ind, length))

    ## check min. t numerosity
    ## NB it is also possible to allow estimation if there *is* one group
    ## with t large enough and average on coefficients removing NAs
    ## Here we choose the explicit way: let estimation fail if we lose df
    ## but a warning would do...
    if(t < (k+1)) stop("Insufficient number of time periods")

    ## one regression for each group i in 1..n
    ## and retrieve coefficients putting them into a matrix
    ## (might be unbalanced => t1!=t2 but we don't care as long
    ## as min(t)>k+1)

    ## "pre-allocate" models' list for the n models
    tmods <- vector("list", n)

    switch(match.arg(model),

           "mg" = {
               ## final data as dataframe, to be subset for single TS models
               ## (if 'trend' fix this variable's name)
               switch(match.arg(type),
                      "trend" = {
                          ## make datafr. removing intercept and add trend
                          adfdati <- data.frame(cbind(y, X[ , -1L, drop = FALSE]))
                          dimnames(adfdati)[[2L]] <- c(clnames, "trend")
                          adffm <- update(adffm, . ~ . -as.numeric(tind) + trend)},
                      "drift" = {
                          ## make df removing intercept
                          adfdati <- data.frame(cbind(y, X[ , -1L, drop = FALSE]))
                          dimnames(adfdati)[[2L]] <- clnames},
                      "none" = {
                          ## just make df (intercept isn't there)
                          adfdati <- data.frame(cbind(y, X))
                          dimnames(adfdati)[[2L]] <- clnames}
               )

               ## for each x-sect. i=1..n
               unind <- unique(ind)
               for(i in 1:n) {
                   tdati <- adfdati[ind == unind[i], ]
                   tmods[[i]] <- lm(adffm, tdati, model = FALSE) # TODO: check if my.lm.fit can be used
               }                              # (with minor modifications to code down below for t-val extraction etc.)
           },

           "dmg" = {
               ## demean (via means over group for each t)
               ## we do not care about demeaning the intercept or not as it is
               ## eliminated anyway
               demX <- Within(X, effect = tind, na.rm = TRUE)
               demy <- Within(y, effect = tind, na.rm = TRUE)

               ## final data as dataframe, to be subset for single TS models
               ## (if 'trend' fix this variable's name)
               switch(match.arg(type),
                      "trend" = {
                          ## make datafr. removing intercept and add trend
                          adfdati <- data.frame(cbind(demy, demX[ , -1L, drop = FALSE]))
                          dimnames(adfdati)[[2L]] <- c(clnames, "trend")
                          adffm <- update(adffm, . ~ . -as.numeric(tind) + trend)},
                      "drift" = {
                          ## make df removing intercept
                          adfdati <- data.frame(cbind(demy, demX[ , -1L, drop = FALSE]))
                          dimnames(adfdati)[[2L]] <- clnames},
                      "none" = {
                          ## just make df (intercept isn't there)
                          adfdati <- data.frame(cbind(demy, demX))
                          dimnames(adfdati)[[2L]] <- clnames})

               ## for each x-sect. i=1..n estimate (over t) a demeaned model
               ## (y_it-my_t) = alpha_i + beta_i*(X_it-mX_t) + err_it
               unind <- unique(ind)
               for(i in 1:n) {
                   tdati <- adfdati[ind == unind[i], ]
                   tmods[[i]] <- lm(adffm, tdati, model = FALSE)  # TODO: check if my.lm.fit can be used
               }
           },

           "cmg" = {
               deterministic2 <- switch(match.arg(type),
                                        "trend" = {"+trend"},
                                        "drift" = {""},
                                        "none"  = {"-1"})
               ## adjust formula
               adffm <- as.formula(paste("de~le+",
                                         paste(clnames[3:(lags+2)], collapse = "+"),
                                         "+", paste(paste(clnames, "bar", sep = "."),
                                                    collapse = "+"),
                                         deterministic2, sep = ""))

               ## between-periods transformation (take means over groups for each t)
               Xm <- Between(X, effect = tind, na.rm = TRUE)
               ym <- Between(y, effect = tind, na.rm = TRUE)

               ## final data as dataframe, to be subset for single TS models
               ## (purge intercepts etc., if 'trend' fix this variable's name)
               switch(match.arg(type),
                      "trend" = {
                          ## purge intercept, averaged intercept and averaged trend
                          ## (the latter is always last col. of Xm)
                          augX <- cbind(X[ , -1L, drop = FALSE], ym, Xm[ , -c(1L, dim(Xm)[[2L]]), drop = FALSE])
                          adfdati <- data.frame(cbind(y, augX))
                          dimnames(adfdati)[[2L]] <- c(clnames, "trend",
                                                       paste(clnames, "bar", sep="."))
                          adffm <- update(adffm, . ~ . -as.numeric(tind) + trend)},

                      "drift" = {
                          # remove intercepts
                          augX <- cbind(X[ , -1L, drop = FALSE], ym, Xm[ , -1L, drop = FALSE])
                          adfdati <- data.frame(cbind(y, augX))
                          dimnames(adfdati)[[2L]] <- c(clnames,
                                                       paste(clnames, "bar", sep="."))},
                      "none" = {
                          ## no intercepts here, so none to be removed
                          augX <- cbind(X, ym, Xm)
                          adfdati <- data.frame(cbind(y, augX))
                          dimnames(adfdati)[[2L]] <- c(clnames,
                                                       paste(clnames, "bar", sep="."))
                      })

               ## for each x-sect. i=1..n estimate (over t) an augmented model
               ## y_it = alpha_i + beta_i*X_it + c1_i*my_t + c2_i*mX_t + err_it
               unind <- unique(ind)
               for(i in 1:n) {
                   tdati <- adfdati[ind == unind[i], ]
                   tmods[[i]] <- lm(adffm, tdati, model = FALSE)  # TODO: check if my.lm.fit can be used
               }
           })


    ## CIPS statistic as an average of the t-stats on the coefficient of 'le'
    tstats <- vapply(tmods, function(mod) gettvalue(mod, "le"), FUN.VALUE = 0.0, USE.NAMES = FALSE)

    if(truncated) {
        ## set bounds, Pesaran (2007), p. 277
        ## NB: there is a  typo in the paper (see p. 279/281 to confirm):
        ##   Case I: "with an intercept or trend" -> "with_out_ an intercept or trend"
        ## "with_out_ an intercept or trend (Case I): K1 = 6.12, K2 = 4.16"
        ## "with an intercept and no trend (Case II): K1 = 6.19, K2 = 2.61"
        ## "with a linear trend (Case III):           K1 = 6.42, K2 = 1.70"
        ## (use negative values for K1's to ease assignment if bound is reached)
        trbounds <- switch(match.arg(type),
                           "none"  = {c(-6.12, 4.16)},
                           "drift" = {c(-6.19, 2.61)},
                           "trend" = {c(-6.42, 1.70)})
        ## formulae (34) in Pesaran (2007):
        ## truncate at lower bound
        tstats <- ifelse(tstats > trbounds[1L], tstats, trbounds[1L])
        ## truncate at upper bound
        tstats <- ifelse(tstats < trbounds[2L], tstats, trbounds[2L])
    }

    ## here allow for '...' to pass 'na.rm=TRUE' in case (but see what happens
    ## if unbalanced!
    cipstat <- mean(tstats, ...) #sum(tstats)/n
    pval <- critvals.cips(stat = cipstat, n= n, T. = T.,
                          type = type, truncated = truncated)

    ## if pval out of critical values' then set at boundary and issue
    ## a warning
    if(pval == "> 0.10") {
        pval <- 0.10
        warning("p-value greater than printed p-value")
    } else if(pval == "< 0.01") {
        pval <- 0.01
        warning("p-value smaller than printed p-value")
    }

    parameter <- lags
    names(parameter) <- "lag order"
    names(cipstat) <- "CIPS test"

    RVAL <- list(statistic   = cipstat,
                 parameter   = parameter,
                 data.name   = paste(deparse(substitute(x))),
                 tmods       = tmods,
                 method      = "Pesaran's CIPS test for unit roots",
                 alternative = "Stationarity",
                 p.value     = pval)
    class(RVAL) <- "htest"
    return(RVAL)
}


## separate function computing critical values:

critvals.cips <- function(stat, n, T., type = c("trend", "drift", "none"),
                          truncated = FALSE) {
    ## auxiliary function for cipstest()
    ## extracts --or calculates by interpolation-- p-values for the
    ## (averaged) CIPS statistic depending on whether n and T,
    ## given the critical values of average of individual cross-sectionally
    ## augmented Dickey-Fuller distribution


    ## Non truncated version
    rnam <- c(10, 15, 20, 30, 50, 70, 100, 200)
    cnam <- rnam
    znam <- c(1, 5, 10)

    ## In all following tables N in rows, T in cols unlike Pesaran (2007)

    ## No intercept, no trend (Case I); Table II(a) Pesaran (2007), p. 279

    ## 1% critical values
    nvals1 <- cbind(
        c(-2.16, -2.02, -1.93, -1.85, -1.78, -1.74, -1.71, -1.70),
        c(-2.03, -1.91, -1.84, -1.77, -1.71, -1.68, -1.66, -1.63),
        c(-2.00, -1.89, -1.83, -1.76, -1.70, -1.67, -1.65, -1.62),
        c(-1.98, -1.87, -1.80, -1.74, -1.69, -1.67, -1.64, -1.61),
        c(-1.97, -1.86, -1.80, -1.74, -1.69, -1.66, -1.63, -1.61),
        c(-1.95, -1.86, -1.80, -1.74, -1.68, -1.66, -1.63, -1.61),
        c(-1.94, -1.85, -1.79, -1.74, -1.68, -1.65, -1.63, -1.61),
        c(-1.95, -1.85, -1.79, -1.73, -1.68, -1.65, -1.63, -1.61)
    )

    ## 5% critical values
    nvals5 <- cbind(
        c(-1.80, -1.71, -1.67, -1.61, -1.58, -1.56, -1.54, -1.53),
        c(-1.74, -1.67, -1.63, -1.58, -1.55, -1.53, -1.52, -1.51),
        c(-1.72, -1.65, -1.62, -1.58, -1.54, -1.53, -1.52, -1.50),
        c(-1.72, -1.65, -1.61, -1.57, -1.55, -1.54, -1.52, -1.50),
        c(-1.72, -1.64, -1.61, -1.57, -1.54, -1.53, -1.52, -1.51),
        c(-1.71, -1.65, -1.61, -1.57, -1.54, -1.53, -1.52, -1.51),
        c(-1.71, -1.64, -1.61, -1.57, -1.54, -1.53, -1.52, -1.51),
        c(-1.71, -1.65, -1.61, -1.57, -1.54, -1.53, -1.52, -1.51)
    )

    ## 10% critical values
    nvals10 <- cbind(
        c(-1.61, -1.56, -1.52, -1.49, -1.46, -1.45, -1.44, -1.43),
        c(-1.58, -1.53, -1.50, -1.48, -1.45, -1.44, -1.44, -1.43),
        c(-1.58, -1.52, -1.50, -1.47, -1.45, -1.45, -1.44, -1.43),
        c(-1.57, -1.53, -1.50, -1.47, -1.46, -1.45, -1.44, -1.43),
        c(-1.58, -1.52, -1.50, -1.47, -1.45, -1.45, -1.44, -1.43),
        c(-1.57, -1.52, -1.50, -1.47, -1.46, -1.45, -1.44, -1.43),
        c(-1.56, -1.52, -1.50, -1.48, -1.46, -1.45, -1.44, -1.43),
        c(-1.57, -1.53, -1.50, -1.47, -1.45, -1.45, -1.44, -1.43)
    )

    ## make critical values' cube
    nvals <- array(data = NA_real_, dim = c(8L, 8L, 3L))
    nvals[ , , 1L] <- nvals1
    nvals[ , , 2L] <- nvals5
    nvals[ , , 3L] <- nvals10
    dimnames(nvals) <- list(rnam, cnam, znam)

    ## Intercept only (Case II), Table II(b) in Pesaran (2007), p. 280

    ## 1% critical values
    dvals1 <- cbind(
        c(-2.97, -2.76, -2.64, -2.51, -2.41, -2.37, -2.33, -2.28),
        c(-2.66, -2.52, -2.45, -2.34, -2.26, -2.23, -2.19, -2.16),
        c(-2.60, -2.47, -2.40, -2.32, -2.25, -2.20, -2.18, -2.14),
        c(-2.57, -2.45, -2.38, -2.30, -2.23, -2.19, -2.17, -2.14),
        c(-2.55, -2.44, -2.36, -2.30, -2.23, -2.20, -2.17, -2.14),
        c(-2.54, -2.43, -2.36, -2.30, -2.23, -2.20, -2.17, -2.14),
        c(-2.53, -2.42, -2.36, -2.30, -2.23, -2.20, -2.18, -2.15),
        c(-2.53, -2.43, -2.36, -2.30, -2.23, -2.21, -2.18, -2.15)
    )

    ## 5% critical values
    dvals5 <- cbind(
        c(-2.52, -2.40, -2.33, -2.25, -2.19, -2.16, -2.14, -2.10),
        c(-2.37, -2.28, -2.22, -2.17, -2.11, -2.09, -2.07, -2.04),
        c(-2.34, -2.26, -2.21, -2.15, -2.11, -2.08, -2.07, -2.04),
        c(-2.33, -2.25, -2.20, -2.15, -2.11, -2.08, -2.07, -2.05),
        c(-2.33, -2.25, -2.20, -2.16, -2.11, -2.10, -2.08, -2.06),
        c(-2.33, -2.25, -2.20, -2.15, -2.12, -2.10, -2.08, -2.06),
        c(-2.32, -2.25, -2.20, -2.16, -2.12, -2.10, -2.08, -2.07),
        c(-2.32, -2.25, -2.20, -2.16, -2.12, -2.10, -2.08, -2.07)
    )

    ## 10% critical values
    dvals10 <- cbind(
        c(-2.31, -2.22, -2.18, -2.12, -2.07, -2.05, -2.03, -2.01),
        c(-2.22, -2.16, -2.11, -2.07, -2.03, -2.01, -2.00, -1.98),
        c(-2.21, -2.14, -2.10, -2.07, -2.03, -2.01, -2.00, -1.99),
        c(-2.21, -2.14, -2.11, -2.07, -2.04, -2.02, -2.01, -2.00),
        c(-2.21, -2.14, -2.11, -2.08, -2.05, -2.03, -2.02, -2.01),
        c(-2.21, -2.15, -2.11, -2.08, -2.05, -2.03, -2.02, -2.01),
        c(-2.21, -2.15, -2.11, -2.08, -2.05, -2.03, -2.03, -2.02),
        c(-2.21, -2.15, -2.11, -2.08, -2.05, -2.04, -2.03, -2.02)
    )

    ## make critical values' cube
    dvals <- array(data = NA_real_, dim = c(8L, 8L, 3L))
    dvals[ , , 1L] <- dvals1
    dvals[ , , 2L] <- dvals5
    dvals[ , , 3L] <- dvals10
    dimnames(dvals) <- list(rnam, cnam, znam)

    ## Intercept and trend (Case III), Table II(c) in Pesaran (2007), p. 281

    ## 1% critical values
    tvals1 <- cbind(
        c(-3.88, -3.61, -3.46, -3.30, -3.15, -3.10, -3.05, -2.98),
        c(-3.24, -3.09, -3.00, -2.89, -2.81, -2.77, -2.74, -2.71),
        c(-3.15, -3.01, -2.92, -2.83, -2.76, -2.72, -2.70, -2.65),
        c(-3.10, -2.96, -2.88, -2.81, -2.73, -2.69, -2.66, -2.63),
        c(-3.06, -2.93, -2.85, -2.78, -2.72, -2.68, -2.65, -2.62),
        c(-3.04, -2.93, -2.85, -2.78, -2.71, -2.68, -2.65, -2.62),
        c(-3.03, -2.92, -2.85, -2.77, -2.71, -2.68, -2.65, -2.62),
        c(-3.03, -2.91, -2.85, -2.77, -2.71, -2.67, -2.65, -2.62)
    )

    ## 5% critical values
    tvals5 <- cbind(
        c(-3.27, -3.11, -3.02, -2.94, -2.86, -2.82, -2.79, -2.75),
        c(-2.93, -2.83, -2.77, -2.70, -2.64, -2.62, -2.60, -2.57),
        c(-2.88, -2.78, -2.73, -2.67, -2.62, -2.59, -2.57, -2.55),
        c(-2.86, -2.76, -2.72, -2.66, -2.61, -2.58, -2.56, -2.54),
        c(-2.84, -2.76, -2.71, -2.65, -2.60, -2.58, -2.56, -2.54),
        c(-2.83, -2.76, -2.70, -2.65, -2.61, -2.58, -2.57, -2.54),
        c(-2.83, -2.75, -2.70, -2.65, -2.61, -2.59, -2.56, -2.55),
        c(-2.83, -2.75, -2.70, -2.65, -2.61, -2.59, -2.57, -2.55)
    )

    ## 10% critical values
    tvals10 <- cbind(
        c(-2.98, -2.89, -2.82, -2.76, -2.71, -2.68, -2.66, -2.63),
        c(-2.76, -2.69, -2.65, -2.60, -2.56, -2.54, -2.52, -2.50),
        c(-2.74, -2.67, -2.63, -2.58, -2.54, -2.53, -2.51, -2.49),
        c(-2.73, -2.66, -2.63, -2.58, -2.54, -2.52, -2.51, -2.49),
        c(-2.73, -2.66, -2.63, -2.58, -2.55, -2.53, -2.51, -2.50),
        c(-2.72, -2.66, -2.62, -2.58, -2.55, -2.53, -2.52, -2.50),
        c(-2.72, -2.66, -2.63, -2.59, -2.55, -2.53, -2.52, -2.50),
        c(-2.73, -2.66, -2.63, -2.59, -2.55, -2.54, -2.52, -2.51)
    )

    ## make critical values' cube
    tvals <- array(data = NA_real_, dim = c(8L, 8L, 3L))
    tvals[ , , 1L] <- tvals1
    tvals[ , , 2L] <- tvals5
    tvals[ , , 3L] <- tvals10
    dimnames(tvals) <- list(rnam, cnam, znam)

    ## if truncated substitute values according to Tables II(a), II(b), II(c)
    ## in Pesaran (2007)

    if(truncated) {
        # Case III (Intercept and trend)
        tvals[,1,1] <- -c(3.51, 3.31, 3.20, 3.10, 3.00, 2.96, 2.93, 2.88) # II(c),  1%
        tvals[,2,1] <- -c(3.21, 3.07, 2.98, 2.88, 2.80, 2.76, 2.74, 2.70) # II(c),  1%
        tvals[,1,2] <- -c(3.10, 2.97, 2.89, 2.82, 2.75, 2.73, 2.70, 2.67) # II(c),  5%
        tvals[,2,2] <- -c(2.92, 2.82, 2.76, 2.69, 2.64, 2.62, 2.59, 2.57) # II(c),  5%
        tvals[,1,3] <- -c(2.87, 2.78, 2.73, 2.67, 2.63, 2.60, 2.58, 2.56) # II(c), 10%
        tvals[,2,3] <- -c(2.76, 2.68, 2.64, 2.59, 2.55, 2.53, 2.51, 2.50) # II(c), 10%

        # Case II (Intercept only)
        dvals[,1,1] <- -c(2.85, 2.66, 2.56, 2.44, 2.36, 2.32, 2.29, 2.25) # II(b),  1%
        dvals[,1,2] <- -c(2.47, 2.35, 2.29, 2.22, 2.16, 2.13, 2.11, 2.08) # II(b),  5%
        dvals[,1,3] <- -c(2.28, 2.20, 2.15, 2.10, 2.05, 2.03, 2.01, 1.99) # II(b), 10%

        # Case I (No intercept, no trend)
        nvals[,1,1] <- -c(2.14, 2.00 ,1.91, 1.84, 1.77, 1.73, 1.71, 1.69) # II(a),  1%
        nvals[,1,2] <- -c(1.79, 1.71, 1.66, 1.61, 1.57, 1.55, 1.53, 1.52) # II(a),  5%
        nvals[,1,3][c(2,4,7)] <- -c(1.55, 1.48, 1.43)                     # II(a), 10%
    }

    ## set this according to model
    switch(match.arg(type),
           "trend" = {cvals <- tvals},
           "drift" = {cvals <- dvals},
           "none"  = {cvals <- nvals})


    ## find intervals for current n and T.
    nintl <- findInterval(n, rnam)
    ninth <- nintl + 1
    nintv <- rnam[nintl:ninth]
    tintl <- findInterval(T., cnam)
    tinth <- tintl + 1
    tintv <- cnam[tintl:tinth]

    ## for each critical value
    cv <- numeric(3)
    for(i in 1:3) {

        ## on N dim
        if(n %in% rnam) {
            ## if n is exactly one of the tabulated values:
            tl <- cvals[which(rnam == n), tintl, i]
            th <- cvals[which(rnam == n), tinth, i]

        } else {
            ## interpolate interval of interest to get cvals(n,T.)
            tl <- approx(nintv, cvals[nintl:ninth, tintl, i],
                         n = max(nintv) - min(nintv))$y[n - min(nintv)]
            th <- approx(nintv, cvals[nintl:ninth, tinth, i],
                         n = max(nintv) - min(nintv))$y[n - min(nintv)]
        }

        ## on T. dim
        if(T. %in% cnam) {
            ## if T. is exactly one of the tabulated values:
            if(n %in% rnam) {
                ## ... and n too:
                cv[i] <- cvals[which(rnam == n), which(cnam == T.), i]
            } else {
                ## or if n is not, interpolate n on T.'s exact row:
                cv[i] <- approx(nintv, cvals[nintl:ninth, which(cnam == T.), i],
                                n = max(nintv) - min(nintv))$y[n - min(nintv)]
            }
        } else {
            ## idem: interpolate T.-interval to get critical value
            cv[i] <- approx(tintv, c(tl, th),
                            n = max(tintv) - min(tintv))$y[T. - min(tintv)]
        }
    }

    ## approximate p-values' sequence
    cvprox <- approx(cv, c(0.01, 0.05, 0.1), n = 200)
    cvseq <- cvprox$x
    pvseq <- cvprox$y

    if(stat < min(cv)) {
        pval <- "< 0.01"
    } else {
        if(stat > max(cv)) {
            pval <- "> 0.10"
        } else {
            if(stat %in% cv) {
                ## if exactly one of the tabulated values
                pval <- c(0.01, 0.05, 0.10)[which(cv == stat)]
            } else {
                ## find interval where true p-value lies and
                ## set p-value as the mean of bounds
                kk <- findInterval(stat, cvseq)
                pval <- mean(pvseq[kk:(kk+1)])
            }
        }
    }

    return(pval)
}


gettvalue <- function(x, coefname) {
    ## non-exported
    ## helper function to extract one or more t value(s)
    ## (coef/s.e.) for a coefficient from model object useful if one wants
    ## to avoid the computation of a whole lot of values with summary()

    # x: model object (usually class plm or lm) coefname: character
    # indicating name(s) of coefficient(s) for which the t value(s) is
    # (are) requested
    # return value: named numeric vector of length == length(coefname)
    # with requested t value(s)
    beta <- coef(x)[coefname]
    se <- sqrt(diag(vcov(x))[coefname])
    tvalue <- beta / se
    return(tvalue)
}

pseries2pdataframe <- function(x, pdata.frame = TRUE, ...) {
    ## non-exported
    ## Transforms a pseries in a (p)data.frame with the indices as regular columns
    ## in positions 1, 2 and (if present) 3 (individual index, time index, group index).
    ## if pdataframe = TRUE -> return a pdata.frame, if FALSE -> return a data.frame
    ## ellipsis (dots) passed on to pdata.frame()
    if(!inherits(x, "pseries")) stop("input needs to be of class 'pseries'")
    indices <- attr(x, "index")
    class(indices) <- setdiff(class(indices), "pindex")
    vx <- remove_pseries_features(x)
    dfx <- cbind(indices, vx)
    dimnames(dfx)[[2L]] <- c(names(indices), deparse(substitute(x)))
    res <- if(pdata.frame == TRUE) {
        pdata.frame(dfx, index = names(indices), ...)
    } else { dfx }
    return(res)
}

pmerge <- function(x, y, ...) {
    ## non-exported
    ## Returns a data.frame, not a pdata.frame.
    ## pmerge is used to merge pseries or pdata.frames into a data.frame or
    ## to merge a pseries to a data.frame

    ## transf. if pseries or pdata.frame
    if(inherits(x, "pseries")) x <- pseries2pdataframe(x, pdata.frame = FALSE)
    if(inherits(y, "pseries")) y <- pseries2pdataframe(y, pdata.frame = FALSE)
    if(inherits(x, "pdata.frame")) x <- as.data.frame(x, keep.attributes = FALSE)
    if(inherits(y, "pdata.frame")) y <- as.data.frame(y, keep.attributes = FALSE)

    # input to merge() needs to be data.frames; not yet suitable for 3rd index (group variable)
    z <- merge(x, y,
               by.x = dimnames(x)[[2L]][1:2],
               by.y = dimnames(y)[[2L]][1:2], ...)
    return(z)
}


# test_general.R#
#' Hausman Test for Panel Models
#'
#' Specification test for panel models.
#'
#' The Hausman test (sometimes also called Durbin--Wu--Hausman test)
#' is based on the difference of the vectors of coefficients of two
#' different models.  The `panelmodel` method computes the original
#' version of the test based on a quadratic form
#' \insertCite{HAUS:78}{plm}. The `formula` method, if
#' `method = "chisq"` (default), computes the original version of the
#' test based on a quadratic form; if `method ="aux"` then the
#' auxiliary-regression-based version as in \insertCite{WOOL:10;textual}{plm},
#' Sec.10.7.3. Only the latter can be robustified by specifying a robust
#' covariance estimator as a function through the argument `vcov` (see
#' **Examples**).
#'
#' The `effect` argument is only relevant for the formula method/interface and
#' is then applied to both models. For the panelmodel method/interface, the test
#' is run with the effects of the already estimated models.
#'
#' The equivalent tests in the **one-way** case using a between
#' model (either "within vs. between" or "random vs. between")
#' \insertCite{@see @HAUS:TAYL:81 or @BALT:13 Sec.4.3}{plm} can also
#' be performed by `phtest`, but only for `test = "chisq"`, not for
#' the regression-based test. NB: These equivalent tests using the
#' between model do not extend to the two-ways case.  There are,
#' however, some other equivalent tests,
#' \insertCite{@see @KANG:85 or @BALT:13 Sec.4.3.7}{plm},
#' but those are unsupported by `phtest`.
#'
#' @aliases phtest
#' @param x an object of class `"panelmodel"` or `"formula"`,
#' @param x2 an object of class `"panelmodel"` (only for panelmodel method/interface),
#' @param model a character vector containing the names of two models
#' (length(model) must be 2),
#' @param effect a character specifying the effect to be introduced to both models,
#'  one of `"individual"`, `"time"`, or `"twoways"` (only for formula method),
#' @param data a `data.frame`,
#' @param method one of `"chisq"` or `"aux"`,
#' @param index an optional vector of index variables,
#' @param vcov an optional covariance function,
#' @param \dots further arguments to be passed on (currently none).
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant, Giovanni Millo
#' @references
#'
#' \insertRef{HAUS:78}{plm}
#'
#' \insertRef{HAUS:TAYL:81}{plm}
#'
#' \insertRef{KANG:85}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("Gasoline", package = "plm")
#' form <- lgaspcar ~ lincomep + lrpmg + lcarpcap
#' wi <- plm(form, data = Gasoline, model = "within")
#' re <- plm(form, data = Gasoline, model = "random")
#' phtest(wi, re)
#' phtest(form, data = Gasoline)
#' phtest(form, data = Gasoline, effect = "time")
#'
#' # Regression-based Hausman test
#' phtest(form, data = Gasoline, method = "aux")
#'
#' # robust Hausman test with vcov supplied as a function and
#' # with additional parameters
#' phtest(form, data = Gasoline, method = "aux", vcov = vcovHC)
#' phtest(form, data = Gasoline, method = "aux",
#'   vcov = function(x) vcovHC(x, method="white2", type="HC3"))
#'
phtest <- function(x,...){
    UseMethod("phtest")
}

#' @rdname phtest
#' @export
phtest.formula <- function(x, data, model = c("within", "random"),
                           effect = c("individual", "time", "twoways"),
                           method = c("chisq", "aux"),
                           index = NULL, vcov = NULL, ...) {

    if (length(model) != 2) stop("two models should be indicated in argument 'model'")
    for (i in 1:2){
        model.name <- model[i]
        if(!(model.name %in% names(model.plm.list))){
            stop("model must be one of ", oneof(model.plm.list))
        }
    }

    effect <- match.arg(effect)

    switch(match.arg(method),
           "chisq" = {
               cl <- match.call(expand.dots = TRUE)
               cl$model <- model[1L]
               cl$effect <- effect
               names(cl)[2L] <- "formula"
               m <- match(plm.arg, names(cl), 0L)
               cl <- cl[c(1L, m)]
               cl[[1L]] <- as.name("plm")
               plm.model.1 <- eval(cl, parent.frame())
               plm.model.2 <- update(plm.model.1, model = model[2L])
               return(phtest(plm.model.1, plm.model.2)) # exit to phtest.panelmodel
           },
           "aux" = {
               ## some interface checks here
               if (model[1L] != "within") {
                   stop("Please supply 'within' as first model type")
               }

               if (!is.null(vcov) && !is.function(vcov)) stop("argument 'vcov' needs to be a function")

               ## set pdata.frame
               if (!inherits(data, "pdata.frame")) data <- pdata.frame(data, index = index) #, ...)

               row.names(data) <- NULL # reset rownames of original data set (->numbers rownames in clean sequence) to make rownames
               # comparable for later comparison to obs used in estimation of models (get rid of NA values)
               # [needed because pmodel.response() and model.matrix() do not retain fancy rownames, but rownames]

               # calculate FE and RE model
               fe_mod <- plm(formula = x, data = data, model = model[1L], effect = effect)
               re_mod <- plm(formula = x, data = data, model = model[2L], effect = effect)

               ## DEBUG printing:
               # print(effect)
               # print(model)
               # print(paste0("mod1: ", describe(fe_mod, "effect")))
               # print(paste0("mod2: ", describe(re_mod, "effect")))
               # print(fe_mod)
               # print(re_mod)

               reY <- pmodel.response(re_mod)
               #               reX <- model.matrix(re_mod)[ , -1, drop = FALSE] # intercept not needed; drop=F needed to prevent matrix
               #               feX <- model.matrix(fe_mod, cstcovar.rm = TRUE)  # from degenerating to vector if only one regressor
               reX <- model.matrix(re_mod, cstcovar.rm = "intercept")
               feX <- model.matrix(fe_mod, cstcovar.rm = "all")

               dimnames(feX)[[2L]] <- paste(dimnames(feX)[[2L]], "tilde", sep=".")
               ## estimated models could have fewer obs (due dropping of NAs) compared to the original data
               ## => match original data and observations used in estimated models
               ## routine adapted from lmtest::bptest
               commonrownames <- intersect(intersect(intersect(row.names(data), names(reY)), row.names(reX)), row.names(feX))
               if (!(all(c(row.names(data) %in% commonrownames, commonrownames %in% row.names(data))))) {
                   data <- data[commonrownames, ]
                   reY <- reY[commonrownames]
                   reX <- reX[commonrownames, ]
                   feX <- feX[commonrownames, ]
               }

               # Tests of correct matching of obs (just for safety ...)
               if(!all.equal(length(reY), nrow(data), nrow(reX), nrow(feX)))
                   stop("number of cases/observations do not match, most likely due to NAs in \"data\"")
               if(any(c(is.na(names(reY)), is.na(row.names(data)), is.na(row.names(reX)), is.na(row.names(feX)))))
                   stop("one (or more) rowname(s) is (are) NA")
               if(!all.equal(names(reY), row.names(data), row.names(reX), row.names(feX)))
                   stop("row.names of cases/observations do not match, most likely due to NAs in \"data\"")

               ## fetch indices here, check pdata
               ## construct data set and formula for auxiliary regression
               data <- pdata.frame(cbind(index(data), reY, reX, feX))
               auxfm <- as.formula(paste("reY~",
                                         paste(dimnames(reX)[[2L]],
                                               collapse="+"), "+",
                                         paste(dimnames(feX)[[2L]],
                                               collapse="+"), sep=""))
               auxmod <- plm(formula = auxfm, data = data, model = "pooling")
               nvars <- dim(feX)[[2L]]
               R <- diag(1, nvars)
               r <- rep(0, nvars) # here just for clarity of illustration
               range <- (nvars+2L):(nvars*2L + 1L)
               omega0 <- vcov(auxmod)[range, range]
               Rbr <- R %*% coef(auxmod)[range] - r

               h2t <- as.numeric(crossprod(Rbr, solve(omega0, Rbr)))
               ph2t <- pchisq(h2t, df = nvars, lower.tail = FALSE)

               df <- nvars
               names(df) <- "df"
               names(h2t) <- "chisq"

               if(!is.null(vcov)) {
                   vcov <- paste(", vcov: ",
                                 paste(deparse(substitute(vcov))),
                                 sep="")
               }

               haus2 <- list(statistic   = h2t,
                             p.value     = ph2t,
                             parameter   = df,
                             method      = paste("Regression-based Hausman test",
                                                 vcov, sep=""),
                             alternative = "one model is inconsistent",
                             data.name   = paste(deparse(substitute(x))))
               class(haus2) <- "htest"
               return(haus2)
           })
}

#' @rdname phtest
#' @export
phtest.panelmodel <- function(x, x2, ...) {
    coef.wi <- coef(x)
    coef.re <- coef(x2)
    vcov.wi <- vcov(x)
    vcov.re <- vcov(x2)
    names.wi <- names(coef.wi)
    names.re <- names(coef.re)
    common_coef_names <- names.re[names.re %in% names.wi]
    coef.h <- common_coef_names[!(common_coef_names %in% "(Intercept)")] # drop intercept if included (relevant when between model input)
    if(length(coef.h) == 0L) stop("no common coefficients in models")
    dbeta <- coef.wi[coef.h] - coef.re[coef.h]
    df <- length(dbeta)
    dvcov <- vcov.wi[coef.h, coef.h] - vcov.re[coef.h, coef.h]

    #### BEGIN cater for equivalent test within vs. between
    # Baltagi (2013), Sec. 4.3, pp. 77, 81
    modx  <- describe(x,  what = "model")
    modx2 <- describe(x2, what = "model")
    effx  <- describe(x,  what = "effect")
    effx2 <- describe(x2, what = "effect")

    # Tests with between model do not extend to two-ways case -> give error
    # There are, however, some equiv. tests with the individual/time between
    # model, but let's not support them (see Kang (1985), Baltagi (2013), Sec. 4.3.7)
    if (   (modx  == "between" || modx2 == "between")
           && (effx == "twoways" || effx2 == "twoways")) stop("tests with between model in twoways case not supported")

    # in case of one-way within vs. between (m3 in Baltagi (2013), pp. 77, 81)
    # the variances need to be added (not subtracted like in the other cases)
    if (   (modx  == "within" && modx2 == "between")
           || (modx2 == "within" && modx  == "between")) {
        dvcov <- vcov.wi[coef.h, coef.h] + vcov.re[coef.h, coef.h]
    }
    #### END cater for equivalent tests with between model

    stat <- as.numeric(abs(t(dbeta) %*% solve(dvcov) %*% dbeta))
    pval <- pchisq(stat, df = df, lower.tail = FALSE)
    names(stat) <- "chisq"
    parameter <- df
    names(parameter) <- "df"
    alternative <- "one model is inconsistent"

    ## DEBUG printing:
    # print(paste0("mod1: ", describe(x,  "effect")))
    # print(paste0("mod2: ", describe(x2, "effect")))

    res <- list(statistic    = stat,
                p.value      = pval,
                parameter    = parameter,
                method       = "Hausman Test",
                data.name    = data.name(x),
                alternative  = alternative)
    class(res) <- "htest"
    return(res)
}

############## plmtest() ############################################
# For a concise overview with original references, see
# Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, pp. 68-76 (balanced), pp. 200-203 (unbalanced).
#
# balanced (original) version of Breusch-Pagan test:
#     T.S. Breusch & A.R. Pagan (1979),
#       A Simple Test for Heteroscedasticity and Random Coefficient Variation,
#       Econometrica 47, pp. 1287-1294
#
# unbalanced version:
#     Baltagi/Li (1990),
#       A lagrange multiplier test for the error components model with incomplete panels,
#       Econometric Reviews, 9, pp. 103-107,


# pchibarsq: helper function: "p-function" for mixed chisq (also called chi-bar-squared)
# used in plmtest(., type = "ghm"), see Baltagi (2013), pp. 71-72, 74, 88, 202-203, 209
#
# a reference for the distribution seems to be
# Dykstra, R./El Barmi, H., Chi-Bar-Square Distributions, in: Encyclopedia of Statistical Sciences,
# DOI: 10.1002/0471667196.ess0265.pub2
pchibarsq <- function(q, df, weights, lower.tail = TRUE, ... ) {
    # NB: other parameters in dots (...): not checked if valid! (ncp, log, ...)
    sum(weights * pchisq(q, df = df, lower.tail = lower.tail, ...))
}




#' Lagrange FF Multiplier Tests for Panel Models
#'
#' Test of individual and/or time effects for panel models.
#'
#' These Lagrange multiplier tests use only the residuals of the
#' pooling model.  The first argument of this function may be either a
#' pooling model of class `plm` or an object of class `formula`
#' describing the model. For input within (fixed effects) or random
#' effects models, the corresponding pooling model is calculated
#' internally first as the tests are based on the residuals of the
#' pooling model.
#'
#' The `"bp"` test for unbalanced panels was derived in
#' \insertCite{BALT:LI:90;textual}{plm}
#' (1990), the `"kw"` test for unbalanced panels in
#' \insertCite{BALT:CHAN:LI:98;textual}{plm}.
#'
#' The `"ghm"` test and the `"kw"` test were extended to two-way
#' effects in \insertCite{BALT:CHAN:LI:92;textual}{plm}.
#'
#' For a concise overview of all these statistics see
#' \insertCite{BALT:03;textual}{plm}, Sec. 4.2, pp. 68--76 (for balanced
#' panels) and Sec. 9.5, pp. 200--203 (for unbalanced panels).
#'
#' @aliases plmtest
#' @param x an object of class `"plm"` or a formula of class
#'     `"formula"`,
#' @param data a `data.frame`,
#' @param effect a character string indicating which effects are
#'     tested: individual effects (`"individual"`), time effects
#'     (`"time"`) or both (`"twoways"`),
#' @param type a character string indicating the test to be performed:
#'
#' - `"honda"` (default) for \insertCite{HOND:85;textual}{plm},
#' - `"bp"` for \insertCite{BREU:PAGA:80;textual}{plm},
#' - `"kw"` for \insertCite{KING:WU:97;textual}{plm}, or
#' - `"ghm"` for \insertCite{GOUR:HOLL:MONF:82;textual}{plm} for
#'     unbalanced panel data sets, the respective unbalanced version
#'     of the tests are computed,
#'
#' @param \dots further arguments passed to `plmtest`.
#' @return An object of class `"htest"`.
#' @note For the King-Wu statistics (`"kw"`), the oneway statistics
#'     (`"individual"` and `"time"`) coincide with the respective
#'     Honda statistics (`"honda"`); twoway statistics of `"kw"` and
#'     `"honda"` differ.
#' @export
#' @author Yves Croissant (initial implementation), Kevin Tappe
#'     (generalization to unbalanced panels)
#' @seealso [pFtest()] for individual and/or time effects tests based
#'     on the within model.
#' @references
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:LI:90}{plm}
#'
#' \insertRef{BALT:CHAN:LI:92}{plm}
#'
#' \insertRef{BALT:CHAN:LI:98}{plm}
#'
#' \insertRef{BREU:PAGA:80}{plm}
#'
#' \insertRef{GOUR:HOLL:MONF:82}{plm}
#'
#' \insertRef{HOND:85}{plm}
#'
#' \insertRef{KING:WU:97}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' g <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
#' plmtest(g)
#' plmtest(g, effect="time")
#' plmtest(inv ~ value + capital, data = Grunfeld, type = "honda")
#' plmtest(inv ~ value + capital, data = Grunfeld, type = "bp")
#' plmtest(inv ~ value + capital, data = Grunfeld, type = "bp",  effect = "twoways")
#' plmtest(inv ~ value + capital, data = Grunfeld, type = "ghm", effect = "twoways")
#' plmtest(inv ~ value + capital, data = Grunfeld, type = "kw",  effect = "twoways")
#'
#' Grunfeld_unbal <- Grunfeld[1:(nrow(Grunfeld)-1), ] # create an unbalanced panel data set
#' g_unbal <- plm(inv ~ value + capital, data = Grunfeld_unbal, model = "pooling")
#' plmtest(g_unbal) # unbalanced version of test is indicated in output
#'
plmtest <- function(x, ...){
    UseMethod("plmtest")
}

#' @rdname plmtest
#' @export
plmtest.plm <- function(x,
                        effect = c("individual", "time", "twoways"),
                        type = c("honda", "bp", "ghm", "kw"),
                        ...) {

    effect <- match.arg(effect)
    type <- match.arg(type)
    if (describe(x, "model") != "pooling") x <- update(x, model = "pooling")
    pdim <- pdim(x)
    n <- pdim$nT$n
    T <- pdim$nT$T
    N_obs <- pdim$nT$N
    balanced <- pdim$balanced
    index <- unclass(attr(model.frame(x), "index")) # unclass for speed
    id <- index[[1L]]
    time <- index[[2L]]
    T_i <- pdim$Tint$Ti
    N_t <- pdim$Tint$nt
    res <- resid(x)

    ### calc of parts of test statistic ##
    # calc. is done w/o using matrix calculation, see, e.g., Baltagi/Li (1990), p. 106
    CP.res <- crossprod(res)
    A1 <- as.numeric(crossprod(tapply(res, id,   sum)) / CP.res - 1) # == A1 <- sum(tapply(res,id,sum)^2)   / sum(res^2) - 1
    A2 <- as.numeric(crossprod(tapply(res, time, sum)) / CP.res - 1) # == A2 <- sum(tapply(res,time,sum)^2) / sum(res^2) - 1

    M11 <- sum(T_i ^ 2)
    M22 <- sum(N_t ^ 2)

    LM1 <- N_obs * (1 / sqrt(2 * (M11 - N_obs))) * A1 # == sqrt( (((N_obs)^2) / 2) * ( A1^2 / (M11 - N_obs)) ) [except sign due to positive sqrt]
    LM2 <- N_obs * (1 / sqrt(2 * (M22 - N_obs))) * A2 # == sqrt( (((N_obs)^2) / 2) * ( A2^2 / (M22 - N_obs)) ) [except sign due to positive sqrt]
    ### END calc of parts of test statistic ##


    if (effect != "twoways"){
        # oneway
        if (!type %in% c("honda", "bp", "kw"))
            stop("type must be one of \"honda\", \"bp\" or \"kw\" for a one way model") # kw oneway coincides with honda

        stat <- if(effect == "individual") LM1 else LM2
        stat <- switch(type,
                       honda = c(normal = stat),
                       bp    = c(chisq  = stat ^ 2),
                       kw    = c(normal = stat))

        parameter <- switch(type,
                            honda = NULL,
                            bp = c(df = 1), # df = 1 in the oneway case (Baltagi (2013), p. 70)
                            kw = NULL)

        pval <- switch(type,
                       honda = pnorm(stat, lower.tail = FALSE), # honda oneway ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
                       bp    = pchisq(stat, df = parameter, lower.tail = FALSE), # df = 1 in the one-way case, alternative is two-sided (Baltagi (2013), p. 70/201)
                       kw    = pnorm(stat, lower.tail = FALSE)) # kw oneway ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
        # END oneway
    }
    else { # twoways
        stat <- switch(type,
                       honda = c(normal = (LM1 + LM2) / sqrt(2)),
                       bp    = c(chisq = LM1 ^ 2 + LM2 ^ 2),
                       kw    = c(normal = (sqrt(M11 - N_obs) / sqrt(M11 + M22 - 2 * N_obs)) * LM1 +
                                     (sqrt(M22 - N_obs) / sqrt(M11 + M22 - 2 * N_obs)) * LM2),
                       ghm   = c(chibarsq = max(0, LM1) ^ 2 + max(0, LM2) ^ 2))

        parameter <- switch(type,
                            honda = NULL,
                            bp    = c(df = 2), # df = 2 in the twoway case (Baltagi (2013), p. 70/201)
                            kw    = NULL,
                            ghm   = c(df0 = 0L, df1 = 1L, df2 = 2L, w0 = 1/4, w1 = 1/2, w2 = 1/4)) # chibarsquared (mixed chisq) has several dfs and weights (Baltagi (2013), p. 72/202)

        pval <- switch(type,
                       honda = pnorm(stat, lower.tail = FALSE), # honda two-ways ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
                       bp    = pchisq(stat, df = parameter, lower.tail = FALSE),  # is df = 2 in the twoway case, alternative is two-sided (Baltagi (2013), p. 70/201)
                       kw    = pnorm(stat, lower.tail = FALSE), # kw twoways ~ N(0,1), alternative is one-sided (Baltagi (2013), p. 71/202)
                       ghm   = pchibarsq(stat, df = c(0L, 1L, 2L), weights = c(1/4, 1/2, 1/4), lower.tail = FALSE)) # mixed chisq (also called chi-bar-square), see Baltagi (2013), pp. 71-72, 74, 88, 202-203, 209
    } # END twoways

    method.type <- switch(type,
                          honda  = "Honda",
                          bp     = "Breusch-Pagan",
                          ghm    = "Gourieroux, Holly and Monfort",
                          kw     = "King and Wu")

    method.effect <- switch(effect,
                            id      = "individual effects",
                            time    = "time effects",
                            twoways = "two-ways effects")

    balanced.type <- if(balanced) "balanced" else "unbalanced"

    method <- paste("Lagrange Multiplier Test - ", method.effect,
                    " (", method.type, ") for ", balanced.type, " panels", sep="")

    if (type %in% c("honda", "kw")) {
        RVAL <- list(statistic = stat,
                     p.value   = pval,
                     method    = method,
                     data.name = data.name(x))
    }
    else { # bp, ghm
        RVAL <- list(statistic = stat,
                     p.value   = pval,
                     method    = method,
                     parameter = parameter,
                     data.name = data.name(x))
    }

    RVAL$alternative <- "significant effects" # TODO: maybe distinguish b/w one-sided and two-sided alternatives?
    #       (bp: two-sided alt.; all others: one-sided alt.?)

    class(RVAL) <- "htest"
    return(RVAL)
}

#' @rdname plmtest
#' @export
plmtest.formula <- function(x, data, ...,
                            effect = c("individual", "time", "twoways"),
                            type = c("honda", "bp", "ghm", "kw")) {

    cl <- match.call(expand.dots = TRUE)
    cl$model <- "pooling" # plmtest is performed on the pooling model...
    cl$effect <- NULL     # ... and pooling model has no argument effect...
    cl$type <- NULL       # ... and no argument type => see below: pass on args effect and type to plmtest.plm()
    names(cl)[2L] <- "formula"
    m <- match(plm.arg, names(cl), 0L)
    cl <- cl[c(1L, m)]
    cl[[1L]] <- as.name("plm")
    plm.model <- eval(cl, parent.frame())
    plmtest(plm.model, effect = effect, type = type) # pass on args effect and type to plmtest.plm()
}


#' F Test for Individual and/or Time Effects
#'
#' Test of individual and/or time effects based on the comparison of the
#' `within` and the `pooling` model.
#'
#' For the `plm` method, the argument of this function is two `plm`
#' objects, the first being a within model, the second a pooling
#' model. The effects tested are either individual, time or twoways,
#' depending on the effects introduced in the within model.
#'
#' @aliases pFtest
#' @param x an object of class `"plm"` or of class `"formula"`,
#' @param z an object of class `"plm"`,
#' @param data a `data.frame`,
#' @param \dots further arguments.
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant
#' @seealso [plmtest()] for Lagrange multiplier tests of individuals
#'     and/or time effects.
#' @keywords htest
#' @examples
#'
#' data("Grunfeld", package="plm")
#' gp <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
#' gi <- plm(inv ~ value + capital, data = Grunfeld,
#'           effect = "individual", model = "within")
#' gt <- plm(inv ~ value + capital, data = Grunfeld,
#'           effect = "time", model = "within")
#' gd <- plm(inv ~ value + capital, data = Grunfeld,
#'           effect = "twoways", model = "within")
#' pFtest(gi, gp)
#' pFtest(gt, gp)
#' pFtest(gd, gp)
#' pFtest(inv ~ value + capital, data = Grunfeld, effect = "twoways")
#'
pFtest <- function(x, ...){
    UseMethod("pFtest")
}

#' @rdname pFtest
#' @export
pFtest.formula <- function(x, data, ...){
    cl <- match.call(expand.dots = TRUE)
    cl$model <- "within"
    names(cl)[2L] <- "formula"
    m <- match(plm.arg,names(cl), 0L)
    cl <- cl[c(1L, m)]
    cl[[1L]] <- as.name("plm")
    plm.within <- eval(cl,parent.frame())
    plm.pooling <- update(plm.within, model = "pooling")
    pFtest(plm.within, plm.pooling, ...)
}

#' @rdname pFtest
#' @export
pFtest.plm <- function(x, z, ...){
    within <- x
    pooling <- z
    ## leave this interface check commented because pkg AER (reverse dependency) has examples that
    ## use pFtest(within_twoway, within_time)
    # if (! (describe(x, "model") == "within" && describe(z, "model") == "pooling"))
    #  stop("the two arguments should be a 'within' and a 'pooling' model (in this order)")

    effect <- describe(x, "effect")
    df1 <- df.residual(pooling)-df.residual(within)
    df2 <- df.residual(within)
    ssrp <- as.numeric(crossprod(residuals(pooling)))
    ssrw <- as.numeric(crossprod(residuals(within)))
    stat <- (ssrp-ssrw)/ssrw/df1*df2
    names(stat) <- "F"
    parameter <- c(df1, df2)
    names(parameter) <- c("df1", "df2")
    pval <- pf(stat, df1, df2, lower.tail = FALSE)
    alternative <- "significant effects"
    res <- list(statistic   = stat,
                p.value     = pval,
                method      = paste("F test for ", effect, " effects", sep=""),
                parameter   = parameter,
                data.name   = data.name(x),
                alternative = alternative)
    class(res) <- "htest"
    res
}

############## pwaldtest() ############################################
# pwaldtest is used in summary.plm, summary.pht, summary.pgmm to compute the
# Chi-square or F statistic, but can be used as a stand-alone test of
# joint significance of all slopes
#
# Short intro (but see associated help file)
# arg 'vcov' non-NULL => the robust tests are carried out
# arg df2adj == TRUE does finite-sample/cluster adjustment for F tests's df2
# args .df1, .df2 are only there if user wants to do overwriting of dfs (user has final say)
#
# Chi-sq test for IV models as in Wooldridge (1990), A note on the Lagrange multiplier and F-statistics for two stage least
#                                                    squares regressions, Economics Letters 34: 151-155.

#' Wald-style Chi-square Test and F Test
#'
#' Wald-style Chi-square test and F test of slope coefficients being
#' zero jointly, including robust versions of the tests.
#'
#'
#' `pwaldtest` can be used stand--alone with a plm object, a pvcm object,
#' and a pgmm object (for pvcm objects only the 'random' type is valid and no
#' further arguments are processed; for pgmm objects only arguments `param`
#' and `vcov` are valid). It is also used in
#' [summary.plm()] to produce the F statistic and the Chi-square
#' statistic for the joint test of coefficients and in [summary.pgmm()].
#'
#' `pwaldtest` performs the test if the slope coefficients of a panel
#' regression are jointly zero. It does not perform general purpose
#' Wald-style tests (for those, see [lmtest::waldtest()] (from package
#' \CRANpkg{lmtest}) or [car::linearHypothesis()] (from package
#' \CRANpkg{car})).
#'
#' If a user specified variance-covariance matrix/function is given in
#' argument `vcov`, the robust version of the tests are carried out.
#' In that case, if the F test is requested (`test = "F"`) and no
#' overwriting of the second degrees of freedom parameter is given (by
#' supplying argument (`.df2`)), the adjustment of the second degrees
#' of freedom parameter is performed by default. The second degrees of
#' freedom parameter is adjusted to be the number of unique elements
#' of the cluster variable - 1, e. g., the number of individuals minus 1.
#' For the degrees of freedom adjustment of the F test in general,
#' see e. g. \insertCite{CAME:MILL:15;textual}{plm}, section VII;
#' \insertCite{ANDR:GOLS:SCMI:13}{plm}, pp. 126, footnote 4.
#'
#' The degrees of freedom adjustment requires the vcov object supplied
#' or created by a supplied function to carry an attribute called
#' "cluster" with a known clustering described as a character (for now
#' this could be either `"group"` or `"time"`). The vcovXX functions
#' of the package \pkg{plm} provide such an attribute for their
#' returned variance--covariance matrices. No adjustment is done for
#' unknown descriptions given in the attribute "cluster" or when the
#' attribute "cluster" is not present. Robust vcov objects/functions
#' from package \CRANpkg{clubSandwich} work as inputs to `pwaldtest`'s
#' F test because a they are translated internally to match the needs
#' described above.
#'
#' @aliases pwaldtest
#' @param x an estimated model of which the coefficients should be
#'     tested (usually of class `"plm"`/`"pvcm"`/`"pgmm"`)`,
#' @param test a character, indicating the test to be performed, may
#'     be either `"Chisq"` or `"F"` for the Wald-style
#'     Chi-square test or F test, respectively,
#' @param vcov `NULL` by default; a `matrix` giving a
#'     variance--covariance matrix or a function which computes such;
#'     if supplied (non `NULL`), the test is carried out using
#'     the variance--covariance matrix indicated resulting in a robust
#'     test,
#' @param df2adj logical, only relevant for `test = "F"`,
#'     indicating whether the adjustment for clustered standard errors
#'     for the second degrees of freedom parameter should be performed
#'     (see **Details**, also for further requirements regarding
#'     the variance--covariance matrix in `vcov` for the
#'     adjustment to be performed),
#' @param .df1 a numeric, used if one wants to overwrite the first
#'     degrees of freedom parameter in the performed test (usually not
#'     used),
#' @param .df2 a numeric, used if one wants to overwrite the second
#'     degrees of freedom parameter for the F test (usually not used),
#' @param param (for pgmm method only): select the parameters to be tested:
#'     `"coef"`, `"time"`, or `"all"``.
#' @param \dots further arguments (currently none).
#' @return An object of class `"htest"`, except for pvcm's within model for which
#'         a data.frame with results of the Wald chi-square tests and F tests per
#'         regression is returned.
#' @export
#' @author Yves Croissant (initial implementation) and Kevin Tappe
#'     (extensions: vcov argument and F test's df2 adjustment)
#' @seealso
#'
#' [vcovHC()] for an example of the vcovXX functions, a robust
#' estimation for the variance--covariance matrix; [summary.plm()]
#' @references
#'
#' \insertRef{WOOL:10}{plm}
#'
#' \insertRef{ANDR:GOLS:SCMI:13}{plm}
#'
#' \insertRef{CAME:MILL:15}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' mod_fe <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
#' mod_re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#' pwaldtest(mod_fe, test = "F")
#' pwaldtest(mod_re, test = "Chisq")
#'
#' # with robust vcov (matrix, function)
#' pwaldtest(mod_fe, vcov = vcovHC(mod_fe))
#' pwaldtest(mod_fe, vcov = function(x) vcovHC(x, type = "HC3"))
#'
#' pwaldtest(mod_fe, vcov = vcovHC(mod_fe), df2adj = FALSE) # w/o df2 adjustment
#'
#' # example without attribute "cluster" in the vcov
#' vcov_mat <- vcovHC(mod_fe)
#' attr(vcov_mat, "cluster") <- NULL  # remove attribute
#' pwaldtest(mod_fe, vcov = vcov_mat) # no df2 adjustment performed
#'
#'
pwaldtest <- function(x, ...) {
    UseMethod("pwaldtest")
}

#' @rdname pwaldtest
#' @export
pwaldtest.plm <- function(x, test = c("Chisq", "F"), vcov = NULL,
                          df2adj = (test == "F" && !is.null(vcov) && missing(.df2)), .df1, .df2, ...) {
    model <- describe(x, "model")
    test <- match.arg(test)
    df1 <- if(model == "within") length(coef(x)) else { length(coef(x)) - has.intercept(x) }
    df2 <- df.residual(x)
    #  tss <- tss(x)        # not good for models without intercept
    #  ssr <- deviance(x)   # -- " --
    vcov_arg <- vcov
    int <- "(Intercept)"
    coefs_wo_int <- coef(x)[!(names(coef(x)) %in% int)]
    if(!length(coefs_wo_int)) stop(paste("No non-intercept regressors in input model 'x',",
                                         "cannot perform Wald joint significance test"))
    # sanity check
    if (df2adj == TRUE && (is.null(vcov_arg) || test != "F")) {
        stop("df2adj == TRUE sensible only for robust F test, i.e., test == \"F\" and !is.null(vcov) and missing(.df2)")
    }

    # if robust test: prepare robust vcov
    if (!is.null(vcov_arg)) {
        if (is.matrix(vcov_arg))   rvcov <- rvcov_orig <- vcov_arg
        if (is.function(vcov_arg)) rvcov <- rvcov_orig <- vcov_arg(x)

        rvcov_name <- paste0(", vcov: ", paste0(deparse(substitute(vcov)))) # save "name" for later

        if (int %in% names(coef(x))) { # drop intercept, if present
            rvcov <- rvcov_orig[!rownames(rvcov_orig) %in% int, !colnames(rvcov_orig) %in% int]
            attr(rvcov, which = "cluster") <- attr(rvcov_orig, which = "cluster") # restore dropped 'cluster' attribute
        }
        # if robust F test: by default, do finite-sample adjustment for df2
        if (df2adj == TRUE && test == "F") {
            # determine the variable that the clustering is done on by
            # attribute "cluster" in the vcov (matrix object)
            # if only one member in cluster: fall back to original df2
            if (!is.null(attr(rvcov, which = "cluster"))) {

                # if supplied vcov is from package "clubSandwich": translate attr "cluster" to fit our code
                # (use rvcov_orig here for the test as the above dropping of the intercept drops the special classes of rvcov)
                if (inherits(rvcov_orig, "vcovCR")) rvcov <- trans_clubSandwich_vcov(CSvcov = rvcov, index = attr(model.frame(x), "index"))

                cluster <- attr(rvcov, which = "cluster")
                pdim <- pdim(x)
                df2 <- switch(cluster,
                              group = { if(pdim$nT$n == 1L) df2 else (pdim$nT$n - 1L) },
                              time  = { if(pdim$nT$T == 1L) df2 else (pdim$nT$T - 1L) },
                              # TODO: what about double clustering? vcovDC? vcovDC identifies itself as attr(obj, "cluster")="group-time")
                              # default:
                              { # warning("unknown/not implemented clustering, no df2 adjustment for finite-samples")
                                  df2}
                )
            } else {
                # no information on clustering found, do not adjust df2
                # (other options would be: assume cluster = "group", or fall-back to non robust statistics (set vcov_arg <- NULL))
                warning("no attribute 'cluster' in robust vcov found, no finite-sample adjustment for df2") # assuming cluster = \"group\"")
                # df2 <- as.integer(pdim(x)$nT$n - 1) # assume cluster = "group"
            }
        }
    }

    # final say: overwrite Dfs if especially supplied
    if (!missing(.df1)) df1 <- .df1
    if (!missing(.df2)) df2 <- .df2

    if (test == "Chisq"){
        # perform non-robust chisq test
        if (is.null(vcov_arg)) {
            names.coefs_wo_int <- names(coefs_wo_int)
            stat <- as.numeric(crossprod(solve(vcov(x)[names.coefs_wo_int, names.coefs_wo_int], coefs_wo_int), coefs_wo_int))
            #     stat < - (tss-ssr)/(ssr/df2) # does not produce correct results for unbalanced RE models and (un)balanced IV models
            names(stat) <- "Chisq"
            pval <- pchisq(stat, df = df1, lower.tail = FALSE)
            parameter <- c(df = df1)
            method <- "Wald test for joint significance"
        } else {
            # perform robust chisq test
            stat <- as.numeric(crossprod(solve(rvcov, coefs_wo_int), coefs_wo_int))
            names(stat) <- "Chisq"
            pval <- pchisq(stat, df = df1, lower.tail = FALSE)
            parameter <- c(df = df1)
            method <- paste0("Wald test for joint significance (robust)", rvcov_name)
        }
    }
    if (test == "F"){
        if(length(formula(x))[2L] > 1L) stop("test = \"F\" not sensible for IV models")
        if (is.null(vcov_arg)) {
            # perform "normal" F test
            names.coefs_wo_int <- names(coefs_wo_int)
            stat <- as.numeric(crossprod(solve(vcov(x)[names.coefs_wo_int, names.coefs_wo_int], coefs_wo_int), coefs_wo_int)) / df1
            #      stat <- (tss-ssr)/ssr*df2/df1 # does not produce correct results for unbalanced RE models
            names(stat) <- "F"
            pval <- pf(stat, df1 = df1, df2 = df2, lower.tail = FALSE)
            parameter <- c(df1 = df1, df2 = df2)
            method <- "F test for joint significance"
        } else {
            # perform robust F test
            stat <- as.numeric(crossprod(solve(rvcov, coefs_wo_int), coefs_wo_int) / df1)
            names(stat) <- "F"
            pval <- pf(stat, df1 = df1, df2 = df2, lower.tail = FALSE)
            parameter <- c(df1 = df1, df2 = df2)
            method  <- paste0("F test for joint significance (robust)", rvcov_name)
        }
    }
    res <- list(data.name = data.name(x),
                statistic = stat,
                parameter = parameter,
                p.value   = pval,
                method    = method,
                alternative = "at least one coefficient is not null"
    )
    class(res) <- "htest"
    return(res)
}

#' @rdname pwaldtest
#' @export
pwaldtest.pvcm <- function(x, ...) {
    model <- describe(x, "model")
    effect <- describe(x, "effect")

    coefs.no.int <- !names(x$coefficients) %in% "(Intercept)" # logical with non-intercept regressors set to TRUE
    if(!length(names(x$coefficients)[coefs.no.int])) {
        # error informatively if only-intercept model (no other regressors)
        stop(paste("No non-intercept regressors in model(s) of input 'x',",
                   "cannot perform Wald joint significance test(s)"))
    }

    if(model == "within") {
        # for the within case, simply return a data.frame with all test results
        # of single estimations (per individual or per time period)

        ii <- switch(effect, "individual" = 1L, "time" = 2L)
        residl <- split(x$residuals, unclass(index(x))[[ii]])

        # vcovs and coefficients w/o intercept
        vcovl <- lapply(x$vcov, function(x) x[coefs.no.int, coefs.no.int])
        coefl <- as.list(data.frame(t(x$coefficients[ , coefs.no.int, drop = FALSE])))
        df1 <- ncol(x$coefficients[ , coefs.no.int, drop = FALSE]) # ncol is same df1 for all models (as all models estimate the same coefs)
        df2 <- lengths(residl) - ncol(x$coefficients) # (any intercept is subtracted)

        statChisqs <- mapply(FUN = function(v, c) as.numeric(crossprod(solve(v, c), c)),
                             vcovl, coefl)
        statFs <- statChisqs / df1

        pstatChisqs <- pchisq(statChisqs, df = df1, lower.tail = FALSE)
        pstatFs <- pf(statFs, df1 = df1, df2 = df2, lower.tail = FALSE)

        stats.pvcm.within <- as.data.frame(cbind("Chisq"    = statChisqs,
                                                 "p(chisq)" = pstatChisqs,
                                                 "F"        = statFs,
                                                 "p(F)"     = pstatFs,
                                                 "df1"      = rep(df1, length(residl)),
                                                 "df2"      = df2))
        # early return
        return(stats.pvcm.within)
    }

    ## case: model == "random"
    coefs_wo_int <- x$coefficients[coefs.no.int]
    stat <- as.numeric(crossprod(solve(vcov(x)[coefs.no.int, coefs.no.int], coefs_wo_int), coefs_wo_int))
    names(stat) <- "Chisq"
    df1 <- length(coefs_wo_int)
    pval <- pchisq(stat, df = df1, lower.tail = FALSE)
    parameter <- c(df = df1)
    method <- "Wald test for joint significance"

    res <- list(data.name = data.name(x),
                statistic = stat,
                parameter = parameter,
                p.value   = pval,
                method    = method,
                alternative = "at least one coefficient is not null"
    )
    class(res) <- "htest"
    return(res)
}


#' @rdname pwaldtest
#' @export
pwaldtest.pgmm <- function(x, param = c("coef", "time", "all"), vcov = NULL, ...) {
    param <- match.arg(param)
    vcov_supplied <- !is.null(vcov)
    myvcov <- vcov
    if (is.null(vcov)) vv <- vcov(x)
    else if (is.function(vcov)) vv <- myvcov(x)
    else vv <- myvcov

    model <- describe(x, "model")
    effect <- describe(x, "effect")
    if (param == "time" && effect == "individual") stop("no time dummies in this model")
    transformation <- describe(x, "transformation")
    coefficients <- if(model == "onestep") x$coefficients else x$coefficients[[2L]]
    Ktot <- length(coefficients)
    Kt <- length(x$args$namest)

    switch(param,
           "time" = {
               start <- Ktot - Kt + if(transformation == "ld") 2 else 1
               end <- Ktot
           },
           "coef" = {
               start <- 1
               end <- if (effect == "twoways") Ktot - Kt else Ktot
           },
           "all" = {
               start <- 1
               end <- Ktot
           })
    coef <- coefficients[start:end]
    vv <- vv[start:end, start:end]
    stat <- as.numeric(crossprod(coef, crossprod(solve(vv), coef)))
    names(stat) <- "chisq"
    parameter <- length(coef)
    names(parameter) <- "df"
    pval <- pchisq(stat, df = parameter, lower.tail = FALSE)
    method <- "Wald test for joint significance"
    if (vcov_supplied) {
        rvcov_name <- paste0(", vcov: ", paste0(deparse(substitute(vcov))))
        method <- paste0(method, " (robust)", rvcov_name)
    }
    wald.pgmm <- list(statistic = stat,
                      p.value   = pval,
                      parameter = parameter,
                      method    = method,
                      alternative = "at least one coefficient is not null",
                      data.name = data.name(x))
    class(wald.pgmm) <- "htest"
    return(wald.pgmm)
}

pwaldtest.default <- function(x, ...) {
    pwaldtest.plm(x, ...)
}


# trans_clubSandwich_vcov: helper function for pwaldtest()
# translate vcov object from package clubSandwich so it is suitable for summary.plm, plm's pwaldtest.
# Attribute "cluster" in clubSandwich's vcov objects contains the cluster variable itself.
# plm's vcov object also has attribute "cluster" but it contains a character as
# information about the cluster dimension (either "group" or "time")
#
# inputs:
#   * CSvcov: a vcov as returned by clubSandwich's vcovCR function [class c("vcovCR", "clubSandwich")]
#   * index: the index belonging to a plm object/model
# return value:
#   * modified CSvcov (substituted attribute "cluster" with suitable character or NULL)
trans_clubSandwich_vcov <- function(CSvcov, index) {
    clustervar <- attr(CSvcov, "cluster")
    if (!is.null(clustervar)) {
        if (isTRUE(all.equal(index[[1L]], clustervar))) {
            attr(CSvcov, "cluster") <- "group"
            return(CSvcov)
        }
        if (isTRUE(all.equal(index[[2L]], clustervar))) {
            attr(CSvcov, "cluster") <- "time"
            return(CSvcov)
        } else {
            attr(CSvcov, "cluster") <- NULL
            return(CSvcov)
        }
    }
    warning("no attribute \"cluster\" found in supplied vcov object")
    return(CSvcov)
}



#' Test of Poolability
#'
#' A Chow test for the poolability of the data.
#'
#' `pooltest` is a *F* test of stability (or Chow test) for the
#' coefficients of a panel model. For argument `x`, the estimated
#' `plm` object should be a `"pooling"` model or a `"within"` model
#' (the default); intercepts are assumed to be identical in the first
#' case and different in the second case.
#'
#' @aliases pooltest
#' @param x an object of class `"plm"` for the plm method; an object of
#' class `"formula"` for the formula interface,
#' @param z an object of class `"pvcm"` obtained with
#' `model="within"`,
#' @param data a `data.frame`,
#' @param \dots further arguments passed to plm.
#' @return An object of class `"htest"`.
#' @export
#' @author Yves Croissant
#' @keywords htest
#' @examples
#'
#' data("Gasoline", package = "plm")
#' form <- lgaspcar ~ lincomep + lrpmg + lcarpcap
#' gasw <- plm(form, data = Gasoline, model = "within")
#' gasp <- plm(form, data = Gasoline, model = "pooling")
#' gasnp <- pvcm(form, data = Gasoline, model = "within")
#' pooltest(gasw, gasnp)
#' pooltest(gasp, gasnp)
#'
#' pooltest(form, data = Gasoline, effect = "individual", model = "within")
#' pooltest(form, data = Gasoline, effect = "individual", model = "pooling")
#'
pooltest <- function(x,...){
    UseMethod("pooltest")
}


#' @rdname pooltest
#' @export
pooltest.plm <- function(x, z, ...){
    rss <- deviance(x)
    uss <- as.numeric(crossprod(residuals(z)))
    dlr <- df.residual(x)
    dlu <- df.residual(z)
    df1 <- dlr - dlu
    df2 <- dlu
    stat <- (rss-uss)/uss*df2/df1
    pval <- pf(stat, df1 = df1, df2 = df2, lower.tail = FALSE)
    parameter <- c(df1 = df1, df2 = df2)
    names(stat) <- "F"
    res <- list(statistic   = stat,
                parameter   = parameter,
                p.value     = pval,
                data.name   = data.name(x),
                alternative = "unstability",
                method      = "F statistic")
    class(res) <- "htest"
    res
}

#' @rdname pooltest
#' @export
pooltest.formula <- function(x, data, ...){
    cl <- match.call(expand.dots = TRUE)
    cl[[1L]] <- as.name("plm")
    names(cl)[[2L]] <- "formula"
    if (is.null(cl$effect)) cl$effect <- "individual"
    plm.model <- eval(cl, parent.frame())

    cl[[1L]] <- as.name("pvcm")
    names(cl)[[2L]] <- "formula"
    if (is.null(cl$effect)) cl$effect <- "individual"
    cl$model <- "within"
    pvcm.model <- eval(cl, parent.frame())

    pooltest(plm.model, pvcm.model)
}


# test_granger.R#
### Panel Granger (Non-)Causality Test
##
## Reference:
##   * Dumitrescu, Elena-Ivona/Hurlin, Christophe (2012), Testing for Granger non-causality in heterogeneous panels,
##                                                        Economic Modelling, 29(4), pp. 1450-460.
##   * supplements (test data, MATLAB code): http://www.runmycode.org/companion/view/42
##
##   * Lopez, Luciano/Weber, Sylvain (2017), Testing for Granger causality in panel data,
##                                          The Stata Journal, Vol 17, Issue 4, pp. 972-984.
##      * Working paper: Testing for Granger causality in panel data,
##                        IRENE Working paper 17-03, September 11, 2017
##      * supplements (xtgcause for Stata) https://ideas.repec.org/c/boc/bocode/s458308.html
##
##   * EViews blog with introduction to the test and a Monte Carlo study:
##     http://blog.eviews.com/2017/08/dumitrescu-hurlin-panel-granger.html
##
## TODO (if someone is willing...)
##  * Lopez/Weber (2017) also demonstrate lag selection procedure by AIC, BIC, ...
##




#' Panel Granger (Non-)Causality Test (Dumitrescu/Hurlin (2012))
#'
#' Test for Granger (non-)causality in panel data.
#'
#'
# % TODO: write about assumptions of panel Granger test: % * cross-sectional
# independence % * convergence
#'
#' The panel Granger (non-)causality test is a combination of Granger
#' tests \insertCite{GRAN:69}{plm} performed per individual. The test
#' is developed by \insertCite{DUMI:HURL:12;textual}{plm}, a shorter
#' exposition is given in \insertCite{LOPE:WEBE:17;textual}{plm}.
#'
#' The formula `formula` describes the direction of the (panel) Granger
#' causation where `y ~ x` means "x (panel) Granger causes y".
#'
#' By setting argument `test` to either `"Ztilde"` (default) or
#' `"Zbar"`, two different statistics can be requested. `"Ztilde"`
#' gives the standardised statistic recommended by Dumitrescu/Hurlin (2012) for
#' fixed T samples. If set to `"Wbar"`, the intermediate Wbar statistic
#' (average of individual Granger chi-square statistics) is given which is used
#' to derive the other two.
#'
#' The Zbar statistic is not suitable for unbalanced panels. For the Wbar
#' statistic, no p-value is available.
#'
#' The implementation uses [lmtest::grangertest()] from
#' package \CRANpkg{lmtest} to perform the individual Granger tests.
#'
#' @param formula a `formula` object to describe the direction of the
#'     hypothesized Granger causation,
#' @param data a `pdata.frame` or a `data.frame`,
#' @param test a character to request the statistic to be returned,
#'     either `"Ztilde"` (default),or `"Zbar"`, alternatively, set to
#'      `"Wbar"` for an intermediate statistic (see Details),
#' @param order integer(s) giving the number of lags to include in the
#'     test's auxiliary regressions, the length of order must be
#'     either 1 (same lag order for all individuals) or equal to the
#'     number of individuals (to specify a lag order per individual),
#' @param index only relevant if `data` is `data.frame` and not a
#'     `pdata.frame`; if `NULL`, the first two columns of the
#'     data.frame are assumed to be the index variables, for further
#'     details see [pdata.frame()].
#' @return An object of class `c("pgrangertest", "htest")`. Besides
#'     the usual elements of a `htest` object, it contains the data
#'     frame `indgranger` which carries the Granger test statistics
#'     per individual along the associated p-values, degrees of
#'     freedom, and the specified lag order.
#' @export
#' @author Kevin Tappe
#' @seealso [lmtest::grangertest()] for the original (non-panel)
#'     Granger causality test in \CRANpkg{lmtest}.
#' @references
#'
#' \insertRef{DUMI:HURL:12}{plm}
#'
#' \insertRef{GRAN:69}{plm}
#'
#' \insertRef{LOPE:WEBE:17}{plm}
#'
#' @keywords htest
#' @examples
#'
#' ## not meaningful, just to demonstrate usage
#' ## H0: 'value' does not Granger cause 'inv' for all invididuals
#'
#' data("Grunfeld", package = "plm")
#' pgrangertest(inv ~ value, data = Grunfeld)
#' pgrangertest(inv ~ value, data = Grunfeld, order = 2L)
#' pgrangertest(inv ~ value, data = Grunfeld, order = 2L, test = "Zbar")
#'
#' # varying lag order (last individual lag order 3, others lag order 2)
#' (pgrt <- pgrangertest(inv ~ value, data = Grunfeld, order = c(rep(2L, 9), 3L)))
#' # chisq statistics per individual
#' pgrt$indgranger
#'
pgrangertest <- function(formula, data, test = c("Ztilde", "Zbar", "Wbar"), order = 1L, index = NULL) {
    # Implementation of formulae follows Lopez/Weber (2017), the formulas are slightly different
    # compared to Dumistrescu/Hurlin (2012), because "Note however that T in DH's formulae
    # must be understood as the number of observations remaining in the estimations, that
    # is the number of periods minus the number of lags included. In order to be consistent
    # with our notation, we therefore replaced DH's T by T - K in the following formulas of
    # the present paper."

    # y ~ x: to test whether x (panel-)Granger causes y

    test <- match.arg(test)
    if (!inherits(data, "pdata.frame")) data <- pdata.frame(data, index = index)

    pdim <- pdim(data)
    balanced <- pdim$balanced
    N  <- pdim$nT$n
    T. <- pdim$nT$T
    Ti <- pdim$Tint$Ti
    indi <- unclass(index(data))[[1L]]
    indi_con <- is.pconsecutive(data)

    # some input checks
    if (!inherits(formula, "formula") || length(all.vars(formula)) > 2L) {
        stop(paste0("Argument 'formula' must be of class \"formula\" and may not contain ",
                    "more than 2 variables, one LHS and one RHS variable, e.g., 'y ~ x'"))
    }

    if (!(is.numeric(order) && all(round(order) == order) && all(order > 0L)))
        stop("Lag order 'order' must contain positive integer(s)")

    if (length(order) > 1L && length(order) != N) stop("'order' must have length 1 or the number of individuals")
    if (test == "Zbar" && !balanced) stop("'test = \"Zbar\"' is not suited for unbalanced panels")
    if (test == "Zbar" && length(unique(order)) != 1L) stop("'test = \"Zbar\"' is not suited for varying lag order")

    # For statistic Ztilde, the second order moments of the individual statistics must exist
    # (formula (10) in Dumitrescu/Hurlin (2012) where T = T - K)
    req.obs <- 5L + 3L*order
    if (length(order) == 1L) {
        if (test == "Ztilde" && !all((Ti > (req.obs)))) {
            stop(paste0("Condition for test = \"Ztilde\" not met for all individuals: length of time series ",
                        "must be larger than 5+3*order (>5+3*", order, "=", req.obs, ")"))
        }
    } else {
        if (test == "Ztilde" && !all((Ti > (req.obs)))) {
            stop(paste0("Condition for test = \"Ztilde\" not met for all individuals: length of time series ",
                        "must be larger than 5+3*order [where order is the order specified for the individuals]"))
        }
    }

    # give warning if data is not consecutive per individual
    if (!all(indi_con)) {
        indnames <- pdim[["panel.names"]][["id.names"]]
        wrn1 <- "pgrangertest: result may be unreliable due to individuals with non-consecutive time periods: "
        wrn2 <- if (sum(!indi_con) <= 5L) {
            paste0(indnames[!indi_con], collapse = ", ")
        }
        else { # cut off enumeration of individuals in warning message if more than 5
            breakpoint <- which(cumsum(!indi_con) == 5L)[1L]
            paste0(paste0(indnames[1L:breakpoint][!indi_con[1L:breakpoint]], collapse = ", "), ", ...")
        }
        wrn <- paste0(wrn1, wrn2)
        warning(wrn)
    }

    listdata <- split(data, indi) # split data per individual


    ## use lmtest::grangertest for the individual Granger tests

    # for this, if necessary, expand order argument for lmtest::grangertest to full length (N)
    # [but leave variable 'order' in its current length for later decision making]
    order_grangertest <- if(length(order) == 1L) rep(order, N) else order

    # Dumitrescu/Hurlin (2012), p. 1453 use the Chisq definition of the Granger test
    grangertests_i <- mapply(function(data, order)
        lmtest::grangertest(formula, data = data,
                            order = order, test = "Chisq"),
        listdata, order_grangertest, SIMPLIFY = FALSE)

    # extract Wald/Chisq-statistics and p-values of individual Granger tests
    Wi   <- vapply(grangertests_i, function(g) g[["Chisq"]][2L],        FUN.VALUE = 0.0, USE.NAMES = FALSE)
    pWi  <- vapply(grangertests_i, function(g) g[["Pr(>Chisq)"]][[2L]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
    dfWi <- vapply(grangertests_i, function(g) abs(g[["Df"]][2L]),      FUN.VALUE = 0.0, USE.NAMES = FALSE)

    Wbar <- c("Wbar" = mean(Wi))

    if(test == "Zbar") {
        stat <- c(sqrt(N/(2*order)) * (Wbar - order))
        names(stat) <- "Zbar"
        pval <- 2*pnorm(abs(stat), lower.tail = FALSE)
    }

    if(test == "Ztilde") {
        # Ztilde recommended for fixed T
        if (balanced && length(order) == 1L) {
            stat <- c(  sqrt( N/(2*order) * (T. - 3*order - 5) / (T. - 2*order - 3) )
                        * ( (T. - 3*order - 3) / (T. - 3*order - 1) * Wbar - order))
        } else {
            # unbalanced and/or varying lag order
            # unbal stat reduces to the balanced case for balanced data but rather treat it separately here
            # formula (33) in Dumitrescu/Hurlin (2012), p. 1459
            if (length(order) == 1L) order <- rep(order, N) # replicate lag order for all individuals
            stat <- c(   sqrt(N) * ( Wbar - 1/N * sum( order * (Ti - 3*order - 1) / (Ti - 3*order - 3) ))
                         * 1/sqrt( 1/N * sum( 2* order * ((Ti - 3*order - 1)^2 * (Ti - 2*order - 3)) /
                                                  ((Ti - 3*order - 3)^2 * (Ti - 3*order - 5)) ) ) )
        }
        names(stat) <- "Ztilde"
        pval <- 2*pnorm(abs(stat), lower.tail = FALSE)
    }

    if(test == "Wbar") {
        stat <- Wbar
        names(stat) <- "Wbar"
        pval <- NULL
    }

    # make data frame with individual Granger test results and lag order
    indgranger <- data.frame(indi[!duplicated(indi)],
                             Wi, pWi, dfWi,
                             (if(length(order) == 1L) rep(order, N) else order))
    colnames(indgranger) <- c(names(index(data))[1L], "Chisq", "p-value", "df", "lag")

    RVAL <- list(statistic = stat,
                 parameter = NULL,
                 p.value   = pval,
                 method = "Panel Granger (Non-)Causality Test (Dumitrescu/Hurlin (2012))",
                 alternative = "Granger causality for at least one individual",
                 data.name = deparse(formula),
                 indgranger = indgranger)
    class(RVAL) <- c("pgrangertest", "htest")

    return(RVAL)
}


# test_serial.R#
#' Breusch--Godfrey Test for Panel Models
#'
#' Test of serial correlation for (the idiosyncratic component of) the
#' errors in panel models.
#'
#' This Lagrange multiplier test uses the auxiliary model on
#' (quasi-)demeaned data taken from a model of class `plm` which may
#' be a `pooling` (default for formula interface), `random` or
#' `within` model. It performs a Breusch--Godfrey test (using `bgtest`
#' from package \CRANpkg{lmtest} on the residuals of the
#' (quasi-)demeaned model, which should be serially uncorrelated under
#' the null of no serial correlation in idiosyncratic errors, as
#' illustrated in \insertCite{WOOL:10;textual}{plm}. The function
#' takes the demeaned data, estimates the model and calls `bgtest`.
#'
#' Unlike most other tests for serial correlation in panels, this one
#' allows to choose the order of correlation to test for.
#'
#' @aliases pbgtest
#' @importFrom lmtest bgtest
#' @param x an object of class `"panelmodel"` or of class `"formula"`,
#' @param order an integer indicating the order of serial correlation
#'     to be tested for. `NULL` (default) uses the minimum number of
#'     observations over the time dimension (see also section
#'     **Details** below),
#' @param type type of test statistic to be calculated; either
#'     `"Chisq"` (default) for the Chi-squared test statistic or `"F"`
#'     for the F test statistic,
#' @param data only relevant for formula interface: data set for which
#'     the respective panel model (see `model`) is to be evaluated,
#' @param model only relevant for formula interface: compute test
#'     statistic for model `pooling` (default), `random`, or `within`.
#'     When `model` is used, the `data` argument needs to be passed as
#'     well,
#' @param \dots further arguments (see [lmtest::bgtest()]).
#' @return An object of class `"htest"`.
#' @note The argument `order` defaults to the minimum number of
#'     observations over the time dimension, while for
#'     `lmtest::bgtest` it defaults to `1`.
#' @export
#' @author Giovanni Millo
#' @seealso For the original test in package \CRANpkg{lmtest} see
#'     [lmtest::bgtest()].  See [pdwtest()] for the analogous
#'     panel Durbin--Watson test.  See [pbltest()], [pbsytest()],
#'     [pwartest()] and [pwfdtest()] for other serial correlation
#'     tests for panel models.
#' @references
#'
#' \insertRef{BREU:78}{plm}
#'
#' \insertRef{GODF:78}{plm}
#'
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#'
#' \insertRef{WOOL:13}{plm}
#'  Sec. 12.2, pp. 421--422.
#' @keywords htest
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' g <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#'
#' # panelmodel interface
#' pbgtest(g)
#' pbgtest(g, order = 4)
#'
#' # formula interface
#' pbgtest(inv ~ value + capital, data = Grunfeld, model = "random")
#'
#' # F test statistic (instead of default type="Chisq")
#' pbgtest(g, type="F")
#' pbgtest(inv ~ value + capital, data = Grunfeld, model = "random", type = "F")
#'
pbgtest <- function (x, ...) {
    UseMethod("pbgtest")
}

#' @rdname pbgtest
#' @export
pbgtest.panelmodel <- function(x, order = NULL, type = c("Chisq", "F"), ...) {
    ## residual serial correlation test based on the residuals of the demeaned
    ## model (see Wooldridge (2002), p. 288) and the regular lmtest::bgtest()

    ## structure:
    ## 1: take demeaned data from 'plm' object
    ## 2: est. auxiliary model by OLS on demeaned data
    ## 3: apply lmtest::bgtest() to auxiliary model and return the result

    model <- describe(x, "model")
    effect <- describe(x, "effect")
    theta <- x$ercomp$theta

    ## retrieve demeaned data
    demX <- model.matrix(x, model = model, effect = effect, theta = theta, cstcovar.rm = "all")
    demy <- pmodel.response(model.frame(x), model = model, effect = effect, theta = theta)
    ## ...and group numerosities
    Ti <- pdim(x)$Tint$Ti
    ## set lag order to minimum group numerosity if not specified by user
    ## (check whether this is sensible)
    if(is.null(order)) order <- min(Ti)

    ## lmtest::bgtest on the demeaned model:

    ## pbgtest is the return value of lmtest::bgtest, exception made for the method attribute
    auxformula <- demy ~ demX - 1
    lm.mod <- lm(auxformula)
    bgtest <- bgtest(lm.mod, order = order, type = type, ...)
    bgtest$method <- "Breusch-Godfrey/Wooldridge test for serial correlation in panel models"
    bgtest$alternative <- "serial correlation in idiosyncratic errors"
    bgtest$data.name <- data.name(x)
    names(bgtest$statistic) <- if(length(bgtest$parameter) == 1) "chisq" else "F"
    return(bgtest)
}

#' @rdname pbgtest
#' @export
pbgtest.formula <- function(x, order = NULL, type = c("Chisq", "F"), data, model=c("pooling", "random", "within"), ...) {
    ## formula method for pbgtest;
    ## defaults to a pooling model
    cl <- match.call(expand.dots = TRUE)
    if (names(cl)[3L] == "") names(cl)[3L] <- "data"
    if (is.null(cl$model)) cl$model <- "pooling"
    names(cl)[2L] <- "formula"
    m <- match(plm.arg, names(cl), 0)
    cl <- cl[c(1L, m)]
    cl[[1L]] <- quote(plm)
    plm.model <- eval(cl,parent.frame())
    pbgtest(plm.model, order = order, type = type, data = data, ...)
}

#' Wooldridge's Test for Unobserved Effects in Panel Models
#'
#' Semi-parametric test for the presence of (individual or time) unobserved
#' effects in panel models.
#'
#' This semi-parametric test checks the null hypothesis of zero
#' correlation between errors of the same group. Therefore, it has
#' power both against individual effects and, more generally, any kind
#' of serial correlation.
#'
#' The test relies on large-N asymptotics. It is valid under error
#' heteroskedasticity and departures from normality.
#'
#' The above is valid if `effect="individual"`, which is the most
#' likely usage. If `effect="time"`, symmetrically, the test relies on
#' large-T asymptotics and has power against time effects and, more
#' generally, against cross-sectional correlation.
#'
#' If the panelmodel interface is used, the inputted model must be a pooling
#' model.
#'
#' @aliases pwtest
#' @param x an object of class `"formula"`, or an estimated model of class
#' `panelmodel`,
#' @param effect the effect to be tested for, one of `"individual"`
#' (default) or `"time"`,
#' @param data a `data.frame`,
#' @param \dots further arguments passed to `plm`.
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo
#' @seealso [pbltest()], [pbgtest()],
#' [pdwtest()], [pbsytest()], [pwartest()],
#' [pwfdtest()] for tests for serial correlation in panel models.
#' [plmtest()] for tests for random effects.
#' @references
#'
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("Produc", package = "plm")
#' ## formula interface
#' pwtest(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc)
#' pwtest(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, effect = "time")
#'
#' ## panelmodel interface
#' # first, estimate a pooling model, than compute test statistics
#' form <- formula(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp)
#' pool_prodc <- plm(form, data = Produc, model = "pooling")
#' pwtest(pool_prodc) # == effect="individual"
#' pwtest(pool_prodc, effect="time")
#'
pwtest <- function(x, ...){
    UseMethod("pwtest")
}

#' @rdname pwtest
#' @export
pwtest.formula <- function(x, data, effect = c("individual", "time"), ...) {

    effect <- match.arg(effect, choices = c("individual", "time")) # match effect to pass it on to pwtest.panelmodel

    cl <- match.call(expand.dots = TRUE)
    if (names(cl)[3] == "") names(cl)[3] <- "data"
    if (is.null(cl$model)) cl$model <- "pooling"
    if (cl$model != "pooling") stop("pwtest only relevant for pooling models")
    names(cl)[2] <- "formula"
    m <- match(plm.arg, names(cl), 0)
    cl <- cl[c(1L, m)]
    cl[[1L]] <- quote(plm)
    plm.model <- eval(cl,parent.frame())
    pwtest.panelmodel(plm.model, effect = effect, ...) # pass on desired 'effect' argument to pwtest.panelmodel

    ## "RE" test a la Wooldridge (2002/2010), see 10.4.4
    ## (basically the scaled and standardized estimator for sigma from REmod)
    ## does not rely on normality or homoskedasticity;
    ## H0: composite errors uncorrelated

    ## ref. Wooldridge (2002), pp. 264-265; Wooldridge (2010), pp. 299-300

    ######### from here generic testing interface from
    ######### plm to my code
}

#' @rdname pwtest
#' @export
pwtest.panelmodel <- function(x, effect = c("individual", "time"), ...) {
    if (describe(x, "model") != "pooling") stop("pwtest only relevant for pooling models")
    effect <- match.arg(effect, choices = c("individual", "time"))
    data <- model.frame(x)
    ## extract indices

    ## if effect="individual" std., else swap
    xindex <- unclass(attr(data, "index")) # unclass for speed
    if (effect == "individual"){
        index  <- xindex[[1L]]
        tindex <- xindex[[2L]]
    }
    else{
        index  <- xindex[[2L]]
        tindex <- xindex[[1L]]
    }
    ## det. number of groups and df
    n <- length(unique(index))
    X <- model.matrix(x)

    k <- ncol(X)
    ## det. total number of obs. (robust vs. unbalanced panels)
    nT <- nrow(X)
    ## det. max. group numerosity
    t <- max(tapply(X[ , 1L], index, length))

    ## ref. Wooldridge (2002), p.264 / Wooldridge (2010), p.299

    ## extract resids
    u <- x$residuals

    ## est. random effect variance
    ## "pre-allocate" an empty list of length n
    tres <- vector("list", n)

    ## list of n "empirical omega-blocks"
    ## with averages of xproducts of t(i) residuals
    ## for each group 1..n
    ## (possibly different sizes if unbal., thus a list
    ## and thus, unlike Wooldridge (eq.10.37), we divide
    ## every block by *its* t(t-1)/2)
    unind <- unique(index) # ????

    for(i in 1:n) {
        ut <- u[index == unind[i]]
        tres[[i]] <- ut %o% ut
    }

    ## det. # of upper triangle members (n*t(t-1)/2 if balanced)
    ## no needed, only for illustration
    # ti <- vapply(tres, function(x) dim(x)[[1L]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
    # uptrinum <- sum(ti*(ti-1)/2)

    ## sum over all upper triangles of emp. omega blocks:
    ## and sum over resulting vector (df corrected)
    sum.uptri <- vapply(tres, function(x) sum(x[upper.tri(x, diag = FALSE)]), FUN.VALUE = 0.0, USE.NAMES = FALSE)
    W <- sum(sum.uptri) # /sqrt(n) simplifies out

    ## calculate se(Wstat) as in 10.40
    seW <- sqrt(as.numeric(crossprod(sum.uptri)))

    ## NB should we apply a df correction here, maybe that of the standard
    ## RE estimator? (see page 261)

    Wstat <- W/seW
    names(Wstat) <- "z"
    pW <- 2*pnorm(abs(Wstat), lower.tail = FALSE) # unlike LM, test is two-tailed!

    ## insert usual htest features
    RVAL <- list(statistic   = Wstat,
                 parameter   = NULL,
                 method      = paste("Wooldridge's test for unobserved",
                                     effect, "effects"),
                 alternative = "unobserved effect",
                 p.value     = pW,
                 data.name   = paste(deparse(substitute(formula))))
    class(RVAL) <- "htest"
    return(RVAL)
}

#' Wooldridge Test for AR(1) Errors in FE Panel Models
#'
#' Test of serial correlation for (the idiosyncratic component of) the errors
#' in fixed--effects panel models.
#'
#' As \insertCite{WOOL:10;textual}{plm}, Sec. 10.5.4 observes, under
#' the null of no serial correlation in the errors, the residuals of a
#' FE model must be negatively serially correlated, with
#' \eqn{cor(\hat{u}_{it}, \hat{u}_{is})=-1/(T-1)} for each
#' \eqn{t,s}. He suggests basing a test for this null hypothesis on a
#' pooled regression of FE residuals on their first lag:
#' \eqn{\hat{u}_{i,t} = \alpha + \delta \hat{u}_{i,t-1} +
#' \eta_{i,t}}. Rejecting the restriction \eqn{\delta = -1/(T-1)}
#' makes us conclude against the original null of no serial
#' correlation.
#'
#' `pwartest` estimates the `within` model and retrieves residuals,
#' then estimates an AR(1) `pooling` model on them. The test statistic
#' is obtained by applying a F test to the latter model to test the
#' above restriction on \eqn{\delta}, setting the covariance matrix to
#' `vcovHC` with the option `method="arellano"` to control for serial
#' correlation.
#'
#' Unlike the [pbgtest()] and [pdwtest()], this test does
#' not rely on large--T asymptotics and has therefore good properties in
#' ``short'' panels.  Furthermore, it is robust to general heteroskedasticity.
#'
#' @aliases pwartest
#' @param x an object of class `formula` or of class `panelmodel`,
#' @param data a `data.frame`,
#' @param \dots further arguments to be passed on to `vcovHC` (see
#'     Details and Examples).
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo
#' @seealso [pwfdtest()], [pdwtest()], [pbgtest()], [pbltest()],
#'     [pbsytest()].
#' @references
#'
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("EmplUK", package = "plm")
#' pwartest(log(emp) ~ log(wage) + log(capital), data = EmplUK)
#'
#' # pass argument 'type' to vcovHC used in test
#' pwartest(log(emp) ~ log(wage) + log(capital), data = EmplUK, type = "HC3")
#'
#'
pwartest <- function(x, ...) {
    UseMethod("pwartest")
}

#' @rdname pwartest
#' @export
pwartest.formula <- function(x, data, ...) {
    ## small-sample serial correlation test for FE models
    ## ref.: Wooldridge (2002/2010) 10.5.4

    cl <- match.call(expand.dots = TRUE)
    if (is.null(cl$model)) cl$model <- "within"
    if (cl$model != "within") stop("pwartest only relevant for within models")
    if (names(cl)[3L] == "") names(cl)[3L] <- "data"
    names(cl)[2L] <- "formula"
    m <- match(plm.arg, names(cl), 0)
    cl <- cl[c(1L, m)]
    cl[[1L]] <- quote(plm)
    plm.model <- eval(cl, parent.frame())
    pwartest(plm.model, ...)
}

#' @rdname pwartest
#' @export
pwartest.panelmodel <- function(x, ...) {

    if (describe(x, "model") != "within") stop("pwartest only relevant for within models")

    FEres <- x$residuals
    data <- model.frame(x)

    ## this is a bug fix for incorrect naming of the "data" attr.
    ## for the pseries in pdata.frame()

    attr(FEres, "data") <- NULL
    N <- length(FEres)
    FEres.1 <- c(NA, FEres[1:(N-1)])
    xindex <- unclass(attr(data, "index")) # unclass for speed
    id   <- xindex[[1L]]
    time <- xindex[[2L]]
    lagid <- as.numeric(id) - c(NA, as.numeric(id)[1:(N-1)])
    FEres.1[lagid != 0] <- NA
    data <- data.frame(id, time, FEres = unclass(FEres), FEres.1 = unclass(FEres.1))
    names(data)[c(1L, 2L)] <- c("id", "time")
    data <- na.omit(data)

    # calc. auxiliary model
    auxmod <- plm(FEres ~ FEres.1, data = data, model = "pooling", index = c("id", "time"))

    ## calc. theoretical rho under H0: no serial corr. in errors
    t. <- pdim(x)$nT$T
    rho.H0 <- -1/(t.-1)
    myH0 <- paste("FEres.1 = ", as.character(rho.H0), sep="")

    ## test H0: rho=rho.H0 with HAC
    myvcov <- function(x) vcovHC(x, method = "arellano", ...) # more params may be passed via ellipsis

    # calc F stat with restriction rho.H0 and robust vcov
    FEARstat <- ((coef(auxmod)["FEres.1"] - rho.H0)/sqrt(myvcov(auxmod)["FEres.1", "FEres.1"]))^2
    names(FEARstat) <- "F"
    df1 <- c("df1" = 1)
    df2 <- c("df2" = df.residual(auxmod))
    pFEARstat <- pf(FEARstat, df1 = df1, df2 = df2, lower.tail = FALSE)

    ## insert usual htest features
    RVAL <- list(statistic   = FEARstat,
                 parameter   = c(df1, df2),
                 p.value     = pFEARstat,
                 method = "Wooldridge's test for serial correlation in FE panels",
                 alternative = "serial correlation",
                 data.name   = paste(deparse(substitute(x))))
    class(RVAL) <- "htest"
    return(RVAL)
}

## Bera, Sosa-Escudero and Yoon type LM test for random effects
## under serial correlation (H0: no random effects) or the inverse;
## test="ar": serial corr. test robust vs. RE
## test="re": RE test robust vs. serial corr.
## test="j":  joint test for serial corr. and random effects

# Reference for the _balanced_ tests="ar"|"re":
#                   Bera/Sosa-Escudero/Yoon (2001), Tests for the error component model in the presence of local misspecifcation,
#                                                   Journal of Econometrics 101 (2001), pp. 1-23.
#
#           for original (balanced) test="j": Baltagi/Li (1991), A joint test for serial correlation and random individual effects,
#                                                     Statistics & Probability Letters 11 (1991), pp. 277-280.
#
# Reference for _un_balanced versions of all three tests (boil down to the balanced versions for balanced panels):
#                    Sosa-Escudero/Bera (2008), Tests for unbalanced error-components models under local misspecification,
#                                               The Stata Journal (2008), Vol. 8, Number 1, pp. 68-78.
#
# Concise treatment of only _balanced_ tests in
#                      Baltagi (2005), Econometric Analysis of Panel Data, 3rd edition, pp. 96-97
#                   or Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, pp. 108.
#
#
## Implementation follows the formulae for unbalanced panels, which reduce for balanced data to the formulae for balanced panels.
##
## Notation in code largely follows Sosa-Escudero/Bera (2008) (m in Sosa-Escudero/Bera (2008) is total number of observations -> N_obs)
## NB: Baltagi's book matrix A is slightly different defined: A in Baltagi is -A in Sosa-Escudera/Bera (2008)



#' Bera, Sosa-Escudero and Yoon Locally--Robust Lagrange Multiplier
#' Tests for Panel Models and Joint Test by Baltagi and Li
#'
#' Test for residual serial correlation (or individual random effects)
#' locally robust vs. individual random effects (serial correlation)
#' for panel models and joint test of serial correlation and the
#' random effect specification by Baltagi and Li.
#'
#' These Lagrange multiplier tests are robust vs. local
#' misspecification of the alternative hypothesis, i.e., they test the
#' null of serially uncorrelated residuals against AR(1) residuals in
#' a pooling model, allowing for local departures from the assumption
#' of no random effects; or they test the null of no random effects
#' allowing for local departures from the assumption of no serial
#' correlation in residuals.  They use only the residuals of the
#' pooled OLS model and correct for local misspecification as outlined
#' in \insertCite{BERA:SOSA:YOON:01;textual}{plm}.
#'
#' For `test = "re"`, the default (`re.normal = TRUE`) is to compute
#' a one-sided test which is expected to lead to a more powerful test
#' (asymptotically N(0,1) distributed).  Setting `re.normal = FALSE` gives
#' the two-sided test (asymptotically chi-squared(2) distributed). Argument
#' `re.normal` is irrelevant for all other values of `test`.
#'
#' The joint test of serial correlation and the random effect
#' specification (`test = "j"`) is due to
#' \insertCite{BALT:LI:91;textual}{plm} (also mentioned in
#' \insertCite{BALT:LI:95;textual}{plm}, pp. 135--136) and is added
#' for convenience under this same function.
#'
#' The unbalanced version of all tests are derived in
#' \insertCite{SOSA:BERA:08;textual}{plm}. The functions implemented
#' are suitable for balanced as well as unbalanced panel data sets.
#'
#' A concise treatment of the statistics for only balanced panels is
#' given in \insertCite{BALT:13;textual}{plm}, p. 108.
#'
#' Here is an overview of how the various values of the `test`
#' argument relate to the literature:
#'
#' \itemize{ \item `test = "ar"`: \itemize{ \item \eqn{RS*_{\rho}} in Bera
#' et al. (2001), p. 9 (balanced) \item \eqn{LM*_{\rho}} in Baltagi (2013), p.
#' 108 (balanced) \item \eqn{RS*_{\lambda}} in Sosa-Escudero/Bera (2008), p. 73
#' (unbalanced) }
#'
#' \item `test = "re", re.normal = TRUE` (default) (one-sided test,
#' asymptotically N(0,1) distributed): \itemize{ \item \eqn{RSO*_{\mu}} in Bera
#' et al. (2001), p. 11 (balanced) \item \eqn{RSO*_{\mu}} in Sosa-Escudero/Bera
#' (2008), p. 75 (unbalanced) }
#'
#' \item `test = "re", re.normal = FALSE` (two-sided test, asymptotically
#' chi-squared(2) distributed): \itemize{ \item \eqn{RS*_{\mu}} in Bera et al.
#' (2001), p. 7 (balanced) \item \eqn{LM*_{\mu}} in Baltagi (2013), p. 108
#' (balanced) \item \eqn{RS*_{\mu}} in Sosa-Escudero/Bera (2008), p. 73
#' (unbalanced) }
#'
#' \item `test = "j"`: \itemize{ \item \eqn{RS_{\mu\rho}} in Bera et al.
#' (2001), p. 10 (balanced) \item \eqn{LM} in Baltagi/Li (2001), p. 279
#' (balanced) \item \eqn{LM_{1}} in Baltagi and Li (1995), pp. 135--136
#' (balanced) \item \eqn{LM1} in Baltagi (2013), p. 108 (balanced) \item
#' \eqn{RS_{\lambda\rho}} in Sosa-Escudero/Bera (2008), p. 74 (unbalanced) } }
#'
#' @aliases pbsytest
#' @param x an object of class `formula` or of class `panelmodel`,
#' @param data a `data.frame`,
#' @param test a character string indicating which test to perform:
#' first--order serial correlation (`"ar"`), random effects (`"re"`)
#' or joint test for either of them (`"j"`),
#' @param re.normal logical, only relevant for `test = "re"`: `TRUE`
#' (default) computes the one-sided `"re"` test, `FALSE` the
#' two-sided test (see also Details); not relevant for other values of
#' `test` and, thus, should be `NULL`,
#' @param \dots further arguments.
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo (initial implementation) & Kevin Tappe (extension to
#' unbalanced panels)
#' @seealso [plmtest()] for individual and/or time random effects
#' tests based on a correctly specified model; [pbltest()],
#' [pbgtest()] and [pdwtest()] for serial correlation tests
#' in random effects models.
#' @references
#'
#' \insertRef{BERA:SOSA:YOON:01}{plm}
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:LI:91}{plm}
#'
#' \insertRef{BALT:LI:95}{plm}
#'
#' \insertRef{SOSA:BERA:08}{plm}
#'
#' @keywords htest
#'
#' @examples
#'
#' ## Bera et. al (2001), p. 13, table 1 use
#' ## a subset of the original Grunfeld
#' ## data which contains three errors -> construct this subset:
#' data("Grunfeld", package = "plm")
#' Grunsubset <- rbind(Grunfeld[1:80, ], Grunfeld[141:160, ])
#' Grunsubset[Grunsubset$firm == 2 & Grunsubset$year %in% c(1940, 1952), ][["inv"]] <- c(261.6, 645.2)
#' Grunsubset[Grunsubset$firm == 2 & Grunsubset$year == 1946, ][["capital"]] <- 232.6
#'
#' ## default is AR testing (formula interface)
#' pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"))
#' pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"), test = "re")
#' pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"),
#'   test = "re", re.normal = FALSE)
#' pbsytest(inv ~ value + capital, data = Grunsubset, index = c("firm", "year"), test = "j")
#'
#' ## plm interface
#' mod <- plm(inv ~ value + capital, data = Grunsubset, model = "pooling")
#' pbsytest(mod)
#'
pbsytest <- function (x, ...) {
    UseMethod("pbsytest")
}

#' @rdname pbsytest
#' @export
pbsytest.formula <- function(x, data, ..., test = c("ar", "re", "j"), re.normal = if (test == "re") TRUE else NULL) {

    ######### from here generic testing interface from
    ######### plm to my code
    if (length(test) == 1L) test <- tolower(test) # for backward compatibility: allow upper case
    test <- match.arg(test)

    cl <- match.call(expand.dots = TRUE)
    if (is.null(cl$model)) cl$model <- "pooling"
    if (cl$model != "pooling") stop("pbsytest only relevant for pooling models")
    names(cl)[2L] <- "formula"
    if (names(cl)[3L] == "") names(cl)[3L] <- "data"
    m <- match(plm.arg, names(cl), 0)
    cl <- cl[c(1, m)]
    cl[[1L]] <- as.name("plm")
    plm.model <- eval(cl, parent.frame())
    pbsytest(plm.model, test = test, re.normal = re.normal, ...)
}

#' @rdname pbsytest
#' @export
pbsytest.panelmodel <- function(x, test = c("ar", "re", "j"), re.normal = if (test == "re") TRUE else NULL, ...) {
    test <- match.arg(test)
    if (describe(x, "model") != "pooling") stop("pbsytest only relevant for pooling models")

    # interface check for argument re.normal
    if (test != "re" && !is.null(re.normal)) {
        stop("argument 're.normal' only relevant for test = \"re\", set re.normal = NULL for other tests")}

    poolres <- x$residuals
    data <- model.frame(x)
    ## extract indices
    index <- attr(data, "index")
    iindex <- index[[1L]]
    tindex <- index[[2L]]


    ## till here.
    ## ordering here if needed.

    ## this needs ordering of obs. on time, regardless
    ## whether before that on groups or after

    ## and numerosity check

    ## order by group, then time
    oo <- order(iindex,tindex)
    ind <- iindex[oo]
    tind <- tindex[oo]
    poolres <- poolres[oo]
    pdim <- pdim(x)
    n <- max(pdim$Tint$n) ## det. number of groups
    T_i <- pdim$Tint$Ti
    N_t <- pdim$Tint$nt
    t <- max(T_i) ## det. max. group numerosity
    N_obs <- pdim$nT$N ## det. total number of obs. (m in Sosa-Escudera/Bera (2008), p. 69)

    ## calc. matrices A and B:
    # Sosa-Escudera/Bera (2008), p. 74
    # Baltagi (2013), p. 108 defines A=(S1/S2)-1 and, thus, has slightly different formulae [opposite sign in Baltagi]
    S1 <- as.numeric(crossprod(tapply(poolres,ind,sum))) # == sum(tapply(poolres,ind,sum)^2)
    S2 <- as.numeric(crossprod(poolres))                 # == sum(poolres^2)
    A <- 1 - S1/S2

    unind <- unique(ind)
    uu <-  uu1 <- rep(NA, length(unind))
    for(i in 1:length(unind)) {
        u.t <- poolres[ind == unind[i]]
        u.t.1 <- u.t[-length(u.t)]
        u.t <- u.t[-1L]
        uu[i] <- crossprod(u.t)
        uu1[i] <- crossprod(u.t, u.t.1)
    }
    B <- sum(uu1)/sum(uu)

    a <- as.numeric(crossprod(T_i)) # Sosa-Escudera/Bera (2008), p. 69

    switch(test,
           "ar" = {
               # RS*_lambda from Sosa-Escudero/Bera (2008), p. 73 (unbalanced formula)
               stat <- (B + (((N_obs - n)/(a - N_obs)) * A))^2 * (((a - N_obs)*N_obs^2) / ((N_obs - n)*(a - 3*N_obs + 2*n)))
               df <- c(df = 1)
               names(stat) <- "chisq"
               pstat <- pchisq(stat, df = df, lower.tail = FALSE)
               tname <- "Bera, Sosa-Escudero and Yoon locally robust test"
               myH0_alt <- "AR(1) errors sub random effects"
           },

           "re" = {
               if(re.normal) {
                   # RSO*_mu from Sosa-Escudero/Bera (2008), p. 75 (unbalanced formula), normally distributed
                   stat <- -sqrt( (N_obs^2) / (2*(a - 3*N_obs + 2*n))) * (A + 2*B)
                   names(stat) <- "z"
                   df <- NULL
                   pstat <- pnorm(stat, lower.tail = FALSE)
                   tname <- "Bera, Sosa-Escudero and Yoon locally robust test (one-sided)"
                   myH0_alt <- "random effects sub AR(1) errors"
               } else {
                   # RS*_mu from Sosa-Escudero/Bera (2008), p. 73 (unbalanced formula), chisq(1)
                   stat <- ((N_obs^2) * (A + 2*B)^2) / (2*(a - 3*N_obs + 2*n))
                   names(stat) <- "chisq"
                   df <- c(df = 1)
                   pstat <- pchisq(stat, df = df, lower.tail = FALSE)
                   tname <- "Bera, Sosa-Escudero and Yoon locally robust test (two-sided)"
                   myH0_alt <- "random effects sub AR(1) errors"
               }
           },

           "j" = {
               # RS_lambda_mu in Sosa-Escudero/Bera (2008), p. 74 (unbalanced formula)
               stat <- N_obs^2 * ( ((A^2 + 4*A*B + 4*B^2) / (2*(a - 3*N_obs + 2*n))) + (B^2/(N_obs - n)))
               # Degrees of freedom in the joint test (test="j") of Baltagi/Li (1991) are 2 (chisquare(2) distributed),
               # see Baltagi/Li (1991), p. 279 and again in Baltagi/Li (1995), p. 136
               df <- c(df = 2)
               names(stat) <- "chisq"
               pstat <- pchisq(stat, df = df, lower.tail = FALSE)
               tname <- "Baltagi and Li AR-RE joint test"
               myH0_alt <- "AR(1) errors or random effects"
           }
    ) # END switch

    dname <- paste(deparse(substitute(formula)))
    balanced.type <- if(pdim$balanced) "balanced" else "unbalanced"
    tname <- paste(tname, "-", balanced.type, "panel", collapse = " ")

    RVAL <- list(statistic   = stat,
                 parameter   = df,
                 method      = tname,
                 alternative = myH0_alt,
                 p.value     = pstat,
                 data.name   = dname)
    class(RVAL) <- "htest"
    return(RVAL)
}

#' Durbin--Watson Test for Panel Models
#'
#' Test of serial correlation for (the idiosyncratic component of) the errors
#' in panel models.
#'
#' This Durbin--Watson test uses the auxiliary model on
#' (quasi-)demeaned data taken from a model of class `plm` which may
#' be a `pooling` (the default), `random` or `within` model. It
#' performs a Durbin--Watson test (using `dwtest` from package
#' \CRANpkg{lmtest} on the residuals of the (quasi-)demeaned model,
#' which should be serially uncorrelated under the null of no serial
#' correlation in idiosyncratic errors. The function takes the
#' demeaned data, estimates the model and calls `dwtest`. Thus, this
#' test does not take the panel structure of the residuals into
#' consideration; it shall not be confused with the generalized
#' Durbin-Watson test for panels in `pbnftest`.
#'
#' @aliases pdwtest
#' @importFrom lmtest dwtest
#' @param x an object of class `"panelmodel"` or of class
#'     `"formula"`,
#' @param data a `data.frame`,
#' @param \dots further arguments to be passed on to `dwtest`,
#'     e.g., `alternative`, see [lmtest::dwtest()] for
#'     further details.
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo
#' @seealso [lmtest::dwtest()] for the Durbin--Watson test
#'     in \CRANpkg{lmtest}, [pbgtest()] for the analogous
#'     Breusch--Godfrey test for panel models,
#'     [lmtest::bgtest()] for the Breusch--Godfrey test for
#'     serial correlation in the linear model. [pbltest()],
#'     [pbsytest()], [pwartest()] and
#'     [pwfdtest()] for other serial correlation tests for
#'     panel models.
#'
#' For the Durbin-Watson test generalized to panel data models see
#' [pbnftest()].
#' @references
#'
#' \insertRef{DURB:WATS:50}{plm}
#'
#' \insertRef{DURB:WATS:51}{plm}
#'
#' \insertRef{DURB:WATS:71}{plm}
#'
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{WOOL:10}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' g <- plm(inv ~ value + capital, data = Grunfeld, model="random")
#' pdwtest(g)
#' pdwtest(g, alternative="two.sided")
#' ## formula interface
#' pdwtest(inv ~ value + capital, data=Grunfeld, model="random")
#'
pdwtest <- function (x, ...) {
    UseMethod("pdwtest")
}

#' @rdname pdwtest
#' @export
pdwtest.panelmodel <- function(x, ...) {
    ## does not respect panel structure:
    ## residual serial correlation test based on the residuals of the demeaned
    ## model and passed on to lmtest::dwtest() for the original DW test
    ## approach justified in Wooldridge (2002/2010), Econometric Analysis of Cross Section and Panel Data, p. 288/328.
    ##
    ## For the Bhargava et al. (1982) generalized DW test see pbnftest()

    ## structure:
    ## 1: take demeaned data from 'plm' object
    ## 2: est. auxiliary model by OLS on demeaned data
    ## 3: apply lmtest::dwtest() to auxiliary model and return the result

    model <- describe(x, "model")
    effect <- describe(x, "effect")
    theta <- x$ercomp$theta

    ## retrieve demeaned data
    demX <- model.matrix(x, model = model, effect = effect, theta = theta, cstcovar.rm = "all")
    demy <- pmodel.response(model.frame(x), model = model, effect = effect, theta = theta)

    ## lmtest::dwtest on the demeaned model:

    ## ARtest is the return value of lmtest::dwtest, exception made for the method attribute
    dots <- list(...)
    order.by    <- if(is.null(dots$order.by)) NULL else dots$order.by
    alternative <- if(is.null(dots$alternative)) "greater" else dots$alternative
    iterations  <- if(is.null(dots$iterations)) 15 else dots$iterations
    exact       <- if(is.null(dots$exact)) NULL else dots$exact
    tol         <- if(is.null(dots$tol)) 1e-10 else dots$tol

    demy <- remove_pseries_features(demy) # needed as lmtest::dwtest cannot cope with pseries

    auxformula <- demy ~ demX - 1
    lm.mod <- lm(auxformula)

    ARtest <- dwtest(lm.mod, order.by = order.by,
                     alternative = alternative,
                     iterations = iterations, exact = exact, tol = tol)

    # overwrite elements of the values produced by lmtest::dwtest
    ARtest$method <- "Durbin-Watson test for serial correlation in panel models"
    ARtest$alternative <- "serial correlation in idiosyncratic errors"
    ARtest$data.name <- data.name(x)
    return(ARtest)
}

#' @rdname pdwtest
#' @export
pdwtest.formula <- function(x, data, ...) {
    ## formula method for pdwtest;
    ## defaults to pooling model

    cl <- match.call(expand.dots = TRUE)
    if (is.null(cl$model)) cl$model <- "pooling"
    names(cl)[2L] <- "formula"
    if (names(cl)[3L] == "") names(cl)[3L] <- "data"
    m <- match(plm.arg, names(cl), 0)
    cl <- cl[c(1L, m)]
    cl[[1L]] <- quote(plm)
    plm.model <- eval(cl, parent.frame())
    pdwtest(plm.model, ...)
}



## references:
## * balanced and consecutive:
##    Bhargava/Franzini/Narendranathan (1982), Serial Correlation and the Fixed Effects Model, Review of Economic Studies (1982), XLIX(4), pp. 533-549.
##    (also in Baltagi (2005/2013), p. 98-99/109-110 for FE application)
## * unbalanced and/or non-consecutive: modified BNF statistic and LBI statistic
##    Baltagi/Wu (1999), Unequally spaced panel data regressions with AR(1) disturbances. Econometric Theory, 15(6), pp. 814-823.
##    (an example is also in Baltagi (2005/2013), p. 90/101)



#' Modified BNF--Durbin--Watson Test and Baltagi--Wu's LBI Test for Panel
#' Models
#'
#' Tests for AR(1) disturbances in panel models.
#'
#' The default, `test = "bnf"`, gives the (modified) BNF statistic,
#' the generalised Durbin-Watson statistic for panels. For balanced
#' and consecutive panels, the reference is
#' Bhargava/Franzini/Narendranathan (1982). The modified BNF is given
#' for unbalanced and/or non-consecutive panels (d1 in formula 16 of
#' \insertCite{BALT:WU:99;textual}{plm}).
#'
#' `test = "lbi"` yields Baltagi--Wu's LBI statistic
#' \insertCite{BALT:WU:99}{plm}, the locally best invariant test which
#' is based on the modified BNF statistic.
#'
#' No specific variants of these tests are available for random effect models.
#' As the within estimator is consistent also under the random effects
#' assumptions, the test for random effect models is performed by taking the
#' within residuals.
#'
#' No p-values are given for the statistics as their distribution is
#' quite difficult. \insertCite{BHAR:FRAN:NARE:82;textual}{plm} supply
#' tabulated bounds for p = 0.05 for the balanced case and consecutive
#' case.
#'
#' For large N, \insertCite{BHAR:FRAN:NARE:82}{plm} suggest it is
#' sufficient to check whether the BNF statistic is < 2 to test
#' against positive serial correlation.
#'
#' @aliases pbnftest
#' @param x an object of class `"panelmodel"` or of class `"formula"`,
#' @param test a character indicating the test to be performed, either
#'     `"bnf"` or `"lbi"` for the (modified) BNF statistic or
#'     Baltagi--Wu's LBI statistic, respectively,
#' @param data a `data.frame` (only relevant for formula interface),
#' @param model a character indicating on which type of model the test
#'     shall be performed (`"pooling"`, `"within"`, `"random"`, only
#'     relevant for formula interface),
#' @param \dots only relevant for formula interface: further arguments
#'     to specify the model to test (arguments passed on to plm()),
#'     e.g., `effect`.
#' @return An object of class `"htest"`.
#' @export
#' @author Kevin Tappe
#' @seealso [pdwtest()] for the original Durbin--Watson test using
#'     (quasi-)demeaned residuals of the panel model without taking
#'     the panel structure into account. [pbltest()], [pbsytest()],
#'     [pwartest()] and [pwfdtest()] for other serial correlation
#'     tests for panel models.
#' @references
#'
#' \insertRef{BALT:13}{plm}
#'
#' \insertRef{BALT:WU:99}{plm}
#'
#' \insertRef{BHAR:FRAN:NARE:82}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("Grunfeld", package = "plm")
#'
#' # formula interface, replicate Baltagi/Wu (1999), table 1, test case A:
#' data_A <- Grunfeld[!Grunfeld[["year"]] %in% c("1943", "1944"), ]
#' pbnftest(inv ~ value + capital, data = data_A, model = "within")
#' pbnftest(inv ~ value + capital, data = data_A, test = "lbi", model = "within")
#'
#' # replicate Baltagi (2013), p. 101, table 5.1:
#' re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#' pbnftest(re)
#' pbnftest(re, test = "lbi")
#'
pbnftest <- function (x, ...) {
    UseMethod("pbnftest")
}

#' @rdname pbnftest
#' @export
pbnftest.panelmodel <- function(x, test = c("bnf", "lbi"), ...) {

    test <- match.arg(test)

    # no test for random effects available: take FE as also consistent (Verbeek (2004, 2nd edition), p. 358)
    model <- describe(x, "model")
    if (model == "random") x <- update(x, model = "within")

    consec <- all(is.pconsecutive(x))
    balanced <- is.pbalanced(x)

    # residuals are now class pseries, so diff.pseries is used and the
    # differences are computed within observational units (not across as
    # it would be the case if base::diff() is used and as it is done for
    # lm-objects) NAs are introduced by the differencing as one
    # observation is lost per observational unit
    if (!inherits(residuals(x), "pseries")) stop("pdwtest internal error: residuals are not of class \"pseries\"") # check to be safe: need pseries

    ind <- unclass(index(x))[[1L]] # unclass for speed
    obs1 <- !duplicated(ind)                  # first ob of each individual
    obsn <- !duplicated(ind, fromLast = TRUE) # last ob of each individual

    #### d1, d2, d3, d4 as in Baltagi/Wu (1999), p. 819 formula (16)
    res_crossprod <- as.numeric(crossprod(residuals(x))) # denominator

    ## d1 consists of two parts:
    ##  d1.1: BNF statistic (sum of squared differenced residuals of consecutive time periods per individual)
    ##  d1.2: sum of squared "later" residuals (not differenced) surrounded by gaps in time periods
    ##  typo in Baltagi/Wu (1999) for d1: index j starts at j = 2, not j = 1
    res_diff <- diff(residuals(x), shift = "time")
    d1.1 <- sum(res_diff^2, na.rm = T) / res_crossprod # == BNF (1982), formula (4)
    d1.2_contrib <- as.logical(is.na(res_diff) - obs1)
    d1.2 <- as.numeric(crossprod(residuals(x)[d1.2_contrib])) / res_crossprod
    d1 <- d1.1 + d1.2 # == modified BNF statistic = d1 in Baltagi/Wu (1999) formula (16)
    #   [reduces to original BNF in case of balanced and consecutive data (d1.2 is zero)]

    if (test == "bnf") {
        stat <- d1
        names(stat) <- "DW"
        method <- "Bhargava/Franzini/Narendranathan Panel Durbin-Watson Test"
        if (!consec || !balanced) method <- paste0("modified ", method)
    }

    if (test == "lbi")  {
        ## d2 contains the "earlier" obs surrounded by gaps in time periods
        d2_contrib <- as.logical(is.na(lead(residuals(x), shift = "time")) - obsn)
        d2 <- as.numeric(crossprod(residuals(x)[d2_contrib])) / res_crossprod

        ## d3, d4: sum squared residual of first/last time period for all individuals / crossprod(residuals)
        d3 <- as.numeric(crossprod(residuals(x)[obs1])) / res_crossprod
        d4 <- as.numeric(crossprod(residuals(x)[obsn])) / res_crossprod

        stat <- d1 + d2 + d3 + d4
        names(stat) <- "LBI"
        method <- "Baltagi/Wu LBI Test for Serial Correlation in Panel Models"
    }

    result <- list(statistic   = stat,
                   # p.value   = NA, # none
                   method      = method,
                   alternative = "serial correlation in idiosyncratic errors",
                   data.name   = data.name(x))
    class(result) <- "htest"
    return(result)
}

#' @rdname pbnftest
#' @export
pbnftest.formula <- function(x, data, test = c("bnf", "lbi"), model = c("pooling", "within", "random"), ...) {
    ## formula method for pdwtest;
    ## defaults to pooling model

    test  <- match.arg(test)
    model <- match.arg(model)

    cl <- match.call(expand.dots = TRUE)
    if (is.null(model)) model <- "pooling"
    names(cl)[2L] <- "formula"
    if (names(cl)[3L] == "") names(cl)[3L] <- "data"
    m <- match(plm.arg, names(cl), 0)
    cl <- cl[c(1L, m)]
    cl[[1L]] <- quote(plm)
    plm.model <- eval(cl, parent.frame())
    pbnftest(plm.model, test = test)
}

######### Baltagi and Li's LM_rho|mu ########
## ex Baltagi and Li (1995) Testing AR(1) against MA(1)...,
## JE 68, 133-151, test statistic (one-sided) is LM_4;
## see also idem (1997), Monte Carlo results...,
## Annales d'Econometrie et Statistique 48, formula (8)

## from version 2: disposes of Kronecker products,
## thus much faster and feasible on large NT (original
## is already infeasible for NT>3000, this takes 10''
## on N=3000, T=10 and even 20000x10 (55'') is no problem;
## lme() hits the memory limit at ca. 20000x20)

#' Baltagi and Li Serial Dependence Test For Random Effects Models
#'
#' \insertCite{BALT:LI:95;textual}{plm}'s Lagrange multiplier test for
#' AR(1) or MA(1) idiosyncratic errors in panel models with random
#' effects.
#'
#' This is a Lagrange multiplier test for the null of no serial
#' correlation, against the alternative of either an AR(1) or a MA(1)
#' process, in the idiosyncratic component of the error term in a
#' random effects panel model (as the analytical expression of the
#' test turns out to be the same under both alternatives,
#' \insertCite{@see @BALT:LI:95 and @BALT:LI:97}{plm}. The
#' `alternative` argument, defaulting to `twosided`, allows testing
#' for positive serial correlation only, if set to `onesided`.
#'
#' @aliases pbltest
#' @importFrom nlme lme
#' @param x a model formula or an estimated random--effects model of
#'     class `plm` ,
#' @param data for the formula interface only: a `data.frame`,
#' @param alternative one of `"twosided"`,
#'     `"onesided"`. Selects either \eqn{H_A: \rho \neq 0} or
#'     \eqn{H_A: \rho = 0} (i.e., the Normal or the Chi-squared
#'     version of the test),
#' @param index the index of the `data.frame`,
#' @param \dots further arguments.
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo
#' @seealso [pdwtest()], [pbnftest()], [pbgtest()],
#'     [pbsytest()], [pwartest()] and
#'     [pwfdtest()] for other serial correlation tests for
#'     panel models.
#' @references
#'
#' \insertRef{BALT:LI:95}{plm}
#'
#' \insertRef{BALT:LI:97}{plm}
#'
#' @keywords htest
#' @examples
#'
#' data("Grunfeld", package = "plm")
#'
#' # formula interface
#' pbltest(inv ~ value + capital, data = Grunfeld)
#'
#' # plm interface
#' re_mod <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#' pbltest(re_mod)
#' pbltest(re_mod, alternative = "onesided")
#'
pbltest <- function (x, ...)
{
    UseMethod("pbltest")
}


#' @rdname pbltest
#' @export
pbltest.formula <- function(x, data, alternative = c("twosided", "onesided"), index = NULL, ...) {
    ## this version (pbltest0) based on a "formula, pdataframe" interface


    ## reduce X to model matrix value (no NAs)
    X <- model.matrix(x, data = data)
    ## reduce data accordingly
    data <- data[which(row.names(data) %in% row.names(X)), ]
    if (! inherits(data, "pdata.frame"))
        data <- pdata.frame(data, index = index)

    ## need name of individual index
    gindex <- dimnames(attr(data, "index"))[[2L]][1L]

    ## make random effects formula
    rformula <- NULL
    eval(parse(text = paste("rformula <- ~1|", gindex, sep = "")))

    ## est. MLE model
    mymod <- lme(x, data = data, random = rformula, method = "ML")

    nt. <- mymod$dims$N
    n. <- as.numeric(mymod$dims$ngrps[1L])
    t. <- nt./n.
    Jt <- matrix(1, ncol = t., nrow = t.)/t.
    Et <- diag(1, t.) - Jt
    ## make 'bidiagonal' matrix (see BL, p.136)
    G <- matrix(0, ncol = t., nrow = t.)
    for(i in 2:t.) {
        G[i-1, i]   <- 1
        G[i,   i-1] <- 1
    }

    ## retrieve composite (=lowest level) residuals
    uhat <- residuals(mymod, level = 0)

    ## sigma2.e and sigma2.1 as in BL
    ## break up residuals by group to get rid of Kronecker prod.
    ## data have to be balanced and sorted by group/time, so this works
    uhat.i <- vector("list", n.)
    for(i in 1:n.) {
        uhat.i[[i]] <- uhat[t.*(i-1)+1:t.]
    }
    s2e <- rep(NA, n.)
    s21 <- rep(NA, n.)
    for(i in 1:n.) {
        u.i <- uhat.i[[i]]
        s2e[i] <- as.numeric(crossprod(u.i, Et) %*% u.i)
        s21[i] <- as.numeric(crossprod(u.i, Jt) %*% u.i)
    }
    sigma2.e <- sum(s2e) / (n.*(t.-1))
    sigma2.1 <- sum(s21) / n.

    ## calc. score under the null:
    star1 <- (Jt/sigma2.1 + Et/sigma2.e) %*% G %*% (Jt/sigma2.1 + Et/sigma2.e)
    star2 <- rep(NA, n.)
    ## again, do this group by group to avoid Kronecker prod.
    for(i in 1:n.) {
        star2[i] <- as.numeric(crossprod(uhat.i[[i]], star1) %*% uhat.i[[i]])
    }
    star2 <- sum(star2)
    Drho <- (n.*(t.-1)/t.) * (sigma2.1-sigma2.e)/sigma2.1 + sigma2.e/2 * star2
    ## star2 is (crossprod(uhat, kronecker(In, star1)) %*% uhat)

    ## components for the information matrix
    a <- (sigma2.e - sigma2.1)/(t.*sigma2.1)
    j.rr <- n. * (2 * a^2 * (t.-1)^2 + 2*a*(2*t.-3) + (t.-1))
    j.12 <- n.*(t.-1)*sigma2.e / sigma2.1^2
    j.13 <- n.*(t.-1)/t. * sigma2.e * (1/sigma2.1^2 - 1/sigma2.e^2)
    j.22 <- (n. * t.^2) / (2 * sigma2.1^2)
    j.23 <- (n. * t.) / (2 * sigma2.1^2)
    j.33 <- (n./2) * (1/sigma2.1^2 + (t.-1)/sigma2.e^2)

    ## build up information matrix
    Jmat <- matrix(nrow = 3L, ncol = 3L)
    Jmat[1L, ] <- c(j.rr, j.12, j.13)
    Jmat[2L, ] <- c(j.12, j.22, j.23)
    Jmat[3L, ] <- c(j.13, j.23, j.33)

    J11 <- n.^2 * t.^2 * (t.-1) / (det(Jmat) * 4*sigma2.1^2 * sigma2.e^2)
    ## this is the same as J11 <- solve(Jmat)[1,1], see BL page 73

    switch(match.arg(alternative),
           "onesided" = {
               LMr.m <- Drho * sqrt(J11)
               pval <- pnorm(LMr.m, lower.tail = FALSE)
               names(LMr.m) <- "z"
               method1 <- "one-sided"
               method2 <- "H0: rho = 0, HA: rho > 0"
               parameter <- NULL
           },
           "twosided" = {
               LMr.m <- Drho^2 * J11
               pval <- pchisq(LMr.m, df = 1, lower.tail = FALSE)
               names(LMr.m) <- "chisq"
               parameter <- c(df = 1)
               method1 <- "two-sided"
               method2 <- "H0: rho = 0, HA: rho != 0"
           }
    )
    dname <- paste(deparse(substitute(x)))
    method <- paste("Baltagi and Li", method1, "LM test")
    alternative <- "AR(1)/MA(1) errors in RE panel model"

    res <- list(statistic   = LMr.m,
                p.value     = pval,
                method      = method,
                alternative = alternative,
                parameter   = parameter,
                data.name   = dname)

    class(res) <- "htest"
    res
}

#' @rdname pbltest
#' @export
pbltest.plm <- function(x, alternative = c("twosided", "onesided"), ...) {
    # only continue if random effects model
    if (describe(x, "model") != "random") stop("Test is only for random effects models.")

    # call pbltest.formula the right way
    pbltest.formula(formula(x$formula), data = cbind(index(x), x$model),
                    index = names(index(x)), alternative = alternative, ...)
}

#' Wooldridge first--difference--based test for AR(1) errors in levels
#' or first--differenced panel models
#'
#' First--differencing--based test of serial correlation for (the idiosyncratic
#' component of) the errors in either levels or first--differenced panel
#' models.
#'
#' As \insertCite{WOOL:10;textual}{plm}, Sec. 10.6.3 observes, if the
#' idiosyncratic errors in the model in levels are uncorrelated (which
#' we label hypothesis `"fe"`), then the errors of the model in first
#' differences (FD) must be serially correlated with
#' \eqn{cor(\hat{e}_{it}, \hat{e}_{is}) = -0.5} for each \eqn{t,s}. If
#' on the contrary the levels model's errors are a random walk, then
#' there must be no serial correlation in the FD errors (hypothesis
#' `"fd"`). Both the fixed effects (FE) and the first--differenced
#' (FD) estimators remain consistent under either assumption, but the
#' relative efficiency changes: FE is more efficient under `"fe"`, FD
#' under `"fd"`.
#'
#' Wooldridge (ibid.) suggests basing a test for either hypothesis on
#' a pooled regression of FD residuals on their first lag:
#' \eqn{\hat{e}_{i,t}=\alpha + \rho \hat{e}_{i,t-1} +
#' \eta_{i,t}}. Rejecting the restriction \eqn{\rho = -0.5} makes us
#' conclude against the null of no serial correlation in errors of the
#' levels equation (`"fe"`). The null hypothesis of no serial
#' correlation in differenced errors (`"fd"`) is tested in a similar
#' way, but based on the zero restriction on \eqn{\rho} (\eqn{\rho =
#' 0}). Rejecting `"fe"` favours the use of the first--differences
#' estimator and the contrary, although it is possible that both be
#' rejected.
#'
#' `pwfdtest` estimates the `fd` model (or takes an `fd` model as
#' input for the panelmodel interface) and retrieves its residuals,
#' then estimates an AR(1) `pooling` model on them. The test statistic
#' is obtained by applying a F test to the latter model to test the
#' relevant restriction on \eqn{\rho}, setting the covariance matrix
#' to `vcovHC` with the option `method="arellano"` to control for
#' serial correlation.
#'
#' Unlike the `pbgtest` and `pdwtest`, this test does not rely on
#' large--T asymptotics and has therefore good properties in ''short''
#' panels.  Furthermore, it is robust to general
#' heteroskedasticity. The `"fe"` version can be used to test for
#' error autocorrelation regardless of whether the maintained
#' specification has fixed or random effects
#' \insertCite{@see @DRUK:03}{plm}.
#'
#' @aliases pwfdtest
#' @param x an object of class `formula` or a `"fd"`-model (plm
#' object),
#' @param data a `data.frame`,
#' @param h0 the null hypothesis: one of `"fd"`, `"fe"`,
#' @param \dots further arguments to be passed on to `vcovHC` (see Details
#' and Examples).
#' @return An object of class `"htest"`.
#' @export
#' @author Giovanni Millo
#' @seealso `pdwtest`, `pbgtest`, `pwartest`,
#' @references
#'
#' \insertRef{DRUK:03}{plm}
#'
#' \insertRef{WOOL:02}{plm}
#' Sec. 10.6.3, pp. 282--283.
#'
#' \insertRef{WOOL:10}{plm}
#' Sec. 10.6.3, pp. 319--320
#'
#' @keywords htest
#' @examples
#'
#' data("EmplUK" , package = "plm")
#' pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK)
#' pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK, h0 = "fe")
#'
#' # pass argument 'type' to vcovHC used in test
#' pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK, type = "HC3", h0 = "fe")
#'
#'
#' # same with panelmodel interface
#' mod <- plm(log(emp) ~ log(wage) + log(capital), data = EmplUK, model = "fd")
#' pwfdtest(mod)
#' pwfdtest(mod, h0 = "fe")
#' pwfdtest(mod, type = "HC3", h0 = "fe")
#'
#'
pwfdtest <- function(x, ...) {
    UseMethod("pwfdtest")
}

#' @rdname pwfdtest
#' @export
pwfdtest.formula <- function(x, data, ..., h0 = c("fd", "fe")) {
    cl <- match.call(expand.dots = TRUE)
    if (is.null(cl$model)) cl$model <- "fd"
    names(cl)[2L] <- "formula"
    if (names(cl)[3L] == "") names(cl)[3L] <- "data"
    m <- match(plm.arg, names(cl), 0)
    cl <- cl[c(1L, m)]
    cl[[1L]] <- quote(plm)
    plm.model <- eval(cl, parent.frame())
    pwfdtest(plm.model, ..., h0 = h0)
}

#' @rdname pwfdtest
#' @export
pwfdtest.panelmodel <- function(x, ..., h0 = c("fd", "fe")) {
    ## first-difference-based serial correlation test for panel models
    ## ref.: Wooldridge (2002/2010), par. 10.6.3

    # interface check
    model <- describe(x, "model")
    if (model != "fd") stop(paste0("input 'x' needs to be a \"fd\" model (first-differenced model), but is \"", model, "\""))

    ## fetch fd residuals
    FDres <- x$residuals
    ## indices (full length! must reduce by 1st time period)
    ## this is an ad-hoc solution for the fact that the 'fd' model
    ## carries on the full indices while losing the first time period
    xindex <- unclass(attr(model.frame(x), "index")) # unclass for speed
    time <- as.numeric(xindex[[2L]])
    id   <- as.numeric(xindex[[1L]])

    ## fetch dimensions and adapt to those of indices
    pdim <- pdim(x)
    n <- pdim$nT$n
    Ti_minus_one <- pdim$Tint$Ti-1

    ## generate new individual index: drop one observation per individual
    ## NB: This is based on the assumption that the estimated FD model performs
    ##     its diff-ing row-wise (it currently does so). If the diff-ing for FD
    ##     is changed to diff-ing based on time dimension, this part about index
    ##     creation needs to be re-worked because more than 1 observation per
    ##     individual can be dropped
    red_id <- integer()
    for(i in 1:n) {
        red_id <- c(red_id, rep(i, Ti_minus_one[i]))
    }
    # additional check
    # (but should error earlier already as the FD model should be nonestimable)
    if(length(red_id) == 0L)
        stop("only individuals with one observation in original data: test not feasible")

    # make pdata.frame for auxiliary regression: time dimension is not relevant
    # as the first observation of each individual was dropped -> let time dimension
    # be created (is not related to the original times anymore)
    auxdata <- pdata.frame(as.data.frame(cbind(red_id, FDres)), index = "red_id")

    # lag residuals by row (as the FD model diffs by row)
    # NB: need to consider change to shift = "time" if behaviour of FD model is changed
    auxdata[["FDres.1"]] <- lag(auxdata[["FDres"]], shift = "row")

    ## pooling model FDres vs. lag(FDres), with intercept (might as well do it w.o.)
    auxmod <- plm(FDres ~ FDres.1, data = auxdata, model = "pooling")

    switch(match.arg(h0),
           "fd" = {h0des <- "differenced"
           ## theoretical rho under H0: no serial
           ## corr. in differenced errors is 0
           rho.H0 <- 0},

           "fe" = {h0des <- "original"
           ## theoretical rho under H0: no serial
           ## corr. in original errors is -0.5
           rho.H0 <- -0.5})

    myH0 <- paste("FDres.1 = ", as.character(rho.H0), sep="")

    ## test H0: rho=rho.H0 with HAC, more params may be passed via ellipsis
    myvcov <- function(x) vcovHC(x, method = "arellano", ...)

    # calc F stat with restriction rho.H0 and robust vcov
    FDARstat <- ((coef(auxmod)["FDres.1"] - rho.H0)/sqrt(myvcov(auxmod)["FDres.1", "FDres.1"]))^2
    names(FDARstat) <- "F"
    df1 <- c(df1 = 1)
    df2 <- c(df2 = df.residual(auxmod))
    pFDARstat <- pf(FDARstat, df1 = df1, df2 = df2, lower.tail = FALSE)

    ## insert usual htest features
    RVAL <- list(statistic   = FDARstat,
                 parameter   = c(df1, df2),
                 p.value     = pFDARstat,
                 method      = "Wooldridge's first-difference test for serial correlation in panels",
                 alternative = paste("serial correlation in", h0des, "errors"),
                 data.name   = paste(deparse(substitute(x))))
    class(RVAL) <- "htest"
    return(RVAL)
}

# test_uroot.R#
padf <- function(x, exo = c("none", "intercept", "trend"), p.approx = NULL, ...){
    # p-value approximation for tau distribution of (augmented) Dickey-Fuller test
    # as used in some panel unit root tests in purtest().
    #
    # argument 'x' must be a numeric (can be length == 1 or >= 1)
    #
    # p-values approximation is performed by the method of MacKinnon (1994) or
    # MacKinnon (1996), the latter yielding better approximated p-values but
    # requires package 'urca'.
    # Default is NULL: check for availability of 'urca' and, if available, perform
    # MacKinnon (1996); fall back to MacKinnon (1994) if 'urca' is not available.
    # User can demand a specific method by setting the argument 'p.approx' to either
    # "MacKinnon1994" or "MacKinnon1996".

    exo <- match.arg(exo)

    # check if ellipsis (dots) has p.approx (could be passed from purtest()'s dots)
    # and if so, use p.approx from ellipsis
    dots <- list(...)
    if (!is.null(dots$p.approx)) p.approx <- dots$p.approx

    if (!is.null(p.approx) && !p.approx %in% c("MacKinnon1994", "MacKinnon1996"))
        stop(paste0("unknown argument value: p.approx = \"", p.approx, "\""))

    # Check if package 'urca' is available on local machine. We placed 'urca'
    # in 'Suggests' rather than 'Imports' so that it is not an absolutely
    # required dependency.)
    ## Procedure for pkg check for pkg in 'Suggests' as recommended in
    ## Wickham, R packages (http://r-pkgs.had.co.nz/description.html).
    urca <- if(!requireNamespace("urca", quietly = TRUE)) FALSE else TRUE

    # default: if no p.approx specified by input (NULL),
    # use MacKinnon (1996) if 'urca' is available, else MacKinnon (1994)
    p.approx <- if(is.null(p.approx)) { if(urca)  "MacKinnon1996" else "MacKinnon1994" } else p.approx

    if (!is.null(p.approx) && p.approx == "MacKinnon1996" && !urca) {
        # catch case when user demands MacKinnon (1996) per argument but 'urca' is unavailable
        warning("method MacKinnon (1996) requested via argument 'p.approx' but requires non-installed package 'urca'; falling back to MacKinnon (1994)")
        p.approx <- "MacKinnon1994"
    }

    if(p.approx == "MacKinnon1996") {
        # translate exo argument to what urca::punitroot expects
        punitroot.exo <- switch (exo,
                                 "none"      = "nc",
                                 "intercept" = "c",
                                 "trend"     = "ct")

        res <- urca::punitroot(x, N = Inf, trend = punitroot.exo) # return asymptotic value
    }

    if(p.approx == "MacKinnon1994") {
        # values from MacKinnon (1994), table 3, 4
        small <- matrix(c(0.6344, 1.2378, 3.2496,
                          2.1659, 1.4412, 3.8269,
                          3.2512, 1.6047, 4.9588),
                        nrow = 3, byrow = TRUE)
        small <- t(t(small) / c(1, 1, 100))
        large <- matrix(c(0.4797, 9.3557, -0.6999,  3.3066,
                          1.7339, 9.3202, -1.2745, -1.0368,
                          2.5261, 6.1654, -3.7956, -6.0285),
                        nrow = 3, byrow = TRUE)
        large <- t(t(large) / c(1, 10, 10, 100))
        limit <- c(-1.04, -1.61, -2.89)
        rownames(small) <- rownames(large) <- names(limit) <- c("none", "intercept", "trend")

        c.x.x2 <- rbind(1, x, x ^ 2)
        psmall <- colSums(small[exo, ] * c.x.x2)
        plarge <- colSums(large[exo, ] * rbind(c.x.x2, x ^ 3))

        res <- as.numeric(pnorm(psmall * (x <= limit[exo]) + plarge * (x > limit[exo])))
    }
    attr(res, "p.approx") <- p.approx
    return(res)
} ## END padf


## IPS (2003), table 3 for Wtbar statistic
# x1: means without time trend from table 3 in IPS (2003)
adj.ips.wtbar.x1 <- c(
    -1.504,-1.514,-1.522,-1.520,-1.526,-1.523,-1.527,-1.519,-1.524,-1.532,
    -1.488,-1.503,-1.516,-1.514,-1.519,-1.520,-1.524,-1.519,-1.522,-1.530,
    -1.319,-1.387,-1.428,-1.443,-1.460,-1.476,-1.493,-1.490,-1.498,-1.514,
    -1.306,-1.366,-1.413,-1.433,-1.453,-1.471,-1.489,-1.486,-1.495,-1.512,
    -1.171,-1.260,-1.329,-1.363,-1.394,-1.428,-1.454,-1.458,-1.470,-1.495,
    NA,    NA,-1.313,-1.351,-1.384,-1.421,-1.451,-1.454,-1.467,-1.494,
    NA,    NA,    NA,-1.289,-1.331,-1.380,-1.418,-1.427,-1.444,-1.476,
    NA,    NA,    NA,-1.273,-1.319,-1.371,-1.411,-1.423,-1.441,-1.474,
    NA,    NA,    NA,-1.212,-1.266,-1.329,-1.377,-1.393,-1.415,-1.456
)
# x2: variances without time trend from table 3 in IPS (2003)
adj.ips.wtbar.x2 <- c(
    1.069,0.923,0.851,0.809,0.789,0.770,0.760,0.749,0.736,0.735,
    1.255,1.011,0.915,0.861,0.831,0.803,0.781,0.770,0.753,0.745,
    1.421,1.078,0.969,0.905,0.865,0.830,0.798,0.789,0.766,0.754,
    1.759,1.181,1.037,0.952,0.907,0.858,0.819,0.802,0.782,0.761,
    2.080,1.279,1.097,1.005,0.946,0.886,0.842,0.819,0.801,0.771,
    NA,   NA,1.171,1.055,0.980,0.912,0.863,0.839,0.814,0.781,
    NA,   NA,   NA,1.114,1.023,0.942,0.886,0.858,0.834,0.795,
    NA,   NA,   NA,1.164,1.062,0.968,0.910,0.875,0.851,0.806,
    NA,   NA,   NA,1.217,1.105,0.996,0.929,0.896,0.871,0.818
)

# x3: means with time trend from table 3 in IPS (2003)
adj.ips.wtbar.x3 <- c(
    -2.166,-2.167,-2.168,-2.167,-2.172,-2.173,-2.176,-2.174,-2.174,-2.177,
    -2.173,-2.169,-2.172,-2.172,-2.173,-2.177,-2.180,-2.178,-2.176,-2.179,
    -1.914,-1.999,-2.047,-2.074,-2.095,-2.120,-2.137,-2.143,-2.146,-2.158,
    -1.922,-1.977,-2.032,-2.065,-2.091,-2.117,-2.137,-2.142,-2.146,-2.158,
    -1.750,-1.823,-1.911,-1.968,-2.009,-2.057,-2.091,-2.103,-2.114,-2.135,
    NA,    NA,-1.888,-1.955,-1.998,-2.051,-2.087,-2.101,-2.111,-2.135,
    NA,    NA,    NA,-1.868,-1.923,-1.995,-2.042,-2.065,-2.081,-2.113,
    NA,    NA,    NA,-1.851,-1.912,-1.986,-2.036,-2.063,-2.079,-2.112,
    NA,    NA,    NA,-1.761,-1.835,-1.925,-1.987,-2.024,-2.046,-2.088
)

# x4: variances with time trend from table 3 in IPS (2003)
adj.ips.wtbar.x4 <- c(
    1.132,0.869,0.763,0.713,0.690,0.655,0.633,0.621,0.610,0.597,
    1.453,0.975,0.845,0.769,0.734,0.687,0.654,0.641,0.627,0.605,
    1.627,1.036,0.882,0.796,0.756,0.702,0.661,0.653,0.634,0.613,
    2.482,1.214,0.983,0.861,0.808,0.735,0.688,0.674,0.650,0.625,
    3.947,1.332,1.052,0.913,0.845,0.759,0.705,0.685,0.662,0.629,
    NA,   NA,1.165,0.991,0.899,0.792,0.730,0.705,0.673,0.638,
    NA,   NA,   NA,1.055,0.945,0.828,0.753,0.725,0.689,0.650,
    NA,   NA,   NA,1.145,1.009,0.872,0.786,0.747,0.713,0.661,
    NA,   NA,   NA,1.208,1.063,0.902,0.808,0.766,0.728,0.670
)

adj.ips.wtbar <- c(adj.ips.wtbar.x1, adj.ips.wtbar.x2,
                   adj.ips.wtbar.x3, adj.ips.wtbar.x4)

adj.ips.wtbar <- array(adj.ips.wtbar, dim = c(10, 9, 2, 2),
                       dimnames = list(
                           c(10, 15, 20, 25, 30, 40, 50, 60, 70, 100),
                           0:8,
                           c("mean", "var"),
                           c("intercept", "trend"))
)

adj.ips.wtbar <- aperm(adj.ips.wtbar, c(2, 1, 3, 4))



###############
## IPS (2003), table 2 (obvious typos (missing minus signs) corrected)

# intercept 1% critical values
critval.ips.tbar.int1 <- c(
    -3.79, -2.66, -2.54, -2.50, -2.46, -2.44, -2.43, -2.42, -2.42, -2.40, -2.40,
    -3.45, -2.47, -2.38, -2.33, -2.32, -2.31, -2.29, -2.28, -2.28, -2.28, -2.27,
    -3.06, -2.32, -2.24, -2.21, -2.19, -2.18, -2.16, -2.16, -2.16, -2.16, -2.15,
    -2.79, -2.14, -2.10, -2.08, -2.07, -2.05, -2.04, -2.05, -2.04, -2.04, -2.04,
    -2.61, -2.06, -2.02, -2.00, -1.99, -1.99, -1.98, -1.98, -1.98, -1.97, -1.97,
    -2.51, -2.01, -1.97, -1.95, -1.94, -1.94, -1.93, -1.93, -1.93, -1.93, -1.92,
    -2.20, -1.85, -1.83, -1.82, -1.82, -1.82, -1.81, -1.81, -1.81, -1.81, -1.81,
    -2.00, -1.75, -1.74, -1.73, -1.73, -1.73, -1.73, -1.73, -1.73, -1.73, -1.73)
# intercept 5% critical values
critval.ips.tbar.int5 <- c(
    -2.76, -2.28, -2.21, -2.19, -2.18, -2.16, -2.16, -2.15, -2.16, -2.15,-2.15,
    -2.57, -2.17, -2.11, -2.09, -2.08, -2.07, -2.07, -2.06, -2.06, -2.06,-2.05,
    -2.42, -2.06, -2.02, -1.99, -1.99, -1.99, -1.98, -1.98, -1.97, -1.98,-1.97,
    -2.28, -1.95, -1.92, -1.91, -1.90, -1.90, -1.90, -1.89, -1.89, -1.89,-1.89,
    -2.18, -1.89, -1.87, -1.86, -1.85, -1.85, -1.85, -1.85, -1.84, -1.84,-1.84,
    -2.11, -1.85, -1.83, -1.82, -1.82, -1.82, -1.81, -1.81, -1.81, -1.81,-1.81,
    -1.95, -1.75, -1.74, -1.73, -1.73, -1.73, -1.73, -1.73, -1.73, -1.73,-1.73,
    -1.84, -1.68, -1.67, -1.67, -1.67, -1.67, -1.67, -1.67, -1.67, -1.67,-1.67)
# intercept 10% critical values
critval.ips.tbar.int10 <- c(
    -2.38, -2.10, -2.06, -2.04, -2.04, -2.02, -2.02, -2.02, -2.02, -2.02, -2.01,
    -2.27, -2.01, -1.98, -1.96, -1.95, -1.95, -1.95, -1.95, -1.94, -1.95, -1.94,
    -2.17, -1.93, -1.90, -1.89, -1.88, -1.88, -1.88, -1.88, -1.88, -1.88, -1.88,
    -2.06, -1.85, -1.83, -1.82, -1.82, -1.82, -1.81, -1.81, -1.81, -1.81, -1.81,
    -2.00, -1.80, -1.79, -1.78, -1.78, -1.78, -1.78, -1.78, -1.78, -1.77, -1.77,
    -1.96, -1.77, -1.76, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75,
    -1.85, -1.70, -1.69, -1.69, -1.69, -1.69, -1.68, -1.68, -1.68, -1.68, -1.69,
    -1.77, -1.64, -1.64, -1.64, -1.64, -1.64, -1.64, -1.64, -1.64, -1.64, -1.64)
# trend 1% critical values
critval.ips.tbar.trend1 <- c(
    -8.12, -3.42, -3.21, -3.13, -3.09, -3.05, -3.03, -3.02, -3.00, -3.00, -2.99,
    -7.36, -3.20, -3.03, -2.97, -2.94, -2.93, -2.90, -2.88, -2.88, -2.87, -2.86,
    -6.44, -3.03, -2.88, -2.84, -2.82, -2.79, -2.78, -2.77, -2.76, -2.75, -2.75,
    -5.72, -2.86, -2.74, -2.71, -2.69, -2.68, -2.67, -2.65, -2.66, -2.65, -2.64,
    -5.54, -2.75, -2.67, -2.63, -2.62, -2.61, -2.59, -2.60, -2.59, -2.58, -2.58,
    -5.16, -2.69, -2.61, -2.58, -2.58, -2.56, -2.55, -2.55, -2.55, -2.54, -2.54,
    -4.50, -2.53, -2.48, -2.46, -2.45, -2.45, -2.44, -2.44, -2.44, -2.44, -2.43,
    -4.00, -2.42, -2.39, -2.38, -2.37, -2.37, -2.36, -2.36, -2.36, -2.36, -2.36)
# trend 5% critical values
critval.ips.tbar.trend5 <- c(
    -4.66, -2.98, -2.87, -2.82, -2.80, -2.79, -2.77, -2.76, -2.75, -2.75, -2.75,
    -4.38, -2.85, -2.76, -2.72, -2.70, -2.69, -2.68, -2.67, -2.67, -2.66, -2.66,
    -4.11, -2.74, -2.66, -2.63, -2.62, -2.60, -2.60, -2.59, -2.59, -2.58, -2.58,
    -3.88, -2.63, -2.57, -2.55, -2.53, -2.53, -2.52, -2.52, -2.52, -2.51, -2.51,
    -3.73, -2.56, -2.52, -2.49, -2.48, -2.48, -2.48, -2.47, -2.47, -2.46, -2.46,
    -3.62, -2.52, -2.48, -2.46, -2.45, -2.45, -2.44, -2.44, -2.44, -2.44, -2.43,
    -3.35, -2.42, -2.38, -2.38, -2.37, -2.37, -2.36, -2.36, -2.36, -2.36, -2.36,
    -3.13, -2.34, -2.32, -2.32, -2.31, -2.31, -2.31, -2.31, -2.31, -2.31, -2.31)
# trend 10% critical values
critval.ips.tbar.trend10 <- c(
    -3.73, -2.77, -2.70, -2.67, -2.65, -2.64, -2.63, -2.62, -2.63, -2.62, -2.62,
    -3.60, -2.68, -2.62, -2.59, -2.58, -2.57, -2.57, -2.56, -2.56, -2.55, -2.55,
    -3.45, -2.59, -2.54, -2.52, -2.51, -2.51, -2.50, -2.50, -2.50, -2.49, -2.49,
    -3.33, -2.52, -2.47, -2.46, -2.45, -2.45, -2.44, -2.44, -2.44, -2.44, -2.44,
    -3.26, -2.47, -2.44, -2.42, -2.41, -2.41, -2.41, -2.40, -2.40, -2.40, -2.40,
    -3.18, -2.44, -2.40, -2.39, -2.39, -2.38, -2.38, -2.38, -2.38, -2.38, -2.38,
    -3.02, -2.36, -2.33, -2.33, -2.33, -2.32, -2.32, -2.32, -2.32, -2.32, -2.32,
    -2.90, -2.30, -2.29, -2.28, -2.28, -2.28, -2.28, -2.28, -2.28, -2.28, -2.28)

critval.ips.tbar <- c(critval.ips.tbar.int1,
                      critval.ips.tbar.int5,
                      critval.ips.tbar.int10,
                      critval.ips.tbar.trend1,
                      critval.ips.tbar.trend5,
                      critval.ips.tbar.trend10)

critval.ips.tbar <- array(critval.ips.tbar, dim = c(11, 8, 3, 2),
                          dimnames = list(
                              c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 100),
                              c(5, 7, 10, 15, 20, 25, 50, 100),
                              c("1%", "5%", "10%"),
                              c("intercept", "trend"))
)

critval.ips.tbar <- aperm(critval.ips.tbar, c(2, 1, 3, 4))


###############

## IPS (2003), table 1
# right hand pane of table 1 for Ztbar statistic
adj.ips.zbar.time  <- c(6, 7, 8, 9, 10, 15, 20, 25, 30, 40, 50, 100, 500, 1000, 2000)
adj.ips.zbar.means <- c(-1.520, -1.514, -1.501, -1.501, -1.504, -1.514, -1.522, -1.520, -1.526, -1.523, -1.527, -1.532, -1.531, -1.529, -1.533)
adj.ips.zbar.vars  <- c(1.745, 1.414, 1.228, 1.132, 1.069, 0.923, 0.851, 0.809, 0.789, 0.770, 0.760, 0.735, 0.715, 0.707, 0.706)
names(adj.ips.zbar.time) <- names(adj.ips.zbar.means) <- names(adj.ips.zbar.vars) <- adj.ips.zbar.time

# left pane of table 1 [not used]
adj.ips.zbarL.means <- c(-1.125, -1.178, -1.214, -1.244, -1.274, -1.349, -1.395, -1.423, -1.439, -1.463, -1.477, -1.504, -1.526, -1.526, -1.533)
adj.ips.zbarL.vars  <- c(0.497, 0.506, 0.506, 0.527, 0.521, 0.565, 0.592, 0.609, 0.623, 0.639, 0.656, 0.683, 0.704, 0.702, 0.706)

################

# table 2 in LLC (2002): mean and standard deviation adjustments
Tn <- c(  25,  30,  35,  40,  45,  50,  60,   70,   80,   90,  100,  250,   500)

v <- c(c( 0.004,  0.003,  0.002,  0.002,  0.001,  0.001,  0.001,  0.000,  0.000,  0.000,  0.000,  0.000,  0.000),
       c( 1.049,  1.035,  1.027,  1.021,  1.017,  1.014,  1.011,  1.008,  1.007,  1.006,  1.005,  1.001,  1.000),
       c(-0.554, -0.546, -0.541, -0.537, -0.533, -0.531, -0.527, -0.524, -0.521, -0.520, -0.518, -0.509, -0.500),
       c( 0.919,  0.889,  0.867,  0.850,  0.837,  0.826,  0.810,  0.798,  0.789,  0.782,  0.776,  0.742,  0.707),
       c(-0.703, -0.674, -0.653, -0.637, -0.624, -0.614, -0.598, -0.587, -0.578, -0.571, -0.566, -0.533, -0.500),
       c( 1.003,  0.949,  0.906,  0.871,  0.842,  0.818,  0.780,  0.751,  0.728,  0.710,  0.695,  0.603,  0.500)
)

adj.levinlin <- array(v, dim = c(13, 2, 3),
                      dimnames = list(Tn,
                                      c("mu", "sigma"),
                                      c("none", "intercept", "trend")))

purtest.names.exo <- c(none      = "None",
                       intercept = "Individual Intercepts",
                       trend     = "Individual Intercepts and Trend")

purtest.names.test <- c(levinlin  = "Levin-Lin-Chu Unit-Root Test",
                        ips       = "Im-Pesaran-Shin Unit-Root Test",
                        madwu     = "Maddala-Wu Unit-Root Test",
                        Pm        = "Choi's modified P Unit-Root Test",
                        invnormal = "Choi's Inverse Normal Unit-Root Test",
                        logit     = "Choi's Logit Unit-Root Test",
                        hadri     = "Hadri Test")


## General functions to transform series:

YClags <- function(object,  k = 3){
    if (k > 0)
        sapply(1:k, function(x) c(rep(NA, x), object[1:(length(object)-x)]))
    else
        NULL
}

YCtrend <- function(object) 1:length(object)

YCdiff <- function(object){
    c(NA, object[2:length(object)] - object[1:(length(object)-1)])
}

selectT <- function(x, Ts){
    ## This function selects the length of the series as it is tabulated
    if (x %in% Ts) return(x)
    if (x < Ts[1L]){
        warning("the time series is short")
        return(Ts[1L])
    }
    if (x > Ts[length(Ts)]){
        warning("the time series is long")
        return(Ts[length(Ts)])
    }
    pos <- which((Ts - x) > 0)[1L]
    return(Ts[c(pos - 1, pos)])
}

lagsel <- function(object, exo = c("intercept", "none", "trend"),
                   method = c("Hall", "AIC", "SIC"), pmax = 10,
                   dfcor = FALSE, fixedT = TRUE, ...){
    # select the optimal number of lags using Hall method, AIC, or SIC
    method <- match.arg(method)
    y <- object
    Dy <- YCdiff(object)
    Ly <- c(NA, object[1:(length(object)-1)])
    if (exo == "none")      m <- NULL
    if (exo == "intercept") m <- rep(1, length(object))
    if (exo == "trend")     m <- cbind(1, YCtrend(object))
    LDy <- YClags(Dy, pmax)
    decreasei <- TRUE
    i <- 0
    narow <- 1:(pmax+1)
    if (method == "Hall"){
        while(decreasei){
            lags <- pmax - i
            if (!fixedT) narow <- 1:(lags+1)
            X <- cbind(Ly, LDy[ , 0:lags], m)[-narow, , drop = FALSE]
            y <- Dy[-narow]
            sres <- my.lm.fit(X, y, dfcor = dfcor)
            tml <- sres$coef[lags+1]/sres$se[lags+1]
            if (abs(tml) < 1.96 && lags > 0)
                i <- i + 1
            else
                decreasei <- FALSE
        }
    }
    else{
        l <- c()
        while(i <= pmax){
            lags <- pmax - i
            if (!fixedT) narow <- 1:(lags+1)
            X <- cbind(Ly, LDy[ , 0:lags], m)[-narow, , drop = FALSE]
            y <- Dy[-narow]
            sres <- my.lm.fit(X, y, dfcor = dfcor)
            AIC <- if (method == "AIC") {
                log(sres$rss / sres$n) + 2 * sres$K / sres$n
            } else {
                log(sres$rss / sres$n) + sres$K * log(sres$n) / sres$n
            }
            l <- c(l, AIC)
            i <- i + 1
        }
        lags <- pmax + 1 - which.min(l)
    }
    lags
} ## END lagsel


adj.levinlin.value <- function(l, exo = c("intercept", "none", "trend")){
    ## extract the adjustment values for Levin-Lin-Chu test
    theTs <- as.numeric(dimnames(adj.levinlin)[[1L]])
    Ts <- selectT(l, theTs)
    if (length(Ts) == 1L){
        return(adj.levinlin[as.character(Ts), , exo])
    }
    else{
        low  <- adj.levinlin[as.character(Ts[1L]), , exo]
        high <- adj.levinlin[as.character(Ts[2L]), , exo]
        return(low + (l - Ts[1L])/(Ts[2L] - Ts[1L]) * (high - low))
    }
} ## END adj.levinlin.value

adj.ips.wtbar.value <- function(l = 30, lags = 2, exo = c("intercept", "trend")){
    ## extract the adjustment values for Im-Pesaran-Shin test for Wtbar statistic (table 3 in IPS (2003))
    if (!lags %in% 0:8) warning("lags should be an integer between 0 and 8")
    lags <- min(lags, 8)
    theTs <- as.numeric(dimnames(adj.ips.wtbar)[[2L]])
    Ts <- selectT(l, theTs)
    if (length(Ts) == 1L){
        # take value as in table
        return(adj.ips.wtbar[as.character(lags), as.character(Ts), , exo])
    }
    else{
        # interpolate value from table
        low  <- adj.ips.wtbar[as.character(lags), as.character(Ts[1L]), , exo]
        high <- adj.ips.wtbar[as.character(lags), as.character(Ts[2L]), , exo]
        return(low + (l - Ts[1L])/(Ts[2L] - Ts[1L]) * (high - low))
    }
} ## END adj.ips.wtbar.value

adj.ips.ztbar.value <- function(l = 30L, time, means, vars){
    ## extract the adjustment values for Im-Pesaran-Shin test's Ztbar statistic
    ## from table 1, right hand pane in IPS (2003) fed by arguments means and vars
    Ts <- selectT(l, time)
    if (length(Ts) == 1L){
        # take value as in table
        return(c("mean" = means[as.character(Ts)], "var" = vars[as.character(Ts)]))
    }
    else{
        # interpolate value from table
        low  <- c("mean" = means[as.character(Ts[1L])], "var" = vars[as.character(Ts[1L])])
        high <- c("mean" = means[as.character(Ts[2L])], "var" = vars[as.character(Ts[2L])])
        return(low + (l - Ts[1L])/(Ts[2L] - Ts[1L]) * (high - low))
    }
} ## END adj.ips.ztbar.value

critval.ips.tbar.value <- function(ind = 10L, time = 19L, critvals, exo = c("intercept", "trend")){
    ## extract and interpolate 1%, 5%, 10% critical values for Im-Pesaran-Shin test's
    ## tbar statistic (table 2 in IPS (2003))
    ##
    ## Interpolation is based on inverse distance weighting (IDW) of
    ## L1 distance (1d case) and L2 distance (euclidean distance) (2d case)
    ## (optical inspections shows this method is a good approximation)

    theInds <- as.numeric(dimnames(critvals)[[1L]])
    theTs <- as.numeric(dimnames(critvals)[[2L]])
    Inds <- selectT(ind, theInds)
    Ts <- selectT(time, theTs)

    exo <- match.arg(exo)

    if(length(Inds) == 1L && length(Ts) == 1L) {
        # exact hit for individual AND time: take value as in table
        return(critvals[as.character(Inds), as.character(Ts), , exo])
    }
    else{
        if(length(Inds) == 1L || length(Ts) == 1L) {
            # exact hit for individual (X)OR time: interpolate other dimension
            if(length(Inds) == 1L) {
                low  <- critvals[as.character(Inds), as.character(Ts[1L]), , exo]
                high <- critvals[as.character(Inds), as.character(Ts[2L]), , exo]
                # L1 distances and inverse weighting for time dimension
                dist1 <- abs(time - Ts[1L])
                dist2 <- abs(time - Ts[2L])
                weight1 <- 1/dist1
                weight2 <- 1/dist2
                return ((weight1 * low + weight2 * high ) / (weight1 + weight2))
            }
            if(length(Ts) == 1L) {
                # L1 distances and inverse weighting for individual dimension
                low  <- critvals[as.character(Inds[1L]), as.character(Ts), , exo]
                high <- critvals[as.character(Inds[2L]), as.character(Ts), , exo]
                dist1 <- abs(ind - Inds[1L])
                dist2 <- abs(ind - Inds[2L])
                weight1 <- 1/dist1
                weight2 <- 1/dist2
                return ((weight1 * low + weight2 * high ) / (weight1 + weight2))
            }
        } else {
            # only get to this part when both dimensions are not an exact hit:
            # 2d interpolate

            # extract the 4 critical values as basis of interpolation interpolate ("corners of box")
            crit4 <- critvals[as.character(Inds), as.character(Ts), , exo]
            dot <- c(ind, time) # point of interest
            m <- as.matrix(expand.grid(Inds, Ts))
            colnames(m) <- c("ind", "time")
            dist <- lapply(1:4, function(x) m[x, ] - dot)
            dist <- vapply(dist, function(x) sqrt(as.numeric(crossprod(x))), 0.0, USE.NAMES = FALSE)
            weight <- 1/dist

            res <- (
                crit4[as.character(Inds[1L]), as.character(Ts[1L]), ] * weight[1L] +
                    crit4[as.character(Inds[2L]), as.character(Ts[1L]), ] * weight[2L] +
                    crit4[as.character(Inds[1L]), as.character(Ts[2L]), ] * weight[3L] +
                    crit4[as.character(Inds[2L]), as.character(Ts[2L]), ] * weight[4L]) / sum(weight)
            return(res)
        }
    }
} ## END critval.ips.tbar.value

tsadf <- function(object, exo = c("intercept", "none", "trend"),
                  lags = NULL, dfcor = FALSE, comp.aux.reg = FALSE, ...){
    # compute some ADF regressions for each time series
    y <- object
    L <- length(y)
    Dy <- YCdiff(object)
    Ly <- c(NA, object[1:(length(object) - 1)])
    if(exo == "none")      m <- NULL
    if(exo == "intercept") m <- rep(1, length(object))
    if(exo == "trend")     m <- cbind(1, YCtrend(object))
    narow <- 1:(lags+1)
    LDy <- YClags(Dy, lags)
    X <- cbind(Ly, LDy, m)[-narow, , drop = FALSE]
    y <- Dy[- narow]
    result <- my.lm.fit(X, y, dfcor = dfcor)
    sigma <- result$sigma
    rho <- result$coef[1L]
    sdrho <- result$se[1L]
    trho <- rho/sdrho
    p.trho <- padf(trho, exo = exo, ...)
    result <- list(rho    = rho,
                   sdrho  = sdrho,
                   trho   = trho,
                   sigma  = sigma,
                   T      = L,
                   lags   = lags,
                   p.trho = p.trho)

    if(comp.aux.reg){
        # for Levin-Lin-Chu test only, compute the residuals of the auxiliary
        # regressions
        X <- cbind(LDy[ , 0:lags], m)[-narow, , drop = FALSE]
        if(lags == 0 && exo == "none"){
            resid.diff  <- Dy[-narow]/sigma
            resid.level <- Ly[-narow]/sigma
        }
        else{
            y <- Dy[-narow]
            resid.diff <- lm.fit(X, y)$residuals/sigma
            y <- Ly[-narow]
            resid.level <- lm.fit(X, y)$residuals/sigma
        }
        result$resid <- data.frame(resid.diff  = resid.diff,
                                   resid.level = resid.level)
    }
    result
}


longrunvar <- function(x, exo = c("intercept", "none", "trend"), q = NULL){
    # compute the long run variance of the dependent variable

    # q: lag truncation parameter: default (q == NULL) as in LLC, p. 14
    # it can be seen from LLC, table 2, that round() was used to get an
    # integer from that formula (not, e.g., trunc)
    T <- length(x)
    if (is.null(q)) q <- round(3.21 * T^(1/3))
    dx <- x[2:T] - x[1:(T-1)]
    if(exo == "intercept") dx <- dx - mean(dx)
    if(exo == "trend")     dx <- lm.fit(cbind(1, 1:length(dx)), dx)$residuals
    dx <- c(NA, dx)
    res <- 1/(T-1)*sum(dx[-1]^2)+
        2*sum(
            sapply(1:q,
                   function(L){
                       sum(dx[2:(T-L)] * dx[(L+2):T]) / (T-1) *
                           (1 - L / (q+1))
                   }
            )
        )
    return(res)
}


hadritest <- function(object, exo, Hcons, dfcor, method,
                      cl, args, data.name, ...) {
    ## used by purtest(<.>, test = "hadri"); non-exported function
    ## Hadri's test is applicable to balanced data only
    ## input 'object' is a list with observations per individual
    if(!is.list(object)) stop("argument 'object' in hadritest is supposed to be a list")
    if(exo == "none") stop("exo = \"none\" is not a valid option for Hadri's test")
    # determine L (= time periods), unique for balanced panel and number of individuals (n)
    if(length(L <- unique(lengths(object, use.names = FALSE))) > 1L)
        stop("Hadri test is not applicable to unbalanced panels")
    n <- length(object)

    if(exo == "intercept"){
        # can use lm.fit here as NAs are dropped in beginning of 'purtest'
        resid <- lapply(object, function(x) lm.fit(matrix(1, nrow = length(x)), x)$residuals)
        adj <- c(1/6, 1/45) # xi, zeta^2 in eq. (17) in Hadri (2000)
    }
    if (exo == "trend"){
        resid <- lapply(object, function(x) {
            lx <- length(x)
            dmat <- matrix(c(rep(1, lx), 1:lx), nrow = lx)
            # can use lm.fit here as NAs are dropped in beginning of 'purtest'
            lm.fit(dmat, x)$residuals
        })
        adj <- c(1/15, 11/6300) # xi, zeta^2 in eq. (25) in Hadri (2000)
    }

    cumres2 <- lapply(resid, function(x) cumsum(x)^2)

    if (!dfcor) {
        sigma2  <- mean(unlist(resid, use.names = FALSE)^2)
        sigma2i <- vapply(resid, function(x) mean(x^2), FUN.VALUE = 0.0, USE.NAMES = FALSE)
    } else {
        # df correction as suggested in Hadri (2000), p. 157
        dfcorval <- switch(exo, "intercept" = (L-1), "trend" = (L-2))
        # -> apply to full length residuals over all individuals -> n*(L-1) or n*(L-2)
        sigma2 <- as.numeric(crossprod(unlist(resid, use.names = FALSE))) / (n * dfcorval)
        # -> apply to individual residuals' length, so just L -> L-1 or L-2
        sigma2i <- vapply(resid, function(x) crossprod(x)/dfcorval, FUN.VALUE = 0.0, USE.NAMES = FALSE)
    }

    Si2 <- vapply(cumres2, function(x) sum(x), FUN.VALUE = 0.0, USE.NAMES = FALSE)
    numerator <- 1/n * sum(1/(L^2) * Si2)
    LM <- numerator / sigma2 # non-het consist case (Hcons == FALSE)
    LMi <- 1/(L^2) * Si2 / sigma2i # individual LM statistics

    if (Hcons) {
        LM <- mean(LMi)
        method <- paste0(method, " (Heterosked. Consistent)")
    }

    stat <- c(z = sqrt(n) * (LM - adj[1L])  / sqrt(adj[2L])) # eq. (14), (22) in Hadri (2000)
    pvalue <- pnorm(stat, lower.tail = FALSE) # is one-sided! was until rev. 572: 2*(pnorm(abs(stat), lower.tail = FALSE))

    htest <- structure(list(statistic   = stat,
                            parameter   = NULL,
                            alternative = "at least one series has a unit root", # correct alternative (at least one unit root)
                            data.name   = data.name,
                            method      = method,
                            p.value     = pvalue),
                       class = "htest")

    idres <- mapply(list, LMi, sigma2i, SIMPLIFY = FALSE)
    idres <- lapply(idres, setNames, c("LM", "sigma2"))

    result <- list(statistic = htest,
                   call      = cl,
                   args      = args,
                   idres     = idres)

    class(result) <- "purtest"
    return(result)
} # END hadritest


#' Unit root tests for panel data
#'
#' `purtest` implements several testing procedures that have been proposed
#' to test unit root hypotheses with panel data.
#'
#'
#' All these tests except `"hadri"` are based on the estimation of
#' augmented Dickey-Fuller (ADF) regressions for each time series. A
#' statistic is then computed using the t-statistics associated with
#' the lagged variable. The Hadri residual-based LM statistic is the
#' cross-sectional average of the individual KPSS statistics
#' \insertCite{KWIA:PHIL:SCHM:SHIN:92;textual}{plm}, standardized by their
#' asymptotic mean and standard deviation.
#'
#' Several Fisher-type tests that combine p-values from tests based on
#' ADF regressions per individual are available:
#'
#' - `"madwu"` is the inverse chi-squared test
#' \insertCite{MADDA:WU:99;textual}{plm}, also called P test by
#' \insertCite{CHOI:01;textual}{plm}.
#'
#' - `"Pm"` is the modified P test proposed by
#' \insertCite{CHOI:01;textual}{plm} for large N,
#'
#' - `"invnormal"` is the inverse normal test by \insertCite{CHOI:01;textual}{plm}, and
#'
#' - `"logit"` is the logit test by \insertCite{CHOI:01;textual}{plm}.
#'
#' The individual p-values for the Fisher-type tests are approximated
#' as described in \insertCite{MACK:96;textual}{plm} if the package \CRANpkg{urca}
#' (\insertCite{PFAFF:08;textual}{plm}) is available, otherwise as described in
#' \insertCite{MACK:94;textual}{plm}.
#'
#' For the test statistic tbar of the test of Im/Pesaran/Shin (2003)
#' (`ips.stat = "tbar"`), no p-value is given but 1%, 5%, and 10% critical
#' values are interpolated from paper's tabulated values via inverse distance
#' weighting (printed and contained in the returned value's element
#' `statistic$ips.tbar.crit`).
#'
#' Hadri's test, the test of Levin/Lin/Chu, and the tbar statistic of
#' Im/Pesaran/Shin are not applicable to unbalanced panels; the tbar statistic
#' is not applicable when `lags > 0` is given.
#'
#' The exogeneous instruments of the tests (where applicable) can be specified
#' in several ways, depending on how the data is handed over to the function:
#'
#' - For the `formula`/`data` interface (if `data` is a `data.frame`,
#' an additional `index` argument should be specified); the formula
#' should be of the form: `y ~ 0`, `y ~ 1`, or `y ~ trend` for a test
#' with no exogenous variables, with an intercept, or with individual
#' intercepts and time trend, respectively. The `exo` argument is
#' ignored in this case.
#'
#' - For the `data.frame`, `matrix`, and `pseries` interfaces: in
#' these cases, the exogenous variables are specified using the `exo`
#' argument.
#'
#' With the associated `summary` and `print` methods, additional
#' information can be extracted/displayed (see also Value).
#'
#' @aliases purtest
#' @param object,x Either a `"data.frame"` or a matrix containing the
#'     time series (individuals as columns), a `"pseries"` object, a formula;
#'     a `"purtest"` object for the print and summary methods,
#' @param data a `"data.frame"` or a `"pdata.frame"` object (required for
#'     formula interface, see Details and Examples),
#' @param index the indexes,
#' @param test the test to be computed: one of `"levinlin"` for
#'     \insertCite{LEVIN:LIN:CHU:02;textual}{plm}, `"ips"` for
#'     \insertCite{IM:PESAR:SHIN:03;textual}{plm}, `"madwu"` for
#'     \insertCite{MADDA:WU:99;textual}{plm}, `"Pm"` , `"invnormal"`,
#'     or `"logit"` for various tests as in
#'     \insertCite{CHOI:01;textual}{plm}, or `"hadri"` for
#'     \insertCite{HADR:00;textual}{plm}, see Details,
#' @param exo the exogenous variables to introduce in the augmented
#'     Dickey--Fuller (ADF) regressions, one of: no exogenous
#'     variables (`"none"`), individual intercepts (`"intercept"`), or
#'     individual intercepts and trends (`"trend"`), but see Details,
#' @param lags the number of lags to be used for the augmented
#'     Dickey-Fuller regressions: either a single value integer (the number of
#'     lags for all time series), a vector of integers (one for each
#'     time series), or a character string for an automatic
#'     computation of the number of lags, based on the AIC
#'     (`"AIC"`), the SIC (`"SIC"`), or on the method by
#'     \insertCite{HALL:94;textual}{plm} (`"Hall"`); argument is irrelevant
#'     for `test = "hadri"`,
#' @param pmax maximum number of lags (irrelevant for `test = "hadri"`),
#' @param Hcons logical, only relevant for `test = "hadri"`,
#'     indicating whether the heteroskedasticity-consistent test of
#'     \insertCite{HADR:00;textual}{plm} should be computed,
#' @param q the bandwidth for the estimation of the long-run variance
#'     (only relevant for `test = "levinlin"`, the default (`q = NULL`)
#'     gives the value as suggested by the authors as round(3.21 * T^(1/3))),
#' @param dfcor logical, indicating whether the standard deviation of
#'     the regressions is to be computed using a degrees-of-freedom
#'     correction,
#' @param fixedT logical, indicating whether the individual ADF
#'     regressions are to be computed using the same number of
#'     observations (irrelevant for `test = "hadri"`),
#' @param ips.stat `NULL` or character of length 1 to request a specific
#'     IPS statistic, one of `"Wtbar"` (also default if `ips.stat = NULL`),
#'     `"Ztbar"`, `"tbar"`,
#' @param \dots further arguments (can set argument `p.approx` to be passed on
#'  to non-exported function `padf` to either `"MacKinnon1994"` or `"MacKinnon1996"`
#'  to force a specific method for p-value approximation, the latter only being
#'  possible if package 'urca' is installed).
#' @return For purtest: An object of class `"purtest"`: a list with the elements
#'   named:
#' - `"statistic"` (a `"htest"` object),
#' - `"call"`,
#' - `"args"`,
#' - `"idres"` (containing results from the individual regressions),
#' - `"adjval"` (containing the simulated means and variances needed to compute
#'      the statistic, for `test = "levinlin"` and `"ips"`, otherwise `NULL`),
#' - `"sigma2"` (short-run and long-run variance for `test = "levinlin"`, otherwise NULL).
#' @export
#' @importFrom stats setNames
#' @author Yves Croissant and for "Pm", "invnormal", and "logit" Kevin Tappe
#' @seealso [cipstest()], [phansitest()]

#' @references
#' \insertAllCited{}
#'
#' @keywords htest
#
# TODO: add more examples / interfaces
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' y <- data.frame(split(Grunfeld$inv, Grunfeld$firm)) # individuals in columns
#'
#' purtest(y, pmax = 4, exo = "intercept", test = "madwu")
#'
#' ## same via pseries interface
#' pGrunfeld <- pdata.frame(Grunfeld, index = c("firm", "year"))
#' purtest(pGrunfeld$inv, pmax = 4, exo = "intercept", test = "madwu")
#'
#' ## same via formula interface
#' purtest(inv ~ 1, data = Grunfeld, index = c("firm", "year"), pmax = 4, test = "madwu")
#'
purtest <- function(object, data = NULL, index = NULL,
                    test = c("levinlin", "ips", "madwu", "Pm" , "invnormal", "logit", "hadri"),
                    exo = c("none", "intercept", "trend"),
                    lags = c("SIC", "AIC", "Hall"),
                    pmax = 10, Hcons = TRUE, q = NULL, dfcor = FALSE,
                    fixedT = TRUE, ips.stat = NULL, ...) {

    data.name <- paste(deparse(substitute(object)))

    id <- NULL
    if (inherits(object, "formula")){
        # exo is derived from specified formula:
        terms <- terms(object)
        lab <- labels(terms)
        if(length(lab) == 0L){
            if(attr(terms, "intercept")) exo <- "intercept"
            else exo <- "none"
        }
        else{
            if(length(lab) > 1L || lab != "trend") stop("incorrect formula")
            exo <- "trend"
        }
        object <- paste(deparse(object[[2L]]))
        if(exists(object) && is.vector(get(object))){
            # is.vector because, eg, inv exists as a function
            object <- get(object)
        }
        else{
            if(is.null(data)) stop("unknown response")
            else{
                if(!inherits(data, "data.frame")) stop("'data' does not specify a data.frame/pdata.frame")
                if(object %in% names(data)){
                    object <- data[[object]]
                    if(!inherits(data, "pdata.frame")){
                        if(is.null(index)) stop("the index attribute is required")
                        else data <- pdata.frame(data, index)
                    }
                    id <- unclass(attr(data, "index"))[[1L]]
                }
                else{
                    stop(paste0("unknown response (\"", object, "\" not in data)"))
                }
            }
        }
    } # END object is a formula
    else{
        exo <- match.arg(exo)
        if(is.null(dim(object))){
            if(inherits(object, "pseries")){
                id <- unclass(attr(object, "index"))[[1L]]
            }
            else stop("the individual dimension is undefined") # cannot derive individual dimension from a vector if not pseries
        }
        if(is.matrix(object) || is.data.frame(object)) {
            if(!is.null(data)) stop("object is data.frame or matrix but argument 'data' is not NULL")
            if(is.matrix(object)) object <- as.data.frame(object)
        }
    }

    # by now, object is either a pseries to be split or a data.frame, code continues with list
    object <- na.omit(object)
    if(!is.null(attr(object, "na.action")))
        warning("NA value(s) encountered and dropped, results may not be reliable")

    if(!inherits(object, "data.frame")){
        if(is.null(id)) stop("the individual dimension is undefined")
        # adjust 'id' to correspond data in 'object' after NA dropping:
        if(!is.null(attr(object, "na.action"))) id <- id[-attr(object, "na.action")]
        object <- split(object, id)
    } else {
        if(!ncol(object) > 1L) warning("data.frame or matrix specified in argument object does not contain more than one individual (individuals are supposed to be in columns)")
        object <- as.list(object)
    }

    cl <- match.call()
    test <- match.arg(test)
    ips.stat <- if (is.null(ips.stat)) "Wtbar" else ips.stat # set default for IPS test
    if (is.character(lags)) lags <- match.arg(lags) # if character, match from list of possible values
    args <- list(test = test, exo = exo, pmax = pmax, lags = lags,
                 dfcor = dfcor, fixedT = fixedT, ips.stat = ips.stat)
    n <- length(object) # number of individuals, assumes object is a list
    sigma2 <- NULL
    pvalues.trho <- NULL
    ips.tbar.crit <- NULL
    alternative <- "stationarity"
    method <- paste0(purtest.names.test[test], " (ex. var.: ",
                     purtest.names.exo[exo],")")

    # If Hadri test, call function and exit early
    if(test == "hadri") return(hadritest(object, exo, Hcons, dfcor,
                                         method, cl, args, data.name, ...))

    # compute the lags for each time series if necessary
    if(is.numeric(lags)){
        if(length(lags) == 1L) lags <- rep(lags, n)
        else{
            if(length(lags) != n) stop("lags should be of length 1 or n")
            else lags <- as.list(lags)
        }
    }
    else{ # lag selection procedure SIC, AIC, or Hall
        lag.method <- match.arg(lags)
        lags <- sapply(object, function(x)
            lagsel(x, exo = exo, method = lag.method,
                   pmax = pmax, dfcor = dfcor, fixedT = fixedT))
    }

    # compute the augmented Dickey-Fuller regressions for each time series
    comp.aux.reg <- (test == "levinlin")
    idres <- mapply(function(x, y)
        tsadf(x, exo = exo, lags = y, dfcor = dfcor, comp.aux.reg = comp.aux.reg, ...),
        object, as.list(lags), SIMPLIFY = FALSE)


    if(test == "levinlin"){
        if(length(T.levinlin <- unique(lengths(object, use.names = FALSE))) > 1L)
            stop("test = \"levinlin\" is not applicable to unbalanced panels")

        # get the adjustment parameters for the mean and the variance
        adjval <- adj.levinlin.value(T.levinlin, exo = exo)
        mymu  <- adjval[1L]
        mysig <- adjval[2L]
        # calculate the ratio of LT/ST variance
        sigmaST <- sapply(idres, function(x) x[["sigma"]])
        sigmaLT <- sqrt(sapply(object, longrunvar, exo = exo, q = q))
        si <- sigmaLT/sigmaST # LLC (2002), formula 6
        sbar <- mean(si)

        # stack the residuals of each time series and perform the pooled
        # regression
        res.level <- unlist(lapply(idres, function(x) x$resid[["resid.level"]]), use.names = FALSE)
        res.diff  <- unlist(lapply(idres, function(x) x$resid[["resid.diff"]]), use.names = FALSE)
        z <- my.lm.fit(as.matrix(res.level), res.diff, dfcor = dfcor)
        # compute the Levin-Lin-Chu statistic
        tildeT <- T.levinlin - mean(lags) - 1
        sigmaeps2 <- z$rss / (n * tildeT)
        rho   <- z$coef
        sdrho <- z$se
        trho  <- rho/sdrho
        stat <- (trho - n * tildeT * sbar / sigmaeps2 * sdrho * mymu)/mysig # LLC (2002), formula 12
        names(stat) <- "z" # avoids a concatenated name like z.x1
        pvalue <- pnorm(stat, lower.tail = TRUE) # need lower.tail = TRUE (like ADF one-sided to the left)
        parameter <- NULL
        sigma2 <- cbind(sigmaST^2, sigmaLT^2)
        colnames(sigma2) <- c("sigma2ST", "sigma2LT")
        pvalues.trho <- vapply(idres, function(x) x[["p.trho"]], FUN.VALUE = 0.0)
    }

    if(test == "ips"){
        if(exo == "none") stop("exo = \"none\" is not a valid option for the Im-Pesaran-Shin test")
        if(!is.null(ips.stat) && !any(ips.stat %in% c("Wtbar", "Ztbar", "tbar"))) stop("argument 'ips.stat' must be one of \"Wtbar\", \"Ztbar\", \"tbar\"")
        lags  <- vapply(idres, function(x) x[["lags"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        L.ips <- vapply(idres, function(x) x[["T"]],    FUN.VALUE = 0.0, USE.NAMES = FALSE) - lags - 1
        trho  <- vapply(idres, function(x) x[["trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        pvalues.trho <- vapply(idres, function(x) x[["p.trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        tbar <- mean(trho)
        parameter <- NULL
        adjval <- NULL


        if(is.null(ips.stat) || ips.stat == "Wtbar") {
            # calc Wtbar - default
            adjval <- mapply(function(x, y) adj.ips.wtbar.value(x, y, exo = exo),
                             as.list(L.ips), as.list(lags))
            Etbar <- mean(adjval[1L, ])
            Vtbar <- mean(adjval[2L, ])
            stat <- c("Wtbar" = sqrt(n) * (tbar - Etbar) / sqrt(Vtbar)) # (3.13) = (4.10) in IPS (2003) [same generic formula for Ztbar and Wtbar]
            pvalue <- pnorm(stat, lower.tail = TRUE) # need lower.tail = TRUE (like ADF one-sided to the left), was until rev. 577: 2*pnorm(abs(stat), lower.tail = FALSE)
        }

        if(!is.null(ips.stat) && ips.stat == "Ztbar") {
            # calc Ztbar
            adjval <- adjval.ztbar <- sapply(L.ips, adj.ips.ztbar.value,
                                             adj.ips.zbar.time, adj.ips.zbar.means, adj.ips.zbar.vars)
            rownames(adjval) <- rownames(adjval.ztbar) <- c("mean", "var")
            Etbar.ztbar <- mean(adjval.ztbar[1L, ])
            Vtbar.ztbar <- mean(adjval.ztbar[2L, ])
            stat <- stat.ztbar <- c("Ztbar" = sqrt(n) * (tbar - Etbar.ztbar) / sqrt(Vtbar.ztbar)) # (3.13) = (4.10) in IPS (2003) [same generic formula for Ztbar and Wtbar]
            pvalue <- pvalue.ztbar <- pnorm(stat.ztbar, lower.tail = TRUE)
        }

        if(!is.null(ips.stat) && ips.stat == "tbar") {
            # give tbar
            T.tbar <- unique(lengths(object, use.names = FALSE))
            if(length(T.tbar) > 1L) stop("tbar statistic is not applicable to unbalanced panels")
            if(any(lags > 0L)) stop("tbar statistic is not applicable when 'lags' > 0 is specified")
            L.tbar <- T.tbar - 1
            stat <- tbar
            names(stat) <- "tbar"
            pvalue <- NA
            ips.tbar.crit <- critval.ips.tbar.value(ind = n, time = L.tbar, critval.ips.tbar, exo = exo)
            adjval <- NULL
        }
    }

    if(test == "madwu"){
        # Maddala/Wu (1999), pp. 636-637; Choi (2001), p. 253; Baltagi (2013), pp. 283-285
        ## does not require a balanced panel
        trho <- vapply(idres, function(x) x[["trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        pvalues.trho <- vapply(idres, function(x) x[["p.trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        stat <- c(chisq = - 2 * sum(log(pvalues.trho)))
        n.madwu <- length(trho)
        parameter <- c(df = 2 * n.madwu)
        pvalue <- pchisq(stat, df = parameter, lower.tail = FALSE)
        adjval <- NULL
    }

    if(test == "Pm"){
        ## Choi Pm (modified P) [proposed for large N]
        trho <- vapply(idres, function(x) x[["trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        pvalues.trho <- vapply(idres, function(x) x[["p.trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        n.Pm <- length(trho)
        # formula (18) in Choi (2001), p. 255:
        stat <- c( "Pm" = 1/(2 * sqrt(n.Pm)) * sum(-2 * log(pvalues.trho) - 2) ) # == -1/sqrt(n.Pm) * sum(log(pvalues.trho) +1)
        pvalue <- pnorm(stat, lower.tail = FALSE) # one-sided
        parameter <- NULL
        adjval <- NULL
    }

    if(test == "invnormal"){
        # inverse normal test as in Choi (2001)
        trho <- vapply(idres, function(x) x[["trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        pvalues.trho <- vapply(idres, function(x) x[["p.trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        n.invnormal <- length(trho)
        stat <- c("z" = sum(qnorm(pvalues.trho)) / sqrt(n.invnormal)) # formula (9), Choi (2001), p. 253
        pvalue <- pnorm(stat, lower.tail = TRUE) # formula (12), Choi, p. 254
        parameter <- NULL
        adjval <- NULL
    }

    if(test == "logit"){
        # logit test as in Choi (2001)
        trho <- vapply(idres, function(x) x[["trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        pvalues.trho <- vapply(idres, function(x) x[["p.trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        n.logit <- length(trho)
        l_stat <-  c("L*" = sum(log(pvalues.trho / (1 - pvalues.trho)))) # formula (10), Choi (2001), p. 253
        k <- (3 * (5 * n.logit + 4)) / (pi^2 * n.logit * (5 * n.logit + 2))
        stat <- sqrt(k) * l_stat  # formula (13), Choi (2001), p. 254
        parameter <- c("df" = 5 * n.logit + 4)
        pvalue <- pt(stat, df = parameter, lower.tail = TRUE)
        adjval <- NULL
    }

    htest <- structure(list(statistic     = stat,
                            parameter     = parameter,
                            alternative   = alternative,
                            data.name     = data.name,
                            method        = method,
                            p.value       = pvalue,
                            ips.tbar.crit = ips.tbar.crit),
                       class = "htest")

    result <- list(statistic = htest,
                   call      = cl,
                   args      = args,
                   idres     = idres,
                   adjval    = adjval,
                   sigma2    = sigma2)
    class(result) <- "purtest"
    result
}


#' @rdname purtest
#' @export
print.purtest <- function(x, ...){
    print(x$statistic, ...)
    if (x$args$test == "ips" && x$args$ips.stat == "tbar"){
        cat("tbar critival values:\n")
        print(x$statistic$ips.tbar.crit, ...)
    }
    invisible(x)
}

#' @rdname purtest
#' @export
summary.purtest <- function(object, ...){
    if(!object$args$test == "hadri"){
        lags   <- vapply(object$idres, function(x) x[["lags"]],   FUN.VALUE = 0.0, USE.NAMES = FALSE)
        L      <- vapply(object$idres, function(x) x[["T"]],      FUN.VALUE = 0.0, USE.NAMES = FALSE)
        rho    <- vapply(object$idres, function(x) x[["rho"]],    FUN.VALUE = 0.0, USE.NAMES = FALSE)
        trho   <- vapply(object$idres, function(x) x[["trho"]],   FUN.VALUE = 0.0, USE.NAMES = FALSE)
        p.trho <- vapply(object$idres, function(x) x[["p.trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        sumidres <- cbind("lags"   = lags,
                          "obs"    = L - lags - 1,
                          "rho"    = rho,
                          "trho"   = trho,
                          "p.trho" = p.trho)

        if (object$args$test == "ips" && !object$args$ips.stat == "tbar") {
            sumidres <- cbind(sumidres, t(object$adjval))
        }
        if (object$args$test == "levinlin") {
            sumidres <- cbind(sumidres, object$sigma2)
        }
    } else {
        # hadri case
        LM     <- vapply(object$idres, function(x) x[["LM"]],     FUN.VALUE = 0.0, USE.NAMES = FALSE)
        sigma2 <- vapply(object$idres, function(x) x[["sigma2"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        sumidres <- cbind("LM" = LM, "sigma2" = sigma2)
    }

    nam <- names(object$idres)
    rownames(sumidres) <- nam
    object$sumidres <- sumidres
    class(object) <- c("summary.purtest", "purtest")
    object
}

#' @rdname purtest
#' @export
print.summary.purtest <- function(x, ...){
    cat(paste(purtest.names.test[x$args$test], "\n"))
    cat(paste("Exogenous variables:", purtest.names.exo[x$args$exo], "\n"))
    if (x$args$test != "hadri") {
        thelags <- vapply(x$idres, function(x) x[["lags"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        if (is.character(x$args$lags)){
            lagselectionmethod <- if (x$args$lags == "Hall") "Hall's method" else x$args$lags
            cat(paste0("Automatic selection of lags using ", lagselectionmethod, ": ",
                       min(thelags), " - ", max(thelags), " lags (max: ", x$args$pmax, ")\n"))
        }
        else{
            cat("User-provided lags\n")
        }
    }

    if (x$args$test == "ips") {
        cat(paste(paste0("statistic (", x$args$ips.stat,"):"), round(x$statistic$statistic, 3), "\n"))
    } else {
        cat(paste("statistic:", round(x$statistic$statistic, 3), "\n"))
    }
    cat(paste("p-value:", round(x$statistic$p.value, 3),   "\n"))
    if (x$args$test == "ips" && x$args$ips.stat == "tbar"){
        cat("tbar critival values:\n")
        print(x$statistic$ips.tbar.crit, ...)
    }
    cat("\n")
    print(x$sumidres, ...)
    invisible(x)
}





#' Simes Test for unit roots in panel data
#'
#' Simes' test of intersection of individual hypothesis tests
#' (\insertCite{SIMES:86;textual}{plm}) applied to panel unit root tests as suggested by
#' \insertCite{HANCK:13;textual}{plm}.
#'
#' Simes' approach to testing is combining p-values from single hypothesis tests
#' with a global (intersected) hypothesis. \insertCite{HANCK:13;textual}{plm}
#' mentions it can be applied to any panel unit root test which yield a p-value
#' for each individual series.
#' The test is robust versus general patterns of cross-sectional dependence.
#'
#' Further, this approach allows to discriminate between individuals for which
#' the individual H0 (unit root present for individual series) is rejected/is
#' not rejected by Hommel's procedure (\insertCite{HOMM:88;textual}{plm}) for
#' family-wise error rate control (FWER) at pre-specified significance level
#' alpha via argument `alpha` (defaulting to `0.05`), i.e., it controls for the
#' multiplicity in testing.
#'
#' The function `phansitest` takes as main input `object` either a plain numeric
#' containing p-values of individual tests or a `purtest` object which holds
#' a suitable pre-computed panel unit root test (one that produces p-values per
#' individual series).
#'
#' The function's return value (see section Value) is a list with detailed
#' evaluation of the applied Simes test.
#'
#' The associated `print` method prints a verbal evaluation.
#'
#' @aliases phansitest
#' @param object either a numeric containing p-values of individual unit root
#' test results (does not need to be sorted) or a suitable `purtest` object
#' (as produced by `purtest()` for a test which gives p-values of the individuals
#' (Hadri's test in `purtest` is not suitable)),
#' @param alpha numeric, the pre-specified significance level (defaults to `0.05`),
#' @param x an object of class `c("phansitest", "list")` as produced by
#'          `phansitest` to be printed,
#' @param cutoff integer, cutoff value for printing of enumeration of individuals with
#' rejected individual H0, for print method only,
#' @param \dots further arguments (currently not used).
#'
#' @return For `phansitest`, an object of class `c("phansitest", "list")` which i
#' s a list with the elements:
#' - `id`: integer, the identifier of the individual (integer sequence referring to
#' position in input),
#' - `name`: character, name of the input's individual (if it has a name,
#' otherwise "1", "2", "3", ...),
#' - `p`: numeric, p-values as input (either the numeric or extracted from
#' the purtest object),
#' - `p.hommel`: numeric, p-values after Hommel's transformation,
#' - `rejected`: logical, indicating for which individual the individual null
#' hypothesis is rejected (`TRUE`)/non-rejected (`FALSE`) (after controlling
#' for multiplicity),
#' - `rejected.no`: integer, giving the total number of rejected individual series,
#' - `alpha`: numeric, the input `alpha`.
#'
#' @export
#' @importFrom stats p.adjust
#'
#' @author Kevin Tappe
#' @seealso [purtest()], [cipstest()]
#'
#' @references
#' \insertAllCited{}
#'
#' @keywords htest
#
#' @examples
#'
#' ### input is numeric (p-values)
#' #### example from Hanck (2013), Table 11 (left side)
#' pvals <- c(0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0050,0.0050,0.0050,
#'            0.0050,0.0175,0.0175,0.0200,0.0250,0.0400,0.0500,0.0575,0.2375,0.2475)
#'
#' countries <- c("Argentina","Sweden","Norway","Mexico","Italy","Finland","France",
#'               "Germany","Belgium","U.K.","Brazil","Australia","Netherlands",
#'               "Portugal","Canada", "Spain","Denmark","Switzerland","Japan")
#' names(pvals) <- countries
#'
#' h <- phansitest(pvals)
#' print(h)              # (explicitly) prints test's evaluation
#' print(h, cutoff = 3L) # print only first 3 rejected ids
#' h$rejected # logical indicating the individuals with rejected individual H0
#'
#'
#' ### input is a (suitable) purtest object
#' data("Grunfeld", package = "plm")
#' y <- data.frame(split(Grunfeld$inv, Grunfeld$firm))
#' obj <- purtest(y, pmax = 4, exo = "intercept", test = "madwu")
#'
#' phansitest(obj)
#'
phansitest <- function(object, alpha = 0.05) {

    is.purtest <- if(inherits(object, "purtest")) TRUE else FALSE
    if(!is.purtest) {
        if(is.numeric(object)) {
            if(anyNA(object)) stop("input p-values in 'object' contain at least one NA/NaN value")
            n <- length(object)
            p <- object
        } else {
            stop("argument 'object' needs to specify either a 'purtest' object or a numeric")
        }
    } else {
        # purtest object
        if(object$args$test == "hadri") stop("phansitest() [Hanck/Simes' test] not possible for purtest objects based on Hadri's test")
        p <- vapply(object$idres, function(x) x[["p.trho"]], FUN.VALUE = 0.0, USE.NAMES = FALSE)
        n <- length(p)
    }

    id <- seq_len(n)
    names(id) <- if(!is.null(names(p))) names(p) else id

    p.hommel <- p.adjust(p, method = "hommel")
    rejected.ind <- p.hommel <= alpha    # is TRUE for individual-H0-rejected individuals
    rejected.ind.no <- sum(rejected.ind) # number of rejected individuals

    res <- structure(list(id           = id,
                          name         = names(id),
                          p            = p,
                          p.hommel     = p.hommel,
                          rejected     = rejected.ind,
                          rejected.no  = rejected.ind.no,
                          alpha        = alpha),
                     class = c("phansitest", "list"))
    return(res)
}

phansi <- function(object, alpha = 0.05) {
    .Deprecated(new = "phansitest", msg = "function 'phansi' renamed to 'phansitest'. Change your code to use 'phansitest'.",
                old = "phansi")
    phansitest(object, alpha = alpha)
}


#' @rdname phansitest
#' @export
print.phansitest <- function(x, cutoff = 10L, ...) {
    if(round(cutoff) != cutoff) stop("Argument 'cutoff' has to be an integer")
    id         <- x$id
    alpha      <- x$alpha
    rej.ind    <- x$rejected
    rej.ind.no <- x$rejected.no
    n <- length(rej.ind)
    H0.txt <- "H0: All individual series have a unit root\n"
    HA.txt <- "HA: Stationarity for at least some individuals\n"
    H0.rej.txt     <- "H0 rejected (globally)"
    H0.not.rej.txt <- "H0 not rejected (globally)"
    test.txt <- "    Simes Test as Panel Unit Root Test (Hanck (2013))"

    cat("\n")
    cat(paste0("    ", test.txt, "\n"))
    cat("\n")
    cat(H0.txt)
    cat(HA.txt)
    cat("\n")
    cat(paste0("Alpha: ", alpha, "\n"))
    cat(paste0("Number of individuals: ", n, "\n"))

    cat("\n")
    cat("Evaluation:\n")
    if(rej.ind.no > 0L) {
        cat(paste0(" ", H0.rej.txt, "\n"))
        cat("\n")

        if(rej.ind.no <= cutoff && cutoff >= 0L) {
            ind.cutoff <- paste0(paste0(id[rej.ind], collapse = ", "))
            ind.txt <- paste0("Individual H0 rejected for ", rej.ind.no, " individual(s) (integer id(s)):\n")
            cat(paste0(" ", ind.txt))
            cat(paste0("  ", ind.cutoff, "\n"))
        }
        else { # cut off enumeration of individuals if more than specified in cutoff
            if(cutoff > 0L) {
                ind.cutoff <- paste0(paste0(id[rej.ind][seq_len(cutoff)], collapse = ", "), ", ...")
                ind.txt <- paste0("Individual H0 rejected for ", rej.ind.no ," individuals, only first ", cutoff, " printed (integer id(s)):\n")
                cat(paste0(" ", ind.txt))
                cat(paste0("  ", ind.cutoff, "\n"))
            } else cat(paste0(" Individual H0 rejected for ", rej.ind.no ," individuals. None printed as 'cutoff' set to ", cutoff, ".\n"))
        }
    } else {
        cat(paste0(" ", H0.rej.txt, "\n"))
    }
    invisible(x)
}


# tool_argvalues.R#
## This file contain named vectors of the acceptable values for different
## arguments used in plm functions.


random.method.list <- c(swar    = "Swamy-Arora",
                        walhus  = "Wallace-Hussain",
                        amemiya = "Amemiya",
                        nerlove = "Nerlove",
                        ht      = "Hausman-Taylor")

effect.plm.list <- c(individual = "Oneway (individual) effect",
                     time       = "Oneway (time) effect",
                     twoways    = "Twoways effects",
                     nested     = "Nested effects")

effect.pvcm.list <- c(individual  = "Oneway (individual) effect",
                      time        = "Oneway (time) effect")

effect.pggls.list <- c(individual = "Oneway (individual) effect",
                       time       = "Oneway (time) effect")

effect.pgmm.list <- c(individual = "Oneway (individual) effect",
                      twoways    = "Twoways effects")

model.plm.list <- c(pooling = "Pooling",
                    within  = "Within",
                    between = "Between",
                    random  = "Random Effect",
                    ht      = "Hausman-Taylor",
                    fd      = "First-Difference")

ht.method.list <- c(ht = "Hausman-Taylor estimator",
                    am = "Amemiya-MaCurdy estimator",
                    bms = "Breusch-Mizon-Schmidt estimator")

model.pvcm.list <- c(within = "No-pooling model",
                     random = "Random coefficients model")

model.pggls.list <- c(within  = "Within FGLS model",
                      random  = "General FGLS model",
                      pooling = "General FGLS model",
                      fd      = "First-Difference FGLS model")

model.pgmm.list <- c(onestep  = "One-step model",
                     twosteps = "Two-steps model")

model.pgmm.transformation.list <- c(d  = "Difference GMM",
                                    ld = "System GMM")

model.pcce.list <- c(ccemg = "Mean Groups model",
                     ccep  = "Pooled model")

model.pmg.list <- c(mg  = "Mean Groups model",
                    dmg = "Demeaned Mean Groups model",
                    cmg = "Common Correlated Effects Mean Groups model")

inst.method.list <- c(bvk     = "Balestra-Varadharajan-Krishnakumar",
                      baltagi = "Baltagi",
                      am      = "Amemiya-MaCurdy",
                      bms     = "Breusch-Mizon-Schmidt")

robust.list <- c(white1   = "White 1",
                 white2   = "White 2",
                 arellano = "Arellano")

weights.list <- c(HC0 = "HC0",
                  HC1 = "HC1",
                  HC2 = "HC2",
                  HC3 = "HC3",
                  HC4 = "HC4")

oneof <- function(x){
    x <- names(x)
    last <- x[length(x)]
    x <- x[-length(x)]
    x <- paste(x,collapse=", ")
    x <- paste(x,last,sep=" and ")
    x
}

plm.arg <- c("formula", "data", "subset", "weights", "na.action", "effect", "model",
             "instruments", "random.method", "inst.method", "index")

# tool_ercomp.R#
#' Estimation of the error components
#'
#' This function enables the estimation of the variance components of a panel
#' model.
#'
#'
#' @aliases ercomp
#' @param object a `formula` or a `plm` object,
#' @param data a `data.frame`,
#' @param effect the effects introduced in the model, see [plm()] for
#'     details,
#' @param method method of estimation for the variance components, see
#'     [plm()] for details,
#' @param models the models used to estimate the variance components
#'     (an alternative to the previous argument),
#' @param dfcor a numeric vector of length 2 indicating which degree
#'     of freedom should be used,
#' @param index the indexes,
#' @param x an `ercomp` object,
#' @param digits digits,
#' @param \dots further arguments.
#' @return An object of class `"ercomp"`: a list containing \itemize{
#'     \item `sigma2` a named numeric with estimates of the variance
#'     components, \item `theta` contains the parameter(s) used for
#'     the transformation of the variables: For a one-way model, a
#'     numeric corresponding to the selected effect (individual or
#'     time); for a two-ways model a list of length 3 with the
#'     parameters. In case of a balanced model, the numeric has length
#'     1 while for an unbalanced model, the numerics' length equal the
#'     number of observations. }
#' @export
#' @author Yves Croissant
#' @seealso [plm()] where the estimates of the variance components are
#'     used if a random effects model is estimated
#' @references
#'
#' \insertRef{AMEM:71}{plm}
#'
#' \insertRef{NERLO:71}{plm}
#'
#' \insertRef{SWAM:AROR:72}{plm}
#'
#' \insertRef{WALL:HUSS:69}{plm}
#'
#' @keywords regression
#' @examples
#'
#' data("Produc", package = "plm")
#' # an example of the formula method
#' ercomp(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc,
#'        method = "walhus", effect = "time")
#' # same with the plm method
#' z <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'          data = Produc, random.method = "walhus",
#'          effect = "time", model = "random")
#' ercomp(z)
#' # a two-ways model
#' ercomp(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc,
#'        method = "amemiya", effect = "twoways")
#'
ercomp <- function(object, ...){
    UseMethod("ercomp")
}

#' @rdname ercomp
#' @export
ercomp.plm <- function(object, ...){
    model <- describe(object, "model")
    if (model != "random") stop("ercomp only relevant for random models")
    object$ercomp
}

#' @rdname ercomp
#' @export
ercomp.pdata.frame <- function(object, effect = c("individual", "time", "twoways", "nested"),
                               method = NULL,
                               models = NULL,
                               dfcor = NULL,
                               index = NULL, ...){
    data <- object
    object <- attr(data, "formula")
    ercomp(object, data, effect = effect, method = method, models = models, dfcor = dfcor, index = index, ...)
}

#' @rdname ercomp
#' @export
ercomp.formula <- function(object, data,
                           effect = c("individual", "time", "twoways", "nested"),
                           method = NULL,
                           models = NULL,
                           dfcor = NULL,
                           index = NULL, ...){
    effect <- match.arg(effect)

    if (! inherits(object, "Formula")) object <- as.Formula(object)
    # if the data argument is not a pdata.frame, create it using plm
    if (! inherits(data, "pdata.frame"))
        data <- plm(object, data, model = NA, index = index)
    if(is.null(attr(data, "terms"))) data <- model.frame(data, object)
    # check whether the panel is balanced
    balanced <- is.pbalanced(data)

    # method and models arguments can't be both set
    if (! is.null(method) && ! is.null(models))
        stop("you can't use both, the 'method' and the 'models' arguments")

    # method and models arguments aren't set, use swar
    if (is.null(method) && is.null(models)) method <- "swar"

    # dfcor is set, coerce it to a length 2 vector if necessary
    if (! is.null(dfcor)){
        if (length(dfcor) > 2L) stop("dfcor length should be at most 2")
        if (length(dfcor) == 1L) dfcor <- rep(dfcor, 2L)
        if (! balanced && any(dfcor != 3))
            stop("dfcor should equal 3 for unbalanced panels")
    }

    # we use later a general expression for the three kinds of effects,
    # select the relevant lines

    therows <- switch(effect,
                      "individual" = 1:2,
                      "time"       = c(1, 3),
                      "twoways"    = 1:3)

    if(! is.null(method) && method == "nerlove") {
        ## special case Nerlove estimator with early exit
        if (effect == "nested") stop("nested random effect model not implemented for Nerlove's estimator")
        est <- plm.fit(data, model = "within", effect = effect)
        pdim <- pdim(data)
        N <- pdim$nT$n
        TS <- pdim$nT$T
        O <- pdim$nT$N
        NTS <- N * (effect != "time") + TS * (effect != "individual") - 1 * (effect == "twoways")
        s2nu <- deviance(est) / O
        # NB: Nerlove takes within residual sums of squares divided by #obs without df correction (Baltagi (2013), p. 23/45)
        s2eta <- s2mu <- NULL
        if(balanced) {
            if (effect != "time")
                s2eta <- as.numeric(crossprod(fixef(est, type = "dmean", effect = "individual"))) / (N - 1)
            if (effect != "individual")
                s2mu <- as.numeric(crossprod(fixef(est, type = "dmean", effect = "time"))) / (TS - 1)
            sigma2 <- c(idios = s2nu, id = s2eta, time = s2mu)
            theta <- list()
            if (effect != "time")       theta$id   <- (1 - (1 + TS * sigma2["id"]  / sigma2["idios"]) ^ (-0.5))
            if (effect != "individual") theta$time <- (1 - (1 + N * sigma2["time"] / sigma2["idios"]) ^ (-0.5))
            if (effect == "twoways") {
                theta$total <- theta$id + theta$time - 1 +
                    (1 + N * sigma2["time"] / sigma2["idios"] +
                         TS * sigma2["id"]   / sigma2["idios"]) ^ (-0.5)
                names(theta$total) <- "total"
                # tweak for numerical precision:
                # if either theta$id or theta$time is 0 => theta$total must be zero
                # but in calculation above some precision is lost
                if(    isTRUE(all.equal(sigma2[["time"]], 0, check.attributes = FALSE))
                       || isTRUE(all.equal(sigma2[["id"]],   0, check.attributes = FALSE)))
                    theta$total <- 0
            }
        } else {
            # Nerlove unbalanced as in Cottrell (2017), gretl working paper #4
            # -> use weighting
            # (albeit the formula for unbalanced panels reduces to original
            # Nerlove formula for balanced data, we keep it separated)
            if (effect != "time")
                s2eta <- sum( (fixef(est, type = "dmean", effect = "individual"))^2 *
                                  pdim$Tint$Ti / pdim$nT$N) * (pdim$nT$n/(pdim$nT$n-1))
            if (effect != "individual")
                s2mu <- sum( (fixef(est, type = "dmean", effect = "time"))^2 *
                                 pdim$Tint$nt / pdim$nT$N) * (pdim$nT$T/(pdim$nT$T-1))
            sigma2 <- c(idios = s2nu, id = s2eta, time = s2mu)
            theta <- list()

            # Tns, Nts: full length
            xindex <- unclass(index(data)) # unclass for speed
            ids <- xindex[[1L]]
            tss <- xindex[[2L]]
            Tns <- pdim$Tint$Ti[as.character(ids)]
            Nts <- pdim$Tint$nt[as.character(tss)]

            if (effect != "time")       theta$id   <- (1 - (1 + Tns * sigma2["id"]   / sigma2["idios"]) ^ (-0.5))
            if (effect != "individual") theta$time <- (1 - (1 + Nts * sigma2["time"] / sigma2["idios"]) ^ (-0.5))
            if (effect == "twoways") {
                theta$total <- theta$id + theta$time - 1 +
                    (1 + Nts * sigma2["time"] / sigma2["idios"] +
                         Tns * sigma2["id"]   / sigma2["idios"]) ^ (-0.5)
                names(theta$total) <- paste0(names(theta$id), "-", names(theta$time))
                # tweak for numerical precision:
                # if either theta$id or theta$time is 0 => theta$total must be zero
                # but in calculation above some precision is lost
                if(    isTRUE(all.equal(sigma2[["time"]], 0, check.attributes = FALSE))
                       || isTRUE(all.equal(sigma2[["id"]],   0, check.attributes = FALSE)))
                    theta$total <- 0

            }
        }
        if (effect != "twoways") theta <- theta[[1L]]
        result <- list(sigma2 = sigma2, theta = theta)
        result <- structure(result, class = "ercomp", balanced = balanced, effect = effect)
        return(result)
    } ## end Nerlove case

    if (! is.null(method) && method == "ht"){
        ## special case HT with early exit
        pdim <- pdim(data)
        N <- pdim$nT$n
        TS <- pdim$nT$T
        O <- pdim$nT$N
        wm <- plm.fit(data, effect = "individual", model = "within")
        X <- model.matrix(data, rhs = 1)
        ixid <- unclass(index(data))[[1L]] # unclass for speed
        charixid <- as.character(ixid)
        constants <- apply(X, 2, function(x) all(tapply(x, ixid, is.constant)))
        FES <- fixef(wm, type = "dmean")[charixid]
        XCST <- X[ , constants, drop = FALSE]
        ra <- if(length(object)[2L] > 1L){
            # with instruments
            W1 <- model.matrix(data, rhs = 2)
            twosls(FES, XCST, W1, lm.type = "lm.fit")
        } else{
            # without instruments
            lm.fit(XCST, FES)
        }
        s2nu <- deviance(wm) / (O - N)
        s21 <- as.numeric(crossprod(ra$residuals)) / N # == deviance(ra) / N
        s2eta <- (s21 - s2nu) / TS
        sigma2 <- c(idios = s2nu, id = s2eta)
        theta <- (1 - (1 + TS * sigma2["id"] / sigma2["idios"]) ^ (-0.5))
        result <- list(sigma2 = sigma2, theta = theta)
        result <- structure(result, class = "ercomp", balanced = balanced, effect = effect)
        return(result)
    } ## end HT

    # method argument is used, check its validity and set the relevant
    # models and dfcor
    if (! is.null(method)){
        if (! method %in% c("swar", "walhus", "amemiya"))
            stop(paste(method, "is not a relevant method"))
        if (method == "swar")    models <- c("within",  "Between")
        if (method == "walhus")  models <- c("pooling", "pooling")
        if (method == "amemiya") models <- c("within",  "within")
        if (is.null(dfcor)){
            if (balanced){
                dfcor <- switch(method,
                                "swar"    = c(2L, 2L),
                                "walhus"  = c(1L, 1L),
                                "amemiya" = c(1L, 1L))
            }
            else dfcor <- c(3L, 3L)
        }
    }
    else{
        # the between estimator is only relevant for the second
        # quadratic form
        if (models[1L] %in% c("Between", "between"))
            stop("the between estimator is only relevant for the between quadratic form")
        # if the argument is of length 2, duplicate the second value
        if (length(models) == 2L) models <- c(models[1L], rep(models[2L], 2L))
        # if the argument is of length 1, triple its value
        if (length(models) == 1L) models <- c(rep(models, 3L))
        # set one of the last two values to NA in the case of one way
        # model
        if (effect == "individual") models[3L] <- NA
        if (effect == "time")       models[2L] <- NA
        # default value of dfcor 3,3
        if (is.null(dfcor)) dfcor <- c(3L, 3L)
    }

    # The nested error component model
    if (effect == "nested"){
        xindex <- unclass(attr(data, "index")) # unclass for speed
        ids <- xindex[[1L]]
        tss <- xindex[[2L]]
        gps <- xindex[[3L]]
        G <- length(unique(gps))
        Z <- model.matrix(data, model = "pooling")
        X <- model.matrix(data, model = "pooling", cstcovar.rm = "intercept")
        y <- pmodel.response(data, model = "pooling", effect = "individual")
        O <- nrow(Z)
        K <- ncol(Z) - (ncol(Z) - ncol(X))
        pdim <- pdim(data)
        N <- pdim$nT$n
        TS <- pdim$nT$T
        TG <- unique(data.frame(tss, gps))
        TG <- table(TG$gps)
        NG <- unique(data.frame(ids, gps))
        NG <- table(NG$gps)
        Tn <- pdim$Tint$Ti
        Nt <- pdim$Tint$nt
        quad <- vector(length = 3L, mode = "numeric")

        M <- matrix(NA_real_, nrow = 3L, ncol = 3L,
                    dimnames = list(c("w", "id", "gp"),
                                    c("nu", "eta", "lambda")))

        if (method == "walhus"){
            estm <- plm.fit(data, model = "pooling", effect = "individual")
            hateps <- resid(estm, model = "pooling")
            Between.hateps.group <- Between(hateps, effect = "group")
            quad <- c(crossprod(Within(hateps, effect = "individual")),
                      crossprod(Between(hateps, effect = "individual") - Between.hateps.group),
                      crossprod(Between.hateps.group))
            ZSeta <- model.matrix(estm, model = "Sum", effect = "individual")
            ZSlambda <- Sum(Z, effect = "group")
            CPZM <- solve(crossprod(Z))
            CPZSeta    <- crossprod(ZSeta,    Z)
            CPZSlambda <- crossprod(ZSlambda, Z)
            Between.Z.ind   <- Between(Z, "individual")
            Between.Z.group <- Between(Z, "group")
            Between.Z.ind_minus_Between.Z.group <- Between.Z.ind - Between.Z.group
            CPZW <- crossprod(Z - Between.Z.ind)
            CPZBlambda <- crossprod(Between.Z.group)
            CPZM.CPZW       <- crossprod(CPZM, CPZW)
            CPZM.CPZBlamda  <- crossprod(CPZM, CPZBlambda)
            CPZM.CPZSeta    <- crossprod(CPZM, CPZSeta)
            CPZM.CPZSlambda <- crossprod(CPZM, CPZSlambda)
            CPZM.CPZW.CPZM.CPZSeta    <- crossprod(t(CPZM.CPZW), CPZM.CPZSeta)
            CPZM.CPZW.CPZM.CPZSlambda <- crossprod(t(CPZM.CPZW), CPZM.CPZSlambda)

            CPZBetaBlambda     <- crossprod(Between.Z.ind_minus_Between.Z.group)
            CPZBetaBlambdaSeta <- crossprod(Between.Z.ind_minus_Between.Z.group, ZSeta)
            CPZBlambdaSeta     <- crossprod(Between.Z.group, ZSeta)

            CPZM.CPZBetaBlambda <- crossprod(CPZM, CPZBetaBlambda)
            CPZM.CPZBlambda     <- crossprod(CPZM, CPZBlambda)

            M["w", "nu"]      <- O - N - trace(CPZM.CPZW)
            M["w", "eta"]     <- trace(CPZM.CPZW.CPZM.CPZSeta)
            M["w", "lambda"]  <- trace(CPZM.CPZW.CPZM.CPZSlambda)
            M["id", "nu"]     <- N - G - trace(CPZM.CPZBetaBlambda)
            M["id", "eta"]    <- O - sum(TG) - 2 * trace(crossprod(CPZM, CPZBetaBlambdaSeta)) +
                trace(crossprod(t(CPZM.CPZBetaBlambda), CPZM.CPZSeta))
            M["id", "lambda"] <- trace(crossprod(t(CPZM.CPZBetaBlambda), CPZM.CPZSlambda))
            M["gp", "nu"]     <- G - trace(CPZM.CPZBlambda)
            M["gp", "eta"]    <- sum(TG) - 2 * trace(crossprod(CPZM, CPZBlambdaSeta)) +
                trace(crossprod(t(CPZM.CPZBlambda), CPZM.CPZSeta))
            M["gp", "lambda"] <- O - 2 * trace(CPZM.CPZSlambda) +
                trace(crossprod(t(CPZM.CPZBlambda), CPZM.CPZSlambda))
        }

        if (method == "amemiya"){
            estm <- plm.fit(data, effect = "individual", model = "within")
            hateps <- resid(estm, model = "pooling")
            Betweeen.hateps.group <- Between(hateps, effect = "group")
            XBlambda <- Between(X, "group")
            quad <- c(crossprod(Within(hateps, effect = "individual")),
                      crossprod(Between(hateps, effect = "individual") - Betweeen.hateps.group),
                      crossprod(Betweeen.hateps.group))
            WX <- model.matrix(estm, model = "within", effect = "individual", cstcovar.rm = "all")
            XBetaBlambda <- Between(X, "individual") - XBlambda
            XBlambda <- t(t(XBlambda) - colMeans(XBlambda))
            CPXBlambda <- crossprod(XBlambda)
            CPXM <- solve(crossprod(WX))
            CPXBetaBlambda <- crossprod(XBetaBlambda)
            K <- ncol(WX)
            MK <- length(setdiff("(Intercept)", attr(WX, "constant"))) # Pas sur, a verifier
            KW <- ncol(WX)
            M["w", "nu"]      <- O - N - K + MK
            M["w", "eta"]     <- 0
            M["w", "lambda"]  <- 0
            M["id", "nu"]     <- N - G + trace(crossprod(CPXM, CPXBetaBlambda))
            M["id", "eta"]    <- O - sum(TG)
            M["id", "lambda"] <- 0
            M["gp", "nu"]     <- G - 1 + trace(crossprod(CPXM, CPXBlambda))
            M["gp", "eta"]    <- sum(TG) - sum(NG     * TG ^ 2) / O
            M["gp", "lambda"] <- O       - sum(NG ^ 2 * TG ^ 2) / O
        }

        if (method == "swar"){
            yBetaBlambda <- pmodel.response(data, model = "Between", effect = "individual") -
                pmodel.response(data, model = "Between", effect = "group")
            ZBlambda <- Between(Z, "group")
            CPZBlambda.solve <- solve(crossprod(ZBlambda))
            ZBetaBlambda <- Between(Z, "individual") - ZBlambda
            XBetaBlambda <- Between(X, "individual") - Between(X, "group")
            yBlambda <- pmodel.response(data, model = "Between", effect = "group")
            ZSeta <- Sum(Z, effect = "individual")
            ZSlambda <- Sum(Z, effect = "group")
            XSeta <- Sum(X, effect = "individual")
            estm1 <- plm.fit(data, effect = "individual", model = "within")
            estm2 <- lm.fit(ZBetaBlambda, yBetaBlambda)
            estm3 <- lm.fit(ZBlambda, yBlambda)
            quad <- c(crossprod(estm1$residuals),
                      crossprod(estm2$residuals),
                      crossprod(estm3$residuals))
            M["w", "nu"]      <- O - N - K
            M["w", "eta"]     <- 0
            M["w", "lambda"]  <- 0
            M["id", "nu"]     <- N - G - K
            M["id", "eta"]    <- O - sum(TG) - trace(crossprod(t(solve(crossprod(XBetaBlambda))), crossprod(XSeta, XBetaBlambda)))
            M["id", "lambda"] <- 0
            M["gp", "nu"]     <- G - K - 1
            M["gp", "eta"]    <- sum(TG) - trace(crossprod(t(CPZBlambda.solve), crossprod(ZBlambda, ZSeta)))
            M["gp", "lambda"] <- O       - trace(crossprod(t(CPZBlambda.solve), crossprod(ZSlambda, Z)))
        }
        Gs <- as.numeric(table(gps)[as.character(gps)])
        Tn <- as.numeric(table(ids)[as.character(ids)])
        sigma2 <- as.numeric(solve(M, quad))
        names(sigma2) <- c("idios", "id", "gp")
        theta <- list(id = 1 - sqrt(sigma2["idios"] / (Tn * sigma2["id"] + sigma2["idios"])),
                      gp = sqrt(sigma2["idios"] / (Tn * sigma2["id"] + sigma2["idios"])) -
                          sqrt(sigma2["idios"] / (Gs * sigma2["gp"] + Tn * sigma2["id"] + sigma2["idios"]))
        )
        result <- list(sigma2 = sigma2, theta = theta)
        return(structure(result, class = "ercomp", balanced = balanced, effect = effect))
    } ### END nested models

    # the "classic" error component model
    Z <- model.matrix(data)
    O <- nrow(Z)
    K <- ncol(Z) - 1  # INTERCEPT
    pdim <- pdim(data)
    N <- pdim$nT$n
    TS <- pdim$nT$T
    NTS <- N * (effect != "time") + TS * (effect != "individual") - 1 * (effect == "twoways")
    Tn <- pdim$Tint$Ti
    Nt <- pdim$Tint$nt
    # Estimate the relevant models
    estm <- vector(length = 3L, mode = "list")
    estm[[1L]] <- plm.fit(data, model = models[1L], effect = effect)
    # Check what is the second model
    secmod <- na.omit(models[2:3])[1L]
    if (secmod %in% c("within", "pooling")){
        amodel <- plm.fit(data, model = secmod, effect = effect)
        if (effect != "time")       estm[[2L]] <- amodel
        if (effect != "individual") estm[[3L]] <- amodel
    }
    if (secmod %in% c("between", "Between")){
        if (effect != "time")       estm[[2L]] <- plm.fit(data, model = secmod, effect = "individual")
        if (effect != "individual") estm[[3L]] <- plm.fit(data, model = secmod, effect = "time")
        # check if Between model was estimated correctly
        swar_Between_check(estm[[2L]], method)
        swar_Between_check(estm[[3L]], method)
    }
    KS <- vapply(estm, function(x) { length(x$coefficients) - "(Intercept)" %in% names(x$coefficients) },
                 FUN.VALUE = 0.0, USE.NAMES = FALSE)

    quad <- vector(length = 3L, mode = "numeric")
    # first quadratic form, within transformation
    hateps_w <- resid(estm[[1L]], model = "pooling")
    quad[1L] <- crossprod(Within(hateps_w, effect = effect))
    # second quadratic form, between transformation
    if (effect != "time"){
        hateps_id <- resid(estm[[2L]], model = "pooling")
        quad[2L] <- as.numeric(crossprod(Between(hateps_id, effect = "individual")))
    }
    if (effect != "individual"){
        hateps_ts <- resid(estm[[3L]], model = "pooling")
        quad[3L] <- as.numeric(crossprod(Between(hateps_ts, effect = "time")))
    }
    M <- matrix(NA_real_, nrow = 3L, ncol = 3L,
                dimnames = list(c("w", "id", "ts"),
                                c("nu", "eta", "mu")))
    # Compute the M matrix :
    ## (    q_w)    ( w_nu      w_eta     w_mu    )   ( s^2_nu )
    ## |       |  = |                             |   |        |
    ## (  q_bid)    ( bid_nu    bid_eta   bid_mu  )   ( s^2_eta)
    ## |       |  = |                             |   |        |
    ## (q_btime)    ( btime_nu  btime_eta btime_mu)   ( s^2_mu )
    # In case of balanced panels, simple denominators are
    # available if dfcor < 3

    if (dfcor[1L] != 3L){
        # The number of time series in the balanced panel is replaced
        # by the harmonic mean of the number of time series in case of
        # unbalanced panels
        barT <- if(balanced) TS else { length(Tn) / sum(Tn ^ (- 1)) }
        M["w", "nu"] <- O
        if (dfcor[1L] == 1L) M["w", "nu"] <- M["w", "nu"] - NTS
        if (dfcor[1L] == 2L) M["w", "nu"] <- M["w", "nu"] - NTS - KS[1L]
        if (effect != "time"){
            M["w", "eta"] <- 0
            M["id", "nu"] <- if(dfcor[2L] == 2L) { N - KS[2L] - 1 } else  N
            M["id", "eta"] <- barT * M["id", "nu"]
        }
        if (effect != "individual"){
            M["w", "mu"] <- 0
            M["ts", "nu"] <- if(dfcor[2L] == 2L) { TS - KS[3L] - 1 } else  TS
            M["ts", "mu"] <- N * M["ts", "nu"]
        }
        if (effect == "twoways") {
            M["ts", "eta"] <- M["id", "mu"] <- 0
        }
    }
    else{
        # General case, compute the unbiased version of the estimators
        if ("pooling" %in% models){
            mp <- match("pooling", models)
            Z <- model.matrix(estm[[mp]], model = "pooling")
            CPZM <- solve(crossprod(Z))
            if (effect != "time"){
                ZSeta <- model.matrix(estm[[mp]], model = "Sum", effect = "individual")
                CPZSeta <- crossprod(ZSeta, Z)
            }
            if (effect != "individual"){
                ZSmu <- model.matrix(estm[[mp]], model = "Sum", effect = "time")
                CPZSmu <- crossprod(ZSmu, Z)
            }
        }
        if (models[1L] == "pooling"){
            ZW <- model.matrix(estm[[1L]], model = "within", effect = effect, cstcovar.rm = "none")
            CPZW <- crossprod(ZW)
            CPZM.CPZW    <- crossprod(CPZM, CPZW)
            M["w", "nu"] <- O - NTS - trace(CPZM.CPZW)
            if (effect != "time"){
                CPZM.CPZSeta <- crossprod(CPZM, CPZSeta)
                M["w", "eta"] <- trace(crossprod(t(CPZM.CPZW), CPZM.CPZSeta))
            }
            if (effect != "individual"){
                CPZM.CPZSmu  <- crossprod(CPZM, CPZSmu)
                M["w", "mu"] <- trace(crossprod(t(CPZM.CPZW), CPZM.CPZSmu))
            }
        }
        if (secmod == "pooling"){
            if (effect != "time"){
                ZBeta <- model.matrix(estm[[2L]], model = "Between", effect = "individual")
                CPZBeta <- crossprod(ZBeta)
                CPZM.CPZBeta <- crossprod(CPZM, CPZBeta)
                CPZM.CPZSeta <- crossprod(CPZM, CPZSeta)
                CPZM.CPZBeta.CPZM.CPZSeta <- crossprod(t(CPZM.CPZBeta), CPZM.CPZSeta) # == CPZM %*% CPZBeta %*% CPZM %*% CPZSeta
                M["id", "nu"]  <- N -     trace(CPZM.CPZBeta)
                M["id", "eta"] <- O - 2 * trace(CPZM.CPZSeta) +
                    trace(CPZM.CPZBeta.CPZM.CPZSeta)
            }
            if (effect != "individual"){
                ZBmu <- model.matrix(estm[[3L]], model = "Between", effect = "time")
                CPZBmu <- crossprod(ZBmu)
                CPZM.CPZBmu <- crossprod(CPZM, CPZBmu)
                CPZM.CPZSmu <- crossprod(CPZM, CPZSmu)
                CPZM.CPZBmu.CPZM.CPZSmu <- crossprod(t(CPZM.CPZBmu), CPZM.CPZSmu)
                M["ts", "nu"] <- TS -    trace(CPZM.CPZBmu)
                M["ts", "mu"] <- O - 2 * trace(CPZM.CPZSmu) +
                    trace(CPZM.CPZBmu.CPZM.CPZSmu)
            }
            if (effect == "twoways"){
                CPZBmuSeta <- crossprod(ZBmu, ZSeta)
                CPZBetaSmu <- crossprod(ZBeta, ZSmu)
                CPZM.CPZBetaSmu <- crossprod(CPZM, CPZBetaSmu)
                CPZM.CPZBmuSeta <- crossprod(CPZM, CPZBmuSeta)
                ## These are already calc. by effect != "individual" and effect != "time"
                # CPZM.CPZSmu <- crossprod(CPZM, CPZSmu)
                # CPZM.CPZBmu <- crossprod(CPZM, CPZBmu)
                # CPZM.CPZBeta <- crossprod(CPZM, CPZBeta)
                # CPZM.CPZSeta <- crossprod(CPZM, CPZSeta)
                CPZM.CPZBeta.CPZM.CPZSmu <- crossprod(t(CPZM.CPZBeta), CPZM.CPZSmu) # == CPZM %*% CPZBeta %*% CPZM %*% CPZSmu
                CPZM.CPZBmu.CPZM.CPZSeta <- crossprod(t(CPZM.CPZBmu), CPZM.CPZSeta) # == CPZM %*% CPZBmu %*% CPZM %*% CPZSeta
                M["id", "mu"]  <- N  - 2 * trace(CPZM.CPZBetaSmu) +
                    trace(CPZM.CPZBeta.CPZM.CPZSmu)
                M["ts", "eta"] <- TS - 2 * trace(CPZM.CPZBmuSeta) +
                    trace(CPZM.CPZBmu.CPZM.CPZSeta)
            }
        }
        if ("within" %in% models){
            WX <- model.matrix(estm[[match("within", models)]], model = "within",
                               effect = effect, cstcovar.rm = "all")
            #            K <- ncol(WX)
            #            MK <- length(attr(WX, "constant")) - 1
            KW <- ncol(WX)
            if (models[1L] == "within"){
                M["w", "nu"] <- O - NTS - KW # + MK                                        # INTERCEPT
                if (effect != "time")       M["w", "eta"] <- 0
                if (effect != "individual") M["w", "mu"]  <- 0
            }
            if (secmod == "within"){
                CPXM <- solve(crossprod(WX))
                if (effect != "time"){
                    XBeta <- model.matrix(estm[[2L]], model = "Between",
                                          effect = "individual")[ , -1L, drop = FALSE]    # INTERCEPT
                    XBeta <- t(t(XBeta) - colMeans(XBeta))
                    CPXBeta <- crossprod(XBeta)
                    amemiya_check(CPXM, CPXBeta, method) # catch non-estimable 'amemiya'
                    M["id", "nu"] <- N - 1 + trace( crossprod(CPXM, CPXBeta) )
                    M["id", "eta"] <- O - sum(Tn ^ 2) / O
                }
                if (effect != "individual"){
                    XBmu <- model.matrix(estm[[3L]], model = "Between",
                                         effect = "time")[ , -1L, drop = FALSE]           # INTERCEPT
                    XBmu <- t(t(XBmu) - colMeans(XBmu))
                    CPXBmu <- crossprod(XBmu)
                    amemiya_check(CPXM, CPXBmu, method) # catch non-estimable 'amemiya'
                    M["ts", "nu"] <- TS - 1 + trace( crossprod(CPXM, CPXBmu) )
                    M["ts", "mu"] <- O - sum(Nt ^ 2) / O
                }
                if (effect == "twoways"){
                    M["id", "mu"]  <- N  - sum(Nt ^ 2) / O
                    M["ts", "eta"] <- TS - sum(Tn ^ 2) / O
                }
            }
        } # END if ("within" %in% models)
        if (length(intersect(c("between", "Between"), models))){
            if (effect != "time"){
                Zeta  <- model.matrix(estm[[2L]], model = "pooling", effect = "individual")
                ZBeta <- model.matrix(estm[[2L]], model = "Between", effect = "individual")
                ZSeta <- model.matrix(estm[[2L]], model = "Sum", effect = "individual")
                CPZSeta <- crossprod(ZSeta, Z)
                CPZMeta <- solve(crossprod(ZBeta))
                M["id", "nu"]  <- N - K - 1
                M["id", "eta"] <- O - trace( crossprod(CPZMeta, CPZSeta) )
            }
            if (effect != "individual"){
                Zmu  <- model.matrix(estm[[3L]], model = "pooling", effect = "time")
                ZBmu <- model.matrix(estm[[3L]], model = "Between", effect = "time")
                ZSmu <- model.matrix(estm[[3L]], model = "Sum", effect = "time")
                CPZSmu <- crossprod(ZSmu, Z)
                CPZMmu <- solve(crossprod(ZBmu))
                M["ts", "nu"] <- TS - K - 1
                M["ts", "mu"] <- O - trace( crossprod(CPZMmu, CPZSmu) )
            }
            if (effect == "twoways"){
                if (! balanced){
                    ZSmuBeta <- Sum(ZBeta, effect = "time")
                    ZBetaSmuBeta <- crossprod(ZBeta, ZSmuBeta)
                    ZSetaBmu <- Sum(ZBmu, effect = "individual")
                    ZBmuSetaBmu <- crossprod(ZBmu, ZSetaBmu)
                    M["id", "mu"]  <- N  - trace(crossprod(CPZMeta, ZBetaSmuBeta))
                    M["ts", "eta"] <- TS - trace(crossprod(CPZMmu, ZBmuSetaBmu))
                }
                else M["id", "mu"] <- M["ts", "eta"] <- 0
            }
        }
    } ## END of General case, compute the unbiased version of the estimators
    sigma2 <- as.numeric(solve(M[therows, therows], quad[therows]))
    names(sigma2) <- c("idios", "id", "time")[therows]
    sigma2[sigma2 < 0] <- 0
    theta <- list()
    if (! balanced){
        xindex <- unclass(index(data)) # unclass for speed
        ids <- xindex[[1L]]
        tss <- xindex[[2L]]
        Tns <- Tn[as.character(ids)]
        Nts <- Nt[as.character(tss)]
    }
    else{
        Tns <- TS
        Nts <- N
    }
    if (effect != "time")       theta$id   <- (1 - (1 + Tns * sigma2["id"]   / sigma2["idios"]) ^ (-0.5))
    if (effect != "individual") theta$time <- (1 - (1 + Nts * sigma2["time"] / sigma2["idios"]) ^ (-0.5))
    if (effect == "twoways") {
        theta$total <- theta$id + theta$time - 1 +
            (1 + Nts * sigma2["time"] / sigma2["idios"] +
                 Tns * sigma2["id"]   / sigma2["idios"]) ^ (-0.5)
        names(theta$total) <- if(balanced) "total" else paste0(names(theta$id), "-", names(theta$time))
        # tweak for numerical precision:
        # if either theta$id or theta$time is 0 => theta$total must be zero
        # but in calculation above some precision is lost
        if(     isTRUE(all.equal(sigma2[["time"]], 0, check.attributes = FALSE))
                || isTRUE(all.equal(sigma2[["id"]],   0, check.attributes = FALSE)))
            theta$total <- 0
    }
    if (effect != "twoways") theta <- theta[[1L]]
    result <- list(sigma2 = sigma2, theta = theta)
    structure(result, class = "ercomp", balanced = balanced, effect = effect)
}

#' @rdname ercomp
#' @export
print.ercomp <- function(x, digits = max(3, getOption("digits") - 3), ...){
    effect <- attr(x, "effect")
    balanced <- attr(x, "balanced")
    sigma2 <- x$sigma2
    theta <- x$theta

    if (effect == "twoways"){
        sigma2 <- unlist(sigma2)
        sigma2Table <- cbind(var = sigma2, std.dev = sqrt(sigma2), share = sigma2 / sum(sigma2))
        rownames(sigma2Table) <- c("idiosyncratic", "individual", "time")
    }
    if (effect == "individual"){
        sigma2 <- unlist(sigma2[c("idios", "id")])
        sigma2Table <- cbind(var = sigma2, std.dev = sqrt(sigma2), share = sigma2 / sum(sigma2))
        rownames(sigma2Table) <- c("idiosyncratic", effect)
    }
    if (effect == "time"){
        sigma2 <- unlist(sigma2[c("idios", "time")])
        sigma2Table <- cbind(var = sigma2, std.dev = sqrt(sigma2), share = sigma2 / sum(sigma2))
        rownames(sigma2Table) <- c("idiosyncratic", effect)
    }
    if (effect == "nested"){
        sigma2 <- unlist(sigma2)
        sigma2Table <- cbind(var = sigma2, std.dev = sqrt(sigma2), share = sigma2 / sum(sigma2))
        rownames(sigma2Table) <- c("idiosyncratic", "individual", "group")
    }

    printCoefmat(sigma2Table, digits)

    if (! is.null(x$theta)){
        if (effect %in% c("individual", "time")){
            if (balanced){
                cat(paste("theta: ", signif(x$theta,digits), "\n", sep = ""))
            }
            else{
                cat("theta:\n")
                print(summary(x$theta))
            }
        }
        if (effect == "twoways"){
            if(balanced){
                cat(paste("theta: ", signif(x$theta$id,digits), " (id) ",
                          signif(x$theta$time,digits), " (time) ",
                          signif(x$theta$total,digits), " (total)\n", sep = ""))
            } else {
                cat("theta:\n")
                print(rbind(id = summary(x$theta$id),
                            time = summary(x$theta$time),
                            total = summary(x$theta$total)))
            }
        }
        if (effect == "nested"){
            cat("theta:\n")
            print(rbind(id = summary(x$theta$id),
                        group = summary(x$theta$gp)))
        }
    }
    invisible(x)
}

amemiya_check <- function(matA, matB, method) {
    ## non-exported, used in ercomp()
    ## little helper function to check matrix multiplication compatibility
    ## in ercomp() for the amemiya estimator: if model contains variables without
    ## within variation (individual or time), the model is not estimable
    if (NROW(matA) < NCOL(matB) && method == "amemiya" ) {
        offending_vars <- setdiff(colnames(matB), rownames(matA))
        offending_vars <- if (length(offending_vars) > 3L) {
            paste0(paste(offending_vars[1:3], collapse = ", "), ", ...")
        } else {
            paste(offending_vars, collapse = ", ")
        }
        stop(paste0("'amemiya' model not estimable due to variable(s) lacking within variation: ", offending_vars))
    } else NULL
}


swar_Between_check <- function(x, method) {
    ## non-exported, used in ercomp()
    ## little helper function to check feasibility of Between model in Swamy-Arora estimation
    ## in ercomp(): if model contains too few groups (individual, time) the Between
    ## model is not estimable (but does not error)
    if (describe(x, "model") %in% c("between", "Between")) {
        pdim <- pdim(x)
        grp <- switch(describe(x, "effect"),
                      "individual" = pdim$nT$n,
                      "time"       = pdim$nT$T)
        # cannot use df.residual(x) here because that gives the number for the "uncompressed" Between model
        if (length(x$aliased) >= grp) stop(paste0("model not estimable: ", length(x$aliased),
                                                  " coefficient(s) (incl. intercept) to be estimated",
                                                  " but only ", grp, " ", describe(x, "effect"), "(s)",
                                                  " in data for the between model necessary for",
                                                  " Swamy-Arora random-effect model estimation"))
    } else NULL
}

# tool_methods.R#
# panelmodel and plm methods :

## panelmodel methods :
# - terms
# - vcov
# - fitted
# - residuals
# - df.residual
# - coef
# - print
# - update
# - deviance
# - nobs

## plm methods :
# - summary
# - print.summary
# - predict
# - formula
# - plot
# - residuals
# - fitted


#' @rdname plm
#' @export
terms.panelmodel <- function(x, ...){
    terms(formula(x))
}

#' @rdname plm
#' @export
vcov.panelmodel <- function(object, ...){
    object$vcov
}

#' @rdname plm
#' @export
fitted.panelmodel <- function(object, ...){
    object$fitted.values
}

#' @rdname plm
#' @export
residuals.panelmodel <- function(object, ...){
    object$residuals
}

#' @rdname plm
#' @export
df.residual.panelmodel <- function(object, ...){
    object$df.residual
}

#' @rdname plm
#' @export
coef.panelmodel <- function(object, ...){
    object$coefficients
}

#' @rdname plm
#' @export
print.panelmodel <- function(x, digits = max(3, getOption("digits") - 2),
                             width = getOption("width"), ...){
    cat("\nModel Formula: ")
    print(formula(x))
    cat("\nCoefficients:\n")
    print(coef(x), digits = digits)
    cat("\n")
    invisible(x)
}


#' Extract Total Number of Observations Used in Estimated Panelmodel
#'
#' This function extracts the total number of 'observations' from a
#' fitted panel model.
#'
#' The number of observations is usually the length of the residuals
#' vector. Thus, `nobs` gives the number of observations actually
#' used by the estimation procedure. It is not necessarily the number
#' of observations of the model frame (number of rows in the model
#' frame), because sometimes the model frame is further reduced by the
#' estimation procedure. This is, e.g., the case for first--difference
#' models estimated by `plm(..., model = "fd")` where the model
#' frame does not yet contain the differences (see also
#' **Examples**).
#'
#' @name nobs.plm
#' @aliases nobs
#' @importFrom stats nobs
#' @export nobs
#' @param object a `panelmodel` object for which the number of
#'     total observations is to be extracted,
#' @param \dots further arguments.
#' @return A single number, normally an integer.
#' @seealso [pdim()]
#' @keywords attribute
#' @examples
#'
#' # estimate a panelmodel
#' data("Produc", package = "plm")
#' z <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,
#'          model="random", subset = gsp > 5000)
#'
#' nobs(z)       # total observations used in estimation
#' pdim(z)$nT$N  # same information
#' pdim(z)       # more information about the dimensions (no. of individuals and time periods)
#'
#' # illustrate difference between nobs and pdim for first-difference model
#' data("Grunfeld", package = "plm")
#' fdmod <- plm(inv ~ value + capital, data = Grunfeld, model = "fd")
#' nobs(fdmod)      # 190
#' pdim(fdmod)$nT$N # 200
#'
NULL

# nobs() function to extract total number of observations used for estimating the panelmodel
# like stats::nobs for lm objects
# NB: here, use object$residuals rather than residuals(object)
#     [b/c the latter could do NA padding once NA padding works for plm objects.
#      NA padded residuals would yield wrong result for nobs!]

#' @rdname nobs.plm
#' @export
nobs.panelmodel <- function(object, ...) {
    if (inherits(object, "plm") || inherits(object, "panelmodel")) return(length(object$residuals))
    else stop("Input 'object' needs to be of class 'plm' or 'panelmodel'")
}

# No of obs calculated as in print.summary.pgmm [code copied from there]
#' @rdname nobs.plm
#' @export
nobs.pgmm <- function(object, ...) {
    if (inherits(object, "pgmm")) return(sum(unlist(object$residuals, use.names = FALSE) != 0))
    else stop("Input 'object' needs to be of class 'pgmm', i. e., a GMM estimation with panel data estimated by pgmm()")
}




# Almost the same as the default method except that update.formula is
# replaced by update, so that the Formula method is used to update the
# formula

#' @rdname plm
#' @export
update.panelmodel <- function (object, formula., ..., evaluate = TRUE){
    if (is.null(call <- object$call)) # was: getCall(object)))
        stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    # update.Formula fails if latter rhs are . ; simplify the formula
    # by removing the latter parts

    if (! missing(formula.)){
        newform <- Formula(formula.)
        if (length(newform)[2L] == 2L && attr(newform, "rhs")[2L] == as.name("."))
            newform <- formula(newform, rhs = 1)
        call$formula <- update(formula(object), newform)
    }
    if (length(extras)) {
        existing <- !is.na(match(names(extras), names(call)))
        for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if (any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
        }
    }
    if (evaluate)
        eval(call, parent.frame())
    else call
}

#' @rdname plm
#' @export
deviance.panelmodel <- function(object, model = NULL, ...){
    if (is.null(model)) as.numeric(crossprod(resid(object)))
    else as.numeric(crossprod(residuals(object, model = model)))
}



# summary.plm creates a specific summary.plm object that is derived
# from the associated plm object


#' Summary for plm objects
#'
#' The summary method for plm objects generates some more information about
#' estimated plm models.
#'
#' The `summary` method for plm objects (`summary.plm`) creates an
#' object of class `c("summary.plm", "plm", "panelmodel")` that
#' extends the plm object it is run on with various information about
#' the estimated model like (inferential) statistics, see
#' **Value**. It has an associated print method
#' (`print.summary.plm`).
#'
#' @aliases summary.plm
#' @param object an object of class `"plm"`,
#' @param x an object of class `"summary.plm"`,
#' @param subset a character or numeric vector indicating a subset of
#'     the table of coefficients to be printed for
#'     `"print.summary.plm"`,
#' @param vcov a variance--covariance matrix furnished by the user or
#'     a function to calculate one (see **Examples**),
#' @param digits number of digits for printed output,
#' @param width the maximum length of the lines in the printed output,
#' @param eq the selected equation for list objects
#' @param \dots further arguments.
#' @return An object of class `c("summary.plm", "plm",
#'     "panelmodel")`.  Some of its elements are carried over from the
#'     associated plm object and described there
#'     ([plm()]). The following elements are new or changed
#'     relative to the elements of a plm object:
#'
#' \item{fstatistic}{'htest' object: joint test of significance of
#' coefficients (F or Chi-square test) (robust statistic in case of
#' supplied argument `vcov`, see [pwaldtest()] for details),}
#'
#' \item{coefficients}{a matrix with the estimated coefficients,
#' standard errors, t--values, and p--values, if argument `vcov` was
#' set to non-`NULL` the standard errors (and t-- and p--values) in
#' their respective robust variant,}
#'
#' \item{vcov}{the "regular" variance--covariance matrix of the coefficients (class "matrix"),}
#'
#' \item{rvcov}{only present if argument `vcov` was set to non-`NULL`:
#' the furnished variance--covariance matrix of the coefficients
#' (class "matrix"),}
#'
#' \item{r.squared}{a named numeric containing the R-squared ("rsq")
#' and the adjusted R-squared ("adjrsq") of the model,}
#'
#' \item{df}{an integer vector with 3 components, (p, n-p, p*), where
#' p is the number of estimated (non-aliased) coefficients of the
#' model, n-p are the residual degrees of freedom (n being number of
#' observations), and p* is the total number of coefficients
#' (incl. any aliased ones).}
#'
#' @export
#' @author Yves Croissant
#' @seealso [plm()] for estimation of various models; [vcovHC()] for
#'     an example of a robust estimation of variance--covariance
#'     matrix; [r.squared()] for the function to calculate R-squared;
#'     [stats::print.power.htest()] for some information about class
#'     "htest"; [fixef()] to compute the fixed effects for "within"
#'     (=fixed effects) models and [within_intercept()] for an
#'     "overall intercept" for such models; [pwaldtest()]
#' @keywords regression
#' @examples
#'
#' data("Produc", package = "plm")
#' zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'           data = Produc, index = c("state","year"))
#' summary(zz)
#'
#' # summary with a furnished vcov, passed as matrix, as function, and
#' # as function with additional argument
#' data("Grunfeld", package = "plm")
#' wi <- plm(inv ~ value + capital,
#'           data = Grunfeld, model="within", effect = "individual")
#' summary(wi, vcov = vcovHC(wi))
#' summary(wi, vcov = vcovHC)
#' summary(wi, vcov = function(x) vcovHC(x, method = "white2"))
#'
#' # extract F statistic
#' wi_summary <- summary(wi)
#' Fstat <- wi_summary[["fstatistic"]]
#'
#' # extract estimates and p-values
#' est <- wi_summary[["coefficients"]][ , "Estimate"]
#' pval <- wi_summary[["coefficients"]][ , "Pr(>|t|)"]
#'
#' # print summary only for coefficent "value"
#' print(wi_summary, subset = "value")
#'
summary.plm <- function(object, vcov = NULL, ...){

    vcov_arg <- vcov
    model <- describe(object, "model")
    effect <- describe(object, "effect")
    random.method <- describe(object, "random.method")

    # determine if intercept-only model (no other regressors)
    coef_wo_int <- object$coefficients[!(names(coef(object)) %in% "(Intercept)")]
    int.only <- !length(coef_wo_int)

    # as cor() is not defined for intercept-only models, use different approach
    # for R-squared ("rss" and "ess" are defined)
    object$r.squared <- if(!int.only) {
        c(rsq    = r.squared(object),
          adjrsq = r.squared(object, dfcor = TRUE))
    } else {
        c(rsq    = r.squared(object, type = "rss"),
          adjrsq = r.squared(object, type = "rss", dfcor = TRUE))
    }

    ## determine if standard normal and Chisq test or t distribution and F test to be used
    ## (normal/chisq for all random models, all IV models, and HT via plm(., model="ht"))
    use.norm.chisq <- if(model == "random" ||
                         length(formula(object))[2L] >= 2L ||
                         model == "ht") TRUE else FALSE

    # perform Wald test of joint sign. of regressors only if there are
    # other regressors besides the intercept
    if(!int.only) {
        object$fstatistic <- pwaldtest(object,
                                       test = if(use.norm.chisq) "Chisq" else "F",
                                       vcov = vcov_arg)
    }


    # construct the table of coefficients
    if (!is.null(vcov_arg)) {
        if (is.matrix(vcov_arg))   rvcov <- vcov_arg
        if (is.function(vcov_arg)) rvcov <- vcov_arg(object)
        std.err <- sqrt(diag(rvcov))
    } else {
        std.err <- sqrt(diag(stats::vcov(object)))
    }
    b <- coefficients(object)
    z <- b / std.err
    p <- if(use.norm.chisq) {
        2 * pnorm(abs(z), lower.tail = FALSE)
    } else {
        2 * pt(abs(z), df = object$df.residual, lower.tail = FALSE)
    }

    # construct the object of class summary.plm
    object$coefficients <- cbind(b, std.err, z, p)
    colnames(object$coefficients) <- if(use.norm.chisq) {
        c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
    } else { c("Estimate", "Std. Error", "t-value", "Pr(>|t|)") }

    ## add some info to summary.plm object
    # robust vcov (next to "normal" vcov)
    if (!is.null(vcov_arg)) {
        object$rvcov <- rvcov
        rvcov.name <- paste0(deparse(substitute(vcov)))
        attr(object$rvcov, which = "rvcov.name") <- rvcov.name
    }

    # mimics summary.lm's 'df' component
    # 1st entry: no. coefs (w/o aliased coefs); 2nd: residual df; 3rd no. coefs /w aliased coefs
    # NB: do not use length(object$coefficients) for 3rd entry!
    object$df <- c(length(b), object$df.residual, length(object$aliased))

    class(object) <- c("summary.plm", "plm", "panelmodel")
    object
}

#' @rdname summary.plm
#' @export
print.summary.plm <- function(x, digits = max(3, getOption("digits") - 2),
                              width = getOption("width"), subset = NULL, ...){
    formula <- formula(x)
    has.instruments <- (length(formula)[2L] >= 2L)
    effect <- describe(x, "effect")
    model  <- describe(x, "model")
    if (model != "pooling") { cat(paste(effect.plm.list[effect], " ", sep = "")) }
    cat(paste(model.plm.list[model], " Model", sep = ""))

    if (model == "random"){
        ercomp <- describe(x, "random.method")
        cat(paste(" \n   (",
                  random.method.list[ercomp],
                  "'s transformation)\n",
                  sep = ""))
    }
    else{
        cat("\n")
    }

    if (has.instruments){
        cat("Instrumental variable estimation\n")
        if(model != "within") {
            # don't print transformation method for FE models as there is only one
            # such method for FE models but plenty for other model types
            ivar <- describe(x, "inst.method")
            cat(paste0("   (", inst.method.list[ivar], "'s transformation)\n"))
        }
    }

    if (!is.null(x$rvcov)) {
        cat("\nNote: Coefficient variance-covariance matrix supplied: ", attr(x$rvcov, which = "rvcov.name"), "\n", sep = "")
    }

    cat("\nCall:\n")
    print(x$call)
    cat("\n")
    pdim <- pdim(x)
    print(pdim)
    if (model %in% c("fd", "between")) {
        # print this extra info, b/c model.frames of FD and between models
        # have original (undifferenced/"un-between-ed") obs/rows of the data
        cat(paste0("Observations used in estimation: ", nobs(x), "\n"))}

    if (model == "random"){
        cat("\nEffects:\n")
        print(x$ercomp)
    }
    cat("\nResiduals:\n")
    df <- x$df
    rdf <- df[2L]
    if (rdf > 5L) {
        save.digits <- unlist(options(digits = digits))
        on.exit(options(digits = save.digits))
        print(sumres(x))
    } else if (rdf > 0L) print(residuals(x), digits = digits)
    if (rdf == 0L) { # estimation is a perfect fit
        cat("ALL", x$df[1L], "residuals are 0: no residual degrees of freedom!")
        cat("\n")
    }

    if (any(x$aliased, na.rm = TRUE)) {
        # na.rm = TRUE because currently, RE tw unbalanced models might have NAs?
        naliased <- sum(x$aliased, na.rm = TRUE)
        cat("\nCoefficients: (", naliased, " dropped because of singularities)\n", sep = "")
    } else cat("\nCoefficients:\n")

    if (is.null(subset)) printCoefmat(coef(x), digits = digits)
    else printCoefmat(coef(x)[subset, , drop = FALSE], digits = digits)
    cat("\n")
    cat(paste("Total Sum of Squares:    ", signif(tss(x),      digits), "\n", sep = ""))
    cat(paste("Residual Sum of Squares: ", signif(deviance(x), digits), "\n", sep = ""))
    cat(paste("R-Squared:      ", signif(x$r.squared[1L], digits),      "\n", sep = ""))
    cat(paste("Adj. R-Squared: ", signif(x$r.squared[2L], digits),      "\n", sep = ""))

    # print Wald test of joint sign. of regressors only if there is a statistic
    # in summary.plm object (not computed by summary.plm if there are no other
    # regressors than the intercept
    if(!is.null(fstat <- x$fstatistic)) {
        if (names(fstat$statistic) == "F"){
            cat(paste("F-statistic: ", signif(fstat$statistic),
                      " on ", fstat$parameter["df1"]," and ", fstat$parameter["df2"],
                      " DF, p-value: ", format.pval(fstat$p.value,digits=digits), "\n", sep=""))
        }
        else{
            cat(paste("Chisq: ", signif(fstat$statistic),
                      " on ", fstat$parameter,
                      " DF, p-value: ", format.pval(fstat$p.value, digits = digits), "\n", sep=""))
        }
    }
    invisible(x)
}

#' @rdname plm
#' @export
predict.plm <- function(object, newdata = NULL, ...){
    tt <- terms(object)
    if (is.null(newdata)){
        result <- fitted(object, ...)
    }
    else{
        Terms <- delete.response(tt)
        m <- model.frame(Terms, newdata)
        X <- model.matrix(Terms, m)
        beta <- coef(object)
        result <- as.numeric(crossprod(beta, t(X)))
    }
    result
}

#' @rdname plm
#' @export
formula.plm <- function(x, ...){
    x$formula
}

#' @rdname plm
#' @export
plot.plm <- function(x, dx = 0.2, N = NULL, seed = 1,
                     within = TRUE, pooling = TRUE,
                     between = FALSE, random = FALSE, ...){
    set.seed(seed)# 8 est bien pour beertax
    subs <- ! is.null(N)
    x <- update(x, model = "within")
    mco <- update(x, model = "pooling")
    if (random) re <- update(x, model = "random")
    if (between) be <- update(x, model = "between")
    pdim <- pdim(x)
    n <- pdim$nT$n
    if (! subs) N <- n
    ids <- unique(index(x, "id"))
    if (subs) ids <- ids[sample(1:length(ids), N, replace = FALSE)]
    sel <- index(x, "id") %in% ids
    T. <- pdim$nT$T
    cols <- rainbow(N)
    pts <- sample(1:25, N, replace = TRUE)
    thex <- as.numeric(model.matrix(x, model = "pooling")[sel, 2L])
    they <- as.numeric(pmodel.response(x, model = "pooling")[sel])
    plot(thex, they, col = rep(cols, each = T.),
         pch = rep(pts, each = T.), ann = FALSE, las = 1)
    idsel <- as.numeric(index(x, "id")[sel])
    meanx <- tapply(thex, idsel, mean)
    meany <- tapply(they, idsel, mean)
    points(meanx, meany, pch = 19, col = cols, cex = 1.5)
    if (within){
        beta <- coef(x)
        alphas <- meany - meanx * beta
        dx <- dx * (max(thex) - min(thex))
        for (i in 1:N){
            xmin <- meanx[i] - dx
            xmax <- meanx[i] + dx
            ymin <- alphas[i] + beta * xmin
            ymax <- alphas[i] + beta * xmax
            lines(c(xmin, xmax), c(ymin, ymax), col = cols[i])
        }
    }
    if(random) abline(coef(re)[1L], coef(re)[2L], lty = "dotted")
    if(pooling) abline(coef(mco), lty = "dashed")
    if(between) abline(coef(be), lty = "dotdash")
    # where to put the legends, depends on the sign of the OLS slope
    modploted <- c(random, pooling, between, within)
    if (sum(modploted)){
        poslegend <- ifelse(beta > 0, "topleft", "topright")
        ltylegend <- c("dotted", "dashed", "dotdash", "solid")[modploted]
        leglegend <- c("random", "pooling", "between", "within")[modploted]
        legend(poslegend, lty = ltylegend, legend = leglegend)
    }
}

#' @rdname plm
#' @export
residuals.plm <- function(object, model = NULL, effect = NULL,  ...){
    if (is.null(model) && is.null(effect)){
        model <- describe(object, "model")
        res <- object$residuals
    }
    else{
        cl <- match.call(expand.dots = FALSE)
        # fitted -> call to the plm method, used to be fitted.plm
        # which is not exported
        #        cl[[1L]] <- as.name("fitted.plm")
        cl[[1L]] <- as.name("fitted")
        bX <- eval(cl, parent.frame())
        if (is.null(model))  model  <- describe(object, "model")
        if (is.null(effect)) effect <- describe(object, "effect")
        y <- pmodel.response(object, model = model, effect = effect)
        res <- y - bX
    }
    res <- if (model %in% c("between", "fd")) {
        # these models "compress" the data, thus an index does not make sense here
        # -> do not return pseries but plain numeric
        res
    } else {
        structure(res, index = index(object), class = unique(c("pseries", class(res))))
    }
    return(res)
}

#' @rdname plm
#' @export
fitted.plm <- function(object, model = NULL, effect = NULL, ...){
    fittedmodel <- describe(object, "model")
    if (is.null(model)) model <- fittedmodel
    if (is.null(effect)) effect <- describe(object, "effect")
    if (fittedmodel == "random") theta <- ercomp(object)$theta else theta <- NULL
    X <- model.matrix(object, model = "pooling")
    y <- pmodel.response(object, model = "pooling", effect = effect)
    beta <- coef(object)
    comonpars <- intersect(names(beta), colnames(X))
    bX <- as.numeric(crossprod(t(X[, comonpars, drop = FALSE]), beta[comonpars]))
    bX <- structure(bX, index = index(object), class = unique(c("pseries", class(bX))))
    if (fittedmodel == "within"){
        intercept <- mean(y - bX)
        bX <- bX + intercept
    }
    ptransform(bX, model = model, effect = effect, theta = theta)
}

# tool_misc.R#
## Function that are used in more than on place in plm (or likely to be used in more than one place in the future)

## - trace : calculate trace of a matrix (used in ercomp())
## - is.constant : check if a numeric vector or columns of a matrix is constant
## - bdiag : takes matrices as argument and returns the block-diagonal matrix (used in pgmm and plm.list)
## - mylm : inner fitting func based on stats::lm with matrix inputs (used in plm.fit)
## - my.lm.fit : like the barebone stats::lm.fit but with some extra information (e.g., SEs, sigma) used in purtest
## - twosls : computes the 2SLS estimator (used in plm and ercomp)
## - data.name : used in a lot tests to generate the 'data.name' entry for htest objects from the model object's call$formula
## - has.intercept : tests the presence of an intercept
## - pres : extract model residuals as pseries (used in several estimation functions)
## - punbalancedness : measures for the unbalancedness of panel data
## - myvar : calculates variance with NA removal, checks if input is constant (also for factor and character)
## - pvar : checks if input varies in individual / time dimension
## - make.dummies : create a contrast-coded dummy matrix from a factor

trace <- function(x) sum(diag(x))

is.constant <- function(x) (max(x) - min(x)) < sqrt(.Machine$double.eps)

bdiag <- function(...){
    ## non-exported
    if (nargs() == 1L)
        x <- as.list(...)
    else
        x <- list(...)
    n <- length(x)
    if(n == 0L) return(NULL)
    x <- lapply(x, function(y) if(length(y)) as.matrix(y) else
        stop("Zero-length component in x"))
    d <- array(unlist(lapply(x, dim)), c(2, n))
    rr <- d[1L, ]
    cc <- d[2L, ]
    rsum <- sum(rr)
    csum <- sum(cc)
    out <- array(0, c(rsum, csum))
    ind <- array(0, c(4, n))
    rcum <- cumsum(rr)
    ccum <- cumsum(cc)
    ind[1, -1] <- rcum[-n]
    ind[2,   ] <- rcum
    ind[3, -1] <- ccum[-n]
    ind[4,   ] <- ccum
    imat <- array(1:(rsum * csum), c(rsum, csum))
    iuse <- apply(ind, 2, function(y, imat) imat[(y[1L]+1):y[2L],
                                                 (y[3L]+1):y[4L]], imat = imat)
    iuse <- as.vector(unlist(iuse))
    out[iuse] <- unlist(x)
    return(out)
}

# mylm is used in plm.fit()
mylm <- function(y, X, W = NULL) {
    ## non-exported
    names.X <- colnames(X)
    result <- if(is.null(W)) lm(y ~ X - 1) else twosls(y, X, W)
    if(any(na.coef <- is.na(result$coefficients))) {
        ## for debug purpose:
        # warning("Coefficient(s) '", paste((names.X)[na.coef], collapse = ", "),
        #"' could not be estimated and is (are) dropped.")
        X <- X[ , !na.coef, drop = FALSE]
        if(dim(X)[2L] == 0L) stop(paste("estimation not possible: all coefficients",
                                        "omitted from estimation due to aliasing"))

        ## re-estimate without the columns which resulted previously in NA-coefficients
        result <- if(is.null(W)) lm(y ~ X - 1) else twosls(y, X, W)
    }
    result$vcov <- vcov(result)
    result$X <- X
    result$y <- y
    result$W <- W
    # aliased is an element of summary.lm-objects:
    # since plm drops aliased coefs, store this info in plm object
    # NB: this only sets coefs to NA that are detected/set to NA by mylm()/lm.fit();
    #     covariates dropped earlier by model.matrix( , cstcovar.rm) are not included here anymore
    result$aliased <- na.coef
    names(result$aliased) <- names.X
    names(result$coefficients) <- colnames(result$vcov) <-
        rownames(result$vcov) <- colnames(X)
    result
}

# my.lm.fit is used in purtest()
my.lm.fit <- function(X, y, dfcor = TRUE, ...){
    reg <- lm.fit(X, y)
    ## 'as' summary method for lm.fit
    p <- reg$rank
    Qr <- reg$qr
    n <- NROW(Qr$qr)
    rdf <- n - p
    p1 <- 1L:p
    r <- reg$residuals
    rss <- as.numeric(crossprod(r))
    resvar <- if (dfcor) rss/rdf else rss/n
    sigma <- sqrt(resvar)
    R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    thecoef <- reg$coefficients[Qr$pivot[p1]] #[lags+1]
    these <- sigma * sqrt(diag(R)) #[lags+1])
    list(coef = thecoef, se = these, sigma = sigma,
         rss = rss, n = n, K = p, rdf = rdf)
}

#' @importFrom stats .lm.fit
twosls <- function(y, X, W, intercept = FALSE, lm.type = "lm"){
    ## non-exported
    # Return value can be controlled by argument lm.type. Often, a full lm model
    # is needed for further processing but can select one of the fast but less
    # rich objects produced by lm.fit or .lm.fit (the latter does not contain, e.g.,
    # fitted.values and is to be used very carefully (e.g., coefs not in input order)).

    # As NA/NaN/(+/-)Inf-freeness needs to be guaranteed when functions call
    # twosls(), so can use lm.fit to calc. Xhat.
    Xhat <- lm.fit(cbind(1, W), X)$fitted.values
    # old: Xhat <- lm(X ~ W)$fitted.values

    if(!is.matrix(Xhat)) {
        # ensure Xhat is a matrix
        Xhat <- matrix(Xhat, ncol = 1L)
        colnames(Xhat) <- colnames(X)
    }

    if(intercept) {
        model <- switch(lm.type,
                        "lm"      =  lm(y ~ Xhat),
                        "lm.fit"  =  lm.fit(cbind(1, Xhat), y),
                        ".lm.fit" = .lm.fit(cbind(1, Xhat), y))
        yhat <- as.vector(crossprod(t(cbind(1, X)), coef(model)))
    }
    else{
        model <- switch(lm.type,
                        "lm"      =  lm(y ~ Xhat - 1),
                        "lm.fit"  =  lm.fit(Xhat, y),
                        ".lm.fit" = .lm.fit(Xhat, y))
        yhat <- as.vector(crossprod(t(X), coef(model)))
    }
    model$residuals <- y - yhat
    model
}

data.name <- function(x) {
    ## non-exported, used in various tests
    data.name <- paste(deparse(x$call$formula))
    if (length(data.name) > 1L) paste(data.name[1L], "...")
    else data.name
}

##### has.intercept methods #####

#' Check for the presence of an intercept in a formula or in a fitted
#' model
#'
#' The presence of an intercept is checked using the formula which is
#' either provided as the argument of the function or extracted from
#' a fitted model.
#'
#' @param object a `formula`, a `Formula` or a fitted model (of class
#'     `plm` or `panelmodel`),
#' @param rhs an integer (length > 1 is possible), indicating the parts of right
#'      hand sides of the formula to be evaluated for the presence of an
#'      intercept or NULL for all parts of the right hand side
#'      (relevant for the `Formula` and the `plm` methods)
#' @param \dots further arguments.
#'
#' @return a logical
#' @export
has.intercept <- function(object, ...) {
    UseMethod("has.intercept")
}

#' @rdname has.intercept
#' @export
has.intercept.default <- function(object, ...) {
    has.intercept(formula(object), ...)
}

#' @rdname has.intercept
#' @export
has.intercept.formula <- function(object, ...) {
    attr(terms(object), "intercept") == 1L
}

#' @rdname has.intercept
#' @export
has.intercept.Formula <- function(object, rhs = NULL, ...) {
    ## NOTE: returns a logical vector of the necessary length
    ## (which might be > 1)
    if (is.null(rhs)) rhs <- 1:length(attr(object, "rhs"))
    res <- sapply(rhs, function(x) {
        aform <- formula(object, lhs = 0, rhs = x)
        # expand the dot if any in all the parts except the first
        if (x > 1L) aform <- update(formula(object, lhs = 0, rhs = 1), aform)
        has.intercept(aform)
    })
    return(res)
}

#' @rdname has.intercept
#' @export
has.intercept.panelmodel <- function(object, ...) {
    object <- attr(model.frame(object), "formula")
    has.intercept(object)
}

#' @rdname has.intercept
#' @export
has.intercept.plm <- function(object, rhs = 1L, ...) {
    has.intercept(formula(object), rhs = rhs, ...)
}


pres <- function(x) {  # pres.panelmodel
    ## extracts model residuals as pseries
    ## not necessary for plm models as residuals.plm returns a pseries,
    ## but used in residuals.pggls, residuals.pcce, residuals.pmg

    ## extract indices
    xindex <- unclass(attr(x$model, "index")) # unclass for speed
    groupind <- xindex[[1L]]
    timeind  <- xindex[[2L]]

    # fix to allow operation with pggls, pmg
    # [TODO: one day, make this cleaner; with the describe framework?]
    if (!is.null(x$args$model))                 maybe_fd <- x$args$model
    if (!is.null(attr(x, "pmodel")$model.name)) maybe_fd <- attr(x, "pmodel")$model.name # this line is currently needed to detect pggls models

    ## Achim's fix: reduce id and time index to accommodate first-differences model's number of observations
    if(exists("maybe_fd") && maybe_fd == "fd") {
        groupi <- as.numeric(groupind)
        ## make vector =1 on first obs in each group, 0 elsewhere
        selector <- groupi - c(0, groupi[-length(groupi)])
        selector[1L] <- 1 # the first must always be 1
        ## eliminate first obs in time for each group
        groupind <- groupind[!selector]
        timeind <- timeind[!selector]
    }

    resdata <- data.frame(ee = x$residuals, ind = groupind, tind = timeind)
    pee <- pdata.frame(resdata, index = c("ind", "tind"))
    pres <- pee$ee
    return(pres)
}


# punbalancedness: measures for unbalancedness of a panel data set as
# defined in Ahrens/Pincus (1981), p. 228 (gamma and
# nu) and for nested panel structures as in Baltagi/Song/Jung (2001), pp. 368-369 .
#
# Ahrens/Pincus (1981), On Two Measures of Unbalancedness in a One-Way Model
#  and Their Relation to Efficiency, Biometrical Journal, Vol. 23, pp. 227-235.
#
# Baltagi/Song/Jung (2001), The unbalanced nested error component regression model,
#  Journal of Econometrics, Vol. 101, pp. 357-381


#' Measures for Unbalancedness of Panel Data
#'
#' This function reports unbalancedness measures for panel data as
#' defined in \insertCite{AHRE:PINC:81;textual}{plm} and
#' \insertCite{BALT:SONG:JUNG:01;textual}{plm}.
#'
#' `punbalancedness` returns measures for the unbalancedness of a
#' panel data set.
#'
#' - For two-dimensional data:\cr The two measures of
#' \insertCite{AHRE:PINC:81;textual}{plm} are calculated, called
#' "gamma" (\eqn{\gamma}) and "nu" (\eqn{\nu}).
#'
#' If the panel data are balanced, both measures equal 1. The more
#' "unbalanced" the panel data, the lower the measures (but > 0). The
#' upper and lower bounds as given in \insertCite{AHRE:PINC:81;textual}{plm}
#' are:\cr
#' \eqn{0 < \gamma, \nu \le 1}, and for \eqn{\nu} more precisely
#' \eqn{\frac{1}{n} < \nu \le 1}{1/n < \nu \le 1}, with \eqn{n} being
#' the number of individuals (as in `pdim(x)$nT$n`).
#'
#' - For nested panel data (meaning including a grouping variable):\cr
#' The extension of the above measures by
#' \insertCite{BALT:SONG:JUNG:01;textual}{plm}, p. 368, are
#' calculated:\cr
#'
#'   - c1: measure of subgroup (individual) unbalancedness,
#'   - c2: measure of time unbalancedness,
#'   - c3: measure of group unbalancedness due to each group size.
#'
#' Values are 1 if the data are balanced and become smaller as the
#' data become more unbalanced.
#'
#'
#' An application of the measure "gamma" is found in e. g.
#' \insertCite{BALT:SONG:JUNG:01;textual}{plm}, pp. 488-491, and
#' \insertCite{BALT:CHAN:94;textual}{plm}, pp. 78--87, where it is
#' used to measure the unbalancedness of various unbalanced data sets
#' used for Monte Carlo simulation studies. Measures c1, c2, c3 are
#' used for similar purposes in
#' \insertCite{BALT:SONG:JUNG:01;textual}{plm}.
#'
#' In the two-dimensional case, `punbalancedness` uses output of
#' [pdim()] to calculate the two unbalancedness measures, so inputs to
#' `punbalancedness` can be whatever `pdim` works on. `pdim` returns
#' detailed information about the number of individuals and time
#' observations (see [pdim()]).
#'
#' @param x a `panelmodel`, a `data.frame`, or a `pdata.frame` object,
#' @param index only relevant for `data.frame` interface, for details
#'     see [pdata.frame()],
#' @param \dots further arguments.
#' @return A named numeric containing either two or three entries,
#'     depending on the panel structure inputted:
#'
#' - For the two-dimensional panel structure, the entries are called
#' `gamma` and `nu`,
#'
#' - For a nested panel structure, the entries are called `c1`, `c2`,
#' `c3`.
#'
#' @note Calling `punbalancedness` on an estimated `panelmodel` object
#'     and on the corresponding `(p)data.frame` used for this
#'     estimation does not necessarily yield the same result (true
#'     also for `pdim`). When called on an estimated `panelmodel`, the
#'     number of observations (individual, time) actually used for
#'     model estimation are taken into account. When called on a
#'     `(p)data.frame`, the rows in the `(p)data.frame` are
#'     considered, disregarding any `NA` values in the dependent or
#'     independent variable(s) which would be dropped during model
#'     estimation.
#' @export
#' @author Kevin Tappe
#' @seealso [nobs()], [pdim()], [pdata.frame()]
#' @references
#'
#' \insertRef{AHRE:PINC:81}{plm}
#'
#' \insertRef{BALT:CHAN:94}{plm}
#'
#' \insertRef{BALT:SONG:JUNG:01}{plm}
#'
#' \insertRef{BALT:SONG:JUNG:02}{plm}
#'
#' @keywords attribute
#' @examples
#'
#' # Grunfeld is a balanced panel, Hedonic is an unbalanced panel
#' data(list=c("Grunfeld", "Hedonic"), package="plm")
#'
#' # Grunfeld has individual and time index in first two columns
#' punbalancedness(Grunfeld) # c(1,1) indicates balanced panel
#' pdim(Grunfeld)$balanced   # TRUE
#'
#' # Hedonic has individual index in column "townid" (in last column)
#' punbalancedness(Hedonic, index="townid") # c(0.472, 0.519)
#' pdim(Hedonic, index="townid")$balanced   # FALSE
#'
#' # punbalancedness on estimated models
#' plm_mod_pool <- plm(inv ~ value + capital, data = Grunfeld)
#' punbalancedness(plm_mod_pool)
#'
#' plm_mod_fe <- plm(inv ~ value + capital, data = Grunfeld[1:99, ], model = "within")
#' punbalancedness(plm_mod_fe)
#'
#' # replicate results for panel data design no. 1 in Ahrens/Pincus (1981), p. 234
#' ind_d1  <- c(1,1,1,2,2,2,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,5,5)
#' time_d1 <- c(1,2,3,1,2,3,1,2,3,4,5,1,2,3,4,5,6,7,1,2,3,4,5,6,7)
#' df_d1 <- data.frame(individual = ind_d1, time = time_d1)
#' punbalancedness(df_d1) # c(0.868, 0.887)
#'
#' # example for a nested panel structure with a third index variable
#' # specifying a group (states are grouped by region) and without grouping
#' data("Produc", package = "plm")
#' punbalancedness(Produc, index = c("state", "year", "region"))
#' punbalancedness(Produc, index = c("state", "year"))
#'
#' @rdname punbalancedness
#' @export
punbalancedness <- function(x, ...) {
    UseMethod("punbalancedness")
}


punbalancedness.default <- function(x, ...) {

    ii <- index(x)
    if(!is.index(ii)) stop("no valid index found for input object 'x'")

    if (ncol(ii) == 2L) {
        ## original Ahrens/Pincus (1981)
        pdim <- pdim(x, ...)
        N <- pdim$nT$n # no. of individuals
        Totalobs <- pdim$nT$N # no. of total observations
        Ti <- pdim$Tint$Ti
        Tavg <- sum(Ti)/N

        r1 <- N / (Tavg * sum(1/Ti))
        r2 <- 1 / (N * (sum( (Ti/Totalobs)^2)))
        result <- c(gamma = r1, nu = r2)
    } else {
        if (ncol(ii) == 3L) {
            ## extension to nested model with additional group variable
            ## Baltagi/Song/Jung (2001), pp. 368-369
            ii <- unclass(ii) # unclass for speed
            ids <- ii[[1L]]
            tss <- ii[[2L]]
            gps <- ii[[3L]]
            Tis <- unique(data.frame(tss, gps))
            Tis <- table(Tis$gps)               # no of max time periods per group
            Nis <- unique(data.frame(ids, gps))
            Nis <- table(Nis$gps)               # no of individuals per group
            M <- length(unique(gps))            # no of unique groups
            Nbar <- sum(Nis)/M
            Tbar <- sum(Tis)/M

            c1 <- M / (Nbar * sum(1/Nis))
            c2 <- M / (Tbar * sum(1/Tis))
            c3 <- M / (sum(Nis * Tis)/M * sum(1/(Nis*Tis)))
            result <- (c(c1 = c1, c2 = c2, c3 = c3))
        } else stop(paste0("unsupported number of dimensions: ", ncol(ii)))
    }
    return(result)
}

#' @rdname punbalancedness
#' @export
punbalancedness.pdata.frame <- function(x, ...) {
    punbalancedness.default(x, ...)
}

#' @rdname punbalancedness
#' @export
punbalancedness.data.frame <- function(x, index = NULL, ...) {
    x <- pdata.frame(x, index = index, ...)
    punbalancedness.default(x, ...)
}

#' @rdname punbalancedness
#' @export
punbalancedness.panelmodel <- function(x, ...) {
    punbalancedness.default(x, ...)
}



myvar <- function(x){
    ## non-exported
    x.na <- is.na(x)
    if(anyNA(x.na)) x <- x[!x.na]
    n <- length(x)

    if(n <= 1L) {
        if(n == 0L) z <- NA
        if(n == 1L) z <- 0
    } else {
        z <- if(!(is.factor(x) || is.character(x))) var(x)
        else !all(duplicated(x)[-1L])
    }
    z
}



#' Check for Cross-Sectional and Time Variation
#'
#' This function checks for each variable of a panel if it varies
#' cross-sectionally and over time.
#'
#' For (p)data.frame and matrix interface: All-`NA` columns are removed
#' prior to calculation of variation due to coercing to pdata.frame
#' first.
#'
#' @aliases pvar
#' @param x a `(p)data.frame` or a `matrix`,
#' @param index see [pdata.frame()],
#' @param \dots further arguments.
#' @return An object of class `pvar` containing the following
#'     elements:
#'
#' \item{id.variation}{a logical vector with `TRUE` values if the
#' variable has individual variation, `FALSE` if not,}
#'
#' \item{time.variation}{a logical vector with `TRUE` values if the
#' variable has time variation, `FALSE` if not,}
#'
#' \item{id.variation_anyNA}{a logical vector with `TRUE` values if
#' the variable has at least one individual-time combination with all
#' `NA` values in the individual dimension for at least one time period,
#' `FALSE` if not,}
#'
#' \item{time.variation_anyNA}{a logical vector with `TRUE` values if
#' the variable has at least one individual-time combination with all
#' `NA` values in the time dimension for at least one individual,
#' `FALSE` if not.}
#'
#' @note `pvar` can be time consuming for ``big'' panels. As a fast alternative
#' [collapse::varying()] from package \CRANpkg{collapse} could be used.
#' @export
#' @author Yves Croissant
#' @seealso [pdim()] to check the dimensions of a 'pdata.frame' (and
#'     other objects),
#' @keywords attribute
#' @examples
#'
#'
#' # Gasoline contains two variables which are individual and time
#' # indexes and are the first two variables
#' data("Gasoline", package = "plm")
#' pvar(Gasoline)
#'
#' # Hedonic is an unbalanced panel, townid is the individual index;
#' # the drop.index argument is passed to pdata.frame
#' data("Hedonic", package = "plm")
#' pvar(Hedonic, "townid", drop.index = TRUE)
#'
#' # same using pdata.frame
#' Hed <- pdata.frame(Hedonic, "townid", drop.index = TRUE)
#' pvar(Hed)
#'
#' # Gasoline with pvar's matrix interface
#' Gasoline_mat <- as.matrix(Gasoline)
#' pvar(Gasoline_mat)
#' pvar(Gasoline_mat, index=c("country", "year"))
#'
pvar <- function(x, ...){
    UseMethod("pvar")
}

pvar.default <- function(x, id, time, ...){
    name.var <- names(x)
    len <- length(x)
    time.variation <- rep(TRUE, len)
    id.variation   <- rep(TRUE, len)
    time.variation_anyNA <- rep(FALSE, len)
    id.variation_anyNA   <- rep(FALSE, len)
    lid   <- split(x, id)   # these split() functions seem particularly slow
    ltime <- split(x, time)
    if(is.list(x)){
        if(len == 1L){
            # time variation
            temp_time.var          <- sapply(lid, function(x) sapply(x, myvar))
            temp_time.var_sumNoVar <- sum(temp_time.var == 0, na.rm = TRUE) # number of non-varying id-time comb. (without all NA groups)
            temp_time.var_sumNA    <- sum(is.na(temp_time.var))             # number of all-NA groups
            temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
            time.variation         <- temp_time.varResult != length(lid) # no variation if (no. non-varying + no. all-NA) == number of groups
            time.variation_anyNA   <- temp_time.var_sumNA > 0            # indicates if at least one id-time comb is all NA

            # id variation
            temp_id.var          <- sapply(ltime, function(x) sapply(x, myvar))
            temp_id.var_sumNoVar <- sum(temp_id.var == 0, na.rm = TRUE)
            temp_id.var_sumNA    <- sum(is.na(temp_id.var))
            temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
            id.variation         <- temp_id.varResult != length(ltime)
            id.variation_anyNA   <- temp_id.var_sumNA > 0
        }
        else{
            # time variation
            temp_time.var          <- sapply(lid, function(x) sapply(x, myvar))
            temp_time.var_sumNoVar <- apply(temp_time.var == 0, 1, sum, na.rm = TRUE)
            temp_time.var_sumNA    <- apply(is.na(temp_time.var), 1, sum)
            temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
            time.variation         <- temp_time.varResult != length(lid)
            time.variation_anyNA   <- temp_time.var_sumNA > 0

            # id variation
            temp_id.var          <- sapply(ltime, function(x) sapply(x, myvar))
            temp_id.var_sumNoVar <- apply(temp_id.var == 0, 1, sum, na.rm = TRUE)
            temp_id.var_sumNA    <- apply(is.na(temp_id.var), 1, sum)
            temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
            id.variation         <- temp_id.varResult != length(ltime)
            id.variation_anyNA   <- temp_id.var_sumNA > 0
        }
    }
    else{ # not a list (not a data.frame, pdata.frame) - try our best for that unknown data structure
        # time variation
        temp_time.var          <- sapply(lid, function(x) sapply(x, myvar))
        temp_time.var_sumNoVar <- sum(temp_time.var == 0, na.rm = TRUE)
        temp_time.var_sumNA    <- sum(is.na(temp_time.var))
        temp_time.varResult    <- temp_time.var_sumNoVar + temp_time.var_sumNA
        time.variation         <- temp_time.varResult != length(lid)
        time.variation_anyNA   <- temp_time.var_sumNA > 0

        # id variation
        temp_id.var          <- sapply(ltime, function(x) sapply(x, myvar))
        temp_id.var_sumNoVar <- sum(temp_id.var == 0, na.rm = TRUE)
        temp_id.var_sumNA    <- sum(is.na(temp_id.var))
        temp_id.varResult    <- temp_id.var_sumNoVar + temp_id.var_sumNA
        id.variation         <- temp_id.varResult != length(ltime)
        id.variation_anyNA   <- temp_id.var_sumNA > 0
    }

    # make 'pvar' object
    names(id.variation) <- names(time.variation) <- names(id.variation_anyNA) <- names(time.variation_anyNA) <- name.var
    dim.var <- list(id.variation         = id.variation,
                    time.variation       = time.variation,
                    id.variation_anyNA   = id.variation_anyNA,
                    time.variation_anyNA = time.variation_anyNA)
    class(dim.var) <- "pvar"
    return(dim.var)
}

#' @rdname pvar
#' @export
pvar.matrix <- function(x, index = NULL, ...){
    x <- pdata.frame(as.data.frame(x), index, ...)
    pvar(x)
}

#' @rdname pvar
#' @export
pvar.data.frame <- function(x, index = NULL, ...){
    x <- pdata.frame(x, index, ...)
    pvar(x)
}

#' @rdname pvar
#' @export
pvar.pdata.frame <- function(x, ...){
    index <- unclass(attr(x, "index")) # unclass for speed
    pvar.default(x, index[[1L]], index[[2L]])
}

#' @rdname pvar
#' @export
pvar.pseries <- function(x, ...){
    # use drop.index = TRUE so that the index columns'
    # variations are not evaluated:
    pdfx <- pseries2pdataframe(x, drop.index = TRUE)
    pvar.pdata.frame(pdfx)
}

#' @rdname pvar
#' @export
print.pvar <- function(x, ...){
    varnames <- names(x$time.variation)
    if(any(!x$time.variation)){
        var <- varnames[x$time.variation == FALSE]
        #    if (!is.null(y)) var <- var[-which(var==y$id)]
        if(length(var)!=0) cat(paste("no time variation:      ", paste(var,collapse=" "),"\n"))
    }
    if(any(!x$id.variation)){
        var <- varnames[x$id.variation == FALSE]
        #    if (!is.null(y)) var <- var[-which(var==y$time)]
        if(length(var)!=0) cat(paste("no individual variation:", paste(var,collapse=" "),"\n"))
    }

    # any individual-time combinations all NA?
    if(any(x$time.variation_anyNA)){
        var_anyNA <- varnames[x$time.variation_anyNA]
        if(length(var_anyNA)!=0) cat(paste("all NA in time dimension for at least one individual: ", paste(var_anyNA,collapse=" "),"\n"))
    }
    if(any(x$id.variation_anyNA)){
        var_anyNA <- varnames[x$id.variation_anyNA]
        if(length(var_anyNA)!=0) cat(paste("all NA in ind. dimension for at least one time period:", paste(var_anyNA,collapse=" "),"\n"))
    }
    invisible(x)
}


#' Create a Dummy Matrix
#'
#' Contrast-coded dummy matrix created from a factor
#'
#' This function creates a matrix of dummies from the levels of a factor.
#' In model estimations, it is usually preferable to not create the dummy matrix
#' prior to estimation but to simply specify a factor in the formula and let the
#' estimation function handle the creation of the dummies.
#'
#' This function is merely a convenience wrapper around `stats::contr.treatment`
#' to ease the dummy matrix creation process shall the dummy matrix be explicitly
#' required. See Examples for a use case in LSDV (least squares dummy variable)
#' model estimation.
#'
#' The default method uses a factor as main input (or something coercible to a
#' factor) to derive the dummy matrix from. Methods for data frame and pdata.frame
#' are available as well and have the additional argument `col` to specify the
#' the column from which the dummies are created; both methods merge the dummy
#' matrix to the data frame/pdata.frame yielding a ready-to-use data set.
#' See also Examples for use cases.
#'
#' @param x a factor from which the dummies are created (x is coerced to
#'          factor if not yet a factor) for the default method or a data
#'          data frame/pdata.frame for the respective method.
#' @param base integer or character, specifies the reference level (base), if
#'             integer it refers to position in `levels(x)`, if character the name
#'             of a level,
#' @param base.add logical, if `TRUE` the reference level (base) is added
#'                 to the return value as first column, if `FALSE` the reference
#'                 level is not included.
#' @param col character (only for the data frame and pdata.frame methods), to
#'            specify the column which is used to derive the dummies from,
#' @param \dots further arguments.
#'
#' @return For the default method, a matrix containing the contrast-coded dummies,
#'         dimensions are n x n where `n = length(levels(x))` if argument
#'        `base.add = TRUE` or `n = length(levels(x)-1)` if `base.add = FALSE`;
#'         for the data frame and pdata.frame method, a data frame or pdata.frame,
#'         respectively, with the dummies appropriately merged to the input as
#'         last columns (column names are derived from the name of the column
#'         used to create the dummies and its levels).
#' @author Kevin Tappe
#' @importFrom stats contr.treatment
#' @export
#' @seealso [stats::contr.treatment()], [stats::contrasts()]
#' @keywords manip
#' @examples
#' library(plm)
#' data("Grunfeld", package = "plm")
#' Grunfeld <- Grunfeld[1:100, ] # reduce data set (down to 5 firms)
#'
#' ## default method
#' make.dummies(Grunfeld$firm) # gives 5 x 5 matrix (5 firms, base level incl.)
#' make.dummies(Grunfeld$firm, base = 2L, base.add = FALSE) # gives 5 x 4 matrix
#'
#' ## data frame method
#' Grun.dummies <- make.dummies(Grunfeld, col = "firm")
#'
#' ## pdata.frame method
#' pGrun <- pdata.frame(Grunfeld)
#' pGrun.dummies <- make.dummies(pGrun, col = "firm")
#'
#' ## Model estimation:
#' ## estimate within model (individual/firm effects) and LSDV models (firm dummies)
#' # within model:
#' plm(inv ~ value + capital, data = pGrun, model = "within")
#'
#' ## LSDV with user-created dummies by make.dummies:
#' form_dummies <- paste0("firm", c(1:5), collapse = "+")
#' form_dummies <- formula(paste0("inv ~ value + capital + ", form_dummies))
#' plm(form_dummies, data = pGrun.dummies, model = "pooling") # last dummy is dropped
#'
#' # LSDV via factor(year) -> let estimation function generate dummies:
#' plm(inv ~ value + capital + factor(firm), data = pGrun, model = "pooling")
make.dummies <- function(x, ...){
    UseMethod("make.dummies")
}

#' @rdname make.dummies
#' @export
make.dummies.default <- function(x, base = 1L, base.add = TRUE, ...) {

    stopifnot(is.numeric(base) || is.character(base))
    if(is.numeric(base)) if(round(base) != base) stop("Argument 'ref' specified as numeric but is not integer")
    if(!is.factor(x)) x <- factor(x)

    lvl <- levels(x)

    if(is.character(base)) {
        pos <- match(base, lvl)
        if(is.na(pos)) stop(paste0("argument 'ref' specified as character but value \"",
                                   base, "\", is not in levels(x)"))
        base <- pos
    }

    dummies <- contr.treatment(lvl, base = base)

    # if requested, add reference level to dummy matrix in 1st position
    if(base.add) {
        lvl.base <- levels(x)[base]
        dummies <- cbind(c(1, rep(0, NROW(dummies)-1)), dummies)
        colnames(dummies) <- c(lvl.base, colnames(dummies)[-1L])
    }
    dummies # is a matrix
}

#' @rdname make.dummies
#' @export
make.dummies.data.frame <- function(x, col, base = 1L, base.add = TRUE, ...) {

    stopifnot(inherits(col, "character"))
    dum.mat <- make.dummies.default(x[ , col], base, base.add) # dummy matrix
    colnames(dum.mat) <- paste0(col, colnames(dum.mat))
    dum.df <- data.frame(cbind("merge.col" = rownames(dum.mat), dum.mat))

    merge(x, dum.df, by.x = col, by.y = "merge.col", sort = FALSE)
}

#' @rdname make.dummies
#' @export
make.dummies.pdata.frame <- function(x, col, base = 1L, base.add = TRUE, ...) {

    stopifnot(inherits(col, "character"))
    #  idx.pos <- pos.index(x)
    #  drop.idx <- anyNA(idx.pos)
    idx <- attr(x, "index")
    res <- make.dummies.data.frame(x, col, base, base.add)
    # add back pdata.frame features (assumption is: merge did not change order of original data.frame)
    attr(res, "index") <- idx
    class(res) <- c("pdata.frame", class(res))
    res
}

# tool_model.extract.R#
# model.frame method for pdata.frame ; the formula argument must be a
# pdata.frame and the data argument must be a formula, which is quite
# esoteric, but consistent with the argument list of
# model.frame.Formula which is latter called.



#' model.frame and model.matrix for panel data
#'
#' Methods to create model frame and model matrix for panel data.
#'
#' The `lhs` and `rhs` arguments are inherited from `Formula`, see
#' there for more details.\cr The `model.frame` methods return a
#' `pdata.frame` object suitable as an input to plm's
#' `model.matrix`.\cr The `model.matrix` methods builds a model matrix
#' with transformations performed as specified by the `model` and
#' `effect` arguments (and `theta` if `model = "random"` is
#' requested), in this case the supplied `data` argument should be a
#' model frame created by plm's `model.frame` method. If not, it is
#' tried to construct the model frame from the data. Constructing the
#' model frame first ensures proper `NA` handling, see **Examples**.
#'
#' @name model.frame.pdata.frame
#' @param object,formula an object of class `"pdata.frame"` or an
#'     estimated model object of class `"plm"`,
#' @param x a `model.frame`
#' @param data a `formula`, see **Details**,
#' @param effect the effects introduced in the model, one of
#'     `"individual"`, `"time"`, `"twoways"` or `"nested"`,
#' @param model one of `"pooling"`, `"within"`, `"Sum"`, `"Between"`,
#'     `"between"`, `"random",` `"fd"` and `"ht"`,
#' @param theta the parameter for the transformation if `model =
#'     "random"`,
#' @param cstcovar.rm remove the constant columns, one of `"none",
#'     "intercept", "covariates", "all")`,
#' @param lhs inherited from package [Formula::Formula()] (see
#'     there),
#' @param rhs inherited from package [Formula::Formula()] (see
#'     there),
#' @param dot inherited from package [Formula::Formula()] (see
#'     there),
#' @param \dots further arguments.
#' @return The `model.frame` methods return a `pdata.frame`.\cr The
#'     `model.matrix` methods return a `matrix`.
#' @author Yves Croissant
#' @seealso [pmodel.response()] for (transformed) response
#'     variable.\cr [Formula::Formula()] from package `Formula`,
#'     especially for the `lhs` and `rhs` arguments.
#' @keywords classes
#' @examples
#'
#' # First, make a pdata.frame
#' data("Grunfeld", package = "plm")
#' pGrunfeld <- pdata.frame(Grunfeld)
#'
#' # then make a model frame from a formula and a pdata.frame
##pform <- pFormula(inv ~ value + capital)
##mf <- model.frame(pform, data = pGrunfeld)
#' form <- inv ~ value
#' mf <- model.frame(pGrunfeld, form)
#'
#' # then construct the (transformed) model matrix (design matrix)
#' # from model frame
##modmat <- model.matrix(pform, data = mf, model = "within")
#' modmat <- model.matrix(mf, model = "within")
#'
#' ## retrieve model frame and model matrix from an estimated plm object
## #fe_model <- plm(pform, data = pGrunfeld, model = "within")
#' fe_model <- plm(form, data = pGrunfeld, model = "within")
#' model.frame(fe_model)
#' model.matrix(fe_model)
#'
#' # same as constructed before
#' all.equal(mf, model.frame(fe_model), check.attributes = FALSE) # TRUE
#' all.equal(modmat, model.matrix(fe_model), check.attributes = FALSE) # TRUE
#'
NULL

#' @rdname model.frame.pdata.frame
#' @export
model.frame.pdata.frame <- function(formula, data = NULL, ...,
                                    lhs = NULL, rhs = NULL, dot = "previous"){
    pdata <- formula
    formula <- as.Formula(data)
    if (is.null(rhs)) rhs <- 1:(length(formula)[2L])
    if (is.null(lhs)) lhs <- if(length(formula)[1L] > 0L) 1 else 0
    index <- attr(pdata, "index")
    mf <- model.frame(formula, as.data.frame(pdata, row.names = FALSE), ..., # NB need row.names = FALSE to ensure mf has integer sequence as row names
                      lhs = lhs, rhs = rhs, dot = dot)
    index <- index[as.numeric(rownames(mf)), ] # reduce index down to rows left in model frame
    checkNA.index(index) # check for NAs in model.frame's index and error if any
    index <- droplevels(index)
    class(index) <- c("pindex", "data.frame")
    structure(mf,
              index = index,
              formula = formula,
              class = c("pdata.frame", class(mf)))
}


#' @rdname model.frame.pdata.frame
#' @export
formula.pdata.frame <- function(x, ...){
    if (is.null(attr(x, "terms")))
        stop("formula expect a model.frame and not an ordinary pdata.frame")
    attr(x, "formula")
}


#' @rdname model.frame.pdata.frame
#' @export
model.matrix.plm <- function(object, ...){
    dots <- list(...)
    model  <- if(is.null(dots$model))  describe(object, "model")  else dots$model
    effect <- if(is.null(dots$effect)) describe(object, "effect") else dots$effect
    rhs    <- if(is.null(dots$rhs)) 1 else dots$rhs
    cstcovar.rm <- dots$cstcovar.rm
    formula <- formula(object)
    data <- model.frame(object)
    if (model != "random"){
        model.matrix(data, model = model, effect = effect,
                     rhs = rhs, cstcovar.rm = cstcovar.rm)
    }
    else{
        theta <- ercomp(object)$theta
        model.matrix(data, model = model, effect = effect,
                     theta = theta, rhs = rhs, cstcovar.rm = cstcovar.rm)
    }
}


#' @rdname model.frame.pdata.frame
#' @export
model.matrix.pdata.frame <- function(object,
                                     model = c("pooling", "within", "Between", "Sum",
                                               "between", "mean", "random", "fd"),
                                     effect = c("individual", "time", "twoways", "nested"),
                                     rhs = 1,
                                     theta = NULL,
                                     cstcovar.rm = NULL,
                                     ...){
    if (is.null(attr(object, "terms")))
        stop("model.matrix expects a model.frame and not an ordinary pdata.frame")
    model <- match.arg(model)
    effect <- match.arg(effect)
    formula <- attr(object, "formula")
    data <- object
    has.intercept <- has.intercept(formula, rhs = rhs)
    # relevant defaults for cstcovar.rm
    if(is.null(cstcovar.rm)) cstcovar.rm <- if(model == "within") "intercept" else "none"
    balanced <- is.pbalanced(data)
    X <- model.matrix(as.Formula(formula), data = data, rhs = rhs, dot = "previous", ...)
    # check for infinite or NA values and exit if there are some
    if(any(! is.finite(X)))
        stop(paste("model matrix or response contains non-finite",
                   "values (NA/NaN/Inf/-Inf)"))
    X.assi <- attr(X, "assign")
    X.contr <- attr(X, "contrasts")
    X.contr <- X.contr[ ! vapply(X.contr, is.null, FUN.VALUE = TRUE, USE.NAMES = FALSE) ]
    index <- index(data)
    attr(X, "index") <- index
    if(effect == "twoways" && model %in% c("between", "fd"))
        stop("twoways effect only relevant for within, random, and pooling models")
    if(model == "within")  X <- Within(X, effect)
    if(model == "Sum")     X <- Sum(X, effect)
    if(model == "Between") X <- Between(X, effect)
    if(model == "between") X <- between(X, effect)
    if(model == "mean")    X <- Mean(X)
    if(model == "fd")      X <- pdiff(X, effect = "individual",
                                      has.intercept = has.intercept)
    if(model == "random"){
        if(is.null(theta)) stop("a theta argument must be provided for model = \"random\"")
        if(effect %in% c("time", "individual")) X <- X - theta * Between(X, effect)
        if(effect == "nested") X <- X - theta$id * Between(X, "individual") -
                theta$gp * Between(X, "group")
        if(effect == "twoways" && balanced)
            X <- X - theta$id * Between(X, "individual") -
                theta$time * Between(X, "time") + theta$total * Mean(X)
        ## TODO: case unbalanced twoways not treated here. Catch and error gracefully?
        # if (effect == "twoways" && !balanced) stop("two-way unbalanced case not implemented in model.matrix.pdata.frame")
    }
    if(cstcovar.rm == "intercept"){
        posintercept <- match("(Intercept)", colnames(X))
        if (! is.na(posintercept)) X <- X[ , - posintercept, drop = FALSE]
    }
    if(cstcovar.rm %in% c("covariates", "all")){
        cols <- apply(X, 2, is.constant)
        cstcol <- names(cols)[cols]
        posintercept <- match("(Intercept)", cstcol)
        cstintercept <- if(is.na(posintercept)) FALSE else TRUE
        zeroint <- if(cstintercept &&
                      max(X[ , posintercept]) < sqrt(.Machine$double.eps))
            TRUE else FALSE
        if(length(cstcol) > 0L){
            if((cstcovar.rm == "covariates" || !zeroint) && cstintercept) cstcol <- cstcol[- posintercept]
            if(length(cstcol) > 0L){
                X <- X[ , - match(cstcol, colnames(X)), drop = FALSE]
                attr(X, "constant") <- cstcol
            }
        }
    }
    structure(X, assign = X.assi, contrasts = X.contr, index = index)
}



#' A function to extract the model.response
#'
#' pmodel.response has several methods to conveniently extract the
#' response of several objects.
#'
#' The model response is extracted from a `pdata.frame` (where the
#' response must reside in the first column; this is the case for a
#' model frame), a `pFormula` + `data` or a `plm` object, and the
#' transformation specified by `effect` and `model` is applied to
#' it.\cr Constructing the model frame first ensures proper `NA`
#' handling and the response being placed in the first column, see
#' also **Examples** for usage.
#'
#' @aliases pmodel.response
#' @param object an object of class `"plm"`, or a formula of
#'     class `"Formula"`,
#' @param data a `data.frame`
#' @param \dots further arguments.
#' @return A pseries except if model responses' of a `"between"`
#'     or `"fd"` model as these models "compress" the data (the number
#'     of observations used in estimation is smaller than the original
#'     data due to the specific transformation). A numeric is returned
#'     for the `"between"` and `"fd"` model.
#' @export
#' @author Yves Croissant
#' @seealso `plm`'s [model.matrix()] for (transformed)
#'     model matrix and the corresponding [model.frame()]
#'     method to construct a model frame.
#' @keywords manip
#' @examples
#'
#' # First, make a pdata.frame
#' data("Grunfeld", package = "plm")
#' pGrunfeld <- pdata.frame(Grunfeld)
#'
#' # then make a model frame from a pFormula and a pdata.frame
#' form <- inv ~ value + capital
#' mf <- model.frame(pGrunfeld, form)
#'
#' # retrieve (transformed) response directly from model frame
#' resp_mf <- pmodel.response(mf, model = "within", effect = "individual")
#'
#' # retrieve (transformed) response from a plm object, i.e., an estimated model
#' fe_model <- plm(form, data = pGrunfeld, model = "within")
#' pmodel.response(fe_model)
#'
#' # same as constructed before
#' all.equal(resp_mf, pmodel.response(fe_model), check.attributes = FALSE) # TRUE
#'
pmodel.response <- function(object, ...) {
    UseMethod("pmodel.response")
}

#' @rdname pmodel.response
#' @export
pmodel.response.plm <- function(object, ...){
    y <- model.response(model.frame(object))
    dots <- list(...)
    model  <- if(is.null(dots$model))   describe(object, "model")  else dots$model
    effect <- if(is.null(dots$effect))  describe(object, "effect") else dots$effect
    theta  <- if(is.null(dots$theta)) {
        if(describe(object, "model") == "random")
            ercomp(object)$theta else NULL
    } else dots$theta
    ptransform(y, model = model, effect = effect, theta = theta)
}

#' @rdname pmodel.response
#' @export
pmodel.response.data.frame <- function(object, ...){
    dots <- list(...)
    if(is.null(attr(object, "terms"))) stop("not a model.frame")
    model  <- if(is.null(dots$model))  "pooling"    else dots$model
    effect <- if(is.null(dots$effect)) "individual" else dots$effect
    theta  <- if(is.null(dots$theta))  NULL         else dots$theta
    y <- model.response(object) # has index attribute but not class 'pseries'
    class(y) <- unique(c("pseries", class(y)))
    ptransform(y, model = model, effect = effect, theta = theta)
}

# "deprecated" (not advertised anymore)
#' @rdname pmodel.response
#' @export
pmodel.response.formula <- function(object, data, ...){
    #  print("pmodel.response.formula")
    dots <- list(...)
    if(is.null(data)) stop("the data argument is mandatory")
    if(! inherits(data, "pdata.frame")) stop("the data argument must be a pdata.frame")
    if(is.null(attr(data, "terms"))) data <- model.frame(data, object)
    model  <- dots$model
    effect <- dots$effect
    theta  <- dots$theta
    if(is.null(model)) model <- "pooling"
    if(is.null(effect)) effect <- "individual"
    if(model == "random" && is.null(theta)) stop("the theta argument is mandatory for model = \"random\"")
    y <- model.response(data)
    class(y) <- unique(c("pseries", class(y)))
    ptransform(y, model = model, effect = effect, theta = theta)
}


ptransform <- function(x, model = NULL, effect = NULL, theta = NULL, ...){
    # NB: ptransform (and hence pmodel.response) does not handle the random 2-way unbalanced case

    if(model == "pooling") return(x) # early exit
    if(effect == "twoways" && model %in% c("between", "fd"))
        stop("twoways effect only relevant for within, random, and pooling models")

    if(model == "within")  x <- Within(x, effect)
    if(model == "between") x <- between(x, effect)
    if(model == "Between") x <- Between(x, effect)
    if(model == "fd")      x <- pdiff(x, "individual")
    if(model == "random") {
        balanced <- is.pbalanced(x) # need to check this right here as long as x is a pseries
        if(is.null(theta)) stop("a theta argument must be provided")
        if(effect %in% c("time", "individual")) x <- x - theta * Between(x, effect)
        if(effect == "nested") x <- x - theta$id * Between(x, "individual") -
                theta$gp * Between(x, "group")
        if(effect == "twoways" && balanced)
            x <- x - theta$id   * Between(x, "individual") -
                theta$time * Between(x, "time") + theta$total * mean(x)
        ## TODO: could catch non-treated RE unbalanced twoways case to error gracefully:
        # if (effect == "twoways" && !balanced) warning("two-way unbalanced case not implemented in ptransform")
    }

    # between and fd models "compress" the data, thus an index does not make
    # sense for those, but add to all others (incl. Between (capital B))
    x <- if(model %in% c("between", "fd")) x
    else structure(x, index = index(x), class = unique(c("pseries", class(x))))
    return(x)
}

# tool_pdata.frame.R#
## pdata.frame and pseries are adaptations of respectively data.frame
## and vector for panel data. An index attribute is added to both,
## which is a data.frame containing the indexes. There is no pseries
## function, it is the class of series extracted from a
## pdata.frame. index and pdim functions are used to extract
## respectively the data.frame containing the index and the dimensions
## of the panel

## pdata.frame:
## - $<-
## - [
## - $
## - [[
## - print
## - as.list
## - as.data.frame
## - pseriesfy

## pseries:
## - [
## - print
## - as.matrix
## - plot
## - summary
## - plot.summary
## - print.summary
## - is.pseries

## pdim:
## - pdim.default
## - pdim.data.frame
## - pdim.pdata.frame
## - pdim.pseries
## - pdim.panelmodel
## - pdim.pgmm
## - print.pdim

## index:
## - index.pindex
## - index.pdata.frame
## - index.pseries
## - index.panelmodel
## - is.index (non-exported)
## - has.index (non-exported)
## - checkNA.index (non-exported)
## - pos.index (non-exported)

fancy.row.names <- function(index, sep = "-") {
    ## non-exported
    # assumes index is a list of 2 or 3 factors [not class pindex]
    if (length(index) == 2L) {result <- paste(index[[1L]], index[[2L]], sep = sep)}
    # this in the order also used for sorting (group, id, time):
    if (length(index) == 3L) {result <- paste(index[[3L]], index[[1L]], index[[2L]], sep = sep)}
    return(result)
}




#' data.frame for panel data
#'
#' An object of class 'pdata.frame' is a data.frame with an index
#' attribute that describes its individual and time dimensions.
#'
#' The `index` argument indicates the dimensions of the panel. It can
#' be: \itemize{
#' \item a vector of two character strings which
#' contains the names of the individual and of the time indexes,
#' \item
#' a character string which is the name of the individual index
#' variable. In this case, the time index is created automatically and
#' a new variable called "time" is added, assuming consecutive and
#' ascending time periods in the order of the original data,
#' \item an integer, the number of individuals. In this case, the data
#' need to be a balanced panel and be organized as a stacked time series
#' (successive blocks of individuals, each block being a time series
#' for the respective individual) assuming consecutive and ascending
#' time periods in the order of the original data. Two new variables
#' are added: "id" and "time" which contain the individual and the
#' time indexes.
#' }
#'
#' The `"[["` and `"$"` extract a series from the `pdata.frame`.  The
#' `"index"` attribute is then added to the series and a class
#' attribute `"pseries"` is added. The `"["` method behaves as for
#' `data.frame`, except that the extraction is also applied to the
#' `index` attribute.  A safe way to extract the index attribute is to
#' use the function [index()] for 'pdata.frames' (and other objects).
#'
#' `as.data.frame` removes the index attribute from the `pdata.frame`
#' and adds it to each column. For its argument `row.names` set to
#' `FALSE` row names are an integer series, `TRUE` gives "fancy" row
#' names; if a character (with length of the resulting data frame),
#' the row names will be the character's elements.
#'
#' `as.list` behaves by default identical to
#' [base::as.list.data.frame()] which means it drops the
#' attributes specific to a pdata.frame; if a list of pseries is
#' wanted, the attribute `keep.attributes` can to be set to
#' `TRUE`. This also makes `lapply` work as expected on a pdata.frame
#' (see also **Examples**).
#'
#' @param x a `data.frame` for the `pdata.frame` function and a
#'     `pdata.frame` for the methods,
#' @param i see [Extract()],
#' @param j see [Extract()],
#' @param y one of the columns of the `data.frame`,
#' @param index this argument indicates the individual and time
#'     indexes. See **Details**,
#' @param drop see [Extract()],
#' @param drop.index logical, indicates whether the indexes are to be
#'     excluded from the resulting pdata.frame,
#' @param optional see [as.data.frame()],
#' @param row.names `NULL` or logical, indicates whether "fancy" row
#'     names (combination of individual index and time index) are to
#'     be added to the returned (p)data.frame (`NULL` and `FALSE` have
#'     the same meaning for `pdata.frame`; for
#'     `as.data.frame.pdata.frame` see Details),
#' @param stringsAsFactors logical, indicating whether character
#'     vectors are to be converted to factors,
#' @param replace.non.finite logical, indicating whether values for
#'     which `is.finite()` yields `TRUE` are to be replaced by `NA`
#'     values, except for character variables (defaults to `FALSE`),
#' @param drop.NA.series logical, indicating whether all-`NA` columns
#'     are to be removed from the pdata.frame (defaults to `FALSE`),
#' @param drop.const.series logical, indicating whether constant
#'     columns are to be removed from the pdata.frame (defaults to
#'     `FALSE`),
#' @param drop.unused.levels logical, indicating whether unused levels
#'     of factors are to be dropped (defaults to `FALSE`) (unused
#'     levels are always dropped from variables serving to construct
#'     the index variables),
#' @param keep.attributes logical, only for as.list and as.data.frame
#'     methods, indicating whether the elements of the returned
#'     list/columns of the data.frame should have the pdata.frame's
#'     attributes added (default: FALSE for as.list, TRUE for
#'     as.data.frame),
#' @param name the name of the `data.frame`,
#' @param value the name of the variable to include,
#' @param \dots further arguments.
#' @return a `pdata.frame` object: this is a `data.frame` with an
#'     `index` attribute which is a `data.frame` with two variables,
#'     the individual and the time indexes, both being factors.  The
#'     resulting pdata.frame is sorted by the individual index, then
#'     by the time index.
#' @export
#' @author Yves Croissant
#' @seealso [index()] to extract the index variables from a
#'     'pdata.frame' (and other objects), [pdim()] to check the
#'     dimensions of a 'pdata.frame' (and other objects), [pvar()] to
#'     check for each variable if it varies cross-sectionally and over
#'     time.  To check if the time periods are consecutive per
#'     individual, see [is.pconsecutive()].
#' @keywords classes
#' @examples
#'
#' # Gasoline contains two variables which are individual and time
#' # indexes
#' data("Gasoline", package = "plm")
#' Gas <- pdata.frame(Gasoline, index = c("country", "year"), drop.index = TRUE)
#'
#' # Hedonic is an unbalanced panel, townid is the individual index
#' data("Hedonic", package = "plm")
#' Hed <- pdata.frame(Hedonic, index = "townid", row.names = FALSE)
#'
#' # In case of balanced panel, it is sufficient to give number of
#' # individuals data set 'Wages' is organized as a stacked time
#' # series
#' data("Wages", package = "plm")
#' Wag <- pdata.frame(Wages, 595)
#'
#' # lapply on a pdata.frame by making it a list of pseries first
#' lapply(as.list(Wag[ , c("ed", "lwage")], keep.attributes = TRUE), lag)
#'
#'
pdata.frame <- function(x, index = NULL, drop.index = FALSE, row.names = TRUE,
                        stringsAsFactors = FALSE,
                        replace.non.finite = FALSE,
                        drop.NA.series = FALSE, drop.const.series = FALSE,
                        drop.unused.levels = FALSE) {

    if (inherits(x, "pdata.frame")) stop("already a pdata.frame")

    if (length(index) > 3L){
        stop("'index' can be of length 3 at the most (one index variable for individual, time, group)")
    }

    # prune input: x is supposed to be a plain data.frame. Other classes building
    # on top of R's data frame can inject attributes etc. that confuse functions
    # in pkg plm.
    x <- data.frame(x)

    # if requested: coerce character vectors to factors
    if (stringsAsFactors) {
        x.char <- names(x)[vapply(x, is.character, FUN.VALUE = TRUE, USE.NAMES = FALSE)]
        for (i in x.char){
            x[[i]] <- factor(x[[i]])
        }
    }

    # if requested: replace Inf, -Inf, NaN (everything for which is.finite is FALSE) by NA
    # (for all but any character columns [relevant if stringAsFactors == FALSE])
    if (replace.non.finite) {
        for (i in names(x)) {
            if (!inherits(x[[i]], "character")) {
                x[[i]][!is.finite(x[[i]])] <- NA
            }
        }
    }

    # if requested: check and remove complete NA series
    if (drop.NA.series) {
        na.check <- vapply(x, function(x) sum(!is.na(x)) == 0L, FUN.VALUE = TRUE, USE.NAMES = FALSE)
        na.serie <- names(x)[na.check]
        if (length(na.serie) > 0L){
            if (length(na.serie) == 1L)
                cat(paste0("This series is NA and has been removed: ", na.serie, "\n"))
            else
                cat(paste0("These series are NA and have been removed: ", paste(na.serie, collapse = ", "), "\n"))
        }
        x <- x[ , !na.check]
    }

    # if requested: check for constant series and remove
    if (drop.const.series) {
        # -> var() and sd() on factors is deprecated as of R 3.2.3 -> use duplicated()
        cst.check <- vapply(x, function(x) {
            if (is.factor(x) || is.character(x)) {
                all(duplicated(x[!is.na(x)])[-1L])
            } else {
                x[! is.finite(x)] <- NA # infinite elements set to NA only for this check
                var(as.numeric(x), na.rm = TRUE) == 0
            }
        }, FUN.VALUE = TRUE, USE.NAMES = FALSE)

        # following line: bug fixed thanks to Marciej Szelfer
        cst.check <- cst.check | is.na(cst.check)
        cst.serie <- names(x)[cst.check]
        if (length(cst.serie) > 0L){
            if (length(cst.serie) == 1L){
                cat(paste0("This series is constant and has been removed: ", cst.serie, "\n"))
            }
            else{
                cat(paste0("These series are constants and have been removed: ",
                           paste(cst.serie, collapse = ", "), "\n"))
            }
        }
        x <- x[ , !cst.check]
    }

    # sanity check for 'index' argument. First, check the presence of a
    # grouping variable, this should be the third element of the index
    # vector or any "group" named element of this vector
    group.name <- NULL
    if (! is.null(names(index)) || length(index == 3L)){
        if (! is.null(names(index))){
            grouppos <- match("group", names(index))
            if (! is.na(grouppos)){
                group.name <- index[grouppos]
                index <- index[- grouppos]
            }
        }
        if (length(index) == 3L){
            group.name <- index[3L]
            index <- index[-3L]
        }
    }
    if (length(index) == 0L) index <- NULL

    # if index is NULL, both id and time are NULL
    if (is.null(index)){
        id <- NULL
        time <- NULL
    }
    # if the length of index is 1, id = index and time is NULL
    if (length(index) == 1L){
        id <- index
        time <- NULL
    }
    # if the length of index is 2, the first element is id, the second
    # is time
    if (length(index) == 2L){
        id <- index[1L]
        time <- index[2L]
    }
    # if both id and time are NULL, the names of the index are the first
    # two names of x
    if (is.null(id) && is.null(time)){
        id.name <- names(x)[1L]
        time.name <- names(x)[2L]
    }
    else{
        id.name <- id
        time.name <- time
    }

    # if index is numeric, this indicats a balanced panel with no. of
    # individuals equal to id.name
    if(is.numeric(id.name)){
        if(!is.null(time.name))
            warning("The time index (second element of 'index' argument) will be ignored\n")
        N <- nrow(x)
        if( (N %% id.name) != 0){
            stop(paste0("unbalanced panel, in this case the individual index may not be indicated by an integer\n",
                        "but by specifying a column of the data.frame in the first element of the 'index' argument\n"))
        }
        else{
            T <- N %/% id.name
            n <- N %/% T
            time <- rep((1:T), n)
            id <- rep((1:n), rep(T, n))
            id.name <- "id"
            time.name <- "time"
            if (id.name %in% names(x)) warning(paste0("column '", id.name, "' overwritten by id index"))
            if (time.name %in% names(x)) warning(paste0("column '", time.name, "' overwritten by time index"))
            x[[id.name]] <- id <- as.factor(id)
            x[[time.name]] <- time <- as.factor(time)
        }
    }
    else{
        # id.name is not numeric, i.e., individual index is supplied
        if (!id.name %in% names(x)) stop(paste("variable ", id.name, " does not exist (individual index)", sep=""))
        if (is.factor(x[[id.name]])){
            id <- x[[id.name]] <- x[[id.name]][drop = TRUE] # drops unused levels of factor
        }
        else{
            id <- x[[id.name]] <- as.factor(x[[id.name]])
        }

        if (is.null(time.name)){
            # if no time index is supplied, add time variable
            # automatically order data by individual index, necessary
            # for the automatic addition of time index to be
            # successful if no time index was supplied
            x <- x[order(x[[id.name]]), ]
            Ti <- table(x[[id.name]]) # was: Ti <- table(id)
            n <- length(Ti)
            time <- c()
            for (i in 1:n){
                time <- c(time, 1:Ti[i])
            }
            time.name <- "time"
            if (time.name %in% names(x))
                warning(paste0("column '", time.name, "' overwritten by time index"))
            time <- x[[time.name]] <- as.factor(time)
        }
        else{
            # use supplied time index
            if (!time.name %in% names(x))
                stop(paste0("variable ", time.name, " does not exist (time index)"))

            if (is.factor(x[[time.name]])){
                time <- x[[time.name]] <- x[[time.name]][drop = TRUE]
            }
            else{
                time <- x[[time.name]] <- as.factor(x[[time.name]])
            }
        }
    }

    # if present, make group variable a factor (just like for id and
    # time variables)
    if (!is.null(group.name)) {
        if (is.factor(x[[group.name]])){
            group <- x[[group.name]] <- x[[group.name]][drop = TRUE]
        }
        else{
            group <- x[[group.name]] <- as.factor(x[[group.name]])
        }
    }

    # sort by group (if given), then by id, then by time
    if (! is.null(group.name)) x <- x[order(x[[group.name]], x[[id.name]], x[[time.name]]), ]
    else x <- x[order(x[[id.name]], x[[time.name]]), ]

    # if requested: drop unused levels from factor variables (spare
    # those serving for the index as their unused levels are dropped
    # already (at least in the attribute "index" they need to be
    # dropped b/c much code relies on it))
    if (drop.unused.levels) {
        var.names <- setdiff(names(x), c(id.name, time.name, group.name))
        for (i in var.names){
            if (is.factor(x[[i]])){
                x[[i]] <- droplevels(x[[i]])
            }
        }
    }
    posindex <- match(c(id.name, time.name, group.name), names(x))
    index <- unclass(x[ , posindex]) # unclass to list for speed in subsetting, make it data.frame again later
    if (drop.index) {
        x <- x[ , -posindex, drop = FALSE]
        if (ncol(x) == 0L) warning("after dropping of index variables, the pdata.frame contains 0 columns")
    }

    ### warn if duplicate couples
    test_doub <- table(index[[1L]], index[[2L]], useNA = "ifany")
    if (any(as.vector(test_doub[!is.na(rownames(test_doub)), !is.na(colnames(test_doub))]) > 1L))
        warning(paste("duplicate couples (id-time) in resulting pdata.frame\n to find out which,",
                      "use, e.g., table(index(your_pdataframe), useNA = \"ifany\")"))

    ### warn if NAs in index as likely not sane [not using check.NA because that outputs a line for each dimension -> not needed here]
    if (anyNA(index[[1L]]) || anyNA(index[[2L]]) || (if(length(index) == 3L) anyNA(index[[3L]]) else FALSE))
        warning(paste0("at least one NA in at least one index dimension ",
                       "in resulting pdata.frame\n to find out which, use, e.g., ",
                       "table(index(your_pdataframe), useNA = \"ifany\")\n"))

    ### Could also remove rows with NA in any index' dimension
    # drop.rows <- is.na(index[[1L]]) | is.na(index[[2L]])
    # if(ncol(index) == 3L) drop.rows <- drop.rows | is.na(index[[3L]])
    # if((no.drop.rows <- sum(drop.rows)) > 0L) {
    #   x <- x[!drop.rows, ]
    #   index <- index[!drop.rows, ]
    #   txt.drop.rows <- paste0(no.drop.rows, " row(s) dropped in resulting pdata.frame due to NA(s) in at least one index dimension")
    #   warning(txt.drop.rows)
    # }

    if (row.names) {
        attr(x, "row.names") <- fancy.row.names(index)
        # NB: attr(x, "row.names") allows for duplicate rownames (as
        # opposed to row.names(x) <- something)
        # NB: no fancy row.names for index attribute (!?):
        # maybe because so it is possible to restore original row.names?
    }

    class(index) <- c("pindex", "data.frame")
    attr(x, "index") <- index
    class(x) <- c("pdata.frame", "data.frame")

    return(x)
}

#' @rdname pdata.frame
#' @export
"$<-.pdata.frame" <- function(x, name, value) {
    if (inherits(value, "pseries")){
        # remove pseries features before adding value as a column to pdata.frame
        if (length(class(value)) == 1L) value <- unclass(value)
        else attr(value, "class") <- setdiff(class(value), "pseries")
        attr(value, "index") <- NULL
    }
    "$<-.data.frame"(x, name, value)
}

# NB: We don't have methods for [<-.pdata.frame and [[<-.pdata.frame, so these functions
#     dispatch to the respective data.frame methods which assign whatever is
#     handed over to the methods. Especially, if a pseries is handed over, this
#     results in really assigning a pseries to the pdata.frame in case of usage of
#     [<- and [[<-. This is inconsistent because the columns of a pdata.frame do not
#     have the 'pseries' features.
#     This can be seen by lapply(some_pdata.frame, class) after
#     assigning with the respective .data.frame methods


# Extracting/subsetting method for class pseries, [.pseries, retaining the
# pseries features. est cases are in tests/test_pdata.frame_subsetting.R.
#
# We do not provide a [[.pseries method in addition (note the double "["). Thus,
# the base R method is used and behaviour for pseries is what one would expect
# and is in line with base R, see ?Extract for [[ with atomic vectors:
# "The usual form of indexing is [. [[ can be used to select a single element
#  dropping names, whereas [ keeps them, e.g., in c(abc = 123)[1]."
# In addition, it also drops other attributes in base R, so applying [[ from
# base R results in dropping names and index which is in line with what one
# would expect for pseries. Example for base R behaviour:
#  a <- 1:10
#  names(a) <- letters[1:10]
#  attr(a, "index") <- "some_index_attribute"
#  a[[3]] # drops names and attribute (a[3] keeps names and drops other attributes)

##### [.pseries is commented because it leads to headache when dplyr is loaded
### boiled down to pkg vctrs https://github.com/r-lib/vctrs/issues/1446
### R.utils::detachPackage("dplyr")
### test_pure <- pcdtest(diff(log(price)) ~ diff(lag(log(price))) + diff(lag(log(price), 2)), data = php)
###
### library(dplyr) # first one will error with [.pseries, for plm 2.4-1 it gives a wrong result (lag is hijacked -> known case)
### test_dplyr        <- pcdtest(diff(price) ~ diff(lag(price)), data = php)
### test_dplyr_plmlag <- pcdtest(diff(log(price)) ~ diff(plm::lag(log(price))) + diff(plm::lag(log(price), 2)), data = php) # save way
##
##
## @rdname pdata.frame
## @export
# "[.pseries" <- function(x, ...) {
#
#  ## use '...' instead of only one specific argument, because subsetting for
#  ## factors can have argument 'drop', e.g., x[i, drop=TRUE] see ?Extract.factor
#   index <- attr(x, "index")
#
#   ## two sanity checks as [.pseries-subsetting was introduced in Q3/2021 and some packages
#   ## produced illegal pseries (these pkg errors were fixed by new CRAN releases but maybe
#   ## other code outhere produces illegal pseries, so leave these sanity checks in here for
#   ## a while, then remove (for speed)
#     if(is.null(index)) warning("pseries object with is.null(index(pseries)) == TRUE encountered")
#     if(!is.null(index) && !is.index(index)) warning(paste0("pseries object has illegal index with class(index) == ", paste0(class(index), collapse = ", ")))
#
#   names_orig <- names(x)
#   keep_rownr <- seq_along(x) # full length row numbers original pseries
#   names(keep_rownr) <- names_orig
#
#   if(is.null(names_orig)) {
#     names(x) <- keep_rownr # if no names are present, set names as integer sequence to identify rows to keep later
#     names(keep_rownr) <- keep_rownr
#   }
#   x <- remove_pseries_features(x)
#   result <- x[...] # actual subsetting
#
#   # identify rows to keep in the index:
#   keep_rownr <- keep_rownr[names(result)] # row numbers to keep after subsetting
#   names(result) <- if(!is.null(names_orig)) names_orig[keep_rownr] else NULL # restore and subset original names if any
#
#   # Subset index accordingly:
#   # Check if index is null is a workaround for R's data frame subsetting not
#   # stripping class pseries but its attributes for factor (for other data types, pseries class is dropped)
#   # see https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18140
#   if (!is.null(index)) {
#     index <- index[keep_rownr, ]
#     index <- droplevels(index) # drop unused levels (like in subsetting of pdata.frames)
#   }
#
#   result <- add_pseries_features(result, index)
#   return(result)
# }

## Non-exported internal function for subsetting of pseries. Can be used
## internally.
## While there is now a "proper" subsetting function for pseries, leave this
## subset_pseries for a while just to be safe (currently used in pcdtest())
subset_pseries <- function(x, ...) {
    ## use '...' instead of only one specific argument, because subsetting for
    ## factors can have argument 'drop', e.g., x[i, drop=TRUE] see ?Extract.factor
    index <- attr(x, "index")
    if(is.null(index)) warning("pseries object with is.null(index(pseries)) == TRUE encountered")
    if(!is.null(index) && !is.index(index)) warning(paste0("pseries object has illegal index with class(index) == ", paste0(class(index), collapse = ", ")))
    names_orig <- names(x)
    keep_rownr <- seq_along(x) # full length row numbers original pseries
    names(keep_rownr) <- names_orig

    if(is.null(names_orig)) {
        names(x) <- keep_rownr # if no names are present, set names as integer sequence to identify rows to keep later
        names(keep_rownr) <- keep_rownr
    }
    x <- remove_pseries_features(x)
    result <- x[...] # actual subsetting

    # identify rows to keep in the index:
    keep_rownr <- keep_rownr[names(result)] # row numbers to keep after subsetting
    names(result) <- if(!is.null(names_orig)) names_orig[keep_rownr] else NULL # restore and subset original names if any

    # Subset index accordingly:
    # Check if index is null is a workaround for R's data frame subsetting not
    # stripping class pseries but its attributes for factor (for other data types, pseries class is dropped)
    # see https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18140
    if(!is.null(index)) {
        index <- index[keep_rownr, ]
        index <- droplevels(index) # drop unused levels (like in subsetting of pdata.frames)
    }

    result <- add_pseries_features(result, index)
    return(result)
}


#' @rdname pdata.frame
#' @export
"[.pdata.frame" <- function(x, i, j, drop) {
    # signature of [.data.frame here

    missing.i    <- missing(i)    # missing is only guaranteed to yield correct results,
    missing.j    <- missing(j)    # if its argument was not modified before accessing it
    missing.drop <- missing(drop) # -> save information about missingness
    sc <- sys.call()
    # Nargs_mod to distinguish if called by [] (Nargs_mod == 2L); [,] (Nargs_mod == 3L); [,,] (Nargs_mod == 4L)
    Nargs_mod <- nargs() - (!missing.drop)

    ### subset index (and row names) appropriately:
    # subsetting data.frame by only j (x[ , j]) or missing j (x[i]) yields full-row
    # column(s) of data.frame, thus do not subset the index because it needs full rows (original index)
    #
    # subset index if:
    #      * [i,j] (supplied i AND supplied j) (in this case: Nargs_mod == 3L (or 4L depending on present/missing drop))
    #      * [i, ] (supplied i AND missing j)  (in this case: Nargs_mod == 3L (or 4L depending on present/missing drop))
    #
    # do not subset index in all other cases (here are the values of Nargs_mod)
    #      * [ ,j] (missing  i AND j supplied)                   (Nargs_mod == 3L (or 4L depending on present/missing drop))
    #      * [i]   (supplied i AND missing j)                    (Nargs_mod == 2L) [Nargs_mod distinguishes this case from the one where subsetting is needed!]
    #      * [i, drop = TRUE/FALSE] (supplied i AND missing j)   (Nargs_mod == 2L)
    #
    # => subset index (and row names) if: supplied i && Nargs_mod >= 3L

    index <- attr(x, "index")
    x.rownames <- row.names(x)
    if (!missing.i && Nargs_mod >= 3L) {
        iindex <- i
        if (is.character(iindex)) {
            # Kevin Tappe 2016-01-04 : in case of indexing (subsetting) a
            # pdata.frame by a character, the subsetting vector should be
            # converted to numeric by matching to the row names so that the
            # index can be correctly subsetted (by this numeric value).
            # Motivation:
            # Row names of the pdata.frame and row names of the pdata.frame's
            # index are not guaranteed to be the same!
            iindex <- match(iindex, rownames(x))
        }
        # subset index and row names
        index <- "[.data.frame"(index, iindex, )
        x.rownames <- x.rownames[iindex]

        # remove empty levels in index (if any)
        # NB: really do dropping of unused levels? Standard R behaviour is to leave the levels and not drop unused levels
        #     Maybe the dropping is needed for functions like lag.pseries/lagt.pseries to work correctly?
        index <- droplevels(index)
        # NB: use droplevels() rather than x[drop = TRUE] as x[drop = TRUE] can also coerce mode!
        # old (up to rev. 251): index <- data.frame(lapply(index, function(x) x[drop = TRUE]))
    }

    ### end of subsetting index

    # delete attribute with old index first:
    # this preserves the order of the attributes because
    # order of non-standard attributes is scrambled by R's data.frame subsetting with `[.`
    # (need to add new index later anyway)
    attr(x, "index") <- NULL

    # Set class to "data.frame" first to avoid coercing which enlarges the (p)data.frame
    # (probably by as.data.frame.pdata.frame).
    # Coercing is the built-in behaviour for extraction from data.frames by "[." (see ?`[.data.frame`)
    # and it seems this cannot be avoided; thus we need to make sure, not to have any coercing going on
    # which adds extra data (such as as.matrix.pseries, as.data.frame.pdata.frame) by setting the class
    # to "data.frame" first
    class(x) <- "data.frame"

    # call [.data.frame exactly as [.pdata.frame was called but arg is now 'x'
    # this is necessary because there could be several missing arguments
    # use sys.call (and not match.call) because arguments other than drop may not be named
    # need to evaluate i, j, drop, if supplied, before passing on (do not pass on as the sys.call caught originally)
    sc_mod <- sc
    sc_mod[[1L]] <- quote(`[.data.frame`)
    sc_mod[[2L]] <- quote(x)

    if (!missing.i) sc_mod[[3L]] <- i # if present, i is always in pos 3
    if (!missing.j) sc_mod[[4L]] <- j # if present, j is always in pos 4
    if (!missing.drop) sc_mod[[length(sc)]] <- drop # if present, drop is always in last position (4 or 5,
    # depending on the call structure and whether missing j or not)

    mydata <- eval(sc_mod)

    if (is.null(dim(mydata))) {
        # if dim is NULL, subsetting did not return a data frame but  a vector or a
        #   factor or NULL (nothing more is left)
        if (is.null(mydata)) {
            # since R 3.4.0 NULL cannot have attributes, so special case it
            res <- NULL
        } else {
            # vector or factor -> make it a pseries
            res <- structure(mydata,
                             names = x.rownames,
                             index = index,
                             class = unique(c("pseries", class(mydata))))
        }
    } else {
        # subsetting returned a data.frame -> add attributes to make it a pdata.frame again
        res <- structure(mydata,
                         index = index,
                         class = c("pdata.frame", "data.frame"))
    }

    return(res)
}

#' @rdname pdata.frame
#' @export
"[[.pdata.frame" <- function(x, y) {
    index <- attr(x, "index")
    attr(x, "index") <- NULL
    class(x) <- "data.frame"
    result <- "[[.data.frame"(x, y)
    if (!is.null(result)){
        # make extracted column a pseries
        # use this order for attributes to preserve original order of attributes for a pseries
        result <- structure(result,
                            names = row.names(x),
                            class = unique(c("pseries", class(result))),
                            index = index
        )
    }
    result
}

#' @rdname pdata.frame
#' @export
"$.pdata.frame" <- function(x, y) {
    "[[.pdata.frame"(x, paste(as.name(y)))
}

#' @rdname pdata.frame
#' @export
print.pdata.frame <- function(x, ...) {
    attr(x, "index") <- NULL
    class(x) <- "data.frame"
    # This is a workaround: print.data.frame cannot handle
    # duplicated row names which are currently possible for pdata frames
    if (anyDuplicated(rownames(x))) {
        print("Note: pdata.frame contains duplicated row names, thus original row names are not printed")
        rownames(x) <- NULL
    }
    print(x, ...)
}


# pseriesfy() takes a pdata.frame and makes each column a pseries
# names of the pdata.frame are not added to the columns as base R's data.frames
# do not allow for names in columns (but, e.g., a tibble does so since 3.0.0,
# see https://github.com/tidyverse/tibble/issues/837)

#' Turn all columns of a pdata.frame into class pseries.
#'
#' This function takes a pdata.frame and turns all of its columns into
#' objects of class pseries.
#'
#' Background: Initially created pdata.frames have as columns the pure/basic
#' class (e.g., numeric, factor, character). When extracting a column from such
#' a pdata.frame, the extracted column is turned into a pseries.
#'
#'  At times, it can be convenient to apply data transformation operations on
#'  such a `pseriesfy`-ed pdata.frame, see Examples.
#'
#' @name pseriesfy
#' @param x an object of class `"pdata.frame"`,
#' @param \dots further arguments (currently not used).
#' @return A pdata.frame like the input pdata.frame but with all columns
#'         turned into pseries.
#' @seealso [pdata.frame()], [plm::as.list()]
#' @keywords attribute
#' @export
#' @examples
#' library("plm")
#' data("Grunfeld", package = "plm")
#' pGrun <- pdata.frame(Grunfeld[ , 1:4], drop.index = TRUE)
#' pGrun2 <- pseriesfy(pGrun) # pseriesfy-ed pdata.frame
#'
#' # compare classes of columns
#' lapply(pGrun,  class)
#' lapply(pGrun2, class)
#'
#' # When using with()
#' with(pGrun,  lag(value)) # dispatches to base R's lag()
#' with(pGrun2, lag(value)) # dispatches to plm's lag() respect. panel structure
#'
#' # When lapply()-ing
#' lapply(pGrun,  lag) # dispatches to base R's lag()
#' lapply(pGrun2, lag) # dispatches to plm's lag() respect. panel structure
#'
#' # as.list(., keep.attributes = TRUE) on a non-pseriesfy-ed
#' # pdata.frame is similar and dispatches to plm's lag
#' lapply(as.list(pGrun, keep.attributes = TRUE), lag)
#'
pseriesfy <- function(x, ...) {
    if(!inherits(x, "pdata.frame")) stop("input 'x' needs to be a pdata.frame")
    ix <- attr(x, "index")
    nam <- attr(x, "row.names")
    pdf <- as.data.frame(lapply(x, function(col) add_pseries_features(col, ix)))
    class(pdf) <- c("pdata.frame", class(pdf))
    attr(pdf, "index") <- ix
    rownames(pdf) <- nam
    return(pdf)
}

pseriesfy.collapse <- function(x, ...) {
    if(!inherits(x, "pdata.frame")) stop("input 'x' needs to be a pdata.frame")
    ix <- attr(x, "index")
    return(collapse::dapply(x, function(col) add_pseries_features(col, ix)))
}


# as.list.pdata.frame:
# The default is to behave identical to as.list.data.frame.
# This default is necessary, because some code relies on this
# behaviour! Do not change this!
#
#  as.list.data.frame does:
#    * unclass
#    * strips all classes but "list"
#    * strips row.names
#
#  By setting argument keep.attributes = TRUE, the attributes of the pdata.frame
#  are preserved by as.list.pdata.frame: a list of pseries is returned
#  and lapply can be used as usual, now working on a list of pseries, e.g.,
#    lapply(as.list(pdata.frame[ , your_cols], keep.attributes = TRUE), lag)
#  works as expected.

#' @rdname pdata.frame
#' @export
as.list.pdata.frame <- function(x, keep.attributes = FALSE, ...) {
    if (!keep.attributes) {
        x <- as.list.data.frame(x)
    } else {
        # make list of pseries objects
        x_names <- names(x)
        x <- lapply(x_names,
                    FUN = function(element, pdataframe){
                        "[[.pdata.frame"(x = pdataframe, y = element)
                    },
                    pdataframe = x)
        names(x) <- x_names

        # note: this function is slower than the corresponding
        # as.list.data.frame function,
        # because we cannot simply use unclass() on the pdata.frame:
        # need to add index etc to all columns to get proper pseries
        # back => thus the extraction function "[[.pdata.frame" is used
    }
    return(x)
}

#' @rdname pdata.frame
#' @export
as.data.frame.pdata.frame <- function(x, row.names = NULL, optional = FALSE, keep.attributes = TRUE, ...) {
    index <- attr(x, "index")

    if(!keep.attributes) {
        attr(x, "index") <- NULL
        class(x) <- "data.frame"
        rownames(x) <- NULL
    } else {
        # make each column a pseries (w/o names)
        x <- lapply(x,
                    function(z){
                        #     names(z) <- row.names(x) # it is not possible to keep the names in the 'pseries'/
                        # in columns because the call to data.frame later deletes
                        # the names attribute of columns (definition of data frame)
                        attr(z, "index") <- index
                        class(z) <- unique(c("pseries", class(z)))
                        return(z)
                    })
    }

    if(is.null(row.names)) {
        # do as base::as.data.frame does for NULL
        x <- as.data.frame(x, row.names = NULL)
    } else {
        if(is.logical(row.names) && row.names == FALSE) {
            # set row names to integer sequence 1, 2, 3, ...
            x <- as.data.frame(x)
            row.names(x) <- NULL
        }
        if(is.logical(row.names) && row.names == TRUE) {
            # set fancy row names
            x <- as.data.frame(x)
            row.names(x) <- fancy.row.names(index)
        }
        if(is.character(row.names)) {
            x <- as.data.frame(x)
            row.names(x) <- row.names
        }
        if(!(isTRUE(row.names) || isFALSE(row.names) || is.character(row.names)))
            stop("argument 'row.names' is none of NULL, FALSE, TRUE, and not a character")
        # using row.names(x) <- "something" is safer (does not allow
        # duplicate row.names) than # attr(x,"row.names") <- "something"
    }
    return(x)
}


#' Check if an object is a pseries
#'
#' This function checks if an object qualifies as a pseries
#'
#' A `"pseries"` is a wrapper around a "basic class" (numeric, factor,
#' logical, character, or complex).
#'
#' To qualify as a pseries, an object needs to have the following
#' features:
#'
#' - class contains `"pseries"` and there are at least two classes
#' (`"pseries"` and the basic class),
#'
#' - have an appropriate index attribute (defines the panel
#' structure),
#'
#' - any of `is.numeric`, `is.factor`, `is.logical`, `is.character`,
#' `is.complex` is `TRUE`.
#'
#' @param object object to be checked for pseries features
#'
#' @export
#' @return A logical indicating whether the object is a pseries (`TRUE`)
#' or not (`FALSE`).
#' @seealso [pseries()] for some computations on pseries and some
#' further links.
#' @keywords attribute
#' @examples
#'
#' # Create a pdata.frame and extract a series, which becomes a pseries
#' data("EmplUK", package = "plm")
#' Em <- pdata.frame(EmplUK)
#' z <- Em$output
#'
#' class(z) # pseries as indicated by class
#' is.pseries(z) # and confirmed by check
#'
#' # destroy index of pseries and re-check
#' attr(z, "index") <- NA
#' is.pseries(z) # now FALSE
#'
is.pseries <- function(object) {
    # checks if an object has the necessary features to qualify as a 'pseries'
    res <- TRUE
    if (!inherits(object, "pseries")) res <- FALSE
    # class 'pseries' is always on top of basic class: min 2 classes needed, if 2 classes "pseries" needs to be first entry
    if (!length(class(object)) >= 2L) res <- FALSE
    if (length(class(object)) == 2L && class(object)[1L] != "pseries") res <- FALSE
    if (!has.index(object)) res <- FALSE
    if (!any(c(is.numeric(object), is.factor(object), is.logical(object),
               is.character(object), is.complex(object)))) {
        res <- FALSE
    }

    return(res)
}


#' Check for the Dimensions of the Panel
#'
#' This function checks the number of individuals and time observations in the
#' panel and whether it is balanced or not.
#'
#' `pdim` is called by the estimation functions and can be also used
#' stand-alone.
#'
#' @name pdim
#' @aliases pdim
#' @param x a `data.frame`, a `pdata.frame`, a `pseries`, a
#'     `panelmodel`, or a `pgmm` object,
#' @param y a vector,
#' @param index see [pdata.frame()],
#' @param \dots further arguments.
#' @return An object of class `pdim` containing the following
#'     elements:
#'
#' \item{nT}{a list containing `n`, the number of individuals, `T`,
#' the number of time observations, `N` the total number of
#' observations,}
#'
#' \item{Tint}{a list containing two vectors (of type integer): `Ti`
#' gives the number of observations for each individual and `nt` gives
#' the number of individuals observed for each period,}
#'
#' \item{balanced}{a logical value: `TRUE` for a balanced panel,
#' `FALSE` for an unbalanced panel,}
#'
#' \item{panel.names}{a list of character vectors: `id.names` contains
#' the names of each individual and `time.names` contains the names of
#' each period.}
#'
#' @note Calling `pdim` on an estimated `panelmodel` object
#'     and on the corresponding `(p)data.frame` used for this
#'     estimation does not necessarily yield the same result. When
#'     called on an estimated `panelmodel`, the number of
#'     observations (individual, time) actually used for model
#'     estimation are taken into account.  When called on a
#'     `(p)data.frame`, the rows in the `(p)data.frame` are
#'     considered, disregarding any `NA`values in the dependent or
#'     independent variable(s) which would be dropped during model
#'     estimation.
#' @export
#' @author Yves Croissant
#' @seealso [is.pbalanced()] to just determine balancedness
#'     of data (slightly faster than `pdim`),\cr
#'     [punbalancedness()] for measures of
#'     unbalancedness,\cr [nobs()],
#'     [pdata.frame()],\cr [pvar()] to check for
#'     each variable if it varies cross-sectionally and over time.
#' @keywords attribute
#' @examples
#'
#' # There are 595 individuals
#' data("Wages", package = "plm")
#' pdim(Wages, 595)
#'
#' # Gasoline contains two variables which are individual and time
#' # indexes and are the first two variables
#' data("Gasoline", package="plm")
#' pdim(Gasoline)
#'
#' # Hedonic is an unbalanced panel, townid is the individual index
#' data("Hedonic", package = "plm")
#' pdim(Hedonic, "townid")
#'
#' # An example of the panelmodel method
#' data("Produc", package = "plm")
#' z <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp,data=Produc,
#'          model="random", subset = gsp > 5000)
#' pdim(z)
#'
pdim <- function(x, ...) {
    UseMethod("pdim")
}

#' @rdname pdim
#' @export
pdim.default <- function(x, y, ...) {
    if (length(x) != length(y)) stop("The length of the two inputs differs\n")
    x <- x[drop = TRUE] # drop unused factor levels so that table()
    y <- y[drop = TRUE] # gives only needed combinations
    z <- table(x,y)
    Ti <- rowSums(z) # faster than: apply(z, 1, sum)
    nt <- colSums(z) #              apply(z, 2, sum)
    n <- nrow(z)
    T <- ncol(z)
    N <- length(x)
    nT <- list(n = n, T = T, N = N)
    id.names <- rownames(z)
    time.names <- colnames(z)
    panel.names <- list(id.names = id.names, time.names = time.names)
    balanced <- if(any(as.vector(z) == 0)) FALSE else TRUE
    if(any(as.vector(z) > 1)) stop("duplicate couples (id-time)\n")
    Tint <- list(Ti = Ti, nt = nt)
    z <- list(nT = nT, Tint = Tint, balanced = balanced, panel.names = panel.names)
    class(z) <- "pdim"
    z
}

#' @rdname pdim
#' @export
pdim.data.frame <- function(x, index = NULL, ...) {
    x <- pdata.frame(x, index)
    index <- unclass(attr(x, "index"))
    pdim(index[[1L]], index[[2L]])
}

#' @rdname pdim
#' @export
pdim.pdata.frame <- function(x,...) {
    index <- unclass(attr(x, "index"))
    pdim(index[[1L]], index[[2L]])
}

#' @rdname pdim
#' @export
pdim.pseries <- function(x,...) {
    index <- unclass(attr(x, "index"))
    pdim(index[[1L]], index[[2L]])
}

#' @rdname pdim
#' @export
pdim.pggls <- function(x, ...) {
    ## pggls is also class panelmodel, but take advantage of the pdim attribute in it
    attr(x, "pdim")
}

#' @rdname pdim
#' @export
pdim.pcce <- function(x, ...) {
    ## pcce is also class panelmodel, but take advantage of the pdim attribute in it
    attr(x, "pdim")
}

#' @rdname pdim
#' @export
pdim.pmg <- function(x, ...) {
    ## pmg is also class panelmodel, but take advantage of the pdim attribute in it
    attr(x, "pdim")
}

#' @rdname pdim
#' @export
pdim.pgmm <- function(x, ...) {
    ## pgmm is also class panelmodel, but take advantage of the pdim attribute in it
    attr(x, "pdim")
}

#' @rdname pdim
#' @export
pdim.panelmodel <- function(x, ...) {
    x <- model.frame(x)
    pdim(x)
}

#' @rdname pdim
#' @export
print.pdim <- function(x, ...) {
    if (x$balanced){
        cat("Balanced Panel: ")
        cat(paste("n = ", x$nT$n, ", ", sep=""))
        cat(paste("T = ", x$nT$T, ", ", sep=""))
        cat(paste("N = ", x$nT$N, "\n", sep=""))
    }
    else{
        cat("Unbalanced Panel: ")
        cat(paste("n = ", x$nT$n,", ", sep=""))
        cat(paste("T = ", min(x$Tint$Ti), "-", max(x$Tint$Ti), ", ", sep=""))
        cat(paste("N = ", x$nT$N, "\n", sep=""))
    }
    invisible(pdim)
}

#' Extract the indexes of panel data
#'
#' This function extracts the information about the structure of the
#' individual and time dimensions of panel data. Grouping information
#' can also be extracted if the panel data were created with a
#' grouping variable.
#'
#' Panel data are stored in a `"pdata.frame"` which has an `"index"`
#' attribute. Fitted models in `"plm"` have a `"model"` element which
#' is also a `"pdata.frame"` and therefore also has an `"index"`
#' attribute. Finally, each series, once extracted from a
#' `"pdata.frame"`, becomes of class `"pseries"`, which also has this
#' `"index"` attribute.  `"index"` methods are available for all these
#' objects.  The argument `"which"` indicates which index should be
#' extracted. If `which = NULL`, all indexes are extracted. `"which"`
#' can also be a vector of length 1, 2, or 3 (3 only if the pdata
#' frame was constructed with an additional group index) containing
#' either characters (the names of the individual variable and/or of
#' the time variable and/or the group variable or `"id"` and `"time"`)
#' and `"group"` or integers (1 for the individual index, 2 for the
#' time index, and 3 for the group index (the latter only if the pdata
#' frame was constructed with such).)
#'
#' @name index.plm
#' @aliases index
#' @importFrom zoo index
#' @export index
#' @param x an object of class `"pindex"`, `"pdata.frame"`,
#'     `"pseries"` or `"panelmodel"`,
#' @param which the index(es) to be extracted (see details),
#' @param \dots further arguments.
#' @return A vector or an object of class `c("pindex","data.frame")`
#'     containing either one index, individual and time index, or (any
#'     combination of) individual, time and group indexes.
#' @author Yves Croissant
#' @seealso [pdata.frame()], [plm()]
#' @keywords attribute
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' Gr <- pdata.frame(Grunfeld, index = c("firm", "year"))
#' m <- plm(inv ~ value + capital, data = Gr)
#' index(Gr, "firm")
#' index(Gr, "time")
#' index(Gr$inv, c(2, 1))
#' index(m, "id")
#'
#' # with additional group index
#' data("Produc", package = "plm")
#' pProduc <- pdata.frame(Produc, index = c("state", "year", "region"))
#' index(pProduc, 3)
#' index(pProduc, "region")
#' index(pProduc, "group")
#'
NULL

#' @rdname index.plm
#' @export
index.pindex <- function(x, which = NULL, ...) {

    if (is.null(which)) {
        # if no specific index is requested, select all index variables
        which <- names(x)
    }
    else{
        # catch case when someone enters "individual" albeit proper value is
        # "id" to extract individual index
        posindividual <- match("individual", which)
        if (! is.na(posindividual)) which[posindividual] <- "id"
    }
    if (length(which) >  3L) stop("the length of argument 'which' should be at most 3")
    if (is.numeric(which)){
        if (! all(which %in% 1:3))
            stop("if integer, argument 'which' should contain only 1, 2 and/or 3")
        if (ncol(x) == 2L && 3 %in% which) stop("no grouping variable, only 2 indexes")
        which <- names(x)[which]
    }
    nindex <- names(x)
    gindex <- c("id", "time")
    if (ncol(x) == 3L) gindex <- c(gindex, "group")
    if (any(! which %in% c(nindex, gindex))) stop("unknown variable")
    if ("id"    %in% which) {
        which[which == "id"]    <- names(x)[1L]
        if("id" %in% names(x)[-1L]) warning("an index variable not being the invidiual index is called 'id'. Likely, any results are distorted.")
    }
    if ("time"  %in% which) {
        which[which == "time"]  <- names(x)[2L]
        if("time" %in% names(x)[-2L]) warning("an index variable not being the time index is called 'time'. Likely, any results are distorted.")
    }
    if (ncol(x) == 3L) if ("group" %in% which) {
        which[which == "group"] <- names(x)[3L]
        if("group" %in% names(x)[-3L]) warning("an index variable not being the group index is called 'group'. Likely, any results are distorted.")
    }

    result <- x[ , which]
    result
}

#' @rdname index.plm
#' @export
index.pdata.frame <- function(x, which = NULL, ...) {
    anindex <- attr(x, "index")
    index(x = anindex, which = which)
}

#' @rdname index.plm
#' @export
index.pseries <- function(x, which = NULL, ...) {
    anindex <- attr(x, "index")
    index(x = anindex, which = which)
}

#' @rdname index.plm
#' @export
index.panelmodel <- function(x, which = NULL, ...) {
    anindex <- attr(x$model, "index")
    index(x = anindex, which = which)
}


is.index <- function(index) {
    # not exported, helper function
    # checks if the index is an index in the sense of package plm
    if(all(class(index) == c("pindex", "data.frame"))) TRUE else FALSE
}

has.index <- function(object) {
    # not exported, helper function
    # checks if an object has an index in sense of package plm
    # (esp. to distinguish from zoo::index() which always returns an index)
    index <- attr(object, "index")
    return(is.index(index))
}

checkNA.index <- function(index, which = "all", error = TRUE) {
    # not exported, helper function
    #
    # check if any NA in indexes (all or specific dimension)
    #
    # index can be of class pindex (proper index attribute of pdata.frame/pseries
    # or a list of factors, thus can call checkNA.index(unclass(proper_index)))
    # which gives a speed up as the faster list-subetting is used (instead of the
    # relatively slower data.frame-subsetting)

    feedback <- if(error) stop else warning

    if(which == "all") {
        if(anyNA(index[[1L]])) feedback("NA in the individual index variable")
        if(anyNA(index[[2L]])) feedback("NA in the time index variable")
        n.index <- if(inherits(index, "pindex")) ncol(index) else length(index) # else-branche is list (for speed)
        if(n.index == 3L) { if(anyNA(index[[3L]])) feedback("NA in the group index variable") }
    }
    if(which == 1L) {
        if(anyNA(index[[1L]])) feedback("NA in the individual index variable")
    }
    if(which == 2L) {
        if(anyNA(index[[2L]])) feedback("NA in the time index variable")
    }
    if(which == 3L) {
        if(anyNA(index[[3L]])) feedback("NA in the group index variable")
    }
}

# pos.index:
# not exported, helper function
#
# determines column numbers of the index variables in a pdata.frame
# returns named numeric of length 2 or 3 with column numbers of the index variables
# (1: individual index, 2: time index, if available 3: group index),
# names are the names of the index variables
#
# returns c(NA, NA) / c(NA, NA, NA) if the index variables are not a column in the pdata.frame
# (e.g., for pdata.frames created with drop.index = TRUE).
# Cannot detect index variables if their columns names were changed after creation of the pdata.frame

pos.index <- function(x, ...) {
    index <- attr(x, "index")
    index_names <- names(index)
    index_pos <- match(index_names, names(x))
    names(index_pos) <- index_names
    return(index_pos)
}

# tool_ranfixef.R#
## Compute the individual and/or time effects for panel model. plm
## methods for the fixef and ranef generics of the nlme
## package. print, summary and print.summary methods are provided for
## fixef objects.
## The within_intercept.plm function computes the overall intercept of
## within fitted models.



#' @title
#' Extract the Fixed Effects
#'
#' @description
#' Function to extract the fixed effects from a `plm` object and
#' associated summary method.
#'
#' @details
#' Function `fixef` calculates the fixed effects and returns an object
#' of class `c("fixef", "numeric")`. By setting the `type` argument,
#' the fixed effects may be returned in levels (`"level"`), as
#' deviations from the first value of the index (`"dfirst"`), or as
#' deviations from the overall mean (`"dmean"`). If the argument
#' `vcov` was specified, the standard errors (stored as attribute "se"
#' in the return value) are the respective robust standard errors.
#' For two-way fixed-effect models, argument `effect` controls which
#' of the fixed effects are to be extracted: `"individual"`, `"time"`, or
#' the sum of individual and time effects (`"twoways"`).
#' NB: See **Examples** for how the sum of effects can be split in an individual
#' and a time component.
#' For one-way models, the effects of the model are extracted and the
#' argument `effect` is disrespected.
#'
#' The associated `summary` method returns an extended object of class
#' `c("summary.fixef", "matrix")` with more information (see sections
#' **Value** and **Examples**).
#'
#' References with formulae (except for the two-ways unbalanced case)
#' are, e.g., \insertCite{GREE:12;textual}{plm}, Ch. 11.4.4, p. 364,
#' formulae (11-25); \insertCite{WOOL:10;textual}{plm}, Ch. 10.5.3,
#' pp. 308-309, formula (10.58).
#' @name fixef.plm
#' @aliases fixef
#' @param x,object an object of class `"plm"`, an object of class
#'     `"fixef"` for the `print` and the `summary` method,
#' @param effect one of `"individual"`, `"time"`, or `"twoways"`, only relevant in
#'     case of two--ways effects models (where it defaults to `"individual"`),
#' @param vcov a variance--covariance matrix furnished by the user or
#'     a function to calculate one (see **Examples**),
#' @param type one of `"level"`, `"dfirst"`, or `"dmean"`,
#' @param digits digits,
#' @param width the maximum length of the lines in the print output,
#' @param \dots further arguments.
#' @return For function `fixef`, an object of class `c("fixef", "numeric")`
#'     is returned: It is a numeric vector containing
#'     the fixed effects with attribute `se` which contains the
#'     standard errors. There are two further attributes: attribute
#'     `type` contains the chosen type (the value of argument `type`
#'     as a character); attribute `df.residual` holds the residual
#'     degrees of freedom (integer) from the fixed effects model (plm
#'     object) on which `fixef` was run. For the two-way unbalanced case, only
#'     attribute `type` is added.
#'
#' For function `summary.fixef`, an object of class
#' `c("summary.fixef", "matrix")` is returned: It is a matrix with four
#' columns in this order: the estimated fixed effects, their standard
#' errors and associated t--values and p--values.
#' For the two-ways unbalanced case, the matrix contains only the estimates.
#' The type of the fixed effects and the standard errors in the
#' summary.fixef object correspond to was requested in the `fixef`
#' function by arguments `type` and `vcov`, respectively.
#'
#' @author Yves Croissant
#' @seealso [within_intercept()] for the overall intercept of fixed
#'     effect models along its standard error, [plm()] for plm objects
#'     and within models (= fixed effects models) in general. See
#'     [ranef()] to extract the random effects from a random effects
#'     model.
#' @references \insertAllCited{}
#' @keywords regression
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
#' fixef(gi)
#' summary(fixef(gi))
#' summary(fixef(gi))[ , c("Estimate", "Pr(>|t|)")] # only estimates and p-values
#'
#' # relationship of type = "dmean" and "level" and overall intercept
#' fx_level <- fixef(gi, type = "level")
#' fx_dmean <- fixef(gi, type = "dmean")
#' overallint <- within_intercept(gi)
#' all.equal(overallint + fx_dmean, fx_level, check.attributes = FALSE) # TRUE
#'
#' # extract time effects in a twoways effects model
#' gi_tw <- plm(inv ~ value + capital, data = Grunfeld,
#'           model = "within", effect = "twoways")
#' fixef(gi_tw, effect = "time")
#'
#' # with supplied variance-covariance matrix as matrix, function,
#' # and function with additional arguments
#' fx_level_robust1 <- fixef(gi, vcov = vcovHC(gi))
#' fx_level_robust2 <- fixef(gi, vcov = vcovHC)
#' fx_level_robust3 <- fixef(gi, vcov = function(x) vcovHC(x, method = "white2"))
#' summary(fx_level_robust1) # gives fixed effects, robust SEs, t- and p-values
#'
#' # calc. fitted values of oneway within model:
#' fixefs <- fixef(gi)[index(gi, which = "id")]
#' fitted_by_hand <- fixefs + gi$coefficients["value"]   * gi$model$value +
#'                            gi$coefficients["capital"] * gi$model$capital
#'
#' # calc. fittes values of twoway unbalanced within model via effects:
#' gtw_u <- plm(inv ~ value + capital, data = Grunfeld[-200, ], effect = "twoways")
#' yhat <- as.numeric(gtw_u$model[ , 1] - gtw_u$residuals) # reference
#' pred_beta <- as.numeric(tcrossprod(coef(gtw_u), as.matrix(gtw_u$model[ , -1])))
#' pred_effs <- as.numeric(fixef(gtw_u, "twoways")) # sum of ind and time effects
#' all.equal(pred_effs + pred_beta, yhat) # TRUE
#'
#' # Splits of summed up individual and time effects:
#' # use one "level" and one "dfirst"
#' ii <- index(gtw_u)[[1L]]; it <- index(gtw_u)[[2L]]
#' eff_id_dfirst <- c(0, as.numeric(fixef(gtw_u, "individual", "dfirst")))[ii]
#' eff_ti_dfirst <- c(0, as.numeric(fixef(gtw_u, "time",       "dfirst")))[it]
#' eff_id_level <- as.numeric(fixef(gtw_u, "individual"))[ii]
#' eff_ti_level <- as.numeric(fixef(gtw_u, "time"))[it]
#'
#' all.equal(pred_effs, eff_id_level  + eff_ti_dfirst) # TRUE
#' all.equal(pred_effs, eff_id_dfirst + eff_ti_level)  # TRUE
#'
#' @importFrom nlme fixef
#' @export fixef
NULL

#' @rdname fixef.plm
#' @importFrom stats weighted.mean
#' @export
fixef.plm <- function(object, effect = NULL,
                      type = c("level", "dfirst", "dmean"),
                      vcov = NULL, ...){

    model.effect <- describe(object, "effect")
    if(is.null(effect)){
        # default for twoway model to individual effect
        effect <- switch(model.effect,
                         "individual" = "individual",
                         "time"       = "time",
                         "twoways"    = "individual")
    }
    else{
        if(model.effect != "twoways" && model.effect != effect) stop("wrong effect argument")
        if(!effect %in% c("individual", "time", "twoways")) stop("wrong effect argument")
    }

    type <- match.arg(type)
    if(!is.null(object$call)){
        if(describe(object, "model") != "within")
            stop("fixef is relevant only for within models")
    }
    formula <- formula(object)
    data <- model.frame(object)
    pdim <- pdim(object)
    # the between model may contain time independent variables, the
    # within model doesn't. So select the relevant elements using nw
    # (names of the within variables)
    nw <- names(coef(object))

    # For procedure to get the individual/time effects by multiplying the within
    # estimates with the between-ed data, see, e.g.:
    #  Wooldridge (2010), Econometric Analysis of Cross Section and Panel Data, 2nd ed.,
    #                     Ch. 10.5.3, pp. 308-309, formula (10.58)
    #  Greene (2012), Econometric Analysis,
    #                 Ch. 11.4.4, p. 364, formulae (11-25)
    #
    # NB: These textbook formulae do not give the correct results in the two-ways unbalanced case,
    #     all other cases (twoways/balanced; oneway(ind/time)/balanced/unbalanced) are correct
    #     for these formulae.
    if(model.effect != "twoways") {
        Xb <- model.matrix(data, rhs = 1, model = "between", effect = effect)
        yb <- pmodel.response(data, model = "between", effect = effect)
        fixef <- yb - as.vector(crossprod(t(Xb[ , nw, drop = FALSE]), coef(object)))

        # use robust vcov if supplied
        if (! is.null(vcov)) {
            if (is.matrix(vcov))   vcov <- vcov[nw, nw]
            if (is.function(vcov)) vcov <- vcov(object)[nw, nw]
        } else {
            vcov <- vcov(object)[nw, nw]
        }

        nother <- switch(effect,
                         "individual" = pdim$Tint$Ti,
                         "time"       = pdim$Tint$nt)

        s2 <- deviance(object) / df.residual(object)
        if (type != "dfirst") {
            sefixef <- sqrt(s2 / nother + apply(Xb[, nw, drop = FALSE], 1,
                                                function(x) t(x) %*% vcov %*% x))
        } else {
            Xb <- t(t(Xb[-1, ]) - Xb[1L, ])
            sefixef <- sqrt(s2 * (1 / nother[-1] + 1 / nother[1])+
                                apply(Xb[, nw, drop = FALSE], 1,
                                      function(x) t(x) %*% vcov %*% x))
        }
        res <- switch(type,
                      "level"  = fixef,
                      "dfirst" = fixef[2:length(fixef)] - fixef[1L],
                      "dmean"  = (fixef - weighted.mean(fixef, w = nother)))

        res <- structure(res, se = sefixef, class = c("fixef", "numeric"),
                         type = type, df.residual = df.residual(object))
    } else {
        ## case model.effect == "twoways"
        ##  * two-way balanced/unbalanced model for all effects

        beta.data <- as.numeric(tcrossprod(coef(object),
                                           model.matrix(object, model = "pooling")[ , nw, drop = FALSE]))
        yhat <- object$model[ , 1L] - object$residuals
        tw.fixef.lvl <- yhat - beta.data # sum of both effects in levels

        idx <- switch(effect,
                      "individual" = 1L,
                      "time"       = 2L,
                      "twoways"    = NA_integer_) # needed for weighted.mean below -> leads to no weights

        indexl <- unclass(index(object)) # unclass to list for speed

        if(effect %in% c("individual", "time")) {
            other.eff <- switch(effect,
                                "individual" = "time",
                                "time"       = "individual")

            other.idx <- switch(effect,
                                "individual" = 2L,
                                "time"       = 1L)

            Xb <- model.matrix(data, rhs = 1, model = "between", effect = other.eff)
            yb <- pmodel.response(data, model = "between", effect = other.eff)
            other.fixef.lvl <- yb - as.vector(crossprod(t(Xb[ , nw, drop = FALSE]), coef(object)))

            ## other dfirst
            other.fixef.dfirst <- other.fixef.lvl - other.fixef.lvl[1L]
            tw.fixef.lvl <- tw.fixef.lvl - other.fixef.dfirst[indexl[[other.idx]]]

            tw.fixef.lvl <- tw.fixef.lvl[!duplicated(indexl[[idx]])]
            names(tw.fixef.lvl) <- pdim[["panel.names"]][[idx]]
        } else {
            # effect = "twoways": everything already computed, just set names
            names(tw.fixef.lvl) <- paste0(pdim[["panel.names"]][[1L]][indexl[[1L]]], "-",
                                          pdim[["panel.names"]][[2L]][indexl[[2L]]])
        }

        res <- switch(type,
                      "level"  = tw.fixef.lvl,
                      "dfirst" = tw.fixef.lvl[2:length(tw.fixef.lvl)] - tw.fixef.lvl[1L],
                      "dmean"  = {
                          if(pdim$balanced || effect == "twoways") {
                              tw.fixef.lvl - mean(tw.fixef.lvl)
                          } else {
                              tw.fixef.lvl - weighted.mean(tw.fixef.lvl, w = pdim$Tint[[idx]])
                          }})

        res <- structure(res, se = NULL, class = c("fixef", "numeric"),
                         type = type, df.residual = NULL)
    }
    res
}


#' @rdname fixef.plm
#' @export
print.fixef <- function(x, digits = max(3, getOption("digits") - 2),
                        width = getOption("width"), ...){
    x.orig <- x
    # prevent attributes from being printed
    attr(x, "se") <- attr(x, "type") <- attr(x, "class") <- attr(x, "df.residual") <- attr(x, "index") <- NULL
    print.default(x, digits, width, ...)
    invisible(x.orig)
}


#' @rdname fixef.plm
#' @export
summary.fixef <- function(object, ...) {
    # for 2-way unbalanced, there are currently no further attributes -> skip construction
    res <- if(!is.null(attr(object, "se"))) {
        se <- attr(object, "se")
        df.res <- attr(object, "df.residual")
        tvalue <- (object) / se
        # was: res <- cbind(object, se, zvalue, (1 - pnorm(abs(zvalue))) * 2)
        res <- cbind(object, se, tvalue, (2 * pt(abs(tvalue), df = df.res, lower.tail = FALSE)))
        # see for distribution and degrees of freedom
        #   Greene (2003, 5th ed.), p.  288     (formula 13-7)
        # = Greene (2012, 7th ed.), pp. 361-362 (formula 11-19)
        colnames(res) <- c("Estimate", "Std. Error", "t-value", "Pr(>|t|)")
        class(res) <- c("summary.fixef", "matrix")
        attr(res, "type") <- attr(object, "type")
        attr(res, "df.residual") <- df.res
        res
    } else {
        matrix(object, dimnames = list(names(object), "Estimate"))
    }
    res
}

#' @rdname fixef.plm
#' @export
print.summary.fixef <- function(x, digits = max(3, getOption("digits") - 2),
                                width = getOption("width"), ...){
    printCoefmat(x, digits = digits)
    invisible(x)
}

#' @rdname fixef.plm
#' @export
fixef.pggls <- fixef.plm







#' Extract the Random Effects
#'
#' Function to calculate the random effects from a `plm` object
#' (random effects model).
#'
#' Function `ranef` calculates the random effects of a fitted random
#' effects model. For one-way models, the effects of the estimated
#' model are extracted (either individual or time effects). For
#' two-way models, extracting the individual effects is the default
#' (both, argument `effect = NULL` and `effect = "individual"` will
#' give individual effects). Time effects can be extracted by setting
#' `effect = "time"`.
#'
#' Not all random effect model types are supported (yet?).
#'
#' @param object an object of class `"plm"`, needs to be a fitted
#'     random effects model,
#' @param effect `NULL`, `"individual"`, or `"time"`, the effects to
#'     be extracted, see **Details**,
#' @param \dots further arguments (currently not used).
#' @return A named numeric with the random effects per dimension
#'     (individual or time).
#' @name ranef.plm
#' @aliases ranef
#' @importFrom nlme ranef
#' @export ranef
#' @author Kevin Tappe
#' @seealso [fixef()] to extract the fixed effects from a fixed
#'     effects model (within model).
#' @keywords regression
#' @examples
#'
#' data("Grunfeld", package = "plm")
#' m1 <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
#' ranef(m1) # individual random effects
#'
#' # compare to random effects by ML estimation via lme from package nlme
#' library(nlme)
#' m2 <- lme(inv ~ value + capital, random = ~1|firm, data = Grunfeld)
#' cbind("plm" = ranef(m1), "lme" = unname(ranef(m2)))
#'
#' # two-ways RE model, calculate individual and time random effects
#' data("Cigar", package = "plm")
#' tw <- plm(sales ~ pop + price, data = Cigar, model = "random", effect = "twoways")
#' ranef(tw)                   # individual random effects
#' ranef(tw, effect = "time")  # time random effects
#'
NULL

#' @rdname ranef.plm
#' @export
ranef.plm <- function(object, effect = NULL, ...) {
    # TODO:
    #      Check if the same procedure can be applied to
    #       * unbalanced two-way case (for now: implemented the same way, but not entirely sure)
    #       * random IV models
    #       * nested random effect models
    model <- describe(object, "model")
    obj.effect <- describe(object, "effect")
    balanced <- is.pbalanced(object)

    if(model != "random") stop("only applicable to random effect models")
    # TODO: Are random effects for nested models and IV models calculated the same way?
    #       Be defensive here and error for such models.
    if(obj.effect == "nested")  stop("nested random effect models are not supported (yet?)")
    if(length(object$formula)[2L] >= 2L) stop("ranef: IV models not supported (yet?)")

    if(!is.null(effect) && !(effect %in% c("individual", "time")))
        stop("argument 'effect' must be NULL, \"individual\", or \"time\"")
    if(obj.effect != "twoways" && !is.null(effect) && effect != obj.effect)
        stop(paste0("for one-way models, argument \"effect\" must be NULL or match the effect introduced in model estimation"))

    # default effect is the model's effect
    # for two-ways RE models: set default to effect = "individual"
    if(obj.effect == "twoways" && is.null(effect)) effect <- "individual"
    if(is.null(effect)) effect <- obj.effect

    erc <- ercomp(object)
    # extract theta, but depending on model/effect, it is adjusted/overwritten later
    theta <- unlist(erc["theta"], use.names = FALSE)

    # res <- object$residuals                # gives residuals of quasi-demeaned model
    res <- residuals_overall_exp.plm(object) # but need RE residuals of overall model

    if(!inherits(res, "pseries")) {
        # just make sure we have a pseries for the following between() to work
        attr(res, "index") <- index(object$model)
        class(res) <- c("pseries", class(res))
    }

    # mean_res <- Between(res, effect = effect)  # has length == # observations
    mean_res <- between(res, effect = effect)    # but need length == # individuals

    if(obj.effect == "twoways" && balanced) {
        theta <- switch(effect,
                        "individual" = theta[1L],
                        "time"       = theta[2L])
    }
    if(obj.effect == "twoways" && !balanced) {
        theta <- erc[["theta"]][[if(effect == "individual") "id" else "time"]]
    }

    if(!balanced) {
        # in the unbalanced cases, ercomp[["theta"]] is full length (# obs)
        #  -> reduce to per id/time
        select <- switch(effect,
                         "individual" = !duplicated(index(object$model)[1L]),
                         "time"       = !duplicated(index(object$model)[2L]))
        theta <- theta[select]
    }

    # calculate random effects:
    # This formula works (at least) for:
    #  balanced one-way (is symmetric for individual/time)
    #  unbalanced one-way (symmetric) is also caught by this line as theta is reduced before
    #  balanced two-way case (symmetric)
    raneffects <- (1 - (1 - theta)^2) * mean_res
    names(raneffects) <- names(mean_res)
    return(raneffects)
}




#' Overall Intercept for Within Models Along its Standard Error
#'
#' This function gives an overall intercept for within models and its
#' accompanying standard error or an within model with the overall intercept
#'
#' The (somewhat artificial) intercept for within models (fixed
#' effects models) was made popular by Stata of StataCorp
#' \insertCite{@see @GOUL:13}{plm}, EViews of IHS, and gretl
#' \insertCite{@see @GRETL:2021, p. 200-201, listing 23.1}{plm}, see for
#' treatment in the literature,
#' e.g., \insertCite{GREE:12;textual}{plm}, Ch. 11.4.4, p. 364. It can
#' be considered an overall intercept in the within model framework
#' and is the weighted mean of fixed effects (see **Examples** for the
#' relationship).
#'
#' `within_intercept` estimates a new model which is
#' computationally more demanding than just taking the weighted
#' mean. However, with `within_intercept` one also gets the
#' associated standard error and it is possible to get an overall
#' intercept for twoway fixed effect models.
#'
#' Users can set argument `vcov` to a function to calculate a
#' specific (robust) variance--covariance matrix and get the
#' respective (robust) standard error for the overall intercept,
#' e.g., the function [vcovHC()], see examples for
#' usage. Note: The argument `vcov` must be a function, not a
#' matrix, because the model to calculate the overall intercept for
#' the within model is different from the within model itself.
#'
#' If argument `return.model = TRUE` is set, the full model object is returned,
#' while in the default case only the intercept is returned.
#'
#' @aliases within_intercept
#' @param object object of class `plm` which must be a within
#'     model (fixed effects model),
#' @param vcov if not `NULL` (default), a function to calculate a
#'     user defined variance--covariance matrix (function for robust
#'     vcov), only used if `return.model = FALSE`,
#' @param return.model a logical to indicate whether only the overall intercept
#'     (`FALSE` is default) or a full model object (`TRUE`) is to be returned,
#' @param \dots further arguments (currently none).
#' @return Depending on argument `return.model`:  If `FALSE` (default), a named
#' `numeric` of length one: The overall intercept for the estimated within model
#'  along attribute "se" which contains the standard error for the intercept.
#'  If `return.model = TRUE`, the full model object, a within model with the
#'  overall intercept (NB: the model identifies itself as a pooling model, e.g.,
#'  in summary()).
#'
#' @export
#' @author Kevin Tappe
#' @seealso [fixef()] to extract the fixed effects of a within model.
#' @references
#'
#' \insertAllCited{}
#'
#' @keywords attribute
#' @examples
#' data("Hedonic", package = "plm")
#' mod_fe <- plm(mv ~ age + crim, data = Hedonic, index = "townid")
#' overallint <- within_intercept(mod_fe)
#' attr(overallint, "se") # standard error
#'
#' # overall intercept is the weighted mean of fixed effects in the
#' # one-way case
#' weighted.mean(fixef(mod_fe), pdim(mod_fe)$Tint$Ti)
#'
#' ### relationship of type="dmean", "level" and within_intercept
#' ## one-way balanced case
#' data("Grunfeld", package = "plm")
#' gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
#' fx_level <- fixef(gi, type = "level")
#' fx_dmean <- fixef(gi, type = "dmean")
#' overallint <- within_intercept(gi)
#' all.equal(overallint + fx_dmean, fx_level, check.attributes = FALSE) # TRUE
#' ## two-ways unbalanced case
#' gtw_u <- plm(inv ~ value + capital, data = Grunfeld[-200, ], effect = "twoways")
#' int_tw_u <- within_intercept(gtw_u)
#' fx_dmean_tw_i_u <- fixef(gtw_u, type = "dmean", effect = "individual")[index(gtw_u)[[1L]]]
#' fx_dmean_tw_t_u <- fixef(gtw_u, type = "dmean", effect = "time")[index(gtw_u)[[2L]]]
#' fx_level_tw_u <- as.numeric(fixef(gtw_u, "twoways", "level"))
#' fx_level_tw_u2 <- int_tw_u + fx_dmean_tw_i_u + fx_dmean_tw_t_u
#' all.equal(fx_level_tw_u, fx_level_tw_u2, check.attributes = FALSE) # TRUE
#'
#' ## overall intercept with robust standard error
#' within_intercept(gi, vcov = function(x) vcovHC(x, method="arellano", type="HC0"))
#'
#' ## have a model returned
#' mod_fe_int <- within_intercept(gi, return.model = TRUE)
#' summary(mod_fe_int)
#' # replicates Stata's robust standard errors
#' summary(mod_fe_int, vcvov = function(x) vcovHC(x, type = "sss"))
#
within_intercept <- function(object, ...) {
    UseMethod("within_intercept")
}

# Note: The name of the function (within_intercept) with an underscore does not
#       follow the regular naming scheme where one would expect a dot (within.intercept).
#       Due to the S3 class system, calling the function within.intercept would result in
#       a name clash as we have a function called 'within' and in this case the S3
#       system interprets '.intercept' as a class called 'intercept'.

# Note: return value of within_intercept is related to return values of fixef.plm,
#       see tests/test_within_intercept.R

#' @rdname within_intercept
#' @export
within_intercept.plm <- function(object, vcov = NULL, return.model = FALSE, ...) {
    if(!inherits(object, "plm")) stop("input 'object' needs to be a \"within\" model estimated by plm()")
    if(length(object$formula)[2L] >= 2L) stop("within_intercept: IV models not supported (yet?)")
    model  <- describe(object, what = "model")
    effect <- describe(object, what = "effect")
    if(model != "within") stop("input 'object' needs to be a \"within\" model estimated by plm(..., model = \"within\", ...)")

    # vcov must be a function, because the auxiliary model estimated to get the
    # overall intercept next to its standard errors is different from
    # the FE model for which the intercept is estimated, e.g., dimensions
    # of vcov differ for FE and for auxiliary model.
    if(!is.null(vcov)) {
        if(is.matrix(vcov)) stop("for within_intercept, 'vcov' may not be of class 'matrix', it must be supplied as a function, e.g., vcov = function(x) vcovHC(x)")
        if(!is.function(vcov)) stop("for within_intercept, argument 'vcov' must be a function, e.g., vcov = function(x) vcovHC(x)")
    }

    index <- attr(object$model, which = "index")

    # Transformation to get the overall intercept is:
    # demean groupwise and add back grand mean of each variable, then run OLS
    mf      <- model.frame(object)
    withinY <- pmodel.response(object) # returns the response specific to the 'effect' of the est. FE model object
    meanY   <- mean(mf[ , 1L]) # mean of original data's response
    transY  <- withinY + meanY

    withinM <- model.matrix(object) # returns the model.matrix specific to the 'effect' of the est. FE model object
    M <- model.matrix(mf, cstcovar.rm = "all")
    M <- M[ , colnames(M) %in% colnames(withinM), drop = FALSE] # just to be sure: should be same columns
    meansM <- colMeans(M)
    transM <- t(t(withinM) + meansM)

    # estimation by lm()
    # data <- data.frame(cbind(transY, transM))
    # auxreg <- lm(data)
    # summary(auxreg)

    # estimation by plm() - to apply robust vcov function if supplied
    # NB: this changes variable names slightly (data.frame uses make.names to, e.g., get rid of parentheses in variable names)
    data <- pdata.frame(data.frame(cbind(index, transY, transM)), drop.index = TRUE)
    form <- as.formula(paste0(names(data)[1L], "~", paste(names(data)[-1L], collapse = "+")))
    auxreg <- plm(form, data = data, model = "pooling")

    # degrees of freedom correction due to FE transformation for "normal" vcov [copied over from plm.fit]
    pdim <- pdim(index)
    card.fixef <- switch(effect,
                         "individual" = pdim$nT$n,
                         "time"       = pdim$nT$T,
                         "twoways"    = pdim$nT$n + pdim$nT$T - 1L)
    df <- df.residual(auxreg) - card.fixef  + 1L # just for within_intercept: here we need '+1' to correct for the intercept

    vcov_mat <- vcov(auxreg)
    vcov_mat <- vcov_mat * df.residual(auxreg) / df
    auxreg$vcov <- vcov_mat # plug in new vcov (adjusted "normal" vcov) in auxiliary model

    res <- if(!return.model) {
        #### return only intercept with SE as attribute
        ##  in case of robust vcov, which is supplied by a function
        ##  no adjustment to the robust vcov is necessary
        if(is.function(vcov)) vcov_mat <- vcov(auxreg) # robust vcov as supplied by a function
        intercept <- auxreg[["coefficients"]]["(Intercept)"]
        attr(intercept, which = "se") <- sqrt(vcov_mat[1L, 1L])
        names(intercept) <- "(overall_intercept)"
        intercept
    } else {
        ### return model
        if(!is.null(vcov)) warning("argument 'vcov' is non-NULL and is ignored as 'return.model = TRUE' is set")
        auxreg
    }
    return(res)
} # END within_intercept.plm

# tool_transformations.R#
## This file contains the relevant transformations used for panel data,
## namely of course Within and between/Between, but also Sum (useful for
## unbalanced panels).

## They are all generics and have default, pseries and matrix
## methods. The effect argument is an index vector for the default method
## and a character ("individual", "time", "group", "twoways") for the
## pseries method. It can be any of the two for the matrix method (the
## second one only if the matrix argument has an index attribute

## diff, lag and lead methods for pseries are also provided (lead is a
## generic exported by plm, lag and diff being generic exported by
## stats). All of them have a shift argument which can be either "time"
## or "row".



#' panel series
#'
#' A class for panel series for which several useful computations and
#' data transformations are available.
#'
#' The functions `between`, `Between`, `Within`, and `Sum` perform specific
#' data transformations, i. e., the between, within, and sum transformation,
#' respectively.
#'
#' `between` returns a vector/matrix containing the individual means (over
#' time) with the length of the vector equal to the number of
#' individuals (if `effect = "individual"` (default); if `effect = "time"`,
#' it returns the time means (over individuals)). `Between`
#' duplicates the values and returns a vector/matrix which length/number of rows
#' is the number of total observations. `Within` returns a vector/matrix
#' containing the values in deviation from the individual means
#' (if `effect = "individual"`, from time means if `effect = "time"`), the so
#' called demeaned data. `Sum` returns a vector/matrix with sum per individual
#' (over time) or the sum per time period (over individuals) with
#' `effect = "individual"` or `effect = "time"`, respectively, and has length/
#' number of rows of the total observations (like `Between`).
#'
#' For `between`, `Between`, `Within`, and `Sum` in presence of NA values it
#' can be useful to supply `na.rm = TRUE` as an additional argument to
#' keep as many observations as possible in the resulting transformation.
#' na.rm is passed on to the mean()/sum() function used by these transformations
#' (i.e., it does not remove NAs prior to any processing!), see also
#' **Examples**.
#'
#' @name pseries
#' @aliases pseries
#' @param x,object a `pseries` or a matrix; or a `summary.pseries` object,
#' @param effect for the pseries methods: character string indicating the
#'     `"individual"`, `"time"`, or `"group"` effect, for `Within`
#'     `"twoways"` additionally; for non-pseries methods, `effect` is a factor
#'     specifying the dimension (`"twoways"` is not possible),
#' @param idbyrow if `TRUE` in the `as.matrix` method, the lines of
#'     the matrix are the individuals,
#' @param plot,scale,transparency,col,lwd plot arguments,
#' @param \dots further arguments, e. g., `na.rm = TRUE` for
#'     transformation functions like `beetween`, see **Details**
#'     and **Examples**.
#' @return All these functions return an object of class `pseries` or a matrix,
#'     except:\cr `between`, which returns a numeric vector or a matrix;
#'     `as.matrix`, which returns a matrix.
#' @author Yves Croissant
#' @seealso [is.pseries()] to check if an object is a pseries. For
#'     more functions on class 'pseries' see [lag()], [lead()],
#'     [diff()] for lagging values,  <- ing values (negative lags) and
#'     differencing.
#' @keywords classes
#' @examples
#'
#' # First, create a pdata.frame
#' data("EmplUK", package = "plm")
#' Em <- pdata.frame(EmplUK)
#'
#' # Then extract a series, which becomes additionally a pseries
#' z <- Em$output
#' class(z)
#'
#' # obtain the matrix representation
#' as.matrix(z)
#'
#' # compute the between and within transformations
#' between(z)
#' Within(z)
#'
#' # Between and Sum replicate the values for each time observation
#' Between(z)
#' Sum(z)
#'
#' # between, Between, Within, and Sum transformations on other dimension
#' between(z, effect = "time")
#' Between(z, effect = "time")
#' Within(z, effect = "time")
#' Sum(z, effect = "time")
#'
#' # NA treatment for between, Between, Within, and Sum
#' z2 <- z
#' z2[length(z2)] <- NA # set last value to NA
#' between(z2, na.rm = TRUE) # non-NA value for last individual
#' Between(z2, na.rm = TRUE) # only the NA observation is lost
#' Within(z2, na.rm = TRUE)  # only the NA observation is lost
#' Sum(z2, na.rm = TRUE)     # only the NA observation is lost
#'
#' sum(is.na(Between(z2))) # 9 observations lost due to one NA value
#' sum(is.na(Between(z2, na.rm = TRUE))) # only the NA observation is lost
#' sum(is.na(Within(z2))) # 9 observations lost due to one NA value
#' sum(is.na(Within(z2, na.rm = TRUE))) # only the NA observation is lost
#' sum(is.na(Sum(z2))) # 9 observations lost due to one NA value
#' sum(is.na(Sum(z2, na.rm = TRUE))) # only the NA observation is lost
#'
NULL



#' @rdname pseries
#' @export
print.pseries <- function(x, ...){
    x.orig <- x
    attr(x, "index") <- NULL
    attr(x, "class") <- base::setdiff(attr(x, "class"), "pseries")
    if(length(attr(x, "class")) == 1L && class(x) %in% c("character", "logical", "numeric", "integer", "complex")) {
        attr(x, "class") <- NULL
    }
    print(x, ...)
    x.orig
}

#' @rdname pseries
#' @export
as.matrix.pseries <- function(x, idbyrow = TRUE, ...){
    index <- unclass(attr(x, "index")) # unclass for speed
    id <- index[[1L]]
    time <- index[[2L]]
    time.names <- levels(time)
    x <- split(data.frame(x, time), id)
    x <- lapply(x, function(x){
        rownames(x) <- x[ , 2L]
        x[ , -2L, drop = FALSE]
    })
    x <- lapply(x, function(x){
        x <- x[time.names, , drop = FALSE]
        rownames(x) <- time.names
        x
    }
    )
    id.names <- names(x)
    x <- as.matrix(as.data.frame((x)))
    colnames(x) <- id.names
    if(idbyrow) x <- t(x)
    x
}

## plots a panel series by time index
##
## can supply any panel function, e.g., a loess smoother
## > mypanel<-function(x,...) {
## + panel.xyplot(x,...)
## + panel.loess(x, col="red", ...)}
## >
## > plot(pres(mod), panel=mypanel)

#' @rdname pseries
#' @importFrom lattice xyplot
#' @export
plot.pseries <- function(x, plot = c("lattice", "superposed"),
                         scale = FALSE, transparency = TRUE,
                         col = "blue", lwd = 1, ...) {

    if(scale) {
        scalefun <- function(x) scale(x)
    } else {
        scalefun <- function(x) return(x)}

    nx <- as.numeric(x)
    ind <- attr(x, "index")[[1L]]
    tind <- attr(x, "index")[[2L]] # possibly as.numeric():
    # activates autom. tick
    # but loses time labels

    xdata <- data.frame(nx = nx, ind = ind, tind = tind)

    switch(match.arg(plot),
           "lattice" = {
               ##require(lattice) # make a ggplot2 version
               xyplot(nx ~ tind | ind, data = xdata, type = "l", col = col, ...)
           },
           "superposed" = {
               ylim <- c(min(tapply(scalefun(nx), ind, min, na.rm = TRUE)),
                         max(tapply(scalefun(nx), ind, max, na.rm = TRUE)))
               unind <- unique(ind)
               nx1 <- nx[ind == unind[1L]]
               tind1 <- as.numeric(tind[ind == unind[1L]])
               ## plot empty plot to provide frame
               plot(NA, xlim = c(min(as.numeric(tind)),
                                 max(as.numeric(tind))),
                    ylim = ylim, xlab = "", ylab = "", xaxt = "n", ...)
               axis(1, at = as.numeric(unique(tind)),
                    labels = unique(tind))

               ## determine lwd and transparency level as a function
               ## of n
               if(transparency) {
                   alpha <- 5 / length(unind)
                   col <- heat.colors(1, alpha = alpha)
                   lwd <- length(unind) / 10
               }
               ## plot lines (notice: tind. are factors, so they
               ## retain the correct labels which would be lost if
               ## using as.numeric
               for(i in 1:length(unind)) {
                   nxi <- nx[ind == unind[i]]
                   tindi <- tind[ind == unind[i]]
                   lines(x = tindi, y = scalefun(nxi),
                         col = col, lwd = lwd, ...)
               }
           })
}

#' @rdname pseries
#' @export
summary.pseries <- function(object, ...) {
    object.orig <- object
    special_treatment_vars <- c("factor", "logical", "character")
    if(!inherits(object, special_treatment_vars)) {
        index <- unclass(attr(object, "index")) # unclass for speed
        id   <- index[[1L]]
        time <- index[[2L]]
        Bid   <- Between(object, na.rm = TRUE)
        Btime <- Between(object, effect = "time", na.rm = TRUE)
        ## res <- structure(c(total = sumsq(object),
        ##                    between_id = sumsq(Bid),
        ##                    between_time = sumsq(Btime)),
        ##                  class = c("summary.pseries", "numeric"))
        res <- structure(c(total        = sum( (na.omit(object) - mean(object, na.rm = TRUE)) ^ 2),
                           between_id   = sum( (na.omit(Bid)    - mean(Bid,    na.rm = TRUE)) ^ 2),
                           between_time = sum( (na.omit(Btime)  - mean(Btime,  na.rm = TRUE)) ^ 2)),
                         class = c("summary.pseries"),
                         class.pseries = class(object.orig))
        attr(res, "SummaryDefault") <- summary(remove_pseries_features(object))
    } else {
        object <- remove_pseries_features(object)
        res <- summary(object, ...)
        attr(res, "class.pseries") <- class(object.orig)
        class(res) <- c("summary.pseries")
    }
    return(res)
}

#' @rdname pseries
#' @export
plot.summary.pseries <- function(x, ...){
    special_treatment_vars <- c("factor", "logical", "character")
    class.basic <- setdiff(attr(x, "class.pseries"), "pseries")
    if(!class.basic %in% special_treatment_vars) {
        x <- as.numeric(x) # get tss, id/time b by coercing summary.pseries to 'numeric'
        share <- x[-1L]/x[1L] # vec with length == 2
        names(share) <- c("id", "time")
        barplot(share, ...)
    } else NULL
}

#' @rdname pseries
#' @export
print.summary.pseries <- function(x, ...){
    x.orig <- x
    digits <- getOption("digits")
    special_treatment_vars <- c("factor", "logical", "character")
    class.basic <- setdiff(attr(x, "class.pseries"), "pseries")
    if(!class.basic %in% special_treatment_vars) {
        x <- as.numeric(x) # get tss, id/time b by coercing summary.pseries to 'numeric'
        share <- x[-1L]/x[1L] # vec with length == 2
        names(share) <- c("id", "time")
        cat(paste("total sum of squares:", signif(x[1L], digits = digits),"\n"))
        print.default(share, ...)
        cat("\n")
        print(attr(x.orig, "SummaryDefault"), ...)
    } else {
        # use base R's facilities
        attr(x, "class.pseries") <- NULL
        # factor is special once again:
        is.fac <- if(class.basic == "factor") TRUE else FALSE
        attr(x, "class") <- if(is.fac) NULL else "summaryDefault"
        print(x, ...)
    }
    invisible(x.orig)
}


Tapply <- function(x, ...) {
    UseMethod("Tapply")
}

myave <- function(x, ...) {
    UseMethod("myave")
}

Tapply.default <- function(x, effect, func, ...) {
    # argument 'effect' is assumed to be a factor
    na.x <- is.na(x)
    uniqval <- tapply(x, effect, func, ...)
    nms <- attr(uniqval, "dimnames")[[1L]]
    attr(uniqval, "dimnames") <- attr(uniqval, "dim") <- NULL
    names(uniqval) <- nms
    result <- uniqval[as.character(effect)]
    result[na.x] <- NA
    return(result)
}

#' @importFrom stats ave
myave.default <- function(x, effect, func, ...) {
    # argument 'effect' is assumed to be a factor
    na.x <- is.na(x)
    res <- ave(x, effect, FUN = function(x) func(x, ...))
    names(res) <- as.character(effect)
    res[na.x] <- NA
    return(res)
}

Tapply.pseries <- function(x, effect = c("individual", "time", "group"), func, ...){
    effect <- match.arg(effect)
    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    effect <- switch(effect,
                     "individual"= xindex[[1L]],
                     "time"      = xindex[[2L]],
                     "group"     = xindex[[3L]]
    )
    z <- as.numeric(x)
    z <- Tapply.default(z, effect, func, ...)
    attr(z, "index") <- attr(x, "index") # insert original index
    class(z) <- c("pseries", class(z))
    return(z)
}

myave.pseries <- function(x, effect = c("individual", "time", "group"), func, ...) {
    effect <- match.arg(effect)
    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    eff.fac <- switch(effect,
                      "individual"= xindex[[1L]],
                      "time"      = xindex[[2L]],
                      "group"     = xindex[[3L]]
    )
    z <- as.numeric(x)
    z <- myave.default(z, eff.fac, func, ...)
    attr(z, "index") <- attr(x, "index") # insert original index
    class(z) <- c("pseries", class(z))
    return(z)
}

Tapply.matrix <- function(x, effect, func, ...) {
    # argument 'effect' is assumed to be a factor
    na.x <- is.na(x)
    uniqval <- apply(x, 2, tapply, effect, func, ...)
    result <- uniqval[as.character(effect), , drop = FALSE]
    result[na.x] <- NA_real_
    return(result)
}

myave.matrix <- function(x, effect, func, ...) {
    # argument 'effect' is assumed to be a factor
    na.x <- is.na(x)
    result <- apply(x, 2, FUN = function(x) ave(x, effect, FUN = function(y) func(y, ...)))
    rownames(result) <- as.character(effect)
    result[na.x] <- NA_real_
    return(result)
}

## non-exported
Mean <- function(x) matrix(.colMeans(x, nrow(x), ncol(x)),
                           nrow(x), ncol(x), byrow = TRUE)

#' @rdname pseries
#' @export
Sum <- function(x, ...) {
    UseMethod("Sum")
}

#' @rdname pseries
#' @export
Sum.default <- function(x, effect, ...) {
    # print("Sum.default(.baseR)")
    # browser()

    # argument 'effect' is assumed to be a factor
    if(!is.numeric(x)) stop("The Sum function only applies to numeric vectors")
    #   Tapply(x, effect, sum, ...)
    return(myave(x, droplevels(effect), sum, ...))
}

#' @rdname pseries
#' @export
Sum.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
    # print("Sum.pseries(.baseR)")
    # browser()

    effect <- match.arg(effect)
    #   Tapply(x, effect, sum, ...)
    # myave.pseries takes care of checking the index for NAs
    return(myave(x, effect, sum, ...))
}

#' @rdname pseries
#' @export
Sum.matrix <- function(x, effect, ...) {
    # print("Sum.matrix(.baseR)")
    # browser()

    # if no index attribute, argument 'effect' is assumed to be a factor
    eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
        droplevels(effect)
    } else {
        if(!is.character(effect) && length(effect) > 1L)
            stop("for matrices with index attributes, the effect argument must be a character")
        if(! effect %in% c("individual", "time", "group"))
            stop("irrelevant effect for a between transformation")
        eff.no <- switch(effect,
                         "individual" = 1L,
                         "time"       = 2L,
                         "group"      = 3L,
                         stop("unknown value of argument 'effect'"))
        xindex <- unclass(xindex) # unclass for speed
        checkNA.index(xindex) # index may not contain any NA
        xindex[[eff.no]]
    }
    return(myave(x, eff.fac, sum, ...))
}

#' @rdname pseries
#' @export
Between <- function(x, ...) {
    UseMethod("Between")
}

#' @rdname pseries
#' @export
Between.default <- function(x, effect, ...) {
    # print("Between.default(.baseR)")
    # browser()

    # argument 'effect' is assumed to be a factor
    if(!is.numeric(x)) stop("The Between function only applies to numeric vectors")
    #   Tapply(x, effect, mean, ...)
    return(myave(x, droplevels(effect), mean, ...))
}

#' @rdname pseries
#' @export
Between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
    # print("Between.pseries(.baseR)")
    # browser()

    effect <- match.arg(effect)
    #   Tapply(x, effect = effect, mean, ...)
    # myave.pseries takes care of checking the index for NAs
    return(myave(x, effect = effect, mean, ...))
}

#' @rdname pseries
#' @export
Between.matrix <- function(x, effect, ...) {
    # print("Between.matrix(.baseR)")
    # browser()

    # if no index attribute, argument 'effect' is assumed to be a factor
    eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
        droplevels(effect)
    } else {
        if(!is.character(effect) && length(effect) > 1L)
            stop("for matrices with index attributes, the effect argument must be a character")
        if(! effect %in% c("individual", "time", "group"))
            stop("irrelevant effect for a between transformation")
        eff.no <- switch(effect,
                         "individual" = 1L,
                         "time"       = 2L,
                         "group"      = 3L,
                         stop("unknown value of argument 'effect'"))
        xindex <- unclass(xindex)
        checkNA.index(xindex) # index may not contain any NA
        xindex[[eff.no]]
    }
    return(myave.matrix(x, eff.fac, mean, ...))
}

#' @rdname pseries
#' @export
between <- function(x, ...) {
    UseMethod("between")
}

#' @rdname pseries
#' @export
between.default <- function(x, effect, ...) {
    # print("between.default(.baseR)")
    # browser()

    # argument 'effect' is assumed to be a factor
    if(!is.numeric(x)) stop("The between function only applies to numeric vectors")

    # use tapply here as tapply's output is sorted by levels factor effect (unlike ave's output)
    # difference is only relevant for between (small "b") as data is compressed down to # levels
    res <- tapply(x, droplevels(effect), mean, ...)
    nms <- attr(res, "dimnames")[[1L]]
    attr(res, "dimnames") <- attr(res, "dim") <- NULL
    names(res) <- nms
    return(res)
}

#' @rdname pseries
#' @export
between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
    # print("between.pseries(.baseR)")
    # browser()

    effect <- match.arg(effect)
    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    eff.fac <- switch(effect,
                      "individual" = xindex[[1L]],
                      "time"       = xindex[[2L]],
                      "group"      = xindex[[3L]],
    )
    res <- between.default(x, effect = eff.fac, ...)
    # data compressed by transformation, so pseries features, esp. index, do not make sense
    res <- remove_pseries_features(res)
    return(res)
}

#' @rdname pseries
#' @export
between.matrix <- function(x, effect, ...) {
    # print("between.matrix(.baseR)")
    # browser()

    # if no index attribute, argument 'effect' is assumed to be a factor
    eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
        droplevels(effect)
    } else {
        if(!is.character(effect) && length(effect) > 1L)
            stop("for matrices with index attributes, the effect argument must be a character")
        if(! effect %in% c("individual", "time", "group"))
            stop("irrelevant effect for a between transformation")
        eff.no <- switch(effect,
                         "individual" = 1L,
                         "time"       = 2L,
                         "group"      = 3L,
                         stop("unknown value of argument 'effect'"))
        xindex <- unclass(xindex) # unclass for speed
        checkNA.index(xindex) # index may not contain any NA
        xindex[[eff.no]]
    }

    # use tapply here as tapply's output is sorted by levels factor effect (unlike ave's output)
    # difference is only relevant for between (small "b") as data is compressed down to # levels
    res <- apply(x, 2, tapply, eff.fac, mean, ...)
    return(res)
}

#' @rdname pseries
#' @export
Within <- function(x, ...) {
    UseMethod("Within")
}

#' @rdname pseries
#' @export
Within.default <- function(x, effect, ...) {
    # print("Within.default(.baseR)")
    # browser()

    # arg 'effect' is assumed to be a factor

    # NB: Contrary to the other Within.* methods, Within.default does not handle
    #     twoways effects
    # TODO: could add support for twoways by supplying a list containing two factors
    if(!is.numeric(x)) stop("the within function only applies to numeric vectors")
    return(x - Between(x, droplevels(effect), ...))
}

#' @rdname pseries
#' @export
Within.pseries <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
    # print("Within.pseries(.baseR)")
    # browser()

    effect <- match.arg(effect)
    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    if(effect != "twoways") result <- x - Between(x, effect, ...)
    else {
        if(is.pbalanced(x)) result <- x - Between(x, "individual", ...) - Between(x, "time") + mean(x, ...)
        else {
            time <- xindex[[2L]]
            Dmu <- model.matrix(~ time - 1)
            attr(Dmu, "index") <- attr(x, "index") # need original index
            W1   <- Within(x,   "individual", ...)
            WDmu <- Within(Dmu, "individual", ...)
            W2 <- lm.fit(WDmu, x)$fitted.values
            result <- W1 - W2
        }
    }
    return(result)
}

#' @rdname pseries
#' @export
Within.matrix <- function(x, effect, ...) {
    # print("Within.matrix(.baseR)")
    # browser()

    if(is.null(xindex <- unclass(attr(x, "index")))) { # unclass for speed
        # non-index case
        result <- Within.default(x, effect, ...)
        # NB: effect is assumed to be a factor; contrary to the other Within.*
        #     methods, Within.default does not handle twoways effects
    }
    else {
        # index case
        if(effect %in% c("individual", "time", "group")) result <- x - Between(x, effect, ...)
        if(effect == "twoways") {
            checkNA.index(xindex) # index may not contain any NA
            if(is.pbalanced(xindex[[1L]], xindex[[2L]])) {
                result <- x - Between(x, "individual", ...) - Between(x, "time", ...) +
                    matrix(colMeans(x, ...), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
            }
            else { # unbalanced twoways
                time <- xindex[[2L]]
                Dmu <- model.matrix(~ time - 1)
                attr(Dmu, "index") <- attr(x, "index") # need orig. index here
                W1   <- Within(x,   "individual", ...)
                WDmu <- Within(Dmu, "individual", ...)
                W2 <- lm.fit(WDmu, x)$fitted.values
                result <- W1 - W2
            }
        }
    }
    return(result)
}

############### LAG and DIFF
#
# lag/lead/diff for pseries are a wrappers for lagt, leadt, difft (if shift = "time") and
#                                          for lagr, leadr, diffr (if shift = "row")
#
# The "t" and "r" methods are not exported (by intention).
#
# The "t" methods perform shifting while taking the time period into
# account (they "look" at the value in the time dimension).
#
# The "r" methods perform shifting row-wise (without taking the value
# in the time dimension into account).
#
# Generic needed only for lead (lag and diff generics are already included in base R)


#' lag, lead, and diff for panel data
#'
#' lag, lead, and diff functions for class pseries.
#'
#' This set of functions perform lagging, leading (lagging in the
#' opposite direction), and differencing operations on `pseries`
#' objects, i. e., they take the panel structure of the data into
#' account by performing the operations per individual.
#'
#' Argument `shift` controls the shifting of observations to be used
#' by methods `lag`, `lead`, and `diff`:
#'
#' - `shift = "time"` (default): Methods respect the
#' numerical value in the time dimension of the index. The time
#' dimension needs to be interpretable as a sequence t, t+1, t+2,
#' \ldots{} where t is an integer (from a technical viewpoint,
#' `as.numeric(as.character(index(your_pdata.frame)[[2]]))` needs to
#' result in a meaningful integer).
#'
#' - `shift = "row": `Methods perform the shifting operation based
#' solely on the "physical position" of the observations,
#' i.e., neighbouring rows are shifted per individual. The value in the
#' time index is not relevant in this case.
#'
#' For consecutive time periods per individual, a switch of shifting
#' behaviour results in no difference. Different return values will
#' occur for non-consecutive time periods per individual
#' ("holes in time"), see also Examples.
#'
#' @name lag.plm
#' @aliases lag lead diff
#' @importFrom stats lag
#' @param x a `pseries` object,
#' @param k an integer, the number of lags for the `lag` and `lead`
#'     methods (can also be negative).  For the `lag` method, a
#'     positive (negative) `k` gives lagged (leading) values.  For the
#'     `lead` method, a positive (negative) `k` gives leading (lagged)
#'     values, thus, `lag(x, k = -1L)` yields the same as `lead(x, k = 1L)`.
#'     If `k` is an integer with length > 1 (`k = c(k1, k2, ...)`), a
#'     `matrix` with multiple lagged `pseries` is returned,
#' @param lag integer, the number of lags for the `diff` method, can also be of
#'     length > 1 (see argument `k`) (only non--negative values in
#'     argument `lag` are allowed for `diff`),
#' @param shift character, either `"time"` (default) or `"row"`
#'     determining how the shifting in the `lag`/`lead`/`diff`
#'     functions is performed (see Details and Examples).
#' @param ... further arguments (currently none evaluated).
#' @return
#'
#' - An object of class `pseries`, if the argument specifying the lag
#'     has length 1 (argument `k` in functions `lag` and `lead`,
#'     argument `lag` in function `diff`).
#'
#' - A matrix containing the various series in its columns, if the
#'     argument specifying the lag has length > 1.
#'
#' @note The sign of `k` in `lag.pseries` results in inverse behaviour
#'     compared to [stats::lag()] and [zoo::lag.zoo()].
#' @author Yves Croissant and Kevin Tappe
#' @seealso To check if the time periods are consecutive per
#'     individual, see [is.pconsecutive()].
#'
#' For further function for 'pseries' objects: [between()],
#' [Between()], [Within()], [summary.pseries()],
#' [print.summary.pseries()], [as.matrix.pseries()].
#' @keywords classes
#' @examples
#'
#' # First, create a pdata.frame
#' data("EmplUK", package = "plm")
#' Em <- pdata.frame(EmplUK)
#'
#' # Then extract a series, which becomes additionally a pseries
#' z <- Em$output
#' class(z)
#'
#' # compute the first and third lag, and the difference lagged twice
#' lag(z)
#' lag(z, 3L)
#' diff(z, 2L)
#'
#' # compute negative lags (= leading values)
#' lag(z, -1L)
#' lead(z, 1L) # same as line above
#' identical(lead(z, 1L), lag(z, -1L)) # TRUE
#'
#' # compute more than one lag and diff at once (matrix returned)
#' lag(z, c(1L,2L))
#' diff(z, c(1L,2L))
#'
#' ## demonstrate behaviour of shift = "time" vs. shift = "row"
#' # delete 2nd time period for first individual (1978 is missing (not NA)):
#' Em_hole <- Em[-2L, ]
#' is.pconsecutive(Em_hole) # check: non-consecutive for 1st individual now
#'
#' # original non-consecutive data:
#' head(Em_hole$emp, 10)
#' # for shift = "time", 1-1979 contains the value of former 1-1977 (2 periods lagged):
#' head(lag(Em_hole$emp, k = 2L, shift = "time"), 10L)
#' # for shift = "row", 1-1979 contains NA (2 rows lagged (and no entry for 1976):
#' head(lag(Em_hole$emp, k = 2L, shift = "row"), 10L)
#'
NULL

#' @rdname lag.plm
#' @export
lead <- function(x, k = 1L, ...) {
    UseMethod("lead")
}

#' @rdname lag.plm
#' @exportS3Method
#' @export lag
lag.pseries <- function(x, k = 1L, shift = c("time", "row"), ...) {
    shift <- match.arg(shift)
    res <- if(shift == "time") lagt.pseries(x = x, k = k, ...) else lagr.pseries(x = x, k = k, ...)
    return(res)
}

#' @rdname lag.plm
#' @export
lead.pseries <- function(x, k = 1L, shift = c("time", "row"), ...) {
    shift <- match.arg(shift)
    res <- if(shift == "time") leadt.pseries(x = x, k = k, ...) else leadr.pseries(x = x, k = k, ...)
    return(res)
}

#' @rdname lag.plm
#' @exportS3Method
diff.pseries <- function(x, lag = 1L, shift = c("time", "row"), ...) {
    shift <- match.arg(shift)
    res <- if(shift == "time") difft.pseries(x = x, lag = lag, ...) else diffr.pseries(x = x, lag = lag, ...)
    return(res)
}

## lagt.pseries lagging taking the time variable into account
lagt.pseries <- function(x, k = 1L, ...) {
    index <- unclass(attr(x, "index")) # unclass for speed
    id <- index[[1L]]
    time <- index[[2L]]

    if(length(k) > 1L) {
        rval <- sapply(k, function(i) alagt(x, i))
        colnames(rval) <- k
    }
    else {
        rval <- alagt(x, k)
    }
    return(rval)
}

## leadt.pseries(x, k) is a wrapper for lagt.pseries(x, -k)
leadt.pseries <- function(x, k = 1L, ...) {
    ret <- lagt.pseries(x, k = -k)
    if(length(k) > 1L) colnames(ret) <- k
    return(ret)
}

## difft: diff-ing taking the time variable into account
difft.pseries <- function(x, lag = 1L, ...){
    ## copied/adapted from diffr.pseries except lines which use lagt() ("t") instead of lagr() ("r")
    islogi <- is.logical(x)
    if(! (is.numeric(x) || islogi)) stop("diff is only relevant for numeric or logical series")

    non.int <- vapply(lag, function(l) round(l) != l, FUN.VALUE = TRUE, USE.NAMES = FALSE)
    if(any(non.int)) stop("Lagging value(s) in 'lag' must be whole-numbered (and non-negative)")

    # prevent input of negative values, because it will most likely confuse users
    # what difft would do in this case
    neg <- vapply(lag, function(l) l < 0L, FUN.VALUE = TRUE, USE.NAMES = FALSE)
    if(any(neg)) stop("diff is only relevant for non-negative values in 'lag'")

    lagtx <- lagt.pseries(x, k = lag) # use "time-based" lagging for difft

    if(is.matrix(lagtx)) {
        # if 'lagtx' is matrix (case length(lag) > 1):
        # perform subtraction without pseries feature of 'x', because otherwise
        # the result would be c("pseries", "matrix") which is not supported
        res <- as.numeric(x) - lagtx
    } else {
        res <- x - lagtx
    }

    return(res)
}

## alagt: non-exported helper function for lagt (actual work horse),
## performs shifting of observations while respecting the time dimension
alagt <- function(x, ak) {
    if(round(ak) != ak) stop("Lagging value 'k' must be whole-numbered (positive, negative or zero)")
    if(ak != 0) {
        index <- unclass(attr(x, "index")) # unclass for speed
        id   <- index[[1L]]
        time <- index[[2L]]

        # Idea: split times in blocks per individuals and do lagging there
        # by computation of correct time shifting

        # need to convert to numeric, do this by coercing to character
        # first (otherwise wrong results!)
        #  see R FAQ 7.10 for coercing factors to numeric:
        #      as.numeric(levels(factor_var))[as.integer(factor_var)] is
        #      more efficient than
        #      as.numeric(as.character(factor_var))

        # YC 2019/08/29 only works if time values can be coerced to
        ## numeric, ie integers like years. When year is period (ie 5 years),
        ## values used to be 1950 for the 1950-54 period, time is now a
        ## factor in the original data.frame with levels "1950-54",
        ## "1955-59", ... In this case coercing the levels to a numeric gives
        ## NA so coerce the *factor* to a numeric.

        levtime <- levels(time)
        numlevtime <- suppressWarnings(as.numeric(levtime))
        if(! anyNA(numlevtime)) time <- as.numeric(levels(time))[as.integer(time)]
        else time <- as.numeric(time)

        list_id_timevar <- split(time, id, drop = TRUE)

        index_lag_ak_all_list <- sapply(list_id_timevar,
                                        function(x) match(x - ak, x, incomparables = NA),
                                        simplify = FALSE, USE.NAMES = FALSE)

        # translate block-wise positions to positions in full vector
        index_lag_ak_all <- unlist(index_lag_ak_all_list, use.names = FALSE)

        NApos <- is.na(index_lag_ak_all) # save NA positions for later
        substitute_blockwise <- index_lag_ak_all

        block_lengths <- lengths(index_lag_ak_all_list, use.names = FALSE)

        # not needed but leave here for illustration:
        #    startpos_block <- cumsum(block_lengths) - block_lengths + 1
        #    endpos_block <- startpos_block + block_lengths - 1

        indexes_blockwise <- unlist(sapply(block_lengths, function(x) seq(from = 1, to = x), simplify = FALSE), use.names = FALSE)

        orig_pos_x <- seq.int(x) # make vector with indexes for original input
        new_pos <- orig_pos_x - (indexes_blockwise - substitute_blockwise) # calc. new positions
        new_pos[NApos] <- orig_pos_x[NApos] # fill NAs with arbitrary values to allow proper subsetting in next step

        orig_attr <- attributes(x)
        x <- x[new_pos] # re-arrange according to lagging
        x[NApos] <- NA  # set NAs where necessary
        attributes(x) <- orig_attr # restore original names and 'pseries' class (lost by subsetting x)
    }
    return(x)
} # END alagt


## lagr: lagging row-wise
lagr.pseries <- function(x, k = 1L, ...) {
    index <- unclass(attr(x, "index")) # unclass for speed
    id <- index[[1L]]
    time <- index[[2L]]

    # catch the case when an index of pdata.frame shall be lagged
    # (index variables are always factors) NB: this catches -
    # unintentionally - also the case when a factor variable is the
    # same "on the character level" as one of the corresponding index
    # variables but not the index variable itself
    #
    # -> shall we prevent lagging of index variables at all? -> turned
    # off for now, 2016-03-03 if(is.factor(x)) if
    # (all(as.character(x) == as.character(id)) |
    # all(as.character(x)==as.character(time))) stop("Lagged vector
    # cannot be index.")

    alagr <- function(x, ak){
        if(round(ak) != ak) stop("Lagging value 'k' must be whole-numbered (positive, negative or zero)")
        if(ak > 0L) {

            # NB: this code does row-wise shifting
            # delete first ak observations for each unit
            isNAtime <- c(rep(TRUE, ak), (diff(as.numeric(time), lag = ak) != ak))
            isNAid   <- c(rep(TRUE, ak), (diff(as.numeric(id),   lag = ak) != 0L))
            isNA <- (isNAtime | isNAid)

            result <- x                                             # copy x first ...
            result[1:ak] <- NA                                      # ... then make first ak obs NA ...
            result[(ak+1):length(result)] <- x[1:(length(x)-ak)]    # ... shift and ...
            result[isNA] <- NA                                      # ... make more NAs in between: this way, we keep: all factor levels, names, classes

        } else if(ak < 0L) { # => compute leading values

            # delete last |ak| observations for each unit
            num_time <- as.numeric(time)
            num_id   <- as.numeric(id)
            isNAtime <- c(c((num_time[1:(length(num_time)+ak)] - num_time[(-ak+1):length(num_time)]) != ak), rep(TRUE, -ak))
            isNAid   <- c(c((num_id[1:(length(num_id)+ak)]     - num_id[(-ak+1):length(num_id)])     != 0L), rep(TRUE, -ak))
            isNA <- (isNAtime | isNAid)

            result <- x                                            # copy x first ...
            result[(length(result)+ak+1):length(result)] <- NA     # ... then make last |ak| obs NA ...
            result[1:(length(result)+ak)] <- x[(1-ak):(length(x))] # ... shift and ...
            result[isNA] <- NA                                     # ... make more NAs in between: this way, we keep: all factor levels, names, classes

        } else { # ak == 0 => nothing to do, return original pseries (no lagging/no leading)
            result <- x
        }

        return(result)
    } # END function alagr

    if(length(k) > 1L) {
        rval <- sapply(k, function(i) alagr(x, i))
        colnames(rval) <- k
    }
    else {
        rval <- alagr(x, k)
    }
    return(rval)
}


# leadr.pseries(x, k) is a wrapper for lagr.pseries(x, -k)
leadr.pseries <- function(x, k = 1L, ...) {
    ret <- lagr.pseries(x, k = -k)
    if(length(k) > 1L) colnames(ret) <- k
    return(ret)
}

## diffr: lagging row-wise
diffr.pseries <- function(x, lag = 1L, ...) {
    islogi <- is.logical(x)
    if(! (is.numeric(x) || islogi)) stop("diff is only relevant for numeric or logical series")

    non.int <- vapply(lag, function(l) round(l) != l, FUN.VALUE = TRUE, USE.NAMES = FALSE)
    if(any(non.int)) stop("Lagging value(s) in 'lag' must be whole-numbered (and non-negative)")

    # prevent input of negative values, because it will most likely confuse users
    # what diff would do in this case
    neg <- vapply(lag, function(l) l < 0L, FUN.VALUE = TRUE, USE.NAMES = FALSE)
    if(any(neg)) stop("diff is only relevant for non-negative values in 'lag'")

    lagrx <- lagr.pseries(x, k = lag)

    if(is.matrix(lagrx)) {
        # if 'lagrx' is matrix (case length(lag) > 1):
        # perform subtraction without pseries feature of 'x', because otherwise
        # the result would be c("pseries", "matrix") which is not supported
        res <- as.numeric(x) - lagrx
    } else {
        res <- x - lagrx
    }
    return(res)
}

## pdiff is (only) used in model.matrix to calculate the
## model.matrix for FD models, works for effect = "individual" only,
## see model.matrix on how to call pdiff. Result is in order (id,
## time) for both effects
##
## Performs row-wise shifting
pdiff <- function(x, effect = c("individual", "time"), has.intercept = FALSE){
    # NB: x is assumed to have an index attribute, e.g., a pseries
    #     can check with has.index(x)
    effect <- match.arg(effect)
    cond <- as.numeric(unclass(attr(x, "index"))[[1L]]) # unclass for speed
    n <- if(is.matrix(x)) nrow(x) else length(x)
    cond <- c(NA, cond[2:n] - cond[1:(n-1)]) # this assumes a certain ordering
    cond[cond != 0] <- NA
    if(! is.matrix(x)){
        result <- c(NA , x[2:n] - x[1:(n-1)])
        result[is.na(cond)] <- NA
        result <- na.omit(result)
    }
    else{
        result <- rbind(NA, x[2:n, , drop = FALSE] - x[1:(n-1), , drop = FALSE])
        result[is.na(cond), ] <- NA
        result <- na.omit(result)
        result <- result[ , apply(result, 2, var) > 1E-12, drop = FALSE]
        if(has.intercept){
            result <- cbind(1, result)
            colnames(result)[1L] <- "(Intercept)"
        }
    }
    attr(result, "na.action") <- NULL
    result
}


# tool_transformations_collapse.R#
## Structural changes made to plm's original data transformation functions
## need to be mimicked in the *.collapse(.*) versions and vice versa.

## 1) Give the base-R version of the functions defined in tool_transformations.R
##    a new name (*.baseR).
## 2) Implement wrapper switched which call the *.baseR or *.collapse versions
##    based on the option plm.fast (a logical, can be set via R's regular option
##    mechanism: options("plm.fast" = TRUE).

## ad 1) new name for base R functions defined in tool_transformations.R
Sum.default.baseR <- plm:::Sum.default
Sum.pseries.baseR <- plm:::Sum.pseries
Sum.matrix.baseR  <- plm:::Sum.matrix

between.default.baseR <- plm:::between.default
between.pseries.baseR <- plm:::between.pseries
between.matrix.baseR  <- plm:::between.matrix

Between.default.baseR <- plm:::Between.default
Between.pseries.baseR <- plm:::Between.pseries
Between.matrix.baseR  <- plm:::Between.matrix

Within.default.baseR <- plm:::Within.default
Within.pseries.baseR <- plm:::Within.pseries
Within.matrix.baseR  <- plm:::Within.matrix

pseriesfy.baseR      <- plm:::pseriesfy # ... in tool_pdata.frame.R:


## ad 2) implement wrapper switches

#### Sum wrapper switches ####
Sum.default <- function(x, effect, ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        Sum.default.baseR(x, effect, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            Sum.default.collapse(x, effect, ...) }
}

Sum.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        Sum.pseries.baseR(x, effect, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            Sum.pseries.collapse(x, effect, ...) }
}

Sum.matrix <- function(x, effect, ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        Sum.matrix.baseR(x, effect, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            Sum.matrix.collapse(x, effect, ...) }
}

#### Between wrapper switches ####
Between.default <- function(x, effect, ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        Between.default.baseR(x, effect, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            Between.default.collapse(x, effect, ...) }
}

Between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        Between.pseries.baseR(x, effect, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            Between.pseries.collapse(x, effect, ...) }
}

Between.matrix <- function(x, effect, ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        Between.matrix.baseR(x, effect, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            Between.matrix.collapse(x, effect, ...) }
}

#### between wrapper switches ####
between.default <- function(x, effect, ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        between.default.baseR(x, effect, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            between.default.collapse(x, effect, ...) }
}

between.pseries <- function(x, effect = c("individual", "time", "group"), ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        between.pseries.baseR(x, effect, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            between.pseries.collapse(x, effect, ...) }
}

between.matrix <- function(x, effect, ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        between.matrix.baseR(x, effect, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            between.matrix.collapse(x, effect, ...) }
}

#### Within wrapper switches ####
Within.default <- function(x, effect, ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        Within.default.baseR(x, effect, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            Within.default.collapse(x, effect, ...) }
}

Within.pseries <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        Within.pseries.baseR(x, effect, ...)
    } else {
        if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)

        if(is.null(getOption("plm.fast.pkg.FE.tw"))) options("plm.fast.pkg.FE.tw" = "collapse")
        switch(getOption("plm.fast.pkg.FE.tw"),
               "collapse" = Within.pseries.collapse(       x, effect, ...), # collapse only,
               "fixest"   = Within.pseries.collapse.fixest(x, effect, ...), # collapse for 1-way FE + fixest for 2-way FE,
               "lfe"      = Within.pseries.collapse.lfe(   x, effect, ...), # collapse for 1-way FE + lfe for 2-way FE,
               stop("unknown value of option 'plm.fast.pkg.FE.tw'"))
    }
}

Within.matrix <- function(x, effect, ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        Within.matrix.baseR(x, effect, ...)
    } else {
        if (!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)

        if(is.null(getOption("plm.fast.pkg.FE.tw"))) options("plm.fast.pkg.FE.tw" = "collapse")
        switch(getOption("plm.fast.pkg.FE.tw"),
               "collapse" = Within.matrix.collapse(       x, effect, ...), # collapse only,
               "fixest"   = Within.matrix.collapse.fixest(x, effect, ...), # collapse for 1-way FE + fixest for 2-way FE,
               "lfe"      = Within.matrix.collapse.lfe(   x, effect, ...), # collapse for 1-way FE + lfe for 2-way FE,
               stop("unknown value of option 'plm.fast.pkg.FE.tw'"))
    }
}


#### Sum ####

Sum.default.collapse <- function(x, effect, ...) {
    # print("Sum.default.collapse")
    # browser()
    # argument 'effect' is assumed to be a factor

    if(!is.numeric(x)) stop("The Sum function only applies to numeric vectors")
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    res <- collapse::fsum(x, g = effect, w = NULL, na.rm = na.rm, TRA = "replace")
    names(res) <- as.character(effect)
    return(res)
}

Sum.pseries.collapse <- function(x, effect = c("individual", "time", "group"), ...) {
    # print("Sum.pseries.collapse")
    # browser()
    effect <- match.arg(effect)
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     "group"      = 3L,
                     stop("unknown value of argument 'effect'"))
    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    eff.fac <- xindex[[eff.no]]
    res <- collapse::fsum(x, g = eff.fac, w = NULL, na.rm = na.rm, TRA = "replace")
    names(res) <- as.character(eff.fac)
    res <- add_pseries_features(res, attr(x, "index"))
    return(res)
}

Sum.matrix.collapse <- function(x, effect, ...) {
    # print("Sum.matrix.collapse")
    # browser()
    # if no index attribute, argument 'effect' is assumed to be a factor
    eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
        effect
    } else {
        if(!is.character(effect) && length(effect) > 1L)
            stop("for matrices with index attributes, the effect argument must be a character")
        if(! effect %in% c("individual", "time", "group"))
            stop("irrelevant effect for a Sum transformation")
        eff.no <- switch(effect,
                         "individual" = 1L,
                         "time"       = 2L,
                         "group"      = 3L,
                         stop("unknown value of argument 'effect'"))
        xindex <- unclass(xindex) # unclass for speed
        checkNA.index(xindex) # index may not contain any NA
        xindex[[eff.no]]
    }
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    res <- collapse::fsum(x, g = eff.fac, w = NULL, na.rm = na.rm, drop = FALSE, TRA = "replace")
    rownames(res) <- as.character(eff.fac)
    attr(res, "index") <- NULL
    return(res)
}

#### B/between ####

# Need separate implementations of Between.pseries and between.pseries due to different NA handling

Between.default.collapse <- function(x, effect, ...) {
    # print("Between.default.collapse")
    # browser()

    # argument 'effect' is assumed to be a factor
    if(!is.numeric(x)) stop("The Between function only applies to numeric vectors")
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    nms <- as.character(effect)
    res <- collapse::fbetween(x, g = effect, w = NULL, na.rm = na.rm)
    names(res) <- nms
    return(res)
}

between.default.collapse <- function(x, effect, ...) {
    # print("between.default.collapse")
    # browser()

    # argument 'effect' is assumed to be a factor
    if(!is.numeric(x)) stop("The Between function only applies to numeric vectors")
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    res <- collapse::fbetween(x, g = effect, w = NULL, na.rm = na.rm, fill = TRUE)
    keep <- !duplicated(effect)
    res <- res[keep]
    names(res) <- as.character(effect[keep])
    # bring into factor level order (not order as appears in orig. data)
    lvl <- levels(collapse::fdroplevels(effect))
    res <- res[lvl]
    return(res)
}

Between.pseries.collapse <- function(x, effect = c("individual", "time", "group"), ...) {
    # print("Between.pseries.collapse")
    # browser()

    # translate arguments
    effect <- match.arg(effect)
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    eff.no <- switch(effect,
                     "individual" = 1L,
                     "time"       = 2L,
                     "group"      = 3L,
                     stop("unknown value of argument 'effect'"))

    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    nms <- as.character(xindex[[eff.no]])
    na.x <- is.na(x)
    # must be fill = TRUE [to catch case when 1 obs of an individual is NA (otherwise result could contain non-intended NA)]
    res <- collapse::fbetween(x, effect = eff.no, w = NULL, na.rm = na.rm, fill = TRUE)
    names(res) <- nms
    res[na.x] <- NA
    return(res)
}

between.pseries.collapse <- function(x, effect = c("individual", "time", "group"), ...) {
    # print("between.pseries.collapse")
    # browser()
    effect <- match.arg(effect)
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    eff.no <-  switch(effect,
                      "individual" = 1L,
                      "time"       = 2L,
                      "group"      = 3L,
                      stop("unknown value of argument 'effect'"))

    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    i <- xindex[[eff.no]]
    # must be fill = TRUE [to catch case when 1 obs of an individual is NA
    # (otherwise result could contain non-intended NA)]
    res <- collapse::fbetween(x, effect = eff.no, w = NULL, na.rm = na.rm, fill = TRUE)
    res <- remove_pseries_features(res)
    keep <- !duplicated(i)
    res <- res[keep]
    names(res) <- as.character(i[keep])
    # bring into factor level order (not order as appears in orig. data)
    lvl <- levels(collapse::fdroplevels(i))
    res <- res[lvl]
    return(res)
}



Between.matrix.collapse <- function(x, effect, ...) {
    # print("Between.matrix.collapse")
    # browser()
    # if no index attribute, argument 'effect' is assumed to be a factor
    eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
        effect
    } else {
        if(!is.character(effect) && length(effect) > 1L)
            stop("for matrices with index attributes, the effect argument must be a character")
        if(! effect %in% c("individual", "time", "group"))
            stop("irrelevant effect for a between transformation")
        eff.no <- switch(effect,
                         "individual" = 1L,
                         "time"       = 2L,
                         "group"      = 3L,
                         stop("unknown value of argument 'effect'"))
        xindex <- unclass(xindex) # unclass for speed
        checkNA.index(xindex) # index may not contain any NA
        xindex[[eff.no]]
    }
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    na.x <- is.na(x)
    res <- collapse::fbetween(x, g = eff.fac, w = NULL, na.rm = na.rm, fill = TRUE)
    attr(res, "index") <- NULL
    rownames(res) <- as.character(eff.fac)
    res[na.x] <- NA
    return(res)
}

between.matrix.collapse <- function(x, effect, ...) {
    # print("between.matrix.collapse")
    # browser()
    # if no index attribute, argument 'effect' is assumed to be a factor
    eff.fac <- if(is.null(xindex <- attr(x, "index"))) {
        effect
    } else {
        if(!is.character(effect) && length(effect) > 1L)
            stop("for matrices with index attributes, the effect argument must be a character")
        if(! effect %in% c("individual", "time", "group"))
            stop("irrelevant effect for a between transformation")
        eff.no <- switch(effect,
                         "individual" = 1L,
                         "time"       = 2L,
                         "group"      = 3L,
                         stop("unknown value of argument 'effect'"))
        xindex <- unclass(xindex) # unclass for speed
        checkNA.index(xindex) # index may not contain any NA
        xindex[[eff.no]]
    }
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    res <- collapse::fbetween(x, g = eff.fac, w = NULL, na.rm = na.rm, fill = TRUE)
    rownames(res) <- as.character(eff.fac)
    # compress data to number of unique individuals (or time periods)
    res <- res[!duplicated(eff.fac), , drop = FALSE]
    # bring into factor level order (not order as appears in orig. data)
    lvl <- levels(collapse::fdroplevels(eff.fac))
    res <- res[lvl, , drop = FALSE]
    return(res)
}


#### Within ####
# Within - default

Within.default.collapse <- function(x, effect, ...) {
    # print("Within.default.collapse")
    # browser()

    # argument 'effect' is assumed to be a factor
    if(!is.numeric(x)) stop("the within function only applies to numeric vectors")
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    res <- collapse::fwithin(x, g = effect, w = NULL, na.rm = na.rm)
    # =(plm)= res <- x - Between(x, effect, ...)
    names(res) <- as.character(effect)
    return(res)
}


Within.pseries.collapse <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
    # print("Within.pseries.collapse")
    # browser()
    effect <- match.arg(effect)
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    if(effect != "twoways") {
        eff.no <- switch(effect,
                         "individual" = 1L,
                         "time"       = 2L,
                         "group"      = 3L,
                         stop("unknown value of argument 'effect'"))
        res <- collapse::fwithin(x, effect = eff.no, w = NULL, na.rm = na.rm, mean = 0)
    } else {
        eff.ind.fac  <- xindex[[1L]]
        eff.time.fac <- xindex[[2L]]
        if(is.pbalanced(eff.ind.fac, eff.time.fac)) {
            # effect = "twoways" - balanced
            res <- collapse::fwithin(  x, effect = 1L, w = NULL, na.rm = na.rm, mean = "overall.mean") -
                collapse::fbetween(x, effect = 2L, w = NULL, na.rm = na.rm, fill = TRUE)
            # =(plm)= res <- x - Between(x, "individual", ...) - Between(x, "time", ...) + mean(x, ...)
        } else {
            # effect = "twoways" - unbalanced
            Dmu <- model.matrix(~ eff.time.fac - 1)
            W1   <- collapse::fwithin(x,   effect = 1L,          w = NULL, na.rm = na.rm, mean = 0) # pseries interface
            WDmu <- collapse::fwithin(Dmu, g      = eff.ind.fac, w = NULL, na.rm = na.rm, mean = 0) # matrix interface
            W2 <- lm.fit(WDmu, x)$fitted.values
            res <- W1 - W2
        }
    }
    return(res)
}

Within.matrix.collapse <- function(x, effect, ...) {
    # print("Within.matrix.collapse")
    # browser()

    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm

    if(is.null(xindex <- attr(x, "index"))) {
        # non-index case, 'effect' needs to be a factor
        result <- collapse::fwithin(x, g = effect, w = NULL, na.rm = na.rm)
    }
    else {
        # index case
        xindex <- unclass(xindex) # unclass for speed
        checkNA.index(xindex) # index may not contain any NA

        if(effect != "twoways") {
            eff.fac <- switch(effect,
                              "individual" = xindex[[1L]],
                              "time"       = xindex[[2L]],
                              "group"      = xindex[[3L]],
                              stop("unknown value of argument 'effect'"))

            result <- collapse::fwithin(x, g = eff.fac, w = NULL, na.rm = na.rm, mean = 0)
            # =(plm)= result <- x - Between(x, effect)

        } else {
            # effect = "twoways"
            eff.ind.fac  <- xindex[[1L]]
            eff.time.fac <- xindex[[2L]]

            if(is.pbalanced(eff.ind.fac, eff.time.fac)) {
                # balanced twoways
                result <- collapse::fwithin(  x, g = eff.ind.fac,  w = NULL, na.rm = na.rm, mean = "overall.mean") -
                    collapse::fbetween(x, g = eff.time.fac, w = NULL, na.rm = na.rm, fill = TRUE)
                # =(plm)= result <- x - Between(x, "individual", ...) - Between(x, "time", ...) +
                #                        matrix(colMeans(x, ...), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
            }
            else { # unbalanced twoways
                # as factor is used twice below, make it a collapse::GRP object -> should give some speed-up
                eff.ind.fac <- collapse::GRP(eff.ind.fac, group.sizes = FALSE, return.groups = FALSE, call = FALSE)
                Dmu <- model.matrix(~ eff.time.fac - 1)
                W1   <- collapse::fwithin(x,   g = eff.ind.fac, w = NULL, na.rm = na.rm, mean = 0)
                WDmu <- collapse::fwithin(Dmu, g = eff.ind.fac, w = NULL, na.rm = na.rm, mean = 0)
                W2 <- lm.fit(WDmu, x)$fitted.values
                result <- W1 - W2
            }
        }
    }
    return(result)
}

#### These functions use collpase::fhdwithin (using internally fixest::demean)
#### or lfe::demeanlist respectively, for
#### the 2-way within transformation which are dramatically faster than
#### the implementation via separate collapse::fwithin calls (due to the special
#### algorithms used to partial out the fixed effects)
Within.pseries.collapse.fixest <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
    # print("Within.pseries.collapse.fixest")
    # browser()
    effect <- match.arg(effect)
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    xindex <- unclass(attr(x, "index")) # unclass for speed
    checkNA.index(xindex) # index may not contain any NA
    if(effect != "twoways") {
        eff.no <- switch(effect,
                         "individual" = 1L,
                         "time"       = 2L,
                         "group"      = 3L,
                         stop("unknown value of argument 'effect'"))
        # in 1-way case fwithin seems faster than fhdwithin, so keep 1-way and 2-way
        # cases separated
        res <- collapse::fwithin(x, effect = eff.no, w = NULL, na.rm = na.rm, mean = 0)
    } else {
        # effect = "twoways"

        # dispatches to pseries method
        res <- collapse::fhdwithin(x, effect = 1:2, w = NULL, na.rm = na.rm)
    }
    return(res)
}

Within.matrix.collapse.fixest <- function(x, effect, ...) {
    # print("Within.matrix.collapse.fixest")
    # browser()

    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm

    if(is.null(xindex <- attr(x, "index"))) {
        # non-index case, 'effect' needs to be a factor
        result <- collapse::fwithin(x, g = effect, w = NULL, na.rm = na.rm)
    }
    else {
        # index case
        xindex <- unclass(xindex) # unclass for speed
        checkNA.index(xindex) # index may not contain any NA

        if(effect != "twoways") {
            eff.fac <- switch(effect,
                              "individual" = xindex[[1L]],
                              "time"       = xindex[[2L]],
                              "group"      = xindex[[3L]],
                              stop("unknown value of argument 'effect'"))

            ## result <- collapse::fhdwithin(x, eff.fac) # needs pkg fixest
            # --> for one-way effect, this seems slower than collapse::fwithin
            result <- collapse::fwithin(x, g = eff.fac, w = NULL, na.rm = na.rm, mean = 0)
            # =(plm)= result <- x - Between(x, effect)
        } else {
            # effect = "twoways"
            # no need to distinguish between balanced/unbalanced
            # as this is fully handled by collapse::fhdwithin()
            # collapse::fhdwithin needs pkg fixest as it uses fixest::demean
            result <- collapse::fhdwithin(x, fl = xindex[1:2], w = NULL, na.rm = na.rm)
        }
    }
    return(result)
}

Within.pseries.collapse.lfe <- function(x, effect = c("individual", "time", "group", "twoways"), ...) {
    # print("Within.pseries.collapse.lfe")
    # browser()

    effect <- match.arg(effect)
    xindex <- unclass(attr(x, "index"))
    checkNA.index(xindex) # index may not contain any NA
    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm
    if(effect != "twoways") {
        eff.no <- switch(effect,
                         "individual" = 1L,
                         "time"       = 2L,
                         "group"      = 3L,
                         stop("unknown value of argument 'effect'"))
        # collapse::fwithin is faster in 1-ways case than lfe::demanlist, so
        # keep cases separated
        res <- collapse::fwithin(x, effect = eff.no, w = NULL, na.rm = na.rm, mean = 0)
    } else {
        # effect = "twoways"
        # no need to distinguish between balanced/unbalanced
        # as this is fully handled by lfe::dmeanlist()
        res <- unlist(lfe::demeanlist(x, fl = xindex[1:2], na.rm = na.rm))
        res <- add_pseries_features(res, attr(x, "index")) # index needs to be a proper pindex here!
    }
    return(res)
}

Within.matrix.collapse.lfe <- function(x, effect,  ...) {
    # print("Within.matrix.collapse.lfe")
    # browser()

    # check for presence of na.rm in dots, if not present set to FALSE
    na.rm <- if(missing(...) || is.null(na.rm <- list(...)$na.rm)) FALSE else na.rm

    if(is.null(xindex <- attr(x, "index"))) {
        # non-index case, 'effect' needs to be a factor
        result <- collapse::fwithin(x, g = effect, w = NULL, na.rm = na.rm)
    }
    else {
        # index case
        xindex <- unclass(xindex)
        checkNA.index(xindex) # index may not contain any NA

        if(effect != "twoways") {
            eff.fac <- switch(effect,
                              "individual" = xindex[[1L]],
                              "time"       = xindex[[2L]],
                              "group"      = xindex[[3L]],
                              stop("unknown value of argument 'effect'"))
            # collapse::fwithin is faster in 1-ways case than lfe::demanlist, so
            # keep cases separated
            result <- collapse::fwithin(x, g = eff.fac, w = NULL, na.rm = na.rm, mean = 0)
            # =(plm)= result <- x - Between(x, effect)
        } else {
            # effect = "twoways"
            # no need to distinguish between balanced/unbalanced
            # as this is fully handled by lfe::dmeanlist()
            #
            # lfe::demeanlist (lfe vers. 2.8-6) return value for matrix input is
            # inconsistent / depends on value of argument na.rm,
            # see https://github.com/sgaure/lfe/issues/50.
            result <- lfe::demeanlist(x, fl = xindex[1:2], na.rm = na.rm)
            if(is.list(result)) result <- result[[1L]]
            attr(result, "index") <- attr(x, "index") # index needs to be a proper pindex here!
        }
    }
    return(result)
}

#### wrapper for pseriesfy ####
# both pseriesfy functions are in file tool_pdata.frame.R
pseriesfy <- function(x,  ...) {
    if(!isTRUE(getOption("plm.fast"))) {
        pseriesfy.baseR(x, ...) } else {
            if(!isTRUE(getOption("plm.fast.pkg.collapse"))) stop(txt.no.collapse, call. = FALSE)
            pseriesfy.collapse(x, ...) }
}

.onAttach <- function(libname, pkgname) {
    options("plm.fast" = TRUE) # since 2.6: needs pkg collapse as hard dependency

    # determine when pkg plm is attached whether pkg collapse, fixest, and lfe are
    # available and set (non-documented) options, which packages are available.
    # These options are used to determine in the wrappers if fast mode can be used
    # and if the speed up by fixest or lfe for the 2-way FE case can be used.
    avail.collapse <- requireNamespace("collapse", quietly = TRUE)
    avail.fixest   <- requireNamespace("fixest",   quietly = TRUE)
    avail.lfe      <- requireNamespace("lfe",      quietly = TRUE)

    if(avail.collapse) {
        options("plm.fast.pkg.collapse" = TRUE)
        options("plm.fast.pkg.FE.tw" = "collapse")
        # fixest wins over lfe
        if(avail.fixest) {
            options("plm.fast.pkg.FE.tw" = "fixest")
        } else {
            if(avail.lfe) {
                options("plm.fast.pkg.FE.tw" = "lfe")
            }
        }
    }
    else options("plm.fast.pkg.collapse" = FALSE)
}


#' Option to Switch On/Off Fast Data Transformations
#'
#' A significant speed up can be gained by using fast (panel) data transformation
#' functions from package `collapse`.
#' An additional significant speed up for the two-way fixed effects case can be
#' achieved if package `fixest` or `lfe` is installed (package `collapse`
#' needs to be installed for the fast mode in any case).
#'
#' @details By default, this speed up is enabled.
#' Option `plm.fast` can be used to enable/disable the speed up. The option is
#' evaluated prior to execution of supported transformations (see below), so
#' `option("plm.fast" = TRUE)` enables the speed up while
#' `option("plm.fast" = FALSE)` disables the speed up.
#'
#' To have it always switched off, put `options("plm.fast" = FALSE)` in your
#' .Rprofile file.
#'
#' See **Examples** for how to use the option and for a benchmarking example.
#'
#' For long, package `plm` used base R implementations and R-based code. The
#' package `collapse` provides fast data transformation functions written
#' in C/C++, among them some especially suitable for panel data.
#' Having package `collapse` installed is a requirement for the speed up, so
#' this package is a hard dependency for package `plm`.
#'
#' Availability of packages `fixest` and `lfe` is checked for once when
#' package plm is attached and the additional speed up for the two-way fixed
#' effect case is enabled automatically (`fixest` wins over `lfe`),
#' given one of the packages is detected and `options("plm.fast" = TRUE)`
#' (default) is set. If so, the packages' fast algorithms to partial out fixed
#' effects are #' used (`fixest::demean` (via `collapse::fhdwithin`),
#' `lfe::demeanlist`). Both packages are 'Suggests' dependencies.
#'
#' Users might experience neglectable numerical differences between enabled and
#' disabled fast mode and base R implementation, depending on the platform and
#' the additional packages installed.
#'
#' Currently, these basic functions benefit from the speed-up, used as building
#' blocks in most model estimation functions, e.g., in `plm` (more functions are
#' under investigation):
#' \itemize{
#'   \item between,
#'   \item Between,
#'   \item Sum,
#'   \item Within,
#'   \item pseriesfy.
#' }
#'
#' @name plm.fast
#' @importFrom collapse fhdwithin fwithin fbetween dapply fdroplevels
#' @keywords sysdata manip
#' @examples
#' \dontrun{
#' ### A benchmark of plm without and with speed-up
#' library("plm")
#' library("collapse")
#' library("microbenchmark")
#' rm(list = ls())
#' data("wlddev", package = "collapse")
#' form <- LIFEEX ~ PCGDP + GINI
#'
#' # produce big data set (taken from collapse's vignette)
#' wlddevsmall <- get_vars(wlddev, c("iso3c","year","OECD","PCGDP","LIFEEX","GINI","ODA"))
#' wlddevsmall$iso3c <- as.character(wlddevsmall$iso3c)
#' data <- replicate(100, wlddevsmall, simplify = FALSE)
#' rm(wlddevsmall)
#' uniquify <- function(x, i) {
#'   x$iso3c <- paste0(x$iso3c, i)
#'   x
#' }
#' data <- unlist2d(Map(uniquify, data, as.list(1:100)), idcols = FALSE)
#' data <- pdata.frame(data, index = c("iso3c", "year"))
#' pdim(data) # Balanced Panel: n = 21600, T = 59, N = 1274400 // but many NAs
#' # data <- na.omit(data)
#' # pdim(data) # Unbalanced Panel: n = 13300, T = 1-31, N = 93900
#'
#' times <- 1 # no. of repetitions for benchmark - this takes quite long!
#'
#' onewayFE <- microbenchmark(
#'  {options("plm.fast" = FALSE); plm(form, data = data, model = "within")},
#'  {options("plm.fast" = TRUE);  plm(form, data = data, model = "within")},
#'   times = times)
#'
#' summary(onewayFE, unit = "relative")
#'
#' ## two-ways FE benchmark requires pkg fixest and lfe
#' ## (End-users shall only set option plm.fast. Option plm.fast.pkg.FE.tw shall
#' ##  _not_ be set by the end-user, it is determined automatically when pkg plm
#' ## is attached; however, it needs to be set explicitly in this example for the
#' ## benchmark.)
#' if(requireNamespace("fixest", quietly = TRUE) &&
#'    requireNamespace("lfe", quietly = TRUE)) {
#'
#' twowayFE <-  microbenchmark(
#'  {options("plm.fast" = FALSE);
#'     plm(form, data = data, model = "within", effect = "twoways")},
#'  {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "collapse");
#'     plm(form, data = data, model = "within", effect = "twoways")},
#'  {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "fixest");
#'     plm(form, data = data, model = "within", effect = "twoways")},
#'  {options("plm.fast" = TRUE, "plm.fast.pkg.FE.tw" = "lfe");
#'     plm(form, data = data, model = "within", effect = "twoways")},
#'   times = times)
#'
#' summary(twowayFE, unit = "relative")
#' }
#'
#' onewayRE <- microbenchmark(
#'  {options("plm.fast" = FALSE); plm(form, data = data, model = "random")},
#'  {options("plm.fast" = TRUE);  plm(form, data = data, model = "random")},
#'   times = times)
#'
#' summary(onewayRE, unit = "relative")
#'
#' twowayRE <-  microbenchmark(
#'  {options("plm.fast" = FALSE); plm(form, data = data, model = "random", effect = "twoways")},
#'  {options("plm.fast" = TRUE);  plm(form, data = data, model = "random", effect = "twoways")},
#'   times = times)
#'
#' summary(twowayRE, unit = "relative")
#' }
NULL


txt.no.collapse <- paste0("options(\"plm.fast\") is set to TRUE but package 'collapse' ",
                          "is not available which is needed for fast data transformation functions. ",
                          "Either set 'options(\"plm.fast\" = FALSE)' or install the ",
                          "missing package, e.g., with 'install.packages(\"collapse\")'. \n",
                          "Having additionally package 'fixest' or 'lfe' installed ",
                          "will speed up the two-way fixed effect case further. \n",
                          "Availability of packages is determined only when ",
                          "plm is attached, so restart R/reload plm when mentioned ",
                          "packages have been installed.")

# tool_vcovG.R#
#' Driscoll and Kraay (1998) Robust Covariance Matrix Estimator
#'
#' Nonparametric robust covariance matrix estimators *a la
#' Driscoll and Kraay* for panel models with cross-sectional
#' *and* serial correlation.
#'
#' `vcovSCC` is a function for estimating a robust covariance matrix
#' of parameters for a panel model according to the
#' \insertCite{DRIS:KRAA:98;textual}{plm} method, which is consistent
#' with cross--sectional and serial correlation in a T-asymptotic
#' setting and irrespective of the N dimension. The use with random
#' effects models is undocumented.
#'
#' Weighting schemes specified by `type` are analogous to those in
#' [sandwich::vcovHC()] in package \CRANpkg{sandwich} and are
#' justified theoretically (although in the context of the standard
#' linear model) by \insertCite{MACK:WHIT:85;textual}{plm} and
#' \insertCite{CRIB:04;textual}{plm} \insertCite{@see @ZEIL:04}{plm}).
#'
#' The main use of `vcovSCC` (and the other variance-covariance estimators
#' provided in the package `vcovHC`, `vcovBK`, `vcovNW`, `vcovDC`) is to pass
#' it to plm's own functions like `summary`, `pwaldtest`, and `phtest` or
#' together with testing functions from the `lmtest` and `car` packages. All of
#' these typically allow passing the `vcov` or `vcov.` parameter either as a
#' matrix or as a function, e.g., for Wald--type testing: argument `vcov.` to
#' `coeftest()`, argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples), see \insertCite{@ZEIL:04, 4.1-2 and examples below}{plm}.
#'
#' @aliases vcovSCC
#' @param x an object of class `"plm"` or `"pcce"`
#' @param type the weighting scheme used, one of `"HC0"`, `"sss"`,
#'     `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param cluster switch for vcovG; set at `"time"` here,
#' @param maxlag either `NULL` or a positive integer specifying the
#'     maximum lag order before truncation
#' @param inner the function to be applied to the residuals inside the
#'     sandwich: `"cluster"` for SCC, `"white"` for Newey-West,
#'     (`"diagavg"` for compatibility reasons)
#' @param wj weighting function to be applied to lagged terms,
#' @param \dots further arguments
#' @return An object of class `"matrix"` containing the estimate of
#'     the covariance matrix of coefficients.
#' @export
#' @author Giovanni Millo, partially ported from Daniel Hoechle's
#'     (2007) Stata code
#' @seealso [sandwich::vcovHC()] from the \CRANpkg{sandwich}
#'     package for weighting schemes (`type` argument).
#' @references
#'
#' \insertRef{CRIB:04}{plm}
#'
#' \insertRef{DRIS:KRAA:98}{plm}
#'
#' \insertRef{HOEC:07}{plm}
#'
#' \insertRef{MACK:WHIT:85}{plm}
#'
#' \insertRef{ZEIL:04}{plm}
#'
#' @keywords regression
#' @examples
#'
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
#' ## as function input to plm's summary method (with and without additional arguments):
#' summary(zz, vcov = vcovSCC)
#' summary(zz, vcov = function(x) vcovSCC(x, method="arellano", type="HC1"))
#' ## standard coefficient significance test
#' library(lmtest)
#' coeftest(zz)
#' ## SCC robust significance test, default
#' coeftest(zz, vcov.=vcovSCC)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovSCC(x, type="HC1", maxlag=4))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovSCC)
#' \dontrun{
#' ## test of hyp.: 2*log(pc)=log(emp)
#' library(car)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovSCC)
#' }
vcovSCC <- function(x, ...){
    UseMethod("vcovSCC")
}



#' Newey and West (1987) Robust Covariance Matrix Estimator
#'
#' Nonparametric robust covariance matrix estimators *a la Newey
#' and West* for panel models with serial correlation.
#'
#' `vcovNW` is a function for estimating a robust covariance matrix of
#' parameters for a panel model according to the
#' \insertCite{NEWE:WEST:87;textual}{plm} method.  The function works
#' as a restriction of the \insertCite{DRIS:KRAA:98;textual}{plm} covariance (see
#' [vcovSCC()]) to no cross--sectional correlation.
#'
#' Weighting schemes specified by `type` are analogous to those in
#' [sandwich::vcovHC()] in package \CRANpkg{sandwich} and are
#' justified theoretically (although in the context of the standard
#' linear model) by \insertCite{MACK:WHIT:85;textual}{plm} and
#' \insertCite{CRIB:04;textual}{plm} \insertCite{@see @ZEIL:04}{plm}.
#'
#' The main use of `vcovNW` (and the other variance-covariance estimators
#' provided in the package `vcovHC`, `vcovBK`, `vcovDC`, `vcovSCC`) is to pass
#' it to plm's own functions like `summary`, `pwaldtest`, and `phtest` or
#' together with testing functions from the `lmtest` and `car` packages. All of
#' these typically allow passing the `vcov` or `vcov.` parameter either as a
#' matrix or as a function, e.g., for Wald--type testing: argument `vcov.` to
#' `coeftest()`, argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples), see \insertCite{@ZEIL:04, 4.1-2 and examples below}{plm}.
#'
#' @aliases vcovNW
#' @param x an object of class `"plm"` or `"pcce"`
#' @param type the weighting scheme used, one of `"HC0"`, `"sss"`,
#'     `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param maxlag either `NULL` or a positive integer specifying the
#'     maximum lag order before truncation
#' @param wj weighting function to be applied to lagged terms,
#' @param \dots further arguments
#' @return An object of class `"matrix"` containing the estimate of
#'     the covariance matrix of coefficients.
#' @export
#' @author Giovanni Millo
#' @seealso [sandwich::vcovHC()] from the \CRANpkg{sandwich} package
#'     for weighting schemes (`type` argument).
#' @references
#'
#' \insertRef{CRIB:04}{plm}
#'
#' \insertRef{DRIS:KRAA:98}{plm}
#'
#' \insertRef{MACK:WHIT:85}{plm}
#'
#' \insertRef{NEWE:WEST:87}{plm}
#'
#' \insertRef{ZEIL:04}{plm}
#'
#' @keywords regression
#' @examples
#'
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
#' ## as function input to plm's summary method (with and without additional arguments):
#' summary(zz, vcov = vcovNW)
#' summary(zz, vcov = function(x) vcovNW(x, method="arellano", type="HC1"))
#' ## standard coefficient significance test
#' library(lmtest)
#' coeftest(zz)
#' ## NW robust significance test, default
#' coeftest(zz, vcov.=vcovNW)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovNW(x, type="HC1", maxlag=4))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovNW)
#' \dontrun{
#' ## test of hyp.: 2*log(pc)=log(emp)
#' library(car)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovNW)
#' }
vcovNW <- function(x, ...){
    UseMethod("vcovNW")
}



#' Double-Clustering Robust Covariance Matrix Estimator
#'
#' High-level convenience wrapper for double-clustering robust
#' covariance matrix estimators *a la*
#' \insertCite{THOM:11;textual}{plm} and
#' \insertCite{CAME:GELB:MILL:11;textual}{plm} for panel models.
#'
#' `vcovDC` is a function for estimating a robust covariance matrix of
#' parameters for a panel model with errors clustering along both dimensions.
#' The function is a convenience wrapper simply summing a group- and a
#' time-clustered covariance matrix and subtracting a diagonal one *a la*
#' White.
#'
#' Weighting schemes specified by `type` are analogous to those in
#' [sandwich::vcovHC()] in package \CRANpkg{sandwich} and are
#' justified theoretically (although in the context of the standard
#' linear model) by \insertCite{MACK:WHIT:85;textual}{plm} and
#' \insertCite{CRIB:04;textual}{plm} \insertCite{@see @ZEIL:04}{plm}.
#'
#' The main use of `vcovDC` (and the other variance-covariance estimators
#' provided in the package `vcovHC`, `vcovBK`, `vcovNW`, `vcovSCC`) is to pass
#' it to plm's own functions like `summary`, `pwaldtest`, and `phtest` or
#' together with testing functions from the `lmtest` and `car` packages. All of
#' these typically allow passing the `vcov` or `vcov.` parameter either as a
#' matrix or as a function, e.g., for Wald--type testing: argument `vcov.` to
#' `coeftest()`, argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples), see \insertCite{@ZEIL:04, 4.1-2 and examples below}{plm}.
#'
#' @aliases vcovDC
#' @param x an object of class `"plm"` or `"pcce"`
#' @param type the weighting scheme used, one of `"HC0"`, `"sss"`,
#'     `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param \dots further arguments
#' @return An object of class `"matrix"` containing the estimate of
#'     the covariance matrix of coefficients.
#' @export
#' @author Giovanni Millo
#' @seealso [sandwich::vcovHC()] from the \CRANpkg{sandwich}
#'     package for weighting schemes (`type` argument).
#' @references
#'
#' \insertRef{CAME:GELB:MILL:11}{plm}
#'
#' \insertRef{CRIB:04}{plm}
#'
#' \insertRef{MACK:WHIT:85}{plm}
#'
#' \insertRef{THOM:11}{plm}
#'
#' \insertRef{ZEIL:04}{plm}
#'
#' @keywords regression
#' @examples
#'
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="pooling")
#' ## as function input to plm's summary method (with and without additional arguments):
#' summary(zz, vcov = vcovDC)
#' summary(zz, vcov = function(x) vcovDC(x, type="HC1", maxlag=4))
#' ## standard coefficient significance test
#' library(lmtest)
#' coeftest(zz)
#' ## DC robust significance test, default
#' coeftest(zz, vcov.=vcovDC)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovDC(x, type="HC1", maxlag=4))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovDC)
#' \dontrun{
#' ## test of hyp.: 2*log(pc)=log(emp)
#' library(car)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovDC)
#' }
vcovDC <- function(x, ...){
    UseMethod("vcovDC")
}



#' Generic Lego building block for Robust Covariance Matrix Estimators
#'
#' Generic Lego building block for robust covariance matrix estimators
#' of the vcovXX kind for panel models.
#'
#' `vcovG` is the generic building block for use by higher--level
#' wrappers [vcovHC()], [vcovSCC()], [vcovDC()], and [vcovNW()]. The
#' main use of `vcovG` is to be used internally by the former, but it
#' is made available in the user space for use in non--standard
#' combinations. For more documentation, see see wrapper functions
#' mentioned.
#'
#' @aliases vcovG
#' @param x an object of class `"plm"` or `"pcce"`
#' @param type the weighting scheme used, one of `"HC0"`,
#'     `"sss"`, `"HC1"`, `"HC2"`, `"HC3"`,
#'     `"HC4"`,
#' @param cluster one of `"group"`, `"time"`,
#' @param l lagging order, defaulting to zero
#' @param inner the function to be applied to the residuals inside the
#'     sandwich: one of `"cluster"` or `"white"` or
#'     `"diagavg"`,
#' @param \dots further arguments
#' @return An object of class `"matrix"` containing the estimate
#'     of the covariance matrix of coefficients.
#' @export
#' @author Giovanni Millo
#' @seealso [vcovHC()], [vcovSCC()],
#'     [vcovDC()], [vcovNW()], and
#'     [vcovBK()] albeit the latter does not make use of
#'     vcovG.
#' @references
#'
#' \insertRef{mil17b}{plm}
#'
#' @keywords regression
#' @examples
#'
#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc,
#' model="pooling")
#' ## reproduce Arellano's covariance matrix
#' vcovG(zz, cluster="group", inner="cluster", l=0)
#' ## define custom covariance function
#' ## (in this example, same as vcovHC)
#' myvcov <- function(x) vcovG(x, cluster="group", inner="cluster", l=0)
#' summary(zz, vcov = myvcov)
#' ## use in coefficient significance test
#' library(lmtest)
#' ## robust significance test
#' coeftest(zz, vcov. = myvcov)
#'
vcovG <- function(x, ...) {
    UseMethod("vcovG")
}


#' @rdname vcovG
#' @export
vcovG.plm <- function(x, type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                      cluster = c("group", "time"),
                      l = 0,
                      inner = c("cluster", "white", "diagavg"),
                      ...) {

    ## general building block for vcov
    ## for panel models (pooling, random, within or fd type plm obj.)
    ##
    ## * (7/11/2016): compliant with IV models

    # stopping control for weighted regressions
    if (!is.null(x$weights)) stop("vcovXX functions not implemented for weighted panel regressions")

    type <- match.arg(type)
    model <- describe(x, "model")
    if (!model %in% c("random", "within", "pooling", "fd")) {
        stop("Model has to be either \"random\", \"within\", \"pooling\", or \"fd\" model")
    }

    ## extract demeaned data
    demy <- pmodel.response(x, model = model)
    demX <- model.matrix(x, model = model, rhs = 1, cstcovar.rm = "all")
    ## drop any linear dependent columns (corresponding to aliased coefficients)
    ## from model matrix X
    ## na.rm = TRUE because currently, RE tw unbalanced models set aliased simply to NA
    if (!is.null(x$aliased) && any(x$aliased, na.rm = TRUE)) demX <- demX[ , !x$aliased, drop = FALSE]

    ## control: IV or not (two- or one-part formula)
    if(length(formula(x))[2L] > 1L) {
        demZ <- model.matrix(x, model = model, rhs = 2, cstcovar.rm = "all")
        ## substitute (transformed) X with projection of X on Z
        ## any linear dependence in Z (demZ) is appropriately taken care of by lm.fit()
        nms <- colnames(demX)
        demX <- lm.fit(demZ, demX)$fitted.values
        # catches case with only one regressor -> need to convert numeric
        # returned from lm.fit()$fitted.values to matrix:
        if(!is.matrix(demX)) demX <- matrix(demX, dimnames = list(NULL, nms[1L]))
    }

    pdim <- pdim(x)
    nT <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    k <- dim(demX)[[2L]]
    n0 <- pdim$nT$n
    t0 <- pdim$nT$T

    ## extract residuals
    uhat <- x$residuals

    ## define residuals weighting function omega(res)
    ## (code taken from meatHC and modified)
    ## (the weighting is defined "in sqrt" relative to the literature)
    ##
    ## (see the theoretical comments in pvcovHC)

    ## this is computationally heavy, do only if needed
    switch(match.arg(type), "HC0" = {diaghat <- NULL},
           "sss" = {diaghat <- NULL},
           "HC1" = {diaghat <- NULL},
           "HC2" = {diaghat <- try(dhat(demX), silent = TRUE)},
           "HC3" = {diaghat <- try(dhat(demX), silent = TRUE)},
           "HC4" = {diaghat <- try(dhat(demX), silent = TRUE)})
    df <- nT - k
    switch(match.arg(type),
           "HC0" = {
               omega <- function(residuals, diaghat, df, g) residuals
           }, "sss" = {
               omega <- function(residuals, diaghat, df, g) residuals *
                   sqrt(g/(g-1)*((nT-1)/(nT-k)))
           }, "HC1" = {
               omega <- function(residuals, diaghat, df, g) residuals *
                   sqrt(length(residuals)/df)
           }, "HC2" = {
               omega <- function(residuals, diaghat, df, g) residuals /
                   sqrt(1 - diaghat)
           }, "HC3" = {
               omega <- function(residuals, diaghat, df, g) residuals /
                   (1 - diaghat)
           }, "HC4" = {
               omega <- function(residuals, diaghat, df, g) {
                   residuals/sqrt(1 - diaghat)^
                       pmin(4, length(residuals) *
                                diaghat/as.integer(round(sum(diaghat),
                                                         digits = 0)))
               }
           })

    ## Definition module for E(u,v)
    if(is.function(inner)) {
        E <- inner
    } else {
        ## outer for clustering/arellano, diag(diag(inner)) for white
        switch(match.arg(inner),
               "cluster" = {
                   E <- function(u, v) outer(u, v)
               },
               "white" = {
                   E <- function(u, v) { # was simply: diag(diag(outer(u,v)))
                       # but unfortunately we have to manage unbalanced panels
                       # in the case l!=0 (the residual vectors are different)
                       # by producing a "pseudo-diagonal" with all those obs.
                       # common to both vectors

                       if(isTRUE(all.equal(names(u), names(v)))) {
                           ## ..then keep it simple! (halves time on EmplUK ex.)
                           n <- length(u)
                           euv <- diag(u*v, n)
                       } else {
                           ## calculate outer product
                           efull <- outer(u, v)
                           ## make matrix of zeros with same dims and names
                           eres <- array(0, dim = dim(efull))
                           dimnames(eres) <- dimnames(efull)
                           ## populate "pseudo-diagonal" with values from efull
                           for(i in 1:length(names(u))) {
                               for(j in 1:length(names(v))) {
                                   if(names(u)[i] == names(v)[j]) {
                                       eres[i, j] <- efull[i, j]
                                   }
                               }
                           }
                           euv <- eres
                       }
                       return(euv)
                   }
               },
               "diagavg" = {
                   E <- function(u,v) {
                       ## this is the averaged version for 'white2'
                       if(isTRUE(all.equal(names(u), names(v)))) {
                           ## ..then keep it simple
                           n <- length(u)
                           euv <- diag(x = sum(u*v)/n, n)
                       } else {
                           ## do just as for 'white' and then average nonzeros:
                           ## calculate outer product
                           efull <- outer(u,v)
                           ## make matrix of zeros with same dims and names
                           eres <- array(0, dim = dim(efull))
                           dimnames(eres) <- dimnames(efull)
                           ## populate "pseudo-diagonal" with values from efull
                           for(i in 1:length(names(u))) {
                               for(j in 1:length(names(v))) {
                                   if(names(u)[i] == names(v)[j]) {
                                       eres[i, j] <- efull[i, j]
                                   }
                               }
                           }
                           euv <- eres
                           ## substitute nonzeros with average thereof
                           euv[euv != 0] <- mean(euv[euv != 0])
                       }
                       return(euv)
                   }
               })
    } ## END: Definition module for E(u,v)


    ## try passing: function (a or b) or matrix (unconditional) to vcovG

    ## robustifying against either serial or xs intragroup dependence:
    ## if 'group' then keep current indexing, if 'time' then swap i<->t
    ## so that residuals get 'clustered' by time period instead of by
    ## group (i.e., the vcov estimator is robust vs. xsectional dependence)

    ## extract indices
    xindex <- unclass(attr(x$model, "index")) # unclass for speed
    groupind <- as.numeric(xindex[[1L]])
    timeind  <- as.numeric(xindex[[2L]])

    ## adjust for 'fd' model (losing first time period)
    if(model == "fd") {
        groupi <- as.numeric(groupind)
        ## make vector =1 on first obs in each group, 0 elsewhere
        selector <- groupi - c(0, groupi[-length(groupi)])
        selector[1L] <- 1 # the first must always be 1
        ## eliminate first obs in time for each group
        groupind <- groupind[!selector]
        timeind <- timeind[!selector]
        nT <- nT - n0
        Ti <- Ti - 1
        t0 <- t0 - 1
    }

    ## set grouping indexes
    switch(match.arg(cluster),
           "group" = {
               n <- n0
               t <- t0
               relevant.ind <- groupind
               lab <- timeind},
           "time" = {
               n <- t0
               t <- n0
               relevant.ind <- timeind
               lab <- groupind})

    tind <- vector("list", n)
    tlab <- vector("list", n)

    for (i in 1:length(unique(relevant.ind))) {
        tind[[i]] <- which(relevant.ind == i)
        tlab[[i]] <- lab[which(relevant.ind == i)]
    }

    ## lab were the 'labels' (a numeric, actually) for the relevant index;
    ## in use again from the need to make pseudo-diagonals for
    ## calc. the lagged White terms on unbalanced panels

    ## transform residuals by weights (here because type='sss' needs to
    ## know who the grouping index 'g' is

    ## set number of clusters for Stata-like small sample correction
    ## (if clustering, i.e., inner="cluster", then G is the cardinality of
    ## the grouping index; if inner="white" it is simply the sample size)
    ## find some more elegant solution for this!
    ## (perhaps if white then sss -> HC1 but check...)
    G <- if(match.arg(inner) == "cluster") n else nT
    uhat <- omega(uhat, diaghat, df, G)

    ## compute basic block: X'_t u_t u'_(t-l) X_(t-l) foreach t,
    ## then calculate Sl_t and sum over t (here i in place of t)

    ## here the benchmark case is time-clustering, but beware
    ## that group-clustering is the default

    ## preallocate k x k x (T-l) array for 'pile' of kxk matrices
    ## holding the X' E(u,ul) X elements
    Sl <- array(dim = c(k, k, n-l))

    ## (l=0 gives the special contemporaneous case where Xi=Xil, ui=uil
    ## for computing W, CX, CT)
    for(i in (1+l):n) {
        X  <- demX[tind[[i]], ,   drop = FALSE]
        Xl <- demX[tind[[i-l]], , drop = FALSE]
        u  <- uhat[tind[[i]]]
        ul <- uhat[tind[[(i-l)]]]
        names(u)  <- tlab[[i]]
        names(ul) <- tlab[[(i-l)]]
        ## calculate V_yy
        Sl[ , , i-l] <- crossprod(X, E(u, ul)) %*% Xl
    }

    ## in order to sum on available observations two things can be done:
    ## a) apply sum(..., na.rm=TRUE) over the third dim
    ## b) apply mean(..., na.rm=TRUE) idem and multiply by n-l
    ## In case a) averaging is then done dividing each covariance point
    ## by (n-l), regardless of whether there are NAs in the "vertical"
    ## vector Sl[p,q, ]
    ## In case b) each mean is calculated correctly on the right number
    ## of observations, excluding missing data. 'salame' has to be
    ## multiplied by (n-l)
    ## But notice, here there should be none left! Each Sl_i is k x k.
    ## Hence use sum().

    ## meat
    ## salame <- apply(Sl, 1:2, mean, na.rm=TRUE) * (n-l)
    salame <- rowSums(Sl, dims = 2L) # == apply(Sl, 1:2, sum) but faster

    ## bread by standard method
    pane <- solve(crossprod(demX))
    ## sandwich
    mycov <-  tcrossprod(crossprod(t(pane), salame), t(pane)) # == pane %*% salame %*% pane

    # save information about cluster variable in matrix (needed for e.g.,
    # robust F test)
    attr(mycov, which = "cluster") <- match.arg(cluster)
    return(mycov)
}

#' Robust Covariance Matrix Estimators
#'
#' Robust covariance matrix estimators *a la White* for panel
#' models.
#'
#' `vcovHC` is a function for estimating a robust covariance matrix of
#' parameters for a fixed effects or random effects panel model
#' according to the White method
#' \insertCite{WHIT:80,WHIT:84b,AREL:87}{plm}. Observations may be
#' clustered by `"group"` (`"time"`) to account for serial
#' (cross-sectional) correlation.
#'
#' All types assume no intragroup (serial) correlation between errors
#' and allow for heteroskedasticity across groups (time periods). As
#' for the error covariance matrix of every single group of
#' observations, `"white1"` allows for general heteroskedasticity but
#' no serial (cross--sectional) correlation; `"white2"` is `"white1"`
#' restricted to a common variance inside every group (time period)
#' \insertCite{@see @GREE:03, Sec. 13.7.1-2, @GREE:12, Sec. 11.6.1-2
#' and @WOOL:02, Sec. 10.7.2}{plm}; `"arellano"` \insertCite{@see
#' ibid. and the original ref. @AREL:87}{plm} allows a fully general
#' structure w.r.t. heteroskedasticity and serial (cross--sectional)
#' correlation.
#'
#' Weighting schemes specified by `type` are analogous to those in
#' [sandwich::vcovHC()] in package \CRANpkg{sandwich} and are
#' justified theoretically (although in the context of the standard
#' linear model) by \insertCite{MACK:WHIT:85;textual}{plm} and
#' \insertCite{CRIB:04;textual}{plm}
#' \insertCite{ZEIL:04}{plm}. `type = "sss"` employs the small sample
#' correction as used by Stata.
#'
# % TODO: give formula for "sss";
# elaborate why different result for FE models (intercept)
#'
#' The main use of `vcovHC` (and the other variance-covariance estimators
#' provided in the package `vcovBK`, `vcovNW`, `vcovDC`, `vcovSCC`) is to pass
#' it to plm's own functions like `summary`, `pwaldtest`, and `phtest` or
#' together with testing functions from the `lmtest` and `car` packages. All of
#' these typically allow passing the `vcov` or `vcov.` parameter either as a
#' matrix or as a function, e.g., for Wald--type testing: argument `vcov.` to
#' `coeftest()`, argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples), see \insertCite{@ZEIL:04, 4.1-2 and examples below}{plm}.
#'
#' A special procedure for `pgmm` objects, proposed by
#' \insertCite{WIND:05;textual}{plm}, is also provided.
#'
#' @name vcovHC.plm
#' @aliases vcovHC
#' @importFrom sandwich vcovHC
#' @export vcovHC
#' @param x an object of class `"plm"` which should be the result of a
#'     random effects or a within model or a model of class `"pgmm"`
#'     or an object of class `"pcce"`,
#' @param method one of `"arellano"`, `"white1"`, `"white2"`,
#' @param type the weighting scheme used, one of `"HC0"`, `"sss"`,
#'     `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param cluster one of `"group"`, `"time"`,
#' @param \dots further arguments.
#' @return An object of class `"matrix"` containing the estimate of
#'     the asymptotic covariance matrix of coefficients.
#' @note The function `pvcovHC` is deprecated. Use `vcovHC` for the
#'     same functionality.
#' @author Giovanni Millo & Yves Croissant
#' @seealso [sandwich::vcovHC()] from the \CRANpkg{sandwich}
#'     package for weighting schemes (`type` argument).
#' @references
#'
#' \insertRef{AREL:87}{plm}
#'
#' \insertRef{CRIB:04}{plm}
#'
#' \insertRef{GREE:03}{plm}
#'
#' \insertRef{GREE:12}{plm}
#'
#' \insertRef{MACK:WHIT:85}{plm}
#'
#' \insertRef{WIND:05}{plm}
#'
#' \insertRef{WHIT:84b}{plm}
#' chap. 6
#'
#' \insertRef{WHIT:80}{plm}
#'
#' \insertRef{WOOL:02}{plm}
#'
#' \insertRef{ZEIL:04}{plm}
#'
#' @keywords regression
#' @examples
#'
#' data("Produc", package = "plm")
#' zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'           data = Produc, model = "random")
#' ## as function input to plm's summary method (with and without additional arguments):
#' summary(zz, vcov = vcovHC)
#' summary(zz, vcov = function(x) vcovHC(x, method="arellano", type="HC1"))
#'
#' ## standard coefficient significance test
#' library(lmtest)
#' coeftest(zz)
#' ## robust significance test, cluster by group
#' ## (robust vs. serial correlation)
#' coeftest(zz, vcov.=vcovHC)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovHC(x, method="arellano", type="HC1"))
#' ## idem, cluster by time period
#' ## (robust vs. cross-sectional correlation)
#' coeftest(zz, vcov.=function(x) vcovHC(x, method="arellano",
#'  type="HC1", cluster="group"))
#' ## idem with parameters, pass vcov as a matrix argument
#' coeftest(zz, vcov.=vcovHC(zz, method="arellano", type="HC1"))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovHC)
#' \dontrun{
#' ## test of hyp.: 2*log(pc)=log(emp)
#' library(car)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovHC)
#' }
#' ## Robust inference for CCE models
#' data("Produc", package = "plm")
#' ccepmod <- pcce(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model="p")
#' summary(ccepmod, vcov = vcovHC)
#'
#' ## Robust inference for GMM models
#' data("EmplUK", package="plm")
#' ar <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
#'            + log(capital) + lag(log(capital), 2) + log(output)
#'            + lag(log(output),2) | lag(log(emp), 2:99),
#'             data = EmplUK, effect = "twoways", model = "twosteps")
#' rv <- vcovHC(ar)
#' mtest(ar, order = 2, vcov = rv)
NULL

#' @rdname vcovHC.plm
#' @export
vcovHC.plm <- function(x, method=c("arellano", "white1", "white2"),
                       type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       cluster=c("group", "time"), ...) {
    ## user-level wrapper for White-Arellano covariances

    ## translate arguments
    inner <- switch(match.arg(method),
                    "arellano" = "cluster",
                    "white1"   = "white",
                    "white2"   = "diagavg")

    return(vcovG(x, type=type, cluster=cluster,
                 l=0, inner=inner, ...))
}

#' @rdname vcovNW
#' @export
vcovNW.plm <- function(x, type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       maxlag=NULL,
                       wj=function(j, maxlag) 1-j/(maxlag+1),
                       ...) {
    ## user-level wrapper for panel Newey-West estimator

    ## set default lag order
    if(is.null(maxlag)) maxlag <- floor((max(pdim(x)$Tint$Ti))^(1/4))

    return(vcovSCC(x, type=type, maxlag=maxlag, inner="white", wj=wj, ...))
}

#' @rdname vcovDC
#' @export
vcovDC.plm <- function(x, type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                       ...) {
    ## user-level wrapper for double-clustering (no persistence)

    Vcx <- vcovG(x, type=type, cluster="group",
                 l=0, inner="cluster", ...)
    Vct <- vcovG(x, type=type, cluster="time",
                 l=0, inner="cluster", ...)
    Vw <- vcovG(x, type=type, l=0, inner="white", ...)

    res <- Vcx + Vct - Vw

    # save information about cluster variable in matrix (needed for e.g.,
    # robust F test)
    attr(res, which = "cluster") <- "group-time"
    return(res)
}

#' @rdname vcovSCC
#' @export
vcovSCC.plm <- function(x, type=c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
                        cluster="time",
                        maxlag=NULL,
                        inner=c("cluster", "white", "diagavg"),
                        wj=function(j, maxlag) 1-j/(maxlag+1),
                        ...) {

    ## replicates vcovSCC

    ## set default lag order
    if(is.null(maxlag)) maxlag <- floor((max(pdim(x)$Tint$Ti))^(1/4))

    ## def. Bartlett kernel
    ## wj <- function(j, maxlag) 1-j/(maxlag+1)
    ## has been passed as argument

    S0 <- vcovG(x, type=type, cluster=cluster, l=0, inner=inner)

    if(maxlag > 0) {
        for(i in 1:maxlag) {
            Vctl <- vcovG(x, type=type, cluster=cluster,
                          l=i, inner=inner)
            S0 <- S0 + wj(i, maxlag) * (Vctl + t(Vctl))
        }
    }

    return(S0)
}


##############################################################

## separate function for BK (PCSE) covariance



#' Beck and Katz Robust Covariance Matrix Estimators
#'
#' Unconditional Robust covariance matrix estimators *a la Beck
#' and Katz* for panel models (a.k.a. Panel Corrected Standard Errors
#' (PCSE)).
#'
#' `vcovBK` is a function for estimating a robust covariance matrix of
#' parameters for a panel model according to the
#' \insertCite{BECK:KATZ:95;textual}{plm} method, a.k.a. Panel
#' Corrected Standard Errors (PCSE), which uses an unconditional
#' estimate of the error covariance across time periods (groups)
#' inside the standard formula for coefficient
#' covariance. Observations may be clustered either by `"group"` to
#' account for timewise heteroskedasticity and serial correlation or
#' by `"time"` to account for cross-sectional heteroskedasticity and
#' correlation. It must be borne in mind that the Beck and Katz
#' formula is based on N- (T-) asymptotics and will not be appropriate
#' elsewhere.
#'
#' The `diagonal` logical argument can be used, if set to
#' `TRUE`, to force to zero all nondiagonal elements in the
#' estimated error covariances; this is appropriate if both serial and
#' cross--sectional correlation are assumed out, and yields a
#' timewise- (groupwise-) heteroskedasticity--consistent estimator.
#'
#' Weighting schemes specified by `type` are analogous to those in
#' [sandwich::vcovHC()] in package \CRANpkg{sandwich} and are
#' justified theoretically (although in the context of the standard
#' linear model) by \insertCite{MACK:WHIT:85;textual}{plm} and
#' \insertCite{CRIB:04;textual}{plm} \insertCite{@see @ZEIL:04}{plm}.
#'
# % TODO: once "sss" has been added: `type = "sss"` employs the small
# % sample correction as used by Stata. give formula for "sss";
# % elaborate why different result for FE models (intercept)
#'
#' The main use of `vcovBK` (and the other variance-covariance estimators
#' provided in the package `vcovHC`, `vcovNW`, `vcovDC`, `vcovSCC`) is to pass
#' it to plm's own functions like `summary`, `pwaldtest`, and `phtest` or
#' together with testing functions from the `lmtest` and `car` packages. All of
#' these typically allow passing the `vcov` or `vcov.` parameter either as a
#' matrix or as a function, e.g., for Wald--type testing: argument `vcov.` to
#' `coeftest()`, argument `vcov` to `waldtest()` and other methods in the
#' \CRANpkg{lmtest} package; and argument `vcov.` to
#' `linearHypothesis()` in the \CRANpkg{car} package (see the
#' examples), see \insertCite{@ZEIL:04, 4.1-2 and examples below}{plm}.
#'
#' @param x an object of class `"plm"`,
#' @param type the weighting scheme used, one of `"HC0"`, `"HC1"`,
#'     `"HC2"`, `"HC3"`, `"HC4"`, see Details,
#' @param cluster one of `"group"`, `"time"`,
#' @param diagonal a logical value specifying whether to force
#'     nondiagonal elements to zero,
#' @param \dots further arguments.
#' @export
#' @return An object of class `"matrix"` containing the estimate of
#'     the covariance matrix of coefficients.
#' @author Giovanni Millo
#' @seealso [sandwich::vcovHC()] from the \CRANpkg{sandwich}
#'     package for weighting schemes (`type` argument).
#' @references
#'
#'
#' \insertRef{BECK:KATZ:95}{plm}
#'
#' \insertRef{CRIB:04}{plm}
#'
#' \insertRef{GREE:03}{plm}
#'
#' \insertRef{MACK:WHIT:85}{plm}
#'
#' \insertRef{ZEIL:04}{plm}
#'
#' @keywords regression
#' @examples
#'

#' data("Produc", package="plm")
#' zz <- plm(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, model="random")
#' summary(zz, vcov = vcovBK)
#' summary(zz, vcov = function(x) vcovBK(x, type="HC1"))
#'
#' ## standard coefficient significance test
#' library(lmtest)
#' coeftest(zz)
#' ## robust significance test, cluster by group
#' ## (robust vs. serial correlation), default arguments
#' coeftest(zz, vcov.=vcovBK)
#' ## idem with parameters, pass vcov as a function argument
#' coeftest(zz, vcov.=function(x) vcovBK(x, type="HC1"))
#' ## idem, cluster by time period
#' ## (robust vs. cross-sectional correlation)
#' coeftest(zz, vcov.=function(x) vcovBK(x, type="HC1", cluster="time"))
#' ## idem with parameters, pass vcov as a matrix argument
#' coeftest(zz, vcov.=vcovBK(zz, type="HC1"))
#' ## joint restriction test
#' waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovBK)
#' \dontrun{
#' ## test of hyp.: 2*log(pc)=log(emp)
#' library(car)
#' linearHypothesis(zz, "2*log(pc)=log(emp)", vcov.=vcovBK)
#' }
vcovBK <- function(x, ...) {
    UseMethod("vcovBK")
}


# TODO: add type "sss" for vcovBK

#' @rdname vcovBK
#' @export
vcovBK.plm <- function(x, type = c("HC0", "HC1", "HC2", "HC3", "HC4"),
                       cluster = c("group", "time"),
                       diagonal = FALSE, ...) {

    ## Robust vcov a la Beck and Katz (1995; AKA 'pcse')
    ## for panel models (pooling, random, within or fd type plm obj.)
    ##
    ## This version: October 20th, 2009; allows choosing the clustering dimension
    ## so as to have serial- or x-sectional-correlation robustness;
    ##
    ## This function takes the demeaned data from the
    ## plm object, then estimates an *unconditional* error covariance by
    ## averaging the empirical covariance blocks by group (time period);
    ## this average block (say, OmegaM in EViews notation) is then put into
    ## White's formula instead of each Omega_i.
    ##
    ## The clustering defaults to "group" for consistency with pvcovHC;
    ## nevertheless the most likely usage is cluster="time" for robustness vs.
    ## cross-sectional dependence, as in the original Beck and Katz paper (where
    ## it is applied to "pooling" models).
    ##
    ## This version: compliant with plm 1.2-0; lmtest.
    ## Code is identical to pvcovHC until mark.
    ##
    ## Usage:
    ## myplm <- plm(<model>,<data>, ...)
    ## # default (cluster by group = robust vs. serial correlation):
    ## coeftest(myplm, vcov=vcovBK)
    ## # cluster by time period (robust vs. XS correlation):
    ## coeftest(myplm, vcov=function(x) vcovBK(x, cluster="time"))
    ## # idem, HC3 weighting:
    ## coeftest(myplm, vcov=function(x) vcovBK(x,cluster="time",type="HC3"))
    ## waldtest(myplm,update(myplm,<new formula>),vcov=vcovBK)
    ##
    ## This weighted version implements a system of weights as
    ## in vcovHC/meatHC. Sure this makes sense for white1, but it
    ## is open to question for white2 and arellano. We'll see.
    ##
    ## Results OK vs. EViews, vcov=PCSE. Unbal. case not exactly the
    ## same (but then, who knows what EViews does!)

    # stopping control for weighted regressions
    if (!is.null(x$weights)) stop("vcovXX functions not implemented for weighted panel regressions")

    type <- match.arg(type)
    model <- describe(x, "model")
    if (!model %in% c("random", "within", "pooling", "fd")) {
        stop("Model has to be either \"random\", \"within\", \"pooling\", or \"fd\" model")
    }

    ## extract demeaned data
    demy <- pmodel.response(x, model = model)
    demX <- model.matrix(x, model = model, rhs = 1, cstcovar.rm = "all")
    ## drop any linear dependent columns (corresponding to aliased coefficients)
    ## from model matrix X
    ##  na.rm = TRUE because currently, RE tw unbalanced models set aliased simply to NA
    if (!is.null(x$aliased) && any(x$aliased, na.rm = TRUE)) demX <- demX[ , !x$aliased, drop = FALSE]

    ## control: IV or not (two- or one-part formula)
    if(length(formula(x))[2L] > 1L) {
        demZ <- model.matrix(x, model = model, rhs = 2, cstcovar.rm = "all")
        ## substitute (transformed) X with projection of X on Z
        ## any linear dependence in Z (demZ) is appropriately taken care of by lm.fit()
        nms <- colnames(demX)
        demX <- lm.fit(demZ, demX)$fitted.values
        # catches case with only one regressor -> need to convert numeric
        # returned from lm.fit()fitted.values to matrix:
        if(!is.matrix(demX)) demX <- matrix(demX, dimnames = list(NULL, nms[1L]))
    }

    pdim <- pdim(x)
    nT <- pdim$nT$N
    Ti <- pdim$Tint$Ti
    k <- dim(demX)[[2L]]
    n0 <- pdim$nT$n
    t0 <- pdim$nT$T

    ## extract residuals
    uhat <- x$residuals

    ## robustifying against either serial or xs intragroup dependence:
    ## if 'group' then keep current indexing, if 'time' then swap i<->t
    ## so that residuals get 'clustered' by time period instead of by
    ## group (i.e., the vcov estimator is robust vs. xsectional dependence)

    ## extract indices
    xindex <- unclass(attr(x$model, "index")) # unclass for speed
    groupind <- as.numeric(xindex[[1L]])
    timeind  <- as.numeric(xindex[[2L]])

    ## Achim's fix for 'fd' model (losing first time period)
    if(model == "fd") {
        groupind <- groupind[timeind > 1]
        timeind <- timeind[timeind > 1]
        nT <- nT - n0
        Ti <- Ti - 1
        t0 <- t0 - 1
    }

    ## set grouping indexes
    switch(match.arg(cluster),
           "group" = {
               n <- n0 # this is needed only for 'pcse'
               t <- t0 # this is needed only for 'pcse'
               relevant.ind <- groupind
               lab <- timeind },
           "time" = {
               n <- t0 # this is needed only for 'pcse'
               t <- n0 # this is needed only for 'pcse'
               relevant.ind <- timeind
               lab <- groupind
           })

    tind <- vector("list", n)
    tlab <- vector("list", n)

    for (i in 1:length(unique(relevant.ind))) {
        tind[[i]] <- which(relevant.ind == i)
        tlab[[i]] <- lab[which(relevant.ind == i)]
    }

    ## define residuals weighting function omega(res)
    ## (code taken from meatHC and modified)
    ## (the weighting is defined "in sqrt" relative to the literature)
    ##
    ## (see the theoretical comments in pvcovHC)

    ## this is computationally heavy, do only if needed
    switch(match.arg(type), "HC0" = {diaghat <- NULL},
           "HC1" = {diaghat <- NULL},
           "HC2" = {diaghat <- try(dhat(demX), silent = TRUE)},
           "HC3" = {diaghat <- try(dhat(demX), silent = TRUE)},
           "HC4" = {diaghat <- try(dhat(demX), silent = TRUE)})
    df <- nT - k
    switch(match.arg(type),
           "HC0" = {
               omega <- function(residuals, diaghat, df) residuals
           }, "HC1" = {
               omega <- function(residuals, diaghat, df) residuals *
                   sqrt(length(residuals)/df)
           }, "HC2" = {
               omega <- function(residuals, diaghat, df) residuals /
                   sqrt(1 - diaghat)
           }, "HC3" = {
               omega <- function(residuals, diaghat, df) residuals /
                   (1 - diaghat)
           }, "HC4" = {
               omega <- function(residuals, diaghat, df) residuals/sqrt(1 -
                                                                            diaghat)^pmin(4, length(residuals) * diaghat/as.integer(round(sum(diaghat),
                                                                                                                                          digits = 0)))
           })

    ## transform residuals by weights
    uhat <- omega(uhat, diaghat, df)

    ## CODE TAKEN FROM pvcovHC() UNTIL HERE except for ind/time labeling ##

    ## the PCSE covariance estimator is based on the unconditional estimate
    ## of the intragroup (intraperiod) covariance of errors, OmegaT or OmegaM
    ## in the EViews help.
    ## we calculate this based on code from pggls().
    ## the Omegai function is then:
    ## - constant if the panel is balanced
    ## - depending only on the intragroup (intraperiod) position index
    ##   if the panel is unbalanced.

    ## (code for estimating OmegaM/OmegaT partly taken from pggls)

    ## est. omega submatrix
    ## "pre-allocate" an empty array
    tres <- array(dim = c(t, t, n))

    ## array of n "empirical omega-blocks"
    ## with outer product of t(i) residuals
    ## for each group 1..n
    ## (use subscripting from condition 'label in labels' set',
    ## the rest stays NA if any)
    for(i in 1:n) {
        ut <- uhat[tind[[i]]]
        tpos <- (1:t)[unique(lab) %in% tlab[[i]]]
        ## put nondiag elements to 0 if diagonal=TRUE
        tres[tpos, tpos, i] <- if(diagonal) diag(diag(ut %o% ut)) else ut %o% ut
    }

    ## average over all omega blocks, removing NAs (apply preserving
    ## *two* dimensions, i.e., over the third) to get the unconditional
    ## covariance matrix of errors for a group (viz. time period):
    OmegaT <- rowMeans(tres, dims = 2L, na.rm = TRUE) # == apply(tres, 1:2, mean, na.rm = TRUE) but faster
    ## end of PCSE covariance calculation.

    ## fetch (all, unique) values of the relevant labels
    unlabs <- unique(lab)

    salame <- array(dim = c(k, k, n))
    for(i in 1:n) {
        groupinds <- tind[[i]]
        grouplabs <- tlab[[i]]
        xi <- demX[groupinds, , drop = FALSE]
        ## for every group, take relevant positions
        tpos <- unlabs %in% grouplabs
        OmegaTi <- OmegaT[tpos, tpos, drop = FALSE]
        salame[ , , i] <- crossprod(xi, OmegaTi) %*% xi
    }
    ## meat
    salame <- rowSums(salame, dims = 2L) # == apply(salame, 1:2, sum) but faster

    ## bread
    pane <- solve(crossprod(demX))

    ## sandwich
    mycov <- tcrossprod(crossprod(t(pane), salame), t(pane)) # == pane %*% salame %*% pane

    # save information about cluster variable in matrix (needed for e.g.,
    # robust F test)
    attr(mycov, which = "cluster") <- match.arg(cluster)
    return(mycov)
}

#######################################################

#####################################
## vcovXX methods for pcce objects ##
#####################################

## pcce is compliant with plm so vcovXX.pcce <- vcovXX.plm
## for any vcov that makes sense computed on the transformed
## data from model.matrix.pcce and pmodel.response.pcce

## TODO: vcovBK.pcce missing? Or not valid?

#' @rdname vcovG
#' @export
vcovG.pcce   <- vcovG.plm

#' @rdname vcovHC.plm
#' @export
vcovHC.pcce  <- vcovHC.plm

#' @rdname vcovNW
#' @export
vcovNW.pcce  <- vcovNW.plm

#' @rdname vcovSCC
#' @export
vcovSCC.pcce <- vcovSCC.plm


####################################
## vcovHC method for pgmm objects ##
####################################

#' @rdname vcovHC.plm
#' @importFrom MASS ginv
#' @export
vcovHC.pgmm <- function(x, ...) {
    model <- describe(x, "model")
    transformation <- describe(x, "transformation")
    A1 <- x$A1
    A2 <- x$A2

    if(transformation == "ld") {
        ##     yX <- lapply(x$model,function(x) rbind(diff(x),x))
        ##     residuals <-lapply(x$residuals,function(x) c(diff(x),x))
        yX <- x$model
        residuals <- x$residuals
    }
    else {
        yX <- x$model
        residuals <- x$residuals
    }
    minevA2 <- min(abs(Re(eigen(A2)$values)))
    eps <- 1E-9

    SA2 <- if(minevA2 < eps){
        warning("a general inverse is used")
        ginv(A2)
    } else solve(A2)

    if(model == "twosteps") {
        coef1s <- x$coefficients[[1L]]
        res1s <- lapply(yX, function(x) x[ , 1L] - crossprod(t(x[ , -1L, drop = FALSE]), coef1s))
        K <- ncol(yX[[1L]])
        D <- c()
        WX <- Reduce("+",
                     mapply(function(x, y) crossprod(x, y[ , -1L, drop = FALSE]), x$W, yX, SIMPLIFY = FALSE))
        We <- Reduce("+", mapply(function(x, y) crossprod(x, y), x$W, residuals, SIMPLIFY = FALSE))
        B1 <- solve(t(WX) %*% A1 %*% WX)
        B2 <- vcov(x)

        vcov1s <- B1 %*% (t(WX) %*% A1 %*% SA2 %*% A1 %*% WX) %*% B1
        for (k in 2:K) {
            exk <- mapply(
                function(x, y){
                    z <- crossprod(t(x[ , k, drop = FALSE]), t(y))
                    - z - t(z)
                },
                yX, res1s, SIMPLIFY = FALSE)
            wexkw <- Reduce("+",
                            mapply(
                                function(x, y)
                                    crossprod(x, crossprod(y, x)),
                                x$W, exk, SIMPLIFY = FALSE))
            Dk <- -B2 %*% t(WX) %*% A2 %*% wexkw %*% A2 %*% We
            D <- cbind(D, Dk)
        }
        vcovr <- B2 + crossprod(t(D), B2) + t(crossprod(t(D), B2)) + D %*% vcov1s %*% t(D)
    }
    else {
        # model = "onestep"
        res1s <- lapply(yX, function(z) z[ , 1L] - crossprod(t(z[ , -1L, drop = FALSE]), x$coefficients))
        K <- ncol(yX[[1L]])
        WX <- Reduce("+", mapply(function(z, y) crossprod(z[ , -1L, drop = FALSE], y), yX, x$W, SIMPLIFY = FALSE))
        B1 <- vcov(x)
        vcovr <- B1 %*% (WX %*% A1 %*% SA2 %*% A1 %*% t(WX)) %*% B1
    }
    vcovr
}


## dhat: diaghat function for matrices
# old: dhat <- function(x) {tx <- t(x); diag(crossprod(tx, solve(crossprod(x), tx)))}
dhat <- function(x) {
    rowSums(crossprod(t(x), solve(crossprod(x))) * x) # == diag(crossprod(tx, solve(crossprod(x), tx)))
}
