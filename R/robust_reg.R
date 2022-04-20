#' Ordinary Least Squares with Robust Standard Errors
#'
#' @param formula
#' @param data
#' @param subset
#' @param weights
#' @param na.action
#' @param clusters
#' @param se_type
#' @param model
#' @param x
#' @param y
#' @param singular.ok
#' @param contrasts
#' @param offset
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
lm_robust <- function (formula, data, subset, weights, na.action,
                       se_type = "HC1", clusters = NULL, model = TRUE, x = FALSE, y = FALSE, singular.ok = TRUE,
                       contrasts = NULL, offset, ...)
{
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
                 "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w))
        stop("'weights' must be a numeric vector")
    offset <- model.offset(mf)
    mlm <- is.matrix(y)
    ny <- if (mlm)
        nrow(y)
    else length(y)
    if (!is.null(offset)) {
        if (!mlm)
            offset <- as.vector(offset)
        if (NROW(offset) != ny)
            stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
                          NROW(offset), ny), domain = NA)
    }
    if (is.empty.model(mt)) {
        x <- NULL
        return <- list(coefficients = if (mlm) matrix(NA_real_, 0,
                                                 ncol(y)) else numeric(), residuals = y, fitted.values = 0 *
                      y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w !=
                                                                                        0) else ny)
        if (!is.null(offset)) {
            return$fitted.values <- offset
            return$residuals <- y - offset
        }
    }
    else {
        x <- model.matrix(mt, mf, contrasts)
        return <- if (is.null(w))
            lm.fit(x, y, offset = offset, singular.ok = singular.ok,
                   ...)
        else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,
                     ...)
    }
    class(return) <- c(if (mlm) "mlm", "lm", "lm_robust")
    return$na.action <- attr(mf, "na.action")
    return$offset <- offset
    return$contrasts <- attr(x, "contrasts")
    return$xlevels <- .getXlevels(mt, mf)
    return$call <- cl
    return$terms <- mt
    if (model)
        return$model <- mf
    if (ret.x)
        return$x <- x
    if (ret.y)
        return$y <- y
    return$rob_se <- sqrt(diag(sandwich::vcovHC(return, se_type, if (!is.null(clusters)) clusters)))
    return$se_type <- se_type
    return
}


#' Summary lm_robust object
#'
#' @param object
#' @param vcov.
#'
#' @return
#' @export
#'
#' @examples
summary_lm_robust <- function(object, vcov. = vcovHC, ...){
    lmtest::coeftest(object, vcov. = vcovHC, type = object$se_type, ...)
}

