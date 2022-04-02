reg <- function(y, x, intercept = TRUE, method = "ols", ...){
    if (is.null(n <- nrow(x))) stop("'x' must be a matrix")
    if (n == 0L) stop("0 (non-NA) cases")
    p <- ncol(x)
    if (p == 0L) {
        return(list(coefficients = numeric(), residuals = y,
                    fitted.values = 0 * y, rank = 0, df.residual = length(y)))
    }
    ny <- NCOL(y)
    if (is.matrix(y) && ny == 1) y <- drop(y)
    if (NROW(y) != n) stop("incompatible dimensions")

    x <- if(intercept) cbind(intercept = rep(1, nrow(x)), x) else x
    x <- as.matrix(x)
    y <- as.matrix(y)
    z <- lm.fit(x, y)
    # coef <- solve(t(x) %*% x) %*% (t(x) %*% y)
    output <- list(coefficients = z$coefficients,
                   residuals = z$residuals,
                   rank = z$rank,
                   fitted.values = z$fitted.values,
                   df.residual = z$df.residual)
    class(output) <- "lm"
    output
}
