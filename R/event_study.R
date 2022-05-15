ggevent <- function(object, ...){
    coeff <- coef(object)
    std <- object$se
    coeff <- coeff[grepl("^time_to_treat", names(coeff))]
    std <- std[grepl("^time_to_treat", names(std))]
    x <- gsub("^time_to_treat::", "", names(coeff)) %>%
        gsub(":treat", "",.) %>% as.numeric()
    data_plot <- data.frame(x, y = coeff, se = std) %>%
        rbind(data.frame(x = -1, y = 0, se = NA)) %>%
        distinct(x, .keep_all= TRUE) %>%
        mutate(ci_lower = y - qnorm(0.975)*se,
               ci_upper = y + qnorm(0.975)*se)
    ggplot(data_plot, aes(x, y, ymax = ci_upper, ymin = ci_lower), ...) +
        geom_point() +
        geom_errorbar() +
        geom_hline(yintercept = 0, color = "red") +
        geom_vline(xintercept = -1, linetype = "dotted")
}


