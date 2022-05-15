# hdfe <- function(fml,
#                  data,
#                  vcov,
#                  weights,
#                  offset,
#                  subset,
#                  split,
#                  fsplit,
#                  cluster,
#                  se,
#                  ssc,
#                  panel.id,
#                  fixef,
#                  fixef.rm = "none",
#                  fixef.tol = 1e-06,
#                  fixef.iter = 10000,
#                  collin.tol = 1e-10,
#                  nthreads = getFixest_nthreads(),
#                  lean = FALSE,
#                  verbose = 0,
#                  warn = TRUE,
#                  notes = getFixest_notes(),
#                  only.coef = FALSE,
#                  combine.quick,
#                  demeaned = FALSE,
#                  mem.clean = FALSE,
#                  only.env = FALSE,
#                  env,
#                  ...){
#     mod <- suppressWarnings(feols(fml,
#                                   data,
#                                   vcov,
#                                   weights,
#                                   offset,
#                                   subset,
#                                   split,
#                                   fsplit,
#                                   cluster,
#                                   se,
#                                   ssc,
#                                   panel.id,
#                                   fixef,
#                                   fixef.rm = "none",
#                                   fixef.tol = 1e-06,
#                                   fixef.iter = 10000,
#                                   collin.tol = 1e-10,
#                                   nthreads = getFixest_nthreads(),
#                                   lean = FALSE,
#                                   verbose = 0,
#                                   warn = TRUE,
#                                   notes = getFixest_notes(),
#                                   only.coef = FALSE,
#                                   combine.quick,
#                                   demeaned = FALSE,
#                                   mem.clean = FALSE,
#                                   only.env = FALSE,
#                                   env,
#                                   ...))
#     output <- list()
#     output$coefficients <- mod$coefficients
#     output$vcov <- mod$cov.scaled
#     output$vcov.iid <- mod$cov.iid
#     output$residuals <- mod$residuals
#     output$formula <- mod$fml_all
#     output$call <- mod$call
#     class(mod) <- c("fixest", "plm", "panelmodel")
#     return(mod)
# }
#
# stargazer.fixest <- function(object, ...){
#     felm_formula <- as.formula(paste0(
#         paste(object$fml[2], object$fml[1], object$fml[3], collapse = " "),
#         paste(gsub( "~", "|",object$fml_all$fixef), collapse = "")))
#     mod <- lfe::felm(felm_formula, data = )
#     stargazer(coef = list(object$coefficients),
#               se = list(object$se),
#               ...)
# }
