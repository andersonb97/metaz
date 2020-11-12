#' Logistic Regression Function
#' @param event.e is a vector containing the number of events in the treatment group
#' @param event.c is a vector containing the number of events in the control group
#' @param n.e is a vector containing the number of observations in the treatment group
#' @param n.c is a vector containing the number of observations in the control group
#' @param covar is a vector containing covariate values
#' @import lme4
#' @return logistic meta-regression output
#' @export
#' @examples
#'

logistic <- function(event.e, event.c, n.e, n.c, covar){

  data.df <- data.frame(event = as.numeric(t(cbind(event.e, event.c))),
                        n = as.numeric(t(cbind(n.e, n.c))),
                        study = rep(c(1:length(n.e)), each = 2),
                        treat = rep(c(1, 0), length(n.e)),
                        covar.rep = rep(covar - mean(covar), each = 2))

  data.df$covar.int <- data.df$treat * data.df$covar.rep

  reg.log <- glmer(cbind(event, n - event) ~ factor(study) + factor(treat) + covar.int + (treat-1|study),
                   data = data.df, family = binomial)

  log.results <<- unname(summary(reg.log)$coefficients[length(summary(reg.log)$coefficients[, 1]),])
}
