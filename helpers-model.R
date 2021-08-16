# Helper functions to fit the models & find sigma, ICC.


chooseModel <- function(data) {
  # Returns model type needed ("fixed effects" or "random intercept")
  tst_glmer1 <- tryCatch(
    lme4::glmer(
      cbind(npos, ntest - npos) ~
        offset(log(sample_size)) + offset(log(inoculum)) + (1 | lab_id),
      data = data, family = binomial(link = "cloglog"),
      control = lme4::glmerControl(optimizer = "bobyqa"), nAGQ = 21
    ),
    error = function(e) e,
    warning = function(w) w
  )
  error_glmer1   <- methods::is(tst_glmer1, "error")
  warning_glmer1 <- methods::is(tst_glmer1, "warning")
  problem_glmer1 <- error_glmer1 || warning_glmer1
  if (problem_glmer1) {
    return("fixed effects")
  } else {
    tst_glmer2 <- tryCatch(
      summary(tst_glmer1),
      error = function(e) e,
      warning = function(w) w
    )
    error_glmer2   <- methods::is(tst_glmer2, "error")
    warning_glmer2 <- methods::is(tst_glmer2, "warning")
    problem_glmer2 <- error_glmer2 || warning_glmer2
    if (problem_glmer2) {
      return("fixed effects")
    } else {
      return("random intercept")
    }
  }
}


fitRandomIntercept <- function(data) {
  # Returns fit, parameter estimates, etc. for random intercept model.
  # Gauss-Hermite quadrature (GHQ)
  fit_random <- lme4::glmer(
    cbind(npos, ntest - npos) ~
      offset(log(sample_size)) + offset(log(inoculum)) + (1 | lab_id),
    data = data, family = binomial(link = "cloglog"),
    control = lme4::glmerControl(optimizer = "bobyqa"), nAGQ = 20
  )
  # Estimate laboratory effects
  summary_fit <- summary(fit_random)
  coef_fit    <- coef(summary_fit)
  mu_log      <- coef_fit["(Intercept)", "Estimate"]    #log mean effect
  mu_log_se   <- coef_fit["(Intercept)", "Std. Error"]  #SE of log mean effect
  mu          <- exp(mu_log)                            #mean effect
  varcor <- as.numeric(summary_fit$varcor)
  if (varcor < 0) {
    sigma <- NA
  } else {
    sigma <- sqrt(varcor)  #SD of log lab effects
  }
  # Calculate ICC
  vc <- as.data.frame(lme4::VarCorr(fit_random))
  var_between <- vc[1, "vcov"]       #between-lab variance
  var_within  <- (base::pi ^ 2) / 6  #within-lab variance
  var_total   <- var_between + var_within
  ICC <- var_between / var_total
  list(
    fit = fit_random, mu = mu, mu_log = mu_log, mu_log_se = mu_log_se,
    sigma = sigma, ICC = ICC
  )
}


fitFixedEffects <- function(data) {
  # Returns fit, parameter estimates, etc. for fixed effects model.
  fit_fixed <- stats::glm(cbind(npos, ntest - npos) ~
      offset(log(sample_size)) + offset(log(inoculum)),
    data = data, family = binomial(link = "cloglog")
  )
  # Estimate laboratory effects
  summary_fit <- summary(fit_fixed)
  coef_fit    <- coef(summary_fit)
  mu_log      <- coef_fit["(Intercept)", "Estimate"]    #log mean effect
  mu_log_se   <- coef_fit["(Intercept)", "Std. Error"]  #SE of log mean effect
  mu          <- exp(mu_log)   #mean effect
  list(
    fit = fit_fixed, mu = mu, mu_log = mu_log, mu_log_se = mu_log_se
  )
}


chooseFixedMethodICC <- function(data) {
  # Returns method to be used for estimating ICC for fixed model.
  #   i.e., 'LMM' or 'ANCOVA'
  tst_lmer1 <- tryCatch(
    lme4::lmer(y ~ (1 | lab_id) + inoculum,
      data = data, REML = TRUE,
      control = lme4::lmerControl(
        check.conv.singular = lme4::.makeCC(
          action = "ignore", tol = 1e-4
        )
      )
    ),
    error = function(e) e,
    warning = function(w) w
  )
  error_lmer1   <- methods::is(tst_lmer1, "error")
  warning_lmer1 <- methods::is(tst_lmer1, "warning")
  problem_lmer1 <- error_lmer1 || warning_lmer1
  if (problem_lmer1) {
    return("ANCOVA")
  } else {
    tst_lmer2 <- tryCatch(
      summary(tst_lmer1),
      error = function(e) e,
      warning = function(w) w
    )
    error_lmer2   <- methods::is(tst_lmer2, "error")
    warning_lmer2 <- methods::is(tst_lmer2, "warning")
    problem_lmer2 <- error_lmer2 || warning_lmer2
    if (problem_lmer2) {
      return("ANCOVA")
    } else {
      return("LMM")
    }
  }
}


iccSigmaLmm <- function(data) {
  # Returns ICC & sigma for LMM.
  fit_lmer <- lme4::lmer(y ~ (1 | lab_id) + inoculum,
    data = data, REML = TRUE,
    control = lme4::lmerControl(
      check.conv.singular = lme4::.makeCC(
        action = "ignore", tol = 1e-4
      )
    )
  )
  vc <- as.data.frame(lme4::VarCorr(fit_lmer))
  var_between <- vc[vc$grp == "lab_id", "vcov"]
  var_within  <- vc[vc$grp == "Residual", "vcov"]
  var_total   <- var_between + var_within
  ICC <- var_between / var_total
  if (var_between < 0) {
    sigma <- NA
  } else {
    sigma <- sqrt(var_between)  #SD of log lab effects (random effects distribution)
  }
  list(
    ICC = ICC, sigma = sigma
  )
}


iccSigmaAncova <- function(data) {
  # Returns ICC & sigma for ANCOVA.
  # Use the method in Stanish and Taylor (1983)
  fit_lm <- stats::lm(
    y ~ as.factor(lab_id) + I(inoculum - mean(inoculum)),
    data = data
  )
  n_labs <- length(unique(data$lab_id))
  ns <- aggregate(
    y ~ as.factor(lab_id), data = data,
    FUN = length
  )[, "y"]
  n0 <- (1 / (n_labs - 1)) * (sum(ns) - sum(ns ^ 2) / sum(ns))
  grand_mean_inoculum <- mean(data$inoculum)
  data_inoculum_lab <- aggregate(
    inoculum ~ lab_id, data = data,
    FUN = mean
  )
  colnames(data_inoculum_lab)[2] <- "mean_inoculum"
  numer <- sum(ns ^ 2 * (data_inoculum_lab$mean_inoculum - grand_mean_inoculum) ^ 2)
  denom <- sum((data$y - grand_mean_inoculum) ^ 2)
  n01 <- (1 / (n_labs - 1)) * ((n_labs - 1) * n0 - (numer / denom))  #Eq 2.4
  fit_anova   <- anova(fit_lm)
  var_within  <- fit_anova$"Mean Sq"[3]  #MS Residuals
  MS_labs     <- fit_anova$"Mean Sq"[1]  #MS Labs
  var_between <- (MS_labs - var_within) / n01
  var_total   <- var_within + var_between
  ICC         <- var_between / var_total
  if (var_between < 0) {
    sigma <- NA
  } else {
    sigma <- sqrt(var_between)  #SD of log lab effects (random effects distribution)
  }
  list(
    ICC = ICC, sigma = sigma
  )
}
