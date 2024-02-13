# Helper functions for calculatiing POD and LOD

format3 <- function(x) formatC(x, digits = 3, format = "f")

#----------------------------  Error-handling  ---------------------------------

isErrorLODRE <- function(fitted_RE_model, lod_prob, sample_size, inoculum_per_unit) {
  # Returns a scalar logical
  tst_LOD <- tryCatch(
    lodPointRE(fitted_RE_model,
      value = lod_prob, sample_size = sample_size, inoculum_per_unit = inoculum_per_unit
    ),
    error = function(e) e, warning = function(w) w
  )
  methods::is(tst_LOD, "error")
}

lodCalcErrorAlert <- function(session) {
  modal("LOD Calculation Error", session,
    span("Possibly due to zero-probability predictions.")
  )
}


#-----------------------  Random effects model  --------------------------------

labEffectsRE <- function(fitted_model, lab_names, mu_log_se) {
  # Random intercept model: Find effect of each lab and its standard error
  #  Individual lab effect equals fixed effect + random effect by labID.
  #    (i.e., overall effect + deviation)
  #  Standard error for individual lab effect (intercept) equals sqrt of
  #    variance for mean lab effect + variance for random effects.
  lab_effects <- as.data.frame(lme4::ranef(fitted_model))
  model_intercept <- lme4::fixef(fitted_model)[['(Intercept)']]
  data.frame(
    lab_id           = lab_effects$grp,  #level of grouping variable
    lab_name         = as.factor(lab_names),
    estimated_effect = model_intercept + lab_effects$condval,  #int + mean
    SE               = sqrt(mu_log_se ^ 2 + lab_effects$condsd ^ 2)
  )
}

lodPointRE <- function(model, value, sample_size, inoculum_per_unit) {
  # Called by isErrorLODRE() for error handling
  # Random intercept model: Returns the point estimate for LOD.
  # LOD50 -> value = 0.5
  findInt <- function(x) {
    my_df <- data.frame(sample_size = sample_size, lab_id = NA, inoculum_per_unit = x)
    my_prediction <- predict(model, my_df, re.form = NA, type = "response")
    my_prediction - value
  }
  lower_endpt <- 0
  upper_endpt <- max(inoculum_per_unit) + 0.1
  sign_lower  <- sign(findInt(lower_endpt))
  sign_upper  <- sign(findInt(upper_endpt))
  if (sign_lower == sign_upper || sign_lower == 0 || sign_upper == 0) {
    # Can occur if 'my_prediction' is zero
    stop("Search space for uniroot() is not valid.")
  }
  search_interval <- range(lower_endpt, upper_endpt)
  uniroot(findInt, interval = search_interval)$root
}

lodCIRE <- function(predicted_all_labs, lod_prob) {
  # Find intersections of horizontal probability line and POD interval curves.
  inoculum_levels <- predicted_all_labs$inoculum_per_unit
  mean_POD_U      <- predicted_all_labs$mean_POD_U
  mean_POD_L      <- predicted_all_labs$mean_POD_L
  LOD_L <- inoculum_levels[which.min(abs(mean_POD_U - lod_prob))]
  LOD_U <- inoculum_levels[which.min(abs(mean_POD_L - lod_prob))]
  c(LOD_L = LOD_L, LOD_U = LOD_U)
}

podEachLabRE <- function(fitted_model, LOD, inoc_max, lab_ids, sample_size) {
  # Random intercept model: Returns a df of POD point estimates for each lab.
  predictFunRE <- function(model, predicted_values) {
    # This is predict.merMod
    predict(model, newdata = predicted_values, re.form = NULL, type = "response")
  }
  ds <- c(LOD, seq(from = 1e-9, to = inoc_max, length.out = 1e+2))  #for plotting only
  ds <- sort(unique(ds))
  predicted_values <- expand.grid(lab_id = lab_ids, inoculum_per_unit = ds)
  predicted_values <- data.frame(sample_size = sample_size, predicted_values)
  predicted_values$POD <- predictFunRE(fitted_model, predicted_values)
  predicted_values
}

podAllLabsRE <- function(fitted_model, LOD, inoc_max, lab_ids, sample_size,
    n_sim, alpha_level, session) {
  # Random intercept model: Returns a df of average POD point estimates & CI.
  #   note: CI must be bootstrapped
  predictFunRE <- function(model, predicted_values) {
    # This is predict.merMod
    #   i.e., fitted values, unconditional (level-0)
    predict(model, newdata = predicted_values, re.form = NA, type = "response")
  }
  if (inoc_max < 10) {
    incr <- 1e-4
  } else {
    incr <- 1e-3  #to avoid overuse of RAM
  }
  ds <- c(1e-9, LOD, seq(from = min(1e-4, LOD), to = inoc_max, by = incr))
  ds <- sort(unique(ds))
  predicted_values <- data.frame(
    sample_size = sample_size, lab_id = NA, inoculum_per_unit = ds
  )
  predicted_values$mean_POD <- predictFunRE(fitted_model, predicted_values)
  predicted_values_CI <- podAllLabsCIRE(fitted_model,
    n_sim, predicted_values, alpha_level, session
  )
  cbind(predicted_values, predicted_values_CI)
}

podAllLabsCIRE <- function(fitted_model, n_sim, predicted_all_labs, alpha_level, session) {
  # Called by podAllLabsRE()
  # Random intercept model: Adds CI to df of average POD point estimates.
  # Get CI for mean POD
  #https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html
  #  from link above - similar to Step 3a: lme4::bootMer() method 1
  # Return predicted values from bootstrap
  mySumm <- function(.) {
    incProgress(amount = .9 / n_sim, session = session)
    predict(., newdata = predicted_all_labs, re.form = NA, type = "response")
  }
  # lme4::bootMer() method
  boot1 <- lme4::bootMer(fitted_model,  #~95% of computation time
    FUN = mySumm, nsim = n_sim, seed = 27181, re.form = NA, type = "parametric"
  )
  # Collapse bootstrap into 95% PI
  boot1_PI <- sumBoot(boot1, alpha_level)  #~5% of computation time
  mean_POD_L <- boot1_PI[, 1]
  mean_POD_U <- boot1_PI[, 2]
  cbind(mean_POD_L, mean_POD_U)
}

sumBoot <- function(merBoot, alpha_level) {
  # Called by podAllLabsCIRE()
  # merBoot: a 'boot' object from lme4::bootMer()
  # https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html
  half_alpha <- alpha_level / 2
  half_alpha_complement <- 1 - half_alpha
  data.frame(
    lwr = apply(merBoot$t, MARGIN = 2,
      FUN = function(x) {
        as.numeric(quantile(x, probs = half_alpha, na.rm = TRUE))
      }
    ),
    upr = apply(merBoot$t, MARGIN = 2,
      FUN = function(x) {
        as.numeric(quantile(x, probs = half_alpha_complement, na.rm = TRUE))
      }
    )
  )
}


#-----------------------  Fixed effects model  ---------------------------------

labEffectsFE <- function(fitted_model, lab_ids, lab_names) {
  # Fixed effects model: Find effect of each lab and its standard error
  summary_fit <- summary(fitted_model)
  coef_fit    <- coef(summary_fit)
  lab_effects <- data.frame(
    lab_id           = as.factor(lab_ids),
    lab_name         = as.factor(lab_names),
    estimated_effect = as.numeric(coef_fit[, "Estimate"]),
    SE               = as.numeric(coef_fit[, "Std. Error"])
  )
  rownames(lab_effects) <- NULL
  lab_effects
}

lodPointFE <- function(mu, lod_prob, sample_size) {
  # Equation 32 from Jarvis et al. 2019
  -log(1 - lod_prob) / (sample_size * mu)
}

lodCIFE <- function(LOD, mu_log_se, t_critval) {
  # Equation 33 from Jarvis et al. 2019
  exp_t_mu_log_se <- exp(t_critval * mu_log_se)
  LOD_L <- LOD / exp_t_mu_log_se
  LOD_U <- LOD * exp_t_mu_log_se
  c(LOD_L = LOD_L, LOD_U = LOD_U)
}

podEachLabFE <- function(fitted_model, LOD, inoc_max, lab_ids, sample_size) {
  # Fixed effects model: Returns a df of POD point estimates for each lab.
  predictFunFE <- function(model, predicted_values) {
    predict(model, newdata = predicted_values, type = "response")
  }
  ds <- c(LOD, seq(from = 1e-9, to = inoc_max, length.out = 1e+2))  #for plotting only
  ds <- sort(unique(ds))
  predicted_values <- expand.grid(lab_id = lab_ids, inoculum_per_unit = ds)
  predicted_values <- data.frame(sample_size = sample_size, predicted_values)
  predicted_values$POD <- predictFunFE(fitted_model, predicted_values)
  predicted_values
}

podAllLabsFE <- function(fitted_model, LOD, inoc_max, lab_ids, sample_size, t_critval) {
  # Fixed effects model: Returns a df of average POD point estimates & CI.
  predictFunFE <- function(model, predicted_values) {
    # This is predict.merMod
    predict(model, newdata = predicted_values, type = "response", se.fit = TRUE)
  }
  ds <- c(LOD, seq(from = 1e-9, to = inoc_max, by = 1e-3))
  ds <- sort(unique(ds))
  predicted_values <- data.frame(
    sample_size = sample_size, lab_id = NA, inoculum_per_unit = ds
  )
  preds <- predictFunFE(fitted_model, predicted_values)
  predicted_values$mean_POD <- preds$fit
  ME <- t_critval * preds$se.fit
  predicted_values$mean_POD_U <- preds$fit + ME
  predicted_values$mean_POD_L <- preds$fit - ME
  predicted_values
}
