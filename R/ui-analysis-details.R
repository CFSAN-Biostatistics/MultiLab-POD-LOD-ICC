###########################  Analysis Details  #################################

my_analysis_details <- tabItem(tabName = "analysis_details",
  tags$section(id = "analysis_details_section",
    tags$h1("Analysis Details"),
    br(),
    em(h2("Background")),
    div(
      p(
        "This app implements the random intercept complementary log-log model",
        "suggested by Jarvis et al. (2019) to estimate probability of",
        "detection (POD) and level of detection (LOD) from a multi-laboratory",
        "validation study for a qualitative (binary) microbiological assay.",
        "The model is fit using R statistical software."
      ),
      class = "analysis-body"
    ),

    br(),
    em(h2("POD and LOD")),
    div(
      p(
        "The", em("lme4"), "R package (Bates et al., 2015) is used to fit the",
        "random intercept model, which is a Generalized Linear Mixed [Effects]",
        "Model (GLMM). If the algorithms used to fit the model fail to converge,",
        "the standard", em("stats"), "R package is used to fit a Generalized",
        "Linear [Fixed Effects] Model (GLM) instead."
      ),
      p(
        "The model is then used to estimate the POD function for the assay.",
        "For the random intercept model, confidence limits for POD are obtained",
        "using model-based parametric bootstrapping; LOD and its confidence limits",
        "are then obtained using a numerical search of the POD function and its",
        "confidence limits. For the fixed effects model, LOD and its confidence",
        "limits are obtained using Equations 32 and 33 in Jarvis et al. (2019);",
        "the confidence limits for POD are obtained using the fitted POD",
        "values and a margin of error using a t critical value and the estimated",
        "standard errors from the fitted POD function."
      ),
      class = "analysis-body"
    ),

    br(),
    em(h2("ICC")),
    div(
      p(
        "This app also calculates the intra-laboratory correlation coefficient (ICC)",
        "to estimate the proportion of total variance attributable to",
        "between-laboratory variance. A small ICC value indicates that an assay",
        "has little dependence on laboratory choice and is therefore more robust."
      ),
      p(HTML(
        "For the random intercept model, the ICC is calculated using between-lab",
        "variance estimated from the model and within-lab variance of (&pi; ^ 2) / 6",
        "(Hedeker et al., 2006).",
        "For the fixed effects model, the <em>lme4</em> package",
        "(Bates et al., 2015) is used if possible (LMM approach) to estimate both",
        "the between-lab variance and within-lab variance; otherwise,",
        "the standard <em>stats</em> package is used to apply the ANCOVA method",
        "of Stanish et al. (1983)."
      )),
      class = "analysis-body"
    )
  )
)
