################################################################################
#
# Purpose: Shiny app for interlaboratory method validation studies.
#
# Authors: Steven Wang, John Ihrie
#
# Details:
#   This app implements the random intercept complementary log-log model
#   suggested by Jarvis et al. (2019) to estimate probability of
#   detection (POD) and level of detection (LOD) from a multi-laboratory
#   validation study for a qualitative (binary) microbiological assay.
#   Also calculates the intra-laboratory correlation coefficient (ICC) to
#   estimate the proportion of total variance attributable to between-laboratory
#   variance.
#
# Files started: 6/4/2020
#
# References:
#   Jarvis B, Wilrich C, Wilrich P-T. Estimation of the POD Function and the LOD
#     of a Binary Microbiological Measurement Method from an Interlaboratory
#     Experiment. Journal of AOAC International. 2019;102(5):1617-1623.
#     doi:10.5740/jaoacint.18-0412.
#
#   Excel tool:
#     https://www.wiwiss.fu-berlin.de/fachbereich/vwl/iso/ehemalige/wilrich
#
################################################################################

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinybusy)
library(lme4)
library(ggplot2)
library(openxlsx)
library(sessioninfo)
library(dplyr)
library(tidyr)  #new

source("helpers.R")
source("helpers-model.R")
source("validate-inputs.R")

#global variables
glob_app_title   <- "MultiLab POD/LOD/ICC"
glob_app_version <- "v1.5.0"

glob_min_labs     <- 2L
glob_max_labs     <- 30L
#glob_max_labs <- 3  #for testing only!!!
glob_default_labs <- 2L
glob_panel_names  <- paste0("panel_lab", 1:glob_max_labs)  #see singleWellPanel()

glob_min_levels <- 3L
#glob_min_levels <- 2L  #for testing only!!!
glob_max_levels <- 12L
#glob_max_levels <- 3L  #for testing only!!!

glob_default_levels <- 3L

glob_min_size     <- 0
glob_default_size <- 25  #Sample (test portion) size in g or ml

glob_plot_aspect_ratio <- 0.45


glob_log_mean_effect_desc <- span(
  withMathJax(HTML("Mean Lab Effect \\((\\widehat{\\mu}\\))")),
  class = 'parameter-estimates-description'
)
glob_se_log_mean_effect_desc <- span(
  HTML("SE of Mean Lab Effect"),
  class = 'parameter-estimates-description'
)
glob_sigma_desc <- span(
  withMathJax(HTML("SD of Lab Effects \\((\\widehat{\\sigma}\\))")),
  class = 'parameter-estimates-description'
)
glob_ICC_desc <- span(
  HTML("ICC"),
  class = 'parameter-estimates-description'
)


# Example data -----------------------------------------------------------------
d  <- c(0, 0.04, 0.08, 0.16, 0.32)  #inoculation level (CFU/mL)
n1 <- c(1, 8, 8, 8, 1)  # number of inoculated tubes for lab1
y1 <- c(0, 5, 6, 8, 1)  # number of positive tubes for lab1
n2 <- c(1, 8, 8, 8, 1)
y2 <- c(0, 4, 6, 8, 1)
n3 <- c(1, 8, 8, 8, 1)
y3 <- c(0, 7, 7, 8, 1)
n4 <- c(1, 8, 8, 8, 1)
y4 <- c(0, 7, 8, 8, 1)
n5 <- c(1, 8, 8, 8, 1)
y5 <- c(0, 6, 7, 8, 1)
n6 <- c(1, 8, 8, 8, 1)
y6 <- c(0, 4, 7, 7, 1)
n7 <- c(1, 8, 8, 8, 1)
y7 <- c(0, 5, 6, 8, 1)
n8 <- c(1, 8, 8, 8, 1)
y8 <- c(0, 5, 6, 7, 1)
n9 <- c(1, 8, 8, 8, 1)
y9 <- c(0, 1, 7, 8, 1)
n10 <- c(1, 8, 8, 8, 1)
y10 <- c(0, 2, 3, 8, 1)

glob_sample_size_example <- 25
glob_sample_unit_example <- "g or mL"
glob_num_labs_example    <- 10
glob_num_levels_example  <- length(d)
dat_example <- data.frame(
  lab_id = rep(1:glob_num_labs_example, each = glob_num_levels_example),
  inoculum = rep(d, 5),
  ntest = as.integer(c(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10)),
  npos  = as.integer(c(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
)

dat_example$sample_size <- glob_sample_size_example
rm(d, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10)
rm(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)

dat_example_ui <- dat_example
dat_example_ui$sample_size <- NULL
dat_example_ui$lab_id <- paste("Lab", dat_example_ui$lab_id)
dat_example_ui$lab_id[1:nrow(dat_example_ui) %% 5 != 1] <- ""

colnames(dat_example_ui) <- c(
  "Lab Name", "Inoculation Level",
  "Inoculated Tubes", "Positive Tubes"
)
