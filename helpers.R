################################################################################
##  Helper functions for Multilab LOD app
################################################################################


#from help(is.integer)
is_wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
  abs(x - round(x)) < tol
}


##########################  LOD  ###############################################

LODpoint <- function(model, value, sample_size, inoculum) {
  # Returns the point estimate for LOD.
  # LOD50 -> value = 0.5
  findInt <- function(x) {
    my_df <- data.frame(sample_size = sample_size, lab_id = NA, inoculum = x)
    my_prediction <- predict(model, my_df, re.form = NA, type = "response")
    my_prediction - value
  }
  lower_endpt <- 0
  upper_endpt <- max(inoculum) + 0.1
  sign_lower  <- sign(findInt(lower_endpt))
  sign_upper  <- sign(findInt(upper_endpt))
  if (sign_lower == sign_upper || sign_lower == 0 || sign_upper == 0) {
    #can occur if 'my_prediction' is zero
    stop("Search space for uniroot() is not valid.")
  }
  search_interval <- range(lower_endpt, upper_endpt)
  pt_est <- uniroot(findInt, interval = search_interval)$root
  pt_est
}

# LODpoint(model = fit1, value = 0.5, sample_size = 25,
#          inoculum = c(0, 0.04, 0.08, 0.16, 0.32)
# )


#not used in v1.2.0  -----------------------------------------------------------
# LODlimit <-
#   function(model, value, sample_size, inoculum,
#            limit = c("lower", "upper"), conf_level = 0.95,
#            num_sim = 200L, seed = 1234) {
#   # Returns the lower (or upper) confidence limit for LOD.
#   limit <- match.arg(limit)
#   findInt.LU <- function(x) {
#     #returns a function whose root is the lower (or upper) CL for LOD
#     my_df <- data.frame(sample_size = sample_size, lab_id = NA, inoculum = x)
#     predictFUN <- function(model) {
#       predict(model, my_df, re.form = NA, type = "response")
#     }
#     my_boot <- lme4::bootMer(model, FUN = predictFUN,
#                              nsim = num_sim, seed = seed
#     )
#     if (limit == "lower") {
#       my_index <- 2
#     } else {
#       my_index <- 1
#     }
#     confint(my_boot, level = conf_level)[my_index] - value
#   }
#
#   #slower, but shouldn't fail
#   # note: cannot start with 0
#   search_interval <- range(0.001, max(inoculum) + 0.1)
#   uniroot(findInt.LU, interval = search_interval)$root
#
#   # ##############################
#   # #faster, but could fail
#   # pt_est <- LODpoint(model, value, sample_size, inoculum)
#   # search_interval <- range(0.75 * pt_est, 1.25 * pt_est)
#   # uniroot(findInt.LU, interval = search_interval, extendInt = "yes")$root
#   # #############################
# }
#
# # LODlimit(model = fit1, value = .5, sample_size = sample_size,
# #          inoc_levels = dat$inoculum, limit = "lower", level = 0.95,
# #          num_sim = 200L, seed = 1234
# # )
#
# ##what it looks like if it fails (faster approach only)
# # my_df <- data.frame(sample_size = 25, labID = NA, inoculum = -1)
# # lme4::bootMer(fit1, FUN = function(model) {
# #        predict(model, my_df, re.form = NA, type = "response")
# # })
#
# # LODlimit(model = fit1, value = .5, sample_size = sample_size,
# #           inoc_levels = dat$inoculum, limit = "upper", level = 0.95,
# #           num_sim = 200L, seed = 1234
# # )
#not used in v1.2.0  -----------------------------------------------------------



########################  User Interface  ######################################

wellSingleLabUI <- function(id, lab_id, num_levels, num_levels_default, ...) {
  # Returns a nested wellPanel object for a single laboratory
  # Inputs/
  #   id:         character scalar
  #   lab_id:     a numeric whole number lab identifier
  #   num_levels: a numeric whole number for number of inoc levels
  #   num_levels_default: number of levels when app starts
  #   ... : additional UI elements to include in the inner wellPanel

  lab_name <- paste0("lab", lab_id)

  wellPanel(
    id = paste0("panel_lab", lab_id),
    class = "lab-well",
    fluidRow(
      column(width = 5, align = "right",
        paste0("Laboratory ", lab_id, " Name:")
      ),
      column(width = 7, align = "left",
        textInput(inputId = lab_name,
          width = "90%", label = NULL, value = paste0("Lab ", lab_id)
        ) %>%
          tagAppendAttributes(class = "lab-well-top")
      )
    ),
    fluidRow(
      column(width = 4,
        h4("Inoculation level"),
        h4("(CFU/g or CFU/mL)")
      ),
      column(width = 4,
        br(),
        h4("How many inoculated tubes?")
      ),
      column(width = 4,
        br(),
        h4("How many positive tubes?")
      )
    ),

    #inoculum levels, etc.
    wellPanel(
      id = paste0("panel_lab_data", lab_id),
      class = "lab-well-inner",
      #https://shiny.rstudio.com/gallery/creating-a-ui-from-a-loop.html
        lapply(1:num_levels, function(i) {  #iterating over rows
          inoc_level_i <- paste0("inoc_level", i)
          ntest_i      <- paste0("ntest", i)
          npos_i       <- paste0("npos", i)
          my_row <- fluidRow(
            id = paste0("panel_lab", lab_id, "_level", i),
            class = "lab-well-level",
            lapply(c(inoc_level_i, ntest_i, npos_i), function(j) {
              input_name <- paste0(lab_name, "_", j)  #'lab1_inoc_level1', etc.
              if (grepl("inoc_level", j, fixed = TRUE)) {
                my_step <- 0.01
                my_icon <- "eye-dropper"
              } else if (grepl("ntest", j, fixed = TRUE)) {
                my_step <- 1
                my_icon <- "vials"
              } else {
                my_step <- 1
                my_icon <- "clipboard-check"
              }
              column(width = 4, align = "center",
                #"lab1_inoc_level1", "lab1_ntest1", "lab1_npos1", etc.
                shinyWidgets::numericInputIcon(
                  inputId = input_name, label = "",
                  value = 0,
                  step = my_step,
                  icon = icon(my_icon),
                  size = NULL,
                  width = "100%"
                ) %>%
                  tagAppendAttributes(class = "lab-well-inner-data")
              )
            })
          )
          if (i > num_levels_default) {
            shinyjs::hidden(my_row)
          } else {
            my_row
          }
        }),
        ...
    )

  )
}

