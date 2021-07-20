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
        p("Inoculation level", class = "lab-well-column-names"),
        p("(CFU/g or CFU/mL)", class = "lab-well-column-names")
      ),
      column(width = 4,
        br(),
        p("How many inoculated tubes?", class = "lab-well-column-names")
      ),
      column(width = 4,
        br(),
        p("How many positive tubes?", class = "lab-well-column-names")
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
                  min = 0,
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

fileInput2 <- function(inputId, label, multiple, accept, width, buttonLabel,
                       placeholder, onchange) {
  #Allows user to specify 'onchange' attribute
  #https://stackoverflow.com/questions/62220495/r-shiny-restrict-fileinput-to-filename-pattern-and-not-just-file-type
  #https://stackoverflow.com/questions/190852/how-can-i-get-file-extensions-with-javascript
  my_input <- fileInput(
    inputId = inputId,
    label = label,
    multiple = multiple,
    accept = accept,
    width = width,
    buttonLabel = buttonLabel,
    placeholder = placeholder
  )
  my_input$children[[2]]$children[[1]]$children[[1]]$children[[2]]$attribs$onchange <- onchange
  return(my_input)
}


radioGroupButtons2 <- function(inputId, label, choices, selected, size, individual, checkIcon) {
  #Allows adding CSS to button text
  my_button <- shinyWidgets::radioGroupButtons(
    inputId    = inputId,
    label      = label,
    choices    = choices,
    selected   = selected,
    size       = size,
    individual = individual,
    checkIcon  = checkIcon
  )
  choice1 <- my_button$children[[3]]$children[[1]]$children[[1]][[1]]$children[[1]]$children[[4]]
  choice2 <- my_button$children[[3]]$children[[1]]$children[[1]][[2]]$children[[1]]$children[[4]]
  choice3 <- my_button$children[[3]]$children[[1]]$children[[1]][[3]]$children[[1]]$children[[4]]
  choice1 <- HTML(paste0("<span class='choose-data-entry-text'>", choice1, "</span>"))
  choice2 <- HTML(paste0("<span class='choose-data-entry-text'>", choice2, "</span>"))
  choice3 <- HTML(paste0("<span class='choose-data-entry-text'>", choice3, "</span>"))
  my_button$children[[3]]$children[[1]]$children[[1]][[1]]$children[[1]]$children[[4]] <- choice1
  my_button$children[[3]]$children[[1]]$children[[1]][[2]]$children[[1]]$children[[4]] <- choice2
  my_button$children[[3]]$children[[1]]$children[[1]][[3]]$children[[1]]$children[[4]] <- choice3
  return(my_button)
}


