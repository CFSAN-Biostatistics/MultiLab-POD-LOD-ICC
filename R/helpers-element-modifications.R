icon2 <- function(name, class = NULL, lib = "font-awesome", ...) {
  icon_tag <- shiny::icon(name, class = class, lib = lib, `aria-hidden` = "true", ...)
  icon_tag$attribs$`aria-label` <- NULL
  icon_tag$attribs$role = "img"
  icon_tag
}

menuItem2 <- function(text, tab_name = NULL,
                      icon, newtab = TRUE, href = NULL) {
  li_tag <- menuItem(text,
    tabName = tab_name, icon = icon, newtab = newtab, href = href
  )
  tagAppendAttributes(li_tag, role = "listitem")
}

helpText2 <- function(text, id = NULL) {
  shiny::helpText(id = id, class = "help-text", HTML(text))
}

modal <- function(title, session, ...) {
  dismiss_button <- modalButton("Dismiss")
  dismiss_button <- shiny::tagAppendAttributes(dismiss_button, class = "modal-dismiss")
  dialog <- shiny::modalDialog(...,
    title = title,
    footer = dismiss_button, 
    size = "m",
    easyClose = TRUE,
    fade = FALSE
  )
  dialog$attribs$tabindex <- 0
  dialog$attribs$role <- "dialog"
  dialog$attribs$`aria-labelledby` <- "dialog_title"
  dialog$attribs$`aria-describedby` <- "dialog_desc"
  dialog$children[[1]]$children[[1]]$children[[1]]$children[[1]]$name <- "span"
  dialog$children[[1]]$children[[1]]$children[[1]]$attribs$id <- "dialog_title"
  dialog$children[[1]]$children[[1]]$children[[2]]$attribs$id <- "dialog_desc"
  shiny::showModal(dialog, session = session)
}
