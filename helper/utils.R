########################### Custom sidebar top ###########################
customSidebar <- function(title, bgColor = NULL, href = NULL, image = NULL, opacity = 0.8) {
  if (!is.null(bgColor)) {
    shiny::tags$a(
      class = "brand-link",
      href = if (!is.null(href)) href else "#",
      target = if (!is.null(href)) "_blank",
      shiny::tags$img(
        src = image,
        class = "brand-image",
        style = paste0("opacity: ", opacity),
        width = 50, # Ajuste conforme necessário
        height = 50  # Ajuste conforme necessário
      ),
      shiny::tags$span(
        class = "brand-text font-weight-light",
        title
      ),
      style = paste0("background-color: ", bgColor, ";")
    )
  } else {
    shiny::tags$a(
      class = "brand-link",
      href = if (!is.null(href)) href else "#",
      target = if (!is.null(href)) "_blank",
      shiny::tags$img(
        src = image,
        class = "brand-image",
        style = paste0("opacity: ", opacity),
        width = 50, # Ajuste conforme necessário
        height = 50  # Ajuste conforme necessário
      ),
      shiny::tags$span(
        class = "brand-text font-weight-light",
        title
      )
    )
  }
}

########################### Custom sidebar Menu Item ###########################
customMenuItem <- function(text, ..., icon = NULL, badgeLabel = NULL, badgeColor = "success",
                           tabName = NULL, href = NULL, newTab = TRUE, selected = NULL,
                           expandedName = as.character(gsub("[[:space:]]", "", text)),
                           startExpanded = FALSE, condition = NULL, bgColor = NULL) {
  subItems <- c(list(...))
  if (!is.null(icon)) {
    #tagAssert(icon, type = "i")
    icon$attribs$class <- paste0(icon$attribs$class, " nav-icon")
  }
  if (!is.null(href) + !is.null(tabName) + (length(subItems) > 0) != 1) {
    stop("Must have either href, tabName, or sub-items (contained in ...).")
  }
  if (!is.null(badgeLabel) && length(subItems) != 0) {
    stop("Can't have both badge and subItems")
  }
  if (!is.null(badgeLabel)) {
    validateStatus(badgeColor)
    badgeTag <- dashboardBadge(badgeLabel, color = badgeColor, position = "right")
  } else {
    badgeTag <- NULL
  }
  if (length(subItems) == 0) {
    return(
      shiny::tags$li(
        class = "nav-item",
        `data-display-if` = condition,
        shiny::tags$a(
          class = paste0("nav-link", if (!is.null(bgColor)) paste0(" bg-", bgColor)),
          id = if (!is.null(tabName)) paste0("tab-", tabName),
          href = if (!is.null(href)) href else "#",
          `data-target` = if (is.null(href)) {
            if (!is.null(tabName)) paste0("#shiny-tab-", tabName)
          },
          target = if (!is.null(href)) {
            if (newTab) "_blank"
          },
          `data-toggle` = if (is.null(href)) "tab",
          `data-value` = if (!is.null(tabName)) tabName,
          `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
          icon,
          shiny::tags$p(text, badgeTag)
        )
      )
    )
  } else {
    for (i in seq_along(subItems)) {
      if (subItems[[i]]$attribs$class == "nav-item") {
        subItems[[i]]$children[[1]]$attribs$class <- paste(subItems[[i]]$children[[1]]$attribs$class,
                                                           "treeview-link")
      }
    }
    default <- if (startExpanded)
      expandedName
    else ""
    dataExpanded <- shiny::restoreInput(id = "sidebarItemExpanded",
                                        default) %OR% ""
    isExpanded <- nzchar(dataExpanded) && (dataExpanded ==
                                             expandedName)
    selectedItems <- dropNulls(lapply(seq_along(subItems),
                                      function(i) {
                                        if (length(subItems[[i]]$children[[1]]$attribs$`data-start-selected`) >
                                            0)
                                          TRUE
                                        else NULL
                                      }))
    if (length(selectedItems) > 1)
      stop("Only 1 subitem may be selected!")
    return(
      shiny::tags$li(
        class = paste0("nav-item has-treeview",
                       if (isExpanded) " menu-open" else ""
        ),
        shiny::tags$a(
          href = "#",
          class = paste0("nav-link", if (!is.null(bgColor)) paste0(" bg-", bgColor)),
          `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
          icon,
          shiny::tags$p(text, shiny::tags$i(class = "right fas fa-angle-left")),
          `data-value` = expandedName
        ),
        shiny::tags$ul(
          class = "nav nav-treeview",
          `data-expanded` = expandedName,
          subItems
        )
      )
    )
  }
}



########################### Criador de tooltip ###########################

createInfo <- function(id, title) {
  button <- bsButton(id, label = "", icon("info"), style = "info", size = "extra-small", class = "custom-button")
  script <- sprintf(
    "$(document).ready(function() {
       $('#%s').tooltip({
         title: `%s`,
         html: true,
         placement: 'left'
       });
       $('#map').css('margin-top', '6vh'); // Applying margin-top to map
     });",
    id, title
  )
  return(list(button, tags$script(HTML(script))))
}

########################### Criador de tooltip - mapas ###########################

create_labels <- function(data, map_level) {
  if (map_level == 1){
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Ondas de calor (média)</sup>",
      data[, "NAME_1"], data$Heatwaves
    )
  } else {
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Ondas de calor</sup>",
      data[, "NAME_2"], data$Heatwaves
    )
  }

  labels %>% lapply(htmltools::HTML)
}

create_labels2 <- function(data, map_level) {
  if (map_level == 1){
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Média de PM2.5 no último mês</sup>",
      data[, "NAME_1"], data$PM25
    )
  } else {
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Média de PM2.5 no último mês</sup>",
      data[, "NAME_2"], data$PM25
    )
  }

  labels %>% lapply(htmltools::HTML)
}
