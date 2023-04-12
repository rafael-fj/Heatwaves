create_labels <- function(data, map_level) {
  if (map_level == 1){
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Ondas de calor (média)</sup>",
      data[, "name_state"], data$Heatwaves
    )
  } else {
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Ondas de calor</sup>",
      data[, "name_muni"], data$Heatwaves
    )
  }

  labels %>% lapply(htmltools::HTML)
}
