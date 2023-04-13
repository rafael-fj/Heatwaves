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
