create_bar_chart <- function(data) {

  validate(
    need(nrow(data) > 0, "Select regions on the map to add their values to this graph.")
  )

  data$name <- ifelse(is.na(data$NAME_2), as.character(data$NAME_1), as.character(data$NAME_2))

  ref <- ifelse(is.na(data$NAME_2), as.character(data$NAME_1), as.character(data$CC_2))
  map_level <- ifelse(is.na(data$NAME_2), 1, 2)



  if (map_level == 2){
    selected <- which(lista_ref %in% data$CC_2)

    for (i in 1:length(selected)){
      df <- data.frame(1990:2021, data_list[[selected[1]]][,1])
      if (length(selected) == 1) break
      for (j in 2:length(selected)){
        df[,j + 1] <-  data_list[[selected[j]]][,1]
      }
    }

    colnames(df) <- c("Ano", data$NAME_2)

    df %>%
      e_charts(Ano) %>%
      reduce(colnames(df)[-1], add_bar, .init = .) %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        min = 1990,
        max = 2022,
        axisLabel = list(
          formatter = htmlwidgets::JS("function(value) { return value.toString().replace(',', ''); }")
        )
      ) %>%
      e_axis_labels(x = "Ano", y = "Intensidade média") %>%
      e_y_axis(nameLocation = "center", nameGap  = 30)  %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
    function(params) {
      return (
        "<strong>" + params.seriesName + "</strong><br />" +
        "Intensidade média em " + params.value[0] + ": " + params.value[1]
      );
    }
    '))
  }
}
