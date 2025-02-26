create_line_plot <- function(data) {
  shiny::validate(
    need(nrow(data) > 0, "Selecione regiões no mapa para ativar este gráfico")
  )

  shiny::validate(
    need(!any(data$Heatwaves == 0), "Favor selecionar apenas regiões válidas")
  )
  #data$name <- ifelse(is.na(data$NAME_2), as.character(data$NAME_1), as.character(data$NAME_2))
  print(head(data))
  #ref <- ifelse(is.na(data$NAME_2), as.character(substr(data$ISO_1,-2,-1)), as.character(data$CC_2))
  map_level <- ifelse("NAME_2" %in% names(data), 2, 1)
  #print(data$cod_muni)
  #print(paste0("map level = ", map_level))

  if (map_level == 1){
    selected <- which(lista_ref %in% data$cod_muni)
    #print(selected)

    for (i in 1:length(selected)){
      df <- data.frame("Ano" = 1990:2021,
                       "Frequencia" = data_list[[selected[1]]][,1],
                       "Cumulativo" = cumsum(data_list[[selected[1]]][,1]),
                       "Cidade" = data$Capital[1])
      if (length(selected) == 1) break
      for (j in 2:length(selected)){
        df <-  rbind(df ,data.frame("Ano" = 1990:2021,
                                  "Frequencia" = data_list[[selected[j]]][,1],
                                  "Cumulativo" = cumsum(data_list[[selected[j]]][,1]),
                                  "Cidade" = data$Capital[j]))
      }
    }

    #colnames(df) <- c("Ano", data$Capital)
    #print(df)
    #print(class(df$Manaus))
    #print(class(df$Ano))
    #write.csv(df, file = "test_input.csv")
    #str(df)

    #saveRDS(df, file = "uf.rds")
    df <- df[order(df$Cidade),]

    plot <- df %>%
      group_by(Cidade) %>%
      e_charts(Ano) %>%
      e_line(Cumulativo) %>%
      e_bar(Frequencia) %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        min = 1990,
        max = 2022,
        axisLabel = list(
          formatter = htmlwidgets::JS("function(value) { return value.toString().replace(',', ''); }")
        )
      ) %>%
      e_axis_labels(x = "Ano", y = "Ondas de Calor") %>%
      e_y_axis(nameLocation = "center", nameGap = 30) %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
          function(params) {
            var cidade = params.seriesName;
            var ano = params.value[0];
            var value = params.value[1];
            var seriesType = params.seriesType;

            var tooltipContent = "";

            if (seriesType === "line") {
              tooltipContent += "<strong>" + cidade + "</strong><br />";
              tooltipContent += "Ondas de Calor de 1990 até " + ano + ": "+ value;
            } else if (seriesType === "bar") {
              var cumulativo = params.value[2];

              tooltipContent += "<strong>" + cidade + "</strong><br />";
              tooltipContent += "Ondas de Calor em " + ano + ": " + value + "<br />";
            }

            return tooltipContent;
          }
        ')
      ) %>%
      e_toolbox(
        right = "5%",
        feature = list(
          saveAsImage = list(
            show = TRUE,
            title = "Salvar Imagem"
          ),
          dataView = list(
            show = TRUE,
            title = "Extrair Dados"
          )
        ),
        orient = "horizontal",
        itemGap = 20
      )

    return(plot)
  }


  if (map_level == 2){
    selected <- which(lista_ref %in% data$CC_2)

    for (i in 1:length(selected)){
      df <- data.frame("Ano" = 1990:2021,
                       "Frequencia" = data_list[[selected[1]]][,1],
                       "Cumulativo" = cumsum(data_list[[selected[1]]][,1]),
                       "Cidade" = data$NAME_2[1])
      if (length(selected) == 1) break
      for (j in 2:length(selected)){
        df <-  rbind(df ,data.frame("Ano" = 1990:2021,
                                    "Frequencia" = data_list[[selected[j]]][,1],
                                    "Cumulativo" = cumsum(data_list[[selected[j]]][,1]),
                                    "Cidade" = data$NAME_2[j]))
      }
    }

    #print(head(df))
    #colnames(df) <- c("Ano", data$NAME_2)

    #saveRDS(df, file = "muni.rds")
    df <- df[order(df$Cidade),]

  df %>%
      group_by(Cidade) %>%
      e_charts(Ano) %>%
      e_line(Cumulativo) %>%
      e_bar(Frequencia) %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        min = 1990,
        max = 2022,
        axisLabel = list(
          formatter = htmlwidgets::JS("function(value) { return value.toString().replace(',', ''); }")
        )
      ) %>%
      e_axis_labels(x = "Ano", y = "Ondas de Calor") %>%
      e_y_axis(nameLocation = "center", nameGap = 30) %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
      function(params) {
        var cidade = params.seriesName;
        var ano = params.value[0];
        var value = params.value[1];
        var seriesType = params.seriesType;

        var tooltipContent = "";

        if (seriesType === "line") {
          tooltipContent += "<strong>" + cidade + "</strong><br />";
          tooltipContent += "Ondas de Calor de 1990 até " + ano + ": " + value;
        } else if (seriesType === "bar") {
          var cumulativo = params.value[2];

          tooltipContent += "<strong>" + cidade + "</strong><br />";
          tooltipContent += "Ondas de Calor em " + ano + ": " + value + "<br />";
        }

        return tooltipContent;
      }
    ')
      ) %>%
    e_toolbox(
      right = "5%",
      feature = list(
        saveAsImage = list(
          show = TRUE,
          title = "Salvar Imagem"
        ),
        dataView = list(
          show = TRUE,
          title = "Extrair Dados"
        )
      ),
      orient = "horizontal",
      itemGap = 20
    )
  }
}
