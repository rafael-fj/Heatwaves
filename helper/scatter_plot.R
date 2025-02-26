create_scatter_plot <- function(data) {

  shiny::validate(
    need(nrow(data) > 0, "Selecione regiões no mapa para ativar este gráfico")
  )
  shiny::validate(
    need(!any(data$Heatwaves == 0), "Favor selecionar apenas regiões válidas")
  )
  #print(head(data))
  #data$name <- ifelse(is.na(data$NAME_2), as.character(data$NAME_1), as.character(data$NAME_2))

  #ref <- ifelse(is.na(data$NAME_2), as.character(substr(data$ISO_1,-2,-1)), as.character(data$CC_2))
  map_level <- ifelse("NAME_2" %in% names(data), 2, 1)
  #print(paste0("map level = ", map_level))
  #print(map_level)
  #print(data$cod_muni)

  if (map_level == 1) {
    selected <- which(lista_ref %in% data$cod_muni)
    #print(selected)

    for (i in 1:length(selected)){
      df <- data.frame("Ano" = 1990:2021,
                       "Intensidade" = round(data_list[[selected[1]]][,3],2),
                       "Duracao" = round(data_list[[selected[1]]][,2],2),
                       "Cidade" = data$Capital[1])
      if (length(selected) == 1) break
      for (j in 2:length(selected)){
        df <-  rbind(df ,data.frame("Ano" = 1990:2021,
                                    "Intensidade" = round(data_list[[selected[j]]][,3],2),
                                    "Duracao" = round(data_list[[selected[j]]][,2],2),
                                    "Cidade" = data$Capital[j]))
      }
    }

   # colnames(df) <- c("Ano", data$Capital)
    #colnames(df2) <- c("Ano", data$Capital)
    #print(head(df))
    #saveRDS(df, file = "df.rds")
    #saveRDS(df2, file = "df2.rds")
    df <- df[order(df$Cidade),]

    plot <- df %>%
      group_by(Cidade) %>%
      e_chart(Ano) %>%
      e_scatter(Intensidade, Duracao, symbol_size = c(1,2)) %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        min = 1990,
        max = 2022,
        axisLabel = list(
          formatter = htmlwidgets::JS("function(value) { return value.toString().replace(',', ''); }")
        )
      ) %>%
      e_axis_labels(x = "Ano", y = "Duração média (dias)") %>%
      e_y_axis(nameLocation = "center", nameGap  = 30)  %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
    function(params) {
      return (
        "<strong>" + params.seriesName + "</strong><br />" +
        "Duração média em " + params.value[0] + ": " + params.value[1] + " Dias" +
        "<br />" +
        "Intensidade média em " + params.value[0] + ": " + params.value[2] + " ºC"
      );
    }
    ')) %>%
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
                       "Intensidade" = round(data_list[[selected[1]]][,3],2),
                       "Duracao" = round(data_list[[selected[1]]][,2],2),
                       "Cidade" = data$NAME_2[1])
      if (length(selected) == 1) break
      for (j in 2:length(selected)){
        df <-  rbind(df ,data.frame("Ano" = 1990:2021,
                                    "Intensidade" = round(data_list[[selected[j]]][,3],2),
                                    "Duracao" = round(data_list[[selected[j]]][,2],2),
                                    "Cidade" = data$NAME_2[j]))
      }
    }

    #colnames(df) <- c("Ano", data$NAME_2)
    #print(head(df))
  #print(paste0("selected = ", selected))
  #print(paste0("data = ", data))
  #print(paste0("df$Cidade = ", df$cidade))
  #print(unique(df$Cidade))
    df <- df[order(df$Cidade),]

    df %>%
      group_by(Cidade) %>%
      e_chart(Ano) %>%
      e_scatter(Intensidade, Duracao, symbol_size = c(1,2)) %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        min = 1990,
        max = 2022,
        axisLabel = list(
          formatter = htmlwidgets::JS("function(value) { return value.toString().replace(',', ''); }")
        )
      ) %>%
      e_axis_labels(x = "Ano", y = "Duração média (dias)") %>%
      e_y_axis(nameLocation = "center", nameGap  = 30)  %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
    function(params) {
      return (
        "<strong>" + params.seriesName + "</strong><br />" +
        "Duração média em " + params.value[0] + ": " + params.value[1] + " Dias" +
        "<br />" +
        "Intensidade média em " + params.value[0] + ": " + params.value[2] + "ºC"
      );
    }
    ')) %>%
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
