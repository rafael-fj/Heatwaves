###################### BAR plot ###########################
create_polu_plot_bar <- function(data) {
  shiny::validate(
    need(nrow(data) > 0, "Selecione regiões no mapa para ativar este gráfico")
  )

#  shiny::validate(
#    need(!any(data$PM25 == 0), "Favor selecionar apenas regiões válidas")
#  )
  #data$name <- ifelse(is.na(data$NAME_2), as.character(data$NAME_1), as.character(data$NAME_2))

  #ref <- ifelse(is.na(data$NAME_2), as.character(substr(data$ISO_1,-2,-1)), as.character(data$CC_2))
  #print("ok")
  map_level <- ifelse("NAME_2" %in% names(data), 2, 1)
  #print(data$cod_muni)
  #print(paste0("map level = ", map_level))
  #print("map lvl ok")
  if (map_level == 1){
    list_select <- which(lista_ref %in% data$cod_muni)
    #print(selected)

    df_polu_calor <- data.frame(
      Year = c(2010:2021),
      Muni = as.character(0),
      PM25 = 0,
      Calor = 0
    )

    for (i in 1:length(list_select)){
      for (j in 1:12){
        next_row <- j + (i-1)*12
        df_polu_calor[next_row,1] <- 2009 + j
        df_polu_calor[next_row,2] <- data$cod_muni[i]
        df_polu_calor[next_row,3] <- df_polu_ano[j,which(colnames(df_polu_ano) == paste0("ID_",data$cod_muni[i]))]
        row_list <- j + 20
        df_polu_calor[next_row,4] <- data_list[[list_select[i]]][row_list,1]
      }
    }
    #print(head(df_polu_calor))

    df_polu_calor <- df_polu_calor %>%
      group_by(Muni, Calor) %>%
      summarise(PM25 = mean(PM25))

    for (i in 1:length(unique(df_polu_calor$Muni))){
      df_polu_calor[df_polu_calor$Muni == unique(df_polu_calor$Muni)[i],"Muni"] <- df_muni[df_muni$code_muni == unique(df_polu_calor$Muni)[i], "Cidade"]
    }

    p7 <- df_polu_calor %>%
      group_by(Muni) %>%
      e_charts(Calor) %>%
      e_bar(PM25) %>%
      e_tooltip(trigger = "axis") %>%
      e_axis_labels(x = "Ondas de Calor", y = "Média de MP2.5") %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        min = -1,
        max = max(df_polu_calor$Calor) + 1,
        minInterval = 1,
        axisLabel = list(
          formatter = htmlwidgets::JS('
        function(value) {
          if (value >= 0) {
            return value;
          } else {
            return "";
          }
        }
      ')
        )
      ) %>%
      e_y_axis(
        nameLocation = "center",
        nameGap = 30,
        axisLine = list(show = FALSE),
        axisTick = list(show = FALSE)
      ) %>%
      e_grid(
        splitLine = list(
          show = TRUE,
          lineStyle = list(
            color = "#f0f0f0",
            width = 1,
            type = "solid"
          )
        )
      ) %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
      function(params) {
        return (
          "<strong>" + params.seriesName + "</strong><br />" +
          "Média de MP2.5 em ondas de calor de " + params.value[0] + " dias: " + params.value[1]
        );
      }
    ')
      ) %>%
      e_toolbox_feature(feature = c("saveAsImage", "dataView"))
    return(p7)
  }


  if (map_level == 2){
    list_select <- which(lista_ref %in% data$CC_2)

    df_polu_calor <- data.frame(
      Year = c(2010:2021),
      Muni = as.character(0),
      PM25 = 0,
      Calor = 0
    )

    for (i in 1:length(list_select)){
      for (j in 1:12){
        next_row <- j + (i-1)*12
        df_polu_calor[next_row,1] <- 2009 + j
        df_polu_calor[next_row,2] <- data$CC_2[i]
        df_polu_calor[next_row,3] <- df_polu_ano[j,which(colnames(df_polu_ano) == paste0("ID_",data$CC_2[i]))]
        row_list <- j + 20
        df_polu_calor[next_row,4] <- data_list[[list_select[i]]][row_list,1]
      }
    }

    df_polu_calor <- df_polu_calor %>%
      group_by(Muni, Calor) %>%
      summarise(PM25 = mean(PM25))

    for (i in 1:length(unique(df_polu_calor$Muni))){
      df_polu_calor[df_polu_calor$Muni == unique(df_polu_calor$Muni)[i],"Muni"] <- df_muni[df_muni$code_muni == unique(df_polu_calor$Muni)[i], "Cidade"]
    }

    df_polu_calor %>%
      group_by(Muni) %>%
      e_charts(Calor) %>%
      e_bar(PM25) %>%
      e_tooltip(trigger = "axis") %>%
      e_axis_labels(x = "Ondas de Calor", y = "Média de MP2.5") %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        min = -1,
        max = max(df_polu_calor$Calor) + 1,
        minInterval = 1,
        axisLabel = list(
          formatter = htmlwidgets::JS('
        function(value) {
          if (value >= 0) {
            return value;
          } else {
            return "";
          }
        }
      ')
        )
      ) %>%
      e_y_axis(
        nameLocation = "center",
        nameGap = 30,
        axisLine = list(show = FALSE),
        axisTick = list(show = FALSE)
      ) %>%
      e_grid(
        splitLine = list(
          show = TRUE,
          lineStyle = list(
            color = "#f0f0f0",
            width = 1,
            type = "solid"
          )
        )
      ) %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
      function(params) {
        return (
          "<strong>" + params.seriesName + "</strong><br />" +
          "Média de MP2.5 em ondas de calor de " + params.value[0] + " dias: " + params.value[1]
        );
      }
    ')
      ) %>%
      e_toolbox_feature(feature = c("saveAsImage", "dataView"))
  }
}

###################### Intensity vs PM25 #############
create_polu_plot_heat <- function(data) {
  shiny::validate(
    need(nrow(data) > 0, "Selecione regiões no mapa para ativar este gráfico")
  )

  #  shiny::validate(
  #    need(!any(data$PM25 == 0), "Favor selecionar apenas regiões válidas")
  #  )
  #data$name <- ifelse(is.na(data$NAME_2), as.character(data$NAME_1), as.character(data$NAME_2))

  #ref <- ifelse(is.na(data$NAME_2), as.character(substr(data$ISO_1,-2,-1)), as.character(data$CC_2))
  #print("ok")
  map_level <- ifelse("NAME_2" %in% names(data), 2, 1)
  #print(data$cod_muni)
  #print(paste0("map level = ", map_level))
  #print("map lvl ok")
  if (map_level == 1){
    code_fix <- paste0("ID_",data$cod_muni)
    #print(code_fix)

    col_ref <- which(colnames(df_polu_mes) %in% code_fix) - 3

    df_polu_calor <- data.frame(
      Duration = as.integer(),
      Intensity = as.numeric(),
      PM25 = as.numeric(),
      Cidade = as.character()
    )

    for (i in 1:length(col_ref)){
      temp <- polu_list[[col_ref[i]]]
      temp$Cidade <- df_muni_polu[df_muni_polu$code_muni == data$cod_muni[i],"Cidade"]
      df_polu_calor <- rbind(df_polu_calor, temp[,c(1,4:6)])
    }
    colnames(df_polu_calor) <- c("Duration", "Intensity","PM25","Muni")
    df_polu_calor$Intensity <- round(df_polu_calor$Intensity, digits = 1)
    df_polu_calor$PM25 <- round(df_polu_calor$PM25, digits = 1)

    print(df_polu_calor)
    df_polu_calor <- df_polu_calor[order(df_polu_calor$Muni$Cidade),]


    p <- df_polu_calor %>%
      group_by(Muni$Cidade) %>%
      e_charts(Intensity) %>%
      e_line(PM25) %>%
      e_tooltip(trigger = "axis") %>%
      e_axis_labels(x = "Intensidade da Onda de Calor", y = "Média de MP2.5") %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        minInterval = 1,
        axisLabel = list(
          formatter = htmlwidgets::JS('
        function(value) {
          if (value >= 0) {
            return value;
          } else {
            return "";
          }
        }
      ')
        )
      ) %>%
      e_y_axis(
        nameLocation = "center",
        nameGap = 30,
        axisLine = list(show = FALSE),
        axisTick = list(show = FALSE)
      ) %>%
      e_grid(
        splitLine = list(
          show = TRUE,
          lineStyle = list(
            color = "#f0f0f0",
            width = 1,
            type = "solid"
          )
        )
      ) %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
      function(params) {
        return (
          "<strong>" + params.seriesName + "</strong><br />" +
          "Intensidade (Onda de Calor): " + params.value[0] + " | MP2.5 (Poluição): " + params.value[1]
        );
      }
    ')
      ) %>%
      e_toolbox_feature(feature = c("saveAsImage", "dataView"))
    return(p)
  }


  if (map_level == 2){
    code_fix <- paste0("ID_",data$CC_2)
    #print(code_fix)

    col_ref <- which(colnames(df_polu_mes) %in% code_fix) - 3
    #print(col_ref)

    df_polu_calor <- data.frame(
      Duration = as.integer(),
      Intensity = as.numeric(),
      PM25 = as.numeric(),
      Cidade = as.character()
    )

    for (i in 1:length(col_ref)){
      temp <- polu_list[[col_ref[i]]]
      temp$Cidade <- df_muni_polu[df_muni_polu$code_muni == data$CC_2[i],"Cidade"]
      df_polu_calor <- rbind(df_polu_calor, temp[,c(1,4:6)])
    }
    colnames(df_polu_calor) <- c("Duration", "Intensity","PM25","Muni")
    df_polu_calor$Intensity <- round(df_polu_calor$Intensity, digits = 1)
    df_polu_calor$PM25 <- round(df_polu_calor$PM25, digits = 1)
    df_polu_calor <- df_polu_calor[order(df_polu_calor$Muni$Cidade),]

    p <- df_polu_calor %>%
      group_by(Muni$Cidade) %>%
      e_charts(Intensity) %>%
      e_line(PM25) %>%
      e_tooltip(trigger = "axis") %>%
      e_axis_labels(x = "Intensidade da Onda de Calor", y = "Média de MP2.5") %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        minInterval = 1,
        axisLabel = list(
          formatter = htmlwidgets::JS('
        function(value) {
          if (value >= 0) {
            return value;
          } else {
            return "";
          }
        }
      ')
        )
      ) %>%
      e_y_axis(
        nameLocation = "center",
        nameGap = 30,
        axisLine = list(show = FALSE),
        axisTick = list(show = FALSE)
      ) %>%
      e_grid(
        splitLine = list(
          show = TRUE,
          lineStyle = list(
            color = "#f0f0f0",
            width = 1,
            type = "solid"
          )
        )
      ) %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
      function(params) {
        return (
          "<strong>" + params.seriesName + "</strong><br />" +
          "Intensidade (Onda de Calor): " + params.value[0] + " | MP2.5 (Poluição): " + params.value[1]
        );
      }
    ')
      ) %>%
      e_toolbox_feature(feature = c("saveAsImage", "dataView"))
    return(p)
  }
}

###################### Line plot / Polution over time ###########################
create_polu_plot_line <- function(data) {
  shiny::validate(
    need(nrow(data) > 0, "Selecione regiões no mapa para ativar este gráfico")
  )

  #  shiny::validate(
  #    need(!any(data$PM25 == 0), "Favor selecionar apenas regiões válidas")
  #  )
  #data$name <- ifelse(is.na(data$NAME_2), as.character(data$NAME_1), as.character(data$NAME_2))

  #ref <- ifelse(is.na(data$NAME_2), as.character(substr(data$ISO_1,-2,-1)), as.character(data$CC_2))
  map_level <- ifelse("NAME_2" %in% names(data), 2, 1)
  #print(data$cod_muni)
  #print(paste0("map level = ", map_level))

  if (map_level == 1){
    #list_select <- which(lista_ref %in% data$cod_muni)
    #print(head(data))
    code_fix <- paste0("ID_",data$cod_muni)
    #print(code_fix)

    col_ref <- which(colnames(df_polu_mes) %in% code_fix)
    #print(col_ref)

    line_data <- df_polu_mes[,c(3,col_ref)] %>%
      pivot_longer(cols = -Date, names_to = "Muni", values_to = "PM25")

    for (i in 1:length(unique(line_data$Muni))){
      line_data[line_data$Muni == unique(line_data$Muni)[i],"Muni"] <- df_muni[df_muni$code_muni == substr(unique(line_data$Muni)[i],4,10), "Cidade"]
    }

    #print(head(line_data))
    line_data$PM25 <- round(line_data$PM25, digits = 1)
    line_data <- line_data[order(line_data$Muni),]

    p <- line_data %>%
      group_by(Muni) %>%
      e_charts(Date) %>%
      e_line(PM25, symbol = 'circle', symbolSyze = 1) %>%
      e_tooltip(trigger = "axis") %>%
      e_axis_labels(x = "Ano", y = "Média mensal MP2.5") %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        min = as.Date("2010-01-01"),  # Set the minimum date to January 1, 2010
        max = as.Date("2022-12-31"),  # Set the maximum date to December 31, 2022
      ) %>%
      e_y_axis(nameLocation = "center", nameGap = 30) %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
      function(params) {
        var date = new Date(params.value[0]);
        var monthNames = [
          "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
          "Jul", "Ago", "Set", "Out", "Nov", "Dez"
        ];
        var formattedDate = monthNames[date.getMonth()] + " " + date.getFullYear();
        return (
          "<strong>" + params.seriesName + "</strong><br />" +
          "Média mensal de MP2.5 em " + formattedDate + ": " + params.value[1]
        );
      }
    ')) %>%
      e_mark_line(data = list(yAxis = 5), title = "Limite OMS", symbol = "none", tooltip = list(show = FALSE),
                  lineStyle = list(color = "lightgray"),  # Cor da linha padrão
                  emphasis = list(
                    lineStyle = list(color = "#800080")  # Cor da linha ao passar o mouse
                  )) %>%
      e_mark_line(data = list(yAxis = 10), title = "2x") %>%
      e_mark_line(data = list(yAxis = 15), title = "3x") %>%
      e_mark_line(data = list(yAxis = 25), title = "5x") %>%
      e_mark_line(data = list(yAxis = 35), title = "7x") %>%
      e_mark_line(data = list(yAxis = 50), title = "10x") %>%

      e_toolbox_feature(feature = c("saveAsImage", "dataView")) # hit the download button!

    return(p)
  }


  if (map_level == 2){
    #list_select <- which(lista_ref %in% data$CC_2)
    print(head(data))

    col_ref <- which(colnames(df_polu_mes) %in% paste0("ID_",data$CC_2))

    line_data <- df_polu_mes[,c(3,col_ref)] %>%
      pivot_longer(cols = -Date, names_to = "Muni", values_to = "PM25")

    for (i in 1:length(unique(line_data$Muni))){
      line_data[line_data$Muni == unique(line_data$Muni)[i],"Muni"] <- df_muni[df_muni$code_muni == substr(unique(line_data$Muni)[i],4,10), "Cidade"]
    }

    line_data$PM25 <- round(line_data$PM25, digits = 1)
    ine_data <- line_data[order(line_data$Muni),]

    line_data %>%
      group_by(Muni) %>%
      e_charts(Date) %>%

      e_line(PM25, symbol = 'circle', symbolSyze = 1) %>%
      e_tooltip(trigger = "axis") %>%
      e_axis_labels(x = "Ano", y = "Média mensal MP2.5") %>%
      e_x_axis(
        nameLocation = "center", nameGap = 30,
        min = as.Date("2010-01-01"),  # Set the minimum date to January 1, 2010
        max = as.Date("2022-12-31"),  # Set the maximum date to December 31, 2022
      ) %>%
      e_y_axis(nameLocation = "center", nameGap = 30) %>%
      e_tooltip(
        formatter = htmlwidgets::JS('
      function(params) {
        var date = new Date(params.value[0]);
        var monthNames = [
          "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
          "Jul", "Ago", "Set", "Out", "Nov", "Dez"
        ];
        var formattedDate = monthNames[date.getMonth()] + " " + date.getFullYear();
        return (
          "<strong>" + params.seriesName + "</strong><br />" +
          "Média mensal de MP2.5 em " + formattedDate + ": " + params.value[1]
        );
      }
    ')) %>%
      e_mark_line(data = list(yAxis = 5), title = "Limite OMS", symbol = "none", tooltip = list(show = FALSE),
                  lineStyle = list(color = "lightgray"),  # Cor da linha padrão
                  emphasis = list(
                    lineStyle = list(color = "#800080")  # Cor da linha ao passar o mouse
                  )) %>%
      e_mark_line(data = list(yAxis = 10), title = "2x") %>%
      e_mark_line(data = list(yAxis = 15), title = "3x") %>%
      e_mark_line(data = list(yAxis = 25), title = "5x") %>%
      e_mark_line(data = list(yAxis = 35), title = "7x") %>%
      e_mark_line(data = list(yAxis = 50), title = "10x") %>%
      e_toolbox_feature(feature = c("saveAsImage", "dataView")) # hit the download button!
  }
}
