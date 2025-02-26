function(input, output, session) {
  observeEvent(input$tab, {
    print(paste0("Selected Tab: ", input$tab))
  })
  output$line_plot <- NULL
  output$lolli_plot <- NULL
  output$calendar <- NULL

  ########################### Dashboard específico - Botão ###########################
  observeEvent(input$buscar, {
    selected_municipio <- as.character(input$dropdown2)
    selected_ano <- as.character(input$dropdown3)
    #print(selected_municipio)
    #print(selected_ano)
    #print(selected_ano, selected_municipio)
    data_especifico <- get_muni(selected_municipio, selected_ano)
    #write.csv(data_especifico, "espec.csv")
    #print(min(data_especifico$t))
    #print(max(data_especifico$t))
    ts <- ts2clm(data_especifico, climatologyPeriod = c(min(data_especifico$t), max(data_especifico$t)))
    #print("ts foi")
    mhw <- detect_event(ts)
    #print("mhw foi")
    # Plots Advanced tab
    t_start <- as.Date(paste0(selected_ano,"-01-01"))
    t_end <- as.Date(paste0(selected_ano,"-12-31"))
    #print(t_start)
    #print(t_end)
    #print(unique(year(mhw$event$date_start)))
    output$line_plot <- renderPlot({
      plot_heat(mhw, spread = 180, metric = "intensity_max",
                start_date = t_start, end_date = t_end)
      #event_line(mhw, spread = 180, metric = "intensity_max", start_date = "1982-01-01", end_date = "2014-12-31")
    })

    #muni_id <-df_muni$code_muni[which(df_muni$Cidade == selected_municipio)]  # Read from input
    muni_index <- which(lista_ref == df_muni$code_muni[which(df_muni$Cidade == selected_municipio)])
    output$lolli_plot <- renderPlot({
      #plot_lolli(mhw, metric = "intensity_max")
      #lolli_plot(mhw, metric = "intensity_max")
      plot_lolli(muni_index)
    })

    subdate <- data_especifico %>%
      filter(year(t) == selected_ano)

    #my_palette <- colorRampPalette(c("white", "red"))
    print("head subdate")
    print(head(subdate))
    output$calendar <- renderPlot({
      calendR(year = selected_ano,
              special.days = subdate$temp,
              gradient = T,
              #low.col = "white",
              special.col = paletteer_c("ggthemes::Classic Orange-White-Blue Light", 30,direction = -1),
              #special.col = my_palette(1:20),  # Gradiente de branco para vermelho para valores entre 20 e 40
              orientation = "p",
              legend.pos = "bottom")
    })
  })
  ########################### Identificador de aba ###########################
  # Set tab variable
  answer <- reactive({
    if (input$tab == 'dashboard' && !input$sidebarCollapsed){
      answer <- 1
    } else {
      answer <- 2
    }
    return(answer)
  })
  ########################### Dashboard inicial - Plot do mapa interativo ###########################
  map_level <- 1
  drill_status <- reactiveVal(1)

  my_leafdown <- Leafdown$new(spdfs_list, "map", input)
  rv <- reactiveValues()
  rv$update_leafdown <- 0
  rv$clicked_polygon <- NULL

  observeEvent(input$map_shape_click, {
    #print("Polygon clicked")
    #print(paste0("Clicked polygon ID: ", input$map_shape_click$id))
    map_level <- my_leafdown$curr_map_level
    #print(map_level)
    #print(head(my_leafdown$curr_data))
    #print(head(my_leafdown$curr_sel_data()))
    #print(paste0("Selected Data: ", my_leafdown$curr_sel_data()[,12]))
    #print(paste0("Selected Tab: ", input$tab))

    if (my_leafdown$curr_map_level == 1) {
      rv$clicked_polygon <- input$map_shape_click$id
    }
  })

  observeEvent(input$drill_down, {
    #print(drill_status)
    if (drill_status() == 1){
      my_leafdown$drill_down()
      rv$update_leafdown <- rv$update_leafdown + 1
      drill_status(2)
    }
  })

  observeEvent(input$drill_up, {
    if (drill_status() == 2){
      my_leafdown$drill_up()
      rv$update_leafdown <- rv$update_leafdown + 1
      drill_status(1)
    }

  })

  output$map <- renderLeaflet({
    req(rv$update_leafdown)
    meta_data <- my_leafdown$curr_data
    curr_map_level <- my_leafdown$curr_map_level
    if (curr_map_level == 1) {
      data <- meta_data %>%
        left_join(df_uf, by = c("NAME_1" = "Estado"))
    } else {
      data <- meta_data %>%
        left_join(df_muni, by = c("CC_2" = "code_muni"))
    }

    #print(head(data))
    #print(paste0("map level = ", curr_map_level))

    my_leafdown$add_data(data)
    labels <- create_labels(data, curr_map_level)
    my_leafdown$draw_leafdown(
      fillColor = ~ colorNumeric("YlOrRd", Heatwaves)(Heatwaves),
      weight = 2, fillOpacity = 0.8, color = "grey", label = labels,
      highlight = highlightOptions(
        weight = 5, color = "purple", fillOpacity = 0.7
      )
    ) %>%
      my_leafdown$keep_zoom(input) %>%
      addLegend("topright",
                pal = colorNumeric("YlOrRd", data$Heatwaves),
                values = data$Heatwaves,
                title = "Ondas de Calor",
                opacity = 1)
  })


  ########################### Poluição - Plot do mapa interativo ###########################
  drill_status2 <- reactiveVal(1)
  my_leafdown2 <- Leafdown$new(spdfs_list_polu, "map_polu", input)
  rv2 <- reactiveValues()
  rv2$update_leafdown <- 0
  rv2$clicked_polygon <- NULL

  observeEvent(input$map_shape_click, {
    #print("Polygon clicked")
    #print(paste0("Clicked polygon ID: ", input$map_shape_click$id))
    map_level2 <- my_leafdown2$curr_map_level
    #print(map_level2)
    #print(map_level)
    #print(head(my_leafdown$curr_data))
    #print(head(my_leafdown$curr_sel_data()))
    #print(paste0("Selected Data: ", my_leafdown$curr_sel_data()[,12]))
    #print(paste0("Selected Tab: ", input$tab))

    if (my_leafdown2$curr_map_level == 1) {
      rv2$clicked_polygon <- input$map_shape_click$id
    }
  })


  observeEvent(input$drill_down2, {
    if (drill_status2() == 1){
      my_leafdown2$drill_down()
      rv2$update_leafdown <- rv2$update_leafdown + 1
      drill_status2(2)
    }
  })

  observeEvent(input$drill_up2, {
    if (drill_status2() == 2){
      my_leafdown2$drill_up()
      rv2$update_leafdown <- rv2$update_leafdown + 1
      drill_status2(1)
    }
  })

  output$map_polu <- renderLeaflet({
    req(rv2$update_leafdown)
    meta_data <- my_leafdown2$curr_data
    curr_map_level <- my_leafdown2$curr_map_level

    print(paste0("curmapllvl ", curr_map_level))

    if (curr_map_level == 1) {
      data2 <- meta_data %>%
        left_join(df_uf_polu, by = c("NAME_1" = "Estado"))
    } else {
      data2 <- meta_data %>%
        left_join(df_muni_polu, by = c("CC_2" = "code_muni"))
    }

    # Remove the NA values from the data$PM_25 variable.
    #data2 <- na.omit(data2)
    #data2 <- data2[is.na(data2$PM25),]
    #print(head(data2))
    #print(data2)
    #print(class(data2$PM25))

    my_leafdown2$add_data(data2)
    labels2 <- create_labels2(data2, curr_map_level)
    #print(labels2)

    #print("labels are ok")

    my_leafdown2$draw_leafdown(
      fillColor = ~ colorNumeric("BuPu", PM25)(PM25),
      weight = 2, fillOpacity = 0.8, color = "grey", label = labels2,
      highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)
    ) %>%
      my_leafdown2$keep_zoom(input) %>%
      addLegend("topright",
                pal = colorNumeric("BuPu", data2$PM25),
                values = data2$PM25,
                title = "PM25",
                opacity = 1)
  })



  ########################### Plots Calor & Poluição###########################
  output$frequencia <- renderEcharts4r({
    create_line_plot(my_leafdown$curr_sel_data())
  })

  output$intensidade <- renderEcharts4r({
    create_bar_chart(my_leafdown$curr_sel_data())
  })

  output$duracao <- renderEcharts4r({
    create_scatter_plot(my_leafdown$curr_sel_data())
  })

  output$polu_heat <- renderEcharts4r({
    create_polu_plot_heat(my_leafdown2$curr_sel_data())
  })

  output$polu_line <- renderEcharts4r({
    create_polu_plot_line(my_leafdown2$curr_sel_data())
  })

  ########################### Dashboard inicial - Tabela lateral com dados ###########################
  output$mytable <- DT::renderDataTable({
    create_mytable(my_leafdown$curr_data, my_leafdown$curr_sel_data(), my_leafdown$curr_map_level)
  })


  ########################### Dashboard inicial - Apresentar dados apenas dos poligonos selecionados no mapa ###########################
  observeEvent(input$mytable_row_last_clicked, {
    sel_row <- input$mytable_row_last_clicked
    #print(paste0("sel_row = ", sel_row))
    #print(paste0("curr_poly_ids = ", my_leafdown$curr_poly_ids))
    sel_shape_id <- my_leafdown$curr_poly_ids[sel_row]
    #print(sel_shape_id)
    my_leafdown$toggle_shape_select(sel_shape_id)
  })

  ########################### Dashboard Específico - Atualizar lista de estados/municipios ###########################
  observe({
    selected_estado <- input$dropdown1
    choices_municipio <- df_muni$Cidade[df_muni$Estado == selected_estado]
    choices_municipio <- sort(choices_municipio)
    updateSelectInput(session, "dropdown2", choices = choices_municipio)
  })

  ########################### Dinamically change color of drill buttons #################

  output$drill_up_calor <- renderUI({
    if (drill_status() == 2) {
      actionButton("drill_up", "Estados", style = "position: absolute; top: 0px; right: 2vh;height: 6vh; width: 16vw; background-color: white; color: #0077f7;")
    } else {
      actionButton("drill_up", "Estados", style = "position: absolute; top: 0px; right: 2vh;height: 6vh; width: 16vw; background-color: #0077f7; color: white;")
    }
  })

  output$drill_down_calor <- renderUI({
    if (drill_status() == 2) {
      actionButton("drill_down", "Municípios", style = "position: absolute; top: 0px; left: 0px;height: 6vh; width: 16vw; background-color: #0077f7; color: white;")
    } else {
      actionButton("drill_down", "Municípios", style = "position: absolute; top: 0px; left: 0px;height: 6vh; width: 16vw; background-color: white; color: #0077f7;")
    }
  })

  output$drill_up_polu <- renderUI({
    if (drill_status2() == 2) {
      actionButton("drill_up2", "Estados", style = "position: absolute; top: 0px; right: 2vh;height: 6vh; width: 16vw; background-color: white; color: #0077f7;")
    } else {
      actionButton("drill_up2", "Estados", style = "position: absolute; top: 0px; right: 2vh;height: 6vh; width: 16vw; background-color: #0077f7; color: white;")
    }
  })

  output$drill_down_polu <- renderUI({
    if (drill_status2() == 2) {
      actionButton("drill_down2", "Municípios", style = "position: absolute; top: 0px; left: 0px;height: 6vh; width: 16vw; background-color: #0077f7; color: white;")
    } else {
      actionButton("drill_down2", "Municípios", style = "position: absolute; top: 0px; left: 0px;height: 6vh; width: 16vw; background-color: white; color: #0077f7;")
    }
  })
}
