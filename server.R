function(input, output, session) {

  # Leafdown DEMO
  my_leafdown <- Leafdown$new(spdfs_list, "map", input)
  rv <- reactiveValues()
  rv$update_leafdown <- 0
  rv$clicked_polygon <- NULL

  observeEvent(input$map_shape_click, {
    print("Polygon clicked")
    print(paste0("Clicked polygon ID: ", input$map_shape_click$id))

    if (my_leafdown$curr_map_level == 1) {
      rv$clicked_polygon <- input$map_shape_click$id
    }
  })

  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    rv$update_leafdown <- rv$update_leafdown + 1
  })

  observeEvent(input$drill_up, {
    my_leafdown$drill_up()
    rv$update_leafdown <- rv$update_leafdown + 1
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

    print(head(data))
    print(paste0("map level = ", curr_map_level))

    my_leafdown$add_data(data)
    labels <- create_labels(data, curr_map_level)
    my_leafdown$draw_leafdown(
      fillColor = ~ colorNumeric("YlOrRd", Heatwaves)(Heatwaves),
      weight = 2, fillOpacity = 0.8, color = "grey", label = labels,
      highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)
    ) %>%
      my_leafdown$keep_zoom(input) %>%
      addLegend("topright",
                pal = colorNumeric("YlOrRd", data$Heatwaves),
                values = data$Heatwaves,
                title = "Ondas de Calor",
                opacity = 1)
  })


  # Leafdown Map
  output$mapbr <- renderLeaflet({
    leaflet(data_muni) %>%
      addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addLegend(
        pal = colorNumeric("YlOrRd", data$Heatwaves, na.color = NA),
        values = data$Heatwaves,
        title = "Ondas de Calor nos Últimos 20 anos",
        labFormat = labelFormat(suffix = "anos"),
        opacity = 1,
        layerId = "legend") %>%
      addPolygons(stroke = F, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  label = ~ paste0("Ondas de Calor em ", name_muni, ": ", Heatwaves),
                  fillColor = ~colorQuantile("YlOrRd", Heatwaves)(Heatwaves),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
  })

  # Plots
  output$line_plot <- renderPlot({
    plot_dur(mhw, spread = 180, metric = "intensity_max",
             start_date = "1982-01-01", end_date = "2014-12-31")
    #event_line(mhw, spread = 180, metric = "intensity_max", start_date = "1982-01-01", end_date = "2014-12-31")
  })

  output$lolli_plot <- renderPlot({
    plot_int(mhw, metric = "intensity_max")
    #lolli_plot(mhw, metric = "intensity_max")
  })
}
