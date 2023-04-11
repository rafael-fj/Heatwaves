function(input, output, session) {

  # Leafdown Map
  output$mapbr <- renderLeaflet({
    leaflet(data_muni) %>%
      addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addLegend(
        pal = colorNumeric("YlOrRd", data$Heatwaves, na.color = NA),
        values = data$Heatwaves,
        title = "Espectativa de Vida",
        labFormat = labelFormat(suffix = "anos"),
        opacity = 1,
        layerId = "legend") %>%
      addPolygons(stroke = F, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  label = ~ paste0("Espectativa de vida em ", name_muni, ": ", Heatwaves),
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
