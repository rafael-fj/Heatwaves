mod_heatwaves <- function(input, output, session) {

  output$mapbr <- renderLeaflet({
    leaflet(data_muni) %>%
      addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addLegend(
        pal = colorNumeric("YlOrRd", data_muni$Heatwaves, na.color = NA),
        values = data_muni$Heatwaves,
        title = "Espectativa de Vida",
        labFormat = labelFormat(suffix = "anos"),
        opacity = 1,
        layerId = "legend") %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  label = ~ paste0("Espectativa de vida em ", name_muni, ": ", Heatwaves),
                  fillColor = ~colorQuantile("YlOrRd", Heatwaves)(Heatwaves),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
  })
}
