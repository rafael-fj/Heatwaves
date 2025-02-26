create_mytable <- function(data, sel_data, map_level) {
  #print(head(data))
  #print(head(sel_data))
  #print(map_level)
  #map_level <- ifelse("NAME_2" %in% names(data), 2, 1)
  #map_level <- drill
  #print(map_level)

  if (map_level == 1) {
    curr_gid <- "NAME_1"
    data_sub <- data[,c(4,12)]
    colnames(data_sub) <- c("Estado", "Ondas")
  } else {
    curr_gid <- "NAME_2"
    data_sub <- data[,c(14,16)]
    colnames(data_sub) <- c("Cidade", "Ondas")
  }

  sel_ids <- which(data[[curr_gid]] %in% sel_data[[curr_gid]])
  #print(sel_ids)
  #print(str(data_sub))
  DT::datatable(data_sub, rownames = F,
                selection = list(selected = sel_ids),
                options = list(
                  dom = 'ft',
                  deferRender = TRUE,
                  scrollY = "55vh",
                  scroller = TRUE,
                  paging = TRUE,
                  bSort = FALSE,
                  rownames = FALSE,
                  language = list(searchPlaceholder = " ", sSearch = "Busca: ")
                ), extensions = list()) %>%
                  DT::formatStyle(columns = names(data_sub),
                                  fontSize = '80%',
                                  lineHeight = '80%'
                                  )

}
