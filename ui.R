ui <- dashboardPage(
  dashboardHeader(title = "Plataforma Heatwave"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(
                  width = 6,
                  box(width = 14, height = 645,
                    useShinyjs(),
                    actionButton("drill_down", "Municípios"),
                    actionButton("drill_up", "Estados"),
                    leafletOutput("map", height = 620),
                    p())),
                column(
                  width = 6,
                  box(
                    plotOutput("line_plot", height = 290),
                    width = 10,
                    collapsible = FALSE,
                    headerBorder = FALSE
                  ),
                  box(
                    plotOutput("lolli_plot", height = 290),
                    width = 10,
                    collapsible = FALSE,
                    headerBorder = FALSE
                  )
                )
              )),
    tabItem(tabName = "data",
              datatable(df_muni,
                        extensions = c("Buttons", "Scroller"),
                        options = list(
                          dom = 'Bfrtip',
                          buttons =
                            list('copy', 'print', list(
                              extend = 'collection',
                              buttons = c('csv', 'excel', 'pdf'),
                              text = 'Download'
                            )),
                          deferRender = TRUE,
                          scrollY = 360,
                          scroller = TRUE
                        )
              )
      )
    )
  )
)
