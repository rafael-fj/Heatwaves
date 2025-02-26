ui <- dashboardPage(
  header = dashboardHeader(
    disable = T,
    title = customSidebar(
                  title = "Plataforma Heatwave",
                  bgColor = azul_heat,
                  href = "https://portal.fiocruz.br/",
                  image = "logo.png")
    ),
  dashboardSidebar(
    collapsed = T,
    #class = "custom-sidebar",  # Adicione a classe personalizada aqui
    sidebarMenu(id = "tab",
      #menuItem("Ondas de Calor", tabName = "dashboard", icon = icon("map-location-dot")),
      menuItem("Ondas de Calor", tabName = "dashboard", icon = icon("sun")),
      menuItem("Consulta Heatwave", tabName = "dashboard2", icon = icon("chart-simple")),
      menuItem("Poluição", tabName = "poluicao", icon = icon("smog")),
      menuItem("Dados", tabName = "data", icon = icon("database"))
  ),
 conditionalPanel(
   condition = "input.tab == 'dashboard'",
   sidebarUserPanel(
     fluidRow(
       tags$head(
         tags$style(HTML("
            .dt-buttons {
              display: flex;
              justify-content: center;
            }
            .dataTables_filter input {
              height: 20px;
              font-size: 12px;
            }
          "))
       ),
       box(
         style = "padding: 0px !important;",
         rightBorder = F,
         collapsible = F,
         headerBorder = FALSE,
         width = 12,
         DT::dataTableOutput("mytable", height = "50vh")
         )
       )
     )
   ),
 conditionalPanel(
   condition = "input.tab == 'dashboard2'",
   div(
     class = "sidebar-custom",
     sidebarUserPanel(
       fluidRow(
         tags$head(
           tags$style(HTML("
            /* Cor do texto dos labels */
            .control-label {
              color: black;
            }

            /* Ajuste o tamanho dos dropdowns ao tamanho da sidebar */
            .sidebar-custom .selectize-input, .sidebar-custom .action-button {
              width: 80% !important;
            }
          "))
         ),
         fluidRow(
           selectInput("dropdown1", "Estado", choices = df_uf$Estado)
         ),
         fluidRow(
           selectInput("dropdown2", "Municipio", choices = c("Option A", "Option B"))
         ),
         fluidRow(
           selectInput("dropdown3", "Ano", choices = c(1990:2021))
         ),
         fluidRow(
           style = "display: flex; justify-content: center; align-items: center; flex-basis: 100%;",
           actionButton("buscar", "Buscar")
         )
       )
     )
   )
 )
 ),
  dashboardBody(
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$head(
      tags$style(HTML("
        .sidebar-menu .active > a {
          background-color: #ffffff !important;
        }
        .selectize-input, .action-button {
            width: 49% !important;
          }
      "))
      ),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(
                  width = 6,
                  height = "100vh",
                  box(
                    width = 12,
                    height = "100%",
                    collapsible = FALSE,
                    closable = FALSE,
                    headerBorder = FALSE,
                    style = "position: absolute; top: 0; padding: 0;",
                    #style = "display: flex; flex-direction: column; justify-content: flex-start; align-items: stretch; height: 100%;",
                    #style = "position: absolute; top: 0; padding: 0;",
                    #style = "position: absolute; top: 0; margin: 0; padding: 0;",
                    #actionButton("drill_down", "Municípios", style = "position: absolute; top: 2px;"),  # Adjusted position
                    uiOutput("drill_down_calor"),
                    uiOutput("drill_up_calor"),
                    tags$head(
                      tags$style(
                        HTML("
                            #map {
                              margin-top: 3vh !important;
                            }
                          ")
                                          )
                    ),
                    leafletOutput("map", height = "90vh"),
                    div(
                      class = "info_tooltip",
                      tags$head(
                        tags$style(HTML("
                                        /* Ajuste o tamanho dos dropdowns ao tamanho da sidebar */
                                        .info_tooltip .selectize-input, .info_tooltip .action-button {
                                          width: 100% !important;
                                        }"))
                      ),
                      style = "position: absolute; top: 0; right: 0;",
                      createInfo("info_01",
                                 "<strong>Visualização:</strong><br>
                                 Selecione uma região e clique em município/estado<br>
                                 <strong>Ondas de calor:</strong><br>
                                 Dados referentes a 1990-2021<br>
                                 Percentil 95")
                    ))),

                column(
                  width = 6,
                  height = "100vh",
                  #bsTooltip("frequencia", "1", placement = "top"),
                  box(
                    style = "top: 0; padding: 0;",
                    div(
                      class = "info_tooltip",
                      style = "position: absolute; top: 0; right: 0;",
                      createInfo("info_02",
                                 "<strong>Colunas:</strong> Ondas de calor/ano.<br>
                                  <strong>Linha:</strong> Cumulativo desde 1990.<br>
                                  Clique em uma cidade/estado para selecionar/descelecionar.")

                    ),
                    echarts4rOutput("frequencia", height = "43.5vh"),
                    width = 12,
                    collapsible = FALSE,
                    closable = FALSE,
                    headerBorder = FALSE
                  ),
                  box(
                    style = "top: 0; padding: 0; text-align: center;",
                    div(
                      class = "info_tooltip",
                      style = "position: absolute; top: 0; right: 0;",
                      createInfo("info_03",
                                 "<strong>Visualização:</strong> Passe o mouse nos círculos para detalhes.<br>
                                 Clique em uma cidade/estado para selecionar/descelecionar.")
                    ),
                    echarts4rOutput("duracao", height = "43.5vh"),
                    width = 12,
                    collapsible = FALSE,
                    closable = FALSE,
                    headerBorder = FALSE
                    )
                  )
                )
              ),
      tabItem(tabName = "poluicao",
              fluidRow(
                column(
                  width = 6,
                  height = "100vh",
                  box(width = 12,
                      height = "100%",
                      collapsible = F,
                      closable = F,
                      headerBorder = FALSE,
                      style = "position: absolute; top: 0; padding: 0;",
                      uiOutput("drill_down_polu"),
                      uiOutput("drill_up_polu"),
                      div(
                        class = "info_tooltip",
                        style = "position: absolute; top: 0; right: 0;",
                        createInfo("info_11",
                                   "<strong>Visualização:</strong><br>
                                    Selecione uma região e clique em município/estado<br><br>
                                    <strong>PM2.5:</strong> Material particulado (poluição) de diâmetro 2.5 microns<br>
                                    Dados referentes às regiões Norte e Centro-Oeste de 2010-2021")

                      ),
                      p(),
                      tags$head(
                        tags$style(
                          HTML("
                            #map_polu {
                              margin-top: 3vh !important;
                            }
                          ")
                        )
                      ),
                      leafletOutput("map_polu", height = "90vh")
                      )),

                column(
                  width = 6,
                  box(
                    style = "top: 0; padding: 0;",
                    div(
                      class = "info_tooltip",
                      style = "position: absolute; top: 0; right: 0;",
                      createInfo("info_12",
                                 "<strong>Visualização:</strong><br>
                                  Passe o mouse nos pontos para detalhes.<br>
                                  Selecione/desmarque estados/municípios no topo para comparar.")

                    ),
                    echarts4rOutput("polu_line", height = "43.5vh"),
                    width = 12,
                    collapsible = FALSE,
                    headerBorder = FALSE
                  ),
                  box(
                    style = "top: 0; padding: 0;",
                    div(
                      class = "info_tooltip",
                      style = "position: absolute; top: 0; right: 0;",
                      createInfo("info_13",
                                 "<strong>Relação entre níveis de poluição e intensidade de onda de calor.</strong><br><br>
                                  <strong>Visualização:</strong><br>
                                  Cada ponto representa uma onda de calor<br>
                                  Média de PM2.5 referente ao período da onda de calor<br>
                                  Passe o mouse nos pontos para detalhes.<br>
                                  Selecione/desmarque estados/municípios no topo para comparar.")

                    ),
                    echarts4rOutput("polu_heat", height = "43.5vh"),
                    width = 12,
                    collapsible = FALSE,
                    headerBorder = FALSE
                    )
                  )
                )
              ),
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
              ),
      tabItem(
        tabName = "dashboard2",
        fluidRow(
          column(
            width = 6,
            box(
              div(
                class = "info_tooltip",
                style = "position: absolute; top: 0; right: 0;",
                createInfo("info_climatologia",
                           "<strong>Climatologia:</strong> Média histórica de temperatura.<br>
                            <strong>Limite:</strong> Temperatura limite com percentil 95 para classificar uma onda de calor")

              ),
              plotOutput("line_plot", height = "35vh")%>% withSpinner(color=loading_color),
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE
            ),
            box(
              div(
                class = "info_tooltip",
                style = "position: absolute; top: 0; right: 0;",
                createInfo("info_lolli",
                           "<strong>Pico:</strong> Dia do auge de intensidade da onda de calor<br>
                           <strong>Intensidade:</strong> Temperatura (ºC) acima do esperado<br>
                           Dados referentes a 2010-2021")

              ),
              plotOutput("lolli_plot", height = "35vh")%>% withSpinner(color=loading_color),
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE
            )
          ),
          column(
            width = 6,
            box(
              div(
                class = "info_tooltip",
                style = "position: absolute; top: 0; right: 0;",
                createInfo("info_calendario",
                           "<strong>Visualização:</strong> Representação visual do avanço de temperatura na região ao longo do ano")

              ),
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              plotOutput("calendar", height = "81vh")%>% withSpinner(color=loading_color)
              )
            )
        )
    )
  )
)
)
