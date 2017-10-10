Sys.setlocale(category = "LC_ALL", locale = "sv_SE.UTF-8")
ui <- dashboardPage(dashboardHeader(title = "Patientsvar",
                                    tags$li(a(href = "https://www.ibm.com/ibm/responsibility/",
                                              img(src="IBM-logo.png",
                                                  title = "Corporate Social Responsibillity", height = "20px", width = "50px")
                                    ),
                                    class="dropdown")
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dataset - Översikt", icon=icon("database"), tabName = "do"),
                        menuItem("Tekniska Hjälpmedel", icon=icon("mobile"), tabName = "th"),
                        menuItem("Psykosocialt Stöd", icon=icon("users"), tabName = "pss"),
                        menuItem("Kontaktvägar", icon=icon("info"), tabName = "kv"),
                        hr(),
                        menuItem("Jämförelser", icon=icon("bar-chart"), tabName = "jmf"),
                        selectInput("qst", "Fokusgrupp: Fråga", quest_cols),
                        selectInput(inputId="ans", label="Fokusgrupp: Svar", choices=""),
                        selectInput("cmp", "Jämförelse-fråga", quest_cols)
                      )
                    ),
                    dashboardBody(tabItems(
                        tabItem("do",
                                fluidRow(
                                  box(plotlyOutput("plotly_age"), width=6, title="Ålder"),
                                  box(plotlyOutput("plotly_landst"), width=6, title="Landsting")
                                ),
                                fluidRow(
                                  box(plotlyOutput("plotly_stod"), width=6, title="Stöd utanför vård"),
                                  box(plotlyOutput("plotly_kontakt"), width=6, title="Kontakt med andra med T1D")
                                ),
                                fluidRow(
                                  box(plotlyOutput("plotly_fortroende"), width=6, title="Förtroende för vården"),
                                  box(plotlyOutput("plotly_bemotande"), width=6, title="Bemötande")
                                )
                        ),
                        tabItem("th",
                                fluidRow()
                        ),
                        tabItem("pss",
                                fluidRow()
                        ),
                        tabItem("kv",
                                fluidRow()
                        ),
                        tabItem("jmf",
                                fluidRow(
                                  valueBoxOutput("FocusGrp_info", width = 6),
                                  valueBoxOutput("TotalGrp_info", width = 6)
                                ),
                                fluidRow(
                                  box(plotlyOutput("plotly_vs_age"), width=6, title="Ålder"),
                                  box(plotlyOutput("plotly_vs_landst"), width=6, title="Landsting")
                                ),
                                fluidRow(
                                  box(plotlyOutput("plotly_vs_informerad"), width=6, title="Hur informerad är patienten"),
                                  box(plotlyOutput("plotly_vs_fortroende"), width=6, title="Förtroende")
                                ),
                                fluidRow(
                                  box(plotlyOutput("plotly_vs_kontakt"), width=6, title="Kontakt med andra med diabetes"),
                                  box(plotlyOutput("plotly_vs_stod"), width=6, title="Stöd utanför vården")
                                ),
                                fluidRow(
                                  box(plotlyOutput("plotly_vs_question"), width=6, title="vs. Fråga 2")
                                )
                        )
                      )
                    )
)