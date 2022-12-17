library(shiny)
library(shinydashboard)
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Juan Hiriarte & Patricio Bonta"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Resumen Datos", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Ingreso por sexo", tabName = "widgets", icon = icon("th")),
                        menuItem("Ingreso por zona geografica", tabName = "idzona", icon = icon("dashboard")),
                        menuItem("escolaridad por sexo", tabName = "idesc", icon = icon("dashboard")),
                        menuItem("distribucion sexo", tabName = "idplot", icon = icon("dashboard")),
                        menuItem("distribucion zona", tabName = "idplot2", icon = icon("dashboard")),
                        menuItem("distribucion Pobreza", tabName = "idplot3", icon = icon("dashboard")),
                        menuItem("distribucion Pobreza por zona", tabName = "idPyZ", icon = icon("dashboard")),
                        menuItem("Correlacion ingresos y escolaridad", tabName = "idplot4", icon = icon("dashboard"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "dashboard",
                                
                                fluidRow(
                                  infoBoxOutput("totaldb"),
                                  infoBoxOutput("vardb"),
                                  infoBoxOutput("nacompleto")
                                ),
                                fluidRow(
                                  #infoBoxOutput("persona2"),
                                  #infoBoxOutput("progressBox22"),
                                  #infoBoxOutput("approvalBox22")
                                )
                                
                        ),
                        
                        
                        # Second tab content
                        tabItem(tabName = "widgets",
                                
                                fluidRow(
                                  column(width = 4,""
                                        ),
                                  
                                  column(width = 8,
                                  
                                box(width = 10,
                                  title = "Ingreso por sexo", background = "maroon", solidHeader = TRUE,
                                  tableOutput("tab_salida")
                                ) 
                                
                                  ),
                                
                                ), #fin fila
                                
                                fluidRow(
                                box(
                                  title = "grafico de Ingreso por sexo", background = "blue",
                                  plotOutput("ingre_sex")
                                  
                                ),
                                box(
                                  title = "Source", background = "black",
                                  "
                                  casen2017$sexo <- as.factor(casen2017$sexo)
    levels(casen2017$sexo) <- c(\"Masculino\", \"Fenenino\")
    tab_aux<- casen2017 %>%  group_by(sexo) %>% summarise( total=mean(ytot,na.rm = TRUE) ) 
    
    ggplot(tab_aux, aes(x=sexo,y=total, fill=sexo)) + geom_bar(stat = \"identity\",position=\"dodge\",colour=\"black\") +
      scale_fill_manual(
        values = c(\"Masculino\" = \"#0710EF\",
                                  \"Fenenino\" = \"#E809C9\")
                                )
                                  "
                                  
                                )
                                ) #fin fila
                                
                                
                                
                        ),
                        
                        tabItem(tabName = "idzona",
                                
                             #   box(title = "Ingresos por zona geografica",
                                    tableOutput("zona_ingresotab")
                              #      ),
                        ),#fin id zona
                             tabItem(tabName = "idesc",
                                     
                                     #   box(title = "Ingresos por zona geografica",
                                     tableOutput("sexo_escolaridadtab")
                                     ), # fin idesc
                        tabItem(tabName = "idplot",
                                
                                #   box(title = "Distribucion de sexo",
                                tabBox(
                                  title = "Distribucion del sexo",
                                  # The id lets us use input$tabset1 on the server to find the current tab
                                  id = "tabset1", height = "250px",width = "70%",
                                  tabPanel("grafico", plotOutput("plot1")
                                           ),
                                  tabPanel("Source", 
                                           tags$div(
                                             
                                             "casen2017$sexo <- as.factor(casen2017$sexo)", tags$br(),
                                             "levels(casen2017$sexo) <- c(\"Masculino\", \"Fenenino\")", tags$br(),
                                             "ggplot(casen2017, aes(x=sexo, fill=sexo)) + geom_bar(position=\"dodge\",colour=\"black\") +",tags$br(),
                                               "scale_fill_manual(values = c(\"Masculino\" = \"#0710EF\",  \"Fenenino\" = \"#E809C9\")+",tags$br(),
                                               "geom_text(aes(label=..count..), stat=\'count\',position=position_dodge(0.9)," ,tags$br(),
                                                         "vjust=-0.1, size=3.0) + ggtitle(\'Distribucion por sexo\')",tags$br(),
                                              )
                                           )
                                )
                                
                                
                        ), # fin idplot
                        tabItem(tabName = "idplot2",
                                
                                tabBox(
                                  title = "Distribucion zona",
                                  # The id lets us use input$tabset1 on the server to find the current tab
                                  id = "tabset1", height = "250px",width = "70%",
                                  tabPanel("grafico", plotOutput("plot2")
                                  ),
                                  tabPanel("Source", 
                                           tags$div(
                                             
                                             "casen2017$sexo <- as.factor(casen2017$sexo)", tags$br(),
                                             "levels(casen2017$zona) <- c(\"Urbano\", \"Rural\")", tags$br(),
                                             "ggplot(casen2017, aes(x=zona)) + geom_bar(position=\"dodge\",colour=\"black\") +",tags$br(),
                                             "geom_text(aes(label=..count..), stat=\'count\',position=position_dodge(0.9)," ,tags$br(),
                                             "vjust=-0.1, size=3.0) + ggtitle(\'Distribucion por zona geografica\')",tags$br(),
                                           )
                                  )
                                )
                                
                                
                                
                        ),  # fin idplot2       
                        tabItem(tabName = "idplot3",
                                
                                plotOutput("plot3")
                        ),  # fin idplot3
                        
                        tabItem(tabName = "idPyZ",
                                
                                tableOutput("pobres_zona")
                        ), # fin idPyZ
                        
                        tabItem(tabName = "idplot4",
                                
                                plotOutput("plot4")
                        )  # fin idplot3
                        
                      ) # fin tab itemsss
                    ) # body dashoar fin

)## fin todo


