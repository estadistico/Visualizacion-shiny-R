
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

library(haven)
Casen_2017 <- read_dta("Casen2017.dta")
library(dplyr)
casen2017 <- select(Casen_2017,region,zona,tot_hog,sexo,edad,ytot,r17a,esc,pobreza)
casen2017_2 <- casen2017
casen2017_2$sexo<-as.factor(casen2017_2$sexo)
#library(haven) # cambiar direccion de los datos
#casen2017 <-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiF7ZVn-vyZ7qrAYqLdUf6N6rWrGPLm2F59BnyM7ROb3ILDR4OQ-H3I9SIOEMXNQ/pub?gid=1397027065&single=true&output=csv")

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  
  output$tab_salida <- renderTable(
    {
     
      
      casen2017$sexo <- as.factor(casen2017$sexo)
      levels(casen2017$sexo) <- c("Masculino", "Fenenino")
      
      tab_ingreso_sexo= casen2017 %>% group_by(sexo) %>% summarise( min=min(ytot,na.rm=TRUE),
                                                                    p25=quantile(ytot, 0.25,na.rm=TRUE),
                                                                    media=mean(ytot,na.rm=TRUE),
                                                                    p75=quantile(ytot, 0.75,na.rm=TRUE),
                                                                    sdt=sd(ytot,na.rm=TRUE),
                                                                    maximo=max(ytot,na.rm=TRUE)
      )
      tab_ingreso_sexo
      
      
    }
  )
  #################################################################################
  output$zona_ingresotab <- renderTable(
    {
      
      
      casen2017$zona <- as.factor(casen2017$zona)
      levels(casen2017$zona) <- c("Urbano", "Rural")
      
      tab_ingreso_zona = casen2017 %>% group_by(zona) %>% summarise( min=min(ytot,na.rm=TRUE),
                                                                    p25=quantile(ytot, 0.25,na.rm=TRUE),
                                                                    media=mean(ytot,na.rm=TRUE),
                                                                    p75=quantile(ytot, 0.75,na.rm=TRUE),
                                                                    sdt=sd(ytot,na.rm=TRUE),
                                                                    maximo=max(ytot,na.rm=TRUE)
      )
      tab_ingreso_zona
      
      
    }
  )
  ################################################################################
  output$sexo_escolaridadtab <- renderTable(
    {
      
      
      casen2017$sexo <- as.factor(casen2017$sexo)
      levels(casen2017$sexo) <- c("Masculino", "Fenenino")
      
      tab_escolaridad_sexo = casen2017 %>% group_by(sexo) %>% summarise( min=min(esc,na.rm=TRUE),
                                                                     p25=quantile(esc, 0.25,na.rm=TRUE),
                                                                     media=mean(esc,na.rm=TRUE),
                                                                     p75=quantile(esc, 0.75,na.rm=TRUE),
                                                                     sdt=sd(esc,na.rm=TRUE),
                                                                     maximo=max(esc,na.rm=TRUE)
      )
      tab_escolaridad_sexo
      
      
    }
  )
  ###############################################################################
  output$plot1 <- renderPlot({
  
    
    casen2017$sexo <- as.factor(casen2017$sexo)
    levels(casen2017$sexo) <- c("Masculino", "Fenenino")
    ggplot(casen2017, aes(x=sexo, fill=sexo)) + geom_bar(position="dodge",colour="black") +
      scale_fill_manual(
        values = c("Masculino" = "#0710EF",
                   "Fenenino" = "#E809C9")
      )+
      geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9),
                vjust=-0.1, size=3.0) + ggtitle('Distribucion por sexo')
    
  })
  ##########################################################################
  output$plot2 <- renderPlot({
    
    casen2017$zona <- as.factor(casen2017$zona)
    levels(casen2017$zona) <- c("Urbano", "Rural")
    
    ggplot(casen2017, aes(x=zona)) + geom_bar(position="dodge",colour="black") +
      geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9),
                vjust=-0.1, size=3.0) + ggtitle('Distribucion por zona geografica')
    
  })
  ###########################################
  output$ingre_sex <- renderPlot({
    
    casen2017$sexo <- as.factor(casen2017$sexo)
    levels(casen2017$sexo) <- c("Masculino", "Fenenino")
    tab_aux<- casen2017 %>%  group_by(sexo) %>% summarise( total=mean(ytot,na.rm = TRUE) ) 
    
    ggplot(tab_aux, aes(x=sexo,y=total, fill=sexo)) + geom_bar(stat = "identity",position="dodge",colour="black") +
      scale_fill_manual(
        values = c("Masculino" = "#0710EF",
                   "Fenenino" = "#E809C9")
      )
    
  })
  ###########################################################################
  output$plot3 <- renderPlot({
    
    
    casen2017$pobreza <- as.factor(casen2017$pobreza)
    levels(casen2017$pobreza) <- c("Pobres extremos", "Pobres no extremos", "No pobres")
    casen2017 <- casen2017[!is.na(casen2017$pobreza),]
    
    ggplot(casen2017, aes(x=pobreza)) + geom_bar(position="dodge",colour="black") +
      geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9),
                vjust=-0.1, size=3.0) + ggtitle('Distribucion por Pobreza')
    
  })
  ############################################################################
  output$pobres_zona <- renderTable(
    {
      
      
      casen2017$pobreza <- as.factor(casen2017$pobreza)
      levels(casen2017$pobreza) <- c("Pobres extremos", "Pobres no extremos", "No pobres")
      casen2017 <- casen2017[!is.na(casen2017$pobreza),]
      
      casen2017$zona <- as.factor(casen2017$zona)
      levels(casen2017$zona) <- c("Urbano", "Rural")
      
      tab_pobres_sexo =  table(casen2017$pobreza,casen2017$zona)
      tab_pobres_sexo
      
      
    })
  ############################################################################
  output$plot4 <- renderPlot({
    
    casen2017 <- casen2017[!is.na(casen2017$ytot),]
    casen2017 <- casen2017[!is.na(casen2017$esc),]
    
    ggplot(casen2017, aes(y=ytot,x=esc)) + geom_point()+
      geom_smooth(method = "lm", se = FALSE)+
      xlab('Años de escolaridad') + ylab('Ingresos') + ggtitle('Años de escolaridad vs Ingresos')
    
    
  })
  ###########################################################################
  output$nacompleto <- renderInfoBox({
    
    
    infoBox(
      "registro completo", sum(complete.cases(casen2017)), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  
  output$vardb <- renderInfoBox({
    
    infoBox(
      "Num variable", ncol(casen2017), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  
  output$totaldb <- renderInfoBox({
    
    
    
    infoBox(
      "Total observaciones:", nrow(casen2017),icon = icon("credit-card"),fill = TRUE
    )
    
    
  })
  
} # fin de todo server