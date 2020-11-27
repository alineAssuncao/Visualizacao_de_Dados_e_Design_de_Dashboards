# Projeto 3
# Web Analytics Dashboard


# Pacote
# ***** Atenção - Instale os pacotes que não estiverem instalados no seu ambiente. Alguns pacotes devem ser instalados a partir do Github *****
library(shiny)
library(shinydashboard)
library(devtools)
library(xts)
library(dplyr)
library(googleVis)
library(dygraphs)
# devtools::install_github("hrbrmstr/streamgraph")
# install.packages("streamgraph")
# devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)
#install.packages("treemap")
library(treemap)
# devtools::install_github("jcheng5/bubbles")
library(bubbles)

# Fonte de dados
# Baixe os seus próprios dados da sua conta no Google Analytics. Aqui estão alguns dados de exemplo:
load("data_source.RData")

# Dashboard

ui <- dashboardPage( 
        skin="yellow",
  dashboardHeader(
    title="Web Analytics Dashboard",
    titleWidth = 450
  ),
  dashboardSidebar(
    radioButtons("radio", "Escolha o Período de Visualização:",
                            c("Últimos 3 meses" = "90",
                             "Últimos 6 meses" = "180")),
    sidebarMenu(
      menuItem("Analisar Dados",href="https://www.datascienceacademy.com.br", icon=icon("file-text-o")),
      menuItem("Enviar E-mail",href="https://www.datascienceacademy.com.br", icon=icon("file-text-o"))
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("sessionBox"),
      valueBoxOutput("goalBox"),
      valueBoxOutput("goalCRBox")
    ),
    
    fluidRow(
      box(title="Tendências dos Principais KPI's (Sessões e Transações no Eixo Esquerdo; Taxa de Conversão no Eixo Direito)",
          status="primary",solidHeader = TRUE,dygraphOutput("dygraph"), height=480, width=12),
      box(title="Performance dos Canais (Transações = Tamanho das Bolhas)",status="primary",solidHeader = TRUE,
          htmlOutput("chart2"),width=12,height=450),
      box(title="Tráfego Por dispositivo",status="primary",solidHeader = TRUE,
          streamgraphOutput("streamgraph"), height=500),
      box(title="Dispositivos e Sistemas Operacionais (Sessões = Tamanho dos Retângulos)",status="primary",solidHeader = TRUE,
          plotOutput("tree"), height=500),
      box(title="Resolução de Dispositivos Móveis (Sessões = Tamanho das Bolhas)",status="primary",solidHeader = TRUE,
          bubblesOutput("bubbles",width = "90%"),height=550)
     
      )

)
)

server <- function(input, output) { 
  
  output$sessionBox <- renderValueBox({
    valueBox(
      format(sum(select(subset(ga1,date>=max(date)-as.numeric(input$radio)),sessions)),format="d",big.mark=","), 
             "Sessões", icon = icon("area-chart"), color = "blue")
  })
  
  output$goalBox <- renderValueBox({
    valueBox(
      format(sum(select(subset(ga1,date>=max(date)-as.numeric(input$radio)),goal10Completions)),format="d",big.mark=","), 
             "Transações", icon = icon("shopping-cart"), color = "blue")
  })
  
  output$goalCRBox <- renderValueBox({
    valueBox(
      paste(round(mean(select(subset(ga1,date>=max(date)-as.numeric(input$radio)),goal10ConversionRate)[,1]),2),"%"), "Taxa de Conversão", 
      icon = icon("shopping-cart"), color = "blue")
  })
  
  
  output$chart2<- renderGvis({
    
    ga3Sub<-select(subset(ga3, date>=max(ga3$date)-as.numeric(input$radio)),-date)
    by_channel <- group_by(ga3Sub, channelGrouping)
    ga3Sub<-summarise(by_channel, sum(sessions), mean(pageviewsPerSession),sum(goal10Completions))
    names(ga3Sub)<-c("channelGrouping","sessions","pageviewsPerSession","goal10Completions")
    Bubble <- gvisBubbleChart(ga3Sub, idvar="channelGrouping", 
                              xvar="sessions", yvar="pageviewsPerSession",
                              colorvar="channelGrouping", sizevar="goal10Completions",
                              options=list( 
                                #title="Performance dos Canais (Transações = Tamanho das Bolhas)",
                                vAxis="{title: 'Page Views'}",
                                hAxis="{title: 'Sessões'}",
                                width=990, height=350,
                                legend = T))
    Bubble
  })
    
  output$dygraph <- renderDygraph({
    
    ga1Sub<-subset(ga1, date>=max(ga1$date)-as.numeric(input$radio))
    
    ses <- zoo(ga1Sub$sessions,ga1Sub$date)
    con<-zoo(ga1Sub$goal10Completions,ga1Sub$date)
    cr<-zoo(ga1Sub$goal10ConversionRate,ga1Sub$date)
    ga2Sub<-cbind(ses, con,cr)
    
    
    dygraph(ga2Sub) %>% 
      dySeries("ses", label = "Sessões",axis="y",stepPlot=TRUE,fillGraph=TRUE) %>% 
      dySeries("con",fillGraph=TRUE, label = "Transações",axis="y") %>% 
      dySeries("cr", label = "Taxa de Conversão",axis="y2",strokePattern = "dashed") %>% 
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>% 
      dyLegend(width = 400)%>% dyRangeSelector()
  })
  
  output$streamgraph <- renderStreamgraph({
    
    ga2Sub<-subset(ga2, date>=max(ga2$date)-as.numeric(input$radio))
    ga2Sub %>%
     streamgraph("deviceCategory", "sessions", "date",interactive=TRUE,offset="expand",interpolate="linear") %>% sg_colors("PuOr")%>%
      sg_legend(TRUE,"Channel of Traffic: ")
  })
  
  output$tree <- renderPlot({
  
    ga4Sub<-subset(ga4, date>=max(ga4$date)-as.numeric(input$radio))
    treemap(ga4Sub, 
          index=c("deviceCategory","operatingSystem"), 
          vSize="sessions", 
          type="index",fontsize.labels = c(12,9))
  })
  
  output$bubbles<-renderBubbles({
  bubbles(value = ga5Sub$sessions, label = ga5Sub$screenResolution,
          tooltip=paste(ga5Sub$sessions,"sessions"," "), 
          color = rainbow(10, alpha=NULL)[sample(10)])
  })
  
}

shinyApp(ui, server)
