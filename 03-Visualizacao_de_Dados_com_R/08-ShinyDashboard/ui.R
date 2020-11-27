# Google Analytics Dashboard com Shiny
# ui.R é o código para interface com o usuário

#install.packages("shinyBS")
#install.packages("leaflet")


# Pacotes
# Instalar os pacotes no console do RStudio, caso não estejam instalados no seu computador
# Para instalar os pacotes, digite: install.packages("nome_pacote")
library(shiny)
library(shinydashboard)
library(shinyBS)
library(leaflet)

header <- dashboardHeader(title = "Google Analytics", dropdownMenuOutput("notifications"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", 
             icon = icon("dashboard")),
    
    menuItem("Mapas", icon = icon("globe"), tabName = "map",
             badgeColor = "red"),
    
    dateRangeInput(inputId = "dateRange", label = "Período de Data", 
                   start = "2013-05-01"), 
    
    radioButtons(inputId = "outputRequired", 
                 label = "Selecione uma Opção", 
                 choices = list("Tempo Médio de Sessão" = "meanSession", 
                                "Usuários" = "users", 
                                "Sessões" = "sessions")),
    
    checkboxInput("smooth", label = "Adicionar Margem de Erro?",
                  value = FALSE),
    
    actionButton("showData", "Mostrar os Dados de Conexão")
  )
)

body <- dashboardBody(
  bsModal(id = "clientData", title = "Client Data", 
          trigger = "showData", 
          verbatimTextOutput("clientdataText")),
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              infoBoxOutput(width = 3, "days"),
              infoBoxOutput(width = 3, "users"),
              infoBoxOutput(width = 3, "percentNew"),
              infoBox(width = 3, "Versão do Shiny", "0.12",
                      icon = icon("desktop"))),
            fluidRow(
              box(width = 5, plotOutput("trend")),
              box(width = 2, htmlOutput("gauge")),
              box(width = 5, plotOutput("histogram"))
            )
    ),
    
    tabItem(tabName = "map",
            box(width = 6, plotOutput("ggplotMap")),
            box(width = 6, leafletOutput("leaflet")))
  )
)

dashboardPage(header, sidebar, body)
