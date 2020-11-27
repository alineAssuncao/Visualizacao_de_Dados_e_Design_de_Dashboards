# Google Analytics Dashboard com Shiny
# server.R é o código para conexão e tratamento dos dados

#install.packages("googleVis")

# Pacotes
# Instalar os pacotes no console do RStudio, caso não estejam instalados no seu computador
# Para instalar os pacotes, digite: install.packages("nome_pacote")
library(shiny)
library(dplyr)
library(ggplot2)
#library(rgdal)
library(RColorBrewer)
library(googleVis)
library(leaflet)


shinyServer(function(input, output, session) {
  
  # Carrega os dados
  load("gadf.Rdata")
  
  # Template da funçõa abaixo em: http://shiny.rstudio.com/articles/client-data.html
  cdata <- session$clientData
  
  # Valores retornados como texto
  output$clientdataText <- renderText({
    cnames <- names(cdata)
    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep = " = ")
    })
    paste(allvalues, collapse = "\n")
  })
  
  # Filtrando os dados
  passData <- reactive({
    firstData <- filter(gadf, date >= input$dateRange[1] & date <= input$dateRange[2])
    if(!is.null(input$domainShow)){
      firstData <- filter(firstData, networkDomain %in% input$domainShow)
      }
    return(firstData)
    })
  
  # Ícones
  output$days <- renderInfoBox({
    infoBox(
      "Dias", input$dateRange[2] - input$dateRange[1], 
      icon = icon("calendar", lib = "font-awesome"),
      color = "blue", 
      fill = ifelse(max(passData()$date) == max(gadf$date), 
                    TRUE, FALSE)
    )
  })
  
  output$users <- renderInfoBox({
    infoBox(
      "Usuários", sum(passData()$users), icon = icon("user"),
      color = "purple", 
      fill = ifelse(sum(passData()$users) / 
                      as.numeric(input$dateRange[2] - 
                                   input$dateRange[1]) > 20, 
                    TRUE, FALSE)
    )
  })
  
  output$percentNew <- renderInfoBox({
    infoBox(
      "Novos Usuários", 
      paste0(round(sum(passData()$newUsers) / 
                     sum(passData()$users) * 100, 1), "%"), 
      icon = icon("pie-chart"),
      color = "green", 
      fill = ifelse(sum(passData()$newUsers) / 
                      sum(passData()$users) * 100 > 50, 
                    TRUE, FALSE)
    )
  })
  
  output$notifications <- renderMenu({
    
    users <- sum(gadf[gadf$date == max(gadf$date), "users"])
    newusers <- sum(gadf[gadf$date == max(gadf$date), "newUsers"]) / sum(gadf[gadf$date == max(gadf$date), "users"]) * 100
    newusers <- round(newusers, 0)
    notifData <- data.frame("number" = c(users, newusers), 
                            "text" = c(" users today", "% new users"), 
                            "icon"= c("users", "user"))
    
    notifs <- apply(notifData, 1, function(row) {
      notificationItem(text = paste0(row[["number"]], row[["text"]]), 
                       icon = icon(row[["icon"]]))
    })
    
    dropdownMenu(type = "notifications", .list = notifs)
    
  })
  
  output$gauge <- renderGvis({
    session$clientData$output_trend_width
    
    df <- data.frame(Label = "Bounce %", Value = round(mean(passData()$bounceRate, trim = .1), 1))
    
    gvisGauge(df,
              options = list(min = 0, max = 100, greenFrom = 0,
                             greenTo = 50, yellowFrom = 50, yellowTo = 70,
                             redFrom = 70, redTo = 100))
    
  })
  
  output$trend <- renderPlot({
    
    groupByDate <- group_by(passData(), YearMonth, networkDomain) %>%
      summarise(meanSession = mean(sessionDuration), 
                users = sum(users),
                newUsers = sum(newUsers), sessions = sum(sessions))
    
    groupByDate$Date <- as.Date(paste0(groupByDate$YearMonth, "01"), format = "%Y%m%d")
    
    thePlot <- ggplot(groupByDate, 
                      aes_string(x = "Date", y = input$outputRequired, 
                                 group = "networkDomain", colour = "networkDomain")) +
      geom_line()
    
    if(input$smooth){
      
      thePlot <- thePlot + geom_smooth()
    }
    
    print(thePlot)
    
  })
  
  output$histogram <- renderPlot({
    
    groupByDate <- group_by(passData(), YearMonth, networkDomain) %>%
      summarise(meanSession = mean(sessionDuration), 
                users = sum(users),
                newUsers = sum(newUsers), sessions = sum(sessions))
    
    groupByDate$Date <- as.Date(paste0(groupByDate$YearMonth, "01"), format = "%Y%m%d")
    
    ggplot(groupByDate, 
           aes_string(x = input$outputRequired, group = "networkDomain")) +
      geom_histogram(binwidth = diff(range(groupByDate[[input$outputRequired]])) / 10)
    
  })
  
  # Produzindo o Mapa
  output$ggplotMap <- renderPlot ({
    groupCountry <- group_by(passData(), country)
    groupByCountry <- summarise(groupCountry, meanSession = mean(sessionDuration), 
                                users = log(sum(users)), sessions = log(sum(sessions)))
    
    world <- readOGR(dsn = ".", layer = "world_country_admin_boundary_shapefile_with_fips_codes")
    countries <- world@data
    countries <- cbind(id = rownames(countries), countries)
    countries <- merge(countries, groupByCountry, by.x = "CNTRY_NAME", by.y = "country", all.x = TRUE)
    map.df <- fortify(world)
    map.df <- merge(map.df, countries, by = "id")
    
    ggplot(map.df, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes_string(fill = input$outputRequired)) +
      geom_path(colour = "grey50") +
      scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")),
                           na.value = "white") +
      coord_fixed() + labs(x = "", y = "")
    
  })
  
  # Leaflet map
  output$leaflet <- renderLeaflet({
    leaflet(gadf) %>% addTiles() %>% setView(lng = 1.1333, lat = 52.95, zoom = 4)
    })
  
})