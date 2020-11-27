# Heat Maps 


# Diretório de trabalho
# Altere o diretório abaixo de acordo com o diretório na sua máquina
setwd("/home/aline/Projetos/6/04-HeatMaps")
getwd()

# Pacote
install.packages("RColorBrewer")
library(RColorBrewer)

# Carregando os dados
vendas <- read.csv("vendas.csv")
head(vendas)

# Preparando os dados
rownames(vendas) <- vendas[,1]
vendas <- vendas[,-1]
data_matrix <- data.matrix(vendas)
head(vendas)

# Paleta de cores e tamanho dos bins (retângulos)
pal = brewer.pal(7,"YlOrRd")
breaks <- seq(3000,12000,1500)

# Cria layout com 1 linha e 2 colunas (para o heatmap e escala); A coluna heatmap é 8 vezes maior que a coluna da escala
?layout
layout(matrix(data = c(1,2), nrow = 1, ncol = 2), widths = c(8,1), heights = c(1,1))

# Escolha margens para o heat map
par(mar = c(5,10,4,2), oma = c(0.2,0.2,0.2,0.2), mex = 0.5)           

# Cria o gráfico
?image
image(x = 1:nrow(data_matrix), y = 1:ncol(data_matrix), 	
      z = data_matrix,
      axes = FALSE,
      xlab = "Mês",
      ylab = "",
      col = pal[1:(length(breaks)-1)], 
      breaks = breaks,
      main = "Heat Map de Vendas")

# Adiciona labels ao eixo x
axis(1, at=1:nrow(data_matrix), labels = rownames(data_matrix), col = "white", las = 1)
    
# Adiciona labels ao eixo y       
axis(2, at=1:ncol(data_matrix), labels = colnames(data_matrix), col = "white", las = 1)

# Adiciona linhas divisórias
abline(h=c(1:ncol(data_matrix))+0.5, 
       v=c(1:nrow(data_matrix))+0.5, col="white",lwd=2,xpd=FALSE)

# Adicionando breaks para usar como esacala
breaks2 <- breaks[-length(breaks)]

# Color Scale
par(mar = c(5,1,4,7)) 

# Se você obtiver um erro de margens de figura ao executar o código acima, amplie o dispositivo de plotagem ou 
# ajuste as margens para que o gráfico e a escala se encaixem no dispositivo.
image(x=1, y=0:length(breaks2),z=t(matrix(breaks2))*1.001,
      col=pal[1:length(breaks)-1],
      axes=FALSE,
      breaks = breaks,
      xlab = "", 
      ylab = "",
      xaxt = "n")

# Labels e linhas divisórias
axis(4, at=0:(length(breaks2)-1), labels = breaks2, col = "white", las = 1)
abline(h = c(1:length(breaks2)), col = "white", lwd = 2, xpd = F)


# Correlation Heat Maps

# Carrega os dados
genes <- read.csv("genes.csv")
head(genes)

# Prepara os dados
rownames(genes) <- genes[,1]
data_matrix <- data.matrix(genes[,-1])

# Cores e breaks
pal = heat.colors(5)
breaks <- seq(0,1,0.2)

# Layout
layout(matrix(data = c(1,2), nrow = 1, ncol = 2), widths = c(8,1), heights = c(1,1))
par(mar = c(3,7,12,2), oma = c(0.2,0.2,0.2,0.2), mex = 0.5)           

# Cria o gráfico
image(x=1:nrow(data_matrix),y=1:ncol(data_matrix),
      z = data_matrix,
      xlab = "",
      ylab = "",
      breaks = breaks,
      col = pal,
      axes = FALSE)

# Adiciona texto
text(x=1:nrow(data_matrix)+0.75, y=par("usr")[4] + 1.25, srt = 45, adj = 1, labels = rownames(data_matrix), xpd = TRUE)

# Labels
axis(2,at=1:ncol(data_matrix), labels = colnames(data_matrix), col = "white", las = 1)

# Linhas
abline(h = c(1:ncol(data_matrix))+0.5, v = c(1:nrow(data_matrix))+0.5, col = "white", lwd = 2, xpd = F)

# Título
title("Correlação Entre Genes", line = 8, adj = 0)



# Sumarizando Dados Multivariados em um único Heat Map

# Carregando os dados
nba <- read.csv("nba.csv")
head(nba)

# Pacote de cores
library(RColorBrewer)

# Obtendo o nome das linhas
rownames(nba) <- nba[,1]

# Ajustando os dados
data_matrix <- t(scale(data.matrix(nba[,-1])))

# Paleta de cores
pal = brewer.pal(6,"Blues")

# Vetor com o nome das estatísticas
statnames <- c("Games Played", "Minutes Played", "Total Points", "Field Goals Made", "Field Goals Attempted", "Field Goal Percentage", "Free Throws Made", "Free Throws Attempted", "Free Throw Percentage", "Three Pointers Made", "Three Pointers Attempted", "Three Point Percentage", "Offensive Rebounds", "Defensive Rebounds", "Total Rebounds", "Assists", "Steals", "Blocks", "Turnovers", "Fouls")

# Parâmetros do gráfico
par(mar = c(3,14,19,2), oma=c(0.2,0.2,0.2,0.2), mex=0.5)

# Heat map          
image(x = 1:nrow(data_matrix),
      y = 1:ncol(data_matrix),
      z = data_matrix,
      xlab = "",
      ylab = "",
      col = pal,
      axes = FALSE)

# Label no eixo x
text(1:nrow(data_matrix), par("usr")[4] + 1, srt = 45, adj = 0, labels = statnames, xpd = TRUE, cex = 0.85)

# Label no eixo y
axis(side=2, at = 1:ncol(data_matrix), labels = colnames(data_matrix), col = "white", las = 1, cex.axis = 0.85)

# Linha divisória
abline(h = c(1:ncol(data_matrix))+0.5, v=c(1:nrow(data_matrix))+0.5, col = "white", lwd = 1, xpd = F)

# Título 
text(par("usr")[1]+5, par("usr")[4] + 12, "Performance por Jogo dos 50 Melhores Atletas da NBA", xpd = TRUE, font = 2, cex = 1.5)



# Contour plots
??countour
?volcano
contour(x = 10*1:nrow(volcano), 
        y = 10*1:ncol(volcano), 
        z = volcano, 
        xlab = "Metros a Oeste",
        ylab = "Metros ao Norte", 
        main = "Topografia do Vulcão Maunga Whau")

# Criando a área de plotagem vazia
par(las = 1)
plot(0,0,
     xlim = c(0,10*nrow(volcano)),
     ylim = c(0,10*ncol(volcano)),
     type = "n",
     xlab = "Metros a Oeste",
     ylab = "Metros ao Norte",
     main = "Topografia do Vulcão Maunga Whau")

# Ajustando a área de plotagem e alterando a cor de fundo
u <- par("usr")
rect(u[1],u[3],u[2],u[4], col = "lightgreen")

# Criando o gráfico
contour(x = 10*1:nrow(volcano),
        y = 10*1:ncol(volcano), 
        volcano,
        col = "red",
        add = TRUE)


# Preenchendo o Countour Plot (filled contour plots)
filled.contour(x = 10*1:nrow(volcano), 
               y = 10*1:ncol(volcano), 
               z = volcano, 
               color.palette = terrain.colors, 
               plot.title = title(main = "Topografia do Vulcão Maunga Whau", xlab = "Metros ao Norte", ylab = "Metros a Oeste"),
               plot.axes = {axis(1, seq(100, 800, by = 100)) 
                 axis(2, seq(100, 600, by = 100))},
               key.title = title(main="Altura\n(metros)"),
               key.axes = axis(4, seq(90, 190, by = 10))) 


# Preenchendo o Countour Plot (filled contour plots) e aumentando o nível de detalhes 
filled.contour(x = 10*1:nrow(volcano), 
               y = 10*1:ncol(volcano), 
               z = volcano, 
               color.palette = terrain.colors, 
               plot.title = title(main = "Topografia do Vulcão Maunga Whau", xlab = "Metros ao Norte", ylab = "Metros a Oeste"),
               nlevels = 100,
               plot.axes = {axis(1, seq(100, 800, by = 100)) 
                 axis(2, seq(100, 600, by = 100))},
               key.title = title(main = "Altura\n(metros)"),
               key.axes = axis(4, seq(90, 190, by = 10))) 


# Surface plots 3D

# Pacote
install.packages("rgl")
library(rgl)

# Variáveis
z <- 2 * volcano
x <- 10 * (1:nrow(z))
y <- 10 * (1:ncol(z))

# Definindo os limites
zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1

# Cores
colorlut <- terrain.colors(zlen) 
col <- colorlut[ z-zlim[1]+1 ] 

# Cria o gráfico
rgl.open()
rgl.surface(x, y, z, color = col, back = "lines")


# Visualizando Séries Temporais com Calendar Heat Maps

# Importa a biblioteca calendarHeat.R
source("calendarHeat.R")

# Dados
stock.data <- read.csv("google.csv")

# Pacote
install.packages("chron")
library(chron)

# Cria o calendário
calendarHeat(dates = stock.data$Date, values = stock.data$Adj.Close, varname = "Google Adjusted Close")

# Usando o openair
install.packages("openair")
library(openair)

# Usando o CalendarPlot
?calendarPlot
calendarPlot(mydata)

# Gerando massa de dados
mydata$sales <- rnorm(length(mydata$nox), mean = 1000, sd = 1500)

# CalendarPlot
calendarPlot(mydata, pollutant = "sales", main = "Vendas Diárias em 2003")



# Acesse o site e crie sua conta
# https://plot.ly/
# https://plot.ly/r/heatmaps/
# acessando config api
  # https://plot.ly/settings/api

# Carregando o pacote
library(plotly)
packageVersion('plotly')

### TROCANDO CONFIGURACAO DA API
#py <- plotly("RgraphingAPI", "ektgzomjbx")
Sys.setenv("plotly_username"=" user_name ")
Sys.setenv("plotly_api_key"=" key_api ")

### TROCANDO FUNÇÃO 
p <- plot_ly(z = volcano, type = "heatmap")
api_create(p)

### TROCANDO FUNÇÃO 
### TROCANDO filename
chart_link = api_create(p, filename = "heatmap_simple")
chart_link




