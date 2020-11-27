# Mapas

# Google maps
# https://www.rdocumentation.org/packages/RgoogleMaps/versions/1.4.1
# https://cran.r-project.org/web/packages/RgoogleMaps/RgoogleMaps.pdf

# RGdal
# https://cran.r-project.org/web/packages/rgdal/index.html

# Diretório de trabalho
getwd()
setwd("/home/aline/Projetos/6/06.2GoogleMaps")

# Pacotes
install.packages("rgdal")
install.packages("RgoogleMaps")
library(rgdal)
library(RgoogleMaps)

# Dataset
air <- read.csv("londonair.csv")
View(air)

# Gravando o mapa 1
london <- GetMap(center = c(51.51,-0.116), zoom = 10, destfile = "London.png", maptype = "mobile")

# Gerando o mapa 1
PlotOnStaticMap(london,lat = air$lat, lon = air$lon, cex = 2, pch = 19, col = as.character(air$color))

# Gravando o mapa 2
london <- GetMap(center = c(51.51,-0.116), zoom = 10, destfile = "London_satellite.png", maptype = "satellite")

# Gerando o mapa 2
PlotOnStaticMap(london,lat = air$lat, lon = air$lon, cex = 2, pch = 19, col = as.character(air$color))

# Obter os dados do mapa
GetMap(center = c(40.714728,-73.99867), zoom = 14, destfile = "Manhattan.png", maptype = "hybrid")


# Gerando o gráfico a partir das variáveis e alterando o tipo
library(RgoogleMaps)

# ylim
lat <- c(48,64) 

# xlim
lon <- c(-140,-110) 

# Como centralizar o mapa
center = c(mean(lat), mean(lon))  

# zoom
zoom <- 5  

# Diversas opções visuais do google maps: 
# maptype = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")
terrmap <- GetMap(center = center, zoom = zoom, maptype = "terrain", destfile = "terrain.png") 




# Toronto Traffic Signals Heat Map
# Myles Harrison
# http://www.everydayanalytics.ca
# Data from Toronto Open Data Portal:
# http://www.toronto.ca/open

library(MASS)
library(RgoogleMaps)
library(RColorBrewer)


# addalpha() define as cores que serão usadas no mapa
addalpha <- function(colors, alpha = 1.0) {
  r <- col2rgb(colors, alpha=T)
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# colorRampPaletteAlpha() cria a paleta de cores
colorRampPaletteAlpha <- function(colors, n = 32, interpolate = 'linear') {
  cr <- colorRampPalette(colors, interpolate=interpolate)(n)
  a <- col2rgb(colors, alpha=T)[4,]
  if (interpolate=='linear') {
    l <- approx(a, n=n)
  } else {
    l <- spline(a, n=n)
  }
  l$y[l$y > 255] <- 255 
  cr <- addalpha(cr, l$y/255.0)
  return(cr)
}

# Carregando os dados
data <- read.csv(file = "traffic_signals.csv", skip = 1, header = T, stringsAsFactors = F)
View(data)

# Coletando dados de latitude e longitude
rawdata <- data.frame(as.numeric(data$Longitude), as.numeric(data$Latitude))
names(rawdata) <- c("lon", "lat")
data <- as.matrix(rawdata)

# Rodar as coordenadas lat-lon usando uma matriz de rotação
# Teste e erro conduzem a pi / 15.0 = 12 graus
theta = pi/15.0
m = matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow=2)
data <- as.matrix(data) %*% m

# Parâmetros e Plot
par(bg = 'black')
plot(data, cex = 0.1, col = "white", pch = 16)

# Crie o heatmap com kde2d e overplot
k <- kde2d(data[,1], data[,2], n=500)

# Intensidade de verde para vermelho
cols <- rev(colorRampPalette(brewer.pal(8, 'RdYlGn'))(100))
par(bg = 'white')
image(k, col = cols, xaxt = 'n', yaxt = 'n')
points(data, cex = 0.1, pch = 16) 

# Mapeamento via RgoogleMaps
# Localizar centro do mapa e obter mapa
center <- rev(sapply(rawdata, mean))
map <- GetMap(center = center, zoom = 11)

# Traduzir dados originais
coords <- LatLon2XY.centered(map, rawdata$lat, rawdata$lon, 11)
coords <- data.frame(coords)

# Gera o Heat Map novamente
k2 <- kde2d(coords$newX, coords$newY, n = 500)

# Crie um vetor de transparência exponencial e adicione
alpha <- seq.int(0.5, 0.95, length.out=100)
alpha <- exp(alpha^6-1)
cols2 <- addalpha(cols, alpha)

# Plot
PlotOnStaticMap(map)
image(k2, col = cols2, add = T)
points(coords$newX, coords$newY, pch = 16, cex = 0.3)
