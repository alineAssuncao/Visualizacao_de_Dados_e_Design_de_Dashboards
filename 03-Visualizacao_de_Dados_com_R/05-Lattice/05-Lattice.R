# Criando gráficos com lattice
getwd()
setwd("/home/aline/Projetos/6/05-Lattice")

# Instalando o pacote
install.packages("lattice")
library(lattice)

# Carregando o dataset
auto <- read.csv("carros.csv", stringsAsFactors = FALSE)
head(auto)
str(auto)

# Convertendo a coluna cylinders para o tipo fator
cyl.factor <- factor(auto$cylinders, labels = c("3cyl","4cyl", "5cyl","6cyl","8cyl"))

# Criando o gráfico - Scatterplots
# (Outros disponíveis são barchart, bwplot, densityplot, dotplot, histogram, etc.).
?xyplot
xyplot(mpg ~ weight|cyl.factor, data = auto, main = "Scatterplots Por Cilindros", ylab = "Milhas por Galão", xlab = "Peso do Carro")

# Para ver as bases de dados disponíveis
data()

# Para carregar a base de dados
library(MASS) 
data(Cars93) 
dim(Cars93)   
?Cars93      
names(Cars93) 
head(Cars93)  
View(Cars93)

# Mostra os valores únicos contidos em colunas específicas
unique(Cars93$Origin) 
unique(Cars93$Make)

# Um dos tipos de gráficos do pacote lattice
xyplot(MPG.city ~ EngineSize, Cars93)

# Selecionando dados
# seleciona dados de quatro fabricantes para trabalhar 
data <- subset(Cars93, Manufacturer %in% c('Ford', 'Chevrolet', 'Toyota', 'Honda')) 

# Criando o gráfico
xyplot(MPG.city ~ EngineSize | Manufacturer, data)
xyplot(MPG.city ~ EngineSize | Manufacturer, data, type = 'l')   

xyplot(MPG.city ~ EngineSize | Manufacturer, data, group = Type, auto.key = T)
               
# seleciona apenas três tipos de carro
data <- subset(Cars93, Type %in% c('Compact', 'Small', 'Large'))

# Plota de acordo com origem, tipo e por número de passageiros
xyplot(MPG.city ~ EngineSize | Origin + Type, data, group = Passengers, auto.key = list(space = 'right'))

# Legenda
xyplot(MPG.city ~ EngineSize | Manufacturer, data, group = Type, auto.key = list(space = 'right'))

# Título e Labels
xyplot(MPG.city ~ EngineSize | Manufacturer, data, 
       group = Type, 
       auto.key = list(space = 'right'),
       main = 'Relação entre rendimento e motor',
       xlab = 'tamanho do motor (litros)',
       ylab = 'rendimento (milhas por galão)')

# Separação ao longo do eixo x
xyplot(MPG.city ~ EngineSize | Manufacturer, data, between = list(x = 1))

# Salvando o gráfico
dev.copy(device = pdf, file = "lattice.pdf", width = 600, paper = "USr")
dev.off()


# Box Plots

# Criando o gráfico - Boxplots
?bwplot
bwplot(~auto$mpg|cyl.factor, main = "Consumo / Número de Cilindros", xlab = "Milhas por Galão")

# Parâmetros do Gráfico
?trellis.par.set
trellis.par.set(theme = col.whitebg())

# Criando o gráfico
bwplot(~mpg|cyl.factor, data = auto, main = "Consumo / Número de Cilindros", xlab = "Milhas por Galão", layout = c(2,3), aspect = 1)


# Violin Plot

# O Violin Plot é semelhante aos boxplots, exceto que eles também mostram a densidade de probabilidade dos dados 
# em diferentes valores (no caso mais simples, isso poderia ser um histograma).

# Tipicamente violin plots incluirão um marcador para a mediana dos dados e uma caixa indicando o intervalo interquartil, 
# como em boxplots padrões. Um violin plot é mais informativo do que um boxplot simples. 
# De fato, enquanto um boxplot mostra apenas estatísticas resumidas, como média / mediana e intervalos interquartis, 
# o gráfico de violino mostra a distribuição completa dos dados.

# Carregando dados
# NASA Surface meteorology and Solar Energy (SSE) Release 6.0 Data Set (Jan 2008)
nasafile <- 'https://eosweb.larc.nasa.gov/sse/global/text/global_radiation'
nasa <- read.table(file = nasafile, skip = 13, header = TRUE)
View(nasa)

# Criando o gráfico
bwplot(Ann~cut(Lat, pretty(Lat, 40)),
       data=nasa, subset = (abs(Lat)<60),
       xlab = 'Latitude', ylab = 'G(0) (kWh/m²)',
       horizontal = FALSE,
       panel = function(..., box.ratio) {panel.violin(..., col = "lightblue", varwidth = FALSE, box.ratio = box.ratio)
         panel.bwplot(..., col = 'black', 
                      cex = 0.8, pch = '|', fill = 'gray', box.ratio = .1)
       },
       par.settings = list(box.rectangle = list(col = 'black'),
                           plot.symbol = list(pch = '.', cex = 0.1)),
       scales = list(x = list(rot = 45, cex = 0.5))
)

# Ajustando os dados
x <- paste(names(nasa)[3:14], collapse='+')
formula <- as.formula(paste(x, '~cut(Lat, pretty(Lat, 20))', sep=''))

# Criando o gráfico
bwplot(formula, data = nasa, subset = (abs(Lat)<60),
       xlab = 'Latitude', ylab = 'G(0) (kWh/m²)',
       outer = TRUE, as.table = TRUE, horizontal = FALSE,
       col = 'lightblue',
       panel = panel.violin,
       scales = list(x = list(rot = 70, cex = 0.5)))


# Violin Plot com o pacote vioplot

# Pacote
install.packages("vioplot")
library(vioplot)

### CONJUNTO DE DADOS NÃO ACESSÍVEL
# Carregando os dados
#ds = read.csv("http://www.math.smith.edu/r/data/help.csv")
#female = subset(ds, female==1)

#with(female, vioplot(pcs[homeless==0], pcs[homeless==1],
 #                    horizontal=TRUE, names=c("non-homeless", "homeless"), 
  #                   col = "lightblue"))



















