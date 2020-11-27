# Criando Gráficos com ggplot2


# Diretório de trabalho
# Altere o diretório abaixo de acordo com o diretório na sua máquina
setwd("/home/aline/Projetos/6/03-ggplot2")
getwd()

# Instalando e carregando o pacote
install.packages("ggplot2")


# Definindo o conjunto de dados com dataset tips
??tips
data(tips, package = 'reshape2')
View(tips)

# Camada 1
?aes
??aes
camada1 <- geom_point(
  mapping = aes(x = total_bill, y = tip, color = sex),
  data = tips,
  size = 3
)

ggplot() + camada1

# Contruindo um modelo de regressão
modelo_base <- lm(tip ~ total_bill, data = tips)
modelo_fit <- data.frame(
  total_bill = tips$total_bill, 
  predict(modelo_base, interval = "confidence")
)

head(modelo_fit)

# Camada 2
camada2 <- geom_line(
  mapping = aes(x = total_bill, y = fit),
  data = modelo_fit,
  color = "darkred"
)

ggplot() + camada1 + camada2


# Camada 3
camada3 <- geom_ribbon(
  mapping = aes(x = total_bill, ymin = lwr, ymax = upr),
  data = modelo_fit,
  alpha = 0.3
)

ggplot() + camada1 + camada2 + camada3


# Versão final otimizada
ggplot(tips, aes(x = total_bill, y = tip)) +
  geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm')


# Gravando o gráfico em um objeto
myplot <- ggplot(tips, aes(x = total_bill, y = tip)) +
  geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm')

class(myplot)
print(myplot)


# Colocando gráficos lado a lado na área de desenho

########## Usando Base Plotting System #####################
# Carregando os dados
bikes <- read.csv("bikes.csv")
head(bikes)
str(bikes)
bikes$season <- factor(bikes$season, levels = c(1,2,3,4), labels = c("Primavera", "Verão", "Outono", "Inverno"))
attach(bikes)
head(bikes$season)

# Dividindo a área de desenho em 4 sub-áreas
par(mfrow = c(2,4))

# Coletando amostras dos dados
primavera <- subset(bikes, season == "Primavera")$cnt
verao <- subset(bikes, season == "Verão")$cnt
outono <- subset(bikes, season == "Outono")$cnt
inverno <- subset(bikes, season == "Inverno")$cnt

# Desenhando os gráficos
hist(primavera, prob = TRUE, xlab = "Aluguel Diário na Primavera", main = "")
lines(density(primavera))

hist(verao, prob = TRUE, xlab = "Aluguel Diário no Verão", main = "")
lines(density(verao))

hist(outono, prob = TRUE, xlab = "Aluguel Diário no Outono", main = "")
lines(density(outono))

hist(inverno, prob = TRUE, xlab = "Aluguel Diário no Inverno", main = "")
lines(density(inverno))

########## Usando ggplot2 #####################
par(mfrow = c(2,2))
qplot(cnt, data = bikes) + facet_wrap(~ season, nrow = 2) + geom_histogram(fill = "blue") 
qplot(cnt, data = bikes, fill = season)

# Gráficos lado a lado na mesma área de gráfico
qplot(season, cnt, data = bikes, geom = c("boxplot"), fill = season)
ggplot(bikes, aes(x = season, y = cnt)) + geom_boxplot()


# Plots multivariados
bikes <- read.csv("bikes.csv")
bikes$season <- factor(bikes$season, levels = c(1,2,3,4), labels = c("Primavera", "Verão", "Outono", "Inverno"))
bikes$weathersit <- factor(bikes$weathersit, levels = c(1,2,3), labels = c("Sol", "Nublado", "Chuva"))
bikes$windspeed.fac <- cut(bikes$windspeed, breaks = 3, labels = c("Baixo", "Médio", "Alto"))
bikes$weekday <- factor(bikes$weekday, levels = c(0:6), labels = c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab"))
attach(bikes)  

# Criando o objeto plot
plot <- ggplot(bikes, aes(temp, cnt))

# Adicionando camadas ao plot
plot + geom_point(size = 3, 
                  aes(color = factor(windspeed.fac))) + 
  geom_smooth(method = "lm", se = FALSE, col = "red") + 
  facet_grid(weekday ~ season) + 
  theme(legend.position = "bottom")


# Scatter Plot e Scatter Plot 3d

# Scatter Plot

# Dados
data = data.frame(cond = rep(c("Obs 1", "Obs 2"), each = 10), var1 = 1:100 + rnorm(100, sd = 9), var2 = 1:100 + rnorm(100,sd = 16))
head(data)

# Plot
ggplot(data, aes(x = var1, y = var2)) +    
  geom_point(shape = 1) +  
  geom_smooth(method = lm , color = "red", se = FALSE)  

# Scatter Plot 3D
install.packages("scatterplot3d") 
library(scatterplot3d)

# Definindo o tamanho da área de desenho
par(mfrow = c(1,1))

scatterplot3d(x = mtcars$wt,
              y = mtcars$disp,
              z = mtcars$mpg)

scatterplot3d(mtcars$wt, mtcars$disp, mtcars$mpg, 	
              pch=16, highlight.3d = TRUE, angle = 20,
              xlab = "Peso", ylab = "Deslocamento", zlab = "Consumo de Combustível(mpg)",
              type = "h", 
              main = "Relacionamento Entre Características de Automóveis")



# BarPlot, Histograma e Polígono de Frequência

# Bar Plot

# Dados
data = data.frame(grupo = c("A ","B ","C ","D ") , 
                  valor = c(33,62,56,67) , 
                  num_obs = c(100,500,459,342))

# Gerando a massa de dados
data$right = cumsum(data$num_obs) + 30 * c(0:(nrow(data)-1))
data$left = data$right - data$num_obs 

# Plot
ggplot(data, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, 
                ymax = valor, colour = grupo, fill = grupo)) +
  xlab("Número de obs") + ylab("Valor")


# Histograma
ggplot(diamonds, aes(carat)) +
  geom_histogram()

ggplot(diamonds, aes(carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(diamonds, aes(carat)) +
  geom_histogram(bins = 200)

ggplot(diamonds, aes(price, fill = cut)) +
  geom_histogram(binwidth = 500)


# Polígono de Frequência
ggplot(diamonds, aes(price, colour = cut)) +
  geom_freqpoly(binwidth = 500)

# Para facilitar a comparação de distribuições com contagens muito diferentes, colocamos densidade no eixo y 
# em vez da contagem padrão
ggplot(diamonds, aes(price, ..density.., colour = cut)) +
  geom_freqpoly(binwidth = 500)


# Personalizando o Gráfico

# Dados
head(mtcars)

# Plot simples
ggplot(data = mtcars, aes(x = disp, y = mpg)) + geom_point()

# Outro aspecto que pode ser mapeado nesse gráfico é a cor dos pontos
ggplot(data = mtcars, 
       aes(x = disp, y = mpg, 
           colour = as.factor(am))) + geom_point()

# No entanto, tambem podemos mapear uma variável contínua à cor dos pontos:
ggplot(mtcars, aes(x = disp, y = mpg, colour = cyl)) + geom_point()

# Também podemos mapear o tamanho dos pontos à uma variável de interesse:
# A legenda é inserida no gráfico automaticamente
ggplot(mtcars, aes(x = disp, y = mpg, colour = cyl, size = wt)) + geom_point()

# Os geoms definem qual forma geométrica será utilizada para a visualização dos dados no gráfico. 
ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) + geom_boxplot()

# Histogramas
ggplot(mtcars, aes(x = mpg), binwidth = 30) + geom_histogram()

# Gráfico de Barras
ggplot(mtcars, aes(x = as.factor(cyl))) + geom_bar()

# Personalizando os gráficos
colors()

ggplot(mtcars, aes(x = as.factor(cyl), y = mpg, 
                   colour = as.factor(cyl))) + geom_boxplot()

ggplot(mtcars, aes(x = as.factor(cyl), y = mpg, 
                   fill = as.factor(cyl))) + geom_boxplot()

ggplot(mtcars, 
       aes(x = as.factor(cyl), y = mpg)) + 
  geom_boxplot(color = "blue", fill = "seagreen4")


# Podemos alterar os eixos
ggplot(mtcars, aes(x = mpg)) + 
  geom_histogram() + 
  xlab("Milhas por galão") + ylab("Frequência")

# Alterar os limites do gráfico
ggplot(mtcars, aes(x = mpg)) + geom_histogram() + xlab("Milhas por galão") + ylab("Frequência") +
  xlim(c(0, 40)) +
  ylim(c(0,8))

# Legendas
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) + 
  geom_bar() +
  labs(fill = "cyl")

# Trocando a posição da legenda
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) + 
  geom_bar() +
  labs(fill = "cyl") +
  theme(legend.position="top")

# Sem legenda
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) + 
  geom_bar() +
  guides(fill=FALSE)


# Facets
ggplot(mtcars, aes(x = mpg, y = disp, colour = as.factor(cyl))) + 
  geom_point() + 
  facet_grid(am~.)

ggplot(mtcars, aes(x = mpg, y = disp, colour = as.factor(cyl))) +
  geom_point() + 
  facet_grid(.~am)


# Plots diferentes juntos (diferente de Facet)
install.packages(c("gridExtra", "ggplot2"))
library(gridExtra)
library(ggplot2)

# Dataset diamonds
data(diamonds)

# Histograma como plot1
plot1 <- qplot(price, data = diamonds, binwidth = 1000)

# ScatterPlot como plot2
plot2 <- qplot(carat, price, data = diamonds, colour = cut)

# Combina os 2 plots na mesma área
grid.arrange(plot1, plot2, ncol = 1)


# Facets com reshape
install.packages("plotly")
library(reshape2)
library(plotly)

sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)
sp + facet_grid(sex ~ .)
ggplotly()
sp + facet_grid(. ~ sex)
ggplotly()
sp + facet_wrap( ~ day, ncol = 2)
ggplotly()


ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~manufacturer)
ggplotly()

