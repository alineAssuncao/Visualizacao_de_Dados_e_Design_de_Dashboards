# Gerando Plots Padrões (histogramas, boxplots e scatterplots)


# Diretório de trabalho
# Altere o diretório abaixo de acordo com o diretório na sua máquina
setwd("/home/aline/Projetos/6/02-Graficos-Padroes")
getwd()

# Carregando o dataset
auto <- read.csv("carros.csv")
View(auto)
str(auto)

# Convertendo uma das variáveis para o tipo fator e anexando o dataset
auto$cylinders <- factor(auto$cylinders, levels = c(3,4,5,6,8), labels = c("3cyl", "4cyl", "5cyl", "6cyl", "8cyl"))
attach(auto)
str(auto)

# Histograma simples
hist(acceleration)	

# Histograma em cor azul
hist(acceleration, col = "blue", xlab = "acceleration", main = "Histograma de Aceleração", breaks = 15)

# Histograma Colorido
hist(mpg, col = rainbow(12))

# Histograma com linha
hist(mpg, prob = TRUE)
lines(density(mpg))

# Boxplot
boxplot(mpg, xlab = "Milhas por Galão")

# Boxplot
boxplot(mpg ~ model_year, xlab = "Milhas por Galão")

# Plot
plot(mpg ~ horsepower)

# Pair Plot
pairs(~mpg+displacement+horsepower+weight)

# Plot com linha de regressão
plot(mpg ~ horsepower)
reg <- lm(mpg ~ horsepower)
abline(reg)

# Preenchendo um plot vazio
plot(mpg ~ horsepower, type = "n")
with(subset(auto, cylinders == "8cyl"), points(horsepower, mpg, col = "blue"))
with(subset(auto, cylinders == "6cyl"),  points(horsepower, mpg, col = "red"))
with(subset(auto, cylinders == "5cyl"),  points(horsepower, mpg, col = "yellow"))
with(subset(auto, cylinders == "4cyl"),  points(horsepower, mpg, col = "green"))
with(subset(auto, cylinders == "3cyl"),  points(horsepower, mpg))


# Selecionando o dispositivo gráfico

# Post Script
postscript(file = "auto-scatter.ps")
boxplot(mpg)
dev.off()

# Auto scatter
pdf(file = "auto-scatter.pdf")
boxplot(mpg)
dev.off()


