# Gráficos Interativos de Séries Tempoarais com Dygraphs

# https://rstudio.github.io/dygraphs/

getwd()
setwd("/home/aline/Projetos/6/07-Dygraphs")
  
# Pacote
install.packages("dygraphs")
library(dygraphs)

# Dataset
lungDeaths <- cbind(mdeaths, fdeaths)

# Gráfico
dygraph(lungDeaths)

# Gráfico com seletor na parte inferior
dygraph(lungDeaths) %>% dyRangeSelector()

# Labels
dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

# Gerando previsões
hw <- HoltWinters(ldeaths)
predicted <- predict(hw, n.ahead = 72, prediction.interval = TRUE)

# Plot das previsões
dygraph(predicted, main = "Predicted Lung Deaths (UK)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))



