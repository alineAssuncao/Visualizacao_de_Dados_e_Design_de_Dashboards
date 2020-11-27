# Pacote ggvis
# http://ggvis.rstudio.com/

# Session Info
sessionInfo()

# Diretório de trabalho
getwd()
setwd("/home/aline/Projetos/6/06.1-ggvis")

# Pacotes
install.packages("ggvis")
library(ggvis)
library(dplyr)

# Gráfico de Barras
pressure %>% ggvis(x = ~temperature, y = ~pressure) %>% layer_bars()

# Gráfico de Barras com variáveis categóricas
pressure %>% ggvis(~factor(temperature), ~pressure) %>% layer_bars()

# Gráfico de Barras
mtcars %>% ggvis(x = ~cyl) %>% layer_bars()

# Observe como o gráfico de barras difere de um histograma: um histograma tem compartimentos que abrangem
# Intervalos de x, mas layer_bars mostra a contagem em cada valor x exclusivo.
mtcars %>% ggvis(~wt) %>% layer_histograms()
mtcars %>% ggvis(~wt) %>% layer_bars()


# Gráfico de Barras Sem empilhamento - as barras se sobrepõem
hec <- as.data.frame(xtabs(Freq ~ Hair + Eye, HairEyeColor))
hec %>% group_by(Eye) %>%
  ggvis(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
  layer_bars(stack = FALSE) %>%
  scale_nominal("fill",
                domain = c("Brown", "Blue", "Hazel", "Green"),
                range = c("#995522", "#88CCFF", "#999933", "#00CC00"))

# Com empilhamento
hec %>% group_by(Eye) %>%
  ggvis(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
  layer_bars() %>%
  scale_nominal("fill",
                domain = c("Brown", "Blue", "Hazel", "Green"),
                range = c("#995522", "#88CCFF", "#999933", "#00CC00"))


# Empilhamento na direção x em vez de padrão y 
hec %>% group_by(Eye) %>%
  ggvis(y = ~Hair, fill = ~Eye, fillOpacity := 0.5) %>%
  compute_stack(stack_var = ~Freq, group_var = ~Hair) %>%
  layer_rects(x = ~stack_lwr_, x2 = ~stack_upr_, height = band()) %>%
  scale_nominal("y", range = "height", padding = 0, points = FALSE) %>%
  scale_nominal("fill",
                domain = c("Brown", "Blue", "Hazel", "Green"),
                range = c("#995522", "#88CCFF", "#999933", "#00CC00"))


# Faça o conjunto de dados com x categórico
mtc <- mtcars
mtc$cyl <- factor(mtc$cyl)
mtc %>% ggvis(~cyl, ~mpg) %>% layer_boxplots()

# Contínuo x: largura de preenchimento de caixas entre valores de dados
mtcars %>% ggvis(~cyl, ~mpg) %>% layer_boxplots()

# A largura de ajuste = 0,5 torna 0,5 de largura no espaço de dados, que é 1/4 do
# distância entre valores de dados neste caso particular.
mtcars %>% ggvis(~cyl, ~mpg) %>% layer_boxplots(width = 0.5)


# Tooltip brush 
x_bar <- "x&#772;"
sigma_hat <- "&sigma;&#770;"

brushed_summary <- function(items, session, page_loc, ...) {
  if (nrow(items) == 0) return()
  
  items$key__ <- NULL
  lines <- Map(function(name, vals) {
    paste0(name, ": ",
           x_bar, " = ", round(mean(vals), 2), " ",
           sigma_hat, " = ", round(sd(vals), 2)
    )
  }, names(items), items)
  html <- paste(unlist(lines), collapse = "<br />\n")
  
  show_tooltip(session, page_loc$r + 5, page_loc$t, html)
}

# Scatter plot com brushing
mtcars %>% ggvis(x = ~wt, y = ~mpg) %>%
  layer_points(size.brush := 400) %>%
  handle_brush(brushed_summary)


# Gráficos de Linha
set.seed(1780)
df <- data.frame(x = runif(12), y = runif(12), z = gl(3, 4))

df %>% ggvis(x = ~x, y = ~y) %>% layer_paths()

# Agrupamento, especificado manualmente
df %>% group_by(z) %>%
  ggvis(x = ~x, y = ~y, stroke = ~z, fill := NA) %>%
  layer_paths() %>%
  layer_points()

# Agrupamento pode acontecer após a chamada ggvis ()
df %>%
  ggvis(x = ~x, y = ~y, stroke = ~z, fill := NA) %>%
  group_by(z) %>%
  layer_paths() %>%
  layer_points()

# Dados ordenados por x
df %>% ggvis(x = ~x, y = ~y) %>%
  arrange(x) %>%
  layer_paths() %>%
  layer_points()

# Dashed lines
dat <- data.frame(x = rep(c(0, 1), 6), g = gl(6, 2))
dat %>% group_by(g) %>%
  ggvis(x = ~x, y = ~g) %>%
  layer_paths(strokeDash = ~g) %>%
  add_axis("y", grid = FALSE) %>%
  add_axis("x", grid = FALSE, title = "", tick_size_major = 0, ticks = 0)


# Mapas interativos com ggvis

# Mapeando os dados
map_data = ggplot2::map_data("state")
head(map_data)

# Agrupando e plotando
map_data %>% select(long, lat, group, order, region) %>% 
  group_by(group) %>% 
  ggvis(x = ~long, y = ~lat) %>% 
  layer_paths(fill = ~region) %>%
  hide_legend("fill") %>% 
  handle_click(on_click = function(data, ...) {print(data)})





# https://rud.is/b/2014/12/29/making-static-interactive-maps-with-ggvis-using-ggvis-maps-wshiny/
# http://rpubs.com/hrbrmstr/ggvis-maps
# https://github.com/hrbrmstr/ggvis-maps



