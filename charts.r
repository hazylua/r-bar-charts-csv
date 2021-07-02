library(tidyr)
library(ggplot2)
library(reshape2)
library(readr)

library("ggsci")
library("viridis")

library(extrafont)

loadfonts(quiet = T)

plot_names <- c(
  "Filtro Gaussiano - 50% de Ruído \U22Sal e Pimenta\U22",
  "Filtro Gaussiano - 5% de Ruído \U22Sal e Pimenta\U22",
  "Filtro Gaussiano - 25% de Ruído \U22Sal e Pimenta\U22",
  "Filtro Gaussiano - Ruído Gaussiano com \U03C3 = 0,5",
  "Filtro Gaussiano - Ruído Gaussiano com \U03C3 = 0,9",
  
  "Filtro da Mediana - 50% de Ruído \U22Sal e Pimenta\U22",
  "Filtro da Mediana - 5% de Ruído \U22Sal e Pimenta\U22",
  "Filtro da Mediana - 25% de Ruído \U22Sal e Pimenta\U22",
  "Filtro da Mediana - Ruído Gaussiano com \U03C3 = 0,5",
  "Filtro da Mediana - Ruído Gaussiano com \U03C3 = 0,9",
  
  "Filtro do AC-mediana - 50% de Ruído \U22Sal e Pimenta\U22",
  "Filtro do AC-mediana - 5% de Ruído \U22Sal e Pimenta\U22",
  "Filtro do AC-mediana - 25% de Ruído \U22Sal e Pimenta\U22",
  "Filtro do AC-mediana - Ruído Gaussiano com \U03C3 = 0,5",
  "Filtro do AC-mediana - Ruído Gaussiano com \U03C3 = 0,9",
  
  "Filtro do AC-média - 50% de Ruído \U22Sal e Pimenta\U22",
  "Filtro do AC-média - 5% de Ruído \U22Sal e Pimenta\U22",
  "Filtro do AC-média - 25% de Ruído \U22Sal e Pimenta\U22",
  "Filtro do AC-média - Ruído Gaussiano com \U03C3 = 0,5",
  "Filtro do AC-média - Ruído Gaussiano com \U03C3 = 0,9"
)

signif.ceiling <- function(x, n){
  pow <- floor( log10( abs(x) ) ) + 1 - n
  y <- ceiling(x / 10 ^ pow) * 10^pow
  # handle the x = 0 case
  y[x==0] <- 0
  y
}

draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.8, "npc"),
    height = grid::unit(0.8, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

my_colors = c("#e1f5f3", "#b3e5fc", "#81d4fa", "#4fc3f7", "#29b6f6", "#03a9f4", "#039be5", "#0288d1", "#0277bd", "#01579b")



df <- read.csv(file='./ssim.csv', sep = ';', fileEncoding = "UTF-8-BOM")
df <- df %>% unite("with_on", "filter_name":"noise_intensity", remove = TRUE)

# File names as categories
categories <- colnames(df)[3:16]
# Columns are fixed - 14 values
columns = c(3:16)
# Lines range every 10 gets a different chart
lines = c(1:10)
for(i in 1:20) {
  to_plot <- data.frame(
    'Files' = categories,
    '1'=unlist(df[lines[1], columns], use.names = FALSE),
    '2'=unlist(df[lines[2], columns], use.names = FALSE),
    '3'=unlist(df[lines[3], columns], use.names = FALSE),
    '4'=unlist(df[lines[4], columns], use.names = FALSE),
    '5'=unlist(df[lines[5], columns], use.names = FALSE),
    '6'=unlist(df[lines[6], columns], use.names = FALSE),
    '7'=unlist(df[lines[7], columns], use.names = FALSE),
    '8'=unlist(df[lines[8], columns], use.names = FALSE),
    '9'=unlist(df[lines[9], columns], use.names = FALSE),
    '10'=unlist(df[lines[10], columns], use.names = FALSE),
    check.names = FALSE
  )
  to_plot.m <- melt(to_plot, id.vars='Files')
  
  to_plot.m$value = as.numeric(readr::parse_number(to_plot.m$value, locale = readr::locale(decimal_mark = ",")))
  
  chart_name = df[lines[1], 1]
  filename = paste(chart_name, ".png", sep = '')

  max_value = signif.ceiling(max(to_plot.m[,3]), 1)
  max_value = max_value + (max_value %% 0.2)
  
  plt = ggplot(
    to_plot.m,
    aes(x=Files, y=value, group=variable, fill=variable)
  ) +
    ggtitle(plot_names[i]) +
    xlab("Imagens") +
    ylab("SSIM") +
    geom_col(
      aes(fill=variable),
      colour = "black",
      width = 0.5,
      position = position_dodge(width = 0.8),
      key_glyph = "polygon3"
    ) +
    scale_y_continuous(
      limits = c(0, max_value),
      breaks = seq(0,  max_value, length.out = 5),
      # breaks = seq(0,  1, length.out = 11),
      labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, nsmall = 2)
    ) +
    scale_fill_manual(
      name = "Variável",
      values = my_colors
    ) +
    theme_light() +
    my_theme

  plot(plt)
  ggsave(plt, path = './', filename = filename, units = 'px', width = 1800, height = 900, device = 'png', dpi = 110)

  lines <- lines + 10
}
?format
