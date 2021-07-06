library(tidyr)
library(ggplot2)
library(reshape2)
library(readr)
library(extrafont)
library("ggsci")
library("viridis")

loadfonts(quiet = T)

df <- read.csv(file='./rmse.csv', sep = ';', fileEncoding = "UTF-8-BOM")
df <- df %>% unite("with_on", "filter_name":"noise_intensity", remove = TRUE)

path <- './data/rmse'

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
  
  max_value = max(to_plot.m[,3])
  size_factor = floor(log10(max(to_plot.m[,3])))
  max_value = round.choose(max_value, 5*10^(size_factor - 1), 1)
  # max_value = signif.ceiling(max(to_plot.m[,3]), 1)
  # max_value = max_value + (max_value %% 0.2)
  
  plt = ggplot(
    to_plot.m,
    aes(x=Files, y=value, group=variable, fill=variable)
  ) +
    ggtitle(plot_names[i]) +
    xlab("Imagens") +
    ylab("RMSE") +
    geom_col(
      aes(fill=variable),
      colour = "black",
      width = 0.5,
      position = position_dodge(width = 0.8),
      key_glyph = "polygon3"
    ) +
    scale_y_continuous(
      limits = c(0, max_value),
      breaks = seq(0,  max_value, length.out = 11),
      # breaks = seq(0,  1, length.out = 11),
    ) +
    scale_fill_manual(
      name = "Variável",
      values = my_colors
    ) +
    theme_light() +
    my_theme
  
  # plot(plt)
  ggsave(plt, path = path, filename = filename, units = 'px', width = 1800, height = 900, device = 'png', dpi = 110)
  
  lines <- lines + 10
}
