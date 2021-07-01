library(tidyr)
library(ggplot2)
library(reshape2)
library(readr)

library("ggsci")
library("viridis")

library(extrafont)

loadfonts(quiet = T)

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

test = scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", 
                            midpoint = median(v$value), space = "rgb", guide = "colourbar")

df <- read.csv(file='./ssim.csv', sep = ';', fileEncoding = "UTF-8-BOM")
df <- df %>% unite("with_on", "filter_name":"noise_intensity", remove = TRUE)

lines = c(1:10)
columns = c(3:16)

df[lines, columns]

# File names as categories
categories <- colnames(df)[3:16]
categories


plotted <- data.frame('Files' = categories,
                      '1'=unlist(df[lines[1], columns], use.names = FALSE),
                      '2'=unlist(df[lines[2], columns], use.names = FALSE),
                      '3'=unlist(df[lines[3], columns], use.names = FALSE),
                      '4'=unlist(df[lines[4], columns], use.names = FALSE),
                      '5'=unlist(df[lines[5], columns], use.names = FALSE),
                      '6'=unlist(df[lines[6], columns], use.names = FALSE),
                      '7'=unlist(df[lines[7], columns], use.names = FALSE),
                      '8'=unlist(df[lines[8], columns], use.names = FALSE),
                      '9'=unlist(df[lines[9], columns], use.names = FALSE),
                      '10'=unlist(df[lines[10], columns], use.names = FALSE), check.names = FALSE)

plotted.m <- melt(plotted, id.vars='Files')

# plotted.m$variable = as.character(plotted.m$variable)
# plotted.m$variable = as.numeric(readr::parse_number(plotted.m$variable, locale = readr::locale(decimal_mark = ",")))
plotted.m$value = as.numeric(readr::parse_number(plotted.m$value, locale = readr::locale(decimal_mark = ",")))

chart_name = df[lines[1], 1]
filename = paste(chart_name, ".png", sep = '')
plt = ggplot(plotted.m, aes(x=Files, y=value, group=variable, fill=variable)) +
  ggtitle("Filtro Gaussiano - Ruído Sal e Pimenta 50%") +
  xlab("Imagens") +
  ylab("SSIM") +
  geom_col(
    aes(fill=variable),
    colour = "black",
    width = 0.5,
    position = position_dodge(width = 0.8),
    key_glyph = "polygon3"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  # scale_y_continuous(n.breaks = 10) +
  scale_fill_manual(name = "Variável", values = my_colors) +
  theme_light() +
  theme(
    plot.title = element_text(size=24, family = "Cambria", face = "bold", hjust = 0.5, margin = margin(20, 0, 20, 0)),
    axis.title.x = element_text(family = "Calibri", size = 16, face = "bold", margin = margin(10, 0, 10, 0)),
    axis.title.y = element_text(family = "Calibri", size = 16, face = "bold", margin = margin(0, 10, 0, 10)),
    axis.text.y = element_text(family = "Calibri", size = 14, colour = "black", vjust = 0.5, margin = margin(0, 5, 0, 0)),
    axis.text.x = element_text(family = "Ubuntu Mono", size = 16, colour = "black",angle = 45, hjust = 1),
    legend.title = element_text(family = "Calibri", colour = "black", size = 14, hjust = 0.5),
    legend.text = element_text(family = "Calibri", colour = "black", size = 12),
    legend.direction = "vertical",
    legend.position = "right",
    axis.ticks = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.5),
    panel.grid.major = element_line(size = 0.5),
    panel.border = element_rect(size=1)
  )


plot(plt)
ggsave(plt, path = './', filename = filename, units = 'px', width = 1800, height = 900, device = 'png', dpi = 110)
# lines <- lines + 10
dev.off()


