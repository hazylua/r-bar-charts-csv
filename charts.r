library(tidyr)
library(ggplot2)
library(reshape2)
library(readr)

library("ggsci")
library("viridis")

library(extrafont)

loadfonts(quiet = T)

mycolors = c("#8ACCFF", "#80C8FF", "#77C4FF", "#6DC0FF", "#64BCFF", "#5AB7FF", "#51B3FF", "#47AFFF", "#3EABFF", "#34A7FF")

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
plotted.m$value = as.numeric(readr::parse_number(plotted.m$value, locale = readr::locale(decimal_mark = ",")))

chart_name = df[lines[1], 1]
ggplot(plotted.m, aes(x=Files, y=value, group=variable, fill=variable)) +
  ggtitle("Filtro Gaussiano - Ruído Sal e Pimenta 50%") +
  geom_col(
    aes(fill=variable),
    # colour = "black",
    width = 0.8,
    position = position_dodge2(width=0.8, preserve = "single"),
    ) +
  scale_fill_manual(
    name = "Variável",
    values = mycolors
    ) +
  theme(
    plot.title = element_text(size=20, family = "Cambria", face = "bold", hjust = 0.5),
    axis.text.y = element_text(family = "Calibri", colour = "black", size = 12, vjust = 0.3),
    axis.text.x = element_text(family = "Calibri", colour = "black", size = 12, angle = 65, hjust = 1)
    )
png(filename = chart_name + ".png")
# lines <- lines + 10





















