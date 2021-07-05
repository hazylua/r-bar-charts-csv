ssim <-
  read.csv(file = './ssim.csv',
           sep = ';',
           fileEncoding = "UTF-8-BOM")
ssim <-
  ssim %>% unite("with_on", "filter_name":"noise_intensity", remove = TRUE)

rmse <-
  read.csv(file = './rmse.csv',
           sep = ';',
           fileEncoding = "UTF-8-BOM")
rmse <-
  rmse %>% unite("with_on", "filter_name":"noise_intensity", remove = TRUE)

compare_theme <- theme(
  plot.title = element_text(
    size = 16,
    family = "Cambria",
    face = "bold",
    hjust = 0.5,
  ),
  axis.title.x = element_text(
    family = "Calibri",
    size = 16,
    face = "bold",
    margin = margin(10, 0, 10, 0)
  ),
  axis.title.y = element_text(
    family = "Calibri",
    size = 16,
    face = "bold",
    margin = margin(0, 10, 0, 10)
  ),
  axis.text.y = element_text(
    family = "Calibri",
    size = 14,
    colour = "black",
    vjust = 0.5,
    margin = margin(0, 5, 0, 0)
  ),
  axis.text.x = element_text(
    family = "Calibri",
    size = 14,
    colour = "black",
    margin = margin(5, 0, 5, 0)
  ),
  legend.title = element_text(
    family = "Calibri",
    colour = "black",
    face = "bold",
    size = 14,
    hjust = 0.5
  ),
  legend.text = element_text(
    family = "Calibri",
    colour = "black",
    size = 12
  ),
  legend.position = c(1, 1),
  legend.justification = c(1.1, 1.3),
  legend.background = element_rect(fill=my_colors[2], linetype="solid", size=0.5, colour = my_colors[10]),
  axis.ticks = element_line(size = 0.5),
  panel.grid.minor = element_line(size = 0.5),
  panel.grid.major = element_line(size = 0.5),
  panel.border = element_rect(size = 1),
)

# Column
column <- c(6)

# File names as categories
ssim_category <- paste("ssim_", colnames(ssim)[column], sep = '')
rmse_category <- paste("rmse_", colnames(rmse)[column], sep = '')

# Lines
lines = c(1:10)
lines <- lines + 130

a = round.choose(lines[1], roundTo = 10, 0) / 10
a = a + 1

to_plot_ssim = data.frame(
  "File" = ssim_category,
  '1' = unlist(ssim[lines[1], column], use.names = FALSE),
  '2' = unlist(ssim[lines[2], column], use.names = FALSE),
  '3' = unlist(ssim[lines[3], column], use.names = FALSE),
  '4' = unlist(ssim[lines[4], column], use.names = FALSE),
  '5' = unlist(ssim[lines[5], column], use.names = FALSE),
  '6' = unlist(ssim[lines[6], column], use.names = FALSE),
  '7' = unlist(ssim[lines[7], column], use.names = FALSE),
  '8' = unlist(ssim[lines[8], column], use.names = FALSE),
  '9' = unlist(ssim[lines[9], column], use.names = FALSE),
  '10' = unlist(ssim[lines[10], column], use.names = FALSE),
  check.names = FALSE
)

to_plot_rmse <- data.frame(
  'File' = rmse_category,
  '1' = unlist(rmse[lines[1], column], use.names = FALSE),
  '2' = unlist(rmse[lines[2], column], use.names = FALSE),
  '3' = unlist(rmse[lines[3], column], use.names = FALSE),
  '4' = unlist(rmse[lines[4], column], use.names = FALSE),
  '5' = unlist(rmse[lines[5], column], use.names = FALSE),
  '6' = unlist(rmse[lines[6], column], use.names = FALSE),
  '7' = unlist(rmse[lines[7], column], use.names = FALSE),
  '8' = unlist(rmse[lines[8], column], use.names = FALSE),
  '9' = unlist(rmse[lines[9], column], use.names = FALSE),
  '10' = unlist(rmse[lines[10], column], use.names = FALSE),
  check.names = FALSE
)

to_plot_ssim.m <- melt(to_plot_ssim, id.vars = 'File')
to_plot_rmse.m <- melt(to_plot_rmse, id.vars = 'File')

to_plot_ssim.m$value = as.numeric(readr::parse_number(to_plot_ssim.m$value, locale = readr::locale(decimal_mark = ",")))
to_plot_rmse.m$value = as.numeric(readr::parse_number(to_plot_rmse.m$value, locale = readr::locale(decimal_mark = ",")))

chart_name = ssim[lines[1], 1]

ssim_name = as.character(to_plot_ssim.m[1, 1])
# get just the image file name
img_name = substr(ssim_name, 6, nchar(ssim_name))
ssim_name = substr(ssim_name, 1, nchar(ssim_name)-4)

rmse_name = as.character(to_plot_rmse.m[1, 1])
rmse_name = substr(rmse_name, 1, nchar(rmse_name)-4)

filename_ssim = paste(chart_name, "_", ssim_name, ".png", sep = '')
filename_rmse = paste(chart_name, "_", rmse_name, ".png", sep = '')

##
max_ssim = signif.ceiling(max(to_plot_ssim.m[, 3]), 1) + 0.1
#
min_ssim = round.choose(min(to_plot_ssim.m[, 3]), 0.1, 0)

##
max_rmse = max(to_plot_rmse.m[, 3])
max_factor = floor(log10(max_rmse))
max_rmse = round.choose(max_rmse, 5 * 10 ^ (max_factor - 1), 1)
#
min_rmse = min(to_plot_rmse.m[, 3])
min_factor = floor(log10(min_rmse))
min_rmse = round.choose(min_rmse, 5 * 10 ^ (min_factor - 1), 0)

# ssim_plot_title = paste(plot_names[a] " - Na imagem \"", img_name, "\"", sep = '')
plt1 = ggplot(to_plot_ssim.m,
              aes(x = variable, y = value, group = File, fill = img_name)) +
  ggtitle(element_blank()) +
  xlab("Variáveis") +
  ylab("SSIM") +
  geom_line(color = my_colors[10],
            key_glyph = "polygon3",
            size = 1) +
  geom_point(color = my_colors[10]) +
  scale_y_continuous(
    limits = c(min_ssim, max_ssim),
    breaks = seq(min_ssim,  max_ssim, length.out = 5),
  ) +
  scale_fill_manual(name = "Imagem",
                    values = my_colors) +
  theme_light() +
  compare_theme

plot(plt1)

plt2 = ggplot(to_plot_rmse.m,
              aes(x = variable, y = value, group = File, fill = img_name)) +
  ggtitle(element_blank()) +
  xlab("Variáveis") +
  ylab("RMSE") +
  geom_line(color = my_colors[10],
            key_glyph = "polygon3",
            size = 1) +
  geom_point(color = my_colors[10],
             size = 2) +
  scale_y_continuous(
    limits = c(min_rmse, max_rmse),
    breaks = seq(min_rmse,  max_rmse, length.out = 6),
  ) +
  scale_fill_manual(name = "Imagem",
                    values = my_colors) +
  theme_light() +
  compare_theme

plot(plt2)

ggsave(
  plt1,
  path = './zoomed',
  filename = filename_ssim,
  units = 'px',
  width = 1800,
  height = 1000,
  device = 'png',
  dpi = 120
)
ggsave(
  plt2,
  path = './zoomed',
  filename = filename_rmse,
  units = 'px',
  width = 1800,
  height = 1000,
  device = 'png',
  dpi = 120
)
