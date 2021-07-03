my_colors = c(
  "#e1f5f3",
  "#b3e5fc",
  "#81d4fa",
  "#4fc3f7",
  "#29b6f6",
  "#03a9f4",
  "#039be5",
  "#0288d1",
  "#0277bd",
  "#01579b"
)

my_theme <- theme(
  plot.title = element_text(
    size = 24,
    family = "Cambria",
    face = "bold",
    hjust = 0.5,
    margin = margin(20, 0, 20, 0)
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
    family = "Ubuntu Mono",
    size = 16,
    colour = "black",
    angle = 45,
    hjust = 1
  ),
  legend.title = element_text(
    family = "Calibri",
    colour = "black",
    size = 14,
    hjust = 0.5
  ),
  legend.text = element_text(
    family = "Calibri",
    colour = "black",
    size = 12
  ),
  legend.direction = "vertical",
  legend.position = "right",
  axis.ticks = element_line(size = 0.5),
  panel.grid.minor = element_line(size = 0.5),
  panel.grid.major = element_line(size = 0.5),
  panel.border = element_rect(size = 1)
)