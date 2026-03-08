# Librerías
library(rio)
library(tidyverse)
library(ggplot2)

# Importamos datos

gdp_clean <- import("gdp_clean.csv") # PBI per cápita
sci_clean <- import("sci_clean.csv") # índice de Capcidad Estatal
dem_clean <- import("dem_clean.csv") # Índice de Democracia Liberal

############################# PBI per cápita (US$) #############################

pbi_graph <- gdp_clean %>% 
  ggplot(
    aes(x = t_event, y = log(GDP), colour = `Country Name`)
  ) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  labs(
    title = "PBI per cápita antes y después de la intervención militar estadounidense",
    subtitle = "Variación del logaritmo del PBI per cápita",
    y = "Log(PBI per cápita)*",
    x = "Años relativos al inicio de la intervención",
    caption = "*Se expresa en logaritmos para facilitar la comparación de cambios relativos a lo largo del tiempo\n**No hay datos previos al 2000 para Afganistán\nFuente: Banco Mundial (2025)\nElaboración: Gianfranco Romero",
    color = "Países:"
  ) +
  theme_classic() +
  scale_x_continuous(breaks = gdp_clean$t_event,
                     expand = expansion(add = c(0, 0.1))) +
  scale_y_continuous(breaks = seq(3.5,10, by=0.5),
                     limits = c(3.5,10)) +
  scale_color_discrete(labels = c("Afganistán**", "Irak", "Libia", "Panamá")) +
  geom_vline(xintercept = 0, linewidth = 0.9, color = "gray") +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "gray",
                                          linetype = "dashed"))

# Exportar gráfico
ggsave("pbi_graph.svg", plot = pbi_graph, width = 9, height = 6)

########################## Índice de Capacidad Estatal ##########################

sci_graph <- sci_clean %>% 
  ggplot(
    aes(x=t_event, y = norm_sci, colour = Entity)
  ) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  labs(
    title = "Índice de Capacidad Estatal antes y después de la intervención militar estadounidense",
    subtitle = "Valor normalizado en una escala de 0 a 1*",
    y = "Índice de Capacidad Estatal",
    x = "Años relativos al inicio de la intervención",
    caption = "*El índice tiene una escala original de -2.31 a 2.96\n**Solo hay datos del índice hasta 2015\nFuente: Hanson, J. & Sigman, R. (2021)\nElaboración: Gianfranco Romero",
    color = "Países:"
  ) +
  theme_classic() +
  scale_x_continuous(breaks = unique(sci_clean$t_event),
                     expand = expansion(add = c(0, 0.1))) +
  scale_y_continuous(breaks = seq(0,1, by=0.1),
                     limits = c(0,1)) +
  scale_color_discrete(labels = c("Afganistán", "Irak", "Libia**", "Panamá")) +
  geom_vline(xintercept = 0, linewidth = 0.9, color = "gray") +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "gray",
                                          linetype = "dashed"))

# Exportar gráfico
ggsave("sci_graph.svg", plot = sci_graph, width = 9, height = 6)

######################### Índice de Democracia Liberal #########################

dem_graph <- dem_clean %>% 
  ggplot(
    aes(x=t_event, y = LD_Index, colour = Country)
  ) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  labs(
    title = "Índice de Democracia Liberal antes y después de la intervención militar estadounidense",
    subtitle = "Grado en que se logra el ideal de democracia liberal",
    y = "Índice de Democracia Liberal",
    x = "Años relativos al inicio de la intervención",
    caption = "Fuente: Coppedge, M. et al. (2025)\nElaboración: Gianfranco Romero",
    color = "Países:"
  ) +
  theme_classic() +
  scale_x_continuous(breaks = unique(dem_clean$t_event),
                     expand = expansion(add = c(0, 0.1))) +
  scale_y_continuous(breaks = seq(0,1, by=0.1),
                     limits = c(0,1)) +
  scale_color_discrete(labels = c("Afganistán", "Irak", "Libia", "Panamá")) +
  geom_vline(xintercept = 0, linewidth = 0.9, color = "gray") +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "gray",
                                          linetype = "dashed"))

# Exportar gráfico
ggsave("dem_graph.svg", plot = dem_graph, width = 9, height = 6)

























