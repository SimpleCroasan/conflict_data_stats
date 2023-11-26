library(sf)
library(tmap)
library(readr)
library(ggplot2)
library(disk.frame)
library(dplyr)
library(purrr)

map <- st_read("MGN2022_MPIO_POLITICO/MGN_MPIO_POLITICO.shp")
map2 <- map[,2] 
plot(map2)











#plot homicides_per_year

ggplot(homicides_per_year, aes(x = yy_hecho, y = n)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Tendencia de Homicidios por Año",
       x = "Año",
       y = "Número de Homicidios") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(homicides_per_year$n) + 1000), breaks = seq(0, max(homicides_per_year$n) + 1000, by = 1000)) +
  scale_x_continuous(breaks = seq(min(homicides_per_year$yy_hecho), max(homicides_per_year$yy_hecho), by = 1))

