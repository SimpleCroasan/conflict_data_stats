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



# PLOTS PER_YEAR
#plot total per year

ggplot(homicides_per_year, aes(x = yy_hecho, y = n, color = "homicidios")) +
  geom_line(size =0.8) +
  geom_line(data = disappearance_per_year, aes(x = yy_hecho, y = n, color = "desapariciones"),size=0.8)+
  geom_line(data = kidnapping_per_year, aes(x = yy_hecho, y = n, color = "secuestros"),size=0.8)+
  geom_line(data = recruitment_per_year, aes(x = yy_hecho, y = n, color = "reclutamiento"),size=0.8)+
  labs(title = "Número de hechos por año",
       x = "Año",
       y = "Número de hechos",
       color = "tipo de hecho") +
  theme_light() +
  scale_x_continuous(breaks = seq(min(homicides_per_group$yy_hecho), max(homicides_per_group$yy_hecho), by = 1))

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




#PLOT PER GROUP

#plot homicides_per_group

ggplot(homicides_per_group, aes(x = yy_hecho, y = n, color = Grupo)) +
  geom_line(size =0.8) +
  geom_line(data = homicides_per_year, aes(x = yy_hecho, y = n, color = "TOTAL"), color="black")+
  labs(title = "Número de Homicidios por Año y Grupo",
       x = "Año",
       y = "Número de Homicidios",
       color = "Grupo") +
  theme_light() +
  scale_x_continuous(breaks = seq(min(homicides_per_group$yy_hecho), max(homicides_per_group$yy_hecho), by = 1))