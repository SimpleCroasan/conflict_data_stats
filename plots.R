library(sf)
library(tmap)
library(readr)
library(ggplot2)
library(disk.frame)
library(dplyr)
library(purrr)

# -*- coding: utf-8 -*-


#mapa homicidios historico por municipio
map <- st_read("Mapas_dept/DepartamentosVeredas.shp")

homicides_group_dept$DPTO_CCDGO<-factor(homicides_group_dept$dept_code_hecho)
homicides_group_dept$dept_code_hecho<-NULL
homicides_group_dept$homicidios<-homicides_group_dept$n
homicides_group_dept$n<-NULL

map <- map[,c(1,2)]

map <- left_join(map, homicides_group_dept)

summary(map)

qtm(map,"homicidios", fill.breaks = c(0,15000,25000, 50000,75000,100000,125000,150000)) # Para todo el pais
#mapa calor departamento

map2 <- st_read("Mapas_dept/DepartamentosVeredas.shp")

homicides_per_group_dept$DPTO_CCDGO<-factor(homicides_per_group_dept$dept_code_hecho)
homicides_per_group_dept$dept_code_hecho<-NULL
homicides_per_group_dept$Grupo_ar<-homicides_per_group_dept$Grupo
homicides_per_group_dept$Grupo<-NULL

map2 <- map2[,c(1,2)]

map2 <- left_join(map2, homicides_per_group_dept)

tm_shape(map2) +
  tm_borders() +
  tm_fill("Grupo_ar", palette = "Set3") +
  tm_layout(legend.position = c("right", "bottom"))

#mapa secuestros

map3 <- st_read("Mapas_dept/DepartamentosVeredas.shp")

kidnapp_group_dept$DPTO_CCDGO<-factor(kidnapp_group_dept$dept_code_hecho)
kidnapp_group_dept$dept_code_hecho<-NULL
kidnapp_group_dept$secuestros<-kidnapp_group_dept$n
kidnapp_group_dept$n<-NULL

map3 <- map3[,c(1,2)]

map3 <- left_join(map3, kidnapp_group_dept)

summary(map3)

qtm(map3,"secuestros", fill.breaks = c(0,1000,3000, 5000,7000,11000)) # Para todo el pais
#mapa calor departamento

map4 <- st_read("Mapas_dept/DepartamentosVeredas.shp")

kidnapp_per_group_dept$DPTO_CCDGO<-factor(kidnapp_per_group_dept$dept_code_hecho)
kidnapp_per_group_dept$dept_code_hecho<-NULL
kidnapp_per_group_dept$Grupo_ar<-kidnapp_per_group_dept$Grupo
kidnapp_per_group_dept$Grupo<-NULL

map4 <- map4[,c(1,2)]

map4 <- left_join(map4, kidnapp_per_group_dept)

tm_shape(map4) +
  tm_borders() +
  tm_fill("Grupo_ar", palette = "Set3") +
  tm_layout(legend.position = c("right", "bottom"))

#reclutamiento por depa

map5 <- st_read("Mapas_dept/DepartamentosVeredas.shp")

recruitment_group_dept$DPTO_CCDGO<-factor(recruitment_group_dept$dept_code_hecho)
recruitment_group_dept$dept_code_hecho<-NULL
recruitment_group_dept$reclutamiento<-recruitment_group_dept$n
recruitment_group_dept$n<-NULL

map5 <- map5[,c(1,2)]

map5 <- left_join(map5, recruitment_group_dept)

summary(map5)

qtm(map5,"reclutamiento", fill.breaks = c(0,500,1000, 1500,2000,2500,3000,3500))





# PLOTS PER_YEAR
#plot total per year

ggplot(homicides_per_year, aes(x = yy_hecho, y = n, color = "homicidios")) +
  geom_line(data = total_per_year, aes(x = yy_hecho, y = total, linetype = "TOTAL"),color="black")+
  geom_line(size =0.8) +
  geom_line(data = disappearance_per_year, aes(x = yy_hecho, y = n, color = "desapariciones"),size=0.8)+
  geom_line(data = kidnapping_per_year, aes(x = yy_hecho, y = n, color = "secuestros"),size=0.8)+
  geom_line(data = recruitment_per_year, aes(x = yy_hecho, y = n, color = "reclutamiento"),size=0.8)+
  labs(title = "Número de hechos por año",
       x = "Año",
       y = "Número de hechos",
       color = "tipo de hecho") +
  theme_light() +
  scale_x_continuous(breaks = seq(min(homicides_per_year$yy_hecho), max(homicides_per_year$yy_hecho), by = 3))

#plot homicides_per_year

ggplot(homicides_per_year, aes(x = yy_hecho, y = n)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Tendencia de Homicidios por Año",
       x = "Año",
       y = "Número de Homicidios") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(homicides_per_year$n) + 1000), breaks = seq(0, max(homicides_per_year$n) + 1000, by = 1000)) +
  scale_x_continuous(breaks = seq(min(homicides_per_year$yy_hecho), max(homicides_per_year$yy_hecho), by = 3))




#PLOT PER GROUP

#plot homicides_per_group

ggplot(homicides_per_group, aes(x = yy_hecho, y = n, color = Grupo)) +
  geom_line(size =0.8) +
  geom_line(data = homicides_per_year, aes(x = yy_hecho, y = n, linetype = "TOTAL"),color="black")+
  labs(title = "Número de Homicidios por Año y Grupo",
       x = "Año",
       y = "Número de Homicidios",
       color = "Grupo") +
  theme_light() +
  scale_x_continuous(breaks = seq(min(homicides_per_group$yy_hecho), max(homicides_per_group$yy_hecho), by = 3))


#plot kidnapping per group


ggplot(kidnapping_per_group, aes(x = yy_hecho, y = n, color = Grupo)) +
  geom_line(size =0.8) +
  geom_line(data = kidnapping_per_year, aes(x = yy_hecho, y = n, linetype = "TOTAL"), color="black")+
  labs(title = "Número de secuestros por Año y Grupo",
       x = "Año",
       y = "Número de secuestros",
       color = "Grupo") +
  theme_light() +
  scale_x_continuous(breaks = seq(min(kidnapping_per_group$yy_hecho), max(kidnapping_per_group$yy_hecho), by = 3))

#plot disappearence_per_group

ggplot(disappearance_per_group, aes(x = yy_hecho, y = n, color = Grupo)) +
  geom_line(size =0.8) +
  geom_line(data = disappearance_per_year, aes(x = yy_hecho, y = n, linetype = "TOTAL"),color="black")+
  labs(title = "Número de desapariciones por Año y Grupo",
       x = "Año",
       y = "Número de desapariciones",
       color = "Grupo") +
  theme_light() +
  scale_x_continuous(breaks = seq(min(disappearance_per_group$yy_hecho), max(disappearance_per_group$yy_hecho), by = 3))


