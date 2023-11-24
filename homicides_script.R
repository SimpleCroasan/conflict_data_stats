library(readr)
library(ggplot2)
library(disk.frame)
library(dplyr)
library(purrr)


#probando como funcionan los data frames 

homicide1 <- homicide_list[[100]]$homicides
homicide1df <- homicide1 %>% collect
View(homicide1df)



#aplicando las funciones de dplyr para un data frame

homicides_per_year <- homicide1%>% 
  group_by(yy_hecho) %>% 
  summarise(n=n()) %>%
  collect()

  View(homicides_per_year)
  
  #para hacer un conteo acumulativo
  accumulative_list <- list()
  
  for(i in 1:100){
    homicide_variable <- homicide_list[[i]]$homicides
    datf_variable <- homicide_variable%>% 
      group_by(yy_hecho) %>% 
      summarise(n=n()) %>%
      collect()
    
    accumulative_list <- append(accumulative_list,list(datf_variable))
    
    
    
  }
  
#graficar


ggplot(homicides_per_year, aes(x = yy_hecho, y = n)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Tendencia de Homicidios por Año",
       x = "Año",
       y = "Número de Homicidios") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(homicides_per_year$n) + 1000), breaks = seq(0, max(homicides_per_year$n) + 1000, by = 1000)) +
  scale_x_continuous(breaks = seq(min(homicides_per_year$yy_hecho), max(homicides_per_year$yy_hecho), by = 1))


