library(readr)
library(ggplot2)
library(disk.frame)
library(dplyr)
library(purrr)









#volver caracter la columna de departamentos y municipios y concatenar un cero a la izquierda para poder luego usar map

homicide1df <- homicide1df %>% mutate(
  dept_code_hecho = ifelse(dept_code_hecho < 10 , sprintf("%02d", dept_code_hecho) %>% as.character(), as.character(dept_code_hecho)),
  muni_code_hecho = ifelse(muni_code_hecho < 10 , sprintf("%02d", muni_code_hecho) %>% as.character(), as.character(muni_code_hecho))
)

disappearance1df <- disappearance1df %>% mutate(
  dept_code_hecho = ifelse(dept_code_hecho < 10 , sprintf("%02d", dept_code_hecho) %>% as.character(), as.character(dept_code_hecho)),
  muni_code_hecho = ifelse(muni_code_hecho < 10 , sprintf("%02d", muni_code_hecho) %>% as.character(), as.character(muni_code_hecho))
)

kidnapping1df <- kidnapping1df %>% mutate(
  dept_code_hecho = ifelse(dept_code_hecho < 10 , sprintf("%02d", dept_code_hecho) %>% as.character(), as.character(dept_code_hecho)),
  muni_code_hecho = ifelse(muni_code_hecho < 10 , sprintf("%02d", muni_code_hecho) %>% as.character(), as.character(muni_code_hecho))
)

recruitment1df <- recruitment1df %>% mutate(
  dept_code_hecho = ifelse(dept_code_hecho < 10 , sprintf("%02d", dept_code_hecho) %>% as.character(), as.character(dept_code_hecho)),
  muni_code_hecho = ifelse(muni_code_hecho < 10 , sprintf("%02d", muni_code_hecho) %>% as.character(), as.character(muni_code_hecho))
)








#aplicando las funciones de dplyr para un data frame

homicides_per_year <- homicide1%>% 
  group_by(yy_hecho) %>% 
  summarise(n=n()) %>%
  collect()
  View(homicides_per_year)
  



