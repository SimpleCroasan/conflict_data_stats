library(readr)
library(ggplot2)
library(disk.frame)
library(dplyr)
library(purrr)

#revisar si el id conecta diferentes hechos
valores_comunes <- homicide1df$match_group_id %in% kidnapping1df$match_group_id
valores_comunes2 <- intersect(homicide1df$match_group_id, kidnapping1df$match_group_id)
print(any(valores_comunes))
print(valores_comunes2)
fila_deseada <- filter(kidnapping1df,match_group_id=="af42ca884de6db37305fa0ed47cb82a50f523aea")


#volver caracter la columna de departamentos y municipios y concatenar un cero a la izquierda para poder luego usar map
#quitar columnas no deseadas

#homicide
homicide1df <- homicide1df %>% mutate(
  dept_code_hecho = ifelse(dept_code_hecho < 10 , sprintf("%02d", dept_code_hecho) %>% as.character(), as.character(dept_code_hecho)),
  muni_code_hecho = ifelse(dept_code_hecho < 10 , as.character() %>% paste("0", muni_code_hecho, sep = "") , as.character(muni_code_hecho))
)
homicide1df <-homicide1df[,-c(3:36,1,51)]
#homicide1df$muni_code_hecho <- substr(homicide1df$muni_code_hecho, nchar(homicide1df$muni_code_hecho) - 2, nchar(homicide1df$muni_code_hecho))


#disappearance
disappearance1df <- disappearance1df %>% mutate(
  dept_code_hecho = ifelse(dept_code_hecho < 10 , sprintf("%02d", dept_code_hecho) %>% as.character(), as.character(dept_code_hecho)),
  muni_code_hecho = ifelse(muni_code_hecho < 10 , sprintf("%02d", muni_code_hecho) %>% as.character(), as.character(muni_code_hecho))
)
disappearance1df <-disappearance1df[,-c(3:36,1,53)]
#disappearance1df$muni_code_hecho <- substr(disappearance1df$muni_code_hecho, nchar(disappearance1df$muni_code_hecho) - 2, nchar(disappearance1df$muni_code_hecho))

#kidnapping

kidnapping1df <- kidnapping1df %>% mutate(
  dept_code_hecho = ifelse(dept_code_hecho < 10 , sprintf("%02d", dept_code_hecho) %>% as.character(), as.character(dept_code_hecho)),
  muni_code_hecho = ifelse(muni_code_hecho < 10 , sprintf("%02d", muni_code_hecho) %>% as.character(), as.character(muni_code_hecho))
)

kidnapping1df <-kidnapping1df[,-c(3:36,1,51)]
#kidnapping1df$muni_code_hecho <- substr(kidnapping1df$muni_code_hecho, nchar(kidnapping1df$muni_code_hecho) - 2, nchar(kidnapping1df$muni_code_hecho))


#recruitment
recruitment1df <- recruitment1df %>% mutate(
  dept_code_hecho = ifelse(dept_code_hecho < 10 , sprintf("%02d", dept_code_hecho) %>% as.character(), as.character(dept_code_hecho)),
  muni_code_hecho = ifelse(muni_code_hecho < 10 , sprintf("%02d", muni_code_hecho) %>% as.character(), as.character(muni_code_hecho))
)

recruitment1df <-recruitment1df[,-c(3:36,1,51)]
#recruitment1df$muni_code_hecho <- substr(recruitment1df$muni_code_hecho, nchar(recruitment1df$muni_code_hecho) - 2, nchar(recruitment1df$muni_code_hecho))







#aplicando las funciones de dplyr para un data frame
#datos por municipio 

homicides_group_year_mun <- homicide1df %>% 
  group_by(yy_hecho,muni_code_hecho, dept_code_hecho) %>%
  summarise(n=n())

homicides_group_mun <- homicide1df %>% 
  group_by(muni_code_hecho) %>%
  summarise(n=n())

# datos por año

disappearance_per_year <- disappearance1df%>% 
  group_by(yy_hecho) %>% 
  summarise(n=n()) 

homicides_per_year <- homicide1df%>% 
  group_by(yy_hecho) %>% 
  summarise(n=n()) 

kidnapping_per_year <- kidnapping1df%>% 
  group_by(yy_hecho) %>% 
  summarise(n=n()) 

recruitment_per_year <- recruitment1df%>% 
  group_by(yy_hecho) %>% 
  summarise(n=n()) 
  

#datos por grupo

homicides_per_group <- homicide1df %>%
  mutate(Grupo = case_when(
    
    
    grepl("GUE",p_str) ~ "GUERRILLA",
    grepl("PARA",p_str) ~ "PARAMILITARES",
    grepl("EST",p_str) ~ "ESTADO",
    grepl("multiple",p_str) ~ "MULTIPLE",
    TRUE ~ "OTRO"
  )) %>%
  group_by(yy_hecho,Grupo) %>%
  summarise(n=n())


kidnapping_per_group <- kidnapping1df %>%
  mutate(Grupo = case_when(
    
    
    grepl("GUE",p_str) ~ "GUERRILLA",
    grepl("PARA",p_str) ~ "PARAMILITARES",
    grepl("EST",p_str) ~ "ESTADO",
    grepl("multiple",p_str) ~ "MULTIPLE",
    TRUE ~ "OTRO"
  )) %>%
  group_by(yy_hecho,Grupo) %>%
  summarise(n=n())

#secuestros que llevan a homicidios
kidnapp_homicidie <-inner_join(homicide1df, kidnapping1df, by = "match_group_id")

kidnapping_homicide_per_group <- kidnapp_homicidie %>%
  mutate(Grupo = case_when(
    
    
    grepl("GUE",p_str.x) ~ "GUERRILLA",
    grepl("PARA",p_str.x) ~ "PARAMILITARES",
    grepl("EST",p_str.x) ~ "ESTADO",
    grepl("multiple",p_str.x) ~ "MULTIPLE",
    TRUE ~ "OTRO"
  )) %>%
  group_by(yy_hecho.x,Grupo) %>%
  summarise(n=n())
