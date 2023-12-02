library(readr)
library(ggplot2)
library(disk.frame)
library(dplyr)
library(purrr)

#Importar archivos csv

#ruta de los archivos
homicide_route <- "../Homicidio.csv/verdata-homicidio-R99.csv"
kidnapping_route <- "../Secuestro.csv/verdata-secuestro-R99.csv"
disappearances_route <- "../Desaparicion.csv/verdata-desaparicion-R99.csv"
recruitment_route <-"../Reclutamiento.csv/verdata-reclutamiento-R99.csv"

#Leer csv
homicide1df <-read.csv(homicide_route)
kidnapping1df <-read.csv(kidnapping_route)
recruitment1df <-read.csv(recruitment_route)
disappearance1df <-read.csv(disappearances_route)

#exploracion de datos

#revisar si hay campos na en los datafrmaes
print(any(is.na(homicide1df)))

#revisar si hay campos nan en los dataframes
print(all(apply(homicide1df,2,function(x) all(!is.nan(x)))))



print(all(apply(kidnapping1df,2,function(x) all(!is.nan(x)))))
print(any(is.na(kidnapping1df)))



print(any(is.na(recruitment1df)))
print(all(apply(recruitment1df,2,function(x) all(!is.nan(x)))))



print(all(apply(disappearance1df,2,function(x) all(!is.nan(x)))))
print(any(is.na(disappearance1df)))



