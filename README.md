# conflict_data_stats
To analyze all the csvs use this command to have a list of 100 csvs per dataset

#code

#Analizar los nucleos para poder usar disk.frame

setup_disk.frame(workers = parallel::detectCores(logical=FALSE))


#Lista de archivos

files <- list.files(homicide_route, pattern = "*.csv", full.names = TRUE) 
filesk <- list.files(kidnapping_route, pattern = "*.csv", full.names = TRUE) 
filesd <- list.files(disappearances_route, pattern = "*.csv", full.names = TRUE) 
filesr <- list.files(recruitment_route, pattern = "*.csv", full.names = TRUE) 

homicide_list <- lapply(files , function(file){
  
  name <- basename(file)
  
  #convertir cada archivo en disk.frame
  
  homicides <- csv_to_disk.frame(file , outdir = file.path(tempdir(), name))
  
  list(name = name, homicides = homicides)
  
  
})


kidnapping_list <- lapply(filesk , function(file){
  
  name <- basename(file)
  
  #convertir cada archivo en disk.frame
  
  kidnapp <- csv_to_disk.frame(file , outdir = file.path(tempdir(), name))
  
  list(name = name, kidnapp = kidnapp)
  
  
})

disappearances_list <- lapply(filesd , function(file){
  
  name <- basename(file)
  
  #convertir cada archivo en disk.frame
  
  disappearances <- csv_to_disk.frame(file , outdir = file.path(tempdir(), name))
  
  list(name = name, disappearances = disappearances)
  
  
})

recruitment_list <- lapply(filesr , function(file){
  
  name <- basename(file)
  
  #convertir cada archivo en disk.frame
  
  recruitment <- csv_to_disk.frame(file , outdir = file.path(tempdir(), name))
  
  list(name = name, recruitment = recruitment)
  
  
})

homicide1 <- homicide_list[[100]]$homicides
homicide1df <- homicide1 %>% collect
View(homicide1df)

kidnapping1df <-kidnapping_list[[100]]$kidnapp %>% collect

recruitment1df <-recruitment_list[[100]]$recruitment %>% collect

disappearance1df <-disappearances_list[[100]]$disappearances%>% collect
