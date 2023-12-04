
## Working with maps
# https://geoportal.dane.gov.co/descargas/mgn_2022/
library(sf)

map <- st_read("Mapas/MGN2022_DPTO_POLITICO/MGN_DPTO_POLITICO.shp")
plot(map)
map2 <- map[,1] # Solo el mapa en terminos de su codigo
plot(map2)

# https://geoportal.dane.gov.co/descargas/descarga_veredas/
map2 <- st_read("Mapas/ShapeFileVeredas/CRVeredas_2020.shp")
## RETO (Grafique los municipios de Colombia (use unique values))


#### Load Data
load('CensoAgro.RData')

P.Total.Ver<-aggregate(P_S7P100~COD_VEREDA, FUN = sum, data = Acuicultura)
P.Total.Mun<-aggregate(P_S7P100~P_MUNIC, FUN = sum, data = Acuicultura)
P.Total.Dep<-aggregate(P_S7P100~P_DEPTO, FUN = sum, data = Acuicultura)

summary(P.Total.Ver)
summary(P.Total.Mun)
summary(P.Total.Dep)

library(sf)
map <- st_read("Mapas/MunicipiosVeredas.shp")
summary(map)
plot(map)


#reprojected_World <- st_transform(map, 'EPSG:32617')
#reprojected_World2 <- st_transform(map, 'WGS84')
#summary(map)

### Proyeccion de puntos
coord <- st_coordinates(st_centroid(map$geometry))
head(P.Total.Mun)
map <- data.frame(map, coord)
colnames(map)[7] <- "LON"
colnames(map)[8] <- "LAT"

datab <- map[,c(1,7,8)]
head(datab)
colnames(datab)[1] <- "P_MUNIC" # Rename to match with P.Total.Mun
head(datab)


# Merge con P.Total.Mun
T.A.Mun <- merge(P.Total.Mun,  datab)
summary(T.A.Mun)


# Plot Latitude and Longitude data
library('ggplot2')
ggplot(data = na.omit(T.A.Mun), aes(LON, LAT)) +
        geom_point(pch=3, col="black") + 
        theme_bw()

# Cut data in 5 categories
T.A.Mun$Int<-cut(T.A.Mun$P_S7P100, breaks = c(0,200,5000,50000,100000,12000000))
summary(T.A.Mun$Int)


airPal <- colorRampPalette(c("springgreen1", "sienna3", "gray5"))(6) # choose colormap
ggplot(data = na.omit(T.A.Mun), aes(LON, LAT, size = Int, fill = Int)) + 
  geom_point(pch=21, col="black") + theme_bw() +
  scale_fill_manual(values=airPal)

##########
####### Proyección con Polígonos
##########

map2 <- st_read("Mapas/MunicipiosVeredas.shp")
summary(map2)

# Cambiar nombre y tipo de clase de DPTOMPIO
P.Total.Mun$DPTOMPIO<-factor(P.Total.Mun$P_MUNIC)
P.Total.Mun$P_MUNIC<-NULL
P.Total.Mun$Acuiq<-P.Total.Mun$P_S7P100
P.Total.Mun$P_S7P100<-NULL


map2 <- map2[,c(1,2,3)]
summary(map2)


library(dplyr)
map2 <- left_join(map2, P.Total.Mun)

library(tmap)
qtm(map2,"Acuiq", fill.breaks = c(0,1000,10000,50000,100000,1000000,5000000,10000000,15000000)) # Para todo el pais
qtm(map2[map2$DPTO_CCDGO=="68",],"Acuiq", fill.breaks = c(0,5000,10000,50000,100000,500000,1000000,5000000)) # Para Santander
qtm(map2[map2$DPTO_CCDGO=="05",],"Acuiq", fill.breaks = c(0,5000,10000,50000,100000,500000,1000000,5000000)) # Para Anqtioquia

#https://geoportal.dane.gov.co/descargas/descarga_mgn/CatalogoDeObjetos.pdf


plot(map, col = "darkgrey") 
sel <- map$DPTO_CCDGO == "68"
plot(map[ sel, ], col = "red", add = TRUE) # add selected zones to map


# ##################################################
# ##### Loading Google Maps
# ##################################################
# library(classInt)
# nClasses <- 5
# intervals <- classIntervals(T.A.Mun$P_S7P100, n=nClasses, style="fisher")
# nClasses <- length(intervals$brks) - 1
# op <- options(digits=4)
# tab <- print(intervals)
# options(op)
# 
# dent <- c(1,2,3,4,5,6)
# dentAQ <- dent[seq_len(nClasses)]
# idx <- findCols(intervals)
# cexAcui <- dentAQ[idx]
# 
# T.A.Mun$classAcui <- factor(names(tab)[idx])
# 
# ColBox <- st_bbox(st_as_sf(map))
# ColBox
# names(ColBox) <- c("left", "bottom", "right", "top")
# 
# library(ggmap)
# ColGG <- get_map(c(ColBox), maptype="satellite", source="google")
# 
# ggmap(ColGG) +
#   geom_point(data=na.omit(T.A.Mun),
#              aes(LON, LAT, size=classAcui, fill=classAcui),
#              pch=21, col="black") +
#   scale_fill_manual(values=airPal) +
#   scale_size_manual(values=dentAQ*2)








