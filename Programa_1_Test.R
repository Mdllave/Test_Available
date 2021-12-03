
#####################################################################################################
############### CLASE AFI - GEOSPATIAL##########################################################
####################################################################################################

#packages_list=c("rgdal","rgeos","raster","maptools","automap","dplyr","dplyr","ggmap",
#                "leaflet","leaflet","ggplot2","spdep","leaflet.extras","sp","geosphere","SpatialEpi",
#                 "rsatscan","earth","ProbitSpatial","Matrix")
#
#for (pkg in packages_list){
#  print(paste0("check: ",pkg))
#  if(!require(pkg,character.only = T)){
#    print(paste0("need to install: ",pkg))
#    install.packages(pkg)
#  }
#  library(pkg,character.only = T)
#}


library(rgdal)               
library(rgeos)               
library(raster)
library(maptools)
library(automap)
library(dplyr)
library(ggmap)
library(leaflet)
library(ggplot2)
library(leaflet.extras)
library(sp)
library(spdep)
library(geosphere)
library(SpatialEpi)
library(rsatscan)            
library(earth)
library(spgwr)

#DIAPOSITIVA 9
##############

#Leo mis ficheros SHP
Poligonos_CP <-readOGR(dsn="C:/Users/AX29501/Desktop/Doctorado/6_Cursos_Docencia/04_Clases_AFI/Clase/0_Codigo_Postal_2016","Capa_CP_Poligonos")
Poligonos_PR <-readOGR(dsn="C:/Users/AX29501/Desktop/Doctorado/6_Cursos_Docencia/04_Clases_AFI/Clase/0_Prov","Provincias_ETRS89_30N")

#
par(mfrow = c(1, 2))
plot(Poligonos_PR,main="Poligonos_PR")

#
Poligonos_PR@proj4string

#
Poligonos_CP_WGS84 <- spTransform(Poligonos_CP, CRS("+proj=longlat +datum=WGS84"))
Poligonos_PR_WGS84 <- spTransform(Poligonos_PR, CRS("+proj=longlat +datum=WGS84"))
plot(Poligonos_CP_WGS84,main="Poligonos_CP_WGS84")

#Leo fichero de datos. Ver sesión 2. 
db_mod <- read.csv("C:/Users/AX29501/Desktop/Doctorado/6_Cursos_Docencia/04_Clases_AFI/Clase/db_mod.csv")
db_mod$id <- 1: nrow(db_mod)
db_mod$producto <- relevel(as.factor(db_mod$producto),ref="RC + IRL")
db_mod$ant_vehiculo_7G <- cut(db_mod$ant_vehiculo, breaks = c(0,6,10,13,15,17,20,Inf),include.lowest=T)
db_mod$ant_vehiculo_7G <- relevel(as.factor(db_mod$ant_vehiculo_7G),ref = "(6,10]") 
db_mod$ant_vehiculo_4G <- cut(db_mod$ant_vehiculo, breaks = c(0,10,13,15,17,Inf),include.lowest=T)
db_mod$ant_vehiculo_4G <- relevel(as.factor(db_mod$ant_vehiculo_4G),ref = "[0,10]") 

#db_mod$n_eventos<-db_mod$pesopotencia
#DIAPOSITIVA 12
##############

#Centroides.
Centroides_CP<-as.data.frame(gCentroid(Poligonos_CP_WGS84, byid=TRUE)@coords)
Centroides_PR<-as.data.frame(gCentroid(Poligonos_PR_WGS84, byid=TRUE)@coords)

#Shp
plot(Poligonos_PR_WGS84)

Poligonos_PR_WGS84@data
Poligonos_PR_WGS84@bbox
#Poligonos_PR_WGS84@polygons

#Rasters.
raster <- raster(ncol = 40, nrow = 40)
extent(raster) <- extent(Poligonos_CP_WGS84)
Raster_CP_WGS84<-rasterize(Poligonos_CP_WGS84, raster, Poligonos_CP_WGS84$CP, fun = mean)


#DIAPOSITIVA 13
##############

#Spatial Points Data Frame.
Centroides_CP2<-Centroides_CP
coordinates(Centroides_CP2)<- c("x","y")
proj4string(Centroides_CP2) <- proj4string(Poligonos_CP_WGS84)

plot(Centroides_CP2,main="Spatial Point Data Frame")

#Spatial Polygon Data Frame.
Nuevo_Spatial_Poligon_Data<-rasterToPolygons(Raster_CP_WGS84)
plot(Nuevo_Spatial_Poligon_Data,main="Spatial Polygon Data Frame")


#DIAPOSITIVA 15
##############

#Coordenadas de Mi Base de Datos.
Datos_CP_Poligonos<-Poligonos_CP_WGS84@data
Datos_CP_Poligonos<-cbind(Datos_CP_Poligonos,Centroides_CP)
Datos_CP_Poligonos$CP<-as.numeric(as.character(Datos_CP_Poligonos$CP))
Datos_CP_Poligonos<-dplyr::select(Datos_CP_Poligonos,CP,x,y)

db_mod_CP<-merge(db_mod,Datos_CP_Poligonos, by="CP", all.x=TRUE)
db_mod_CP<-filter(db_mod_CP,x>-1000)

plot(db_mod_CP$x,db_mod_CP$y)


#Otra forma de conseguir datos. A través de Geocode.
id<-c(1,2,3,4,5)
calles<-c("Calle de Embajadores 181 28045 Madrid",
          "Calle de Embajadores 177 28045 Madrid",
          "Calle de Embajadores 175 28045 Madrid",
          "Calle de Embajadores 173 28045 Madrid",
          "Calle de Embajadores 171 28045 Madrid")

bbdd<-as.data.frame(cbind(id,calles))

for (i in 1:length(calles)){
  Sys.sleep(5) #3 SEGUNDOS PARA QUE NO NOS BLOQUEEN GOOGLE.
  Geolocalizaciones <- geocode(calles[i], output = "latlona", source = "google")
  print(head(Geolocalizaciones))
  bbdd$lon[i] <- as.numeric(Geolocalizaciones[1])
  bbdd$lat[i] <- as.numeric(Geolocalizaciones[2])
  bbdd$Address[i] <- as.character(Geolocalizaciones[3])
}

plot(bbdd$lon,bbdd$lat,col="Red",main="Puntos Calle Embajadores")
lines(bbdd$lon,bbdd$lat,col="Red")


#OPEN STREET MAPS

  #.
mapa1 <- opq(bbox = "Barcelona, Spain")
Poligonos_dentro <- add_osm_feature(mapa1, key = 'building', value = "hospital")
df <- osmdata_sp(Poligonos_dentro)

  #
spChFIDs(df$osm_polygons) <- 1:nrow(df$osm_polygons@data)
centroides <- gCentroid(df$osm_polygons, byid = TRUE)
leaflet(centroides) %>% addTiles() %>% addCircles()

  #
buffer <- gBuffer(centroides, byid = TRUE, width = 0.002)
leaflet(centroides) %>% addTiles() %>% addPolygons(data = buffer, col = "red") %>% addCircles()

  #
buffer <- SpatialPolygonsDataFrame(buffer, data.frame(row.names = names(buffer), n = 1:length(buffer)))

  #
gt <- gIntersects(buffer, byid = TRUE, returnDense = FALSE)
ut <- unique(gt)
nth <- 1:length(ut)
buffer$n <- 1:nrow(buffer)
buffer$nth <- NA
for(i in 1:length(ut)){
  x <- ut[[i]];  buffer$nth[x] <- i
}
buffdis <- gUnaryUnion(buffer, buffer$nth)
leaflet(centroides) %>% addTiles() %>% addPolygons(data = buffdis, col = "red") %>% addCircles()

  #
gt <- gIntersects(buffdis, byid = TRUE, returnDense = FALSE)
ut <- unique(gt)
nth <- 1:length(ut)
buffdis <- SpatialPolygonsDataFrame(buffdis, data.frame(row.names = names(buffdis), n = 1:length(buffdis)))
buffdis$nth <- NA
for(i in 1:length(ut)){
  x <- ut[[i]];  buffdis$nth[x] <- i
}
buffdis <- gUnaryUnion(buffdis, buffdis$nth)
leaflet(centroides) %>% addTiles() %>% addPolygons(data = buffdis, col = "red") %>% addCircles()

#Polígonos Finales. Ya solamente hay un punto
centroides_p<-gCentroid(buffdis, byid = TRUE)
leaflet(centroides_p) %>% addTiles() %>% addCircles()

#Calculamos distancias entre hospitales
Dist<-distm(cbind(centroides_p$x,centroides_p$y),cbind(centroides_p$x,centroides_p$y),fun = distCosine )/1000
Dist<-apply(Dist,1,min)



#DIAPOSITIVA 16
##############

head(db_mod_CP)
hist(db_mod_CP$pesopotencia,main="Nivel Individual")

#Spatial Point Data Frame
db_mod_CP_SP<-db_mod_CP
coordinates(db_mod_CP_SP)<- c("x","y")
proj4string(db_mod_CP_SP) <- proj4string(Poligonos_PR_WGS84)

#Mezclamos capas del mapa de dos formas
  #1
db_mod_CP_SP$Codigo <- over(x = db_mod_CP_SP, y = Poligonos_PR_WGS84)$Codigo
PR_pesopotencia<-aggregate(pesopotencia~Codigo,data = db_mod_CP_SP@data, FUN = mean)
head(PR_pesopotencia)


  #2
Poligonos_PR_WGS84@data$pesopotencia <- over(x = Poligonos_PR_WGS84, y = db_mod_CP_SP,fn = mean)$pesopotencia
PR_pesopotencia2<-dplyr::select(Poligonos_PR_WGS84@data, Codigo, pesopotencia)
head(PR_pesopotencia)

Poligonos_CP_WGS84@data$pesopotencia <- over(x = Poligonos_CP_WGS84, y = db_mod_CP_SP,fn = mean)$pesopotencia
PR_pesopotencia3<-dplyr::select(Poligonos_CP_WGS84@data, CP, pesopotencia)

hist(PR_pesopotencia2$pesopotencia,main="Nivel Agregado Provincia")
hist(PR_pesopotencia3$pesopotencia,main="Nivel Agregado Codigo Postal")


#DIAPOSITIVA 17
##############

#Filtramos los datos
Poligonos_PR_WGS84@data$pesopotencia_filter<-(Poligonos_PR_WGS84@data$pesopotencia>13.2)*1+1
colores<-c("White","Red")
colores_pintar<-colores[Poligonos_PR_WGS84@data$pesopotencia_filter]
plot(Poligonos_PR_WGS84,col=colores_pintar)

Poligonos_filtrados<-Poligonos_PR_WGS84[Poligonos_PR_WGS84$pesopotencia_filter==2,]
plot(Poligonos_filtrados)


#DIAPOSITIVA 18
##############

#Vamos a visualizar una nueva variable. 
db_mod_CP_SP@data$nev_exp<-db_mod_CP_SP@data$n_eventos/db_mod_CP_SP@data$expuestos


Poligonos_PR_WGS84@data$n_even_exp <- over(x = Poligonos_PR_WGS84, y = db_mod_CP_SP,fn = mean)$nev_exp
PR_n_even_exp<-dplyr::select(Poligonos_PR_WGS84@data, Codigo, n_even_exp)
head(PR_n_even_exp)


Poligonos_PR_WGS84@data$n_even_exp2<-(Poligonos_PR_WGS84@data$n_even_exp>mean(db_mod_CP_SP@data$nev_exp))*1+1+(Poligonos_PR_WGS84@data$n_even_exp>1.5*mean(db_mod_CP_SP@data$nev_exp))*1
colores<-c("White","Red","Purple")
colores_pintar<-colores[Poligonos_PR_WGS84@data$n_even_exp2]
plot(Poligonos_PR_WGS84,col=colores_pintar,main="Número de Eventos por encima de la media")


#DIAPOSITIVA 20
##############

#Plot
table(floor(db_mod_CP$n_eventos/db_mod_CP$expuestos))

db_mod_CP$n_eventos_exp<-floor(db_mod_CP$n_eventos/db_mod_CP$expuestos)

colores<-c("White","Red","Purple","Purple","Purple","Purple","Purple","Purple","Purple","Purple","Purple","Purple")
colores_pintar<-colores[db_mod_CP$n_eventos_exp]
plot(db_mod_CP$x,db_mod_CP$y,col=colores_pintar,main="Numero de Eventos por individuo")


#GGPLOT2. Lenguaje de Mapas. 
ciudad <- get_map("Spain", zoom=5)

Plot<-ggmap(ciudad) +
  stat_density2d(data=db_mod_CP, aes(x = x, y = y, fill=..level..), alpha=0.2, geom = "polygon") +
  scale_fill_gradient(low = "yellow", high = "red")

Plot2<-ggmap(ciudad) +
  geom_point(data = db_mod_CP, aes(x=x, y=y, color = factor(n_eventos_exp))) 


#LEAFLET. Mapas dinámicos
db_mod_CP2<-db_mod_CP
coordinates(db_mod_CP2)<-c("x","y")
proj4string(db_mod_CP2) <- proj4string(Poligonos_CP_WGS84)

Poligonos_CP_WGS84@data$n_eventos_exp <- over(x = Poligonos_CP_WGS84, y = db_mod_CP2,fn = mean)$n_eventos_exp
db_mod_CP2@data<-dplyr::select(db_mod_CP2@data,CP,expuestos)
Poligonos_CP_WGS84@data$expuestos<- over(x = Poligonos_CP_WGS84, y = db_mod_CP2,fn = sum)$expuestos

Poligonos_CP_WGS84@data$n_eventos_exp[is.na(Poligonos_CP_WGS84@data$n_eventos_exp)]<-0
Poligonos_CP_WGS84@data$expuestos[is.na(Poligonos_CP_WGS84@data$expuestos)]<-0

colores<-c("Blue","Red","Purple")
nueva<-(Poligonos_CP_WGS84@data$n_eventos_exp>0.05)*1+1
colores_pintar<-colores[nueva]

Plot3<-leaflet() %>% 
  addTiles() %>%
  addPolygons(data=Poligonos_CP_WGS84, weight = 1,fill = colores_pintar,fillColor =colores_pintar)

Plot3

#DIAPOSITIVA 21
##############

#Leemos nivel municipio
#1
Poligonos_MU <-readOGR(dsn="C:/Users/AX29501/Desktop/Doctorado/6_Cursos_Docencia/04_Clases_AFI/Clase/0_Muni","Municipios_ETRS89_30N")
Poligonos_MU <- spTransform(Poligonos_MU, CRS("+proj=longlat +datum=WGS84"))

#2
raster <- raster(ncol = 60, nrow = 60)
extent(raster) <- extent(Poligonos_CP_WGS84)
Raster_CP_WGS84<-rasterize(Poligonos_CP_WGS84, raster, Poligonos_CP_WGS84$CP, fun = mean)
Poligonos_MU<-rasterToPolygons(Raster_CP_WGS84)
ID<-1:nrow(Poligonos_MU@data)
Poligonos_MU@data<-cbind(ID,Poligonos_MU@data)
X11()
plot(Poligonos_MU)

#Agregamos a nivel municipio.
head(db_mod_CP)
db_mod_CP2<-db_mod_CP
coordinates(db_mod_CP2)<-c("x","y")
proj4string(db_mod_CP2) <- proj4string(Poligonos_MU)

Poligonos_MU@data$n_eventos_exp <- over(x = Poligonos_MU, y = db_mod_CP2,fn = mean)$n_eventos_exp
Poligonos_MU@data$n_eventos <- over(x = Poligonos_MU, y = db_mod_CP2,fn = mean)$n_eventos
db_mod_CP2@data<-dplyr::select(db_mod_CP2@data,CP,expuestos,n_eventos)
Poligonos_MU@data$expuestos<- over(x = Poligonos_MU, y = db_mod_CP2,fn = sum)$expuestos
Poligonos_MU@data$eventos<- over(x = Poligonos_MU, y = db_mod_CP2,fn = sum)$n_eventos
Poligonos_MU@data$eventos_expuestos<-Poligonos_MU@data$eventos/Poligonos_MU@data$expuestos

Poligonos_MU@data$n_eventos_exp[is.na(Poligonos_MU@data$n_eventos_exp)]<-0
Poligonos_MU@data$n_eventos[is.na(Poligonos_MU@data$n_eventos)]<-0
Poligonos_MU@data$eventos[is.na(Poligonos_MU@data$eventos)]<-0
Poligonos_MU@data$expuestos[is.na(Poligonos_MU@data$expuestos)]<-0
Poligonos_MU@data$eventos_expuestos[is.na(Poligonos_MU@data$eventos_expuestos)]<-0

head(Poligonos_MU)

#Generamos el mapa de percentiles. 

pal <- colorBin("RdYlGn", domain = Poligonos_MU@data$eventos_expuestos, 
                bins = unique(quantile(Poligonos_MU@data$eventos_expuestos, c(0,.60,.90, 1))),
                na.color = "grey40", reverse = T)

Plot<-leaflet() %>% 
  addTiles() %>%
  addPolygons(data=Poligonos_MU, weight = 1,opacity = 2,
              color=pal(Poligonos_MU@data$eventos_expuestos),
              fill = Poligonos_MU@data$eventos_expuestos,
              fillColor =pal(Poligonos_MU@data$eventos_expuestos),
              label = ~as.character(paste(get("eventos_expuestos"), sep=" / ")))  %>%
  addLegend(pal = pal, values = round(Poligonos_MU@data$eventos_expuestos, 3), opacity = 2, position = "bottomright", title = "")
Plot

#DIAPOSITIVA 22
##############

#Escribimos un shape file. 
writeOGR(Poligonos_MU, ".", "Poligonos_MU", driver="ESRI Shapefile")

#DIAPOSITIVA 24
##############

#IMORAN
w<-poly2nb(Poligonos_MU, queen=FALSE)
w3<-nb2listw(w,zero.policy=TRUE)

plt<-moran.plot(x = Poligonos_MU@data$eventos_expuestos, listw = w3, zero.policy = TRUE,main="Gráfico I Moran")
tst<-moran.test(x = Poligonos_MU@data$eventos_expuestos, listw = w3, zero.policy = TRUE)

tst

#DIAPOSITIVA 25
##############

#TEST ILISA
imoranlocal<-as.data.frame(localmoran(x = Poligonos_MU@data$eventos_expuestos, listw = w3, zero.policy = TRUE))
Poligonos_MU@data<-cbind(Poligonos_MU@data,imoranlocal)
Poligonos_MU@data$Z.Ii[is.na(Poligonos_MU@data$Z.Ii)]<-0

X11()
brks <- quantile(Poligonos_MU@data$Z.Ii)
labs <- c("white","yellow","orange","red")
q <- cut(Poligonos_MU@data$Z.Ii, brks, labels = labs, include.lowest = T)
qc <- as.character(q) 
plot(Poligonos_MU, col = qc,main="Mapa del IMoran")


#DIAPOSITIVA 26
##############

#SCAN TESTS
Kurdorf_scan<-function(Geo,X,response,EXP,filter=0){

    X$response<-response
  X$response_exp<-EXP
  
  
  bbdd2<-X
  coordinates(bbdd2)<- c("x","y")
  proj4string(bbdd2) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  bbdd33<-bbdd2

  for (i in 1:ncol(bbdd33@data)){
    if (!is.numeric(bbdd33@data[,i])) {
      bbdd33@data[,i]<-as.numeric(as.factor(bbdd33@data[,i]))
    } }
  
  
  Geo_SC_Ag<-Geo
  agrss<-over(Geo,bbdd33,fn = sum)
  
  vivos<-agrss$VIVO
  responses<-agrss$response
  response_exp<-agrss$response_exp
  
  agrss<-agrss/vivos
  
  agrss$VIVOS<-vivos
  agrss$responses<-responses
  agrss$response_exp<-response_exp
  
  agrss[is.na(agrss)] <- 0
  
  Geo_SC1<-as.numeric(as.character(Geo@data[,1]))
  comb<-cbind(Geo_SC1,agrss)
  Geo_SC_Ag@data<-comb
  
  LAT_IND<-centroid(Geo_SC_Ag)[,2]
  LONG_IND<-centroid(Geo_SC_Ag)[,1]
  Geo_Y<-cbind(Geo_SC_Ag@data,LAT_IND,LONG_IND)
  Geo_Y <- as.data.frame(sapply(Geo_Y, function(x) as.numeric(as.character(x))))
  
  Geo_Y<-filter(Geo_Y,VIVOS>filter)
  
  ## Process geographical information and convert to grid
  geo2<-dplyr::select(Geo_Y,LONG_IND,LAT_IND)
  geo2<-rename(geo2,x=LONG_IND,y=LAT_IND)
  geo2 <- latlong2grid(geo2)
  
  ## Get aggregated counts of population and cases for each county
  population2 <- tapply(Geo_Y$VIVOS,Geo_Y$Geo_SC1,sum)
  cases2 <- tapply(Geo_Y$responses,Geo_Y$Geo_SC1,sum)
  expected.cases2 <- abs(as.numeric(Geo_Y$response_exp))
  ## Based on the 16 strata levels, computed expected numbers of disease
  #n.strata <- 16
  
  population2<-Geo_Y$VIVOS
  cases2<-abs(Geo_Y$responses)
 
  ## Set Parameters
  pop.upper.bound <- 0.5
  n.simulations <- 200
  alpha.level <- 0.05
  plot <- FALSE
  
  poisson <- SpatialEpi::kulldorff(geo = geo2, cases = cases2, population = population2,expected.cases =expected.cases2 , pop.upper.bound = pop.upper.bound,n.simulations = n.simulations, alpha.level = alpha.level, plot = plot)
  
  
  cluster <- poisson$most.likely.cluster$location.IDs.included
  
  GR<-Geo_Y[cluster,]
  
  GR3<-as.numeric(GR[,1])
  
  poisson$most.likely.cluster$GR3<-GR3
  
  return(poisson$most.likely.cluster)
  
}

SatScanp<-function(Geo,X,response,filter=0,shape=0){
  
  td = tempdir()
  
  X$response<-response
  
  bbdd2<-X
  coordinates(bbdd2)<- c("x","y")
  proj4string(bbdd2) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  bbdd33<-bbdd2

  for (i in 1:ncol(bbdd33@data)){
    if (!is.numeric(bbdd33@data[,i])) {
      bbdd33@data[,i]<-as.numeric(as.factor(bbdd33@data[,i]))
    } }
  
  
  Geo_SC_Ag<-Geo
  agrss<-over(Geo,bbdd33,fn = sum)
  
  vivos<-agrss$VIVO
  responses<-agrss$response
  
  agrss<-agrss/vivos
  
  agrss$VIVOS<-vivos
  agrss$responses<-responses
  
  agrss[is.na(agrss)] <- 0
  
  Geo_SC1<-as.numeric(as.character(Geo@data[,1]))
  comb<-cbind(Geo_SC1,agrss)
  Geo_SC_Ag@data<-comb
  
  Geo22<-Geo_SC_Ag
  
  Geo22<-Geo22[Geo22$VIVOS > filter ,]
  
  ###Obtengo los centroides de mi polygondataframe
  
  LAT_IND<-centroid(Geo22)[,2]
  LONG_IND<-centroid(Geo22)[,1]
  centroides<-as.data.frame(cbind(LONG_IND,LAT_IND))
  centroides<-latlong2grid(centroides)
  
  ###Construyo los inputs para SATSCAN
  NHumbersidecas1<-as.data.frame(matrix(0,nrow =nrow(Geo22) ,ncol=2))
  NHumbersidectl1<-as.data.frame(matrix(0,nrow =nrow(Geo22) ,ncol=2))
  NHumbersidegeo1<-as.data.frame(matrix(0,nrow =nrow(Geo22) ,ncol=3))
  NHumbersidepop1<-as.data.frame(matrix(0,nrow =nrow(Geo22) ,ncol=2))
  
  ###Identificación del ID y de la respuesta
  NHumbersidecas1$locationid<-Geo22@data[,1]
  NHumbersidecas1$numcases<- floor(abs(Geo22@data$responses ))
  
  NHumbersidectl1$locationid<-Geo22@data[,1]
  NHumbersidectl1$numcontrols<-floor(Geo22@data$VIVOS)
  
  NHumbersidepop1$locationid<-Geo22@data[,1]
  NHumbersidepop1$numpopulation<-floor(Geo22@data$VIVOS)
  
  NHumbersidegeo1$locationid<-Geo22@data[,1]
  NHumbersidegeo1$x<-coordinate<-centroides$x
  NHumbersidegeo1$y<-coordinate<-centroides$y
  
  NHumbersidecas<-dplyr::select(NHumbersidecas1,-V1,-V2)
  NHumbersidectl<-dplyr::select(NHumbersidectl1,-V1,-V2)
  NHumbersidegeo<-dplyr::select(NHumbersidegeo1,-V1,-V2,-V3)
  NHumbersidepop<-dplyr::select(NHumbersidepop1,-V1,-V2)
  
  ###Genero archivos temporales
  write.cas(NHumbersidecas, td, "NHumberside")
  write.ctl(NHumbersidectl, td, "NHumberside")
  write.geo(NHumbersidegeo, td, "NHumberside")
  write.pop(NHumbersidepop, td, "NHumberside")
  
  ###Opciones SatScan
  invisible(ss.options(reset=TRUE))
  ss.options(list(CaseFile="NHumberside.cas", 
                  AnalysisType=1,#(1=Purely Spatial, 2=Purely Temporal, 3=Retrospective Space-Time, 4=Prospective Space-Time, 5=Spatial Variation in Temporal Trends, 6=Prospective Purely Temporal)"
                  ControlFile="NHumberside.ctl",
                  PopulationFile="NHumberside.pop",
                  CoordinatesFile="NHumberside.geo",
                  StartDate="2001/11/1", 
                  EndDate="2001/11/24",
                  ScanAreas=1, #scan areas (1=High Rates(Poison,Bernoulli,STP); High Values(Ordinal,Normal); Short Survival(Exponential), 2=Low Rates(Poison,Bernoulli,STP);
                  PrecisionCaseTimes=0, #time precision (0=None, 1=Year, 2=Month, 3=Day, 4=Generic)
                  CoordinatesType=0, #coordinate type (0=Cartesian, 1=latitude/longitude)
                  ModelType=1, #(0=Discrete Poisson, 1=Bernoulli, 2=Space-Time Permutation, 3=Ordinal, 4=Exponential, 5=Normal, 6=Continuous Poisson, 7=Multinomial)"
                  TimeAggregationUnits = 0, #time aggregation units (0=None, 1=Year, 2=Month, 3=Day, 4=Generic
                  MaxSpatialSizeInPopulationAtRisk=50,
                  SpatialWindowShapeType=shape , #window shape (0=Circular, 1=Elliptic)
                  NonCompactnessPenalty=0,
                  MonteCarloReps=999,
                  IterativeScanMaxPValue=0.1,
                  ReportGiniClusters="y", 
                  LogRunToHistoryFile="n"
  ))
  
  
  write.ss.prm(td, "NHumberside")
  
  NHumberside <- satscan(td, "NHumberside",verbose = T)
  
  vec<-as.vector(as.numeric(as.character(NHumberside$gis$LOC_ID)))
  
  NHumberside$GRE3<-vec
  
  return(NHumberside)
  
}

view_cluster<-function(Geo,high,filter=0,nn=800){
  
  Geo<-Geo[Geo$VIVOS > filter ,]
  
  LAT_IND<-centroid(Geo)[,2]
  LONG_IND<-centroid(Geo)[,1]
  centroides<-as.data.frame(cbind(LONG_IND,LAT_IND))
  
  
  cruzar<-cbind(Geo@data,centroides)
  fin<-cruzar[cruzar[,1] %in% high,]

  rer <- cruzar[sapply(cruzar,is.numeric)]
  a<-sum(rer$VIVOS)
  nueva1<-(apply(X = rer,MARGIN = 2,FUN = sum )/a)
  nueva1$VIVOS<-a
  
  rer <- fin[sapply(fin,is.numeric)]
  a<-sum(rer$VIVOS)
  nueva2<-(apply(X = rer,MARGIN = 2,FUN = sum )/a)
  nueva2$VIVOS<-a
  

  s<-leaflet() %>%
    addTiles() %>%
    addCircleMarkers(lng=cruzar$LONG_IND,lat=cruzar$LAT_IND,radius=0.5) %>%
    addCircles(fin$LONG_IND, fin$LAT_IND, radius=nn, stroke=FALSE, fillOpacity=0.5, fillColor="red")
  
  
  kas<-as.data.frame(rbind(nueva1,nueva2))
  
  df<-list(s,kas)
  
  return(df)
  
}


#SCAN TESTS
db_mod_CP$VIVO<-db_mod_CP$expuestos
Poligonos_MU@data$VIVOS<-Poligonos_MU@data$expuestos
EXP<-rep(sum(db_mod_CP$n_eventos)/sum(db_mod_CP$expuestos),nrow(db_mod_CP))
db_mod_CP2<-dplyr::select(db_mod_CP,x,y,VIVO,n_eventos)

Kulld_1<-Kurdorf_scan(Poligonos_MU,db_mod_CP,db_mod_CP$n_eventos,EXP,filter=0)
Sat_scan<-SatScanp(Poligonos_MU,db_mod_CP2,db_mod_CP2$n_eventos,filter=0,shape=1  )

#Identificamos
view_cluster(Poligonos_MU,Kulld_1$GR3,filter=0,nn=15800) 
view_cluster(Poligonos_MU,Sat_scan$GRE3,filter=0,nn=15800) 



#DIAPOSITIVA 29
##############

#Residuos
m4 <- glm(n_eventos ~ producto + ant_carneconducir + ant_vehiculo_4G + offset(log(expuestos)), family = poisson("log"), data = db_mod_CP)
res4 <- summary(m4)
summary(m4)

plot(m4, which = c(1)) # predicted vs residuals
plot(m4, which = c(3)) # predicted values vs std.deviance resid.
plot(m4, which = c(2)) # qqplot
plot(m4, which = c(4)) # Cook's distance
plot(m4, which = c(5)) # Leverage vs Std.Pearson resid.
plot(m4, which = c(6)) # Cook's distance vs Leverage 


paste0("m4 AIC = ", round(res4$aic,4))
paste0("m4 BIC = ", round(BIC(m4),4))
paste0("m4 Deviance = ", round(res4$deviance,4))


residuos<-residuals(m4,type="deviance")
#residuos<-predict(m4,type="response")


hist(residuos)
quantile(residuos)
mean(residuos)


#Residuos
db_mod_CP$residuos<-residuos
db_mod_CP2<-db_mod_CP
coordinates(db_mod_CP2)<-c("x","y")
proj4string(db_mod_CP2) <- proj4string(Poligonos_MU)

Poligonos_MU@data$residuos <- over(x = Poligonos_MU, y = db_mod_CP2,fn = mean)$residuos
Poligonos_MU@data$residuos[is.na(Poligonos_MU@data$residuos)]<-0

#IMORAN
plt<-moran.plot(x = Poligonos_MU@data$residuos, listw = w3, zero.policy = TRUE,main="Gráfico I Moran")
tst<-moran.test(x = Poligonos_MU@data$residuos, listw = w3, zero.policy = TRUE)

#ILISA
imoranlocal<-as.data.frame(localmoran(x = Poligonos_MU@data$residuos, listw = w3, zero.policy = TRUE))
Poligonos_MU@data<-cbind(Poligonos_MU@data,imoranlocal)
Poligonos_MU@data$Z.Ii[is.na(Poligonos_MU@data$Z.Ii)]<-0

X11()
brks <- quantile(Poligonos_MU@data$Z.Ii)
labs <- c("white","yellow","orange","red")
q <- cut(Poligonos_MU@data$Z.Ii, brks, labels = labs, include.lowest = T)
qc <- as.character(q) 
plot(Poligonos_MU, col = qc,main="Mapa del IMoran")

#CLUSTERIZACIÓN
EXP<-rep(sum(db_mod_CP$residuos)/sum(db_mod_CP$expuestos),nrow(db_mod_CP))
db_mod_CP2<-dplyr::select(db_mod_CP,x,y,VIVO,residuos)

Kulld_1<-Kurdorf_scan(Poligonos_MU,db_mod_CP,db_mod_CP$residuos,EXP,filter=0)
Sat_scan<-SatScanp(Poligonos_MU,db_mod_CP2,db_mod_CP2$residuos,filter=0,shape=1  )

view_cluster(Poligonos_MU,Kulld_1$GR3,filter=0,nn=15800) 
view_cluster(Poligonos_MU,Sat_scan$GRE3,filter=0,nn=15800) 


#DIAPOSITIVA 30
##############

Kulld_1

db_mod_CP_SP<-db_mod_CP
coordinates(db_mod_CP_SP)<- c("x","y")
proj4string(db_mod_CP_SP) <- proj4string(Poligonos_PR_WGS84)

db_mod_CP_SP$ID <- over(x = db_mod_CP_SP, y = Poligonos_MU)$ID
db_mod_CP$ID<-db_mod_CP_SP@data$ID
db_mod_CP$ID1<-(db_mod_CP$ID %in% Kulld_1$GR3)*1+0


m41 <- glm(n_eventos ~ producto + ant_carneconducir + ant_vehiculo_4G + ID1+ offset(log(expuestos)), family = poisson("log"), data = db_mod_CP)
res41 <- summary(m41)
summary(m41)
                
residuos<-residuals(m41,type="deviance")

#DIAPOSITIVA 31
##############

head(db_mod_CP)

Dist<-distm(cbind(db_mod_CP$x,db_mod_CP$y),cbind(c(-1.1315273),c(37.9748133)),fun = distCosine )/1000
db_mod_CP$Dist<-log(as.vector(Dist))

m42 <- glm(n_eventos ~ producto + ant_carneconducir + ant_vehiculo_4G + Dist+ offset(log(expuestos)), family = poisson("log"), data = db_mod_CP)
res42 <- summary(m42)
summary(m42)

residuos<-residuals(m42,type="deviance")

#Modelo no Lineal
db_mod_CPP<-dplyr::select(db_mod_CP,n_eventos,producto,ant_carneconducir,ant_vehiculo_4G,Dist,expuestos)
Modelo_earth<-earth::earth(formula=n_eventos~.+ offset(log(expuestos)),data=db_mod_CPP, glm=list(family=poisson(link=log)),pmethod="none")
summary(Modelo_earth)



#DIAPOSITIVA 32
##############
head(db_mod_CP)

w<-poly2nb(Poligonos_MU, queen=FALSE)
w2<-nb2mat(w,zero.policy = T)
w3<-nb2listw(w,zero.policy=TRUE)

m43 <- lagsarlm(residuos ~ 1,data=Poligonos_MU@data, w3,zero.policy = T)
summary(m43)


#DIAPOSITIVA 33
##############
head(Poligonos_MU@data)

#Modelo GWR
puntos<-cbind(Poligonos_MU@data,as.data.frame(gCentroid(Poligonos_MU, byid=TRUE)@coords))
poligonos<-Poligonos_MU

m <- lm(residuos ~ 1, data=puntos)

puntos_sp<-puntos
coordinates(puntos_sp)<- c("x","y")
proj4string(puntos_sp) <- proj4string(poligonos)

bw <- gwr.sel(residuos ~ 1, data=puntos_sp)
g <- gwr(residuos ~ 1, data=puntos_sp, bandwidth=bw)#, fit.points=newpts[, 1:2])


mean(g$SDF$'(Intercept)')
Poligonos_MU@data$residuos_model<-g$SDF$'(Intercept)'

#Pintamos
pal <- colorBin("RdYlGn", domain = Poligonos_MU@data$residuos_model, 
                bins = unique(quantile(Poligonos_MU@data$residuos_model, c(0,.20,.40,.60,.90, 1))),
                na.color = "grey40", reverse = T)

Plot<-leaflet() %>% 
  addTiles() %>%
  addPolygons(data=Poligonos_MU, weight = 1,opacity = 2,
              color=pal(Poligonos_MU@data$residuos_model),
              fill = Poligonos_MU@data$residuos_model,
              fillColor =pal(Poligonos_MU@data$residuos_model),
              label = ~as.character(paste(get("residuos_model"), sep=" / ")))  %>%
  addLegend(pal = pal, values = round(Poligonos_MU@data$residuos_model, 3), opacity = 2, position = "bottomright", title = "")
Plot


#DIAPOSITIVA 34
##############
library(gstat)

gs <- gstat(formula=residuos~1, locations=puntos_sp,nmax=12, set=list(idp = 0))

p <- predict(gs,puntos_sp)
residuos_predicted<-p$var1.pred

mean(residuos_predicted)
Poligonos_MU@data$residuos_predicted<-residuos_predicted


#Pintamos
pal <- colorBin("RdYlGn", domain = Poligonos_MU@data$residuos_predicted, 
                bins = unique(quantile(Poligonos_MU@data$residuos_predicted, c(0,.20,.40,.60,.90, 1))),
                na.color = "grey40", reverse = T)

Plot<-leaflet() %>% 
  addTiles() %>%
  addPolygons(data=Poligonos_MU, weight = 1,opacity = 2,
              color=pal(Poligonos_MU@data$residuos_predicted),
              fill = Poligonos_MU@data$residuos_predicted,
              fillColor =pal(Poligonos_MU@data$residuos_predicted),
              label = ~as.character(paste(get("residuos_predicted"), sep=" / ")))  %>%
  addLegend(pal = pal, values = round(Poligonos_MU@data$residuos_predicted, 3), opacity = 2, position = "bottomright", title = "")
Plot

#KRIDGE
coef <- lm(residuos~1, puntos)$coef
coef

k<- krige(residuos~1, puntos_sp, meuse.grid, vgm(.6, "Sph", 900), beta = coef)

