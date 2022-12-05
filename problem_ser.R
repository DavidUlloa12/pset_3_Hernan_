# Hernan David Ulloa Torres
# Comprobamos la version de  R
sessionInfo()
# Version 4.1.3
## limpiamos r
rm(list = ls()) # limpia el entorno de R
require(pacman)
p_load(tidyverse, arrow, rio , 
       broom, # tidy-coefficients
       mfx, # marginal effects
       margins,  # marginal effects
       estimatr, # robust standard errors
       lmtest, # HAC (Newey-West) standard errors
       fixest, # hdfe regressions (feols)
       modelsummary, # Coefplot with modelplot
       stargazer, # export tables to latex 
       sf,
       leaflet,
       rvest,
       xml2,
       osmdata,
       ggsn,
       tmaptools,
       skimr,
       ggmap,wordcloud,wordcloud2,SnowballC,tm,pdftools)  
# Cargamos la base de datos 
df <- readRDS("~/Documents/GitHub/pset-3/input/data_regresiones.rds")
modelo_1 = lm(price ~ rooms + bathrooms , data = df) 
modelo_2 = lm(price ~ rooms  + surface_total, data = df) 
modelo_3 = lm(price ~ rooms + dist_park, data = df) 
# punto 1.2
data <- modelsummary(list(modelo_1,modelo_2,modelo_3),output = "dataframe") ## Saca la grafica
# otra Forma
tidy(modelo_1)
tabla = full_join(tidy(modelo_1),tidy(modelo_2),"term") %>% full_join(tidy(modelo_3),"term")

# Grafico
modelo = list('modelo 1 ' = modelo_1 , 'modelo 2' = modelo_2,  "modelo 3" = modelo_3)
modelo

png("~/Documents/GitHub/pset-3/plot_regresiones.png")
grafico <- modelplot(modelo) + coord_flip() + 
  labs(title = "modelos Econometricos")
dev.off()

# punto 1.3
export(data,"~/Documents/GitHub/pset-3/output/resultados_regresiones.xlsx")

# punto 2
opq(bbox = getbb("Bogotá Colombia"))

## objeto osm
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="restaurant") 
class(osm)

osm_sf = osm %>% osmdata_sf()
osm_sf
#### Punto  2.2 #####
## Graficamos Parques
parques <- opq(bbox = getbb("Bogota colombia")) %>%
  add_osm_feature(key = "leisure", value = "park")%>%
  osmdata_sf()%>% .$osm_polygon

leaflet() %>% addTiles() %>% addPolygons(data=parques,color = 'royalblue')
### Graficamos Restaurantes
available_tags("amenity")
restaurantes <- opq(bbox = getbb("Bogota colombia")) %>%
  add_osm_feature(key = "amenity", value = "restaurant")%>%
  osmdata_sf()%>% .$osm_points
leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurantes , col="tan",label = "restaurantes")
###  Punto 2.3 Geocodificar Direcciones ###
Dirrecion<- geocode_OSM("Calle 50 %40% 4-29", as.sf=T)
Dirrecion
## Punto 2.4
bog <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
bog <- bog$osm_multipolygons %>% subset(admin_level==9)
## se adicion osm layer
osm_layer <- get_stamenmap(bbox = as.vector(st_bbox(bog)), 
                           maptype="toner", source="osm", zoom=13) 

png("~/Documents/GitHub/pset-3/output/mapa_amenities.png")

map2 <- ggmap(osm_layer) + 
  geom_sf(data=bog  , alpha=0.3 , inherit.aes=F) +
  geom_sf(data= restaurantes,aes(col='A'), inherit.aes=F)+ 
  geom_sf(data= Dirrecion,aes(col='B') , inherit.aes=F)+
  geom_sf(data= parques,aes(col="C") , inherit.aes=F) +
  scale_color_manual(label =c("A"="restaurantes","B"="Direccion","C"="parques"), values = c("A"="red","B"=" tan","C"="blue"))+
  theme_linedraw() + labs(x="" , y="")
map2

dev.off()


## Punto 3.1
my_url = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
browseURL(my_url)
my_html=read_html(my_url)
class(my_html)
## punto 3.2
my_html %>% html_node(xpath='//*//*[@id="firstHeading"]/span') %>% html_text()
## punto 3.3 
my_table= my_html %>% html_table()
length(my_table)
departamentos_Colombia<-my_table[4]
export(departamentos_Colombia,"~/Documents/GitHub/pset-3/output/stabla_departamento.xlsx")
## punto 3.4
mtd <- TermDocumentMatrix(my_html)
m <- as.matrix(mtd)
v <- sort(rowSums(m),decreasing=TRUE)
p <- data.frame(word = names(v),freq=v)
wordcloud2(data = p, size = 0.5, shape = "cloud",
           color="random-dark", ellipticity = 0.5)

