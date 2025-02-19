#instalar osmdata para obtener datos de Open Street Map
#remotes::install_github("ropensci/osmdata")

library(dplyr)
library(osmdata)
library(ggmap)
library(rvest)
library(sf)
library(stringr)

#explorar etiquetas disponibles en Open Street Map
available_tags("water")

#definir ciudad a obtener
# ciudad <- "Chile"
# getbb(ciudad)
# 
# # #generar caja de Chile
# # bb_chile = matrix(
# #   c(-75.6443953112, -55.61183, 
# #     -66.95992, -17.5800118954),
# #   nrow = 2,  
# #   ncol = 2,        
# #   byrow = TRUE)
# # 
# # rownames(bb_chile) = c("x", "y")
# # colnames(bb_chile) = c("min", "max")
# #bb_chile
# 
# bb_chile <- getbb("Chile", featuretype = "country")

#nombres de las regiones para luego obtener las coordenadas de sus bounding box
regiones <- c("Región de Arica y Parinacota",
              "Región de Tarapacá",
              "Región de Antofagasta",
              "Región de Atacama",
              "Región de Coquimbo",
              "Región de Valparaíso",
              "Región Metropolitana de Santiago",
              "Región del Libertador General Bernardo O’Higgins",
              "Región del Maule",
              "Región del Ñuble",
              "Región del Biobío",
              "Región de La Araucanía",
              "Región de Los Ríos",
              "Región de Los Lagos",
              "Región de Aysén",#"Región de Aysén del General Carlos Ibáñez del Campo",
              "Región de Magallanes y la Antártica Chilena")

#resumir nombres
nombre_regiones <- str_remove(regiones, "Región de |Región del |Región ")

#getbb("Región de Arica y Parinacota", featuretype = "administrative")

#obtener coordenadas de regiones ----
#crear lista vacía
regiones_bb <- list()

#loop
for (r in regiones) {
  #nombres de regiones resumidos
  nombre_region <- str_remove(r, "Región de |Región del |Región ")
  
  cat(nombre_region, fill=T)
  
  #obtener caja
  bb <- getbb(r, featuretype = "administrative")
  
  #guardar en lista
  regiones_bb[[nombre_region]] <- bb
  cat("OK", fill=T)
}

#revisar
regiones_bb[[nombre_regiones[16]]]

#—----


#prueba de descargar ríos desde Open Street Map
bb_n <- regiones_bb$Maule

rios_n <- bb_n %>%
  opq(timeout = 900)%>%
  add_osm_feature(key = "waterway",
                  value = c("canal", "river")) %>%
  osmdata_sf()

#hay que sacar Ñuble porque por alguna razón falla


#descargar datos ríos ----
#lista vacía
rios_regiones <- list()

#loop que por cada región, entrega la caja o bounding box y descarga sus ríos desde Open Street Map
for (r in nombre_regiones[-10]) {
  #obtener datos
  cat(r, fill=T)
  rios <- regiones_bb[[r]] %>% #caja de la región
    opq(timeout = 900) %>% #query a OSM
    add_osm_feature(key = "waterway", 
                    value = c("canal", "river")) %>% #obtener estos tipos de ríos
    osmdata_sf() #guardar en formato SF
  
  #corregir coordenadas
  try(
    st_crs(rios$osm_lines) <- 4326
  )
  
  #guardar en lista
  rios_regiones[[r]] <- rios
  cat("OK", fill=T)
}

#guardar como archivo para la posteridad
save(rios_regiones, file="rios_regiones.rdata")
#load("rios_regiones.rdata") #para cargar la data una vez que fue generada


#descargar datos lagos ----
# #no descarga nada por alguna razón
# lagos_regiones <- list()
# 
# for (r in nombre_regiones[-10]) {
#   #obtener datos
#   cat(r, fill=T)
#   lagos <- regiones_bb[[r]] %>% 
#     opq(timeout = 900)%>%
#     add_osm_feature(key = "waterway", 
#                     value = c("lake", "lagoon", "pond", "reservoir")) %>%
#     osmdata_sf()
#   
#   #corregir coordenadas
#   try(
#     st_crs(lagos$osm_polygons) <- 4326
#   )
#   
#   #guardar en lista
#   lagos_regiones[[r]] <- lagos
#   cat("OK", fill=T)
# }
# #guardar
# save(lagos_regiones, file="lagos_regiones.rdata")



#mapa de chile ----
#descargar un mapa comunal de chile desde el paquete ChileMapas de Mauricio Vargas
mapa_chile <- chilemapas::mapa_comunas

#probar el mapa de chile
ggplot() +
  geom_sf(data = mapa_chile, aes(geometry = geometry),
          size = 0.3, col = "white", fill = "grey90") +
  coord_sf(xlim = c(-76.04944, -66.07535)) +
  theme_void()


#mapa de sudamérica ----
#descargar un mapa de sudamérica desde el paquete RNaturalEarth
sudamerica <- rnaturalearth::ne_countries(continent = "south america", 
                                          scale = "large", 
                                          returnclass = "sf") #formato SF

#corregir coordenadas
st_crs(sudamerica$geometry) <- 4326

#probar mapa
ggplot() + 
  geom_sf(data = sudamerica %>% filter(admin != "Chile"), 
          aes(geometry=geometry), col="red")

#corregir coordenadas... de nuevo?
sudamerica$geometry <- st_transform(sudamerica$geometry, 
                                    crs = 4326)


#graficar mapa hidrológico de Chile ----
nombre_regiones

#definir colores y opciones
color1 <- "#0096C7"
color_2 <- "#e8f9fc" #"#CAF0F8"
color3 <- "#CAF0F8"
grosor1 <- 0.1 #grosor de los ríos
grosor2 <- 2 #grosor del borde de los ríos (ríos gruesos)
transparencia1 <- 1 #por si se quiere transparentar los ríos (sale feo)

#graficar mezclando capas
mapa_rios <- ggplot() +
  #mapa comunal de chile
  geom_sf(data = mapa_chile, aes(geometry = geometry),
          size = 0.3, col = color_2, fill = color_2) +
  #rios gruesos
  geom_sf(data = rios_regiones$`Arica y Parinacota`$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$Tarapacá$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$Antofagasta$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$Atacama$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$Coquimbo$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$Valparaíso$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$`Metropolitana de Santiago`$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$`Libertador General Bernardo O’Higgins`$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$Maule$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$Biobío$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$`La Araucanía`$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$`Los Ríos`$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$`Los Lagos`$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$`Aysén`$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  geom_sf(data = rios_regiones$`Magallanes y la Antártica Chilena`$osm_lines, aes(geometry = geometry),
          fill = color3, col = color3, size = grosor2, alpha = transparencia1, lineend= "round") +
  #ríos delgados
  geom_sf(data = rios_regiones$`Arica y Parinacota`$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round", lineend= "round") +
  geom_sf(data = rios_regiones$Tarapacá$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$Antofagasta$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$Atacama$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$Coquimbo$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$Valparaíso$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$`Metropolitana de Santiago`$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$`Libertador General Bernardo O’Higgins`$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$Maule$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$Biobío$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$`La Araucanía`$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$`Los Ríos`$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$`Los Lagos`$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$`Aysén`$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  geom_sf(data = rios_regiones$`Magallanes y la Antártica Chilena`$osm_lines, aes(geometry = geometry),
          fill = color1, col = color1, size = grosor1, lineend= "round") +
  #parche fuera de Chile, para tapar los ríos que cruzan Argentina y Bolivia
  geom_sf(data = sudamerica %>% filter(admin != "Chile"), aes(geometry=geometry), 
          col= "white", fill = "white") +
  #parches para tapar algunos rios de Argentina que se colaron
  geom_point(aes(x=-68.6, y=-50.1), col = "white", size=45, alpha = 1) +
  geom_point(aes(x=-69.15, y=-50.95), col = "white", size=35, alpha = 1) +
  geom_point(aes(x=-69, y=-51.6), col = "white", size=35, alpha = 1) +
  geom_point(aes(x=-67.5, y=-54), col = "white", size=45, alpha = 1)

#recortar mapa para que muestre solo Chile
mapa_rios2 <- mapa_rios +
  coord_sf(xlim = c(-76.04944, -66.07535),
           ylim = c(-56.72500, -17.5800118954)) +
  theme_void()

#ver mapa
mapa_rios2

#guardar mapa en PDF ----
ggsave(filename = "mapa_rios.pdf",
       plot = mapa_rios2,
       device = "pdf",
       width = unit(20, "cm"), 
       height = unit(50,"cm"),
       limitsize = F)

#guardar mapa en JPG ----
ggsave(filename = "mapa_hidrologico_chile.jpg",
       plot = mapa_rios2,
       width = unit(20, "cm"), 
       height = unit(50, "cm"),
       limitsize = F)
#si se mofidica el tamaño de la exportación, hay que acomodar los parches!




# #—-----
# 
# 
# # geom_point(aes(x=-70.17, y=-18.95), col = "red", size=14) +
# 
# regiones_bb[[nombre_regiones[16]]]
# 
# ggplot() +
#   geom_sf(data = rios_regiones$`Magallanes y la Antártica Chilena`$osm_lines, aes(geometry = geometry),
#           fill = color1, col = color1, size = 1, lineend= "round") +
#   # geom_sf(data = lagos_regiones$`Magallanes y la Antártica Chilena`$osm_polygons, aes(geometry = geometry),
#   #         fill = "purple", col = "purple", size = 1, lineend= "round") +
#   #parche fuera de Chile
#   geom_sf(data = sudamerica %>% filter(admin != "Chile"), aes(geometry=geometry), 
#           col= "white", fill = "white") +
#   #parche rios argentina
#   geom_point(aes(x=-68.5, y=-50.1), col = "red", size=14, alpha = 0.5) +
#   geom_point(aes(x=-69.15, y=-50.95), col = "red", size=14, alpha = 0.5) +
#   geom_point(aes(x=-69, y=-51.6), col = "red", size=14, alpha = 0.5) +
#   geom_point(aes(x=-67.5, y=-54), col = "red", size=14, alpha = 0.5) +
#   coord_sf(xlim = c(-76.04944, -66.07535),
#            ylim = c(-56.72500, -48.58223))