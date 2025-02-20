# Cargar paquetes y datos
#install.packages("geojsonio")
#install.packages("highcharter")
library(geojsonio)
library(tidyverse)
library(readxl)
library(highcharter)
getwd()
url_biobio <- "https://raw.githubusercontent.com/robsalasco/precenso_2016_geojson_chile/master/Comunas/R08.geojson"
pobreza_bio <- read_excel("poverty-chile/BASE_TECNICA_FCM_2020_Version3_07052020_H2349_SINIM.xlsx", 
                      sheet = "IND POBREZA", skip = 2)

biobio <- jsonlite::fromJSON(url_biobio, simplifyVector = FALSE)

total_comunas <- biobio$features %>% 
  purrr::map_df("properties") %>% 
  rename_all(stringr::str_to_lower) %>% 
  select(comuna, nom_comuna) %>% 
  mutate(
    comuna = as.numeric(comuna),
    nom_comuna = stringr::str_to_title(nom_comuna)
  )

biobio_geojson <- geojsonio::as.json(biobio)


# Formatear datos pobreza
names(pobreza_bio)[names(pobreza_bio) == "Estimación de pobreza Comunal (CASEN 2017)"] <- "pobreza_dec"
names(pobreza_bio)[names(pobreza_bio) == "Código Territorial"] <- "codigo"
names(pobreza_bio)[names(pobreza_bio) == "Nombre de la Comuna"] <- "nom_comuna"

# Convertir a porcentajes y redondear
pobreza_select_bio <- pobreza_bio %>%
  mutate(value = round(pobreza_dec * 100, 2),
         nom_comuna = stringr::str_to_title(nom_comuna)) %>% 
  select(value, nom_comuna)

# Unir datos pobreza con mapa a través del nombre de la comuna
total_pob <- total_comunas %>% 
  left_join(pobreza_select_bio, by = "nom_comuna")

# Highchart
highchart(type = "map") %>%
  hc_add_series(
    mapData = biobio_geojson,
    data = total_pob,
    joinBy = c("COMUNA", "comuna"),
    showInLegend = FALSE,
    name = "Estimación de pobreza Comunal",
    value = "value",
  ) %>% 
  hc_colorAxis(minColor = "white", maxColor = "red", endOnTick = FALSE) %>% 
  hc_tooltip(
    # estos campos son de los datos dvar
    pointFormat = "<b>Comuna:</b> {point.nom_comuna}<br><b>Porcentaje de personas en situación de pobreza</b>: %{point.value}"
  ) %>% 
  hc_title(
    text = "<b>Estimación de pobreza comunal en la Región del Biobío (2017)</b>"
  ) %>% 
  hc_subtitle(
    text = "Fuente de datos: CASEN 2017 y Shapes Precenso de @robsalasco"
  )

