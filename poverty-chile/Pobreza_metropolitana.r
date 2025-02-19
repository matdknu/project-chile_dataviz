library(geojsonio)
library(tidyverse)
library(readxl)
library(highcharter)

url_santiago <- "https://raw.githubusercontent.com/robsalasco/precenso_2016_geojson_chile/master/Comunas/R13.geojson"
pobreza <- read_excel("BASE_TECNICA_FCM_2020_Version3_07052020_H2349_SINIM.xlsx", 
                      sheet = "IND POBREZA", skip = 2)

santiago <- jsonlite::fromJSON(url_santiago, simplifyVector = FALSE)

total_comunas <- santiago$features %>% 
  purrr::map_df("properties") %>% 
  rename_all(stringr::str_to_lower) %>% 
  select(comuna, nom_comuna) %>% 
  mutate(
    comuna = as.numeric(comuna),
    nom_comuna = stringr::str_to_title(nom_comuna)
  )

santiago_geojson <- geojsonio::as.json(santiago)


# Formatear datos pobreza
names(pobreza)[names(pobreza) == "Estimación de pobreza Comunal (CASEN 2017)"] <- "pobreza_dec"
names(pobreza)[names(pobreza) == "Código Territorial"] <- "codigo"
names(pobreza)[names(pobreza) == "Nombre de la Comuna"] <- "nom_comuna"

# Convertir a porcentajes y redondear
pobreza_select <- pobreza %>%
  mutate(value = round(pobreza_dec * 100, 2),
         nom_comuna = stringr::str_to_title(nom_comuna)) %>% 
  select(value, nom_comuna)

# Unir datos pobreza con mapa a través del nombre de la comuna
total_pob <- total_comunas %>% 
  left_join(pobreza_select, by = "nom_comuna")


write.csv(total_pob, "total_pob.csv")


# Highchart
highchart(type = "map") %>%
  hc_add_series(
    mapData = santiago_geojson,
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
    text = "<b>Estimación de pobreza comunal en la Región Metropolitana (2017)</b>"
  ) %>% 
  hc_subtitle(
    text = "Fuente de datos: CASEN 2017 y Shapes Precenso de @robsalasco"
  )

