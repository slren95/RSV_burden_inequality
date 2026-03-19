library(tidyverse)
library(rio)
library(leaflet)
library(sf)



df_nirsevimab <- import('rda/df_nirsevimab.csv')
df_abrysvo <- import('rda/df_abrysvo.csv')
df_nir_abr_sf<-readRDS('rda/df_nir_abr_sf.rds')

class(df_nir_abr_sf)


create_link_html <- function(url, text = "Source") {
  ifelse(is.na(url),"",
         sprintf('<a href="%s" target="_blank">%s</a>', url, text))
}

pal <- colorFactor(
  palette = c("skyblue", "salmon"),
  domain = c("Licensed", "NIP-included")
)

leaflet(df_nir_abr_sf,
        options = leafletOptions(worldCopyJump = FALSE,minZoom = 1)) %>%
  setView(lng = 10, lat = 50, zoom = 2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setMaxBounds(
    lng1 = -30, lat1 = 35,   # 左下角
    lng2 = 60, lat2 = 70     # 右上角
  ) %>% 
  addPolygons(
    fillColor=~pal(type),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>", Country, "</b><br/>",
      "Nirsevimab Status: ", type, "<br/>",
      ifelse(!is.na(Licensed_Date), 
             paste0("License Date: ", Licensed_Date, "<br/>"), ""),
      ifelse(!is.na(Source1), 
             paste0("Source: ", create_link_html(Source1), "<br/>"), ""),
      ifelse(!is.na(Recommend_Date), 
             paste0("Recommended Date: ", Recommend_Date, "<br/>"), ""),
      ifelse(!is.na(Source2), 
             paste0("Source: ", create_link_html(Source2), "<br/>"), "")
    )
  ) %>%
  addLegend("bottomright",
            pal = pal,
            values = c("Licensed", "NIP-included"),
            labels = c("Licensed", "NIP-included"),
            title = "Nirsevimab Status")
