#participation Birdlab

# creation db_partic_birdlab : df de participation qui permet de trouver commune miroir
# creation df_commune_birdlab : df permettant de spatialiser à l'échelle des communes le df de données birdlab


source("programs/library.R")
source("programs/function_geo.R")
readRenviron(".env")

# couche des communes avec leur code INSEE
insee <- sf::read_sf(here::here("maps", "communes-version-simplifiee.geojson")) #importer couche metropole


### FONCTIONS ----


#probleme d'inversion de latitude et longitude
# fonction qui vérifie et corrige
inversion_lat_long <- function(df_lat_long) {
  # Identifier les lignes où latitude est inférieure à longitude
  condition <- df_lat_long$latitude < df_lat_long$longitude
  
  # Intervertir les valeurs en utilisant une méthode vectorisée
  df_lat_long[condition, c("latitude", "longitude")] <- df_lat_long[condition, c("longitude", "latitude")]
  
  return(df_lat_long)
}

############################################################################


# jointure spatiale df birdlab et couche insee ----

func_df_commune_birdlab <- function() {
  # source("programs/db_birdlab.R") 
  # write_rds(data_birdlab, "data/data_birdlab.rds")
  data_birdlab <- readRDS("data/data_birdlab.rds")
  
  data_birdlab_filtre = data_birdlab %>%
    filter(!longitude == 0) %>%
    filter(!latitude == 0) 
  
  data_birdlab_filtre <- inversion_lat_long(data_birdlab_filtre)
  
  df_points_sf <- data_birdlab_filtre %>% 
 #   select(participation_id, user_id, participation_date, latitude, longitude) %>% 
 #   na.omit() %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
  
  return(df_points_sf)
}

birdlab_spatial <- func_df_commune_birdlab()

join_insee_birdlab <- function() {  
  # join avec la couche des communes insee
  join <- st_join(insee, birdlab_spatial) %>%
    filter(!is.na(participation_id)) %>%
 #   select(-participation_id) %>%
    unique() #
  
  return(join)
}

df_commune_birdlab <- join_insee_birdlab()# df site miroir ----
# fonction qui permet de produire le df de participation permettant de chercher les sites miroirs avec les autres protocoles



#join <- df_commune_birdlab


func_db_partic_birdlab <- function(join) {
  
  
  ## Nombre de participations par commune
  
  recap_join <- as.data.frame(join) %>%
    select(-geometry) %>%
    select(code, nom, participation_id) %>% #user_id, participation_date ?
    unique() %>%
    group_by(code) %>%
    summarize(nb_participation = n()) %>%
    ungroup() 
  
  ## Nombre d'années de participation par commune
  
  #recuperation des annees
  join$year = gsub("(\\d{4})-.*","\\1" , join$participation_date)
  
  # nb d'années de participation au protocole par intercommunes
  recap_join1 <- as.data.frame(join) %>%
    select(-geometry) %>% 
    select(code, nom, year) %>%
    unique() %>%
    as.data.frame() %>%
    group_by(code) %>%
    summarize(nb_an = n()) %>%
    ungroup() 
  
  
  
  ## nombre de participations par participants par commune
  recap_join2 <- as.data.frame(join) %>%
    select(-geometry) %>%
    select(code, nom, participation_date, participation_id, user_id) %>%
    unique() %>%
    as.data.frame() %>%
    group_by(user_id, code) %>%
    summarize(nb_partic_user_par_com = n()) %>%
    ungroup()   
  
  
  ## nombre de participants par commune
  recap_join3 <- as.data.frame(join) %>%
    select(-geometry) %>%
    select(code, nom, user_id) %>%
    unique() %>%
    as.data.frame() %>%
    group_by(code) %>%
    summarize(nb_participants_com = n()) %>%
    ungroup()  
  
  
  
  db_partic_birdlab = recap_join1 %>% 
    left_join(recap_join) %>%
    left_join(recap_join2) %>%
    left_join(recap_join3) %>%
    mutate(protocole = "birdlab") %>%
    left_join(insee)
    
  return(db_partic_birdlab)
}

db_partic_birdlab <- func_db_partic_birdlab(df_commune_birdlab)


# CARTES ----
# nombre de participations par commune
# 
# mypalette <- colorNumeric(palette = "viridis", domain = recap_join$nb_participation)
# 
# 
# map = leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = recap_join$geometry, 
#               color = mypalette(recap_join$nb_participation), 
#               fillOpacity = 1, 
#               stroke = FALSE,
#               label = recap_join$nom) %>%
#   addLegend(position = c("topright"), pal = mypalette, values = recap_join$nb_participation,
#             title = "nombre de participations par commune",
#             opacity = 1)
# 
# map
# 
# 
# # nombre d'annees de participation par commune
# 
# mypalette <- colorNumeric(palette = "viridis", domain = recap_join1$nb_an)
# 
# map = leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = recap_join1$geometry, 
#               color = mypalette(recap_join1$nb_an), 
#               fillOpacity = 1, 
#               stroke = FALSE ) %>%
#   addLegend(position = c("topright"), pal = mypalette, values = recap_join1$nb_an,
#             title = "nombre d'annees de participation par commune",
#             opacity = 1)
# 
# map
# 





