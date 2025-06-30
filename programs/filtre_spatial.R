# ce script permet de produire 2 types de df : un spatial et l'autre non, de manière 
# à faciliter la manipulation des données en fonction qu'elles aient besoin d'être
# spatialisées ou non :

# - coord_... : df contenant juste les coordonnées des points pour différentes 
# échelles

# - data_..., dt_... : df contenant les infos sur les collections à différentes échelles
# (contient aussi longitude et latitude donc on peut faire cartes à partir de ces données)


# les filtres spatiaux sont à choisir manuellement 

# FILTRE SPATIAL LOCAL ####################################

## coordonnées birdlab national ----
# a l'échelle nationale, on applique différents filtre pour ne garder que les coordonnées 
# correctes

invisible(capture.output({france = read_sf(here::here("maps","metropole-version-simplifiee.geojson"), crs = 4326)}))

coord_birdlab_national <- data_birdlab %>% 
  select(partie_id, longitude, latitude) %>% 
  mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude)) %>% # certaines coordonnées sont nulles
  na.omit() %>% # on enlève les na
  distinct() %>%
  filter(longitude != 0 & latitude != 0) %>%
  filter(
    longitude != 0,
    latitude != 0,
    longitude >= -180,
    longitude <= 180,
    latitude >= -90,
    latitude <= 90) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  %>% # on passe tout en format sf, projection WGS 84
  st_join(france, left = F)




# verification graphique
# ggplot() +
#   geom_sf(data = france) +
#   geom_sf(data = coord_birdlab_national[1:40000, ])
#   coord_sf()


# chargement d'autres emprises spatiales
## import des couches des intercommunes
invisible(capture.output({coord_epci <- st_read(here::here("maps/ADMIN-EXPRESS_3-2__SHP_WGS84G_FRA_2024-12-18/ADMIN-EXPRESS_3-2__SHP_WGS84G_FRA_2024-12-18/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-12-00243/ADE_3-2_SHP_WGS84G_FRA-ED2024-12-18/EPCI.shp"), quiet = TRUE) }))

# epci <- epci[-c(26, 526),]
coord_plaineco = coord_epci %>% filter(NOM == "Plaine Commune") 
coord_terreenv = coord_epci %>% filter(NOM == "Paris Terres d'Envol") 
coord_metro_lyon = coord_epci %>% filter(NOM == "Métropole de Lyon") 

## differentes coordonnees birdlab locales ----
# jointure avec les coordonnées des points birdlab
# coordonnées birdlab pour les intercommunes selectionnées
coord_birdlab_plaineco = st_join(coord_birdlab_national, coord_plaineco, left = FALSE)
coord_birdlab_terreenv = st_join(coord_birdlab_national, coord_terreenv, left = FALSE)
coord_birdlab_lyon = st_join(coord_birdlab_national, coord_metro_lyon, left = FALSE)


# verification
# ggplot() +
#   geom_sf(data = birdlab_plaineco) +
#   geom_sf(data = birdlab_terreenv) +
#   geom_sf(data = birdlab_lyon) +
#   coord_sf()



## df local birdlab ----
# on récupère toutes les données liées aux parties de birdlab dans l'emprise spatial 
# locale qui nous intéresse
# dt_birdlab_local = df de travail par la suite, non spatialisé
# contient quand meme longitude et latitude
dt_birdlab_local = as.data.frame(coord_birdlab_plaineco) %>% # on repasse en df, on enlève la colonne spatialisée
  select(partie_id) %>% 
  left_join(data_birdlab) 

## coordonnées birdlab locales ----
# coordonnées associées aux parties du df dt_birdlab_local
coord_birdlab_local <- dt_birdlab_local %>% 
  select(partie_id, longitude, latitude) %>% 
  distinct() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# REFERENTIELS ##########################################################

## import des referentiels ----
invisible(capture.output({regions = st_read("maps/regions-version-simplifiee.geojson") }))



invisible(capture.output({biogeoregions = st_read("maps/region_biogeo_fr/region_biogeographique.shp") %>%
  st_transform(crs = 4326) # passage du lambert 93 au WGS 84
}))


# certains sommets dupliqués : on corrige les erreurs
invalid_index <- which(!st_is_valid(biogeoregions))
biogeoregions[invalid_index, ] <- st_make_valid(biogeoregions[invalid_index, ])


# # verification
# ggplot() +
#   geom_sf(data = biogeoregions, aes(fill = N_DOMAINE))

descri_greco = read.csv(here::here("maps/ser_l93/correspondance_greco_description.csv"), sep = ";")

invisible(capture.output({data_greco =  st_read("maps/ser_l93/ser_l93.shp")  %>%
  st_transform(crs = 4326) %>% # passage du lambert 93 au WGS 84
  mutate(n_greco = str_sub(codeser, start = 1, end = 1)) %>%
  na.omit() }))



greco = data_greco %>%
  group_by(n_greco) %>%
  summarize(geom = st_union(geometry)) %>%
  ungroup() %>%
  left_join(descri_greco)

rm(data_greco, descri_greco)

# verification
# ggplot() +
#   geom_sf(data = greco, aes(fill = n_greco)) +
#   coord_sf()


## donnees des referentiels calcules par rapport aux donnees locales ----
# 
# la partie suivante sert à coder les df regroupant les coordonnées des parties de birdlab
# pour différents referentiels calculés à partir des données de birdlab locales. Ce code s'ajuste
# automatiquement au changement de localité pour les données sur lesquelles on decide de travailler

###ref_region ----

# identification du referentiel regional administratif
region_reference = coord_birdlab_local %>%
  st_join(regions) %>%
  as.data.frame() %>%
  select(nom) %>%
  unique() %>%
  left_join(as.data.frame(regions)) %>%
  st_as_sf()

# donnees birdlab produites dans ce référentiel, filtre sur les données nationales
ref_regionadmin = coord_birdlab_national %>%
  st_join(region_reference) %>%
  na.omit()

rm(region_reference)
  

###ref_biogeoregion ----
# identification biogeoregion de ref
biogeoregion_reference = coord_birdlab_local %>%
  st_join(biogeoregions) %>%
  as.data.frame() %>%
  select(CODE, N_DOMAINE) %>%
  unique() %>%
  left_join(as.data.frame(biogeoregions)) %>%
  st_as_sf()

# donnees birdlab produites dans ce référentiel, filtre sur les données nationales
ref_biogeoregion = coord_birdlab_national %>%
  st_join(biogeoregion_reference) %>%
  na.omit()

rm(biogeoregion_reference)


###ref_greco ----
# identification biogeoregion de ref
greco_reference = coord_birdlab_local %>%
  st_join(greco) %>%
  as.data.frame() %>%
  select(n_greco, description) %>%
  unique() %>%
  left_join(as.data.frame(greco)) %>%
  st_as_sf()

# donnees birdlab produites dans ce référentiel, filtre sur les données nationales
ref_greco = coord_birdlab_national %>%
  st_join(greco_reference) %>%
  na.omit()

rm(greco_reference)




# PAYSAGE ######################################################################

## CLC ----
#  # CLC_12 : CLC niveau 1 et 2 
# CLC_12 = st_read(here::here("maps/CLC12_FR_RGF_SHP/CLC12_FR_RGF_SHP/CLC12_FR_RGF.shp")) %>%
#   st_transform(crs = 4326)

# 
# # recuperation polygone parc de la Tete d'Or Lyon a partir de la couche CLC ----
# CLC_site = CLC_12 %>% filter(ID == "FR-34337") 
# 
# coord_birdlab_site <- coord_birdlab_national %>%
#   st_join(CLC_site) %>%
#   filter(!is.na(CODE_12)) %>%
#   select(session_id, geometry)
# 
# dt_birdlab_site =  as.data.frame(coord_birdlab_site) %>% select(session_id) %>% left_join(dt_birdlab_total)
# 
# # jointure couche métro lyon et habitats CLC = habitats lyon
# CLC_lyon = CLC_12 %>% st_join(metro_lyon, left = F) 
# 
# # jointure couche régions et habitats CLC = habitats regionaux ou nationaux
# CLC_regions = CLC_12 %>% st_join(regions, left = F)
# CLC_aura = CLC_regions %>% filter(nom == "Auvergne-Rhône-Alpes")
# 
# dt_birdlab_aura =  as.data.frame(coord_birdlab_aura) %>% select(session_id) %>% left_join(dt_birdlab_total)

