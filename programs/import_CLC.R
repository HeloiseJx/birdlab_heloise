
# IMPORT DE LA COUCHE HABITAT CLC ----------------------------------------------
CLC = st_read("maps/CLC12_FR_RGF_SHP/CLC12_FR_RGF_SHP/CLC12_FR_RGF.shp", quiet = TRUE) 

# libelle des habitats CLC pour les niveaux 1 & 2
descr_clc2 = read.csv2("maps/CLC_niveau_2.csv") %>%
  mutate(code_2 = as.character(code_2)) %>%
  rename(libelle_code_2 = libelle_fr)

descr_clc1 = data.frame(libelle_code_1 = c("Territoires artificialisés", "Territoires agricoles", "Forêts et milieux semi-naturels", "Zones humides", "Surfaces en eau"), code_1 = as.character(c(1:5)) )


# left_join avec les libelles
CLC_12 = CLC %>%
  mutate(code_1 = substr(CODE_12, 1, 1), code_2 = substr(CODE_12, 1, 2)) %>%
  left_join(descr_clc2) %>%
  left_join(descr_clc1) %>%
  st_as_sf() %>% 
  st_transform(crs = 4326)

rm(descr_clc2)
rm(descr_clc1)


# ggplot(CLC_12 %>% select(code_1, libelle_code_1)) +
#   geom_sf(aes(fill = libelle_code_1))
#   

