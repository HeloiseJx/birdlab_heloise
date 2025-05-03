

####-----------EXTRACTION DES DONNÉES POUR LEAFLET---------------####


#' Extraction de coordonnées de la LineString json pour leaflet
#' polyline
#' 
#' @param my_linestring Character json LineString
#'
#' @return 
#' @export
#'
#' @examples
polyline <- function(my_linestring){
  geojson_obj <- fromJSON(my_linestring)
  coordinates <- geojson_obj$coordinates
  coordinates
}




#' Transformation de la linestring de la colonne `site_coordonnees` en st_polygone lisible par leaflet
#' polygone
#'
#' @param my_string 
#'
#' @return
#' @export
#'
#' @examples
polygone <- function(my_string){
  latlong <- unlist(str_extract_all(my_string, "-?\\d+(\\.\\d+)?")) #récupère juste les coordonnées
  lat <- as.numeric(latlong[which(seq_along(latlong) %% 2 == 1)])
  long <- as.numeric(latlong[which(seq_along(latlong) %% 2 == 0)])
  st_sfc(st_polygon(x = list((cbind(lat, long)))), crs = 4326)
}



####------------CARTES LEAFLET ------------------------------------####



#' UN TRANSECT, PLUSIEURS TRACÉS EXISTANTS
#' carte_traces
#'
#' @param id_transect A character
#' @param df_geo A data_frame contenant les colonnes transect_id, 
#'                transect_coordonnees et year (une liste des années)
#'
#' @return A leaflet object
#' @export
#'
#' @examples
#' 
#' 
carte_traces <- function(id_transect, df_geo){
  
  # Combien de fois le transect a t il été défini et en quelles années? 
  df_ex_trans <- df_geo %>%
    filter(transect_id == id_transect) %>%
    group_by(transect_id, transect_coordonnees) %>%
    summarise(nb_annees=n(), liste_annees = paste(year, collapse = ", "))
  
  # Représentation graphique des différents tracés
  palette_couleurs <- rainbow(nrow(df_ex_trans))
  etiquettes <- paste('Transect défini en', df_ex_trans$liste_annees)
  map <- leaflet() |>
    addTiles()
  for (i in 1:nrow(df_ex_trans)) {
    map <- map %>%
      addPolylines(data = polyline(df_ex_trans$transect_coordonnees[i]),
                   color = palette_couleurs[i], 
                   label= etiquettes[i]
      ) 
  }
  map %>%
    addLegend(colors = palette_couleurs,
              labels = etiquettes
    )
}





#' UN SITE ET SES TRANSECTS
#'
#' @param nsite 
#' @param df_geo A data_frame contenant les colonnes site_id, site_coordonnées,
#'                 transect_id, transect_coordonnees 
#'               
#'
#'                
#' @return map
#' @export
#'
#' @examples
carte_site_transects <- function(nsite, df_geo){
  
  # On récupère le nomtre de transects du site 
  nb_transect_site <- nrow(subset(df_geo, site_id == nsite))
  
  # Je projette d'abord le site
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = polygone(df_geo$site_coordonnees[df_geo$site_id == nsite]),
                color = 'red',
                fillColor = 'red',                      # Couleur de remplissage
                fillOpacity = 0.2,
                weight = 1)
 
   # Je projette ensuite les transects
  for (i in 1:nb_transect_site){
    map <- map |>
      addPolylines(data = polyline(df_geo$transect_coordonnees[df_geo$site_id == nsite][i]),
                   color = 'blue',
                   weight = 1.5)
  }
  map
}





####------------CALCULS DE DISTANCES ------------------------------------####



#' Calcul de la longueur du transect à partir de ses coordonnées
#'
#' @param my_linestring 
#'
#' @return une longueur (sf object)
#' @export
#'
#' @examples

transect_length <- function(my_linestring){
  # Lire le LineString à partir de la colonne JSON
  line <- st_transform(st_read(my_linestring, quiet = TRUE))
  # Projeter en Lambert 93 (EPSG:2154)
  line_projected <- st_transform(line, crs = 2154)
  
  # Calculer la longueur en mètres
  return(st_length(line_projected))
}



#' Calcule la distance entre deux poins étant donnés lat et long
#'
#' @param lat1 
#' @param long1 
#' @param lat 
#' @param long 
#'
#' @return
#' @export
#'
#' @examples
#' 
distance_m <- function(lat1, long1, lat, long) {
  # Utiliser distHaversine avec les coordonnées du point fixe et celles du point à comparer
  return(distHaversine(c(long1, lat1), c(long, lat)))  # distHaversine renvoie la distance en mètres
}


#' Une fonction qui tranforme une linestring de la colonne `transect_coordonnees`en liste de coordonnées
#'
#' @param my_string 
#'
#' @return
#' @export
#'
#' @examples
coords <- function(my_string){
  latlong <- unlist(str_extract_all(my_string, "-?\\d+(\\.\\d+)?")) #récupère juste les coordonnées
  lat <- as.numeric(latlong[which(seq_along(latlong) %% 2 == 0)])
  long <- as.numeric(latlong[which(seq_along(latlong) %% 2 == 1)])
  return(list(long, lat))
}



# Des fonctions qui à partir de la liste de coordonnées précédentes donnent un "point moyen"
long_moy <- function(my_string){
  mean(coords(my_string)[[1]])
}
lat_moy <- function(my_string){
  mean(coords(my_string)[[2]])
}




#### ------- FONCTIONS HABITATS ------------------------####



#' Table de tous les transects d'un habitat donné (avec variables fiche Habitat)
#' table_par_habitat
#'
#' @param df_propage_hab A dataframe des transects 
#'                      avec TOUTES les variables d'habitat
#' @param habitat A character : une modalité de "transect_habitat"
#'
#' @return
#' @export
#'
#' @examples
table_par_habitat <- function(df_propage_hab, habitat){
  
  liste_prairie = c("transect_type_prairie", 
                           "transect_paturage_prairie",
                           "transect_rythme_fauchage_prairie")
  
  liste_friche = c("transect_paturage_friche", 
                           "transect_rythme_fauchage_friche",
                           "transect_rythme_arbustes_friche")
  
  liste_square = c("transect_type_arbres_square", 
                          "transect_type_plantes_square")
  
  liste_gazon = c("transect_frequence_tontes", 
                          "transect_type_arbres_gazon",
                          "transect_type_plantes_gazon")
  
  liste_jardin = c("transect_type_plantations", 
                          "transect_plantes_ornementales",
                          "transect_plantes_aromatiques")
  
  liste_cimetiere = c("transect_type_allees", 
                             "transect_type_arbres_cimetiere")
  
  liste_infrastructure = c("transect_arbustes_infrastructures", 
                                  "transect_rythme_fauchage_infrastructures",
                                  "transect_environnement")
  
  liste_lisiere = c("transect_type_arbres_lisiere")
  
  variables_hab <- if (habitat == "Prairie"){
    variables_hab <- liste_prairie
  }else if (habitat == "Friche"){
    variables_hab <- liste_friche
  }else if (habitat == "Jardin horticole / Jardin potager"){
    variables_hab <- liste_jardin
  }else if (habitat == "Square urbain (mixte surface pelouse / surfaces non végétalisée)"){
    variables_hab <- liste_square
  }else if (habitat == "Gazon"){
    variables_hab <- liste_gazon
  }else if (habitat == "Cimetière"){
    variables_hab <- liste_cimetiere
  }else if (habitat == "Bords d’infrastructures de transport (routes, voies ferrées...)"){
    variables_hab <- liste_infrastructure
  }else if (habitat == "Lisière de bois ou de forêt" ){
    variables_hab <- liste_lisiere}
  
  df_habitat <- df_propage_hab %>% 
    filter(transect_habitat == habitat) %>%
    select(transect_id, variables_hab, year) %>%
    unique() 
  return(df_habitat)                   
}


#' Table des NA par habitat
#' table_na_habitat
#'
#' @param habitat A character de l'habitat étudié
#'                  
#'
#' @return
#' @export
#'
#' @examples
table_na_habitat <- function(df_propage_hab, habitat){
  
  # On utilise la fonction précédente pour créer le df par habitat
  df_habitat = table_par_habitat(df_propage_hab, habitat)
  
  # Création du data frame pour répertorier les NA selon les variables de chaque
  # habitat sur chaque année. 
  # On adapte ensuite ce dataframe aux différentes parties où
  # on a besoin de visualiser ces variables
  df_na_hab = data.frame()
  df_na_hab_val = data.frame()
  
  # On fait une boucle sur les années
  for (an in sort(unique(df_habitat$year))) {
    # On filtre sur l'année qu'on veut regarder
    df_tmp = df_habitat %>%
      filter(year == an)
    
    # On colle les data frame de chaque année les unes à la suite des autres
    df_na_hab = rbind(df_na_hab,
                      # Sur le df filtré, on applique sur toutes les colonnes un calcul
                      # de la proportion de NA sur chaque colonne
                      df_tmp %>%
                        summarise(across(everything(),
                                         ~ mean(is.na(.)),
                                         .names = "{col}")) %>%
                        mutate(annee = as.numeric(an)))
    
    # On fait la même chose avec un dataframe qui contient les valeurs et non les proportions
    df_na_hab_val = rbind(df_na_hab_val,
                          df_tmp %>%
                            summarise(across(everything(),
                                             ~ sum(is.na(.)),
                                             .names = "{col}")) %>%
                            mutate(annee = as.numeric(an)))
    
  }
  
  df_na_hab = df_na_hab %>% 
    
    # On ne garde que les colonnes où la moyenne de la colonne est différente de 0
    # (On supprime les variables sans aucune NA sur toutes les années)
    # modif : Je ne le fais pas sur les habitats!
    # select(c(annee, where(~ mean(., na.rm = TRUE) != 0))) %>%
    
    # On enlève les colonnes transect_id et year
    select(-year, -transect_id) %>%
    
    # On remplace les cases égales à 0 par des NA (pour que les cases == 0 
    # s'affichent en gris sur le graphique)
    mutate(across(everything(), ~ na_if(., 0))) %>%
    pivot_longer(cols = !annee, names_to = "variable", values_to = "valeur")
  
  df_na_hab_val = df_na_hab_val %>%
    #select(c(annee, where(~ mean(., na.rm = TRUE) != 0))) %>%
    select(-year, -transect_id) %>%
    pivot_longer(cols = !annee, names_to = "variable", values_to = "texte")
  
  # On joint les deux dataframe
  df_na_hab = df_na_hab %>%
    left_join(df_na_hab_val, by = c("annee"="annee", "variable"="variable"))
  
  # On redéfinit les facteurs pour qu'ils apparaissent dans l'ordre de la fiche
  
  df_na_hab = df_na_hab %>%
    mutate(variable = case_when(
      habitat == "Prairie" ~ factor(variable, levels = c("transect_type_prairie",
                                                            "transect_paturage_prairie",
                                                            "transect_rythme_fauchage_prairie")),
      
      habitat == "Friche" ~ factor(variable, levels = c("transect_paturage_friche", 
                                                           "transect_rythme_fauchage_friche",
                                                           "transect_rythme_arbustes_friche")),
      habitat == "Square urbain (mixte surface pelouse / surfaces non végétalisée)" ~ factor(variable,levels = c("transect_type_arbres_square", 
                                                                                                                 "transect_type_plantes_square")),
      habitat == "Gazon" ~ factor(variable, levels = c("transect_frequence_tontes", 
                             "transect_type_arbres_gazon",
                             "transect_type_plantes_gazon")),
      habitat == "Jardin horticole / Jardin potager" ~ factor(variable, levels = c("transect_type_plantations", 
                                                                                   "transect_plantes_ornementales",
                                                                                   "transect_plantes_aromatiques")),
      habitat == "Cimetière" ~ factor(variable, levels = c("transect_type_allees", 
                                                           "transect_type_arbres_cimetiere")),
      habitat == "Bords d’infrastructures de transport (routes, voies ferrées...)" ~ factor(variable, levels = c("transect_arbustes_infrastructures", 
                                                                                                                 "transect_rythme_fauchage_infrastructures",
                                                                                                                 "transect_environnement")),
      habitat == "Lisière de bois ou de forêt" ~ factor(variable, levels = c("transect_type_arbres_lisiere"))
        
    ))
  
  
  return(df_na_hab)
}


#' Table des transects avec variables habitats incomplètes 
#' (par habitat)
#'
#' @param df_propage_hab 
#' @param hab 
#'
#' @return
#' @export
#'
#' @examples
table_hab_incomp <- function(df_propage_hab, hab){
  df_habitat <- table_par_habitat(df_propage_hab, hab) 
  df_habitat_incomp <- df_habitat %>%
    filter(!complete.cases(across(-year))) %>%
    group_by(across(-year)) %>%
    summarise(annees = paste(year, collapse = ","))
  return(df_habitat_incomp)
}

#' Title
#'
#' @param df_propage_hab 
#' @param hab 
#'
#' @return df_cur_hab
#' @export
#'
#' @examples
table_curation_hab <- function(df_propage_hab, hab){
  
  df_habitat <- table_par_habitat(df_propage_hab, hab)
  
  # table df complète de référence
  df_hab_comp <- df_habitat %>% 
    filter(complete.cases(.)) %>%
    group_by(transect_id, year) %>%
    summarise(n = n())
  
  # Rem : ce dataframe permet de détecter les changements de valeurs des variables
  
  # Transects pour lesquels les variables d'habitat sont renseignées 
  # différemment la même année et pour lesquels il y a complétude
  df_multi_var <- df_habitat %>% 
    group_by(transect_id, year) %>%
    summarise(n =n()) %>%
    ungroup() %>%
    #filter(n > 1 & transect_id %in% liste_trans_incomp)
    filter(n > 1) %>%
    semi_join(df_hab_comp, by = c("transect_id", "year"))
  
  
  # Transects qui peuvent être curés
  df_curables <- df_habitat %>%
    filter(!complete.cases(.)) %>%
    inner_join(df_multi_var, by = c("transect_id", "year"))
  
  # Table de curation
  df_cur_hab <- df_habitat %>%
    semi_join(df_curables, by = c("transect_id", "year")) %>%
    arrange(transect_id) 
  
  return(list(df_cur_hab=df_cur_hab, df_curables=df_curables))
}




#' Annuaire des personnes à contacter (par Habitat)
#'
#' @param df_propage_hab 
#' @param hab 
#'
#' @return df_contacts
#' 
#'
#' @examples
table_contacts_hab <- function(df_propage_hab, hab) {
  
  df_habitat <- table_par_habitat(df_propage_hab, hab)
  df_curables <- table_curation_hab(df_propage_hab, hab)$df_curables
  
  df_contacts<- df_habitat %>%
    filter(!complete.cases(across(-year))) %>%
    inner_join(df_annuaire, by = c("transect_id", "year")) %>%
    # j'enlève ceux que je peux curer han
    anti_join(df_curables, by = c("transect_id", "year")) %>%
    group_by(structure_nom, site_departement, user_pseudo, user_email, year) %>%
    summarise(#annees = paste(year, collapse = ","),
      nombre_transects = n(),
      transects_concernés = paste(transect_id, collapse = ", ")
    ) %>%
    ungroup()%>%
    arrange(desc(nombre_transects))
    
    return(df_contacts)
}


# FONCTIONS TRANSECTS -------
  
# une fonction qui prend en argument une distance min, max
# renvoie la liste des transects possédant des tracés distants de entre min et max km
# dans la liste des transect que l'on avait déjà repérés

liste_1_id_multi_traces <- function(dist_min, dist_max, df_trans_modif, df_geo_moy){
  liste_bug <- c()
  for (i in df_trans_modif$transect_id){
    df1 <- df_geo_moy[df_geo_moy$transect_id==i,]
    
    long1 <- df1$long_moy[1]
    lat1 <- df1$lat_moy[1]
    
    # Appliquer la fonction à chaque ligne du tibble pour calculer les distances par rapport au point fixe
    df_distances <- df1 %>%
      rowwise() %>%
      mutate(distance_from_point = distance_m(lat1, long1, lat_moy, long_moy)) %>%
      ungroup()
    liste_bug <- c(liste_bug, unique(df_distances$transect_id[df_distances$distance_from_point < dist_max & df_distances$distance_from_point > dist_min]))
  }
  return(liste_bug)
}



table_selec_releves <- function(trans_id){
  coord_trans <- (df_geo_trans %>%
                    filter(transect_id == trans_id))$transect_coordonnees
  df_table_selec_releves <- df_propage %>%
    select(transect_id, id_releve, releve_date, year, 
           site, structure_nom, user_pseudo, site_departement, site_coordonnees,transect_coordonnees) %>%
    unique() %>%
    filter(transect_coordonnees %in% coord_trans) %>%
    arrange(releve_date)
  return(df_table_selec_releves)
}
