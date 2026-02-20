#  STATS PAR CARTO
# OBJ  : 
# 1 _ choisir les meilleures carto pour chaque jeu de données
# 2 _ voir si la combinaison de référentiels fonctionne
# 3 _ voir quelles sont les structures qui participent et construire un référentiel à partir de ça




# COUVERTURE SOLS ----

## CLC ----
# 
# dt_protoc_clc = dt_protoc %>%
#   st_join(CLC_12) %>%
#   ggplot(aes(x = libelle_code_1, fill = libelle_code_1)) +
#   geom_histogram(stat = "count", width = 0.5) +
#   theme_minimal() +
#   theme(legend.position = "none")

# dt_protoc = dt_florileges



# diagramme ternaire représentant la distribution des points du protoc selon la ouverture du sol
func_diag_tern = function(dt_protoc_oso) {
  library(ggtern)
  
  
  # 
  # # Données : couche d'exemple pour les lignes à 33 %
  # lignes_33 <- data.frame(
  #   x     = c(0.33, 0.67, 0.67),
  #   y     = c(0.67, 0.33, 0),
  #   z     = c(0,    0,    0.33),
  #   xend  = c(0.33, 0,    0),
  #   yend  = c(0,    0.33, 0.67),
  #   zend  = c(0.67, 0.67, 0.33),
  #   color = c("red", "green", "blue")
  # )
  # 
  # # Tracer avec les lignes à 33%
  # ggtern(data = dt_protoc_oso, aes(x = `semi-naturel`, y = impermeable, z = agricole)) +
  #   geom_point() +
  #   geom_segment(
  #     data = lignes_33,
  #     aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend, color = color),
  #     # linetype = "dashed",
  #     size = 1,
  #     inherit.aes = FALSE
  #   ) +
  #   scale_color_identity()
  
  
  
  # # Tracer avec les lignes à 50%
  # ggtern(data = dt_protoc_oso, aes(x = `semi-naturel`, y = impermeable, z = agricole)) +
  #   geom_point() +
  #   geom_segment(
  #     data = lignes_50,
  #     aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend),
  #     # linetype = "dashed",
  #     size = 1,
  #     inherit.aes = FALSE
  #   ) +
  #   
  #   # Triangle des milieux
  #   geom_path(
  #     data = points_milieu,
  #     aes(x = x, y = y, z = z),
  #     color = "black",
  #     linewidth = 1.2,
  #     inherit.aes = FALSE
  #   )
  
  
  
  
  
  lignes_50 <- data.frame(
    x     = c(0.5, 0.5, 0),
    y     = c(0.5, 0,   0.5),
    z     = c(0,   0.5, 0.5),
    xend  = c(0.5, 0,   0.5),
    yend  = c(0,   0.5, 0.5),
    zend  = c(0.5, 0.5, 0)
  )
  
  points_milieu <- data.frame(
    x = c(0.25, 0.5, 0.25),
    y = c(0.25, 0.25, 0.5),
    z = c(0.5, 0.25, 0.25)
  )
  
  # Ajouter le 1er point à la fin pour fermer le triangle
  points_milieu <- rbind(points_milieu, points_milieu[1, ])
  
  # diagramme
  diag_ter = ggtern(data = dt_protoc_oso, aes(x = `semi-naturel`, y = impermeable, z = agricole)) +
    geom_point(aes(color = paysage_local) )+
    geom_segment(
      data = lignes_50,
      aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend),
      # linetype = "dashed",
      size = 1,
      inherit.aes = FALSE
    ) +
    
    # Triangle des milieux
    geom_path(
      data = points_milieu,
      aes(x = x, y = y, z = z),
      color = "black",
      linewidth = 1.2,
      inherit.aes = FALSE
    )
  
  return(diag_ter)
  
}

func_diag_tern_birdlab = function(dt_protoc_oso) {
  library(ggtern)
  
  
  lignes_50 <- data.frame(
    x     = c(0.5, 0.5, 0),
    y     = c(0.5, 0,   0.5),
    z     = c(0,   0.5, 0.5),
    xend  = c(0.5, 0,   0.5),
    yend  = c(0,   0.5, 0.5),
    zend  = c(0.5, 0.5, 0)
  )
  
  # points_milieu <- data.frame(
  #   x = c(0.25, 0.5, 0.25),
  #   y = c(0.25, 0.25, 0.5),
  #   z = c(0.5, 0.25, 0.25)
  # )
  # 
  # # Ajouter le 1er point à la fin pour fermer le triangle
  # points_milieu <- rbind(points_milieu, points_milieu[1, ])
  
  # diagramme
  diag_ter = ggtern(data = dt_protoc_oso, aes(x = `vegetation`, y = impermeable, z = agricole)) +
    geom_point(aes(color = paysage_local) )+
    geom_segment(
      data = lignes_50,
      aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend),
      # linetype = "dashed",
      size = 1,
      inherit.aes = FALSE
    ) 
  # +
    # 
    # # Triangle des milieux
    # geom_path(
    #   data = points_milieu,
    #   aes(x = x, y = y, z = z),
    #   color = "black",
    #   linewidth = 1.2,
    #   inherit.aes = FALSE
    # )
  
  return(diag_ter)
  
}



# histogramme repartition des points dans paysage local :
func_histo_paysage_local = function(dt_protoc_oso, buffer = NULL) {
  pal_paysloc <- c(
    "equilibre" = "lightblue",  
    "mixte_agricole_impermeable" = "#AC9ECEFF", 
    "mixte_impermeable_seminaturel" = "#FF9898FF", 
    "mixte_agricole_seminaturel" = "lightgreen",
    "dominance_impermeable" = "darkgrey", 
    "dominance_agricole" = "#E9A800FF", 
    "dominance_semi_naturel" = "#02734AFF",
    "dominance_vegetation" = "#02734AFF"
  )
  
  # histogrammes pour différents buffer (en m)
  plot_buffer_oso = ggplot(dt_protoc_oso, aes(x = paysage_local)) +
    geom_histogram(stat = "count", aes(fill = paysage_local)) +
    theme(axis.text.x = element_text(angle = 10, hjust = 1)) +
    scale_fill_manual(values = pal_paysloc) +
    labs(title = paste0("buffer ", buffer, " m")) +
    theme(legend.position = "none") 
  
  return(plot_buffer_oso)
}









# représentation carto de ces buffer
func_carto_paysage_local = function(dt_protoc_oso, buffer = NULL) {
  
  func_pal_paysloc <- colorFactor(palette = c( "lightblue",  "#AC9ECEFF", "#E9A800FF", "darkgrey", "#02734AFF", "#FF9898FF", "lightgreen"), 
                                  levels = unique(dt_protoc_oso$paysage_local))
  
  # Créer la carte
  leafl_oso = leaflet(dt_protoc_oso) %>%
    addTiles() %>%
    addCircleMarkers(
      color = ~ func_pal_paysloc(paysage_local),
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.8
    ) %>%
    addLegend("bottomright", 
              pal = func_pal_paysloc, 
              values = ~paysage_local,
              title = paste0("Couverture du sol sur", buffer,  " m"),
              opacity = 1)
  
  return(leafl_oso)
}
  



# répartition des points dans les différentes catégories de couverture du sol, à 
# l'emplacement où le point a été réalisé
# # donnée pas nécessaire car contrainte pas le protocole
# pal_couvpoint <- c(
#   "impermeable" = "darkgrey", 
#   "agricole" = "#E9A800FF", 
#   "semi-naturel" = "#02734AFF"
# )
# 
# # plot_enplact_oso = 
# ggplot(dt_protoc_oso, aes(x = couv_point)) +
#   geom_histogram(stat = "count", aes(fill = couv_point)) +
#   theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
#   scale_fill_manual(values = pal_couvpoint) 
# 
# 
# func_pal_couvpoint <- colorFactor(palette = c("darkgrey", "#E9A800FF", "#02734AFF"), 
#                              levels = unique(dt_protoc_oso$couv_point))
# 
# # Créer la carte
# leaflet(dt_protoc_oso) %>%
#   addTiles() %>%
#   addCircleMarkers(
#     color = ~ func_pal_couvpoint(couv_point),
#     radius = 5,
#     stroke = FALSE,
#     fillOpacity = 0.8
#   ) %>%
#   addLegend("bottomright", 
#             pal = func_pal_couvpoint, 
#             values = ~couv_point,
#             title = "Couverture",
#             opacity = 1)
# 

# 



# REFERENTIEL ECOLOGIQUE ---- 


func_plot_reg_ecolo = function(dt_protoc) {
  
  ## biogeoregions ----
  plot_protoc_biogeo = dt_protoc %>%
    st_join(biogeoregions) %>%
    ggplot(aes(x = N_DOMAINE, fill = N_DOMAINE)) +
    geom_histogram(stat = "count", width = 0.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
  ## greco  ----
  plot_protoc_greco = dt_protoc %>%
    st_join(greco) %>%
    ggplot(aes(x = description, fill = description)) +
    geom_histogram(stat = "count", width = 0.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
  plot_ecologique = plot_protoc_biogeo + plot_protoc_greco
  
  return(plot_ecologique)
}

  


# plot_ecologique = plot_protoc_biogeo + plot_protoc_greco

# REFERENTIEL ADMINISTRATIF ----

## regions ----

func_plot_reg_admin = function(dt_protoc) {
  
  plot_protoc_region = dt_protoc %>%
    st_join(regions) %>%
    ggplot(aes(x = nom, fill = nom)) +
    geom_histogram(stat = "count", width = 0.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(plot_protoc_region)
}


## metropole ----

func_plot_metro_admin = function(dt_protoc) {
  plot_protoc_metro = dt_protoc %>%
    st_join(metro) %>%
    # na.omit() %>% # on affiche les NA pour comprendre quelle quantité de donnée n'est pas dans une métropole
    ggplot(aes(x = NOM, fill = NOM)) +
    geom_histogram(stat = "count", width = 0.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(plot_protoc_metro)
}




