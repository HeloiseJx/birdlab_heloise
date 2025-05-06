# Indicateurs locaux

# choix de l'échelle de référence : à modifier ici une fois
dt_ref = data_birdlab

# DF GENERAUX RECAP ############################################################

df_turnover_participants = dt_birdlab_local %>%
  group_by(user_id) %>%
  reframe(first_time = min(saison)) %>%
  ungroup() %>%
  rename(saison = first_time) %>%
  group_by(saison) %>%
  reframe(nb_new_user = n_distinct(user_id))

# par saison : la richesse spécifique, le nombre de parties, de participants, de nouveaux participants
df_stat_saison = dt_birdlab_local %>%
  # filter(valide == "saison") %>% # filtre des données produites en saison %>%
  filter(espece != "Espèce inconnue") %>%
  group_by(saison) %>%
  reframe(nb_espece = n_distinct(espece), 
          nb_partie = n_distinct(partie_id), 
          nb_participant = n_distinct(user_id)) %>%
  left_join(df_turnover_participants)


# dernière saison finie
der_saison = case_when(month(Sys.Date()) %in% c(4,5,6,7,8,9,10) ~ paste0(year(Sys.Date())-1, "-", year(Sys.Date())),
                       TRUE ~ paste0(year(Sys.Date())-2, "-", year(Sys.Date())-1)
                       )

# donne soit la saison en cours, et si l'on est hors saison, la dernière saison
saison_en_cours_ou_derniere = case_when(month(Sys.Date()) %in% c(11,12,1,2,3) ~ paste0(year(Sys.Date())-1, "-", year(Sys.Date())),
                                        TRUE ~ der_saison)




# BIODIVERSITE #################################################################


# #carte collections --> plutôt  carte plus en bas dont la taille des cercles est 
# proportionnelle au nombre de parties par mangeoire
# pal <- colorFactor(palette = "viridis", domain = dt_birdlab_local$saison)
# 
# carte_collections <- leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(data = coord_birdlab_local %>% 
#                      left_join(dt_birdlab_local), 
#                    radius = 7, 
#                    color = ~pal(saison), 
#                    fillOpacity = 1, 
#                    stroke = F) %>% 
#   addPolygons(data = coord_metro_lyon, fillOpacity = 0) %>% 
#   addLegend(values = dt_birdlab_local$saison, title = "Saison", position = "topright", pal = pal)
# 

# richesse spécifique ----

# evolution 
plot_evol_richesse = ggplot(df_stat_saison, aes(x = factor(saison), y = nb_espece)) +
  geom_point() +
  labs(x = "richesse spécifique", 
       y = "saison birdlab") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# nombre d'espèces observées sur la saison / jauge nombre d'espèces de la liste
gauge(value = df_stat_saison$nb_espece[df_stat_saison$saison == saison_en_cours_ou_derniere], 
      min = 0, 
      max = 33, 
      label = paste0("saison ", saison_en_cours_ou_derniere))


# nombre d'espèces observées au total vs jauge--> fait, rmd
# hors saison compris
gauge(value = n_distinct(dt_birdlab_local$espece[dt_birdlab_local$espece != "Espèce inconnue"]), 
      min = 0, 
      max = 33, 
      label = paste0("relevés de ", min(dt_birdlab_local$year), " à ", max(dt_birdlab_local$year), "\n hors saison compris"))

# nombre d'espèces moyen observé lors d'une partie sur le site, sur l'ensemble des 
# relevés en saison
nb_moy_esp_partie_local = dt_birdlab_local %>%
  filter(valide == "saison", espece != "Espèce inconnue") %>% # filtre sur les données en saison
  group_by(partie_id) %>%
  summarize(nb_moy = n_distinct(espece)) %>%
  ungroup() %>%
  pull(nb_moy) %>%
  mean()
  

# nombre d'espèces moyen observé lors d'une partie dans le référentiel
nb_moy_esp_partie_ref = dt_ref %>%
  filter(valide == "saison", espece != "Espèce inconnue" ) %>% # filtre sur les données en saison
  group_by(partie_id) %>%
  summarize(nb_moy = n_distinct(espece)) %>%
  ungroup() %>%
  pull(nb_moy) %>%
  mean()


# fréquences d'espèces ----

# top / flop des espèces les plus fréquentes
df_freq_sp_local = dt_birdlab_local %>%
  filter(valide == "saison", espece != "Espèce inconnue" ) %>%
  group_by(espece) %>%
  summarize(freq = (n_distinct(partie_id)/ n_distinct(dt_birdlab_local$partie_id)) * 100) %>%
  ungroup() %>%
  arrange(desc(freq))

# barplot représentant les fréquences d'espèces localement.
# Ajouter les rangs des espèces dans le référentiel ?
plot_freq_local = ggplot(df_freq_sp_local, aes(y = freq, x = reorder(espece, freq))) +  
  geom_col(width = 0.3) +
  labs(title = "fréquence d'observation des espèces localement \n pourcentage de sites sur lesquelles elles ont été observées",
       x = "espèce",
       y = "fréquence d'observation (%)") +
  # scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +  # Rendre l'axe Y plus lisible
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))



# rerentiel : a modif

df_freq_sp_ref = dt_ref %>%
  filter(valide == "saison", espece != "Espèce inconnue" ) %>%
  group_by(espece) %>%
  summarize(freq = (n_distinct(partie_id)/ n_distinct(dt_ref$partie_id)) * 100) %>%
  ungroup() %>%
  arrange(desc(freq))

plot_freq_ref = ggplot(df_freq_sp_ref[1:5,], aes(y = freq, x = reorder(espece, freq))) +  
  geom_col(width = 0.3) +
  labs(title = "5 espèces les plus fréquentes dans le  \n référentiel",
       x = "espèce",
       y = "fréquence d'observation (%)") +
  # scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +  # Rendre l'axe Y plus lisible
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

# temps moyen par espèce passé à la mangeoire ----



# interaction ----

# reseau d'association 

# Cette représentation fonctionne plutôt sur des petits nombres de parties
# Elle considère les interactions comme l'observation d'especes lors de la meme partie
#  alors qu'il faudrait voir les espèces qui se nourrissent ensemble aux mangeoires ou pas

func_test_reseau_asso = function() {
  library(igraph)
  library(combinat)  # Pour les combinaisons
  
  # Étape 1 : filtrer et garder uniquement les parties avec >= 2 espèces
  parties_valides <- dt_birdlab_local %>%
    filter(espece != "Espèce inconnue") %>%
    distinct(partie_id, espece) %>%
    group_by(partie_id) %>%
    filter(n_distinct(espece) >= 2) %>%
    ungroup()
  
  # Étape 2 : créer les paires d'espèces par partie
  df_paires <- parties_valides %>%
    group_by(partie_id) %>%
    summarise(paires = list(combn(espece, 2, simplify = FALSE)), .groups = "drop") %>%
    unnest(paires) %>%
    mutate(pair = map_chr(paires, ~ paste(sort(.x), collapse = "_")))
  
  # Étape 3 : compter les interactions (une interaction par paire par partie)
  df_interact <- df_paires %>%
    distinct(partie_id, pair) %>%   # <--- Évite les doublons dans une même partie
    count(pair) %>%
    separate(pair, into = c("from", "to"), sep = "_")
  
  # Étape 4 : créer le graphe
  g <- graph_from_data_frame(df_interact, directed = FALSE)
  
  # Exemple : df_freq_sp_local contient deux colonnes : espece, freq
  # Assurons-nous que les noms correspondent bien à V(g)$name
  vertex_sizes <- df_freq_sp_local %>%
    filter(espece %in% V(g)$name) %>%  # garder seulement les espèces présentes dans le graphe
    select(espece, freq)
  
  # Créer un vecteur de taille aligné avec l'ordre des sommets
  sizes <- sapply(V(g)$name, function(x) {
    size <- vertex_sizes$freq[vertex_sizes$espece == x]
    if (length(size) == 0) return(5) else return(size)
  })
  
  # Dessiner le graphe avec les bonnes tailles
  plot(g,
       edge.width = 100* E(g)$n / sum(E(g)$n),
       vertex.label.color = "black",
       vertex.size = sizes /5,
       edge.color = "grey40",
       layout=layout.fruchterman.reingold, main="fruchterman.reingold")
  
  mon_p <- recordPlot()
  
  return(mon_p)
  
  
}

func_modularite_interaction = func_modul(){
  
  # Supposons que tu as déjà ton graphe `g` comme dans ton code précédent
  
  # 1. Détecter les communautés (clusters)
  communities <- cluster_walktrap(g)  # tu peux essayer d'autres algos aussi
  
  # 2. Attribuer une couleur par cluster
  V(g)$color <- communities$membership  # cluster id pour chaque nœud
  
  # 3. Convertir les clusters en couleurs réelles (facultatif)
  colors <- rainbow(length(unique(communities$membership)))
  V(g)$color <- colors[communities$membership]
  
  # 4. Plot avec couleurs de clusters
  plot(g,
       edge.width = 100* E(g)$n / sum(E(g)$n),
       vertex.label.color = "black",
       vertex.size = 5,  # comme défini plus tôt
       edge.color = "grey70",
       layout = layout_with_fr)
  # 
    modularity(communities)
   # [1] 0.1202382 --> varie entre 0 et 1 : ici modularité faible, groupe pas très bien définis

  # Vérifier la densité du graphe 
  edge_density(g) #--> interconnexion réseau, ici il vaut 0.46, assez connecté, pas très modulaire
  
  # Analyser les degrés des sommets (espèces très connectées) :
  sort(degree(g), decreasing = TRUE)
  
}


# Complexité (linkage density)







# APPROPRIATION ################################################################

# par mangeoire ----

# matériel : type de mangeoires utilisées

# référentiel & non référentiel : nombre moyen de parties par mangeoire 
# carto avec circle marker proportionnels au nombre de parties par mangeoire

pal <- colorFactor(palette = "viridis", domain = dt_birdlab_local$saison)

df_mangeoire = dt_birdlab_local %>%
  left_join(coord_birdlab_local) %>%
  group_by(mangeoires_nom, geometry) %>%
  summarize(pourc_partie = (n_distinct(partie_id)/n_distinct(dt_birdlab_local$partie_id))*100, 
            nb_partie = n_distinct(partie_id)) %>%
  ungroup() %>% 
  st_as_sf()
  
# carto des mangeoires, taille du cercle en fonction du nombre de parties
carte_collections <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = df_mangeoire, 
                   radius = ~nb_partie, 
                   color = "orange", 
                   fillOpacity = 1, 
                   stroke = F, 
                   popup = ~ paste0("mangeoire : ", mangeoires_nom, " - ", nb_partie, " partie(s)")) %>% 
  addPolygons(data = coord_metro_lyon, fillOpacity = 0) # popup ne fonctionne pas


# participants ----

# nombre total de participants
ntot_participant = length(unique(dt_birdlab_local$user_id))

# turn-over des participants et evolution du nombre de participants

plot_evol_participant = ggplot(df_stat_saison %>% 
                                 filter(!str_detect(saison, "hors-saison")),
                               aes(x = factor(saison), y = nb_participant)) +
  geom_bar(stat = "identity", fill = nb_new_user) +
  labs(y = "nombre de participants", 
       x = "saison birdlab") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))


plot_evol_participant = df_stat_saison %>%
  filter(!str_detect(saison, "hors-saison")) %>%
  mutate(nb_old_user = nb_participant - nb_new_user) %>%
  pivot_longer(cols = c(nb_new_user, nb_old_user),
               names_to = "type_participant",
               values_to = "nombre") %>%
  ggplot(aes(x = factor(saison), y = nombre, fill = type_participant)) +
  geom_bar(stat = "identity") +
  labs(
    y = "nombre de participants",
    x = "saison birdlab",
    fill = "type de participant"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))



# activité des participants --> référentiel : nombre de parties moyen par participant dans le référentiel
# nombre de participants ayant participé une seule fois
participant_1_partie = dt_birdlab_local %>% 
  group_by(user_id) %>%
  summarize(nb_partie = n_distinct(partie_id)) %>%
  ungroup() %>%
  filter(nb_partie == 1) %>%
  pull() %>%
  length()

# nombre de gros participants (< 20 parties)
participant_gros = dt_birdlab_local %>% 
  group_by(user_id) %>%
  summarize(nb_partie = n_distinct(partie_id)) %>%
  ungroup() %>%
  filter(nb_partie > 19) %>%
  pull() %>%
  length()

# nombre de parties moyen par participant localement
participant_moy = dt_birdlab_local %>% 
  group_by(user_id) %>%
  summarize(nb_partie = n_distinct(partie_id)) %>%
  ungroup() %>%
  pull() %>%
  mean()

# nombre de parties moyen par participant dans le référentiel
participant_moy_ref = dt_ref %>% 
  group_by(user_id) %>%
  summarize(nb_partie = n_distinct(partie_id)) %>%
  ungroup() %>%
  pull() %>%
  mean()



# niveau des participants : donut
# /!\ ce diagramme ne distingue pas si un utilisateur a plusieurs niveaux. Ainsi 
# un utilisateur qui a changé de niveau est compté plusieurs fois, une fois pour chacun 
# de ses niveaux passés et actuels
df_niv_participant <- dt_birdlab_local %>%
  select(user_id, user_niveau) %>%
  distinct() %>%
  count(user_niveau, name = "nb_participant") %>%
  mutate(
    rang = case_when(
      user_niveau == "novice"   ~ 1,
      user_niveau == "amateur"  ~ 2,
      user_niveau == "confirmé" ~ 3,
      user_niveau == "expert"   ~ 4
    )
  ) %>%
  arrange(rang) %>%
  mutate(
    fraction = nb_participant / sum(nb_participant),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, -1)),
    label_text = paste0(user_niveau, "\n", round(fraction * 100, 1), "%"),
    label_pos = cumsum(fraction) - fraction / 2
  )

# Graphe donut avec pourcentages + texte
plot_niv_participant = ggplot(df_niv_participant, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = user_niveau)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +  # enlever le quadrillage
  geom_label(
    aes(y = label_pos,
        x = 3.5,
        label = label_text),
    fill = "#f8f8f8", size = 3
  ) +
  theme(legend.position = "none")





# evolution du niveau des participants
func_pourc_chang_niv = function(dt_birdlab){
  df = dt_birdlab %>%
    select(user_id, user_niveau) %>% #, year
    unique() %>%
    group_by(user_id) %>%
    reframe(nb_chang_niv = n_distinct(user_niveau) - 1) %>%
    ungroup() %>%
    group_by(nb_chang_niv) %>%
    reframe(pourc_chang_niv = round(100*n_distinct(user_id)/n_distinct(dt_birdlab$user_id))) %>% 
    ungroup()
  return(df)
}

df_evol_niv_partic_local = func_pourc_chang_niv(dt_birdlab_local) %>%
  rename(pourc_chang_niv_local = pourc_chang_niv)
df_evol_niv_partic_ref = func_pourc_chang_niv(dt_ref) %>%
  rename(pourc_chang_niv_ref = pourc_chang_niv)







# diag radar pourc des participants changeant de niveau
# prend en compte les 2 df précédents 
func_diag_radar_chang_niv = function() {
  df_radar_chang_niv <- df_evol_niv_partic_local %>%
    left_join(df_evol_niv_partic_ref, by = "nb_chang_niv") %>%
    t() %>%
    as.data.frame()
  
  # Utiliser la première ligne comme noms de colonnes
  colnames(df_radar_chang_niv) <- as.character(df_radar_chang_niv[1, ])
  df_radar_chang_niv <- df_radar_chang_niv[-1, ]
  
  # Convertir en numérique (chaque colonne) (l'utilisation de t() a tout passé en character)
  df_radar_chang_niv[] <- lapply(df_radar_chang_niv, as.numeric)
  
  # Ajouter les lignes min/max requises pour le diagramme radar
  # doivent être les 2 premières lignes du df utilisé pour le diag radar
  df_radar_chang_niv <- rbind(
    rep(100, ncol(df_radar_chang_niv)),
    rep(0, ncol(df_radar_chang_niv)),
    df_radar_chang_niv
  )
  
  # Optionnel : donner des noms aux lignes pour la légende
  rownames(df_radar_chang_niv) <- c("Max", "Min", "Local", "Référence")
  
  
  radarchart(
    df_radar_chang_niv,
    pcol = c("#B0986CFF", "#009474FF"),
    pfcol = c(rgb(176/255, 152/255, 108/255, alpha = 0.3)  # couleur 1 avec transparence
              # , rgb(0/255, 148/255, 116/255, alpha = 0.3)     # couleur 2 avec transparence
    ),
    plwd = 4 # épaisseur de la ligne
  )
  
  legend(
    "topright",
    legend = c("Local", "Référence"),
    col = c("#B0986CFF", "#009474FF"),
    lty = 1,
    bty = "n"
  )
  
  # Ajouter un titre
  title("Pourcentages de participants dont le niveau \n évolue 0, 1, 2 ou 3 fois localement, \n mis en regard avec les données du référentiel", cex.main = 1) 
  
  mon_graphe <- recordPlot()
  
  return(mon_graphe)
  
}

plot_radar_chang_niv = func_diag_radar_chang_niv()








# nombre d'espèces observées par parties en fonction du niveau
plot_sp_niv_participant <- dt_birdlab_local %>%
  filter(espece != "Espèce inconnue" ) %>%
  mutate(
    rang = case_when(
      user_niveau == "novice"   ~ 1,
      user_niveau == "amateur"  ~ 2,
      user_niveau == "confirmé" ~ 3,
      user_niveau == "expert"   ~ 4
    )
  ) %>%
  select(user_id, user_niveau, partie_id, espece, rang) %>%
  distinct() %>%
  group_by(user_id, user_niveau, partie_id, rang) %>%
  summarize(nb_sp = n_distinct(espece)) %>%
  ungroup() %>%
  arrange(rang) %>% # range permet d'arranger l'ordre des catégories de niveau dans le graphe final
  ggplot(aes(x = reorder(user_niveau, rang), y = nb_sp))+
  geom_boxplot(aes(fill = user_niveau)) +
  theme_minimal() +
  theme(legend.position = "none")
  
# par parties ----

# nombre total de parties jouées 
ntot_partie = length(unique(dt_birdlab_local$partie_id))


# richesse spécifique en fonction du nombre de parties, avec la ligne de jauge

#df qui nous donne pour chaque date le nombre de parties cumulées à ce jour localement
df_nb_partie_cum = dt_birdlab_local %>%
  select(date, partie_id) %>%
  unique() %>%
  arrange(date) %>%
  mutate(nb_part = row_number()) %>%
  select(-partie_id) %>%
  group_by(date) %>%
  summarize(nb_partie = max(nb_part)) %>%
  ungroup()

# plot_richesse_cum_partie <- 
dt_birdlab_local %>%
  # calcul de la première date d'observation de l'espèce
  filter(espece != "Espèce inconnue") %>%
  select(partie_id, espece, date) %>%
  distinct() %>%
  group_by(espece) %>%
  summarize(first_obs = min(date)) %>% 
  ungroup() %>%
  rename(date = first_obs) %>%
  # jointure avec la table liant date et nombre cumulé de parties jouées 
  left_join(df_nb_partie_cum) %>% 
  # calcul de la somme cumulée du nombre d'espèces en fonction du nombre de parties 
  # jouées, en conaissant les first_obs des espèces
  group_by(nb_partie) %>%
  summarize(nb_sp = n_distinct(espece)) %>%
  ungroup() %>%
  mutate(richesse_cum = cumsum(nb_sp)) %>%  
  # graphe
  ggplot(aes(x = nb_partie, y = richesse_cum, group = 1)) +
  geom_line(linewidth = 0.8) +
  labs(x = "nombre de parties", 
       y = "richesse en espèces cumulée") +
  # ligne du nombre d'espèces de la liste
  geom_hline(yintercept = 33, linetype = "dashed", color = "purple", linewidth = 0.8) +
  geom_hline(yintercept = 16, linetype = "dashed", color = "lightblue", linewidth = 0.8) +
  annotate("text", x = 2, y = 16, label = "50% des espèces de\nla liste observées", hjust = 0, size = 3, color = "lightblue") +
  theme_minimal()


# phénologie de participation à la dernière saison
plot_pheno_partic_saison =  dt_birdlab_local %>%
  filter(saison == saison_en_cours_ou_derniere) %>%
  select(date, partie_id, month) %>%
  unique() %>%
  mutate(month = factor(
      month,
      levels = c(11, 12, 1, 2, 3),
      labels = c("Nov", "Déc", "Jan", "Fév", "Mar")
    )) %>%
  count(month) %>%
  complete(month = factor(c("Nov", "Déc", "Jan", "Fév", "Mar"),
                          levels = c("Nov", "Déc", "Jan", "Fév", "Mar")),
           fill = list(n = 0)) %>%
  ggplot(aes(x = month, y = n)) +
  geom_col(fill = "steelblue", width = 0.4) +
    labs(x = "Mois",
         y = "Nombre de parties",
         title = paste0("Nombre de parties au cours de \n la saison ", saison_en_cours_ou_derniere, " (novembre à mars)" )) +
    theme_minimal()

# evolution du nombre de parties au cours du temps (années)
plot_evol_participation =  dt_birdlab_local %>%
  select(date, partie_id, year) %>%
  unique() %>%
  count(year) %>%
  complete(year = c(unique(min(dt_birdlab_local$year) : unique(max(dt_birdlab_local$year)))),
           fill = list(n = 0)) %>% # si jamais il y a des années sans participation, permet de les faire apparaître quand même
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "#FFAD0AFF", width = 0.4) +
  labs(x = "année",
       y = "Nombre de parties",
       title = paste0("Evolution du nombre de parties" )) +
  theme_minimal()




