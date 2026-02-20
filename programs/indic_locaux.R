# Indicateurs locaux

# A faire : ----
#   
#   Dans le référentiel –> au bout de combien de parties en moyenne atteint-on un plateau ?
#   
#   réseau d'association : espèces présentes simultanément aux mangeoires ? à la même mangeoire ?
# 
# phénologie des espèces au cours de la saison ? au cours de la journée en combinant les observations ?
# 
# remplacer donut niveau par diagramme radar des répartitions des participants par niveaux par rapport à un référentiel
# 
# Référentiels :
# 
# sortir la restit pour plaine commune, avec en référentiel département seine-saint-denis ou idf ?

##################################################################################














  

# nombre total de parties jouées 
ntot_partie = n_distinct(dt_birdlab_local$partie_id)
nb_parties_ref = n_distinct(dt_ref$partie_id)


poids_ds_ref = round(100 * ntot_partie / nb_parties_ref)

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
          nb_participant = n_distinct(user_id),
          nb_mangeoire = n_distinct(mangeoires_nom, mangeoires_type, mangeoires_code_postal)) %>%
  left_join(df_turnover_participants)


# dernière saison finie, indépendant de s'il y a eu de la participation sur le site ou pas
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
# plot_evol_richesse = ggplot(df_stat_saison, aes(x = factor(saison), y = nb_espece)) +
#   geom_point() +
#   labs(x = "richesse spécifique", 
#        y = "saison birdlab") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 25, hjust = 1))

# nombre d'espèces observées sur la dernière saison / jauge nombre d'espèces de la liste
jauge_dernière_saison = gauge(value = df_stat_saison$nb_espece[df_stat_saison$saison == der_saison], 
      min = 0, 
      max = 30, 
      label = paste0("saison ", der_saison))


# nombre d'espèces observées au total vs jauge--> fait, rmd
# hors saison compris
jauge_total = gauge(value = n_distinct(dt_birdlab_local$espece[dt_birdlab_local$espece != "Espèce inconnue"]), 
      min = 0, 
      max = 30, 
      label = paste0("relevés de ", min(dt_birdlab_local$year), " à ", max(dt_birdlab_local$year), "\n hors saison compris"))


nb_sp_tot_local = length(unique(dt_birdlab_local$espece[dt_birdlab_local$espece != "Espèce inconnue"]))

nb_sp_tot_ref = length(unique(dt_ref$espece[dt_ref$espece != "Espèce inconnue"]))
  


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


# nombre d'occurences des espèces : nombre de parties où elles ont été observées
func_nb_occu_sp = function(dt_birdlab) {
  dt = dt_birdlab %>%
    filter(espece != "Espèce inconnue") %>%
    select(partie_id, espece) %>%
    unique() %>%
    group_by(espece) %>%
    reframe(nb_occu = n_distinct(partie_id)) %>%
    ungroup()
  
  return(dt)
}

 
  

# rerentiel : a modif

df_freq_sp_ref = dt_ref %>%
  filter(valide == "saison", espece != "Espèce inconnue" ) %>%
  group_by(espece) %>%
  summarize(freq = (n_distinct(partie_id)/ n_distinct(dt_ref$partie_id)) * 100) %>%
  ungroup() %>%
  arrange(desc(freq))

plot_freq_ref = ggplot(df_freq_sp_ref, aes(y = freq, x = reorder(espece, freq))) +  
  geom_col(width = 0.3, color = "lightblue", fill = "lightblue") +
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


# ref et local combiné sur un meme graphe

func_plot_freq_combi = function() {
  df_freq_combi = df_freq_sp_local %>%
    mutate(echelle = "local") %>%
    rbind(df_freq_sp_ref %>% mutate(echelle = "ref"))
  
  
  # 1. Extraire la fréquence locale par espèce
  ordre_local <- df_freq_combi |>
    filter(echelle == "local") |>
    mutate(ordre = row_number()) %>%
    select(espece, ordre)
  
  # 2. Joindre et trier les espèces par freq locale décroissante
  df_freq_combi <- df_freq_combi |>
    left_join(ordre_local) |>
    mutate(ordre = replace_na(ordre, 100))
  
  
  # 3. Tracer le graphique avec position_dodge2
  plot_freq_combi <- ggplot(df_freq_combi,  aes(x = reorder(espece, -ordre), y = freq, fill = echelle)) +
    geom_col( position = position_dodge2(width = 0.6, preserve = "single"), width = 0.6) +
    labs(title = paste0("Fréquence des espèces\n(référentiel (", nb_parties_ref, " parties) vs local (", ntot_partie, " parties)"),
         x = "Espèce",
         y = "Fréquence d'observation (%)",
         fill = "Échelle") +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)) +
    scale_fill_manual(values = c("ref" = "lightgrey", "local" = "steelblue"))
  
  return(plot_freq_combi)
}

# plot_freq_combi = func_plot_freq_combi()


# temps moyen passé à la mangeoire ----

# Cette fonction ne considère que le départ et l'arrivée des individus sur les
# mangeoires. Un changement de mangeoire est donc invisibilisé, la durée considérée 
# est seulement celle passée sur les mangeoires, peu importe s'il y a des changements

# colonne action : arrivée sur la mangeoire droite; arrivée sur la mangeoire gauche; départ de l'oiseau ; 
# passage de la droite vers la gauche ; passage de la mangeoire gauche vers la droite 



func_duree_mangeoire = function(dt_birdlab, nb_occu_min, titre = NULL) {
  # on fait une moyenne sur les espèces qui ont été observées au moins dans 10 collections
  dt = func_nb_occu_sp(dt_birdlab) %>% 
    filter(nb_occu >= nb_occu_min)
  
  p = dt_birdlab %>%
    filter(espece %in% dt$espece) %>%
    filter(espece != "Espèce inconnue") %>%
    select(partie_id, numero_individu, espece, seconde, action) %>%
    unique() %>%
    group_by(partie_id, numero_individu, espece) %>%
    reframe(duree = max(seconde) - min(seconde)) %>%
    ungroup() %>% 
    group_by(espece) %>%
    reframe(duree_moy = mean(duree)) %>%
    ungroup() %>%
    ggplot(aes(x = reorder(espece, duree_moy), y = duree_moy)) +
    geom_col(width = 0.5, fill = "#FED789FF")+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(x = "espèces",
         y = "durée moyenne à la mangeoire (s)",
         title = titre)
  
  
  
  return(p)
}


func_duree_mangeoire_combi = function(dt_local, dt_reference, nb_occu_min, titre = NULL) {
  # on fait une moyenne sur les espèces qui ont été observées au moins dans 10 collections
  dt1 = dt_local %>%
    mutate(echelle = "local")
  
  dt2 = dt_reference %>%
    mutate(echelle = "reference") 
  
  # calcul des nombres d'occurences par espece, pour pouvoir calculer un temps moyen par la suite
  dt_nb_occu = rbind(dt1, dt2) %>%
    filter(espece != "Espèce inconnue") %>%
    select(partie_id, espece, echelle) %>%
    unique() %>%
    group_by(espece, echelle) %>%
    reframe(nb_occu = n_distinct(partie_id)) %>%
    ungroup() %>%
    filter(nb_occu >= nb_occu_min) 
  
  # calcul durée moyenne
  p = rbind(dt1, dt2) %>%
    filter(espece %in% dt1$espece) %>%
    filter(espece != "Espèce inconnue") %>%
    select(partie_id, numero_individu, espece, seconde, action, echelle) %>%
    unique() %>%
    group_by(partie_id, numero_individu, espece, echelle) %>%
    reframe(duree = max(seconde) - min(seconde)) %>%
    ungroup() %>% 
    group_by(espece, echelle) %>%
    reframe(duree_moy = mean(duree)) %>%
    ungroup() %>%
    mutate(ordre = case_when(echelle == "local"~ duree_moy, 
                             TRUE ~ 0)) %>%
    # graphe
    ggplot( aes(x = reorder(espece, ordre), y = duree_moy)) +
    geom_bar(aes(fill = echelle), stat = "identity", position = "dodge", width = 0.4) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(x = "espèces",
         y = "durée moyenne à la mangeoire (s)",
         title = titre) +
      scale_fill_manual(values = c( "#EE6100FF", "#1BB6AFFF"))
  
  
  
  return(p)
}



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




func_modularite_interaction = function(){
  
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

# phenologie ----
# phenologie des especes au cours de la journée

# phenologie des especes au cours de la saison --> pas trop faisable


# activité ----
df_activite_local = dt_birdlab_local %>%
  group_by(partie_id) %>%
  mutate(nb_contact = n_distinct(numero_individu)) %>% 
  ungroup() %>%
  group_by(partie_id, espece) %>%
  mutate(nb_contact_sp_partie = n_distinct(numero_individu)) %>% 
  ungroup() %>%
  select(partie_id, espece, nb_contact, nb_contact_sp_partie, mangeoires_type) %>% 
  unique() %>% 
  mutate(echelle = "local")

df_activite_ref = dt_ref %>%
  group_by(partie_id) %>%
  mutate(nb_contact = n_distinct(numero_individu)) %>% 
  ungroup() %>%
  group_by(partie_id, espece) %>%
  mutate(nb_contact_sp_partie = n_distinct(numero_individu)) %>% 
  ungroup() %>%
  select(partie_id, espece, nb_contact, nb_contact_sp_partie, mangeoires_type) %>% 
  unique()%>% 
  mutate(echelle = "ref")


# ACTIVITE PAR PARTIE
# val numeriques
acti_partie_local = df_activite_local %>%
  select(partie_id, nb_contact) %>%
  unique() %>%
  mutate(moy = mean(nb_contact)) %>%
  pull(moy) %>%
  unique() %>%
  round()

acti_partie_ref = df_activite_ref %>%
  select(partie_id, nb_contact) %>%
  unique() %>%
  mutate(moy = mean(nb_contact)) %>%
  pull(moy) %>%
  unique() %>%
  round()


# graphe
dt_nb_sess = df_activite_local %>%
  bind_rows(df_activite_ref) %>%
  select(-espece, -mangeoires_type, -nb_contact_sp_partie) %>%
  unique() %>%
  add_count(echelle, name = "nb_partie")

plot_activite_partie = ggplot(dt_nb_sess, aes(x = echelle, fill = echelle)) +
  geom_boxplot(aes(y = nb_contact), width = 0.1)+
  geom_text(aes(y = 0, label = paste0(nb_partie, " partie(s)")), size = 3)  +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("nombre d'individus observés sur la mangeoire par partie")



# PAR TYPE DE MANGEOIRES
# nb indiv pour mangeoire suspendue, données locales
acti_partie_loc_susp = df_activite_local %>%
  filter(mangeoires_type =="suspendu") %>%
  select(partie_id, nb_contact) %>%
  unique() %>%
  mutate(moy = mean(nb_contact)) %>%
  pull(moy) %>%
  unique() %>%
  round()

# nb indiv pour mangeoire plateau, données locales
acti_partie_loc_plato = df_activite_local %>%
  filter(mangeoires_type =="plateau") %>%
  select(partie_id, nb_contact) %>%
  unique() %>%
  mutate(moy = mean(nb_contact)) %>%
  pull(moy) %>%
  unique() %>%
  round()

dt1 = df_activite_local %>%
  select(partie_id, mangeoires_type) %>% 
  unique() %>%
  group_by(mangeoires_type) %>%
  mutate(nb_partie = n_distinct(partie_id)) %>%
  ungroup()

plot_acti_loc_comparaison_mangeoire = df_activite_local %>%
  select(partie_id, nb_contact, mangeoires_type) %>%
  unique() %>%
  left_join(dt1) %>%
  na.omit() %>%
  ggplot(aes(x = mangeoires_type, y = nb_contact, fill = mangeoires_type)) +
  geom_text(aes(y = 0, label = paste0(nb_partie, " partie(s)")), size = 3)  +
  geom_boxplot(width = 0.1) +
  theme_minimal() +
  geom_jitter() +
  theme(legend.position = "none") +
  ylab("nombre d'individus observés")+
  xlab("type de mangeoire")

# nb indiv pour mangeoire suspendue, données de ref
acti_partie_ref_susp = df_activite_ref %>%
  filter(mangeoires_type =="suspendu") %>%
  select(partie_id, nb_contact) %>%
  unique() %>%
  mutate(moy = mean(nb_contact)) %>%
  pull(moy) %>%
  unique() %>%
  round()

# nb indiv pour mangeoire plateau, données de ref
acti_partie_ref_plato = df_activite_ref %>%
  filter(mangeoires_type =="plateau") %>%
  select(partie_id, nb_contact) %>%
  unique() %>%
  mutate(moy = mean(nb_contact)) %>%
  pull(moy) %>%
  unique() %>%
  round()

# 
# plot_acti_ref_comparaison_mangeoire = df_activite_ref %>%
#   select(partie_id, nb_contact, mangeoires_type) %>%
#   unique() %>%
#   ggplot(aes(x = mangeoires_type, y = nb_contact)) +
#   geom_boxplot(width = 0.4) +
#   theme_minimal()




# PAR ESPECE
# faire un ordre pour le graphe
dt_tri = df_activite_local %>%
  filter(echelle == "local") %>%
  group_by(espece) %>%
  mutate(tri =  median(nb_contact_sp_partie)) %>%
  select(espece, tri) %>%
  unique()

plot_sp_acti = 
  df_activite_local %>%
  bind_rows(df_activite_ref) %>%
  left_join(dt_tri) %>%
  filter(espece %in% unique(df_activite_local$espece)) %>%
  filter(espece != "Espèce inconnue") %>%
  select(-nb_contact) %>%
  ggplot(aes(x = reorder(espece, tri), y = nb_contact_sp_partie, fill = echelle)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  ylab("nombre d'individus de l'espèce observés en une partie") +
  xlab("espèces") 
  



# APPROPRIATION ################################################################

  

# par mangeoire ----

# matériel : type de mangeoires utilisées
type_mangeoire = unique(dt_birdlab_local$mangeoires_type[!is.na(dt_birdlab_local$mangeoires_type )])


# référentiel & non référentiel : nombre moyen de parties par mangeoire 


nb_mangeoire_local = n_distinct(dt_birdlab_local$mangeoires_nom, dt_birdlab_local$mangeoires_type, dt_birdlab_local$mangeoires_code_postal)


# participants ----

# nombre total de participants
ntot_participant = length(unique(dt_birdlab_local$user_id))

# turn-over des participants et evolution du nombre de participants
plot_evol_participant = df_stat_saison %>%
  filter(!str_detect(saison, "hors-saison")) %>%
  mutate(nb_old_user = nb_participant - nb_new_user) %>%
  pivot_longer(cols = c(nb_new_user, nb_old_user),
               names_to = "type_participant",
               values_to = "nombre") %>%
  ggplot(aes(x = factor(saison), y = nombre, fill = type_participant)) +
  geom_bar(stat = "identity") +
  labs(y = "nombre de participants",
       x = "saison birdlab",
       fill = "type de participant") +
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


# activité des participants 
# 
dt_activite = dt_birdlab_local %>%
    group_by(user_id) %>%
    reframe(nb_partie = n_distinct(partie_id)) %>%
    ungroup() 

plot_activite =
  ggplot(dt_activite, aes(x = nb_partie))+
    geom_bar(fill = "#5E9432FF", binwidth = 0.7, color = "white") +
    theme_minimal() +
    labs(x = "Nombre de parties réalisées",
         y = "Nombre de participants") +
    scale_y_continuous(breaks = seq(0, 10000, 1)) +
    scale_x_continuous(breaks = seq(0, max(dt_activite$nb_partie), 1))





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
    axistype = 1,  # Type 1 : graduations standard + axes
    caxislabels = seq(0, 100, 20),  # Valeurs sur les axes circulaires
    axislabcol = "black",
    pcol = c("#B0986CFF", "#009474FF"),
    pfcol = c(rgb(176 / 255, 152 / 255, 108 / 255, alpha = 0.3)),
    plwd = 4
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

# plot_radar_chang_niv = func_diag_radar_chang_niv()








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



# richesse spécifique en fonction du nombre de parties, avec la ligne de jauge
func_richesse_cumul = function(dt_birdlab) {
  #df qui nous donne pour chaque date le nombre de parties cumulées à ce jour localement
  df_nb_partie_cum = dt_birdlab %>%
    select(date, partie_id) %>%
    unique() %>%
    arrange(date) %>%
    mutate(nb_part = row_number()) %>%
    select(-partie_id) %>%
    group_by(date) %>%
    summarize(nb_partie = max(nb_part)) %>%
    ungroup()
  
  plot_richesse_cum_partie <- dt_birdlab %>%
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
    geom_hline(yintercept = 30, linetype = "dashed", color = "purple", linewidth = 0.8) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "lightblue", linewidth = 0.8) +
    annotate("text", x = 2, y = 15, label = "50% des espèces de\nla liste observées", hjust = 0, size = 3, color = "lightblue") +
    theme_minimal()
  
  return(plot_richesse_cum_partie)
  
  
}





# phénologie de participation à la dernière saison
plot_pheno_partic_saison = dt_birdlab_local %>%
  filter(saison == der_saison) %>%
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
         title = paste0("Nombre de parties au cours de \n la saison ", der_saison, " (novembre à mars)" )) +
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
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(dt_birdlab_local$year), max(dt_birdlab_local$year), 1))


# CARTE ----
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
carte_collections <- leaflet(df_mangeoire) %>% 
  addTiles() %>% 
  addPolygons(data = coord_plaineco, fillOpacity = 0) %>% # popup ne fonctionne pas
  addCircleMarkers(radius = 10, 
                   color = "orange", 
                   fillOpacity = 1, 
                   stroke = F, 
                   label = ~ paste0("mangeoire : ", mangeoires_nom, " - ", nb_partie, " partie(s)")) 


