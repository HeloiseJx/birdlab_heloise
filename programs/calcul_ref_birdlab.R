###                   ###
# REFERENTIELS BIRDLAB
###                   ###

# IMPORT DATA ----




### df recapitulatif des mangeoires et niveau des participants par session ----
df_specifiq_birdlab = dt_birdlab_tot %>%
  select(partie_id, mangeoires_type, user_niveau) %>%
  unique() 



# REFERENTIELS ----

## SESSION ----
# version taux urbanisation
#' Title
#'
#' @param taux_urba taux d'urbanisation du site étudié
#'
#' @returns le df contenant le référentiel choisi pour chaque session
#' @export
#'
#' @examples
func_choix_ref_tauxurba = function(taux_urba) { 
  
  df_merge = dt_birdlab_tot %>%
    select(partie_id) %>%
    left_join(dt_protoc_oso) %>%
    left_join(df_specifiq_birdlab) %>%
    select(partie_id, im, mangeoires_type, user_niveau) %>%
    unique() %>%
    
  # Ajout des différents taux d'urbanisation
    mutate(U10 = case_when(im >=  taux_urba -0.1 & im <=  taux_urba +0.1 ~ "U10", TRUE ~ NA_character_),
           U15 = case_when(im >=  taux_urba -0.15 & im <=  taux_urba +0.15 ~ "U15", TRUE ~ NA_character_),
           U20 = case_when(im >=  taux_urba -0.2 & im <=  taux_urba +0.2 ~ "U20", TRUE ~ NA_character_),
           U25 = case_when(im >=  taux_urba -0.25 & im <=  taux_urba +0.25 ~ "U25", TRUE ~ NA_character_)
           )
  
  
  # 0_ construction des combinaisons : M * N * U  
  # Construction de TOUS LES REFERENTIELS POSSIBLES !!! 
  # Liste des référentiels possibles, sachant que NA est possible pour M et CS : 
  # N, M, CS x N, CS x M, M x N, M x CS x N
  df_combi = df_merge %>%
    mutate(
      combi_M = paste0("10", mangeoires_type),
      combi_N = paste0("10", user_niveau),
      combi_M_N = paste0("9", mangeoires_type,"---", user_niveau),
      combi_N_U10 = paste0("5", user_niveau,"---", U10),
      combi_N_U15 = paste0("6", user_niveau,"---", U15),
      combi_N_U20 = paste0("7", user_niveau,"---", U20),
      combi_N_U25 = paste0("8", user_niveau,"---", U25),
      combi_M_U10 = paste0("5", mangeoires_type,"---", U10),
      combi_M_U15 = paste0("6", mangeoires_type,"---", U15),
      combi_M_U20 = paste0("7", mangeoires_type,"---", U20),
      combi_M_U25 = paste0("8", mangeoires_type,"---", U25),
      combi_M_N_U10 = paste0("1", user_niveau, "---", mangeoires_type,"---", U10),
      combi_M_N_U15 = paste0("2", user_niveau, "---", mangeoires_type,"---", U15),
      combi_M_N_U20 = paste0("3", user_niveau, "---", mangeoires_type,"---", U20),
      combi_M_N_U25 = paste0("4", user_niveau, "---", mangeoires_type,"---", U25)) %>%
    rename(M = mangeoires_type, N = user_niveau) 
  
  # 1 calcul du nombre de sessions POUR TOUS LES REFERENTIELS POSSIBLES !!!!!!!
  # N, M, U x N, U x M, M x N, M x U x N
  df_ref_M  = df_combi %>%
    count(combi_M) %>%
    mutate(ref_lies = "M") %>%
    rename(combi = combi_M)
  
  df_ref_N  = df_combi %>%
    count(combi_N) %>%
    mutate(ref_lies = "N")  %>%
    rename(combi = combi_N)
  
  df_ref_M_N  = df_combi %>%
    count(combi_M_N) %>%
    mutate(ref_lies = "M_N")  %>%
    rename(combi = combi_M_N)
  
  df_ref_N_U10  = df_combi %>%
    count(combi_N_U10) %>%
    mutate(ref_lies = "N_U10")  %>%
    rename(combi = combi_N_U10)
  
  df_ref_N_U15  = df_combi %>%
    count(combi_N_U15) %>%
    mutate(ref_lies = "N_U15") %>%
    rename(combi = combi_N_U15)
  
  df_ref_N_U20  = df_combi %>%
    count(combi_N_U20) %>%
    mutate(ref_lies = "N_U20") %>%
    rename(combi = combi_N_U20)
  
  df_ref_N_U25  = df_combi %>%
    count(combi_N_U25) %>%
    mutate(ref_lies = "N_U25") %>%
    rename(combi = combi_N_U25)
  
  df_ref_M_U10  = df_combi %>%
    count(combi_M_U10) %>%
    mutate(ref_lies = "M_U10")  %>%
    rename(combi = combi_M_U10)
  
  df_ref_M_U15  = df_combi %>%
    count(combi_M_U15) %>%
    mutate(ref_lies = "M_U15") %>%
    rename(combi = combi_M_U15)
  
  df_ref_M_U20  = df_combi %>%
    count(combi_M_U20) %>%
    mutate(ref_lies = "M_U20") %>%
    rename(combi = combi_M_U20)
  
  df_ref_M_U25  = df_combi %>%
    count(combi_M_U25) %>%
    mutate(ref_lies = "M_U25") %>%
    rename(combi = combi_M_U25)
  
  df_ref_N_M_U10  = df_combi %>%
    count(combi_M_N_U10) %>%
    mutate(ref_lies = "M_N_U10") %>%
    rename(combi = combi_M_N_U10)
  
  df_ref_N_M_U15  = df_combi %>%
    count(combi_M_N_U15) %>%
    mutate(ref_lies = "M_N_U15") %>%
    rename(combi = combi_M_N_U15)
  
  df_ref_N_M_U20  = df_combi %>%
    count(combi_M_N_U20) %>%
    mutate(ref_lies = "M_N_U20") %>%
    rename(combi = combi_M_N_U20)
  
  df_ref_N_M_U25  = df_combi %>%
    count(combi_M_N_U25) %>%
    mutate(ref_lies = "M_N_U25") %>%
    rename(combi = combi_M_N_U25)
  
  
  # 2_ associations des différents référentiels possibles à chaque session ainsi que la taille totale du référentiel
  df_merge_ref_partie_id =  df_ref_M %>%
    bind_rows(df_ref_N) %>%
    bind_rows(df_ref_M_N) %>%
    bind_rows(df_ref_N_U10) %>%
    bind_rows(df_ref_N_U15) %>%
    bind_rows(df_ref_N_U20) %>%
    bind_rows(df_ref_N_U25) %>%
    bind_rows(df_ref_M_U10) %>%
    bind_rows(df_ref_M_U15) %>%
    bind_rows(df_ref_M_U20) %>%
    bind_rows(df_ref_M_U25) %>%
    bind_rows(df_ref_N_M_U10) %>%
    bind_rows(df_ref_N_M_U15) %>%
    bind_rows(df_ref_N_M_U20) %>%
    bind_rows(df_ref_N_M_U25) 
  
  # 3_ Choix du référentiel pour chaque session en fonction de la taille des différents référentiels et du rang taxonomique retenu pour le taxon
  
  
  df_ref_choisi <- df_combi %>%
    pivot_longer(cols = starts_with("combi_"),
                 values_to = "combi") %>%
    left_join(df_merge_ref_partie_id) %>%
    rename(n_ref_lies = n) %>%
    mutate(finesse_ref = str_extract(combi, "\\d+")) %>%
    mutate(combi = str_remove(combi, "\\d+")) %>%
    # on retire les référentiels avec des NA
    filter(!str_detect(combi, "NA")) %>%
    # choix ref par partie
    group_by(partie_id) %>%
    mutate(ref_choisi = combi[which.min(finesse_ref)], 
           n_ref_choisi = n_ref_lies[which.min(finesse_ref)]) %>%
    # distinction finesse ref_lies et finesse ref_choisi
    mutate(finesse_ref_choisi = finesse_ref[ref_choisi == combi]) %>%
    ungroup() %>%
    # sel colonnes finales
    select(partie_id, ref_choisi, n_ref_choisi, combi, n_ref_lies, finesse_ref_choisi, finesse_ref) %>%
    rename(ref_lies = combi, 
           finesse_ref_lies = finesse_ref)
    

  
  return(df_ref_choisi)
}





df_ref_choisi_VA = func_choix_ref_tauxurba(0.8)
df_ref_choisi_VM = func_choix_ref_tauxurba(0.6)




# version paysage local
func_choix_ref = function() {

df_merge = dt_birdlab_tot %>%
  select(partie_id) %>%
  left_join(dt_protoc_oso) %>%
  left_join(df_specifiq_birdlab) %>%
  select(partie_id, paysage_local, mangeoires_type, user_niveau) %>%
  unique()


# 0_ construction des combinaisons : M * N * CS
# Construction de TOUS LES REFERENTIELS POSSIBLES !!!
# Liste des référentiels possibles, sachant que NA est possible pour M et CS :
# N, M, CS x N, CS x M, M x N, M x CS x N
df_combi = df_merge %>%
  mutate(
    combi_CS_N = paste0(paysage_local,"---", user_niveau),
    combi_CS_M = paste0(paysage_local,"---", mangeoires_type),
    combi_M_N = paste0(mangeoires_type,"---", user_niveau),
    combi_CS_M_N = paste0(paysage_local,"---", mangeoires_type,"---", user_niveau)) %>%
  rename(M = mangeoires_type, N = user_niveau, CS = paysage_local)

# 1 calcul du nombre de sessions POUR TOUS LES REFERENTIELS POSSIBLES !!!!!!!
# N, M, CS x N, CS x M, M x N, M x CS x N
df_ref_CS_M_N  = df_combi %>%
  count(combi_CS_M_N) %>%
  rename(n_CS_M_N = n)

df_ref_CS_M  = df_combi %>%
  count(combi_CS_M) %>%
  rename(n_CS_M = n)

df_ref_CS_N  = df_combi %>%
  count(combi_CS_N) %>%
  rename(n_CS_N = n)

df_ref_M_N  = df_combi %>%
  count(combi_M_N) %>%
  rename(n_M_N = n)

df_ref_N  = df_combi %>%
  count(N) %>%
  rename(n_N = n)

df_ref_M  = df_combi %>%
  count(M) %>%
  rename(n_M = n)



# 2_ associations des différents référentiels possibles à chaque session ainsi que la taille totale du référentiel
df_merge_ref_partie_id = df_combi %>%
  left_join(df_ref_CS_M_N) %>%
  left_join(df_ref_CS_M) %>%
  left_join(df_ref_CS_N) %>%
  left_join(df_ref_M_N) %>%
  left_join(df_ref_N) %>%
  left_join(df_ref_M)

# 3_ Choix du référentiel pour chaque session en fonction de la taille des différents référentiels et du rang taxonomique retenu pour le taxon


  df_ref_choisi <- df_merge_ref_partie_id %>%
    mutate(
      ref_choisi = case_when(

        is.na(M) & is.na(CS) ~ N,
        is.na(M) & !is.na(CS) ~ combi_CS_N,
        !is.na(M) & is.na(CS) & n_M_N > 49 ~ combi_M_N,
        !is.na(M) & !is.na(CS) & n_CS_M_N > 49 ~ combi_CS_M_N,
        !is.na(M) & !is.na(CS) & n_CS_M_N <= 49 & n_M_N > 49 ~ combi_M_N,
        TRUE ~ NA_character_
      ),

      n_ref_choisi = case_when(
        is.na(M) & is.na(CS) ~ n_N,
        is.na(M) & !is.na(CS) ~ n_CS_N,
        !is.na(M) & is.na(CS) & n_M_N > 49 ~ n_M_N,
        !is.na(M) & !is.na(CS) & n_CS_M_N > 49 ~ n_CS_M_N,
        !is.na(M) & !is.na(CS) & n_CS_M_N <= 49 & n_M_N > 49 ~ n_M_N,
        TRUE ~ NA)
    ) %>%
    select(ref_choisi, n_ref_choisi, everything())

  return(df_ref_choisi)
}





df_ref_choisi = func_choix_ref()


























## PARTICIPANT ----
## BUFFER
# func_stat_participants = function(dt_protocole, dt_protocole_tot){
#   
#   # récupération des mangeoires associés aux participants pour pouvoir avoir l'info de cb de mangeoires sont étudiées par participant, ainsi que le nombre de sessions par mangeoires --> pb : mangeoire un peu bancal, à considérer ?
#   df_continu = dt_protocole_tot %>%
#     mutate(an = format(as.Date(date), "%Y")) %>%
#     select(user_id, user_nom, mangeoires_nom, partie_id, an) %>%
#     unique() %>% 
#     group_by(user_id, user_nom, mangeoires_nom, an) %>%
#     reframe(nb_passage = n_distinct(partie_id)) %>%
#     ungroup() %>%
#     # filter(nb_passage >= 3 & nb_passage < 15) %>% #(filtre grossier haut pour enlever les anomalies)
#     group_by(user_id, user_nom, mangeoires_nom) %>%
#     reframe(nb_an_suivi = n_distinct(an)) %>%
#     ungroup() %>%
#     # left_join(dt_protoc_tot) %>%
#     # select(participant_id, site_nom, nb_an_suivi, partie_id) %>%
#     unique()
#   
#   
#   
#   # calcul d'une coordonnée par participant
#   df_oso = dt_protoc_oso %>%
#     as.data.frame() %>%
#     select(partie_id, user_id, user_pseudo, paysage_local) %>%
#     unique() 
#   
#   dt_centroide_participant = dt_protocole %>%
#     group_by(user_id, user_pseudo) %>%
#     summarize(centroide = st_centroid(st_union(geometry))) %>%  # calcule le centroïde de cette géométrie)
#     ungroup() #%>%
#   # left_join(df_oso) 
#   
#   
#   
#   
#   
#   
#   # BOUCLE : calcul des participants dans un buffer de 200 km  
#   dt_stat_struct_buffer = data.frame()
#   
#   for (participant in unique(dt_centroide_participant$user_id)) {
#     
#     zone_buffer <- dt_centroide_participant %>%
#       filter(user_id == participant) %>%
#       pull(centroide) %>%
#       st_buffer(dist = 200000) # fabrique un cercle de 200 km de rayon autour du point
#     
#     # je récupère une coordonnée par participant = centroide des points
#     struct_proches <- dt_centroide_participant %>%
#       filter(user_id != participant) %>%
#       st_intersection(zone_buffer) %>% # je récupère les points qui sont dans le buffer = participants dans le buffer
#       left_join(df_oso)
#     
#     df_conti_buffer = df_continu %>%
#       filter(user_id %in% unique(struct_proches$user_id))
#     
#     
#     list_struct_proches = c(unique(struct_proches$user_id))
#     
#     
#     dt_stat = tibble(user_id = participant, # participant concernée
#                      liste_struct_proches = list(list_struct_proches), # liste des participants dans un buffer de 200km
#                      nb_struct = n_distinct(list_struct_proches), # nombre de participants dans le buffer 
#                      nb_sessions_buffer = n_distinct(struct_proches$partie_id), # nombre de sessions réalisées dans le buffer (exclut les sessions de la participant concernée)
#                      nb_sess_vg_buffer = sum(struct_proches$paysage_local == "dominance_vegetation", na.rm = TRUE),# nombre de sessions dans un paysage vegetation pour les autres participants 
#                      nb_sess_ag_buffer = sum(struct_proches$paysage_local == "dominance_agricole", na.rm = TRUE),# nombre de sessions dans un paysage Agricole pour les autres participants 
#                      nb_sess_im_buffer = sum(struct_proches$paysage_local == "dominance_impermeable", na.rm = TRUE),# nombre de sessions dans un paysage Impermeable pour les autres participants 
#                      nb_sess_eq_buffer = sum(struct_proches$paysage_local == "equilibre", na.rm = TRUE),# nombre de sessions dans un paysage Equilibré pour les autres participants 
#                      
#                      # nombre de sites dans le buffer
#                      nb_site_buffer = n_distinct(df_conti_buffer$mangeoires_nom),
#                      # maximum de continuité dans le buffer
#                      max_conti_site_buffer = unique(max(df_conti_buffer$nb_an_suivi, na.rm = TRUE))
#     ) %>% 
#       
#       # on récupère les mêmes infos pour le participant concerné
#       left_join(df_oso) %>%
#       mutate(nb_sess_user_id = n_distinct(partie_id), 
#              nb_sess_im_user_id = sum(paysage_local == "dominance_impermeable", na.rm = TRUE),
#              nb_sess_ag_user_id = sum(paysage_local == "dominance_agricole", na.rm = TRUE),
#              nb_sess_vg_user_id = sum(paysage_local == "dominance_vegetation", na.rm = TRUE),
#              nb_sess_eq_user_id = sum(paysage_local == "equilibre", na.rm = TRUE),
#              nb_site_struct = sum(df_continu$user_id == participant, na.rm = TRUE),
#              max_conti_site_struct = max(df_continu$nb_an_suivi[df_continu$user_id == participant], na.rm = TRUE)) %>%
#       select(-partie_id, -paysage_local) %>%
#       unique()
#     
#     dt_stat_struct_buffer = dt_stat_struct_buffer %>%
#       bind_rows(dt_stat)
#     
#   }
#   
#   # FIN DE BOUCLE sur tous les participants
#   
#   
#   # ajout de quelques calculs en dehors de la boucle sur le df presque achevé
#   dt_stat_struct_buffer = dt_stat_struct_buffer %>% 
#     mutate(
#       # pourcentage de sessions dans impermeable dans le buffer
#       pourc_im_buffer = round(100 * nb_sess_im_buffer / nb_sessions_buffer, 1),
#       # pourcentage de sessions dans agricole dans le buffer
#       pourc_ag_buffer = round(100 * nb_sess_ag_buffer / nb_sessions_buffer, 1),
#       # pourcentage de sessions dans vegetation dans le buffer
#       pourc_vg_buffer = round(100 * nb_sess_vg_buffer / nb_sessions_buffer, 1),
#       # pourcentage de sessions dans equilibré dans le buffer
#       pourc_eq_buffer = round(100 * nb_sess_eq_buffer / nb_sessions_buffer, 1),
#       
#       # pourcentage de sessions dans impermeable en local
#       pourc_im_struct = round(100 * nb_sess_im_user_id / nb_sess_user_id, 1),
#       # pourcentage de sessions dans agricole en local
#       pourc_ag_struct = round(100 * nb_sess_ag_user_id / nb_sess_user_id, 1),
#       # pourcentage de sessions dans vegetation en local
#       pourc_vg_struct = round(100 * nb_sess_vg_user_id / nb_sess_user_id, 1),
#       # pourcentage de sessions dans equilibre en local
#       pourc_eq_struct = round(100 * nb_sess_eq_user_id / nb_sess_user_id, 1),
#       
#       # moyenne du nombre de sessions par participant 
#       moy_nb_sess_par_user_buffer = round(nb_sessions_buffer / nb_struct),
#       # poids de la participant dans le buffer
#       poids_struct_buffer = round(100 * nb_sess_user_id / (nb_sessions_buffer + nb_sess_user_id), 1),
#       # nombre de sites moyens par participant
#       nb_moy_site_buffer = round(100 * nb_site_buffer / nb_struct, 1)
#     ) %>%
#     # on remplace les -Inf qui apparaissent pour les participants qui n'ont aucun site avec des années suivant le protocole ( 3 sessions ) par des 0
#     mutate(max_conti_site_struct = ifelse(is.infinite(max_conti_site_struct), 0, max_conti_site_struct), 
#            max_conti_site_buffer = ifelse(is.infinite(max_conti_site_buffer), 0, max_conti_site_buffer))  %>%
#     # reordonnéer les colonnes
#     select(user_id, 
#            pourc_im_buffer, pourc_im_struct,  
#            pourc_ag_buffer, pourc_ag_struct,
#            pourc_vg_buffer, pourc_vg_struct,
#            pourc_eq_buffer, pourc_eq_struct,
#            poids_struct_buffer, nb_struct, moy_nb_sess_par_user_buffer, nb_sessions_buffer, nb_sess_user_id, 
#            nb_sess_im_buffer, nb_sess_im_user_id, 
#            nb_sess_ag_buffer, nb_sess_ag_user_id,
#            nb_sess_vg_buffer, nb_sess_vg_user_id,
#            nb_sess_eq_buffer, nb_sess_eq_user_id,
#            nb_moy_site_buffer, nb_site_struct, nb_site_buffer, max_conti_site_buffer,  max_conti_site_struct, liste_struct_proches)
#   
#   return(dt_stat_struct_buffer)
#   
# }

# dt_stat_participant_buffer_birdlab = func_stat_participants(dt_birdlab, dt_birdlab_tot)



