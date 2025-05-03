#' export data birdlab
#'
#' @param
#'
#' @return data_birdlab, un dataframe avec l'ensemble des participations à Birdlab, ecrit un nouveau csv et supprime l'ancien csv

export_birdlab <- function() {
  
  # requete SQL
  query <- read_sql_query(here::here("sql" ,"export_a_plat_birdlab.sql")) #_simple
  
  # export donnees et ajout colonnes annee, mois et saison
  data_birdlab <- import_from_mosaic(query, database_name = "birdlab")
  
  data_birdlab <- data_birdlab %>%
    #separer les champs longitude et latitude
    mutate(longitude = stringr::str_split_fixed(data_birdlab$mangeoires_coordonnees_gps, pattern = ", ", 2)[,1],
           latitude = stringr::str_split_fixed(data_birdlab$mangeoires_coordonnees_gps, pattern = ", ", 2)[,2]) %>%
    select(-mangeoires_coordonnees_gps) %>%
    #ajouter l'agregat de la saison birdlab
    mutate(agregat_1_id = ifelse(month(date) %in% c(11,12),
                                 year(date)-2014+1,
                                 ifelse(month(date) %in% c(1,2,3),
                                        year(date)-2014,
                                        "hors_saison"))) %>%
    mutate(agregat_1_type = "saison_birdlab")
  
  #formatage dashboard
  data_birdlab <- data_birdlab %>%
    #date en format Date
    mutate(date = as.Date(date)) %>%
    #ajouter "birdlab" aux id user et participation
    mutate(user_id = paste0("birdlab_", user_id),
           partie_id = paste0("birdlab_", partie_id)) %>%
    #ajouter les champs protocole et observatoire
    mutate(protocole = "birdlab",
           type_observatoire = "grand_public") 
   
  
  #retirer l'ancien fichier de donnees
  files_to_delete <- dir(path = here::here("data", "data_output"), pattern = "data_birdlab")
  file.remove(file.path(here::here("data", "data_output"), files_to_delete))
  
  #ecrire un nouveau csv pour les donnees oab avec la date d'extraction
  data.table::fwrite(sep = ";",
                     data_birdlab,
                     file = paste0(here::here("data", "data_output"),
                                   "/data_birdlab_",
                                   Sys.Date(),
                                   ".csv"))
  
  data_birdlab <<- data_birdlab
}


