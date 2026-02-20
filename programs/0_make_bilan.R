# Make bilan : fait tourner les différents scripts pour produire un bilan


source("programs/library.R")
# readRDS("data/data_birdlab_2025-04-30.rds")
data_birdlab = readRDS("data/dt_birdlab_tot_2025_05_19.rds") %>%
  mutate(year = year(date), 
         month = month(date)) %>%
  # suppression de la mention mâle ou femelle pour ne garder que les espèces
  mutate(espece = str_remove_all(espece, "(?i)\\s*\\(?\\b(mâle|femelle)\\b\\)?") %>% 
           str_squish()) # enleve les espaces restants



# Process des données : regroupement en saisons 
# fonction qui permet de regrouper les données par saisons de birdlab, et d'identifier 
# les données hors-saison
func_groupe_saison = function(dt_birdlab) {
  
  df_saison = dt_birdlab %>%
    select(year, month) %>%
    unique() %>%
    mutate(valide = case_when(
      month %in% c(11, 12, 1, 2, 3) ~ "saison",
      TRUE ~ "hors-saison"
    ))
  

  
  # données produites lors des saisons birdlab
  df_groupe_saison = data.frame()
  
  for (y in unique(df_saison$year)-1) {
    df = df_saison %>%
      filter(year == y & month %in% c(11, 12) | year == y+1 & month %in% c(1, 2, 3)) %>%
      mutate(saison = paste0(y, "-", y+1))
    df_groupe_saison = bind_rows(df_groupe_saison, df)
  }
  
  # données produites hors des saisons birdlab
  
  df_groupe_horssaison = data.frame()
  for (y in unique(df_saison$year)-1) {
    df = df_saison %>%
      filter(year == y & month %in% c(4,5,6,7,8,9,10) ) %>%
      mutate(saison = paste0(y, "_", "hors-saison"))
    
    df_groupe_horssaison = bind_rows(df_groupe_horssaison, df)
  }
  
  
  dt_birdlab = dt_birdlab %>%     
    left_join(rbind(df_groupe_saison, df_groupe_horssaison))
  
  
  return(dt_birdlab)
}

data_birdlab = func_groupe_saison(data_birdlab)





# source(here::here("programs", "render.R"))


