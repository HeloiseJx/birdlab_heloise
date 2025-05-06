install.packages("igraph")  # Si ce n'est pas déjà installé
library(igraph)

# Exemple de données
data <- data.frame(
  Echantillon = c(1, 1, 2, 2, 2, 3, 3, 3),
  Espece = c("A", "B", "A", "C", "D", "B", "C", "D")
)

# Créer une matrice d’occurrence (espèces x échantillons)
library(reshape2)
mat_occurrence <- dcast(data, Echantillon ~ Espece, length)
mat_occurrence <- mat_occurrence[,-1]  # Enlever la colonne des échantillons

# Calculer la matrice de co-occurrence (espèce x espèce)
mat_cooccur <- t(as.matrix(mat_occurrence)) %*% as.matrix(mat_occurrence)

# Mettre la diagonale à 0 (on ne veut pas d'auto-cooccurrences)
diag(mat_cooccur) <- 0


# Convertir la matrice en un graphe
g <- graph_from_adjacency_matrix(mat_cooccur, mode = "undirected", weighted = TRUE, diag = FALSE)

# Définir les poids des arêtes en fonction des co-occurrences
E(g)$width <- E(g)$weight  # L’épaisseur des liens est proportionnelle aux co-occurrences

# Définir l’apparence des nœuds
V(g)$size <- degree(g) * 2  # Taille des nœuds selon leur degré (nombre de connexions)
V(g)$color <- "lightblue"

# Afficher le graphe
plot(g, vertex.label.cex = 1, edge.width = E(g)$width, layout = layout_with_fr)



