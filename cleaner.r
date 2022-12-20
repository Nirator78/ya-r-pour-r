# Import csv
df <- read.csv("data.csv", header = TRUE, sep = ";")

# Retirer les lignes avec des valeurs manquantes
df <- na.omit(df)
# Retirer les lignes avec toutes les valeurs a 0 (sauf les 3 premières)
df <- df[!apply(df[-1:-3], 1, function(x) all(x == 0)), ]

# Order by date
df <- df[order(df$jour), ]
# On recupere l'année de la date
dfjour <- strsplit(df$jour, "-")
df$annee <- sapply(dfjour, function(x) x[1])
# Recuperation des années uniques
annees <- unique(df$annee)

# Création d'un csv par année
for (i in 1:length(annees)) {
  df2 <- df[df$annee == annees[i], ]
  write.csv2(df2, paste0("data_clean_", annees[i], ".csv"), row.names = FALSE, quote = FALSE)
}
