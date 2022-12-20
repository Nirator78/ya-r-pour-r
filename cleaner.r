# Import csv
df <- read.csv("data.csv", header = TRUE, sep = ";")

# Retirer les lignes avec des valeurs manquantes
df <- na.omit(df)
# Retirer les lignes avec toutes les valeurs a 0 (sauf les 3 premières)
df <- df[!apply(df[-1:-3], 1, function(x) all(x == 0)), ]

# Creation d'un nouveau csv
write.csv(df, "data_clean.csv", row.names = FALSE)