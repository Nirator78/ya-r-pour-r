# Import csv
df <- read.csv("data.csv", header = TRUE, sep = ";")

# Retirer les lignes avec des valeurs manquantes
df <- na.omit(df)
# Retirer les lignes avec toutes les valeurs a 0 (sauf les 3 premières)
df <- df[!apply(df[-1:-3], 1, function(x) all(x == 0)), ]

# Order by date
df <- df[order(df$jour), ]
# Creation d'un nouveau csv
write.csv2(df, "data_clean.csv", row.names = FALSE, quote = FALSE)
