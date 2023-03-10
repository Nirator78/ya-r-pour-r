# Import dplyr
library(dplyr)
# Import ggplot2
library(ggplot2)


## df <- lapply(list.files(pattern = "data_clean.csv"), function(x) read.csv(x, header = TRUE, sep = ";"))
# Import csv de la liste dans une liste de dataframes 
df <- read.csv("data_clean.csv", header = TRUE, sep = ";")
df$date <- as.Date(df$jour, format = "%Y-%m-%d")
# Faire 3 colonnes pour les mois, les jours et les années
df$annee <- format(df$date, "%Y")
df$mois <- format(df$date, "%m")
df$jour <- format(df$date, "%d")

# group by annee et mois pour les hommes
dfAnneeMoisHomme <- df %>% group_by(annee, mois) %>% summarise(cum_dose1_homme_annee_mois = sum(n_dose1_h+n_rappel_h+n_2_rappel_h+n_3_rappel_h))
# group by annee et mois pour les femmes
dfAnneeMoisFemme <- df %>% group_by(annee, mois) %>% summarise(cum_dose1_femme_annee_mois = sum(n_dose1_f+n_rappel_f+n_2_rappel_f+n_3_rappel_f))

View(dfAnneeMoisHomme)
View(dfAnneeMoisFemme)

# group by annee et jour pour les hommes
dfAnneeJourHomme <- df %>% group_by(annee, jour) %>% summarise(cum_dose1_homme_annee_jour = sum(n_dose1_h+n_rappel_h+n_2_rappel_h+n_3_rappel_h)) 
# groupe by annee et jour pour les femmes
dfAnneeJourFemme <- df %>% group_by(annee, jour) %>% summarise(cum_dose1_femme_annee_jour = sum(n_dose1_f+n_rappel_f+n_2_rappel_f+n_3_rappel_f))

# Reunir les 2 dataframes ensemble
dfAnneeMois <- merge(dfAnneeMoisHomme, dfAnneeMoisFemme, by = c("annee", "mois"))
View(dfAnneeMois)
# Courbe comparatif des doses par mois hommes et femmes
ggplot() + geom_point(data = dfAnneeMois, aes(x = mois, y = cum_dose1_homme_annee_mois, color = "Homme")) + geom_point(data = dfAnneeMois, aes(x = mois, y = cum_dose1_femme_annee_mois, color = "Femme")) + theme_minimal() + labs(title = "Evolution des doses par mois", x = "Mois", y = "Nombre de doses") + theme(plot.title = element_text(hjust = 0.5))

# graphique en barre de l'evolution des doses par mois
ggplot(dfAnneeMoisHomme, aes(x = mois, y = cum_dose1_homme_annee_mois, fill = annee)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(title = "Evolution des doses par mois", x = "Mois", y = "Nombre de doses") + theme(plot.title = element_text(hjust = 0.5))
ggsave("AnneeMoisHomme.png", bg = "white")
ggplot(dfAnneeMoisFemme, aes(x = mois, y = cum_dose1_femme_annee_mois, fill = annee)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(title = "Evolution des doses par mois", x = "Mois", y = "Nombre de doses") + theme(plot.title = element_text(hjust = 0.5))
ggsave("AnneeMoisFemme.png", bg = "white")
# graphique en barre de l'evolution des doses par jour
ggplot(dfAnneeJourHomme, aes(x = jour, y = cum_dose1_homme_annee_jour, fill = annee)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(title = "Evolution des doses par jour", x = "Jour", y = "Nombre de doses") + theme(plot.title = element_text(hjust = 0.5))
ggsave("AnneeJourHomme.png", bg = "white")
ggplot(dfAnneeJourFemme, aes(x = jour, y = cum_dose1_femme_annee_jour, fill = annee)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(title = "Evolution des doses par jour", x = "Jour", y = "Nombre de doses") + theme(plot.title = element_text(hjust = 0.5))
ggsave("AnneeJourFemme.png", bg = "white")

# ecart type
sd(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
sd(dfAnneeMoisFemme$cum_dose1_femme_annee_mois)
# variance
var(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
var(dfAnneeMoisFemme$cum_dose1_femme_annee_mois)
# moyenne
mean(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
mean(dfAnneeMoisFemme$cum_dose1_femme_annee_mois)
# mediane
median(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
median(dfAnneeMoisFemme$cum_dose1_femme_annee_mois)
# Quantile 1
quantile(dfAnneeMoisHomme$cum_dose1_homme_annee_mois, 0.25)
quantile(dfAnneeMoisFemme$cum_dose1_femme_annee_mois, 0.25)
# Quantile 3
quantile(dfAnneeMoisHomme$cum_dose1_homme_annee_mois, 0.75)
quantile(dfAnneeMoisFemme$cum_dose1_femme_annee_mois, 0.75)

# covariances
cov(dfAnneeMoisHomme$cum_dose1_homme_annee_mois, dfAnneeMoisFemme$cum_dose1_femme_annee_mois)
# correlation
cor(dfAnneeMoisHomme$cum_dose1_homme_annee_mois, dfAnneeMoisFemme$cum_dose1_femme_annee_mois)


# Calculer a et b
a <- cov(dfAnneeMoisHomme$cum_dose1_homme_annee_mois, dfAnneeMoisFemme$cum_dose1_femme_annee_mois) / var(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
b <- mean(dfAnneeMoisFemme$cum_dose1_femme_annee_mois) - a * mean(dfAnneeMoisHomme$cum_dose1_homme_annee_mois)
# Calculer la droite de regression
dfAnneeMoisHomme$y <- a * dfAnneeMoisHomme$cum_dose1_homme_annee_mois + b

# Graphique de la droite de regression avec lm
ggplot(dfAnneeMoisHomme, 
aes(x=dfAnneeMoisHomme$cum_dose1_homme_annee_mois, y=dfAnneeMoisFemme$cum_dose1_femme_annee_mois)) + geom_point() + geom_smooth(method=lm) + labs(title = "Evolution des doses par mois", x = "Nombre de doses par mois pour les hommes", y = "Nombre de doses par mois pour les femmes")+
geom_text(aes(label=annee), vjust=-0.5, hjust=0.5, size=3) 
# Graphique de la droite de regression 
ggplot(dfAnneeMoisHomme, aes(x = cum_dose1_homme_annee_mois, y = dfAnneeMoisFemme$cum_dose1_femme_annee_mois)) + geom_point() + geom_line(aes(y = y), color = "red") + theme_minimal() + labs(title = "Evolution des doses par mois", x = "Nombre de doses par mois pour les hommes", y = "Nombre de doses par mois pour les femmes") + theme(plot.title = element_text(hjust = 0.5))
ggsave("AnneeMoisRegression.png", bg = "white")         

# Si augmentation des doses femmes, il y a augmentation des doses hommes
# Si augmentation des doses hommes, il y a augmentation des doses femmes

# Crée une colone regroupent le mois et l'année de AnneeMoisHomme
dfAnneeMoisHomme$mois_annee <- paste(dfAnneeMoisHomme$annee, dfAnneeMoisHomme$mois, 1, sep = "-")
dfAnneeMoisHomme$mois_annee <- as.Date(dfAnneeMoisHomme$mois_annee, format = "%Y-%m-%d")
View(dfAnneeMoisHomme)

dfAnneeMoisFemme$mois_annee <- paste(dfAnneeMoisFemme$annee, dfAnneeMoisFemme$mois, 1, sep = "-")
dfAnneeMoisFemme$mois_annee <- as.Date(dfAnneeMoisFemme$mois_annee, format = "%Y-%m-%d")
View(dfAnneeMoisFemme)
# Un modele pour prevoir le nombre de doses 1 pour les hommes en 2023
modeleHomme <- lm(cum_dose1_homme_annee_mois ~ poly(mois_annee,1), data = dfAnneeMoisHomme)
# prediction du nombre de doses 1 pour les hommes en 2023
newHomme <- data.frame(mois_annee = as.Date(c("2023-01-01","2023-02-01","2023-03-01","2023-04-01","2023-05-01","2023-06-01","2023-07-01","2023-08-01","2023-09-01","2023-10-01","2023-11-01","2023-12-01")))
predHomme <- predict(modeleHomme, newHomme, interval = "prediction")
predHomme <- data.frame(predHomme)
newHomme <- data.frame(newHomme)

# Prendre le range entre fit et upr
predHomme$range <- (predHomme$upr + predHomme$fit) /2
View(predHomme)

# Generation du graphique de prediction homme
png(file="prediction_2023_homme.png")
plot(dfAnneeMoisHomme$mois_annee, dfAnneeMoisHomme$cum_dose1_homme_annee_mois, col = "blue", pch = 20, cex = 1.5, xlab = "Mois", ylab = "Nombre de doses", main = "Evolution des doses par mois pour les hommes", xlim= c(as.Date('2020-12-01'), as.Date('2023-12-01')))
lines(dfAnneeMoisHomme$mois_annee, dfAnneeMoisHomme$cum_dose1_homme_annee_mois, col = "blue", pch = 20, cex = 1.5, xlab = "Mois", ylab = "Nombre de doses", main = "Evolution des doses par mois pour les hommes", xlim= c(as.Date('2020-12-01'), as.Date('2023-12-01')))
points(newHomme$mois_annee, predHomme$fit, col = "green")
points(newHomme$mois_annee, predHomme$upr, col = "orange")
points(newHomme$mois_annee, predHomme$range, col = "red")
dev.off()

# Le meme modele pour prevoir le nombre de doses 1 pour les femmes en 2023
modelFemme <- lm(cum_dose1_femme_annee_mois ~ poly(mois_annee,1), data = dfAnneeMoisFemme)
# prediction du nombre de doses 1 pour les femmes en 2023
newFemme <- data.frame(mois_annee = as.Date(c("2023-01-01","2023-02-01","2023-03-01","2023-04-01","2023-05-01","2023-06-01","2023-07-01","2023-08-01","2023-09-01","2023-10-01","2023-11-01","2023-12-01")))
predFemme <- predict(modelFemme, newFemme, interval = "prediction")
newFemme <- data.frame(newFemme)
predFemme <- data.frame(predFemme)

# Prendre le range entre fit et upr
predFemme$range <- (predFemme$upr + predFemme$fit) /2
View(predFemme)

# Generation du graphique de prediction femme
png(file="prediction_2023_femme.png")
plot(dfAnneeMoisFemme$mois_annee, dfAnneeMoisFemme$cum_dose1_femme_annee_mois, col = "blue", pch = 20, cex = 1.5, xlab = "Mois", ylab = "Nombre de doses", main = "Evolution des doses par mois pour les femmes", xlim= c(as.Date('2020-12-01'), as.Date('2023-12-01')))
lines(dfAnneeMoisFemme$mois_annee, dfAnneeMoisFemme$cum_dose1_femme_annee_mois, col = "blue", pch = 20, cex = 1.5, xlab = "Mois", ylab = "Nombre de doses", main = "Evolution des doses par mois pour les femmes", xlim= c(as.Date('2020-12-01'), as.Date('2023-12-01')))
points(newFemme$mois_annee, predFemme$fit, col = "green")
points(newFemme$mois_annee, predFemme$upr, col = "orange")
points(newFemme$mois_annee, predFemme$range, col = "red")
dev.off()


