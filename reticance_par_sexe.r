# Import dplyr
library(dplyr)
# Import ggplot2
library(ggplot2)

# Import csv de la liste dans une liste de dataframes 
## df <- lapply(list.files(pattern = "data_clean.csv"), function(x) read.csv(x, header = TRUE, sep = ";"))
df <- read.csv("data_clean.csv", header = TRUE, sep = ";")
df$date <- as.Date(df$jour, format = "%Y-%m-%d")
# Faire 3 colonnes pour les mois, les jours et les années
df$annee <- format(df$date, "%Y")
df$mois <- format(df$date, "%m")
df$jour <- format(df$date, "%d")


# cumule les doses global
df$cum_dose1_homme_global <- cumsum(df$n_dose1_h)
df$cum_dose1_femme_global <- cumsum(df$n_dose1_f)

# group by annee et mois pour les hommes
dfAnneeMoisHomme <- df %>% group_by(annee, mois) %>% summarise(cum_dose1_homme_annee_mois = sum(n_dose1_h+n_rappel_h+n_2_rappel_h+n_3_rappel_h))
# group by annee et mois pour les femmes
dfAnneeMoisFemme <- df %>% group_by(annee, mois) %>% summarise(cum_dose1_femme_annee_mois = sum(n_dose1_f+n_rappel_f+n_2_rappel_f+n_3_rappel_f))

# group by annee et jour pour les hommes
dfAnneeJourHomme <- df %>% group_by(annee, jour) %>% summarise(cum_dose1_homme_annee_jour = sum(n_dose1_h+n_rappel_h+n_2_rappel_h+n_3_rappel_h)) 
# groupe by annee et jour pour les femmes
dfAnneeJourFemme <- df %>% group_by(annee, jour) %>% summarise(cum_dose1_femme_annee_jour = sum(n_dose1_f+n_rappel_f+n_2_rappel_f+n_3_rappel_f))

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

# Un modele de regression lineaire avec lm
modele <- lm(dfAnneeMoisFemme$cum_dose1_femme_annee_mois ~ dfAnneeMoisHomme$cum_dose1_homme_annee_mois, data = dfAnneeMoisHomme)
# prediction de la valeur de la dose 1 pour les hommes
predict(modele, data.frame(dfAnneeMoisHomme$cum_dose1_homme_annee_mois == 1000000))
# prediction de la valeur de la dose 1 pour les femmes
predict(modele, data.frame(dfAnneeMoisFemme$cum_dose1_femme_annee_mois == 1000000))


View(dfAnneeMoisHomme)
# Crée une colone regroupent le mois et l'année de AnneeMoisHomme

dfAnneeMoisHomme$mois_annee <- paste(dfAnneeMoisHomme$annee, dfAnneeMoisHomme$mois, 1, sep = "-")
dfAnneeMoisHomme$mois_annee <- as.Date(dfAnneeMoisHomme$mois_annee, format = "%Y-%m-%d")
View(dfAnneeMoisHomme)
# Un modele pour prevoir le nombre de doses 1 pour les hommes en 2023
modele2 <- lm(cum_dose1_homme_annee_mois ~ poly(mois_annee,1), data = dfAnneeMoisHomme)
# prediction du nombre de doses 1 pour les hommes en 2023
new <- data.frame(mois_annee = as.Date(c("2023-01-01","2023-02-01","2023-03-01","2023-04-01","2023-05-01","2023-06-01","2023-07-01","2023-08-01","2023-09-01","2023-10-01","2023-11-01","2023-12-01")))
pred <- predict(modele2, new, interval = "prediction")
new <- data.frame(new)
pred <- data.frame(pred)

# Prendre le range entre fit et upr
pred$range <- (pred$upr + pred$fit) /2
View(pred)

png(file="prediction_2023_homme.png")
plot(dfAnneeMoisHomme$mois_annee, dfAnneeMoisHomme$cum_dose1_homme_annee_mois, col = "blue", pch = 20, cex = 1.5, xlab = "Mois", ylab = "Nombre de doses", main = "Evolution des doses par mois pour les hommes", xlim= c(as.Date('2020-12-01'), as.Date('2023-12-01')))
lines(dfAnneeMoisHomme$mois_annee, dfAnneeMoisHomme$cum_dose1_homme_annee_mois, col = "blue", pch = 20, cex = 1.5, xlab = "Mois", ylab = "Nombre de doses", main = "Evolution des doses par mois pour les hommes", xlim= c(as.Date('2020-12-01'), as.Date('2023-12-01')))
points(new$mois_annee, pred$fit, col = "green")
points(new$mois_annee, pred$upr, col = "orange")
points(new$mois_annee, pred$range, col = "red")
dev.off()
