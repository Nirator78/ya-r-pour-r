library(ggplot2)
library(dplyr)
library(scales)

# Import csv
df <- read.csv("data_clean.csv", header = TRUE, sep = ";")

# Transform date to int (number of days since 1970-01-01)
df$jour <- as.Date(df$jour, format = "%Y-%m-%d")
df$date <- as.integer(df$jour)

# Delete row with clage_vacsi = 0
df <- subset(df, clage_vacsi != 0)


df$n_cum_dose1 <- df$n_cum_dose1_h + df$n_cum_dose1_f

# Group by date and clage_vacsi
df <- df %>%
  group_by(date, jour, clage_vacsi) %>%
  summarise(n_cum_dose1 = sum(n_cum_dose1))

# Transform n_cum_dose1 to percentage by age
df2 <- df %>% group_by(clage_vacsi) %>% summarise(max = (max(n_cum_dose1)))

df <- merge(x = df, y = df2, by = "clage_vacsi", all.x = TRUE)

df$n_cum_dose1 <- ((as.double(100)*df$n_cum_dose1) / df$max)

dfFirstMarch <- df %>% filter(jour == "2021-03-01")
dfFifteenJune <- df %>% filter(jour == "2021-06-15")
dfFirstJanuary <- df %>% filter(jour == "2022-01-01")

ggplot() +
geom_boxplot(
    dfFirstMarch,
    mapping = aes(x = "001 - Début", y =  n_cum_dose1),
    color = "#009E73",
    fill = "#A4A4A4",
    outiler.shape = 8,
    outlier.size = 4,
    lwd = 2
) +
geom_boxplot(
    dfFifteenJune,
    mapping = aes(x = "002 - Milieu", y = n_cum_dose1),
    color = "#E69F00",
    fill = "#A4A4A4",
    outiler.shape = 8,
    outlier.size = 4,
    lwd = 2
) +
geom_boxplot(
    dfFirstJanuary,
    mapping = aes(x = "003 - Fin", y = n_cum_dose1),
    color = "#56B4E9",
    fill = "#A4A4A4",
    outiler.shape = 8,
    outlier.size = 4,
    lwd = 2
)

ggsave("reticence-age-date.png", width = 100, height = 60, units = "cm", dpi = 300)