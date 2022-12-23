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

df$n_cum_rappel <- df$n_cum_2_rappel_f + df$n_cum_2_rappel_h

# Group by date and reg
df <- df %>%
  group_by(date, jour, reg) %>%
  summarise(n_cum_rappel = sum(n_cum_rappel))

# Transform n_cum_rappel to percentage by age
df2 <- df %>% group_by(reg) %>% summarise(max = (max(n_cum_rappel)))

df <- merge(x = df, y = df2, by = "reg", all.x = TRUE)

df$n_cum_rappel <- ((as.double(100)*df$n_cum_rappel) / df$max)

dfFirstNovember <- df %>% filter(jour == "2021-11-01")
dfFifteenMarch <- df %>% filter(jour == "2022-03-01")
dfFirstSeptember <- df %>% filter(jour == "2022-09-01")

ggplot() +
geom_boxplot(
    dfFirstNovember,
    mapping = aes(x = "001 - Début", y =  n_cum_rappel),
    color = "#009E73",
    fill = "#A4A4A4",
    outiler.shape = 8,
    outlier.size = 4,
    lwd = 2
) +
geom_boxplot(
    dfFifteenMarch,
    mapping = aes(x = "002 - Milieu", y = n_cum_rappel),
    color = "#E69F00",
    fill = "#A4A4A4",
    outiler.shape = 8,
    outlier.size = 4,
    lwd = 2
) +
geom_boxplot(
    dfFirstSeptember,
    mapping = aes(x = "003 - Fin", y = n_cum_rappel),
    color = "#56B4E9",
    fill = "#A4A4A4",
    outiler.shape = 8,
    outlier.size = 4,
    lwd = 2
)

ggsave("reticence-region-date.png", width = 100, height = 60, units = "cm", dpi = 300)