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

# Group by date and clage_vacsi
df <- df %>%
  group_by(date, jour, clage_vacsi) %>%
  summarise(n_cum_rappel = sum(n_cum_rappel))

# Transform n_cum_rappel to percentage by age
df2 <- df %>% group_by(clage_vacsi) %>% summarise(max = (max(n_cum_rappel)))

df <- merge(x = df, y = df2, by = "clage_vacsi", all.x = TRUE)

df$n_cum_rappel <- ((as.double(100)*df$n_cum_rappel) / df$max)

ggplot(data = df, aes(jour, n_cum_rappel, group = clage_vacsi)) +
  geom_line(aes(color = as.factor(clage_vacsi)), linewidth = 1.2) +
  geom_label(aes(label = clage_vacsi),
              data = df %>% filter(date == median(df$date)),
              nudge_x = 0.35,
              size = 2) +
  scale_x_date(
    labels = date_format(format = "%m-%Y"),
    breaks = date_breaks("2 month")
  ) +
  ggtitle("% de vaccination par tranche d'âge") +
  ylab("Pourcentage de la population vaccinée") +
  xlab("Date") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(legend.position = "bottom")
  ggsave("reticence-age.png", width = 100, height = 60, units = "cm", dpi = 300)