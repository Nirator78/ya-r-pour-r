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

df$n_cum_rappel <- df$n_cum_rappel_h + df$n_cum_rappel_f

# Group by date and reg
df <- df %>%
  group_by(date, jour, reg) %>%
  summarise(n_cum_rappel = sum(n_cum_rappel))

# Transform n_cum_rappel to percentage by age
df2 <- df %>% group_by(reg) %>% summarise(max = (max(n_cum_rappel)))

df <- merge(x = df, y = df2, by = "reg", all.x = TRUE)

df$n_cum_rappel <- ((as.double(100)*df$n_cum_rappel) / df$max)

ggplot(data = df, aes(jour, n_cum_rappel, group = reg)) +
  geom_line(aes(color = as.factor(reg)), linewidth = 1.2) +
  geom_label(aes(label = reg),
              data = df %>% filter(date == median(df$date)),
              nudge_x = 0.35,
              size = 2) +
  scale_x_date(
    labels = date_format(format = "%m-%Y"),
    breaks = date_breaks("2 month")
  ) +
  ggtitle("% de vaccination par region") +
  ylab("Pourcentage de la population vaccinée") +
  xlab("Date") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(legend.position = "bottom")
  ggsave("reticence-region.png", width = 100, height = 60, units = "cm", dpi = 300)