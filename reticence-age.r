library(ggplot2)
library(dplyr)

# Import csv
df <- read.csv("data_clean.csv", header = TRUE, sep = ";")

# Transform date to int (number of days since 1970-01-01)
df$date <- as.integer(as.Date(df$jour, format = "%Y-%m-%d"))

# Delete row with clage_vacsi = 0
df <- subset(df, clage_vacsi != 0)

df$n_cum_dose1 = df$n_cum_dose1_h + df$n_cum_dose1_f

# Group by date and clage_vacsi
df <- df %>% 
  group_by(date, clage_vacsi) %>% 
  summarise(n_cum_dose1 = sum(n_cum_dose1))

ggplot(df, aes(x=date, y=n_cum_dose1, group=clage_vacsi)) +
  geom_line(aes(color=clage_vacsi))+
  theme(legend.position="top")