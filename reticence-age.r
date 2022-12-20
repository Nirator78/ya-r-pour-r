library(ggplot2)
library(dplyr)

# Import csv
df <- read.csv("data_clean.csv", header = TRUE, sep = ";")

# Transform date to int (number of days since 1970-01-01)
df$date <- as.integer(as.Date(df$jour, format = "%Y-%m-%d"))

# Delete row with clage_vacsi = 0
df <- subset(df, clage_vacsi != 0)

# Display distinct values of clage_vacsi
unique(df$clage_vacsi)

# Switch age to string
df$clage_vacsi[df$clage_vacsi == 4] <- "A"
df$clage_vacsi[df$clage_vacsi == 9] <- "A"
df$clage_vacsi[df$clage_vacsi == 11] <- "A"
df$clage_vacsi[df$clage_vacsi == 17] <- "A"
df$clage_vacsi[df$clage_vacsi == 24] <- "B"
df$clage_vacsi[df$clage_vacsi == 29] <- "B"
df$clage_vacsi[df$clage_vacsi == 39] <- "B"
df$clage_vacsi[df$clage_vacsi == 49] <- "B"
df$clage_vacsi[df$clage_vacsi == 59] <- "C"
df$clage_vacsi[df$clage_vacsi == 64] <- "C"
df$clage_vacsi[df$clage_vacsi == 69] <- "C"
df$clage_vacsi[df$clage_vacsi == 74] <- "D"
df$clage_vacsi[df$clage_vacsi == 79] <- "D"
df$clage_vacsi[df$clage_vacsi == 80] <- "D"

df$n_cum_dose1 = df$n_cum_dose1_h + df$n_cum_dose1_f

# Group by date and clage_vacsi
df <- df %>% group_by(date, clage_vacsi) %>% summarise(n_cum_dose1 = sum(n_cum_dose1))

# Create a new dataframe with clage_vacsi = A
df_A <- subset(df, clage_vacsi == "A")
# Create a new dataframe with clage_vacsi = B
df_B <- subset(df, clage_vacsi == "B")
# Create a new dataframe with clage_vacsi = C
df_C <- subset(df, clage_vacsi == "C")
# Create a new dataframe with clage_vacsi = D
df_D <- subset(df, clage_vacsi == "D")

ggplot() +
  geom_line(df_A, mapping = aes(x=date, y=n_cum_dose1), color = "blue", size = 1.5) + 
  geom_line(df_B, mapping = aes(x=date, y=n_cum_dose1), color = "green", size = 1.5) + 
  geom_line(df_C, mapping = aes(x=date, y=n_cum_dose1), color = "orange", size = 1.5) + 
  geom_line(df_D, mapping = aes(x=date, y=n_cum_dose1), color = "red", size = 1.5)

# Save df_A in csv
write.csv(df_A, "df_A.csv", row.names = FALSE, quote = FALSE)