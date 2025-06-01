install.packages("GGIR")
library(GGIR)

GGIR(
  datadir = "C:/Users/baous/Downloads/geneactiv_data/",
  outputdir = "C:/Users/baous/Downloads/resultat/",
  overwrite = TRUE
)

data = load("C:/Users/baous/Downloads/resultat/output_geneactiv_data/meta/basic/meta_file1.RData")
head(data)

head(M$metalong)
head(M$metashort)

metadata <- g.inspectfile("C:/Users/baous/Downloads/geneactiv_data/file1.bin")
print(metadata)

library(ggplot2)
library(lubridate)
library(dplyr)

# 📌 Convert timestamps to date-time format
M$metalong$timestamp <- ymd_hms(M$metalong$timestamp)
M$metashort$timestamp <- ymd_hms(M$metashort$timestamp)

# 📌 Filter the first 48 hours
start_time <- min(M$metalong$timestamp)
end_time <- start_time + hours(48)

df_48h <- M$metalong %>% filter(timestamp >= start_time & timestamp <= end_time)
df_activity_48h <- M$metashort %>% filter(timestamp >= start_time & timestamp <= end_time)

# 📌 Plot light and temperature data for 48 hours
ggplot(df_48h, aes(x = timestamp)) +
  geom_line(aes(y = lightmean, color = "Light")) +
  geom_line(aes(y = temperaturemean, color = "Temperature")) +
  labs(title = "Light and Temperature over 48 Hours", x = "Time", y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("Light" = "blue", "Temperature" = "red"))

# 📌 Plot activity (ENMO) over 48 hours
ggplot(df_activity_48h, aes(x = timestamp, y = ENMO)) +
  geom_line(color = "purple") +
  labs(title = "Activity (ENMO) over 48 Hours", x = "Time", y = "ENMO") +
  theme_minimal()

######### Sleep Detection #########
df_activity_48h <- df_activity_48h %>%
  mutate(sleep = ifelse(ENMO < 0.02, "Sleep", "Awake"))

ggplot(df_activity_48h, aes(x = timestamp, y = ENMO, color = sleep)) +
  geom_line() +
  labs(title = "Activity (ENMO) with Sleep/Awake Detection", x = "Time", y = "ENMO") +
  scale_color_manual(values = c("Sleep" = "blue", "Awake" = "red")) +
  theme_minimal()

######### Walking Detection #########
df_activity_48h <- df_activity_48h %>%
  mutate(walking = ifelse(ENMO > 0.1, "Walking", "Other"))

ggplot(df_activity_48h, aes(x = timestamp, y = ENMO, color = walking)) +
  geom_line() +
  labs(title = "Activity (ENMO) with Walking Detection", x = "Time", y = "ENMO") +
  scale_color_manual(values = c("Walking" = "green", "Other" = "black")) +
  theme_minimal()
