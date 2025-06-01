
library(readxl)
library(ggplot2)

# read data and conert to numeric 
df <- read_excel(
  "C:/Users/baous/Downloads/somary_Waso.xlsx",
  sheet     = "summary_Wasso",
  col_types = c("text", "numeric", "numeric", "numeric")
)

# rename colonn

colnames(df) <- c("WASO", "Count", "Mean", "SD")

#  Preserve order

df$WASO <- factor(df$WASO, levels = df$WASO)

# plot
ggplot(df, aes(x = WASO)) +
  geom_boxplot(
    stat   = "identity",
    aes(
      ymin   = Mean - SD,
      lower  = Mean - SD,
      middle = Mean,
      upper  = Mean + SD,
      ymax   = Mean + SD
    ),
    fill  = "blue",
    color = "black",
    width = 0.6,
    na.rm = TRUE
  ) +
  # add the count
  geom_text(
    aes(
      y     = Mean + SD,
      label = Count
    ),
    vjust = -0.5,
    size  = 3,
    na.rm = TRUE
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "boxplots WASO",
    x     = "Subject",
    y     = "Average WASO ± STD"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(hjust = 0.5)
  )
