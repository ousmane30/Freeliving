library(readxl)
library(ggplot2)

# read data and convert to numeric beacause my excel open it like a caractere

df <- read_excel(
  "C:/Users/baous/Downloads/somary_Waso.xlsx",
  sheet     = "summary_Wasso",
  col_types = c("text", "numeric", "numeric", "numeric")
)

# choose the colonne
colnames(df) <- c("WASO", "Count", "Mean", "STD")

# 3. Preserve order
df$WASO <- factor(df$WASO, levels = df$WASO)

# 4. Plot
ggplot(df, aes(x = WASO, y = Mean)) +
  geom_col(fill  = "blue", color = "black") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Distribution of average WASO by subject (with number of nights)",
    x     = "Subject",
    y     = "average WASO"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(hjust = 0.5)
  )
