# load_data
data <- read.csv("C:/Users/baous/OneDrive/Documents/new_predictions_Total_3.csv", sep=";", header=TRUE)

# select colonne
models <- c("Lasso", "Tree", "SVR")

#  Calculate corrélations of Pearson
corr_vals <- sapply(models, function(mod) {
  cor(data$actual, data[[mod]], method = "pearson")
})

#  Gather and round off the results
results <- data.frame(
  Modèle      = models,
  Corrélation = round(corr_vals, 3)
)

#  print 
print(results)

# plot
library(ggplot2)


#   data.frame
df <- data.frame(
  Modèle = factor(models, levels=models),
  Corr   = corr_vals
)

# plot heatmap 
ggplot(df, aes(x = Modèle, y = 1, fill = Corr)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Corr))) +
  scale_fill_gradient2(
    low      = "blue",
    mid      = "white",
    high     = "red",
    midpoint = 0,
    limits   = c(-1,1),
    name     = "toatal_3"
  ) +
  theme_minimal() +
  theme(
    axis.title   = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid   = element_blank()
  )
