
library(ggplot2)
library(dplyr)
library(tidyr)

#raw data and  convert the characters to numeric.
df_raw <- read_excel("C:/Users/baous/Downloads/data_without_nparact_values.xlsx", sheet = "Feuil2")


df <- df_raw %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.numeric, as.numeric)
names(df) <- make.names(names(df))

# I apply the LOOCV in order to be able to care for all the patients.
#  Wrapper LOOCV(Leave-One-Out Cross-Validation ) 

run_loocv <- function(data, response_var){
  n       <- nrow(data)
  preds   <- data.frame(
    LM    = numeric(n),
    Lasso = numeric(n),
    Tree  = numeric(n),
    SVR   = numeric(n)
  )
  actuals <- data[[response_var]]
  
  for(i in seq_len(n)){
    train <- data[-i, ]
    test  <- data[i, , drop = FALSE]
    
    x_train <- as.matrix(select(train, -all_of(response_var)))
    y_train <- train[[response_var]]
    x_test  <- as.matrix(select(test,  -all_of(response_var)))
    
    # 2.1  linéaire Régression
    lm_mod <- lm(reformulate(setdiff(names(train), response_var), response_var),
                 data = train)
    preds$LM[i] <- predict(lm_mod, newdata = test)
    
    # 2.2 Lasso 
    cvm <- cv.glmnet(x_train, y_train, alpha = 1)
    preds$Lasso[i] <- as.numeric(predict(cvm, x_test, s = "lambda.min"))
    
    # 2.3 décision tree 
    tree_mod <- rpart::rpart(
      reformulate(setdiff(names(train), response_var), response_var),
      data = train, method = "anova"
    )
    preds$Tree[i] <- predict(tree_mod, newdata = test)
    
    # 2.4 SVR 
    svr_mod <- e1071::svm(
      reformulate(setdiff(names(train), response_var), response_var),
      data = train, type = "eps-regression"
    )
    preds$SVR[i] <- predict(svr_mod, newdata = test)
  }
  
  #  métrics : RMSE et ME
  metrics <- lapply(preds, function(p) {
    err <- p - actuals
    c(
      RMSE = Metrics::rmse(actuals, p),
      ME   = mean(err)
    )
  })
  # data.frame
  metrics_df <- do.call(rbind, metrics) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Model")
  
  # Résults
  list(
    predictions = bind_cols(patient = seq_len(n),
                            actual  = actuals,
                            preds),
    metrics     = metrics_df
  )
}

# 3.  Loop over target variables and compile
variable <- c("Total_1","Total_2","Total_3")  
all_results <- list()
all_metrics <- data.frame()

for(t in variable){
  res <- run_loocv(df, t)
  all_results[[t]] <- res$predictions
  df_met <- res$metrics
  df_met$variable <- t
  all_metrics  <- bind_rows(all_metrics, df_met)
}

# 4. print  résults

# Exemple : prédictions for Total1
print(all_results[["Total_1"]])

# somarry metrics: RMSE et ME
print(all_metrics)

# 5. Exporting CSV files


for(t in variable){
  write.csv(
    all_results[[t]],
    file      = paste0("predictions_", t, ".csv"),
    row.names = FALSE
  )
}


write.csv(
  all_metrics,
  file      = "metrics_results_.csv",
  row.names = FALSE
)



