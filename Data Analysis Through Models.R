install.packages("caret")
install.packages("randomForest")
install.packages("pROC")

library(caret)
library(randomForest)
library(pROC)
library(ggplot2)

# 1.Classification - Random Forest

# Combine scaled features with original diagnosis
model_df <- data.frame(
  scaled_df,
  diagnosis = df$diagnosis
)

# Correct factor conversion 
model_df$diagnosis <- factor(model_df$diagnosis)

# Check class counts
table(model_df$diagnosis)


# Train-test split
set.seed(123)
train_index <- createDataPartition(
  model_df$diagnosis,
  p = 0.7,
  list = FALSE)

train_data <- model_df[train_index, ]
test_data  <- model_df[-train_index, ]

# Random Forest Model
rf_model <- randomForest(
  diagnosis ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Predictions
rf_pred_class <- predict(rf_model, test_data)
rf_pred_prob  <- predict(rf_model, test_data, type = "prob")[, 2]


# Confusion Matrix
confusionMatrix(rf_pred_class, test_data$diagnosis)

importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

importance_df <- importance_df[order(importance_df$MeanDecreaseGini, decreasing = TRUE), ]




# Plot top 10 features

ggplot(importance_df[1:10, ], aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 10 Feature Importance", x = "Feature", y = "MeanDecreaseGini") +
  theme_minimal()


roc_obj <- roc(
  response = test_data$diagnosis,
  predictor = rf_pred_prob
)

plot(
  roc_obj,
  main = "ROC Curve - Random Forest Classifier",
  col = "blue",
  lwd = 2
)

auc(roc_obj)





install.packages("factoextra")   
install.packages("dplyr") 
library(dplyr)        
library(factoextra) 


# 2.Clustering - K Means

# remove diagnosis
cluster_df <- scaled_df %>% select(-diagnosis)

# Determine optimal clusters (Elbow Method)
fviz_nbclust(
  cluster_df,
  kmeans,
  method = "wss"
)

# Apply K-Means with k = 2
set.seed(123)

kmeans_model <- kmeans(
  cluster_df,
  centers = 2,
  nstart = 25
)

# PCA-based cluster visualization
fviz_cluster(
  kmeans_model,
  data = cluster_df,
  geom = "point",
  ellipse.type = "convex",
  main = "K-Means Clustering of Breast Cancer Data (k = 2)"
)

# Compare clusters with actual diagnosis
table(
  Cluster = kmeans_model$cluster,
  Diagnosis = df$diagnosis
)





