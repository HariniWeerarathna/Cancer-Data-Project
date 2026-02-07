library(caret)
library(randomForest)
library(pROC)
library(factoextra)
library(cluster)

# 1. Random Forest - Model Evaluation & Tuning

# Confusion Matrix - already done, repeat to confirm
conf_matrix <- confusionMatrix(rf_pred_class, test_data$diagnosis)
print(conf_matrix)



# ROC & AUC - already done, repeat to confirm

roc_obj <- roc(response = test_data$diagnosis, predictor = rf_pred_prob)
plot(roc_obj, main = "ROC Curve - Random Forest Classifier", col = "blue", lwd = 2)
auc_val <- auc(roc_obj)
print(paste("AUC:", round(auc_val, 3)))



# Hyper parameter tuning - number of variables randomly sampled at each split

best_mtry <- floor(sqrt(ncol(train_data)-1))  
rf_model_tuned <- randomForest(
  diagnosis ~ ., 
  data = train_data, 
  ntree = 500,
  mtry = best_mtry,
  importance = TRUE
)
#Print model summary 
rf_model_tuned


## Evaluate tuned model

rf_pred_class <- suppressWarnings(predict(rf_model_tuned, test_data))
rf_pred_prob  <- suppressWarnings(predict(rf_model_tuned, test_data, type = "prob")[,2])


# Confusion Matrix for test data
conf_matrix <- confusionMatrix(rf_pred_class, test_data$diagnosis)
print(conf_matrix)


# ROC Curve & AUC
roc_obj <- roc(test_data$diagnosis, rf_pred_prob)
plot(roc_obj, main="ROC Curve - Random Forest", col="darkgreen", lwd=2)
auc_val <- auc(roc_obj)
print(paste("Test AUC:", round(auc_val, 3)))



#  Feature Importance Plot 

importance_df <- as.data.frame(importance(rf_model_tuned))
importance_df$Feature <- rownames(importance_df)
importance_df <- importance_df[order(importance_df$MeanDecreaseGini, decreasing = TRUE), ]

ggplot(importance_df[1:10, ], aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 10 Feature Importance", x = "Feature", y = "MeanDecreaseGini") +
  theme_minimal()






install.packages("mclust")
library(mclust)


# 2. K-Means - Model Evaluation & Tuning

#  Determine optimal k 
fviz_nbclust(cluster_df, kmeans, method = "wss")        
fviz_nbclust(cluster_df, kmeans, method = "silhouette") 

ari_score <- adjustedRandIndex(kmeans_model$cluster, df$diagnosis)
print(paste("Adjusted Rand Index (ARI):", round(ari_score, 3)))


#Let k = 3 
set.seed(123)
kmeans_model_3 <- kmeans(cluster_df, centers = 3, nstart = 25)
fviz_cluster(kmeans_model_3, data = cluster_df, geom = "point",
             ellipse.type = "convex", main = "K-Means Clustering (k=3)")
ari_score_3 <- adjustedRandIndex(kmeans_model_3$cluster, df$diagnosis)
print(paste("ARI (k=3):", round(ari_score_3, 3)))












