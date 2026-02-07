installed.packages("ggplot2")
installed.packages("dplyr")
installed.packages("corrplot")
installed.packages("factoextra")

library(ggplot2)
library(dplyr)
library(corrplot)
library(factoextra)


# 1. Bar Chart: Diagnosis Distribution 

ggplot(df, aes(x = diagnosis, fill = diagnosis)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Distribution of Breast Cancer Diagnosis",
    x = "Diagnosis",
    y = "Count"
  )



# 2. Box plot: Feature vs Diagnosis

ggplot(df, aes(x = diagnosis, y = radius_mean, fill = diagnosis)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Radius Mean by Diagnosis",
    x = "Diagnosis",
    y = "Radius Mean"
  )



# 3. Histograms of Numeric Features

numeric_cols <- names(df)[sapply(df, is.numeric)]

for (col in numeric_cols) {
  print(
    ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(bins = 30, fill = "green", color = "black") +
      theme_minimal() +
      labs(
        title = paste("Distribution of", col),
        x = col,
        y = "Frequency"
      )
  )
}



# 4. Scatter Plot: Two Features colored by Diagnosis

ggplot(df, aes(x = radius_mean, y = texture_mean, color = diagnosis)) +
  geom_point(size = 2, alpha = 0.7) +  
  theme_minimal() +
  labs(
    title = "Scatter Plot of Radius Mean vs Texture Mean",
    x = "Radius Mean",
    y = "Texture Mean",
    color = "Diagnosis"
  )



# 5. Correlation Heat map

numeric_df <- df %>% select(where(is.numeric))
corr_matrix <- cor(numeric_df)

corrplot(
  corr_matrix,
  method = "color",
  tl.cex = 0.6,
  number.cex = 0.5
)




# 6. K-Means Cluster Visualization

kmeans_df <- scaled_df %>% select(-diagnosis)

set.seed(123)
k2 <- kmeans(kmeans_df, centers = 2, nstart = 25)

# Visualize clusters
fviz_cluster(
  k2,
  data = kmeans_df,
  geom = "point",
  main = "K-Means Clustering (k = 2)"
)


# 6. Elbow Method (K-Means)

fviz_nbclust(
  df_scaled,
  kmeans,
  method = "wss"
) +
  labs(title = "Elbow Method for Optimal Number of Clusters")



# 8. PCA Visualization

pca_res <- prcomp(scaled_df)

fviz_pca_ind(
  pca_res,
  geom.ind = "point",
  col.ind = df$diagnosis,
  palette = c("#00AFBB", "#FC4E07"),
  addEllipses = TRUE,
  title = "PCA Visualization of Breast Cancer Dataset"
)





