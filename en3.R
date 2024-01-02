# Load necessary libraries
library(ggplot2)
library(readxl)

# Reading the data from the CSV file
data <- read.csv("C:\\Users\\alves\\OneDrive\\ec3\\spinosus_data.csv", sep = ";")

# Convert 'Diet' to a factor and set the levels to ensure the desired order
data$Diet <- factor(data$Diet, levels = c("Animal", "Plant", "Mixed"))

# Generalized Linear Model (GLM) for Regression Analysis
glm_model <- glm(size46 ~ Temp + Diet, data = data)

# Print the summary of the model
print(summary(glm_model))

#####################################################################
# Example: Predict the size of tadpoles at 15°C for each diet
# Create a new data frame for prediction
predict_data <- data.frame(Temp = rep(15, length(levels(data$Diet))),
                           Diet = levels(data$Diet))

# Predict the sizes
predicted_sizes <- predict(glm_model, newdata = predict_data, type = "response")

# Combine the prediction with the diet for easier interpretation
predicted_results <- data.frame(Diet = levels(data$Diet), PredictedSize = predicted_sizes)

# Print the predicted results
print(predicted_results)
# K-means Clustering for Multivariate Analysis
# Selecting relevant columns for clustering

#################################################################################
cluster_data <- data[, c("weight_i", "sizeBL", "SizeTT", "weight46", "size46")]

# Performing K-means clustering
set.seed(123)  # Ensuring reproducibility
clusters <- kmeans(cluster_data, centers = 3)  # Let's assume we want 3 clusters

# Adding cluster assignments to the data
data$cluster <- as.factor(clusters$cluster)

# Plotting the clusters
ggplot(data, aes(x = size46, y = weight46, color = cluster)) +
  geom_point() +
  theme_minimal() +
  ggtitle("K-means Clustering of Tadpoles")

# Principal Component Analysis (PCA) for Ordination
pca_result <- prcomp(cluster_data, center = TRUE, scale. = TRUE)

# Plotting the PCA results
plot(pca_result, type = "l")
