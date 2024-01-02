# Load necessary libraries
library(ggplot2)
library(readxl)

# Reading the Excel data
data <- read.csv("C:\\Users\\alves\\OneDrive\\ec3\\spinosus_data.csv", sep = ";")

# Convert 'Diet' to a factor for regression analysis
data$Diet <- as.factor(data$Diet)

# Generalized Linear Model (GLM) for Regression Analysis
glm_model <- glm(size46 ~ Temp + Diet, data = data)
print(summary(glm_model))

# K-means Clustering for Multivariate Analysis
# Selecting relevant columns for clustering
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

# Load necessary libraries
library(ggplot2)
library(readxl)

# Reading the data from an Excel file
data <- read.csv("C:\\Users\\alves\\OneDrive\\ec3\\spinosus_data.csv", sep = ";")

# Convert 'Diet' to a factor and set 'Animal' as the reference level explicitly
data$Diet <- relevel(factor(data$Diet), ref = "Animal")

# Generalized Linear Model (GLM) for Regression Analysis
glm_model <- glm(size46 ~ Temp + Diet, data = data)
print(summary(glm_model))

# Plotting the effect of Temperature on Size at Stage 46
ggplot(data, aes(x = Temp, y = size46)) +
  geom_point(aes(color = Diet)) +
  geom_smooth(method = "lm", aes(color = Diet)) +
  theme_minimal() +
  labs(title = "Effect of Temperature on Size at Stage 46",
       x = "Temperature (°C)",
       y = "Size at Stage 46")

# Plotting the effect of Diet on Size at Stage 46
ggplot(data, aes(x = Diet, y = size46)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Effect of Diet on Size at Stage 46",
       x = "Diet",
       y = "Size at Stage 46")

