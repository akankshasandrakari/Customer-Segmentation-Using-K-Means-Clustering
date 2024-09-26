# 1. Loading the necessary libraries
library(ggplot2)
library(dplyr)

# 2. Load the dataset
# For this example, use a CSV file with 'Age', 'Annual Income', and 'Spending Score'
data <- read.csv("C:\\Users\\akank\\OneDrive\\Documents\\customer\\archive\\Mall_Customers.csv")


# 3. Select the features for clustering
# We are using 'Annual Income' and 'Spending Score' for clustering
customer_data <- data[, c("Annual.Income..k..", "Spending.Score..1.100.")]

# 4. Standardize the data
customer_data_scaled <- scale(customer_data)

# 5. Finding the optimal number of clusters using the Elbow Method
set.seed(123)
wcss <- vector()
for (i in 1:10) {
  kmeans_model <- kmeans(customer_data_scaled, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plotting the Elbow plot to find the optimal number of clusters
plot(1:10, wcss, type="b", main="Elbow Method",
     xlab="Number of clusters", ylab="WCSS")

# 6. Applying K-Means to the dataset with the optimal number of clusters (say 5)
set.seed(123)
kmeans_model <- kmeans(customer_data_scaled, centers = 5)

# 7. Adding the cluster results to the original data
data$Cluster <- as.factor(kmeans_model$cluster)

# 8. Visualizing the clusters using ggplot2
ggplot(data, aes(x = Annual.Income..k.., y = Spending.Score..1.100., color = Cluster)) +
  geom_point(size = 4) +
  ggtitle("Customer Segments") +
  xlab("Annual Income (k$)") +
  ylab("Spending Score (1-100)") +
  theme_minimal()

# 9. Inspecting cluster centers and summary statistics
cluster_centers <- kmeans_model$centers
print(cluster_centers)

summary(data$Cluster)
# head(data)
# str(data)
# list.files("C:/Users/akank/OneDrive/Documents/customer/archive")
# data(iris)  # Load the built-in iris dataset
# head(iris)  # View the first few rows of the iris dataset



