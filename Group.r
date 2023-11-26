# Loading Packages
library(tidyverse)
library(GGally)
library(pheatmap)

# loading Data
data<-read.csv("Water_Quality_Testing.csv")
view(data)

# checking if data is null or not
is.null(data)

# checking if there is duplicated value or not
duplicated(data)

# creating histograms of PH level distribution in the water samples with a bin width of 0.1
hist(data$pH, col = 'blue', breaks = seq(min(data$pH), max(data$pH) + 0.1, by = 0.1),main="PH level distribution in the water samples")

#ploting all data
plot(data,col='blue')

plot(data$pH,data$Conductivity..µS.cm.,
     pch=19, #solid circle
     cex=1.5, #make 150% size
     col='red',
     main='Relation between pH level and the conductivity',
     xlab="weight in pounds",
     ylab='MPG',
)
abline(lm(Conductivity..µS.cm. ~ pH, data = data), col = 'blue',lwd=3)

plot(data$Dissolved.Oxygen..mg.L., data$pH,
     pch = 19, # solid circle
     cex = 1.5, # make 150% size
     col = 'green',
     main = 'Relation between Dissolved Oxygen and pH',
     xlab = 'Dissolved Oxygen (mg/L)',
     ylab = 'pH'
)
abline(lm(pH ~ Dissolved.Oxygen..mg.L., data = data), col = 'black', lwd = 3)


# Convert the dataset into a matrix for analysis
data_matrix <- as.matrix(data)
# Standardize the data (mean=0, std=1 for each feature)
scaled_data <- scale(data_matrix)

# Assign each data point to a cluster using K-Means clustering
# We want to find 3 clusters (groups) in the data
set.seed(2023)
# Number of clusters
k <- 3 
kmeans_model <- kmeans(scaled_data, centers = k)
# Get cluster assignments
cluster <- kmeans_model$cluster


# Convert cluster assignments into a factor (categorical variable)
level <- factor(cluster)

# Load libraries for t-SNE and data visualization
library(tsne)
library(ggplot2)
library(Rtsne)

# Perform t-SNE dimensionality reduction to 2D
tsne_result <- Rtsne(data, dims = 2)
# Create a dataframe for plotting
tsne_df <- data.frame(x = tsne_result$Y[, 1], y = tsne_result$Y[, 2], cluster = factor(level))
# Create a scatter plot to visualize the data points in 2D
ggplot(tsne_df, aes(x, y, color = cluster)) +
  geom_point() +
  labs(title = "Classification of water sample into 3 categories") +theme_minimal()

# Load necessary libraries
library(stats)
library(ggplot2)
# Define a function to scale the data
scale_data <- function(data) {
  scaled_data <- scale(data)  # Standardize the data (mean=0, std=1 for each feature)
  return(scaled_data)
}
# Define a function to perform Principal Component Analysis (PCA)
perform_pca <- function(data) {
  pca_result <- prcomp(data, scale = TRUE)  # Perform PCA with scaling
  return(pca_result)
}
# Scale the data using the scale_data function
scaled_data <- scale_data(data)  

# Perform PCA on the scaled data using the perform_pca function
pca_result <- perform_pca(scaled_data) 

# Extract the explained variances from the PCA result
explained_variances <- pca_result$sdev^2

# Create a bar plot to visualize the explained variances
barplot(explained_variances, 
        names.arg = colnames(data), 
        xlab = 'PCA feature', 
        ylab = 'variance', 
        main = 'Explained Variances',
        col = 'blue',
        las=2)
#

# Assign the 'cluster' values to a new column 'classification'
data$classification <- cluster
# Display the first few rows of the data frame to see the changes
head(data)
# Convert the 'classification' column to integer type
data$classification <- as.integer(data$classification)
# Map the integer values to categories with labels
data$classification <- factor(data$classification, levels = c(1, 2, 3), labels = c("category1", "category2", "category3"))
# Create a table to count the occurrences of each category
table(data$classification)

# Create a data frame from the category counts
category <- table(data$classification)
# Create a pie chart using ggplot2
category_df <- data.frame(category = names(category), counts = as.numeric(category))
pie_chart <- ggplot(category_df, aes(x = "", y = counts, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(counts / sum(counts))), position = position_stack(vjust = 0.5)) +
  labs(title = "Category distribution in sample data") +
  theme_void()

# Display the pie chart
print(pie_chart)

# Create a scatter plot with color-coded points based on 'classification'
ggplot(data, aes(x = pH, y =data$Dissolved.Oxygen..mg.L., color = classification)) +
  geom_point() +
  labs(title = "Oxygen distribution based on pH level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Create a box plot
box_plot <- ggplot(data, aes(x = classification, y =data$Turbidity..NTU.)) +
  geom_boxplot() +
  labs(title = "Turbidity distribution based on pH level",
       x = "Water sample category") +
  theme_minimal()
# Adjust the plot size
options(repr.plot.width=15, repr.plot.height=8)
# Display the box plot
print(box_plot)


# Load the necessary library for arranging plots
library(cowplot)
# Create a scatter plot
scatter_plot <- ggplot(data, aes(x = pH, y = data$Temperature...C., color = classification)) +
  geom_point() +
  labs(title = "Joint Plot: pH vs. Temperature",
       x = "pH", y = "Temperature (°C)") +
  theme_minimal()

# Create a KDE (Kernel Density Estimate) plot
kde_plot <- ggplot(data, aes(x = pH, y =data$Temperature...C.)) +
  geom_density_2d(aes(color = classification), alpha = 0.7) +
  theme_void()

# Create a rug plot along the x-axis
rug_plot_x <- ggplot(data, aes(x = pH)) +
  geom_rug(aes(color = classification), sides = "b") +
  theme_void()

# Create a rug plot along the y-axis
rug_plot_y <- ggplot(data, aes(x = data$Temperature...C.)) +
  geom_rug(aes(color = classification), sides = "l") +
  theme_void()

# Combine the plots side by side
joint_plot <- plot_grid(scatter_plot, kde_plot + theme(legend.position = "none"),
                        rug_plot_x + theme(legend.position = "none"),
                        rug_plot_y + theme(legend.position = "none"),
                        ncol = 2, rel_widths = c(2, 1))

# Print and display the combined plot
print(joint_plot)

# Create a scatter plot of pH vs. Conductivity
# Add points to the plot and a linear regression line
scatter_plot <- ggplot(data, aes(x = pH, y = data$Conductivity..µS.cm., color = classification)) +
  geom_point() +  # Add points to the plot
  geom_smooth(method = 'lm', se = FALSE) +  
  labs(title = "Relation between pH level and the conductivity",
       x = "pH", y = "Conductivity (µS/cm)") + 
  theme_minimal() 
# Print and display the scatter plot
print(scatter_plot)

# Calculate the mean of Dissolved Oxygen by classification
mean_dissolved_oxygen <- aggregate(data$Dissolved.Oxygen..mg.L., by = list(data$classification), FUN = mean)
# Rename the columns for clarity
colnames(mean_dissolved_oxygen) <- c("classification", "Mean_Dissolved_Oxygen")
print(mean_dissolved_oxygen)

head(data)



# Load necessary libraries
library(randomForest)
library(caret)

# Load and prepare the data
X <- data[, !(names(data) %in% c("classification"))]
y <- data$classification

# Set a random seed for reproducibility
set.seed(42)
# Split the data into training and testing sets
index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test <- y[-index]
# Build the Random Forest model on the training data
model <- randomForest(y_train ~ ., data = X_train)
y_pred <- predict(model, newdata = X_test)

# Perform k-fold cross-validation (k=5)
folds <- createFolds(y, k = 5, list = TRUE, returnTrain = FALSE)
scores <- numeric(length(folds))


for (i in 1:length(folds)) {
  fold_indices <- folds[[i]]
  fold_train <- X_train[-fold_indices, ]
  fold_test <- X_train[fold_indices, ]
  fold_y_train <- y_train[-fold_indices]
  fold_y_test <- y_train[fold_indices]
  
  # Create and train the model on fold_train
  fold_model <- randomForest(fold_y_train ~ ., data = fold_train)
  
  # Make predictions on fold_test
  fold_y_pred <- predict(fold_model, newdata = fold_test)
  
  # Calculate accuracy and store it in the 'scores' vector
  scores[i] <- sum(fold_y_pred == fold_y_test) / length(fold_y_test)
}

cat("Accuracy:", sum(y_pred == y_test) / length(y_test), "\n")

