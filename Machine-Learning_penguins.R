install.packages("RSQLite")
install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")
install.packages("ISLR")
install.packages("cluster")
install.packages("factoextra")
install.packages("ggplot2")
install.packages("caTools")
install.packages("e1071")
install.packages("caret")
install.packages("class")
install.packages("randomForest")
install.packages("glmnet")
install.packages("rpart")

library(RSQLite)

library(dplyr)
library(readr)
library(tidyverse)
library(ISLR)
library(cluster)
library(factoextra)
library(ggplot2)
library(caTools)
library(e1071)
library(caret)
library(class)
library(randomForest)
library(glmnet)
library(rpart)

# https://www.kaggle.com/datasets/youssefaboelwafa/clustering-penguins-species/data
penguins <- read_csv("penguins.csv")
view(penguins)

# Dropping NA values
penguins1 <- na.omit(penguins)
summary(penguins1) # flipper length looks like having outliers

# Checking and remove outliers
boxplot(penguins1$flipper_length_mm)
penguins2 <- subset(penguins1, penguins1$flipper_length_mm < 300 & penguins1$flipper_length_mm > 100)
summary(penguins2)

# Ensuring there is only MALE and FEMALE in the sex column
unique(penguins2$sex)
penguins3 <- penguins2[penguins2$sex != ".", ]
unique(penguins3$sex)

######################### Our data is clean now, ready to do ML! #########################

######### 1. Clustering

# Creating dummies for sexes, then dropping original column 
penguins4 <- penguins3
penguins4$sex_f <- ifelse(penguins3$sex == "FEMALE", 1, 0)
penguins4$sex_m <- ifelse(penguins3$sex == "MALE", 1, 0)
penguins4 <- penguins4[, -5]
view(penguins4)

# Scaling data
pengscale = scale(penguins4)
view(pengscale)


# Set seed for reproducibility
set.seed(123)

#### Gap stat ####

# Compute the Gap Statistic
gap_stat <- clusGap(pengscale, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Print the Gap Statistic results
print(gap_stat, method = "firstmax")

# Visualize the Gap Statistic
fviz_gap_stat(gap_stat) 

#### Elbow method ####

# Determine the optimal number of clusters using the Elbow Method
fviz_nbclust(pengscale, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")

# Enter number of clusters after data set (try with 3 first, but 4 seems most optimal in visual!)
km.res <- kmeans(pengscale, 3, nstart = 25)  
## nstart argument runs the clustering 25 times each with different centers and pick the most optimal one

# Visualizing optimal clusters
fviz_cluster(km.res, data = pengscale, palette = "jco",
             ggtheme = theme_minimal())

# Enter number of clusters after data set (4 seems most optimal in visual!, 5 and 6 are overlapping)
km.res <- kmeans(pengscale, 4, nstart = 25)  
## nstart argument runs the clustering 25 times each with different centers and pick the most optimal one

# Visualizing optimal clusters
fviz_cluster(km.res, data = pengscale, palette = "jco",
             ggtheme = theme_minimal())


#### Hierarchical clustering ####
hc.complete = hclust(dist(pengscale), method="complete")
plot(hc.complete)

# Draw rectangles around clusters (3)
rect.hclust(hc.complete, k = 3, border = "red") 
clusters <- cutree(hc.complete, k = 3)
table(clusters)

# Draw rectangles around clusters (4)
rect.hclust(hc.complete, k = 4, border = "red") 
clusters <- cutree(hc.complete, k = 4)
table(clusters)



######## 2. Classification

#Sex as factor
penguins5 <- penguins3
penguins5$sex = as.factor(penguins3$sex)
str(penguins5)


#### Naive Bayes

# Splitting the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(penguins5$sex, p = 0.8, list = FALSE)
pengTrain <- penguins5[trainIndex, ]
pengTest <- penguins5[-trainIndex, ]

# create a naive bayes model
model = naiveBayes(x = pengTrain[,c("culmen_length_mm", "culmen_depth_mm")], y = pengTrain$sex)

# predict the sex on test data
y_pred = predict(model, newdata = pengTest[,c("culmen_length_mm", "culmen_depth_mm")])

# prepare to plot the train data
data = pengTrain

# create meshgrid
minX1 = min(data[,1]); maxX1 = max(data[,1]); range1 = diff(range(data[,1]))
minX2 = min(data[,2]); maxX2 = max(data[,2]); range2 = diff(range(data[,2]))
len = 120
X1 = seq(from=minX1-0.1*range1, to=maxX1+0.1*range2, length.out = len)
X2 = seq(from=minX2-0.1*range2, to=maxX2+0.1*range2, length.out = len)

grid_data = expand.grid(X1,X2)
colnames(grid_data) = c("culmen_length_mm", "culmen_depth_mm")

y_grid = predict(model, newdata = grid_data)


### Plot the train data
plot(data[,c("culmen_length_mm", "culmen_depth_mm")], main = "Naive Bayes (Training Data)", xlab = "Culmen length", ylab = "Culmen depth", xlim = range(X1), ylim = range(X2))

# plot the mesh grid
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

# plot the points
points(data, pch = 21, bg = ifelse(data[,"sex"] == "MALE", "skyblue", "pink"))
legend("topright", legend=c("Male", "Female"), fill=c("skyblue", "pink"), cex=0.8, box.lty=0)


# prepare to plot the test data
data = pengTest

# create meshgrid
minX1 = min(data[,1]); maxX1 = max(data[,1]); range1 = diff(range(data[,1]))
minX2 = min(data[,2]); maxX2 = max(data[,2]); range2 = diff(range(data[,2]))
len = 120
X1 = seq(from=minX1-0.1*range1, to=maxX1+0.1*range2, length.out = len)
X2 = seq(from=minX2-0.1*range2, to=maxX2+0.1*range2, length.out = len)

grid_data = expand.grid(X1,X2)
colnames(grid_data) = c("culmen_length_mm", "culmen_depth_mm")

y_grid = predict(model, newdata = grid_data)

### Plot the test data
plot(data[,c("culmen_length_mm", "culmen_depth_mm")], main = "Naive Bayes (Test Data)", xlab = "Culmen length", ylab = "Culmen depth", xlim = range(X1), ylim = range(X2))

# plot the mesh grid
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

# plot the points
points(data, pch = 21, bg = ifelse(data[,"sex"] == "MALE", "skyblue", "pink"))

# code the legend
legend("topright", legend=c("Male", "Female"), fill=c("skyblue", "pink"), cex=0.8, box.lty=0)


# Create a confusion matrix
confMatrix <- confusionMatrix(y_pred,pengTest$sex)
print(confMatrix)

#### KNN

# Splitting the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(penguins5$sex, p = 0.8, list = FALSE)
pengTrain <- penguins5[trainIndex, ]
pengTest <- penguins5[-trainIndex, ]


# Separate features and labels
trainFeatures <- pengTrain[, c("culmen_length_mm", "culmen_depth_mm")]
trainLabels <- pengTrain$sex
testFeatures <- pengTest[, c("culmen_length_mm", "culmen_depth_mm")]
testLabels <- pengTest$sex

# Check dimensions to ensure consistency
cat("Training features dimensions: ", dim(trainFeatures), "\n")
cat("Training labels length: ", length(trainLabels), "\n")
cat("Testing features dimensions: ", dim(testFeatures), "\n")
cat("Testing labels length: ", length(testLabels), "\n")

# Ensure that trainFeatures and trainLabels have the same number of rows
stopifnot(nrow(trainFeatures) == length(trainLabels))

# Set up cross-validation to find best k
trainControl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train the model using different values of k
set.seed(123)
knnFit <- train(
  trainFeatures, 
  trainLabels,
  method = "knn",
  tuneGrid = expand.grid(k = 1:10),
  trControl = trainControl
)

# Print the results
print(knnFit)
plot(knnFit)

# Set the number of neighbors
k <- 5

# Train the KNN model
knnPred <- knn(train = trainFeatures, test = testFeatures, cl = trainLabels, k = k)

# Prepare a mesh grid for visualization
x_min <- min(trainFeatures[, 1]) - 1
x_max <- max(trainFeatures[, 1]) + 1
y_min <- min(trainFeatures[, 2]) - 1
y_max <- max(trainFeatures[, 2]) + 1
x_seq <- seq(x_min, x_max, by = 0.01)
y_seq <- seq(y_min, y_max, by = 0.01)
grid <- expand.grid(x_seq, y_seq)
colnames(grid) <- c("culmen_length_mm", "culmen_depth_mm")

# Predict on the mesh grid
grid_pred <- knn(train = trainFeatures, test = grid, cl = trainLabels, k = k)

# prepare to plot the train data
data = pengTrain

# Plot the train data
plot(data[,c("culmen_length_mm", "culmen_depth_mm")], 
     main = "KNN (Training Data)", 
     xlab = "Culmen length", ylab = "Culmen depth", xlim = range(X1), ylim = range(X2))


points(data, pch = 21, bg = ifelse(data[,"sex"] == "MALE", "skyblue", "pink"))

contour(x_seq, y_seq, matrix(as.numeric(grid_pred), length(x_seq), length(y_seq)), add = TRUE)

legend("topright", legend=c("Male", "Female"), fill=c("skyblue", "pink"), cex=0.8, box.lty=0)


# prepare to plot the train data
data = pengTest

# Plot the test data
plot(data[,c("culmen_length_mm", "culmen_depth_mm")], 
     main = "KNN (Test Data)", 
     xlab = "Culmen length", ylab = "Culmen depth", xlim = range(X1), ylim = range(X2))


points(data, pch = 21, bg = ifelse(data[,"sex"] == "MALE", "skyblue", "pink"))

contour(x_seq, y_seq, matrix(as.numeric(grid_pred), length(x_seq), length(y_seq)), add = TRUE)

legend("topright", legend=c("Male", "Female"), fill=c("skyblue", "pink"), cex=0.8, box.lty=0)


# Create a confusion matrix
confMatrix <- confusionMatrix(knnPred, testLabels)
print(confMatrix)


######## 3. Regression

set.seed(123)
trainIndex <- createDataPartition(penguins5$body_mass_g, p = 0.8, list = FALSE)
trainData <- penguins5[trainIndex, ]
testData <- penguins5[-trainIndex, ]

### Linear Regression

lm_model <- lm(body_mass_g ~ ., data = trainData)
lm_pred <- predict(lm_model, testData)

# Calculate metrics
lm_rmse <- sqrt(mean((lm_pred - testData$body_mass_g)^2))
lm_r2 <- 1 - sum((lm_pred - testData$body_mass_g)^2) / sum((mean(trainData$body_mass_g) - testData$body_mass_g)^2)

### Decision Tree

tree_model <- rpart(body_mass_g ~ ., data = trainData)
tree_pred <- predict(tree_model, testData)

# Calculate metrics
tree_rmse <- sqrt(mean((tree_pred - testData$body_mass_g)^2))
tree_r2 <- 1 - sum((tree_pred - testData$body_mass_g)^2) / sum((mean(trainData$body_mass_g) - testData$body_mass_g)^2)

### Random Forest

rf_model <- randomForest(body_mass_g ~ ., data = trainData)
rf_pred <- predict(rf_model, testData)

# Calculate metrics
rf_rmse <- sqrt(mean((rf_pred - testData$body_mass_g)^2))
rf_r2 <- 1 - sum((rf_pred - testData$body_mass_g)^2) / sum((mean(trainData$body_mass_g) - testData$body_mass_g)^2)

### Support Vector Regression (SVR)

svr_model <- svm(body_mass_g ~ ., data = trainData)
svr_pred <- predict(svr_model, testData)

# Calculate metrics
svr_rmse <- sqrt(mean((svr_pred - testData$body_mass_g)^2))
svr_r2 <- 1 - sum((svr_pred - testData$body_mass_g)^2) / sum((mean(trainData$body_mass_g) - testData$body_mass_g)^2)

### Ridge Regression
x <- model.matrix(body_mass_g ~ ., trainData)[, -1]
y <- trainData$body_mass_g
ridge_model <- cv.glmnet(x, y, alpha = 0)
ridge_pred <- predict(ridge_model, s = "lambda.min", newx = model.matrix(body_mass_g ~ ., testData)[, -1])

# Calculate metrics
ridge_rmse <- sqrt(mean((ridge_pred - testData$body_mass_g)^2))
ridge_r2 <- 1 - sum((ridge_pred - testData$body_mass_g)^2) / sum((mean(trainData$body_mass_g) - testData$body_mass_g)^2)

### Lasso Regression
lasso_model <- cv.glmnet(x, y, alpha = 1)
lasso_pred <- predict(lasso_model, s = "lambda.min", newx = model.matrix(body_mass_g ~ ., testData)[, -1])

# Calculate metrics
lasso_rmse <- sqrt(mean((lasso_pred - testData$body_mass_g)^2))
lasso_r2 <- 1 - sum((lasso_pred - testData$body_mass_g)^2) / sum((mean(trainData$body_mass_g) - testData$body_mass_g)^2)


results <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest", "SVR", "Ridge", "Lasso"),
  RMSE = c(lm_rmse, tree_rmse, rf_rmse, svr_rmse, ridge_rmse, lasso_rmse),
  R2 = c(lm_r2, tree_r2, rf_r2, svr_r2, ridge_r2, lasso_r2)
)

print(results)


# Combine predictions into a data frame
predictions <- data.frame(
  Actual = testData$body_mass_g,
  Linear_Regression = lm_pred,
  Decision_Tree = tree_pred,
  Random_Forest = rf_pred,
  SVR = svr_pred,
  Ridge = as.numeric(ridge_pred),
  Lasso = as.numeric(lasso_pred))

#Visualize results
ggplot(results, aes(x = Model)) +
  geom_bar(aes(y = RMSE), stat = "identity", fill = "lightgreen", alpha = 0.5) +
  geom_point(aes(y = R2 * max(RMSE)), color = "purple", size = 5) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / max(results$RMSE), name = "R-squared")
  ) +
  labs(title = "Regression Model Comparison", y = "RMSE", x = "Model") +
  theme_minimal()


# Plot predictions
ggplot(predictions, aes(x = Actual)) +
  geom_point(aes(y = Linear_Regression, color = "Linear Regression")) +
  geom_point(aes(y = Random_Forest, color = "Random Forest")) +
  geom_point(aes(y = Decision_Tree, color = "Decision Tree")) +
  geom_point(aes(y = SVR, color = "SVR")) +
  geom_point(aes(y = Ridge, color = "Ridge")) +
  geom_point(aes(y = Lasso, color = "Lasso")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(y = "Predicted Body Mass (g)", color = "Model") +
  ggtitle("Model Predictions vs Actual Body Mass") +
  theme_minimal()

