#Final_Project_6000L_NWACHUKWU_OGOCHUKWU_GLORIA

#Libraries I intend to use
library(ggplot2) #for creating plots 
library(dplyr) #for data manipulation.
library(caret)
library(reshape2)
library(lintr)
library(leaflet)
library(styler)
library(randomForest)
library(xgboost)
library(readr)
library(tidyr)
library(yardstick)
library(rpart)
library(rpart2)
library(kernlab)
library(rpart.plot)
library(gridExtra)
library(grid)
library(e1071)


#To Load my dataset
Africa_Project_data <- read_csv("Class-School/DATA_ANALYTICS/PROJECT/Africa_Project_data.csv")


#To view dataset 
View(Africa_Project_data)
head(Africa_Project_data)
 

#With this, i was enable to see the overall Central tendency of the variables in intend to use
summary(Africa_Project_data)

#To see the structure of my data
str(Africa_Project_data)

#To handle missing values
Africa_Project_dataa <- na.omit(Africa_Project_data)

#I decided to change the column names of my column as the names where too long
#Changed "Poverty_gap columns
colnames(Africa_Project_data)[colnames(Africa_Project_data) == "Poverty_$1.90_gap"] <- "gap1.90_"
colnames(Africa_Project_data)[colnames(Africa_Project_data) == "Poverty_$3.20_gap"] <- "gap3.20_"
colnames(Africa_Project_data)[colnames(Africa_Project_data) == "Poverty_$5.50_gap"] <- "gap5.50_"

#changed  "Poverty_headcount column
colnames(Africa_Project_data)[colnames(Africa_Project_data) == "Poverty_$1.90_headcount"] <- "count1.90_"
colnames(Africa_Project_data)[colnames(Africa_Project_data) == "Poverty_$3.20_headcount"] <- "count3.20_"
colnames(Africa_Project_data)[colnames(Africa_Project_data) == "Poverty_$5.50_headcount"] <- "count5.50_"

#changed  "household column
colnames(Africa_Project_data)[colnames(Africa_Project_data) == "Household_Cons"] <- "household"

# View the dataset again to confirm the changes
View(Africa_Project_data)


#Next step is Exploratory data analysis  to gain further insights into the distribution of my variables 
# Scatter Plots
# To Create a 2x2 layout for scatter plots
par(mfrow = c(2, 2))
#Scatter plot for Corruption_index vs. Inflation:
plot(Africa_Project_data$Corruption_index, Africa_Project_data$Inflation,
     xlab = "Corruption_index", ylab = "Inflation",
     main = "Scatter Plot of Corruption index vs. Inflation",
     col = "brown", pch = 16)


#Scatter plot for Corruption_index vs. Poverty_index:
plot(Africa_Project_data$Corruption_index, Africa_Project_data$Poverty_index,
     xlab = "Corruption_index", ylab = "Poverty_index",
     main = "Scatter Plot of Corruption index vs. Poverty index",
     col = "skyblue", pch = 16)


#Scatter plot for Corruption_index vs. Poverty_gap$3.20:
plot(Africa_Project_data$Corruption_index, Africa_Project_data$gap3.20_,
     xlab = "Corruption_index", ylab = "gap3.20_",
     main = "Scatter Plot of Corruption index vs. Poverty gap_$3.20",
     col = "limegreen", pch = 16)


#Scatter plot for Corruption_index vs. Poverty_$3.20_headcount:
plot(Africa_Project_data$Corruption_index, Africa_Project_data$count3.20_,
     xlab = "Corruption_index", ylab = "count3.20_",
     main = "Scatter Plot of Corruption index vs. Poverty headcount_$3.20",
     col = "black", pch = 16)


# To visualize the Corruption index of countries using histogram
ggplot(Africa_Project_data, aes(x = Country, y = Corruption_index, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Corruption index by Country",
       x = "Country",
       y = "Corruption_index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE)












# Correlation matrix
numeric_columns <- sapply(Africa_Project_data, is.numeric) 
cor_matrix <- cor(Africa_Project_data[, numeric_columns, omit= 2])
print(cor_matrix) #this result showed the variables which are highly correlated in my dataset

# i then converted the correlation matrix to a data frame for heatmap
cor_df <- as.data.frame(as.table(cor_matrix))
colnames(cor_df) <- c("Var1", "Var2", "Correlation")

#To create a heatmap using ggplot2 to see the varaibles correlations
ggplot(cor_df, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "green", high = "navy") +
  labs(title = "Heatmap of Africa_Project_data",
       x = "Variable 1",
       y = "Variable 2")


#To check for Normality using Shapiro-Wilk test
dim(Africa_Project_data)

# To Perform Shapiro-Wilk tests for normality on all my datasets
shapiro_tests <- sapply(Africa_Project_data[, -c(1, 2)], function(x) shapiro.test(x)$p.value)

# Sort variables based on p-values
sorted_vars <- names(sort(shapiro_tests))

# Create a bar plot
barplot(shapiro_tests[sorted_vars], names.arg = sorted_vars, col = "skyblue",
        main = "Shapiro-Wilk Test Results for Normality",
        xlab = "", ylab = "p-value",
        las = 2, cex.names = 0.7)

# Add a horizontal line at alpha = 0.05 significance level
abline(h = 0.05, col = "red", lty = 2)



#I decided to explore further for each of my interesting variables
#Inflation
shapiro_Inflation <- shapiro.test(Africa_Project_data$Inflation)
print(shapiro_Inflation)
#Poverty_index
shapiro_Poverty_index <- shapiro.test(Africa_Project_data$Poverty_index)
print(shapiro_Poverty_index)
#Poverty gap
shapiro_gap3.20_ <- shapiro.test(Africa_Project_data$gap3.20_)
print(shapiro_gap3.20_)
#Poverty headcount
shapiro_count3.20_ <- shapiro.test(Africa_Project_data$count3.20_)
print(shapiro_count3.20_)

#using histogram to view distribution for Shapiro-Wilk test on the variables
hist(Africa_Project_data$Inflation, main = "Inflation", col = "pink")
hist(Africa_Project_data$Poverty_index, main = "Poverty Index", col = "brown")
hist(Africa_Project_data$gap3.20_, main = "Poverty_$3.20_headcount", col = "orange")
hist(Africa_Project_data$count3.20_, main = "Poverty_$3.20_headcount", col = "blue")







#Outliers
# To identify outliers using IQR method
Q1 <- quantile(Africa_Project_data$Corruption_index, 0.25)
Q3 <- quantile(Africa_Project_data$Corruption_index, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

#To  Identify rows with outliers
outliers <- Africa_Project_data$Corruption_index < lower_bound | Africa_Project_data$Corruption_index > upper_bound

# Display summary statistics and outliers
summary(Africa_Project_data$Corruption_index)

outliers_data <- Africa_Project_data[outliers, ]
head(outliers_data) #This flagged some data points as outliers. to understand why
# To understand why they are outliers by examining the characteristics of outliers
summary(outliers_data)

#To explore the outliers further

#Poverty_index is a significant factor, so i will explore it further 
table(outliers_data$Poverty_index)
summary(outliers_data$Poverty_index)
# Visualize Poverty_index
barplot(table(outliers_data$Poverty_index), main="Poverty_index", xlab="index number", ylab="Frequency")


#Modeling
#Four models will be used of this analysis
#model 1 - Linear regression

# Split the dataset into training and testing sets
set.seed(129)  # For reproducibility
split_index <- createDataPartition(Africa_Project_data$Corruption_index, p = 0.8, list = FALSE)
train_data <- Africa_Project_data[split_index, ]
test_data <- Africa_Project_data[-split_index, ]


#Training and Testing and predict Corruption_index using Inflation, poverty_index, gap3.20_ and count3.20
# Train linear regression model
lm_model <- lm(Corruption_index ~ Inflation + Poverty_index + gap3.20_ + count3.20_, data = train_data)
summary(lm_model)
# Predictions on training set
train_preds_lm <- predict(lm_model, newdata = train_data)
# Predictions on test set
test_preds_lm <- predict(lm_model, newdata = test_data) # lm model

# Evaluate linear regression model
lm_mae <- mean(abs(test_preds_lm - test_data$Corruption_index)) #MAE
lm_rmse <- sqrt(mean((test_preds_lm - test_data$Corruption_index)^2))#RMSE

##model 2 - decision tree model
# Train decision tree model
tree_model <- rpart(Corruption_index ~ Inflation + Poverty_index + gap3.20_ + count3.20_, data = train_data)
summary(tree_model)
rpart.plot(tree_model) #to plot my tree

# Predictions on training set
train_preds_tree <- predict(tree_model, newdata = train_data)
# Predictions on test set
test_preds_tree <- predict(tree_model, newdata = test_data) # Decision tree model

# Evaluate decision tree model
tree_mae <- mean(abs(test_preds_tree - test_data$Corruption_index))
tree_rmse <- sqrt(mean((test_preds_tree - test_data$Corruption_index)^2))


##model 3 - Support vector machine (SVM) model
# Train SVM model
svm_model <- svm(Corruption_index ~ Inflation + Poverty_index + gap3.20_ + count3.20_, data = train_data)
summary(svm_model)

# Predictions on training set
train_preds_svm <- predict(svm_model, newdata = train_data)
# Predictions on test set
test_preds_svm <- predict(svm_model, newdata = test_data) # SVM model

# Evaluate SVM model
svm_mae <- mean(abs(test_preds_svm - test_data$Corruption_index))
svm_rmse <- sqrt(mean((test_preds_svm - test_data$Corruption_index)^2))


# #model 4 -  Random forest model
# Train random forest model
rf_model <- randomForest(Corruption_index ~ Inflation + Poverty_index + gap3.20_ + count3.20_, data = train_data)
summary(rf_model)
varImpPlot(rf_model)
# Predictions on training set
train_preds_rf <- predict(rf_model, newdata = train_data)
# Predictions on test set
test_preds_rf <- predict(rf_model, newdata = test_data) # rf model

# Evaluate random forest model
rf_mae <- mean(abs(test_preds_rf - test_data$Corruption_index))#MAE
rf_rmse <- sqrt(mean((test_preds_rf - test_data$Corruption_index)^2)) #RMSE



# Display evaluation metrics
cat("Linear Regression - MAE:", lm_mae, " RMSE:", lm_rmse, "\n")
cat("Decision Tree - MAE:", tree_mae, " RMSE:", tree_rmse,  "\n")
cat("SVM - MAE:", svm_mae, " RMSE:", svm_rmse, "\n")
cat("Random Forest - MAE:", rf_mae, " RMSE:", rf_rmse, "\n")

# To visualize the result
all_results <- rbind(
  data.frame(Model = "Linear Regression", MAE = lm_mae, RMSE = lm_rmse),
  data.frame(Model = "Decision Tree", MAE  = tree_mae, RMSE = tree_rmse),
  data.frame(Model = "SVM", MAE  = svm_mae, RMSE = svm_rmse),
  data.frame(Model = "Random Forest", MAE  = rf_mae, RMSE= rf_rmse)
)

# To visualize my result comparism
melted_all_results <- gather(all_results, Metric, Value, -Model)
# Create a vector of custom colors
custom_colors <- c( "lightgreen", "goldenrod", "lightcoral", "skyblue")

# Create a bar plot for MAE and RMSE for all models with custom colors
ggplot(melted_all_results, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(title = "Model Comparison - MAE and RMSE",
       y = "Value",
       x = "Model",
       fill = "Metric") +
  theme_minimal()


#SVM Model has the lowest Mean average error (MAE) for  prediction error at 0.5591482  #The Random Forest model has the lowest RMSE at 0.753179 which prompted cross-validation 


#Cross-validation and tuning of parameters
# control parameters for my cross-validation
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

formula <- Corruption_index ~ Inflation + Poverty_index + gap3.20_ + count3.20_

# Set the seed for reproducibility
set.seed(150)

# Model 1: Linear Regression
lm_model_cv <- train(formula, data = train_data, method = "lm", trControl = ctrl)

# Model 2: Decision Tree
tree_model_cv <- train(formula, data = train_data, method = "rpart2", trControl = ctrl)

# Model 3: Support Vector Machine (SVM)
svm_model_cv <- train(formula, data = train_data, method = "svmRadial", trControl = ctrl)

# Model 4: Random Forest
rf_model_cv <- train(formula, data = train_data, method = "rf", trControl = ctrl)

#For all my cross-validated results
print(lm_model_cv)
print(tree_model_cv)
print(svm_model_cv)
print(rf_model_cv)

#To visualize my cross-validated results
plot(lm_model_cv)
plot(tree_model_cv)
plot(svm_model_cv)
plot(rf_model_cv)


#At the end of the cross-validation,  both SVM models and Random Forest have relatively low MAE values, but the Random Forest model has a slightly lower MAE. Therefore, based on the MAE metric alone, the Random Forest model performed slightly better in this comparison.



#End_of_A6_6000L_NWACHUKWU_OGOCHUKWU_GLORIA

#Thank_You!
