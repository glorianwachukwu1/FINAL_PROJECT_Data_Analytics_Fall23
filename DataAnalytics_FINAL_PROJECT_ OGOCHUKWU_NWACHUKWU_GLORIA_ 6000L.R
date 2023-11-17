#Final_Project_6000L_NWACHUKWU_OGOCHUKWU_GLORIA

#Libraries I intend to use
library(ggplot2) #for creating plots 
library(dplyr) #for data manipulation.
library(caret)
library(reshape2)

#Installed the following libraries to help flag errors
library(lintr)
library(styler)

# To read my loaded CSV file data
library(readr)

#To Load my dataset
GLORIA_Project_data <- read_csv("Class-School/DATA_ANALYTICS/PROJECT/GLORIA_Project_data.csv")


#To view dataset 
View(GLORIA_Project_data)
head(GLORIA_Project_data)
 

#Decided to do correlation analysis between  between the corruption index and my intended poverty vaariables for this anaysis 'Inflation','Poverty_index' and 'Poverty_gap$1.90'
correlation_matrix <- cor(GLORIA_Project_data[, c("Corruption_index", "Inflation", "Poverty_index", "Poverty_gap$1.90")])

print(correlation_matrix)



# Using Heatmap to visualize the correlations of my variables 
# dark blue cell indicates a strong negative correlation
#dark red means there is a strong positive correlation
correlation_melted <- melt(correlation_matrix)
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") + theme_minimal()



#To see the summary of my data and overall Central tendency of the variables in intend to use
summary(GLORIA_Project_data)

#To see the structure of my data
str(GLORIA_Project_data)



#I decided to change the column name from "Poverty_gap$1.90" to "Poverty_gap"
names(GLORIA_Project_data)[names(GLORIA_Project_data) == "Poverty_gap$1.90"] <- "Poverty_gap"

#i Will now use "Poverty_gap" as the column name for Poverty_gap$1.90, though this will be used to label my visaulizations
Poverty_gap <- GLORIA_Project_data$Poverty_gap
View(GLORIA_Project_data)




#Next step is Exploratory data analysis  to get further insights 


# Scatter Plots

#Scatter plot for Corruption_index vs. Inflation:
plot(GLORIA_Project_data$Corruption_index, GLORIA_Project_data$Inflation,
     xlab = "Corruption_index", ylab = "Inflation",
     main = "Scatter Plot of Corruption_index vs. Inflation",
     col = "brown", pch = 16)


#Scatter plot for Corruption_index vs. Poverty_index:
plot(GLORIA_Project_data$Corruption_index, GLORIA_Project_data$Poverty_index,
     xlab = "Corruption_index", ylab = "Poverty_index",
     main = "Scatter Plot of Corruption_index vs. Poverty_index",
     col = "skyblue", pch = 16)



#Scatter plot for Corruption_index vs. Poverty_gap:
plot(GLORIA_Project_data$Corruption_index, GLORIA_Project_data$Poverty_gap,
     xlab = "Corruption_index", ylab = "Poverty_gap",
     main = "Scatter Plot of Corruption_index vs. Poverty_gap$1.90",
     col = "limegreen", pch = 16)




# Box plot

# Box plot for Corruption_index, Inflation','Poverty_index' and 'Poverty_gap$1.90
boxplot(GLORIA_Project_data$Corruption_index, GLORIA_Project_data$Inflation, GLORIA_Project_data$Poverty_index,GLORIA_Project_data$Poverty_gap,
        names = c("Corruption_index", "Inflation", "Poverty_index", "Poverty_gap$1.90"),
        main = "Boxplot of Corruption_index, 'Inflation','Poverty_index' and 'Poverty_gap$1.90",
        col = c("blueviolet", "sienna", "green",  "salmon"))



# To Create a 2x2 layout for histograms
par(mfrow = c(2, 2))

# Histogram for Corruption_index
hist(GLORIA_Project_data$Corruption_index, 
     main = "Histogram: Corruption_index",
     xlab = "Corruption_index",
     col = "skyblue",
     border = "black",
     breaks = 15
)

# Histogram for Inflation
hist(GLORIA_Project_data$Inflation, 
     main = "Histogram: Inflation",
     xlab = "Inflation",
     col = "lightgreen",
     border = "black",
     breaks = 15
)

# Histogram for Poverty_index
hist(GLORIA_Project_data$Poverty_index, 
     main = "Histogram: Poverty_index",
     xlab = "Poverty_index",
     col = "lightcoral",
     border = "black",
     breaks = 15
)

# Histogram for Poverty_gap$1.90
hist(GLORIA_Project_data$Poverty_gap, 
     main = "Histogram: Poverty_gap$1.90",
     xlab = "Poverty_gap$1.90",
     col = "yellow",
     border = "black",
     breaks = 15
)




#End_of_Final_Project_6000L_NWACHUKWU_OGOCHUKWU_GLORIA
