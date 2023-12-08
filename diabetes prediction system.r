# Dataset Taken From : https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset

# There 3 dataset: 
# df, dataset with data from patients without diabetes, pre-diabetes and diabetes
# df2, balanced patients with and without diabetes
# df3, imbalanced dataset of patients with and without diabetes
# 
# This analysis will work with the last 2, where we will explore df3 imbalanced and large dataset to understand the features 
# and target relationship. We will use this information to predict patients in balanced dataset df2.
# Logistic Regression
# Naive Bayes
# Decision Tree
# Random Forest
# XGBoost

#setting the work space
setwd("C:/Users/Sharon/OneDrive - Asia Pacific University/MASTERS/SEMESTER 1/AML/Assignment/Dataset")

#----------------Installing Libraries-----------------------------------

#Installing and Loading library 
install.packages("ggthemes")
install.packages("ggsignif")
install.packages('skimr')
install.packages('caTools')
install.packages("rpart.plot")
install.packages('patchwork')
install.packages("gridExtra")
install.packages("rattle")
install.packages("Boruta")
install.packages("ROSE")
install.packages("naivebayes")
install.packages("glmnet")
install.packages("tidymodels")
installed.packages("yardstick")

library(glmnet)
library(yardstick)
library(tidymodels)
library(naivebayes)
library(ROSE)
library(Boruta)
library(rattle)
library(RColorBrewer)
library(gridExtra)
library(patchwork)
library(tibble)
library(rpart)
library(rpart.plot)
library(readr)
library(caTools)
library(caret)
library(ROCR)
library(dplyr)
library(skimr)
library(ggplot2)
library(DataExplorer)
library(ggthemes)
library(ggsignif)

#----------------Reading to Data Frame -----------------------------------

#Writing into a data frame
fileURL <- "https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset"
download.file(fileURL, destfile="diabetes_binary_health_data.csv", method="curl")

#df1 <- read.table("diabetes_012_health_indicators_BRFSS2015", na.strings = "?", sep=",", header = TRUE)
#df2<- read.table("diabetes_binary_5050split_health_indicators_BRFSS2015.csv", na.strings = "?", sep=",", header = TRUE)
df3 <- read.table("diabetes_binary_health_indicators_BRFSS2015.csv", na.strings = "?", sep=",", header = TRUE)
View (df3)

#------------------Data Exploration ---------------------------------------
#create_report(df4)

#----------------EDA -Data Exploration Analysis -----------------------------------
# View dataset
dim(df3) 
str(df3)
glimpse(df3)
head(df3, 5)
summary(df3)
skim(df3)
create_report(df3)

#----------------EDA -Multi-Variate Analysis -----------------------------------

# plotting the correlation heat map
corr_matrix <- cor(df3)
corr_df3 <- melt(corr_matrix)

# Create the heat map
ggplot(corr_df3, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "navy", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Correlation Heatmap")

# Strong Correlations:
#   Variables Physical Health and General Health correlated positivily
# BMI_bins was extracted from BMI (ignore this for now)
# 
# The analysis revealed a significant positive correlation between the Physical Health and General Health variables, suggesting that General Health may be a composite measure comprising Physical Health Sum and other related factors.
# 
# Variables Income and General Health correlated negativily
# Furthermore, the analysis indicated a negative correlation between Income and General Health, implying that individuals with lower incomes may experience limited access to private medical care, potentially leading to negative impacts on their overall health outcomes.

# Calculate the correlation between all variables and the target variable
correlations <- cor(df3)[, "Diabetes_binary"]
correlations <- correlations[order(abs(correlations), decreasing = TRUE)]

# Create a horizontal bar plot
library(ggplot2)
df_cor <- data.frame(variable = names(correlations), y = correlations)
ggplot(df_cor, aes(x = correlations, y = reorder(variable, correlations))) + 
  geom_bar(stat = "identity", fill = "#238e71") +
  coord_flip()+
  labs(title = "Correlation with Diabetes_binary", x = "Correlation", y = "Variable") +
  theme(axis.text.y = element_text(size = 10, color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 10, color = "black"),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

#This variables that have a very weak correlation :AnyHealthcare,NoDocbcCost, Fruits,Sex, Smoker, Veggies

#----------------EDA -Uni variate Analysis --------------------------------------
#histogram of df3
plot_histogram(df3)

# Gaussian test - Check if the density plot of Gaussian
plot_density(df3)

#plot the numerical variable distributions
#Age
Age_Hist_Plot <- ggplot(df3, aes(Age))+
  geom_histogram(position = "dodge", bins = 30, color = "darkblue", fill = "lightblue")+
  labs(title = "Age Histogram Plot")+
  theme_tufte()

Age_Box_Plot <- ggplot(df3, aes(Age))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "Age Box Plot")+
  theme_tufte()
Age_Hist_Plot + Age_Box_Plot 

# BMI
BMI_Hist_Plot <- ggplot(df3, aes(BMI))+
  geom_histogram(position = "dodge", bins = 30, color = "darkblue", fill = "lightblue")+
  labs(title = "BMI Histogram Plot")+
  theme_tufte()

BMI_Box_Plot <- ggplot(df3, aes(BMI))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "BMI Box Plot")+
  theme_tufte()
BMI_Hist_Plot + BMI_Box_Plot

# Education
Edu_Hist_Plot <- ggplot(df3, aes(Education))+
  geom_histogram(position = "dodge", bins = 30, color = "darkblue", fill = "lightblue")+
  labs(title = "Education Histogram Plot")+
  theme_tufte()

Edu_Box_Plot <- ggplot(df3, aes(Education))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "Education Box Plot")+
  theme_tufte()
Edu_Hist_Plot + Edu_Box_Plot

#General Health
GenHealth_Hist_Plot <- ggplot(df3, aes(GenHlth))+
  geom_histogram(position = "dodge", bins = 30, color = "darkblue", fill = "lightblue")+
  labs(title = "General Histogram Plot")+
  theme_tufte()

GenHealth_Box_Plot <- ggplot(df3, aes(GenHlth))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "General Box Plot")+
  theme_tufte()
GenHealth_Hist_Plot + GenHealth_Box_Plot

#Income
Income_Hist_Plot <- ggplot(df3, aes(Income))+
  geom_histogram(position = "dodge", bins = 30,color = "darkblue", fill = "lightblue")+
  labs(title = "Income Histogram Plot")+
  theme_tufte()

Income_Box_Plot <- ggplot(df3, aes(Income))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "Income Box Plot")+
  theme_tufte()
Income_Hist_Plot + Income_Box_Plot

#Mental Health
MenHlth_Hist_Plot <- ggplot(df3, aes(MentHlth))+
  geom_histogram(position = "dodge", bins = 30,color = "darkblue", fill = "lightblue")+
  labs(title = "Mental Health Histogram Plot")+
  theme_tufte()

MenHlth_Box_Plot <- ggplot(df3, aes(MentHlth))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "Mental Health Box Plot")+
  theme_tufte()
MenHlth_Hist_Plot + MenHlth_Box_Plot

#Physical Health
PhysHlth_Hist_Plot <- ggplot(df3, aes(PhysHlth))+
  geom_histogram(position = "dodge", bins = 30, color = "darkblue", fill = "lightblue")+
  labs(title = "Physical Health Histogram Plot")+
  theme_tufte()

PhysHlth_Box_Plot <- ggplot(df3, aes(PhysHlth))+
  geom_boxplot(color = "darkblue", fill = "lightblue")+
  labs(title = "Physical Health Box Plot")+
  theme_tufte()
PhysHlth_Hist_Plot + PhysHlth_Box_Plot


#Create a count plot of Diabetes_binary
df3$Diabetes_binary <- as.factor(df3$Diabetes_binary)
print(ggplot(data = df3, aes(x = Diabetes_binary, fill = Diabetes_binary)) +
        geom_bar() +
        scale_fill_manual(values = c("#00BFFF", "#FFA500")) +
        geom_text(stat = "count", aes(label = after_stat(count)), 
                  position = position_stack(vjust = 0.5)) +
        labs(title = "Count Plot of Target Variable", x = "Diabetes", y = "Count"))

#count values and calculate percentage
counts <- table(df3$Diabetes_binary)
percentages <- prop.table(counts) * 100
print(barplot(percentages, horiz=TRUE, xlim=c(0,100), main="Diabetes_binary Target Distribution (%)", 
              xlab="% of Patients", ylab="",
              col=c("cornflowerblue", "salmon"), names.arg=c("Non-Diabetic", "Diabetic"),
              las=1, cex.names=0.8, border=NA))
text(percentages, 1:2, labels=paste(round(percentages, 1), "%"), pos=4);
#conclusion -15% of the cleaned dataset has diabetes. Imbalanced dataset.

#----------------Data Pre- Processing-----------------------------------
# Check Missing Data
colSums(sapply(df3,is.na)) 
plot_missing(df3)

# Check Duplicated value
duplicates <- df3[duplicated(df3), ]
cat("Duplicate Rows: ", nrow(duplicates), "\n")
head(duplicates)

#-------------------Data Cleaning -----------------------------------------
data_clean <- function(dataframe) {
  bins <- c(0, 18.5, 24.9, 29.9, Inf)
  labels <- c(1, 2, 3, 4)
  
  dataframe <- dataframe %>%
    distinct() %>%
    mutate(BMI_bins = cut(BMI, breaks = bins, labels = labels)) %>%
    rownames_to_column() %>%
    select(-rowname) %>%
    mutate(
      HighBP = as.integer(HighBP),
      HighChol = as.integer(HighChol),
      CholCheck = as.integer(CholCheck),
      BMI = as.integer(BMI),
      Smoker = as.integer(Smoker),
      Stroke = as.integer(Stroke),
      HeartDiseaseorAttack = as.integer(HeartDiseaseorAttack),
      PhysActivity = as.integer(PhysActivity),
      Fruits = as.integer(Fruits),
      Veggies = as.integer(Veggies),
      HvyAlcoholConsump = as.integer(HvyAlcoholConsump),
      AnyHealthcare = as.integer(AnyHealthcare),
      NoDocbcCost = as.integer(NoDocbcCost),
      GenHlth = as.integer(GenHlth),
      MentHlth = as.integer(MentHlth),
      PhysHlth = as.integer(PhysHlth),
      DiffWalk = as.integer(DiffWalk),
      Sex = as.integer(Sex),
      Age = as.integer(Age),
      Education = as.integer(Education),
      Income = as.integer(Income),
      BMI_bins = as.integer(BMI_bins)
    )
  return(dataframe)
}

df3= data_clean(df3)
str(df3)

#Rename the columns
colnames(df3) <-c("Diabetes_binary","High_BP","High_Chol", "Chol_Check", "BMI","Smoker","Stroke","HeartDiseaseorAttack", "Physical_Activity", 
                  "Consume_Fruits", "Consume_Veges","Heavy_Drinker","Health_Care_Access","Healthcare_CostIssue", "General_Health",
                  "Mental_Health","Physical_Health","Difficult_to_Walk","Sex","Age","Education","Income","BMI_bins")

#Get the first column and remove it from the data frame and Add the first column as the last column
Diabetes_binary <- df3[, 1]
df3 <- df3[, -1]
df3 <- cbind(df3, Diabetes_binary)
str(df3)

#------------------Feature Selection - Boruta Algorithm-------------------------------------
# Convert the Diabetes_binary column to a factor variable
df3$Diabetes_binary <- as.factor(df3$Diabetes_binary)

#running boruta algorithm for feature selections
boruta <- Boruta(df3[,1:23], df3$Diabetes_binary, doTrace=2)
important.features <- getSelectedAttributes(boruta, withTentative = FALSE)
print(important.features)
# [1] "Diabetes_binary"      "HighBP"               "HighChol"             "CholCheck"            "BMI"                 
# [6] "Smoker"               "HeartDiseaseorAttack" "HvyAlcoholConsump"    "AnyHealthcare"        "GenHlth"             
# [11] "MentHlth"             "PhysHlth"             "DiffWalk"             "Sex"  

#therefore we will drop the other columns
df3 <- select(df3, -c(Stroke, Education, Healthcare_CostIssue, Income, Age, Consume_Fruits, Consume_Veges, BMI_bins))
View(df3)

#----------------Data Splitting for imbalance data Preds--------------------------------------------------------------
# Split df3_norm into training and testing sets
set.seed(1234) # for reproducibility
split <- sample.split(df3$Diabetes_binary, SplitRatio = 0.7)
train <- subset(df3, split == TRUE)
test <- subset(df3, split == FALSE)
head(train,5)
head(test,5)
# Checking Class distribution
table(df3$Diabetes_binary)
prop.table(table(df3$Diabetes_binary))
prop.table(table(train$Diabetes_binary))

#----------------Baseline machine learning models - imbalance data------------------------------

# Define Baseline machine learning models -without balancing the data
lr_model <- glm(Diabetes_binary ~ ., data = train, family = "binomial")
summary(lr_model)

nb_model <- naiveBayes(Diabetes_binary~ ., data = train)
summary(nb_model)

rf_model <- randomForest(Diabetes_binary ~ ., data = train)
print(rf_model)
#attributes(rf_model)

# Build the decision tree model with entropy
dt_model = rpart(Diabetes_binary~ ., train, method="class" ,parms = list(split = 'information'), 
                 control =rpart.control(minsplit = 1,minbucket=2, cp=0.00002))
dt_model_ptree<- prune(dt_model,cp= dt_model$cptable[which.min(dt_model$cptable[,"xerror"]),"CP"])
#summary(dt_model_ptree)
#fancyRpartPlot(dt_model_ptree, uniform=TRUE, main="Pruned Classification Tree")
printcp(dt_model_ptree)


# Make predictions on train data
lr_train_pred <- predict(lr_model, newdata = train[,-15], type = "response")
lr_train_pred_class <- ifelse(lr_train_pred > 0.5, 1, 0)

nb_train_pred <- predict(nb_model, newdata = train[,-15], type = "raw")
nb_train_pred_class <- ifelse(nb_train_pred[,2] > 0.5, 1, 0)

rf_train_pred <- predict(rf_model, newdata = train[, -15])

dt_train_pred <- predict(dt_model_ptree, train, type = "class")

# Calculate the evaluation metrics on the train data
lr_train_metrics <- calc_metrics(train$Diabetes_binary, lr_train_pred_class)
nb_train_metrics <- calc_metrics(train$Diabetes_binary, nb_train_pred_class)
rf_train_metrics <- calc_metrics(train$Diabetes_binary, rf_train_pred)
dt_train_metrics <- calc_metrics(train$Diabetes_binary, dt_train_pred)

# Create a data frame to store evaluation metrics on train data
train_metrics_df <- data.frame(Model = c("Logistic Regression", "Naive Bayes", "Random Forest", "Decision Tree"),
                               Accuracy = c(lr_train_metrics[1], nb_train_metrics[1], rf_train_metrics[1], dt_train_metrics[1]),
                               Sensitivity = c(lr_train_metrics[2], nb_train_metrics[2], rf_train_metrics[2], dt_train_metrics[2]),
                               Precision = c(lr_train_metrics[3], nb_train_metrics[3], rf_train_metrics[3], dt_train_metrics[3]),
                               F1_Score = c(lr_train_metrics[4], nb_train_metrics[4], rf_train_metrics[4], dt_train_metrics[4]))

train_metrics_df


# Make predictions on test data
#make predictions on the test set
# Predict using the Logistic Regression model
lr_pred <- predict(lr_model, newdata = test[,-15], type = "response")
lr_pred_class <- ifelse(lr_pred > 0.5, 1, 0)

# Predict using the Naive Bayes model
nb_pred <- predict(nb_model, newdata = test[,-15], type = "raw")
nb_pred_class <- ifelse(nb_pred[,2] > 0.5, 1, 0)

# Predict using the Random Forest model
rf_pred <-predict(rf_model, newdata = test[, -15]) 

# Make predictions on test data
dt_pred <- predict(dt_model_ptree,test, type = "class")



#Evaluate model 
# Define a function to calculate accuracy, sensitivity, precision, and F1-score
calc_metrics <- function(actual, predicted){
  TP <- sum(actual == 1 & predicted == 1)
  TN <- sum(actual == 0 & predicted == 0)
  FP <- sum(actual == 0 & predicted == 1)
  FN <- sum(actual == 1 & predicted == 0)
  
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)
  precision <- TP / (TP + FP)
  F1_score <- 2 * precision * sensitivity / (precision + sensitivity)
  
  metrics <- c(accuracy, sensitivity, precision, F1_score)
  return(metrics)
}

#----------------Feature Engineering - Data Normalization-----------------------------------

# Balance the training data using ROSE
df3_balanced_new <- ovun.sample(Diabetes_binary ~ ., data = df3, method = "both", N = nrow(df3), seed = 1234)$data

#Create a count plot of Diabetes_binary
print(ggplot(data = df3_balanced_new, aes(x = Diabetes_binary, fill = Diabetes_binary)) +
        geom_bar() +
        scale_fill_manual(values = c("#00BFFF", "#FFA500")) +
        geom_text(stat = "count", aes(label = after_stat(count)), 
                  position = position_stack(vjust = 0.5)) +
        labs(title = "Count Plot of Target Variable After Data Balancing", x = "Diabetes", y = "Count"))


df3_balanced_norm <-df3_balanced_new
df3_balanced_norm[, c("BMI", "General_Health", "Mental_Health", "Physical_Health")] <- 
  apply(df3_balanced_new[, c("BMI", "General_Health", "Mental_Health", "Physical_Health")], 2, function(x) (x - min(x)) / (max(x) - min(x)))
View(df3_balanced_norm)
#----------------Data Splitting for balance data Preds --------------------------------------------------------------

# Split df3_norm into training and testing sets
set.seed(1234) # for reproducibility
split <- sample.split(df3_balanced_norm$Diabetes_binary, SplitRatio = 0.7)
train_balanced <- subset(df3_balanced_norm, split == TRUE)
test_balanced <- subset(df3_balanced_norm, split == FALSE)

# Checking Class distribution
table(df3_balanced_norm$Diabetes_binary)
prop.table(table(df3_balanced_norm$Diabetes_binary))
prop.table(table(train_balanced$Diabetes_binary))

#----------------Baseline machine learning models - balance data------------------------------

lr_model_balanced  <- glm(Diabetes_binary ~ ., data = train_balanced, family = "binomial")
summary(lr_model_balanced)

nb_model_balanced  <- naiveBayes(Diabetes_binary~ ., data = train_balanced)
summary(nb_model_balanced)

rf_model_balanced  <- rf_model <- randomForest(Diabetes_binary ~ ., data = train_balanced, ntree = 500)
print(rf_model_balanced)

dt_model_balanced = rpart(Diabetes_binary~ ., train_balanced, method="class" ,parms = list(split = 'information'), 
                          control =rpart.control(minsplit = 1,minbucket=2, cp=0.00002))
dt_model_ptree_balanced<- prune(dt_model_balanced,cp= dt_model_balanced$cptable[which.min(dt_model_balanced$cptable[,"xerror"]),"CP"])
summary(dt_model_ptree_balanced)
#printcp(dt_model_balanced)
#fancyRpartPlot(dt_model_ptree_balanced, uniform=TRUE, main="Pruned Classification Tree")
#printcp(dt_model_ptree_balanced)


#make predictions on the train set
# Predict using the Logistic Regression model
lr_train_pred_balanced <- predict(lr_model_balanced, newdata = train_balanced[, -15], type = "response")
lr_train_pred_class_balanced <- ifelse(lr_train_pred_balanced > 0.5, 1, 0)

# Predict using the Naive Bayes model
nb_train_pred_balanced <- predict(nb_model_balanced, newdata = train_balanced[, -15], type = "raw")
nb_train_pred_class_balanced <- ifelse(nb_train_pred_balanced[,2] > 0.5, 1, 0)

# Predict using the Random Forest model
rf_train_pred_balanced <- predict(rf_model_balanced, newdata = train_balanced[, -15], type = "class")

# Predict using the Decision Tree model
dt_train_pred_balanced  <- predict(dt_model_ptree_balanced, newdata = test_balanced, type = "class")

#make predictions on the test set
# Predict using the Logistic Regression model
lr_pred_balanced <- predict(lr_model_balanced, newdata = test_balanced[,-15], type = "response")
lr_pred_class_balanced <- ifelse(lr_pred_balanced > 0.5, 1, 0)

# Predict using the Naive Bayes model
nb_pred_balanced  <- predict(nb_model_balanced, newdata = test_balanced[, -15], type = "raw")
nb_pred_class_balanced  <- ifelse(nb_pred_balanced[,2] > 0.5, 1, 0)

# Predict using the Random Forest model
predicted <- predict(dt_model_ptree, newdata = test_balanced[,-15], type = "class")

# Predict using the Decision Tree model
dt_train_pred_balanced<- predict(dt_model_pred, newdata = test_balanced[, -15], type = "class")


#Evaluate model 
# Calculate the accuracy, sensitivity, precision, and F1-score for each model on the test data
lr_metrics_balanced <- calc_metrics(test_balanced$Diabetes_binary, lr_pred_class_balanced)
nb_metrics_balanced <- calc_metrics(test_balanced$Diabetes_binary, nb_pred_class_balanced)
rf_metrics_balanced <- calc_metrics(test_balanced$Diabetes_binary, rf_pred)
dt_metrics_balanced <- calc_metrics(test_balanced$Diabetes_binary, dt_pred_balanced)

# Create a data frame to store evaluation metrics
metrics_df_balanced <- data.frame(Model = c("Logistic Regression", "Naive Bayes", "Random Forest", "Decision Tree"),
                                  Accuracy = c(lr_metrics_balanced[1], nb_metrics_balanced[1], rf_metrics_balanced[1], dt_metrics_balanced[1]),
                                  Sensitivity = c(lr_metrics_balanced[2], nb_metrics_balanced[2], rf_metrics_balanced[2], dt_metrics_balanced[2]),
                                  Precision = c(lr_metrics_balanced[3], nb_metrics_balanced[3], rf_metrics_balanced[3], dt_metrics_balanced[3]),
                                  F1_Score = c(lr_metrics_balanced[4], nb_metrics_balanced[4], rf_metrics_balanced[4], dt_metrics_balanced[4]))

# Display the data frame without styles
knitr::kable(metrics_df_balanced, format = "markdown", row.names = FALSE, col.names = c("Model", "Accuracy", "Sensitivity", "Precision", "F1-Score"))

library(ROCR)

library(ROCR)

# Calculate predicted probabilities for each model on the test set
lr_prob_balanced <- predict(lr_model_balanced, newdata = test_balanced[, -15], type = "response")
nb_prob_balanced <- predict(nb_model_balanced, newdata = test_balanced[, -15], type = "raw")[,2]
rf_prob_balanced <- predict(rf_model_balanced, newdata = test_balanced[, -15], type = "prob")[,2]
dt_prob_balanced <- predict(dt_model_ptree_balanced, newdata = test_balanced[, -15], type = "prob")[,2]

# Create prediction objects for each model
lr_pred_obj_balanced <- prediction(lr_prob_balanced, test_balanced$Diabetes_binary)
nb_pred_obj_balanced <- prediction(nb_prob_balanced, test_balanced$Diabetes_binary)
rf_pred_obj_balanced <- prediction(rf_prob_balanced, test_balanced$Diabetes_binary)
dt_pred_obj_balanced <- prediction(dt_prob_balanced, test_balanced$Diabetes_binary)

# Calculate AUC for each model
lr_auc_balanced <- performance(lr_pred_obj_balanced, measure = "auc")@y.values[[1]]
nb_auc_balanced <- performance(nb_pred_obj_balanced, measure = "auc")@y.values[[1]]
rf_auc_balanced <- performance(rf_pred_obj_balanced, measure = "auc")@y.values[[1]]
dt_auc_balanced <- performance(dt_pred_obj_balanced, measure = "auc")@y.values[[1]]

# Plot ROC curve and AUC for each model separately
plot_ROC_and_AUC(lr_pred_obj_balanced, lr_auc_balanced, "blue", "ROC Curve - Logistic Regression")
plot_ROC_and_AUC(nb_pred_obj_balanced, nb_auc_balanced, "red", "ROC Curve - Naive Bayes")
plot_ROC_and_AUC(rf_pred_obj_balanced, rf_auc_balanced, "green", "ROC Curve - Random Forest")
plot_ROC_and_AUC(dt_pred_obj_balanced, dt_auc_balanced, "purple", "ROC Curve - Decision Tree")

# Define a function to plot the ROC curve and AUC for a given model
plot_ROC_and_AUC <- function(pred_obj, auc, col, title){
  perf <- performance(pred_obj, measure = "tpr", x.measure = "fpr")
  plot(perf, col = col, lwd = 2, main = title)
  abline(a = 0, b = 1, lty = 2)
  text(x = 0.5, y = 0.5, labels = paste("AUC =", round(auc, 3)), cex = 1.5)
}


#----------------Hyper Tuned machine learning models - balance data-----------------------------------------

#random forest
library(caret)
set.seed(123)
rf_model_tune <- train(Diabetes_binary ~ ., data = train_balanced, method = "rf", tuneLength = 05, trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))

# Make predictions on the test data
test_preds <- predict(rf_model_tune, newdata = test_balanced)

# Make predictions on the train data
train_preds <- predict(rf_model_tune, newdata = train_balanced)

# Print the confusion matrix and overall accuracy on the test data
confusionMatrix(test_preds, test_balanced$Diabetes_binary)
cat("Overall accuracy on the test data: ", mean(test_preds == test_balanced$Diabetes_binary), "\n")


#decision tree
# Build the decision tree model with entropy
dt_model_tune = rpart(Diabetes_binary ~ ., train_balanced, method = "class", parms = list(split = "information"), 
                 control = rpart.control(minsplit = 1, minbucket = 2, cp = 0.00002))

# Build a bagged ensemble of decision trees
bag_model <- randomForest(Diabetes_binary ~ ., data = train_balanced, mtry = sqrt(ncol(train) - 1),
                          ntree = 100, replace = TRUE)

# Predict on the test set using the original DT model
dt_preds <- predict(dt_model_tune, test_balanced, type = "class")

# Predict on the test set using the bagged ensemble of DT models
bag_preds <- predict(bag_model, test_balanced)

# Evaluate the performance of the original DT model and the bagged ensemble
dt_acc <- mean(dt_preds == test_balanced$Diabetes_binary)
bag_acc <- mean(bag_preds == test_balancedt$Diabetes_binary)

# Print the results
cat("Accuracy of the original DT model: ", round(dt_acc, 4), "\n")
cat("Accuracy of the bagged ensemble of DT models: ", round(bag_acc, 4), "\n")


# ~~~~~~~~~~~~~~~~~~~~~~ Grid Search - Decision Tree~~~~~~~~~~~~~~~~~~~~~~ 
# Create a tuning grid for the "cp" parameter
cp_grid <- expand.grid(.cp = seq(0, 0.1, 0.001))

# Define the train control settings
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, 
                     repeats = 1, 
                     verboseIter = TRUE)

# Train the model with grid search
dt_model_grid <- train(Diabetes_binary ~ ., 
                       data = train_balanced, 
                       method = "rpart", 
                       trControl = ctrl, 
                       tuneGrid = cp_grid, 
                       tuneLength = nrow(cp_grid),
                       control = rpart.control(minsplit = 1,minbucket=2))

# Make predictions on test data using the tuned model
dt_pred_grid <- predict(dt_model_grid, newdata = test_balanced[, -15], type = "raw")

# Evaluate the model
confusionMatrix(dt_pred_grid, test_balanced$Diabetes_binary)

#-----------------------------------Grid Search -Naive Baye--------------------------------

# Fit the Naive Bayes model with adjusted smoothing parameter
nb_model_smooth <- naiveBayes(Diabetes_binary ~ ., data = train_balanced, laplace = 0.5)

# Evaluate the model on the test set
test_preds_smooth <- predict(nb_model_smooth, newdata = test_balanced[, -15])
confusionMatrix(test_preds_smooth, test_balanced$Diabetes_binary)

#------------------------Grid Search - Logisitic Regression---------------------------------------
# Define the tuning grid
grid <- expand.grid(intercept=c(TRUE, FALSE),
                    penalty=c("none", "l1", "l2"),
                    cost=c(0.01, 0.1, 1, 10, 100))

# Define cross-validation parameters
ctrl <- trainControl(method="cv", number=5)

# Train the logistic regression model with grid search
lr_grid <- train(Diabetes_binary ~ ., data=train, method="glm", trControl=ctrl, tuneGrid=grid)

# Predict on the test set
logic_pred <- predict(logic_pred, newdata = test_balanced[, -15])


# Compute confusion matrix and other performance metrics
nb_cm <- confusionMatrix(nb_pred, test_balanced$Diabetes_binary)
nb_cm

#-------------------------tune logistic regression-------------------------

diabetes_split <- initial_split(df3_balanced_new, prop = 0.80)
diabetes_train <- training(diabetes_split)
diabetes_test  <- testing(diabetes_split)

diabetes_cv <- vfold_cv(diabetes_train, v = 5)

# Define preprocessing recipe
model_recipe <- recipe(Diabetes_binary ~ ., data = diabetes_train) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

## Logistic Regression
log_model <- 
  logistic_reg(mode = "classification",
               penalty = tune(),
               mixture = tune(),
               engine = "glmnet")

log_wf <-
  workflow() %>%
  add_model(log_model) %>% 
  add_recipe(model_recipe)
log_wf


log_results <-
  log_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(yardstick::accuracy)
  )

log_results %>%
  collect_metrics()

# Final Hyperparameter
param_final <- log_results %>%
  select_best(metric = "accuracy")

log_wf <- log_wf %>%
  finalize_workflow(param_final)
log_wf

## Last Fit
log_fit <- log_wf %>%
  last_fit(diabetes_split)

## Test Data Predictions
test_performance <- log_fit %>% collect_predictions()

## Performance Metrics
diabetes_metrics <- metric_set(yardstick::accuracy, 
                               yardstick::f_meas, 
                               yardstick::precision, 
                               yardstick::recall)
diabetes_metrics(data = test_performance, truth = Diabetes_binary, estimate = .pred_class)

## Confusion Matrix
conf_mat(test_performance, Diabetes_binary, .pred_class)
log_pred <- predict(log_fit, diabetes_test, type = "prob")
log_pred_class <- ifelse(log_pred$Diabetes_binary.Yes > 0.5, "Yes", "No")




