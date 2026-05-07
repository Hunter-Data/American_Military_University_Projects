
# Telecom Customer Churn Analysis
#install.packages("FactoMineR")

# Load required libraries
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(factoextra)
library(FactoMineR)


# 1. Data Loading and Exploration --------------------------------------------

data <- read.csv("~/AMU/Dats201/Week 8/Telecom_Customer_Churn_Dataset.csv")
str(data)
summary(data)
head(data)
colnames(data)
colSums(is.na(data))

data$CustomerID <- NULL

# Convert categorical variables to factors
data$HasTechSupport <- as.factor(data$HasTechSupport)
data$IsSeniorCitizen <- as.factor(data$IsSeniorCitizen)
data$Churn <- as.factor(data$Churn)
data$NumOfProducts <- as.factor(data$NumOfProducts)

# Data Visualization
ggplot(data, aes(x=Churn, fill=Churn)) + 
  geom_bar() + 
  ggtitle("Churn Distribution")

ggplot(data, aes(x=MonthlyCharges, fill=Churn)) +
  geom_density(alpha=0.5) +
  ggtitle("Monthly Charges by Churn Status")

numeric_vars <- c("Age",
                  "AnnualIncome",
                  "MonthlyCharges",
                  "TotalCharges",
                  "TenureMonths")

data %>%
  pivot_longer(cols = all_of(numeric_vars), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = Churn, y = value, fill = Churn)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Numeric Variables by Churn Status",
       x = "Churn",
       fill = "Churn Status")


# 2. Predictive Modeling --------------------------------------------------

# Split data
set.seed(1)
trainIndex <- createDataPartition(data$Churn, p=0.7, list=FALSE)
train <- data[trainIndex,]
test <- data[-trainIndex,]

# Logistic Regression
logit_model <- glm(Churn ~ ., data=train, family="binomial")
summary(logit_model)

# Decision Tree
tree_model <- rpart(Churn ~ ., data=train, method="class")
rpart.plot(tree_model, main="Decision Tree for Customer Churn")


# Resampling Methods ------------------------------------------------------

# Set up cross-validation
ctrl <- trainControl(method = "cv",
                     number = 10,
                     savePredictions = TRUE,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# Logistic Regression with CV
logit_cv <- train(Churn ~ ., data=train,
                  method = "glm",
                  family = "binomial",
                  trControl = ctrl,
                  metric = "ROC")
print(logit_cv)

# Decision Tree with CV
tree_cv <- train(Churn ~ ., data=train,
                 method = "rpart",
                 trControl = ctrl,
                 tuneLength = 10,
                 metric = "ROC")
print(tree_cv)
plot(tree_cv, main="Decision Tree Tuning Results")

# Model comparison
models <- list(Logistic = logit_cv, DecisionTree = tree_cv)
resamples <- resamples(models)
summary(resamples)
bwplot(resamples, metric = "ROC", main="Model Comparison by ROC")


# Unsupervised Learning ---------------------------------------------------

# PCA Analysis
pca_data <- data %>% select(all_of(numeric_vars)) %>% scale()
pca_result <- PCA(pca_data, graph=FALSE)

# PCA Visualization
fviz_eig(pca_result, addlabels=TRUE, main="Scree Plot - Variance Explained")
fviz_pca_var(pca_result, col.var="contrib",
             gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=TRUE, title="Variable Contributions to PCs")

# K-means Clustering with PCA components
pca_scores <- as.data.frame(pca_result$ind$coord)
colnames(pca_scores) <- paste0("PC", 1:ncol(pca_scores))

# Determine optimal clusters
fviz_nbclust(pca_scores, kmeans, method="wss")

# Perform clustering
set.seed(123)
kmeans_result <- kmeans(pca_scores, centers=3, nstart=25)
data$Cluster <- as.factor(kmeans_result$cluster)

# Visualize clusters
fviz_cluster(kmeans_result, data=pca_scores, 
             geom="point",
             ellipse.type="convex",
             palette="jco",
             main="Customer Segments from PCA")


# Analysis and Insight ----------------------------------------------------

# Cluster analysis
cluster_summary <- data %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    ChurnRate = mean(Churn == "Yes"),
    AvgMonthlyCharge = mean(MonthlyCharges),
    AvgTenure = mean(TenureMonths),
    AvgAge = mean(Age),
    AvgIncome = mean(AnnualIncome)
  )

print(cluster_summary)

# Key churn factors
varImp(logit_cv)
varImp(tree_cv)




































































































