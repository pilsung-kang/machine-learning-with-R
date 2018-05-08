# Performance Evaluation Function -----------------------------------------
perf_eval2 <- function(cm){
  
  # True positive rate: TPR
  TPR = cm[2,2]/sum(cm[2,])
  # True negative rate: TNR
  TNR = cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC = (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR = sqrt(TPR*TNR)
  
  return(c(TPR, TNR, ACC, BCR))
}

perf_eval3 <- function(cm){
  
  # Simple accuracy
  ACC <- sum(diag(cm))/sum(cm)
  
  # ACC for each class
  A1 <- cm[1,1]/sum(cm[1,])
  A2 <- cm[2,2]/sum(cm[2,])
  A3 <- cm[3,3]/sum(cm[3,])
  BCR <- (A1*A2*A3)^(1/3)
  
  return(c(ACC, BCR))
}

# Part 1: Binary Classification
# Naive Bayesian Classifier -----------------------------------------------
# e1071 package install
install.packages("e1071", dependencies = TRUE)

# Call the e1071 package
library(e1071)

ploan <- read.csv("Personal Loan.csv")

input_idx <- c(2,3,4,6,7,8,9,11,12,13,14)
target_idx <- 10

ploan_input <- ploan[,input_idx]
ploan_target <- as.factor(ploan[,target_idx])
ploan_data <- data.frame(ploan_input, ploan_target)

# Initialize the performance matrix
perf_mat <- matrix(0, 4, 3)
colnames(perf_mat) <- c("Naive Bayes", "LR with all variables", "LR with selected variables")
rownames(perf_mat) <- c("TPR", "TNR", "ACC", "BCR")

# Split the data into the training/validation sets
set.seed(12345)
trn_idx <- sample(1:dim(ploan_data)[1], round(0.7*dim(ploan_data)[1]))
ploan_trn <- ploan_data[trn_idx,]
ploan_tst <- ploan_data[-trn_idx,]

# Training the Naive Bayesian Classifier
nb_model <- naiveBayes(ploan_target ~ ., data = ploan_trn)
nb_model$apriori
nb_model$tables

# Predict the new input data based on Naive Bayesian Classifier
posterior = predict(nb_model, ploan_tst, type = "raw")
nb_prey = predict(nb_model, ploan_tst, type ="class")

# Generate a confusion matrix
cfmatrix <- table(ploan_tst$ploan_target, nb_prey)

# Evaluate the performance
perf_mat[,1] <- perf_eval2(cfmatrix)
perf_mat

# Logistic Regression -----------------------------------------------------
# Conduct the normalization
ploan_input <- scale(ploan_input, center = TRUE, scale = TRUE)
ploan_target <- as.numeric(ploan_target)-1
ploan_data <- data.frame(ploan_input, ploan_target)

ploan_trn <- ploan_data[trn_idx,]
ploan_tst <- ploan_data[-trn_idx,]

# Train the Logistic Regression Model with all variables
full_lr <- glm(ploan_target ~ ., family=binomial, ploan_trn)
summary(full_lr)

# Train the Logistic Regression Model with selected variables
reduced_lr <- step(full_lr, direction = "backward")
summary(reduced_lr)

# Evaluate the logistic regression performance on the validation data
# Case 1: full model
full_response <- predict(full_lr, type = "response", newdata = ploan_tst)
full_target <- ploan_tst$ploan_target
full_predicted <- rep(0, length(full_target))
full_predicted[which(full_response >= 0.5)] <- 1
cm_full <- table(full_target, full_predicted)

perf_mat[,2] <- perf_eval2(cm_full)

# Case 2: reduced model
reduced_response <- predict(reduced_lr, type = "response", newdata = ploan_tst)
reduced_target <- ploan_tst$ploan_target
reduced_predicted <- rep(0, length(reduced_target))
reduced_predicted[which(reduced_response >= 0.5)] <- 1
cm_reduced <- table(reduced_target, reduced_predicted)

perf_mat[,3] <- perf_eval2(cm_reduced)
perf_mat

# Part 2: Multi-class classification
wine <- read.csv("wine.csv")

# Initialize the performance matrix
perf_mat_wine <- matrix(0, 2, 2)
colnames(perf_mat_wine) <- c("Naive Bayes", "Multinomial Logistic Regression")
rownames(perf_mat_wine) <- c("ACC", "BCR")

# Naive Bayes
wine$Class <- as.factor(wine$Class)

# Split the data into the training/validation sets
set.seed(12345)
trn_idx <- sample(1:dim(wine)[1], round(0.7*dim(wine)[1]))
wine_trn <- wine[trn_idx,]
wine_tst <- wine[-trn_idx,]

# Training the Naive Bayesian Classifier
nb_model <- naiveBayes(Class ~ ., data = wine_trn)
nb_model$apriori
nb_model$tables

# Predict the new input data based on Naive Bayesian Classifier
posterior = predict(nb_model, wine_tst, type = "raw")
options(scipen=10)
posterior[1:10,]

nb_prey = predict(nb_model, wine_tst, type ="class")

# Generate a confusion matrix
cfmatrix <- table(wine_tst$Class, nb_prey)
cfmatrix
perf_mat_wine[,1] <- perf_eval3(cfmatrix)
perf_mat_wine

# Multinomial logistic regression
install.packages("nnet")
library(nnet)

# Define the baseline class
wine$Class <- relevel(wine$Class, ref = "3")
wine_trn <- wine[trn_idx,]
wine_tst <- wine[-trn_idx,]

# Train multinomial logistic regression
ml_logit <- multinom(Class ~ ., data = wine_trn)

# Check the coefficients
summary(ml_logit)
t(summary(ml_logit)$coefficients)

# Conduct 2-tailed z-test to compute the p-values
z_stats <- summary(ml_logit)$coefficients/summary(ml_logit)$standard.errors
t(z_stats)

p_value <- (1-pnorm(abs(z_stats), 0, 1))*2
options(scipen=10)
t(p_value)
cbind(t(summary(ml_logit)$coefficients), t(p_value))

# Predict the class probability
ml_logit_haty <- predict(ml_logit, type="probs", newdata = wine_tst)
ml_logit_haty[1:10,]

# Predict the class label
ml_logit_prey <- predict(ml_logit, newdata = wine_tst)

cfmatrix <- table(wine_tst$Class, ml_logit_prey)
cfmatrix
perf_mat_wine[,2] <- perf_eval3(cfmatrix)
perf_mat_wine

