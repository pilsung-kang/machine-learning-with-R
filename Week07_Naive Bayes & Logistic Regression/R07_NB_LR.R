# Performance Evaluation Function -----------------------------------------
perf_eval <- function(cm){
  
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

# Split the data into the training/validation sets
trn_idx <- sample(1:dim(ploan_data)[1], round(0.7*dim(ploan_data)[1]))
ploan_trn <- ploan_data[trn_idx,]
ploan_val <- ploan_data[-trn_idx,]

# Training the Naive Bayesian Classifier
nb_model <- naiveBayes(ploan_target ~ ., data = ploan_trn)
nb_model$apriori
nb_model$tables

# Predict the new input data based on Naive Bayesian Classifier
posterior = predict(nb_model, ploan_val, type = "raw")
nb_prey = predict(nb_model, ploan_val, type ="class")

# Generate a confusion matrix
cfmatrix <- table(ploan_val$ploan_target, nb_prey)

# Evaluate the performance
perf_mat <- matrix(0, 4, 3)

perf_mat[,1] <- perf_eval(cfmatrix)

# Logistic Regression -----------------------------------------------------
# Conduct the normalization
ploan_input <- scale(ploan_input, center = TRUE, scale = TRUE)
ploan_target <- as.numeric(ploan_target)-1
ploan_data <- data.frame(ploan_input, ploan_target)

ploan_trn <- ploan_data[trn_idx,]
ploan_val <- ploan_data[-trn_idx,]

# Train the Logistic Regression Model with all variables
full_lr <- glm(ploan_target ~ ., family=binomial, ploan_trn)
full_lr
summary(full_lr)

# Train the Logistic Regression Model with selected variables
reduced_lr <- step(full_lr, direction = "backward")
summary(reduced_lr)

# Evaluate the logistic regression performance on the validation data
# Case 1: full model
full_response <- predict(full_lr, type = "response", newdata = ploan_val)
full_target <- ploan_val$ploan_target
full_predicted <- rep(0, length(full_target))
full_predicted[which(full_response >= 0.5)] <- 1
cm_full <- table(full_target, full_predicted)

perf_mat[,2] <- perf_eval(cm_full)

# Case 2: reduced model
reduced_response <- predict(reduced_lr, type = "response", newdata = ploan_val)
reduced_target <- ploan_val$ploan_target
reduced_predicted <- rep(0, length(reduced_target))
reduced_predicted[which(reduced_response >= 0.5)] <- 1
cm_reduced <- table(reduced_target, reduced_predicted)

perf_mat[,3] <- perf_eval(cm_reduced)
colnames(perf_mat) <- c("Naive Bayes", "LR with all variables", "LR with selected variables")
rownames(perf_mat) <- c("TPR", "TNR", "ACC", "BCR")
perf_mat
