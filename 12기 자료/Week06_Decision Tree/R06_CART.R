# Classification and Regression Tree (CART) -------------------------------
# Personal Loan Prediction
ploan <- read.csv("Personal Loan.csv")

# For CART
install.packages("party")
library(party)

# For AUROC
install.packages("ROCR")
library(ROCR)

ploan.x <- ploan[,-c(1,5,10)]
ploan.y <- as.data.frame(as.factor(ploan[,10]))

set.seed(12345)
trn_idx <- sample(1:dim(ploan.y)[1], round(0.7*dim(ploan.y)[1]))

ploan.trn <- cbind(ploan.x[trn_idx,], ploanYN = ploan.y[trn_idx,])
ploan.val <- cbind(ploan.x[-trn_idx,], ploanYN = ploan.y[-trn_idx,])
ploan.all <- rbind(ploan.trn, ploan.val)

# construct single tree and evaluation
# tree parameter settings
min_criterion = c(0.9, 0.95, 0.99)
min_split = c(10, 30, 50, 100)
max_depth = c(0, 10, 5)
tree_result = matrix(0,length(min_criterion)*length(min_split)*length(max_depth),9)

iter_cnt = 1

for (i in 1:length(min_criterion))
{
  for ( j in 1:length(min_split))
  {
    for ( k in 1:length(max_depth))
    {
      
      cat("CART Min criterion:", min_criterion[i], ", Min split:", min_split[j], ", Max depth:", max_depth[k], "\n")
      tmp_control = ctree_control(mincriterion = min_criterion[i], minsplit = min_split[j], maxdepth = max_depth[k])
      tmp_tree <- ctree(ploanYN ~ ., data = ploan.trn, controls = tmp_control)
      tmp_tree_val_prediction <- predict(tmp_tree, newdata = ploan.val)
      tmp_tree_val_response <- treeresponse(tmp_tree, newdata = ploan.val)
      tmp_tree_val_prob <- 1-unlist(tmp_tree_val_response, use.names=F)[seq(1,nrow(ploan.val)*2,2)]
      tmp_tree_val_rocr <- prediction(tmp_tree_val_prob, ploan.val$ploanYN)
      
      tmp_tree_val_cm <- table(ploan.val$ploanYN, tmp_tree_val_prediction)
      
      # parameters
      tree_result[iter_cnt,1] = min_criterion[i]
      tree_result[iter_cnt,2] = min_split[j]
      tree_result[iter_cnt,3] = max_depth[k]
      # Recall
      Recall = tmp_tree_val_cm[2,2]/(tmp_tree_val_cm[2,1]+tmp_tree_val_cm[2,2])
      tree_result[iter_cnt,4] = Recall
      # Precision
      Precision <- tmp_tree_val_cm[2,2]/(tmp_tree_val_cm[1,2]+tmp_tree_val_cm[2,2])
      tree_result[iter_cnt,5] = Precision
      # Accuracy
      tree_result[iter_cnt,6] = (tmp_tree_val_cm[1,1]+tmp_tree_val_cm[2,2])/sum(tmp_tree_val_cm)
      # F1 measure
      tree_result[iter_cnt,7] = 2*Recall*Precision/(Recall+Precision)
      # AUROC
      tree_result[iter_cnt,8] = unlist(performance(tmp_tree_val_rocr, "auc")@y.values)
      # Number of leaf nodes
      tree_result[iter_cnt,9] = length(nodes(tmp_tree, unique(where(tmp_tree))))
      iter_cnt = iter_cnt + 1
    }
  }
}

# Find the best set of parameters
tree_result
tree_result <- tree_result[order(tree_result[,8], decreasing = T),]
best_criterion <- tree_result[1,1]
best_split <- tree_result[1,2]
best_depth <- tree_result[1,3]

# Construct the best tree
tree_control = ctree_control(mincriterion = best_criterion, minsplit = best_split, maxdepth = best_depth)
tree <- ctree(ploanYN ~ ., data = ploan.all, controls = tree_control)
tree_all_prediction <- predict(tree, newdata = ploan.all)
tree_all_response <- treeresponse(tree, newdata = ploan.all)
tree_all_prob <- 1-unlist(tree_all_response, use.names=F)[seq(1,nrow(ploan.all)*2,2)]
tree_all_rocr <- prediction(tree_all_prob, ploan.all$ploanYN)

# Performance of the best tree
tree_all_cm <- table(ploan.all$ploanYN, tree_all_prediction)
best_result <- matrix(0,1,6)
# Recall
Recall = tree_all_cm[2,2]/(tree_all_cm[2,1]+tree_all_cm[2,2])
best_result[1,1] = Recall
# Precision
Precision <- tree_all_cm[2,2]/(tree_all_cm[1,2]+tree_all_cm[2,2])
best_result[1,2] = Precision
# Accuracy
best_result[1,3] = (tree_all_cm[1,1]+tree_all_cm[2,2])/sum(tree_all_cm)
# F1 measure
best_result[1,4] = 2*Recall*Precision/(Recall+Precision)
# AUROC
best_result[1,5] = unlist(performance(tree_all_rocr, "auc")@y.values)
# Number of leaf nodes
best_result[1,6] = length(nodes(tree, unique(where(tree))))

# Plot the ROC
tmp <- 1-unlist(tree_all_response, use.names=F)[seq(1,nrow(ploan.all)*2,2)]
tmp.rocr <- prediction(tmp, ploan.all$ploanYN) 
tmp.perf <- performance(tmp.rocr, "tpr","fpr") 
plot(tmp.perf, col=5, lwd = 3)

# Plot the best tree
plot(tree)
plot(tree, type="simple")

# Print rules
print(tree)
