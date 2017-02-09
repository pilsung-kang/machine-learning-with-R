# Part 1: Classification with Single Model -----------------------------------------

# Performance Evaluation Function -----------------------------------------
perf_eval <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

Perf.Table <- matrix(0, nrow = 6, ncol = 6)
rownames(Perf.Table) <- c("ANN", "CART", "Bagging ANN", "AdaBoost", "GBM", "Random Forests")
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

# Model 1: Artificial Neural Network -----------------------------------------------
# nnet package install
install.packages("nnet", dependencies = TRUE)
library(nnet)

# Load the data & Preprocessing
Ploan <- read.csv("Personal Loan.csv")
input.idx <- c(2,3,4,6,7,8,9,11,12,13,14)
target.idx <- 10

Ploan.input <- Ploan[,input.idx]
Ploan.input.scaled <- scale(Ploan.input, center = TRUE, scale = TRUE)
Ploan.target <- as.factor(Ploan[,target.idx])

Ploan.data.scaled <- data.frame(Ploan.input.scaled, Ploan.target)

trn.idx <- 1:1500
tst.idx <- 1501:2500

# Input/Target configuration
ANN.trn.input <- Ploan.input.scaled[trn.idx,]
ANN.trn.target <- class.ind(Ploan.target[trn.idx])

ANN.tst.input <- Ploan.input.scaled[tst.idx,]
ANN.tst.target <- class.ind(Ploan.target[tst.idx])

# Trainin ANN
ANN.model <- nnet(ANN.trn.input, ANN.trn.target, size = 14, decay = 5e-4, maxit = 300)

# Performance evaluation
ANN.prey <- predict(ANN.model, ANN.tst.input)
ANN.cfm <- table(max.col(ANN.tst.target), max.col(ANN.prey))

Perf.Table[1,] <- perf_eval(ANN.cfm)
Perf.Table

# Model 2: Classification Tree -----------------------------------------------
install.packages("party")
library(party)

CART.trn <- data.frame(Ploan.input[trn.idx,], PloanYN = Ploan.target[trn.idx])
CART.tst <- data.frame(Ploan.input[tst.idx,], PloanYN = Ploan.target[tst.idx])

# CART parameters
tree.control = ctree_control(mincriterion = 0.95, minsplit = 10, maxdepth = 0)

# Training the tree
CART.model <- ctree(PloanYN ~ ., data = CART.trn, controls = tree.control)

# Prediction
CART.prey <- predict(CART.model, newdata = CART.tst)
CART.cfm <- table(CART.tst$PloanYN, CART.prey)

Perf.Table[2,] <- perf_eval(CART.cfm)
Perf.Table

# Part 2: Classification with Ensemble Models -----------------------------

# Model 3: Bagging with Neural Network ------------------------------------
# 멀티코어 CPU를 사용하여 작업 수행가능 
install.packages("caret")
install.packages("doParallel")

library(caret)
library(doParallel)

# Multi-core의 경우 parallel training이 가능
cl <- makeCluster(8)
registerDoParallel(cl)

# Bagging Training
ptm <- proc.time()
Bagging.ANN.model <- avNNet(ANN.trn.input, ANN.trn.target, size = 14, decay = 5e-4,
                      repeats = 100, bag = TRUE, allowParallel = TRUE, trace = TRUE)
Bagging.Time <- proc.time() - ptm

# Bagging Test
Bagging.ANN.prey <- predict(Bagging.ANN.model, newdata = ANN.tst.input)
Bagging.ANN.cfm <- table(max.col(ANN.tst.target), max.col(Bagging.ANN.prey))

Perf.Table[3,] <- perf_eval(Bagging.ANN.cfm)
Perf.Table

# Model 4: AdaBoost with Stump Tree ---------------------------------------
install.packages("ada")
library(ada)

AdaBoost.trn <- CART.trn
AdaBoost.tst <- CART.tst

# Training AdaBoost with Stump Tree (Tree with 1 depth)
ptm <- proc.time()
AdaBoost.model <- ada(AdaBoost.trn[,1:11], AdaBoost.trn[,12], loss = "exponential", 
                      iter = 100, bag.frac = 0.5, verbose = TRUE)
Boosting.Time <- proc.time() - ptm

print(AdaBoost.model)

# Prediction
AdaBoost.prey <- predict(AdaBoost.model, AdaBoost.tst[,1:11])
AdaBoost.cfm <- table(AdaBoost.tst$PloanYN, AdaBoost.prey)

Perf.Table[4,] <- perf_eval(AdaBoost.cfm)
Perf.Table

# Model 5: Gradient Boosting Machine --------------------------------------
install.packages("gbm")
library(gbm)

GBM.trn <- data.frame(Ploan.input[trn.idx,], PloanYN = Ploan[trn.idx,target.idx])
GBM.tst <- data.frame(Ploan.input[tst.idx,], PloanYN = Ploan[tst.idx,target.idx])

# Training the GBM
ptm <- proc.time()
GBM.model <- gbm.fit(GBM.trn[,1:11], GBM.trn[,12], distribution = "bernoulli", 
                 n.trees = 1000, shrinkage = 0.02, bag.fraction = 0.8, nTrain = 1000)
GBM.Time <- proc.time() - ptm

summary(GBM.model)

# Prediction
GBM.prey <- predict(GBM.model, GBM.tst[,1:11], type = "response")
GBM.prey <- round(GBM.prey)
GBM.cfm <- table(GBM.prey, GBM.tst$PloanYN)

Perf.Table[5,] <- perf_eval(GBM.cfm)
Perf.Table

# Model 6: Random Forest --------------------------------------------------
install.packages("randomForest")
library(randomForest)

RF.trn <- CART.trn
RF.tst <- CART.tst

# Training the Random Forest
ptm <- proc.time()
RF.model <- randomForest(PloanYN ~ ., data = RF.trn, ntree = 100, importance = TRUE, do.trace = TRUE)
RF.Time <- proc.time() - ptm

# Check the result
print(RF.model)
plot(RF.model)

# 변수의 중도도 산출 및 그래프 도시
Var.imp <- importance(RF.model)
barplot(Var.imp[order(Var.imp[,4], decreasing = TRUE),4])

# Prediction
RF.prey <- predict(RF.model, newdata = RF.tst, type = "class")
RF.cfm <- table(RF.prey, RF.tst$PloanYN)

Perf.Table[6,] <- perf_eval(RF.cfm)
Perf.Table
