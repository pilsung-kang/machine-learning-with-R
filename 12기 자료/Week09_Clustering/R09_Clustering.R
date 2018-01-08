# Package for cluster validity
install.packages("clValid")
install.packages("plotrix")

library(clValid)
library(plotrix)

# Load the Iris dataset
data(iris)

# Part 1: K-Means Clustering ----------------------------------------------
# Remove the class label
newiris <- iris
newiris$Species <- NULL
rownames(newiris) <- paste("I", 1:150, sep = "_")
  
# Perform K-Means Clustering with K=3
kc <- kmeans(newiris,3)

str(kc)
kc$centers
kc$size
kc$cluster

# Compare the assigned clusters and the Species
table(iris$Species, kc$cluster)

plot(newiris[,c("Sepal.Length", "Sepal.Width")], col = kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)

# Perform K-Means Clustering with K=5
kc <- kmeans(newiris,5)

# Compare the assigned clusters and the Species
table(iris$Species, kc$cluster)

plot(newiris[,c("Sepal.Length", "Sepal.Width")], col = kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:5, pch = 8, cex=2)

# Evaluating the cluster validity measures
newiris.clValid <- clValid(newiris, 2:10, clMethods = "kmeans", 
                           validation = c("internal", "stability"))
summary(newiris.clValid)

# Part 2: Hierarchical Clustering -----------------------------------------
ploan <- read.csv("Personal Loan.csv")
ploan.x <- ploan[,-c(1,5,10)]
ploan.x.scaled <- scale(ploan.x, center = TRUE, scale = TRUE)

# Compute the similarity using the spearman coefficient
cor.Mat <- cor(t(ploan.x.scaled), method = "spearman")
dist.ploan <- as.dist(1-cor.Mat)

# Perform hierarchical clustering
hr <- hclust(dist.ploan, method = "complete", members=NULL)

# plot the results
plot(hr)
plot(hr, hang = -1)
plot(as.dendrogram(hr), edgePar=list(col=3, lwd=4), horiz=T)

# Find the clusters
mycl <- cutree(hr, k=10)
mycl

plot(hr)
rect.hclust(hr, k=10, border="red")


# Compare each cluster for HC
cluster.hc <- data.frame(ploan.x.scaled, ploanYN = ploan[,10], 
                         clusterID = as.factor(mycl))
hc.summary <- data.frame()

for (i in 1:(ncol(cluster.hc)-1)){
  hc.summary = rbind(hc.summary, 
                     tapply(cluster.hc[,i], cluster.hc$clusterID, mean))
}

colnames(hc.summary) <- paste("cluster", c(1:10))
rownames(hc.summary) <- c(colnames(ploan.x), "LoanRatio")
hc.summary

# Radar chart
par(mfrow = c(2,5))
for (i in 1:10){
  plot.title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(hc.summary[,i], labels = rownames(hc.summary), 
              radial.lim=c(-2,3), rp.type = "p", main = plot.title, 
              line.col = "red", lwd = 3, show.grid.labels=1)
}
dev.off()


# Part 3: Self-Organizing Map ---------------------------------------------
# som package install
install.packages("som", dependencies = TRUE)
detach("package:kohonen", unload=TRUE)
library(som)

# Load the yeast dataset
data(yeast)
yeast <- yeast[, -c(1, 11)]
yeast <- normalize(yeast, byrow=FALSE)

# Train SOM with two different settings
som1 <- som(yeast, xdim=5, ydim=5, topol="rect", neigh="gaussian")
som2 <- som(yeast, xdim=5, ydim=5, topol="hexa", neigh="bubble")

# See the results
summary(som1)
plot(som1)
som1$visual[1:10,]

summary(som2)
plot(som2)
som2$visual[1:10,]

# kohonen package install
install.packages("kohonen", dependencies = TRUE)
detach("package:som", unload=TRUE)
library(kohonen)

data(wines)
trn = sample(nrow(wines), 120)
wines_trn <- scale(wines[trn,])
wines_tst <- scale(wines[-trn,], center = attr(wines_trn, "scaled:center"), scale = attr(wines_trn,"scaled:scale"))

som.wines <- som(wines_trn, grid = somgrid(5,5,"hexagonal"))
map(som.wines, wines_tst)
wine.cluster <- cutree(hclust(dist(som.wines$codes)), 3)

graphics.off()
par(mfrow = c(2, 2))
plot(som.wines, type = "property", property = som.wines$codes[,1], main = colnames(som.wines$codes)[1])
plot(som.wines, type = "property", property = som.wines$codes[,2], main = colnames(som.wines$codes)[2])
plot(som.wines, type = "property", property = som.wines$codes[,3], main = colnames(som.wines$codes)[3])
plot(som.wines, type = "property", property = som.wines$codes[,4], main = colnames(som.wines$codes)[4])
graphics.off()

par(mfrow = c(2, 2))
plot(som.wines, type = "quality")
plot(som.wines, type = "codes")
plot(som.wines, type = "changes")
dev.off()

som.pal <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
plot(som.wines, type="mapping", bgcol = som.pal[wine.cluster], main = "Clusters")
add.cluster.boundaries(som.wines, wine.cluster)

