'''
# Title: Principal Component Analysis and Clustering on NC160 (Gene Expression Measurements)
# Author: Leo Genders
'''

# bring in the ISLR2 data
library(ISLR2)
nci.labs <- NCI60$labs # each line is a cancer type
nci.data <- NCI60$data

dim(nci.data) # 64 rows and 6830 cols

# explore the cancer types for the cell lines
nci.labs[1:4]
table(nci.labs)

# perform PCA on the data after scaling the variables (genes) to have stdev 1 
pr.out <- prcomp(nci.data, scale = TRUE)

# Plot the first few PC score vectors to visualize
Cols <- function(vec) {
  cols <- rainbow(length(unique(vec))) # assign a distinct color to each element
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1,2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab ='Z2')
plot(pr.out$x[, c(1, 3)], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z2")
# Projections of the NCI60 cancer cell lines onto the first three PCs (first PC scores)
# Without dim reduction, (6830 2) possible scatterplots would have made this analysis
# almost useless

# plot the PC score vectors now that we can see obs belonging to a single cancer type 
# tend to lie near each other in the low-dimensional space above
summary(pr.out)

# plot the variance explained by the first few PCs
plot(pr.out)

# Plot the Portion of Variance Explained of the PCs of the NCI60 cancel cell line microarray dataset
pve <- 100 * pr.out$sdev^2 / sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve, type = "o", ylab = "PVE",
     xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", 
     xlab = "Principal Component", col = "brown3")


# Cluster observations of the NCI60 Data
sd.data <- scale(nci.data)

# hierarchical clustering of the observations using complete, single, and avg linkage
# rememebr that Euclidean distance is used as the dissimilarity measure
par(mfrow = c(1,3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",
     labels = nci.labs, main = "Complete Linkage")
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",
     labels = nci.labs, main = "Average Linkage")
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",
     labels = nci.labs, main = "Single Linkage")

# Cut the dendrogram at the hieght that yields 4 clusters
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)
# all the leukemia cell lines are in cluster 3 and breast cancer are across three
# different clusters

# plot the dendrogram that produces the four clusters
par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red") # abline() draws a straight line on top of any 
# existing plot in R and h = 139 plots a horizontal line at hieght 139 on the dendrogram

# Get summary of hclust
hc.out


# Now try k-Means with K=4 clusters
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)
# cluster 4 in k-means clustering is identical to cluster 3 in hierarchical clustering
# cluster 2 in k-means clustering contains a portion of the obs assigned to cluster 1
# in hierarchical clustering as well as all the obs assigned to cluster 2 by HC

# Perform HC on first few PC score vectors
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs,
     main = "Hier. Clust. on First 5 Score Vectors")
table(cutree(hc.out, 4, nci.labs))
