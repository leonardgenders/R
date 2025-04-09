'''
# Title: Principal Component Analysis - USArrests from ISLRv2
# Author: Leo Genders
'''

# Perform PCA on USArrests data set from base R package
summary(USArrests) # 3x as many rapes as murders

USArrests <- USArrests[, -1]  # Remove the first column


# rownames
states <- row.names(USArrests)
states # view

# Check Col names
names(USArrests) # rownames, Murder, Assault, UrbanPop, Rape

# check variance
apply(USArrests, 2, var) # UrbanPop is percentage of population living in an urban area, rapes is num rapes per 100k individuals



str(USArrests)
USArrests[] <- lapply(USArrests, function(x) as.numeric(as.character(x)))

# standardize
pr.out <- prcomp(USArrests, scale = TRUE)
# view
names(pr.out)
pr.out$center # prior to scaling
pr.out$scale # after scaling

# rotation matrix for principal component loadings, each col contains the 
# corresponding principal component loading vector - gives the coordinates 
# of the data in the rotated coordinate system, coords are PC scores
pr.out$rotation # shows four distinct PCs

# plot the first two principal components
biplot(pr.out, scale = 0)

# PC are only unique up to a sign change
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)

# access the stdevs
pr.out$sdev

# extract variance explained by each PC by squaring 
pr.var <- pr.out$sdev^2
pr.var

# compute portion of variance explained by each PC by dividing the variance explained
# by each Pc by the total variance explained by all four PCs
pve <- pr.var / sum(pr.var)
pve
# first PC explains 62.0% of the variance, next PC 24.7% and so on

# Plot the Proportion of Variance Explained (PVE)
par(mfrow = c(1,2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')
