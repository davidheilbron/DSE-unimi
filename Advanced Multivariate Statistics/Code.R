# UNIVERSITY OF MILAN - M.SC. DATA SCIENCE AND ECONOMICS
# ADVANCED MULTIVARIATE STATISTICS EXAM
# DAVID ALEJANDRO FERNANDEZ - 988346
# JANUARY 2023

# PROJECT TITLE: ANALYSIS OF ROAD INFRASTRUCTURE INVESTMENTS

# THE FOLLOWING PROJECT HAS THE OBJECTIVE TO EXPLORE DATA ON ROAD INFRASTRUCTURE (SAFETY, SIZE OF NETWORK, USAGE)
# AND MODEL THE FEATURE 'ROAD INFRASTRUCTURE INVESTMENT' FOR A GIVEN COUNTRY
# DATA WAS EXTRACTED ON DECEMBER 7 2022 FROM THE OECD.STAT WEB SITE

pacman::p_load("tidyverse", "janitor", "readxl", "VIM", "FactoMineR", "factoextra",
               "mice", "mclust", "MASS", "ggplot2","EFAtools", "PCAtools",
               "ggfortify", "dplyr", "ggpubr", "Hmisc", "PerformanceAnalytics",
               "corrplot", "olsrr", "flexclust", "fpc", "dbscan")

###############################################################################
# 0.DATA PRE PROCESSING

data <- read_excel("data.xlsx", range = "A4:AG49") #read file
data <- data[,-2] #drop unnecessary columns
colnames(data) <- as.character(data[1,]) #change column names
data <- data[-c(1:2),] #drop unnecessary rows
data <- data.frame(data) #set data as data frame
row.names(data) <- data[,1] #keep country names as row names
data <- data[,-1] #drop first column
data[data == ".."] <- NA #replace ".." with NA
data <- data.frame(
  lapply(data,
         function(x) as.numeric(as.character(x))),
  check.names = F,
  row.names = rownames(data)) #transform data from character to numeric
str(data) #check structure of the data

data <- data %>% clean_names() #adjust messy names
original_col_names <- colnames(data) #our variable of interest is "road_infrastructure_investment_in_constant_usd_per_inhabitant"
colnames(data) <- paste0("V", 1:ncol(data)) #our variable of interest is now called "V8"
names <- data.frame(cbind(original_col_names, colnames(data))) #create a separate object with the original features' names
colnames(names) <- c("long", "number") #change column names of the names object
names <- names %>% mutate(group = case_when(
  number == c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11") ~ "X1",
  number == c("V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22") ~ "X2",
  number == c("V23", "V24") ~ "X3",
  number == c("V25", "V26", "V27") ~ "X4",
  number == c("V28", "V29", "V30", "V31") ~ "X5",
)) #group features by its type
names <- names %>% replace_na(list(group = "X5")) #last code had an output of NA (don't know why), fixing it here
rm(original_col_names) #remove objects that are not longer needed

#final check and data summary
dim(data)
str(data)
summary(data) #we have several NAs in the data that must be dealt with


###############################################################################
# 1.DEALING WITH MISSING DATA

#describe missing data status
a <- sum(is.na(data)) #count of missing values
b <- length(data)*nrow(data) #total values
round((a/b)*100,2) #13.5% of our data are missing values

aggr(data, sortVar = TRUE) #some variables have up to 60% of missing values
matrixplot(data,sortby = 2) #another visualization of NAs
md.pattern(data) #NA pattern

data = data[,!sapply(data, function(x) mean(is.na(x))) > 0.3] #drop features with NA count > 30% of data
aggr(data, sortVar = TRUE, numbers = TRUE, ylab = c("Missing data", "Pattern")) #check results
matrixplot(data,sortby = 2) #another visualization of NAs
md.pattern(data) #pattern after removing the most 'affected' features

#Mice imputation
mice_imp <- mice(data, m = 1, method = c("pmm"), maxit = 20, seed = 2023) #for simplicity, only 1 estimation is done
summary(mice_imp)

head(mice_imp$imp) #check results
tail(mice_imp$imp) #check results

data_imp <- complete(mice_imp, 1) #create new object for imputed data
matrixplot(data_imp,sortby = 2) #imputed data pattern
str(data_imp) #check structure
summary(data_imp) #summary imputed data

rm(a,b, mice_imp) #remove objects that are not longer needed


###############################################################################
# 2. DATASET ANALYSIS

# DEPENDENT VARIABLE WILL BE SEPARATED FROM THE DATASET TO AVOID BIAS
# FIRST PART (UNSUPERVISED): MULTIDIMENSIONAL SCALING AND CLUSTERING
# SECOND PART (UNSUPERVISED): PRINCIPAL COMPONENT ANALYSIS AND CLUSTERING
# THIRD PART (SUPERVISED): FEATURE SELECTION AND MODELLING

y <- data_imp$V8 #y = road infrastructure investment per inhabitant
X <- data_imp[ ,-which(names(data_imp) == "V8")] #independent variables/features


#DATA EXPLORATION
hist.data.frame(X) #a lot of skewed data, log transformation may be useful
cor <- cor(X) 
round(cor, 2) 
round(cor(y, X),2) #really high correlation between y and V6 -- looking at them it is obvious: they are linearly dependent
chart.Correlation(cor, histogram = TRUE, pch = 19) #some high correlation among features


# FIRST PART: MULTIDIMENSIONAL SCALING AND CLUSTERING
# Because of high colinearity with y, V6 and V7 will be dropped for MDS analysis
x_scale <- scale(X)
dmatrix = dist(x_scale[,-which(names(X) == c("V6", "V7"))], method = "euclidean", diag = TRUE, upper = TRUE) #distance matrix using euclidean distance
head(as.matrix(dmatrix))
mds <- cmdscale(dmatrix, k = 2, eig = TRUE) #k=2 for interpretability
mds_points <- data.frame(mds$points)
colnames(mds_points) <- c("Dim1", "Dim2")

#plot dimensions
ggscatter(mds_points, x = "Dim1", y = "Dim2", 
          label = rownames(mds_points),
          size = 1,
          repel = TRUE) 

#to which of our original X variables are these dimensions linearly correlated?
dims_cor <- data.frame(cor(mds_points[,1], X))
dims_cor <- rbind(dims_cor, cor(mds_points[,2], X))
dims_cor <- t(dims_cor)
colnames(dims_cor) <- c("Dim 1", "Dim 2")
head(sort(dims_cor[,1])) #Dim1 is capturing the effect of V24 and V23
tail(sort(dims_cor[,1])) #Dim1 is also capturing the effect of V12 and V9
head(sort(dims_cor[,2])) #Dim2 is capturing the effect of V16
tail(sort(dims_cor[,2])) #Dim2 is also capturing some effect from V15

#are these dimensions linearly correlated with y?
cor(y,mds_points) #low correlation against both dimensions
plot(mds_points[,1], y) #it seems there' are some outliers' a relation that is not linear, there are outliers as well
plot(mds_points[,2], y) #same here


# K-MEANS CLUSTERING
fviz_nbclust(mds_points[,1:2], kmeans, method = "wss") #3 clusters could be a good choice
clust <- kmeans(mds_points, 3)$cluster %>% as.factor()
mds_points <- mds_points %>% mutate(groups = clust)

# Plot and color by groups
ggscatter(mds_points, x = "Dim1", y = "Dim2", 
          label = rownames(mds_points),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

# Do these clusters explain differences in y?
kmeans <- data.frame(cbind(y, mds_points[,"groups"]))
colnames(kmeans) <- c("y", "cluster")
ggboxplot(kmeans, x = "cluster", y = "y", 
          color = "cluster",
          order = c("1", "2", "3"),
          ylab = "road infrastructure investment per capita", xlab = "cluster")

#anova test
AOV <- aov(y ~ cluster, data = kmeans)
summary(AOV) #we do not have enough evidence to reject H0
bartlett.test(y ~ cluster, data = kmeans) #Bartlett's test indicate significant differences in the variance of the clusters

# MODEL BASED CLUSTERING USING DIM1 AND DIM2 OF MDS
# do we get different/better results by using model-based clustering?
m_clust <- mds_points[,1:2]
head(m_clust)

mc <- Mclust(m_clust)
plot(mc) #3 clusters seem to be the right choice under the VEI model
summary(mc) #2 elements in cluster number 2 suggest the presence of outliers
fviz_mclust_bic(mc) #better visualization of the model selection
fviz_mclust(mc, 'classification') #better visualization of the clusters -- Belgium and Netherlands outliers

#does this clustering explains variation in y?
mclust <- data.frame(cbind(y, mc$classification))
colnames(mclust) <- c("y", "cluster")
ggboxplot(mclust, x = "cluster", y = "y", 
          color = "cluster",
          order = c("1", "2", "3"),
          ylab = "road infrastructure investment per capita", xlab = "cluster")

#anova test
AOV <- aov(y ~ cluster, data = mclust)
summary(AOV) #it is the exact same classification as with k-means
bartlett.test(y ~ cluster, data = mclust) #H0 rejected: at least one group have a variance different than the others

#ROBUST CLUSTERING
rob_kmeans <- kcca(mds_points[.1:2], k = 3, family = kccaFamily("kmedians", trim = 0.2))
rob_clust <- data.frame(cbind(y, rob_kmeans@cluster))
colnames(rob_clust) <- c("y", "cluster") 

#Graph MDS dimensions and robust clusters
rclusters <- mds_points %>% mutate(groups = rob_clust$cluster)
rclusters[,3] <- as.factor(rclusters[,3])

# Plot and color by groups
ggscatter(rclusters, x = "Dim1", y = "Dim2", 
          label = rownames(mds_points),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

ggboxplot(rob_clust, x = "cluster", y = "y", 
          color = "cluster",
          order = c("1", "2", "3"),
          ylab = "road infrastructure investment per capita", xlab = "cluster")

AOV <- aov(y ~ cluster, data = rob_clust)
summary(AOV) #this clustering does not explain the differences in means with respect to y
bartlett.test(y ~ cluster, data = rob_clust) #H0 rejected

###############################################################################
# SECOND PART (UNSUPERVISED): PRINCIPAL COMPONENT ANALYSIS AND CLUSTERING

# Is this dataset suitable for factor analysis?
KMO(cor(scale(X))) #data is unacceptable for factor analysis, KMO score = 0.367
pca <- prcomp(X, scale = T, center = T)
pca
summary(pca) #8 or 9 components will do the job

cor(y,pca$x) #highest correlation around 44%

fviz_contrib(pca, choice = "var", axes = 1) #features that contribute the most to PC1
fviz_contrib(pca, choice = "var", axes = 2) #idem for PC2

#plots
fviz_eig(pca)
fviz_pca_ind(pca, axes = c(1,2), repel = T)
fviz_pca_var(pca, axes = c(1,2), repel = T)
fviz_pca_biplot(pca, repel = TRUE, axes = c(1,2),
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

#will work with 9 components
pc <- pca$x[,1:9]
head(pc)

# K-means clustering
rm(kmeans) #avoid conflict with the next function
fviz_nbclust(pc, kmeans, method = "wss") #4 to 6 clusters could be a good choice
clust <- kmeans(pc, 4)$cluster %>% as.factor()
pc_clust <- data.frame(cbind(pc, clust))

my_cols_4 <- c("#00AFBB", "#E7B800", "#FC4E07", "#41CF52")
my_cols_6 <- c("#00AFBB", "#E7B800", "#FC4E07", "#41CF52", "#aa41cf", "#000000")

pairs(pc_clust[,1:9],
      col = my_cols_4[pc_clust$clust],
      lower.panel = NULL) #it seems to be some clear clustering between PC1 and PC2

clust <- cbind(y, pc_clust[,"clust"])
colnames(clust) <- c("y", "cluster")
clust <- data.frame(clust)

# does this clustering explain our dependent variable?
ggboxplot(clust, x = "cluster", y = "y", 
          color = "cluster",
          order = c("1", "2", "3", "4"),
          ylab = "road infrastructure investment per capita", xlab = "cluster")

#anova test
AOV <- aov(y ~ cluster, data = clust)
summary(AOV)

#4 clusters do not explain differences
#6 clusters do explain a difference, but interpretation is hard


# Model Based clustering using PCs
dens <- densityMclust(pc)
mc <- Mclust(pc)
plot(mc)
summary(mc)
fviz_mclust_bic(mc) #2 clusters - VEI
fviz_mclust(mc, 'classification') #it doesn't make much sense

#does this clustering explains variations in y?
mclust <- data.frame(cbind(y, mc$classification))
colnames(mclust) <- c("y", "cluster")
ggboxplot(mclust, x = "cluster", y = "y", 
          color = "cluster",
          order = c("1", "2", "3"),
          ylab = "road infrastructure investment per capita", xlab = "cluster")

#anova test
AOV <- aov(y ~ cluster, data = mclust)
summary(AOV) #not enough evidence to reject H0
bartlett.test(y ~ cluster, data = mclust) #H0 rejected: at least one group have a variance different than the others



# CLUSTERING AFTER USING PRINCIPAL COMPONENTS DOESN'T WORK, WHY??? MY DATA IS NOT LINEARLY CORRELATED

########
# DBSCAN WITH MDS
db <- dbscan(mds_points[,1:2], eps = 0.7, MinPts = 3)
print(db)
fviz_cluster(db, mds_points[,1:2], stand = FALSE, geom = "point")
db$cluster
test <- data.frame(cbind(y, db$cluster))
library(ggpubr)
ggboxplot(test, x = "V2", y = "y", 
          color = "V2",
          order = c("1", "2", "3", "4", "5", "6"),
          ylab = "road infrastructure investment per capita", xlab = "cluster")
AOV <- aov(y ~ V2, data = test[test$V2 != 0,])
summary(AOV) #we do not have enough evidence to reject H0 - The three clusters proposed by DBSCAN do not explain y


# DBSCAN WITH MDS DIDN'T WORK NEITHER


###############################################################################
# THIRD PART (SUPERVISED): FEATURE SELECTION AND MODELLING
# Let's start by modelling y against the MDS points
# Recall that Dim1 is capturing the effect of V13, V12, and somewhat V14 and is also capturing the effect of V24 and somewhat V23
plot(mds_points[,1], y)
plot(X$V13, y) #looks like an exponential relation
plot(X$V12, y) #will try to model it
plot(X$V14, y)
plot(X$V24, y) #looks like a 1/x relationship
plot(X$V23, y)

# On the other hand Dim2 is capturing the effect of V1 and somewhat V4 and is also capturing some effect from V23
plot(mds_points[,2], y)
plot(X$V15, y)
plot(X$V16, y)

pairs(y ~ ., data = X)

# Just to be sure that we are taking all features into consideration
pairs(y ~ X$V1 + X$V2 + X$V4 + X$V5)
pairs(y ~ X$V6 + X$V7 + X$V9 + X$V10) #recall that V6 and V7 are linear combinations of y
pairs(y ~ X$V12 + X$V13 + X$V14 + X$V15)
pairs(y ~ X$V16 + X$V17 + X$V18 + X$V19)
pairs(y ~ X$V20 + X$V23 + X$V24 + X$V25) #V20 looks like an exponential relation
pairs(y ~ X$V26 + X$V27 + X$V28 + X$V29) #V28 may have something
pairs(y ~ X$V30 + X$V31)

#Will try to model Dim1 MDS, V12, V13, V20, V24 and V28
M <- data.frame(cbind(y, mds_points[,1], X$V12, X$V13, X$V20, X$V24, X$V28))
colnames(M) <- c("y", "D1MDS", "V12", "V13", "V20", "V24", "V28")
head(M)

# Linear models

# subset selection
base <- lm(y ~ ., data = M)
ols_step_all_possible(base) #just to confirm what we already knew, linear models won't do it
plot(ols_step_best_subset(base)) #no matter which feature combination we use, Adj R2 remains low


plot(M$D1MDS, y) #y vs Dim1 MDS
summary(lm(y ~ I(1/D1MDS), data = M)) #poor model
summary(lm(y ~ I(1/D1MDS) -1, data = M)) #poor model
summary(lm(y ~ I(1/D1MDS^2) -1, data = M)) #poor model
summary(rlm(y ~ D1MDS, data = M)) #somewhat better than non-robust model, will keep it
summary(rlm(y ~ I(1/D1MDS^2) -1, data = M)) #poor model
m1 <- rlm(y ~ D1MDS, data = M)

plot(M$V13, y) #y vs V13
summary(lm(y ~ V13, data = M)) #poor model
m2 <- rlm(y ~ V13, data = M)
plot(m2) #outliers presence
m2$w #weights assigned to rows for iterative regression
fit <- fitted(m2)
head(fit)
res <- stack(data.frame(Observed = y , Predicted = fit))
res <- cbind(res, x = rep(M$V13, 2))
head(res)
xyplot(values ~ x, data = res, group = ind, auto.key = TRUE)

plot(M$V20, y)
summary(rlm(y ~ V20, data = M)) #poor model
m3 <- rlm(y ~ V20, data = M)
plot(m3) #outliers presence
m3$w #weights assigned to rows for iterative regression
fit <- fitted(m3)
head(fit)
res <- stack(data.frame(Observed = y , Predicted = fit))
res <- cbind(res, x = rep(M$V20, 2))
head(res)
xyplot(values ~ x, data = res, group = ind, auto.key = TRUE)

plot(M$V24, y)
summary(rlm(y ~ I(1/V24)-1, data = M)) #poor model
m4 <- rlm(y ~ I(1/V24)-1, data = M)
plot(m4) #outliers presence
m4$w #weights assigned to rows for iterative regression
fit <- fitted(m4)
head(fit)
res <- stack(data.frame(Observed = y , Predicted = fit))
res <- cbind(res, x = rep(M$V24, 2))
head(res)
xyplot(values ~ x, data = res, group = ind, auto.key = TRUE)

plot(M$V28, y)
summary(rlm(y ~ V28, data = M)) #poor model
m5 <- rlm(y ~ V28, data = M)
plot(m5) #outliers presence
m5$w #weights assigned to rows for iterative regression
fit <- fitted(m5)
head(fit)
res <- stack(data.frame(Observed = y , Predicted = fit))
res <- cbind(res, x = rep(M$V28, 2))
head(res)
xyplot(values ~ x, data = res, group = ind, auto.key = TRUE,
       xlab = "CO2 emissions from transport, tonnes per inhabitant",
       ylab = "road investment, constant USD per capita",
       main = "Model 5",
       col = c("blue", "red"),
       pch = 17)

# How to choose the best model?
# Compute weighted R2

#test with m1
fitted(m1)*m1$w
head(fitted(m1))
head(m1$w)


tss <- sum((y - mean(y))^2) #Total sum of squares
fits <- data.frame(fitted(m1), fitted(m2), fitted(m3), fitted(m4), fitted(m5))
resid <- data.frame(y - fits)
sq_resid <- data.frame(resid^2)

wsq_resid = data.frame(matrix(nrow = nrow(M), ncol = length(sq_resid)))
colnames(wsq_resid) <- c("m1", "m2", "m3", "m4", "m5")
wsq_resid$m1 <- sq_resid$fitted.m1.*m1$w 
wsq_resid$m2 <- sq_resid$fitted.m2.*m2$w 
wsq_resid$m3 <- sq_resid$fitted.m3.*m3$w
wsq_resid$m4 <- sq_resid$fitted.m4.*m4$w
wsq_resid$m5 <- sq_resid$fitted.m5.*m5$w 

#R-squared
sort(round(1 - (colSums(wsq_resid)/tss), 3)) #m5 is the best model among the estimated models

summary(m5)
plot(M$V28, y)
names %>% filter(number == "V28") #best predictor is C02 emissions from transport


#END
