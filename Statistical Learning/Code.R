pacman::p_load("tidyverse",
               "dplyr",
               "stringr",
               "moments",
               "readxl",
               "caret",
               "ggplot2",
               "VIM",
               "cluster",
               "mice",
               "psych",
               "factoextra",
               "corrplot",
               "bootstrap",
               "leaps",
               "car",
               "plm",
               "glmnet",
               "pls",
               "foreign",
               "xtable",
               "stargazer")



#UNSUPERVISED LEARNING


#Goal: to find a set of countries which are similar, in construction market terms, to Turkey
#Database was obtained from Global Data, an information provider for the construction sector
#Each variable measures the value (real USD 2021) of Equipment used in New Construction in each country in 2021

#Load data
equipment<-as.data.frame(read_excel("Database.xlsx", col_names=T, sheet="equipment"))
rownames(equipment)<-equipment[,1]
equipment<-equipment[,-1]

#Scale data
equipment<-as.data.frame(scale(equipment))

head(equipment)
str(equipment)
summary(equipment)

#PCA
pca<-prcomp(equipment)
plot(pca, type="l", main="Scree Diagram")
abline(h=1, lwd=3, col="red")
biplot(pca, scale=0)

#Extract PCs
pca$x
equipment_pca<-pca$x[,1:2] #extract PC1 and PC2
str(equipment_pca)
equipment_pca<-as.data.frame(equipment_pca)

#Plot PC1 and PC2
ggplot(equipment_pca, aes(PC1, PC2))+
  stat_ellipse(geom="polygon", col="black", alpha=0.5)+
  geom_point(shape=21, col="black")+
  geom_text(label=rownames(equipment_pca))


#Which are the most alike countries?
PC1<-equipment_pca[order(equipment_pca$PC1),]
round(PC1, 2)
PC2<-equipment_pca[order(equipment_pca$PC2),] 
round(PC2, 2) 
round(equipment_pca, 1)

rm(equipment_pca, PC1, PC2, pca)



#HHIERACHICAL CLUSTERING

#Distance. The data has already been scaled
dist<-dist(equipment)

#1 Complete distance
hc1<-hclust(dist, method="complete")
plot(hc1)
rect.hclust(hc1, k=3, border=2:5)
c1<-cutree(hc1, k=3)
fviz_cluster(list(data=equipment, cluster=c1), main="Complete Linkage")

#2 Average
hc2<-hclust(dist, method="average")
plot(hc2)
rect.hclust(hc2, k=3, border=2:5)
c2<-cutree(hc2, k=3)
fviz_cluster(list(data=equipment, cluster=c2), main="Average Linkage")

#3 Ward Distance
hc3<-hclust(dist, method="ward.D")
plot(hc3)
rect.hclust(hc3, k=3, border=2:5)
c3<-cutree(hc3, k=3)
fviz_cluster(list(data=equipment, cluster=c3), main="Ward Distance")

#Compare solutions
table(c1,c2)
table(c1,c3)
table(c2,c3)

#Plot all solutions
par(mfrow = c(1, 3))
plot(hc1, main="Complete Linkage")
rect.hclust(hc1, k=3, border=2:5)
plot(hc2, main="Average Linkage")
rect.hclust(hc2, k=3, border=2:5)
plot(hc3, main="Ward Distance")
rect.hclust(hc3, k=3, border=2:5)

#What does it mean to belong to Cluster 1, 2 or 3?. Example with Complete Linkage Cluster
plot(equipment[,1:5], col=c1, main="Complete Linkage")
plot(equipment[,6:10], col=c1, main="Complete Linkage")
plot(equipment[,11:15], col=c1, main="Complete Linkage")
plot(equipment[,16:20], col=c1, main="Complete Linkage")
plot(equipment[,21:24], col=c1, main="Complete Linkage")

#Explore solutions
aggregate(equipment, list(c1), mean) #Complete Linkage
aggregate(scale(equipment), list(c2), mean) #Average Linkage
aggregate(scale(equipment), list(c3), mean) #Ward Distance

#Heatmap
heatmap(as.matrix(equipment))


#Compute R^2 for each cluster and compare results
#Cluster 1 - Complete Linkage
R2.C1<-equipment
R2.C1$group<-c1

R2 <- rep(NA, (ncol(R2.C1)-1))
for(i in 1:(ncol(R2.C1)-1)) 
  R2[i] <- anova(aov(R2.C1[,i] ~ R2.C1[,ncol(R2.C1)]))[1,2]/(anova(aov(R2.C1[,i] ~ R2.C1[,ncol(R2.C1)]))[1,2]+anova(aov(R2.C1[,i] ~ R2.C1[,ncol(R2.C1)]))[2,2])

R2
R2.C1<-R2.C1[,-ncol(R2.C1)]
col.c1<-colnames(R2.C1)
final.c1<-cbind(col.c1,R2)
final.c1<-as.data.frame(final.c1)
final.c1[,2]<-as.numeric(final.c1[,2])
mean(final.c1[,2]) 

#Cluster 2 - Average Linkage
R2.C2<-equipment
R2.C2$group<-c2

R2 <- rep(NA, (ncol(R2.C2)-1))
for(i in 1:(ncol(R2.C2)-1)) 
  R2[i] <- anova(aov(R2.C2[,i] ~ R2.C2[,ncol(R2.C2)]))[1,2]/(anova(aov(R2.C2[,i] ~ R2.C2[,ncol(R2.C2)]))[1,2]+anova(aov(R2.C2[,i] ~ R2.C2[,ncol(R2.C2)]))[2,2])

R2
R2.C2<-R2.C2[,-ncol(R2.C2)]
col.c2<-colnames(R2.C2)
final.c2<-cbind(col.c2,R2)
final.c2<-as.data.frame(final.c2)
final.c2[,2]<-as.numeric(final.c2[,2])
mean(final.c2[,2]) 


#Cluster 3 - Ward Distance
R2.C3<-equipment
R2.C3$group<-c3

R2 <- rep(NA, (ncol(R2.C3)-1))
for(i in 1:(ncol(R2.C3)-1)) 
  R2[i] <- anova(aov(R2.C3[,i] ~ R2.C3[,ncol(R2.C3)]))[1,2]/(anova(aov(R2.C3[,i] ~ R2.C3[,ncol(R2.C3)]))[1,2]+anova(aov(R2.C3[,i] ~ R2.C3[,ncol(R2.C3)]))[2,2])

R2
R2.C3<-R2.C3[,-ncol(R2.C3)]
col.c3<-colnames(R2.C3)
final.c3<-cbind(col.c3,R2)
final.c3<-as.data.frame(final.c3)
final.c3[,2]<-as.numeric(final.c3[,2])
mean(final.c3[,2]) 



#K-MEANS CLUSTER
par(mfrow=c(1,1))

#How many clusters should we use?
set.seed(2022)
wss <- (nrow(equipment))*sum(apply(equipment,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(equipment, 
                                     centers=i)$withinss)
plot(1:10, wss, type="b",main="Within Deviance by number of Cluster",
     xlab="Number of clusters",
     ylab="Within Deviance")
#We will try k=3 and k=5


#K=3 
set.seed(2022)
fit.k3 <- kmeans(equipment, 3) #3 cluster solution
aggregate(equipment,by=list(fit.k3$cluster),FUN=mean)
kmean3<-data.frame(equipment, fit.k3$cluster)
kmean3[,ncol(kmean3)]

#Compare against Hierarchical cluster solution
table(c1,kmean3$fit.k3.cluster)
table(c2,kmean3$fit.k3.cluster)
table(c3,kmean3$fit.k3.cluster)


#R^2 estimation for K-mean cluster solution with k=3
R2 <- rep(NA, (ncol(kmean3)-1))
for(i in 1:(ncol(kmean3)-1)) 
  R2[i] <- anova(aov(kmean3[,i] ~ kmean3[,ncol(kmean3)]))[1,2]/(anova(aov(kmean3[,i] ~ kmean3[,ncol(kmean3)]))[1,2]+anova(aov(kmean3[,i] ~ kmean3[,ncol(kmean3)]))[2,2])

R2
kmean3<-kmean3[,-ncol(kmean3)]
col.kmean3<-colnames(kmean3)
final.kmean3<-cbind(col.kmean3,R2)
final.kmean3<-as.data.frame(final.kmean3)
final.kmean3[,2]<-as.numeric(final.kmean3[,2])
mean(final.kmean3[,2])


#K=5
set.seed(2022)
fit.k5<-kmeans(equipment, 5) #4 cluster solution
aggregate(equipment,by=list(fit.k5$cluster),FUN=mean)
kmean5<-data.frame(equipment, fit.k5$cluster)

#Compare against Hierarchical cluster solution
table(c1,fit.k5$cluster)
table(c2,fit.k5$cluster)
table(c3,fit.k5$cluster)

#R^2 estimation for K-mean cluster solution with k=3
R2 <- rep(NA, (ncol(kmean5)-1))
for(i in 1:(ncol(kmean5)-1)) 
  R2[i] <- anova(aov(kmean5[,i] ~ kmean5[,ncol(kmean5)]))[1,2]/(anova(aov(kmean5[,i] ~ kmean5[,ncol(kmean5)]))[1,2]+anova(aov(kmean5[,i] ~ kmean5[,ncol(kmean5)]))[2,2])

R2
kmean5<-kmean5[,-ncol(kmean5)]
col.kmean5<-colnames(kmean5)
final.kmean5<-cbind(col.kmean5,R2)
final.kmean5<-as.data.frame(final.kmean5)
final.kmean5[,2]<-as.numeric(final.kmean5[,2])
mean(final.kmean5[,2])



#Compare solution against Hierarchical Clusters
final_clusters<-cbind(final.c1, final.c2[,2], final.c3[,2], final.kmean3[,2], final.kmean5[,2])
final_clusters<-as.data.frame(final_clusters)
colnames(final_clusters)<-c("Variables",
                            "R^2 Cluster 1",
                            "R^2 Cluster 2",
                            "R^2 Cluster 3",
                            "R^2 K-mean 3",
                            "R^2 K-mean 5")
summary(final_clusters)
round(final_clusters[,2:6],3)
round(colMeans(final_clusters[,2:6]),2)

rm(list = ls())
graphics.off()
cat("\f")


################################################################################
################################################################################
################################################################################
################################################################################




#SUPERVISED LEARNING

#1) Data loading and cleaning

#Load databases
exc<-as.data.frame(read_xlsx("Database.xlsx", col_names=T, sheet="exc"))
econ<-as.data.frame(read_xlsx("Database.xlsx", col_names=T, sheet="econ"))
dem<-as.data.frame(read_xlsx("Database.xlsx", col_names=T, sheet="dem"))

#Set independent variable
rownames(exc)<-exc[,1]
y<-as.matrix(exc)
y<-y[,2]
y<-as.data.frame(y)
colnames(y)<-c("Excavators")
rm(exc)

#Add Turkey now to the dependent variable even if the estimation will be done at the end
y[nrow(y)+1, ]<-0
rownames(y)[nrow(y)]<-c("Turkey")
y

#Econ database clean up
rownames(econ)<-econ[,1] #set row names
econ<-econ[,-1] #drop first column
econ <- merge(y, econ, by=0) #merge datasets on countries with dependent variable
rownames(econ)<-econ[,1] #set rownames again
econ<-econ[,-1] #drop first column of new dataset
econ[,2:ncol(econ)]<-as.numeric(unlist(econ[,2:ncol(econ)])) #convert chr columns to numeric
econ<-econ[ , apply(econ, 2, function(x) !any(is.na(x)))] #drop columns with at least one NA

#Demographics database clean up
rownames(dem)<-dem[,1] #set row names
dem<-dem[,-1] #drop first column
dem <- merge(y, dem, by=0) #merge datasets on countries with dependent variable
rownames(dem)<-dem[,1] #set rownames again
dem<-dem[,-1] #drop first column of new dataset
dem[,2:ncol(dem)]<-as.numeric(unlist(dem[,2:ncol(dem)])) #convert chr columns to numeric
dem<-dem[, colSums(is.na(dem)) != nrow(dem)] #drop columns full of NA's
dem<-dem[ , apply(dem, 2, function(x) !any(is.na(x)))] #drop columns with at least one NA

#Adjust column length
names<-names(dem)
names[2:length(names)]<-str_sub(names[2:length(names)], end=-7)
names(dem)<-names
rm(names)

#Drop y from dem
dem<-dem[,-1]

#Merge econ and dem
econ<-merge(econ, dem, by=0)
rownames(econ)<-econ[,1] #set row names
econ<-econ[,-1]
rm(dem)
econ[,1]<-as.numeric(econ[,1])
str(econ[,1])

#Check Skewness
sk<-as.data.frame(skewness(econ)) #Log transformation needed for very skewed data

#Before doing the log transformation, negative data will be shifted to the non-negative quadrant
for (i in 1:ncol(econ)){
  if (min(econ[,i]) < 0){
    econ[,i]<-econ[,i]+(-min(econ[,i]-0.01))
  }
}  

#Log transformation for skewed values
for (i in 1:ncol(econ)){
  if (skewness(econ[,i]) > 1){
    econ[,i]<-log(econ[,i])
  } else if (skewness(econ[,i]) < -1){
    econ[,i]<-log(econ[,i])
  }
}

#Compare skewness before and after
sk.new<-as.data.frame(skewness(econ))
round(cbind(sk, sk.new), 2)
econ<-econ[ , apply(econ, 2, function(x) !any(is.na(x)))]
rm(sk, sk.new, i, y)

#Scale predictors
econ[,2:ncol(econ)]<-as.data.frame(scale(econ[,2:ncol(econ)]))

#Final Check for NA's on scaled dataset
any(is.na(econ))
econ<-econ[ , apply(econ, 2, function(x) !any(is.na(x)))] #drop columns with at least one NA
any(is.na(econ))

#Store Turkey row from database. This row will be used at the end to estimate the market value
Turkey.x<-econ["Turkey",]
Turkey.x<-Turkey.x[,-1]

#Drop Turkey from dataset before estimating models
econ<-econ[rownames(econ) != "Turkey", , drop=F]




####################
#2) Estimating models

#Linear regression using regularization and LOOCV
#Lasso regression LOOCV
n=nrow(econ)
p=ncol(econ)

set.seed(2022)
folds=sample(rep(1:n,length=n))
folds
table(folds)

lasso.results=matrix(NA,n,3)
colnames(lasso.results)=c("Lambda", "Prediction", "SE")

set.seed(2022)
for (i in 1:nrow(econ)){
  x.test<-econ[folds==i,2:p]
  x.train<-econ[folds!=i, 2:p]
  y.test<-econ[folds==i,1]
  y.train<-econ[folds!=i, 1]
  lasso.results[i,1] <- cv.glmnet(as.matrix(x.train), y.train, type.measure="mse", nfolds=n-1, alpha = 1,family = "gaussian")$lambda.min
  lasso <- glmnet(as.matrix(x.train),y.train,alpha = 1,family = "gaussian",lambda = lasso.results[i,1])
  lasso.results[i,2] = predict(lasso,newx=as.matrix(x.test))
  lasso.results[i,3] <- (y.test-lasso.results[i,2])^2
}

round(lasso.results, 2)
print(xtable(lasso.results, type = "latex"), file = "Lassoresults.tex")

best.lambda.lasso<-mean(lasso.results[,1])
round(best.lambda.lasso,2)
SSE.lasso<-sum(lasso.results[,3])

#Final model
model_lasso <- glmnet(as.matrix(econ[,2:p]),
                      econ[,1],
                      alpha = 1,
                      family = "gaussian",
                      lambda = best.lambda.lasso)

coef_lasso<-as.matrix(coef(model_lasso)[coef(model_lasso)[,1]!=0,])
colnames(coef_lasso)<-c("Coefficient")
print(xtable(coef_lasso, type = "latex"), file = "Lassocoefs.tex")


#Ridge regression LOOCV
ridge.results=matrix(NA,n,3)
colnames(ridge.results)=c("Lambda", "Prediction", "SE")

set.seed(2022)
for (i in 1:nrow(econ)){
  x.test<- econ[folds==i,2:p]
  x.train<-econ[folds!=i, 2:p]
  y.test<-econ[folds==i,1]
  y.train<-econ[folds!=i, 1]
  ridge.results[i,1] <- cv.glmnet(as.matrix(x.train), y.train, nfolds=n-1, type.measure="mse", alpha = 0, family = "gaussian")$lambda.min
  ridge <- glmnet(as.matrix(x.train),y.train,alpha = 0, family = "gaussian", lambda = ridge.results[i,1])
  ridge.results[i,2] = predict(ridge,newx=as.matrix(x.test))
  ridge.results[i,3] <- ((y.test-ridge.results[i,2])^2)
}

round(ridge.results, 2)
print(xtable(ridge.results, type = "latex"), file = "Ridgeresults.tex")

best.lambda.ridge<-mean(ridge.results[,1])
SSE.ridge<-sum(ridge.results[,3])

#Final model
model_ridge <- glmnet(as.matrix(econ[,2:p]),
                      econ[,1],
                      alpha = 0,
                      family = "gaussian",
                      lambda = best.lambda.ridge)

coef_ridge<-as.matrix(coef(model_ridge)[coef(model_ridge)[,1]!=0,])
colnames(coef_ridge)<-c("Coefficient")
print(xtable(coef_ridge, type = "latex"), file = "Ridgecoefs.tex")

#Final results
round(c(SSE.lasso, SSE.ridge),2) #Ridge Regression outperforms Lasso
dim(coef_lasso)



#Elastic Net regression
best.lambdas<-data.frame(matrix(NA, nrow=n, ncol=10))

#First, let's get the lambda parameter using LOOCV for values of alpha between 0.01 and 1
for (q in 1:10){
  colnames(best.lambdas)[q]<-paste0("alpha ", q/10)
}

set.seed(2022)
for (i in 1:n){
  x.test<- econ[folds==i,2:p]
  x.train<- econ[folds!=i, 2:p]
  y.test<-econ[folds==i,1]
  y.train<-econ[folds!=i, 1]
  
  for (k in 1:10){
    best.lambdas[i,k]<-cv.glmnet(as.matrix(x.train),
                                 y.train,
                                 type.measure="mse",
                                 alpha=k/10,
                                 nfolds=n-1,
                                 family="gaussian")$lambda.min
  }
}

#Average lambda.min for each value of alpha
elastic.net.lambdas<-as.data.frame(colMeans(best.lambdas))
colnames(elastic.net.lambdas)<-c("Lambda")

#Now we can proceed with the prediction for each value of alpha
elastic.net.pred<-data.frame(matrix(NA, nrow=n, ncol=10)) #Matrix to store predictions (100 predictions for each validation set)
colnames(elastic.net.pred)<-colnames(best.lambdas)

elastic.net.sse<-data.frame(matrix(NA, nrow=n, ncol=10)) #Vector to store sum squared residuals of each alpha used for prediction
colnames(elastic.net.sse)<-colnames(elastic.net.pred)

set.seed(2022)
for (i in 1:n){
  x.test<- econ[folds==i,2:p]
  x.train<- econ[folds!=i, 2:p]
  y.test<-econ[folds==i,1]
  y.train<-econ[folds!=i, 1]
  
  for (k in 1:10){
    elastic.net <- glmnet(as.matrix(x.train),y.train, nfolds=n-1, alpha = k/10, family = "gaussian", lambda = elastic.net.lambdas[k,1])
    elastic.net.pred[i,k]<-predict(elastic.net, newx=as.matrix(x.test))
    elastic.net.sse[i,k] <- (y.test-elastic.net.pred[i,k])^2
  }
}

elastic.net.sse<-colSums(elastic.net.sse)
elastic.net.results<-round(cbind(elastic.net.lambdas, elastic.net.sse),2)
colnames(elastic.net.results)<-c("Lambda", "SSR")
print(xtable(elastic.net.results, type = "latex"), file = "elasticnetresults.tex")


best.alpha.id<-which.min(as.matrix(elastic.net.sse))
SSE.elastic.net<-elastic.net.sse[which.min(elastic.net.sse)]
#Elastic Net regression is the best model


#Final results
round(c(SSE.lasso, SSE.ridge, SSE.elastic.net), 2) #Elastic net is the best solution with alpha=1

y<-econ[,1]
y.mean<-mean(y)
SST<-sum((y-y.mean)^2)
SSR.lasso<-SST-SSE.lasso
SSR.ridge<-SST-SSE.ridge
SSR.elastic.net<-SST-SSE.elastic.net

round(c(SSR.lasso, SSR.ridge, SSR.elastic.net), 2)
R.sq<-as.data.frame(round(c(SSR.lasso/SST, SSR.ridge/SST, SSR.elastic.net/SST), 2))
rownames(R.sq)<-c("Lasso", "Ridge", "Elastic Net, a=0.1")
colnames(R.sq)<-c("Value")
R.sq
print(xtable(R.sq, type = "latex"), file = "R.sq.tex")

#Final model
model_elastic <- glmnet(x=as.matrix(econ[,2:p]),
                        y=econ[,1],
                        alpha = 0.1,
                        family = "gaussian",
                        lambda = elastic.net.lambdas[best.alpha.id,1])

coefs<-coef(model_elastic)
coefs<-data.frame(name = coefs@Dimnames[[1]][coefs@i + 1], coefficient = coefs@x)




#Bootstrap Regression


#1. Estimate regression coefficients for the original sample and calculate fitted values and residuals
y<-econ[,1]
yhat<-predict(model_elastic, newx=as.matrix(econ[,2:p]))
yhat #Fitted values
y #Real values
e<-y-yhat #Residuals for each observation
e


#2. Bootstrap the residuals
k=10000
set.seed(2022)
e.bootstrapped<-replicate(k, sample(e, replace=T))
as.matrix(e.bootstrapped)
dim(e.bootstrapped) #Check dimensions

y.bootstrapped<-matrix(NA, nrow=length(yhat), ncol=k)
y.bootstrapped[,1:k]<-yhat
y.bootstrapped<-y.bootstrapped+e.bootstrapped


#3. Regress the bootstrapped Y values using the Elastic Net regression with the optimal lambda and alpha
betas.bootstrap<-matrix(NA, length(coefs[,1]), k)
rownames(betas.bootstrap)<-coefs[,1]
x.bootstrap<-econ %>% select(coefs[-1,1])

for (i in 1:k){
  betas.bootstrap[,i] <- coef(glmnet(x=as.matrix(x.bootstrap),
                               y=y.bootstrapped[,i],
                               alpha = 0.1,
                               family = "gaussian",
                               lambda = elastic.net.lambdas[best.alpha.id,1]))[,1]
}

#Compute means and standard errors of coefficients
coefs.bootstrap<-rowMeans(betas.bootstrap)
sigma.bootstrap<-apply(betas.bootstrap, 1, sd)
t.bootsrap<-coefs.bootstrap/sigma.bootstrap
H0.bootstrap<-t.bootsrap>1.96
H0.elastic.net<-coefs[,2]/sigma.bootstrap
H0.elastic.net<-H0.elastic.net>1.96

sum.bootstrap<-cbind(coefs[,2],
                           coefs.bootstrap,
                           sigma.bootstrap,
                           t.bootsrap,
                           H0.bootstrap,
                           H0.elastic.net)

colnames(sum.bootstrap)<-c("Coefficients without Bootstrap",
                            "Coefficients with Bootstrap",
                           "Standard errors",
                           "t-statistic",
                           "Significant.Bootstrap",
                           "Significant.Elastic.Net")

sum.bootstrap<-as.data.frame(sum.bootstrap)

sum.bootstrap$Significant.Bootstrap[sum.bootstrap$Significant.Bootstrap>0]<-"Yes"
sum.bootstrap$Significant.Bootstrap[sum.bootstrap$Significant.Bootstrap<1]<-"No"

sum.bootstrap$Significant.Elastic.Net[sum.bootstrap$Significant.Elastic.Net>0]<-"Yes"
sum.bootstrap$Significant.Elastic.Net[sum.bootstrap$Significant.Elastic.Net<1]<-"No"

sum.bootstrap %>% filter(Significant.Bootstrap=="Yes") #Final variables that are significant according to bootstrap coefficients
a<-sum.bootstrap %>% filter(Significant.Elastic.Net=="Yes") #Final variables that are significant according to Elastic Net coefficients

print(xtable(sum.bootstrap, type = "latex"), file = "summarybootstrap.tex")
round(a[,1],2)

#Compare results against Elastic Net without Bootstrap
#yhat<-predict(model_elastic, newx=as.matrix(econ[,2:p]))
#yhat.bootstrap<-sum.bootstrap[1,2]+rowSums(x.bootstrap*as.vector(sum.bootstrap[-1,2]))
#cbind(y, yhat, yhat.bootstrap)
#colSums(cbind(y, yhat, yhat.bootstrap))
#sum((y-yhat.bootstrap)^2) #SSE Elastic Net without Bootstrap - even higher than the SSE of Elastic Net LOOCV
#Bootstrap helps to estimate st errors of parameters but no their coefficients



#Estimate Mini Excavators sales in Turkey in 2021

#Elastic Net model without Bootstrapping
Turkey.y<-predict(model_elastic, newx=as.matrix(Turkey.x))
exp(Turkey.y) #16,280 Mini Excavators sold in Turkey in 2021


#Elastic Net model with Bootstrap
#Turkey.x.bootstrap<-Turkey.x %>% select(coefs[-1,1])
#Turkey.y.bootstrap<-sum.bootstrap[1,2]+sum(Turkey.x.bootstrap*sum.bootstrap[-1,2])
#exp(Turkey.y.bootstrap) #11,019 Mini Excavators sold in Turkey in 2021

#Mean of both solutions
#((exp(Turkey.y)+exp(Turkey.y.bootstrap)))/2


rm(list = ls())
graphics.off()
cat("\f")


