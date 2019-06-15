A = read.csv(file.choose())
A = na.omit(A)
A
sf = sample(c(2),nrow(A),replace=TRUE,prob=c(0.8,0.2))
trd = A[sf ==1,]  #dataframe trd
tst = A[sf ==2,]
cor(A[,c(1,2,3,5)])  # error for: cor(A$RND,A$ADMIN,A$MKT,A$PROFIT)

install.packages("psych") # this is for getting the graph and corelation value at the same time
library(psych)
pairs.panels(A)  #command to get the graph and co-relation value

model1 = lm(PROFIT~RND,data=trd) #creating model for traning, profit vs rnd--> this is to be done on trd data
pred = predict(model1,tst) # creating pred where we are predicting profit value using trained model on test data.
CB = cbind(pred,tst$PROFIT) # cbind is used to add column
error = pred - tst$PROFIT
error
actualval = tst$PROFIT
cbind(pred,actualval,error)
#=============================== simple linear regression example ============
#1)

library(ISLR)
library(psych)
A = data.frame(Credit)
str(A)
head(A)
pairs.panels(A)

cor(A[,c(1,2,3,5)])

numcols = unlist(lapply(A,is.numeric)) #numcols : this is to only numeric columns , lapply is for fetching all the values which are numeric
B = A[,numcols]
pairs.panels(B)
cor(B) #kya predictable hai kya nai hai

sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model1_Inc = lm(Income ~ Limit,data=trd)
summary(model1_Inc)  
#model2_Inc = lm(Income ~ Rating,data=trd)
#model3_Inc = lm(Income ~ Balance,data=trd)

#model1_Limit = lm(Limit ~ Balance,data=trd)
model2_Limit = lm(Limit ~ Rating,data=trd)
#model3_Limit = lm(Limit ~ Income,data=trd)

model1_Rating = lm(Rating ~ Balance,data=trd)

#model1_Bal = lm(Balance ~ Income,data=trd)
#model2_Bal = lm(Balance ~ Limit,data=trd)
model3_Bal = lm(Balance ~ Rating,data=trd)

pred_Inc = predict(model1_Inc,tsd)
head(cbind(Limit = tsd$Limit,Predival = pred_Inc,Actval = tsd$Income))
pred_Rating = predict(model1_Rating,tsd)

pred_Bal = predict(model3_Bal,tsd)

pred_Lim = predict(model2_Limit,tsd)


#=============================== multiple linear regression example ============
A = read.csv(file.choose())
str(A)
head(A)
ncols = unlist(lapply(A,is.numeric))
B = A[,ncols]
cor(B)
sf = sample(2,nrow(B),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]
model1 = lm(PROFIT ~ RND,data=trd)
summary(model1)
model2 = lm(PROFIT ~ RND+MKT+STATE,data=trd) 
summary(model2)
model3 = lm(PROFIT ~ RND+MKT,data=trd)
summary(model3)
model4 = lm(PROFIT ~ ADMIN,data=trd)
summary(model4)

pred1 = predict(model1,tsd)
pred2 = predict(model2,tsd)
pred3 = predict(model3,tsd)
pred4 = predict(model4,tsd)

head(cbind(PROFIT = tsd$PROFIT,Pred1 = pred1,Pred2 = pred2,Pred3 = pred3,Pred4 = pred4))

#========= Polinomial multiple lilnear regression=========

install.packages("ggplot2")

library(ggplot2)

a = c(21,19,39,50,60,55,78,69,90,110,111,120,130,141)

b = c(32,46,38,47,40,48,67,50,40,52,65,74,85,79)

f = data.frame(a,b)

ggplot(f, aes(y=b, x=a)) + 
  geom_point(alpha = .9) +
   stat_smooth(method = "lm", formula = y ~ I(x^2))
#=========  ridge/lasso  =========

install.packages("caret")  #used for trian control

#glmnet method used for rigde and lasso

#======= classification ==========


A = read.csv(choose.files())
str(A)
fivenum(A$PROFIT)
A$PROFIT_TYPE = ifelse(A$PROFIT <= 107978,1,0)
A$PROFIT_TYPE = as.factor(A$PROFIT_TYPE)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.8,0.2))
trd = A[sf==1,]
tsd = A[sf==2,]
model1 = glm(PROFIT_TYPE ~ RND+MKT+ADMIN,data = trd,family = "binomial")
pred = predict(model1,tsd)
pred
w = ifelse(pred<=0.5,0,1)
w
cm = table(predicted = w,actual = tsd$PROFIT_TYPE)
cm
cbind(w,tsd$PROFIT_TYPE)

#=======================RIDGE REGRESSION========================

install.packages("caret")
install.packages("glmnet")

library(caret)
library(glmnet)
library(ISLR)

set.seed(123) 

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A = read.csv(file.choose(),header = TRUE)

# A = data.frame(Hitters)

A1 = na.omit(A)

model1 = train(PROFIT ~ RND + MKT ,method = "glmnet",data = A, trControl = tc,tuneGrid = expand.grid(alpha = 0,lambda = seq(3000,5000,length=200)))

model1 = train(Salary ~ AtBat + Hits + League ,method = "glmnet",data = A1, trControl = tc,tuneGrid = expand.grid(alpha = 0,lambda = seq(40,50,length=20)))

model1$results
varImp(model1)

model1$bestTune

#model1(LOOK AT FINAL MODEL)

#plot(model1)   (PLOT IT)

#attributes(model1)  (SEE ITS ATTRIBUTES)

#model1$finalModel  (LOOK AT THE SELECTED MODEL COEFFICIENTS)

#plot(model1$finalModel,xvar="lambda")  (PLOT FINAL MODEL AGAINST PENALTY)

#plot(model1$finalModel,xvar="dev")  (PLOT FINAL MODEL AGAINST PENALTY)

#plot(varImp(model1,scale = FALSE))  (SEE VARIABLE SIGNIFICANCE...FEATURE SELECTION)

#=======================LASSO REGRESSION========================
  
  library(caret)
library(glmnet)

set.seed(123) 

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A = read.csv(file.choose())

model2 = train(PROFIT ~ . ,method = "glmnet",data = A, trControl = tc,tuneGrid = expand.grid(alpha = 1,lambda = seq(0.1,1,length=7)))


#model2 (LOOK AT FINAL MODEL)

#plot(model1)   (PLOT IT)

#attributes(model1)  (SEE ITS ATTRIBUTES)

#model2$finalModel  (LOOK AT THE SELECTED MODEL COEFFICIENTS)

#plot(model2$finalModel,xvar="lambda")  (PLOT FINAL MODEL AGAINST PENALTY)

#plot(model2$finalModel,xvar="dev")  (PLOT FINAL MODEL AGAINST PENALTY)

#plot(varImp(model2,scale = FALSE))  (SEE VARIABLE SIGNIFICANCE...FEATURE SELECTION)

#=================ELASTIC NET REGRESSION=================

library(caret)
library(glmnet)

set.seed(123) 

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A = read.csv(file.choose())
A = data.frame(iris)
#=================ELASTIC NET REGRESSION=================

library(caret)
library(glmnet)

set.seed(123) 

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A = read.csv(file.choose())
A = data.frame(iris)

model3 = train(Sepal.Length ~ . ,method = "glmnet",data = A, trControl = tc,tuneGrid = expand.grid(alpha = seq(0,1,length=10),lambda = seq(0.1,1,length=7)))

model3 #(LOOK AT FINAL MODEL)
#pred = predict(model3,trd)
cbind(pred,tsd$Sepal.Length)

plot(varImp(model3))  # (PLOT IT)

attributes(model3) # (SEE ITS ATTRIBUTES)

model3$finalModel  #(LOOK AT THE SELECTED MODEL COEFFICIENTS)

#== naivebayes


A = data.frame(iris)
head(A)
View(A)
A = na.omit(A)
sf = sample(2,nrow(A),replace=TRUE,prob = c(0.7,0.3))
trd= A[sf==1,]
tsd=A[sf==2,]
install.packages("naivebayes")
library(naivebayes)
model1 = naive_bayes(Species~.,data = trd)
pred = predict(model1,tsd)
cbind(pred,tsd$Species)
w = table(pred,tsd$Species)
#misclassification
misclassified = (1-(sum(diag(w))/nrow(tsd)))*100
model1$data

#=============================== non linear regression ======

install.packages("rpart")
install.packages("tree")
library(naivebayes)
library(tree)

A = data.frame(iris)
psych::pairs.panels(A[,1:4])
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model_tree = rpart::rpart(Species~.,data = trd)
model_nb = naive_bayes(Species~.,data = trd)

plot(model_tree)
text(model_tree)

predict_tree = predict(model_tree,tsd) #this gives error as length is diffrent of values 
pred_t = ifelse(predict_tree[,1] > 0.5,"setosa",
                ifelse(predict_tree[,2] > 0.5,"versicolor"
                       ,"virginica"))
predict_nb = predict(model_nb,tsd)

cf_tree = table(pred_t,tsd$Species)
cf_nb = table(predict_nb,tsd$Species)
cf_nb
cf_tree
#rpart as a special parametr called as complexity parameter , which tree does not have.

##

rpart::car9

#===
install.packages("randomForest")
library(randomForest)
A = data.frame(ISLR::Hitters)
str(A)
B = na.omit(A)
sf = sample(2,nrow(B),replace = TRUE,prob = c(0.7,0.3))
trd = B[sf == 1,]
tsd = B[sf == 2,]

#---------FOREST---------

rf = randomForest(League ~ .,data = trd,na.action = na.pass,ntree = 5)
pred = predict(rf,tsd)
cbind(pred,tsd$League)
cm = table(pred,Actual = tsd$League)
misclassification_rf = (1-(sum(diag(cm))/nrow(tsd))) * 100
misclassification_rf

#----TREE-----
tr = tree::tree(League ~ .,data = trd,na.action = na.pass)
predtr = predict(tr,tsd$League)
W = ifelse(predtr[,1]>0.5,"A","N")
cbind(W,tsd$League)
cm1 = table(W,tsd$League)
misclassification_tr = (1-(sum(diag(cm1))/nrow(tsd))) * 100
#ifelse()

#===========KNN============

install.packages("e1071")
library(caret)
ISLR::OJ
A = data.frame(iris)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,] 
tsd = A[sf == 2,] 

tc = trainControl(method = 'cv',number = 4,verboseIter = TRUE)

model1 = train(Species ~ . ,
               data = trd, 
               method = 'knn', 
               trControl = tc, 
               preProc = c("center","scale")) # preproc is used for standardization

pred = predict(model1,tsd)
cbind(pred,actual = tsd$Species)

cm = table(pred,actual = tsd$Species)
cm
cclass =(sum(diag(cm))/nrow(tsd))*100
mclass = 100-cclass
#example 2: by using knn function
A = ISLR::Hitters
str(A)
sf = sample(2,nrow(A),replace = TRUE,prob = c(07,0.3))
trd = A[sf==1,]
tsd = A[sf==2,]
cm ? # prepare manually by using knn homework
m1 = knn(trd,tsd,k = 9,cm)
#====== classitfiction over===========

#========== clustering==============

#=========KMEANS ON IRIS==========

A = data.frame(iris)
str(A)
library(data.table)
library(mltools)
A = one_hot(as.data.table(A))
#----k = 2----
m1 = kmeans(A,2)
m1$cluster
par(mfrow=c(3,2))
plot(A$Sepal.Length,A$Sepal.Width,col = m1$cluster)
plot(A$Sepal.Length,A$Petal.Length,col = m1$cluster)
plot(A$Sepal.Length,A$Petal.Width,col = m1$cluster)
plot(A$Sepal.Width,A$Petal.Length,col = m1$cluster)
plot(A$Sepal.Width,A$Petal.Width,col = m1$cluster)
plot(A$Petal.Length,A$Petal.Width,col = m1$cluster)

#----k = 3----

m3 = kmeans(A,3)
par(mfrow=c(3,2))
plot(A$Sepal.Length,A$Sepal.Width,col = m3$cluster)
plot(A$Sepal.Length,A$Petal.Length,col = m3$cluster)
plot(A$Sepal.Length,A$Petal.Width,col = m3$cluster)
plot(A$Sepal.Width,A$Petal.Length,col = m3$cluster)
plot(A$Sepal.Width,A$Petal.Width,col = m3$cluster)
plot(A$Petal.Length,A$Petal.Width,col = m3$cluster)

cbind(A$Species,m3$cluster)

#----k = 4----
m4 = kmeans(A,4)
par(mfrow=c(3,2))
plot(A$Sepal.Length,A$Sepal.Width,col = m4$cluster)
plot(A$Sepal.Length,A$Petal.Length,col = m4$cluster)
plot(A$Sepal.Length,A$Petal.Width,col = m4$cluster)
plot(A$Sepal.Width,A$Petal.Length,col = m4$cluster)
plot(A$Sepal.Width,A$Petal.Width,col = m4$cluster)
plot(A$Petal.Length,A$Petal.Width,col = m4$cluster)


#----k = 5-----

m5 = kmeans(A1,5)
par(mfrow=c(3,2))
plot(A$Sepal.Length,A$Sepal.Width,col = m5$cluster)
plot(A$Sepal.Length,A$Petal.Length,col = m5$cluster)
plot(A$Sepal.Length,A$Petal.Width,col = m5$cluster)
plot(A$Sepal.Width,A$Petal.Length,col = m5$cluster)
plot(A$Sepal.Width,A$Petal.Width,col = m5$cluster)
plot(A$Petal.Length,A$Petal.Width,col = m5$cluster)
levels(A$Species)
plot(A$Sepal.Length,A$Species,col = m2$cluster)
#===================

#---------ELBOW CURVE---------

A = data.frame(iris)
scaled_data = as.matrix(scale(A[,1:4]))
# Compute and plot wss for k = 2 to k = 15.
k.max = 20
data = scaled_data
wss = sapply(2:k.max, 
             function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#============= hirarchical

library(class)
install.packages("mltools")
library(data.table)
A = data.frame(iris)
library(mltools)
B = one_hot(as.data.table(A))
#---------KMEANS-----------
model_kmeans = kmeans(B,4)
model_kmeans$cluster
C = data.frame(B,CV_K= model_kmeans$cluster)
#---------HIERARCHICAL-----------
model_hc = hclust(dist(B),method = "complete")
#cutree(model_hc,h=3)
CV_hc = cutree(model_hc,k=4)
C = data.frame(C,CV_hc)
plot(model_hc)
