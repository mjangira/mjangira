# histogram-------------------

library(MASS)
MASS::Cars93  #if we  use :: to import the perticular data frame then no need to use library()
DF = data.frame(Cars93)
DF[1:20,]
table(DF$RPM)
hist(DF$RPM)
hist(DF$RPM,col = 3)
hist(DF$RPM,col = c(3,1,4,5,6,7),xlab = "RPM's",ylab="Freq",main = "Histogram of frequency")
hist(DF$RPM,col = c(3,1,4,5,6,7),xlab = "RPM's",ylab="Freq",main = "Histogram of frequency",labels = TRUE,breaks = seq(3500,6500,300))
par(mfrow = c(2,2))

#exmple 2
MASS::Animals
str(Animals)
DF1 = data.frame(Animals)
DF1[1:5,1:2]
hist(DF1$brain)
hist(DF1$brain,col = c(7,2,3,4,5,1),xlab="brain",ylab="freq",main = "Animal",labels=TRUE,breaks = seq(0,6000,by = 1000))
legend("topright", c("Cow","Grey Wolf","Goat","Guinea Pig","Donkey","Elephant"), fill = c(7,2,3,4,2,1))
par(mfrow = c(1,2))


#Scatterplot ----------------------------------
par(mfrow = c(1,1))
Age = c(10,45,66,12,50)
Height = c(122,143,124,115,126)
DF3 = data.frame(Age,Height)
plot(DF3)
plot(Age,Height)
plot(DF3,main="Graph1",col = c(1,2,3,4,5),xlab = "AGE",ylab = "HEIGHT",cex = 0.9,pch = 2,xlim = c(10,40),ylim = c(110,130))
plot(DF3,main = "Graph2")
legend("topright",c("A","B","C","D","E"),fill = c(1,2,3,4,5))


#adding points in graph
age1 = c(10,20,30)
height1 = c(118,135,139)
DF4 = data.frame(age1,height1)
points(DF4,col = 2 ,cex = 0.8,pch= 8)
points(age1*0.2,height1*0.3,col = 7 ,cex = 0.8,pch= 6)

# creating conditions by adding new col to data frame
library(ISLR)
A = data.frame(A$Limit)

newcol = ifelse(A$Limit<3087,1,ifelse(A$Limit<5876.5,2,5))
Q = data.frame(A$Limit,A$income,newcol)
str(Q)

