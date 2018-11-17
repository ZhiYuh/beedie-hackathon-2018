library(readr)
dataset <- read_csv("NewData.csv")
require(ggplot2)
require(class)
require(rpart)
require(randomForest)
require(dplyr)
require(rpart.plot)
require(reshape)
require(tidyr)
#install.packages("GGally")
require(GGally)

require(caret)
require(corrplot)
require(glmnet)
#install.packages("mice")
require(mice)
#View(dataset)

#finding the people who did subscribe

yes <- subset(dataset, SUBSCRIBE == "Y")
no <- subset(dataset, SUBSCRIBE == "N")
#creating histrogram to see the distributuion of income of those who have suscribed:
hist(yes$DA_Income,main="Income of Subscribers", xlab="Income", border="black", col="blue")
hist(no$DA_Income,main="Income of NonSubscribers", xlab="Income", border="black", col="blue")


#nothing worked for categorical data 
#ageGroup <- table(dataset$SUBSCRIBE, dataset$Pcode)
#chisq.test(ageGroup)


#get rid of SUBSCRIBE first.
#select the column for trainset
DA <- select(dataset, -Record, -Disc, -Title, -LastOrder, -Weeks3Meals, Sample)
woutSUB <- select(DA, -SUBSCRIBE) 

naIncome <- subset(woutSUB, is.na(woutSUB$DA_Income))

#use mice to impute the missing value
tempdata <- mice(woutSUB)
#create five different dataset from MICE method
completeMice1 <- complete(tempdata, 1)
completeMice2 <- complete(tempdata, 2)
completeMice3 <- complete(tempdata, 3)
completeMice4 <- complete(tempdata, 4)
completeMice5 <- complete(tempdata, 5)


newMice1<-merge(completeMice1, dataset[, c("custid", "SUBSCRIBE")], by="custid")
newMice2<-merge(completeMice2, dataset[, c("custid", "SUBSCRIBE")], by="custid")
newMice3<-merge(completeMice3, dataset[, c("custid", "SUBSCRIBE")], by="custid")
newMice4<-merge(completeMice4, dataset[, c("custid", "SUBSCRIBE")], by="custid")
newMice5<-merge(completeMice5, dataset[, c("custid", "SUBSCRIBE")], by="custid")

#Continous data ggpairs

ggpairs(dataset, columns = c(2:5, 1), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(dataset, columns = c(6:9, 1), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(dataset, columns = c(10:12, 1), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)


