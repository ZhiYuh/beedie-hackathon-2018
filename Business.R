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
install.packages("glmnet")
require(glmnet)
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
DA <- select(dataset, -Record, -Disc, -Title, -LastOrder, -Weeks3Meals, -Sample)
woutSUB <- select(DA, -SUBSCRIBE) 

naIncome <- subset(woutSUB, is.na(woutSUB$DA_Income))

#use mice to impute the missing value
tempdata <- mice(woutSUB,seed=500)
#create five different dataset from MICE method
completeMice1 <- complete(tempdata, 1)
completeMice2 <- complete(tempdata, 2)
completeMice3 <- complete(tempdata, 3)
completeMice4 <- complete(tempdata, 4)
completeMice5 <- complete(tempdata, 5)

#adding subscribe back to the mice data 
newMice1<-merge(completeMice1, dataset[, c("custid", "SUBSCRIBE")], by="custid")
newMice2<-merge(completeMice2, dataset[, c("custid", "SUBSCRIBE")], by="custid")
newMice3<-merge(completeMice3, dataset[, c("custid", "SUBSCRIBE")], by="custid")
newMice4<-merge(completeMice4, dataset[, c("custid", "SUBSCRIBE")], by="custid")
newMice5<-merge(completeMice5, dataset[, c("custid", "SUBSCRIBE")], by="custid")

#seperate test set from training set 
test_df1 <- newMice1[rowSums(is.na(newMice1)) > 0,]
train_df1 <- newMice1[rowSums(is.na(newMice1)) == 0,]

test_df2 <- newMice2[rowSums(is.na(newMice1)) > 0,]
train_df2 <- newMice2[rowSums(is.na(newMice1)) == 0,]

test_df3 <- newMice3[rowSums(is.na(newMice1)) > 0,]
train_df3 <- newMice3[rowSums(is.na(newMice1)) == 0,]

test_df4 <- newMice4[rowSums(is.na(newMice1)) > 0,]
train_df4 <- newMice4[rowSums(is.na(newMice1)) == 0,]

test_df5 <- newMice5[rowSums(is.na(newMice1)) > 0,]
train_df5 <- newMice5[rowSums(is.na(newMice1)) == 0,]


#Continous data ggpairs
#for mice train data1 
ggpairs(train_df1, columns = c(2:5, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(train_df1, columns = c(6:9, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(train_df1, columns = c(10:12, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)

#for mice train data2 
ggpairs(train_df2, columns = c(2:5, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(train_df2, columns = c(6:9, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(train_df2, columns = c(10:12, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)

#for mice train data3 
ggpairs(train_df3, columns = c(2:5, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(train_df3, columns = c(6:9, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(train_df3, columns = c(10:12, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)

#for mice train data4 
ggpairs(train_df4, columns = c(2:5, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(train_df4, columns = c(6:9, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(train_df4, columns = c(10:12, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)


#for mice train data5 
ggpairs(train_df5, columns = c(2:5, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(train_df5, columns = c(6:9, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)
ggpairs(train_df5, columns = c(10:12, 14), mapping = aes(color = SUBSCRIBE), lower=list(combo=wrap("facethist", binwidth=0.5)), progress = FALSE)



#5-fold cross validation

k = 5
n = floor(nrow(train_df1)/k) #size of each fold
acc <- rep(NA, k) #store the accuracy here.


# for (i in 1:k) {
#   s1 <- ((i - 1) * n + 1) #the start of the subset.
#   s2 <- (i * n) #the end of the subset.
#   subset <- s1:s2 #the range of subset.
#   
#   cvTrain <- train_df1[-subset, ]
#   cvTrain<- factor(cvTrain$SUBSCRIBE)
#   
#   cvTest <- train_df1[subset, ]
#   cvTestX <- select(cvTest, -SUBSCRIBE)
#   ActualDT <- cvTest$SUBSCRIBE
#   
#   LogReg <- glm(SUBSCRIBE ~ ., family = binomial(link = 'logit'), data = cvTrain)
#   DTPred <- predict(LogReg, cvTestX, type = "response")
#   DTPred <- ifelse(DTPred > 0.5, "high", "low")
#   acc[i] <- sum(DTPred == cvTest[, "SUBSCRIBE"])/nrow(cvTest)
#   print(paste("The accuracy for fold", i, "is", acc[i], "."))
# }
#the mean accuracy for 5-fold decision tree method.
paste("The mean accuracy for 5-fold decision tree method is", round(mean(acc), 4), ".")

#the estimate of the loss for 5-fold decision tree method.
paste("The estimate of the loss for 5-fold random forest method is", round(1 - mean(acc), 4), ".")
