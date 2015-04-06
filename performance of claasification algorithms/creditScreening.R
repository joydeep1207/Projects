##
# name : Joydeep
#
#
##


# locating the working directory
setwd("C:/Users/deepj/Desktop/predict_exectime/POC/credit_screening")

# packages
# install.packages("class")
# install.packages("imputation")
# install.packages("VIM")


# loading packages
library(class)


# Normalization function
Norm=function(x)
      { 
        return (x-mean(x,na.rm=T))/sd(x,na.rm=T) 
      } 



# loading the data set
myData = read.csv("data.txt", header = F)

# data cleaning 
# replacing all "?" by NA
myData$V1 =as.factor (gsub("?", NA, myData$V1 ,fixed = T))
myData$V2 =scale (as.numeric (gsub("?", NA, myData$V2 ,fixed = T)))
myData$V3 =scale (as.numeric(gsub("?", NA, myData$V3 ,fixed = T)))
myData$V4 =as.factor (gsub("?", NA, myData$V4 ,fixed = T))
myData$V5 =as.factor (gsub("?", NA, myData$V5 ,fixed = T))
myData$V6 =as.factor (gsub("?", NA, myData$V6 ,fixed = T))
myData$V7 =as.factor (gsub("?", NA, myData$V7 ,fixed = T))
myData$V8 =scale(as.numeric(gsub("?", NA, myData$V8 ,fixed = T)))
myData$V9 =as.factor (gsub("?", NA, myData$V9 ,fixed = T))
myData$V10 =as.factor (gsub("?", NA, myData$V10 ,fixed = T))
myData$V11 =scale(as.numeric(gsub("?", NA, myData$V11 ,fixed = T)))
myData$V12 =as.factor (gsub("?", NA, myData$V12 ,fixed = T))
myData$V13 =as.factor (gsub("?", NA, myData$V13 ,fixed = T))
myData$V14 =scale(as.numeric (gsub("?", NA, myData$V14 ,fixed = T)))
myData$V15 =scale(as.numeric(gsub("?", NA, myData$V15 ,fixed = T)))
myData$V16 = as.factor(myData$V16)


set.seed(1234)
ind = sample(2, nrow(myData), replace = T, prob = c(70,30))


# preparing training and test data set
trainData = myData[ind ==1, ]
testData = myData[ind == 2, ]

trainData = data.frame(A1 = trainData$V1, A2 = trainData$V2, A3 = trainData$V3, A4 = trainData$V4, 
                       A5 = trainData$V5, A6 = trainData$V6, A7 = trainData$V7, A8 = trainData$V8,
                       A9 = trainData$V9, A10 = trainData$V10, A11 = trainData$V11, 
                       A12 = trainData$V12, A13 = trainData$V13, A14 = trainData$V14, 
                       A15 = trainData$V15, A16 = trainData$V16)

testData = data.frame(A1 = testData$V1, A2 = testData$V2, A3 = testData$V3, A4 = testData$V4, 
                       A5 = testData$V5, A6 = testData$V6, A7 = testData$V7, A8 = testData$V8,
                       A9 = testData$V9, A10 = testData$V10, A11 = testData$V11, 
                       A12 = testData$V12, A13 = testData$V13, A14 = testData$V14, 
                       A15 = testData$V15, A16 = testData$V16)

# ####################################################################################################
# Implementing knn 

# lebels of training and test data
trainLebel = trainData[16]
testLebel = testData[16]

# prediction
modelPred = knn(train = trainData[-16], test = testData[-16], trainLebel, k = 25, prob = 0.60)

### output of knn:
### Error in knn(train = trainData[-16], test = testData[-16], trainLebel,  : 
### no missing values are allowed

#2 using VIM package 
library("VIM")
myDataAfterImputation = kNN(myData, k = 25)

# ####################################################################################################
# implementing Decision trees

# 1. party package
install.packages("party")
library("party")

# defining formula 
myFormula = A16 ~ A15+A14+A13+A12+A11+A10+A9+A8+A7+A6+A5+A4+A3+A2+A1
tree = ctree (myFormula, data = trainData)
# checking prediction
table (predict(tree), trainData$A16)

# printing the tree
print (tree)

plot (tree)

# predict on test data
testPred= predict(tree, testData)

table (testPred,testData$A16)

### training set error rate
###         -   +
###     - 207  17
###     +  57 196

###  testPred  -  +
###         - 99  6
###         + 20 88
###
###  Accuracy 87.79%



#2 Using rpart

install.packages("rpart")
install.packages("gmodels")
library("rpart")

# tree using rpart
rpartTree = rpart(myFormula, trainData, control = rpart.control(minsplit = 20))

attributes(rpartTree)


print(rpartTree$cptable)

plot(rpartTree)
text (rpartTree, use.n = T)


summary(rpartTree)

#prediction on testData
datapredict = predict(rpartTree, testData)
datapredict

library(gmodels)




datapredict = predict(rpartTree,testData,type="class")


table (datapredict,testData$A16)


# to see the tree plot
plot(rpartTree)
text(rpartTree, use.n=T)
plot(rpartTree, uniform = T , compress= T)
text(rpartTree, use.n = T, all = T , cex =0.7)

### out put of rpart
### datapredict   -   +
###           - 103  13
###           +  16  81
###
### Accuracy = 86.38%






#3 C5.0


library(C50)
#building model
model = C5.0(trainData[-16],trainData$A16)

#checking model
model

# checking structure of model
summary(model)
# Attribute usage is 100% Hense the most important atrribute is A9

#Evaluating model performance
library("gmodels")
pred = predict(model, testData)
CrossTable (testData$A16 , pred)

datapredict = predict(model,testData,type="class")


table (datapredict, testData$A16)

### OUTPUT 
###   datapredict   -   +
###              - 101  14
###              +  18  80
###
###accuracy 84.97%



#################################################################################################
# Neural Networks


# defining formula 
myFormula = A16 ~ A15+A14+A13+A12+A11+A10+A9+A8+A7+A6+A5+A4+A3+A2+A1

# install package neural network
# install.packages("neuralnet")

# loading package in memory
library("neuralnet")

model = neuralnet(myFormula, data = trainData)

### Error
###  
###     Error in neurons[[i]] %*% weights[[i]] : 
###     requires numeric/complex matrix/vector arguments

### Hence we cannot apply on our data since it has categorical as well as numeric data




###############################################################################################
# Naive bayes

# installing packages
# install.packages('e1071', dependencies = TRUE)

#loading packages in memory
library(class) 
library(e1071) 

model = naiveBayes(trainData[ ,1:15], trainData[ , 16])
table( predict(model, testData[ , 1:15]), testData[ , 16])

### output 
###
###         -   +
###      - 109  32
###      +  10  62
###
### Accuracy 80.2%






