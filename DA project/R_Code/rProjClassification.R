## 
#     Author : Joydeep
#     Date   : 26/11/2014
#     Topic  : Exp 1 (Decision tree)
##

#-------------------------------------------------------------------------------------------

setwd("C:/Users/joydeep/Desktop/DA/Project")
mydata = read.csv("DataPrep.csv",header = T)

#data preparation
set.seed(1234)
ind = sample(2, nrow(mydata), replace = T , prob = c(70,30))
trainData = mydata[ind == 1,]
testData = mydata[ind == 2,]
trainData = data.frame(L1 = trainData$L1_CLASS, L2 = trainData$L2_CLASS,
                       L3 = trainData$L3_CLASS, S1 = trainData$S1_CLASS,
                       S2 = trainData$S2_CLASS, S3 = trainData$S3_CLASS,
                       NRC_CLASS = trainData$NRC_Class_Modified)

testData = data.frame(L1 = testData$L1_CLASS, L2 = testData$L2_CLASS,
                      L3 = testData$L3_CLASS, S1 = testData$S1_CLASS,
                      S2 = testData$S2_CLASS, S3 = testData$S3_CLASS,
                      NRC_CLASS = testData$NRC_Class_Modified)

# Formula building
myFormula = NRC_CLASS~L1+L2+L3+S1+S2+S3

# building tree
library("rpart")
tree = rpart(myFormula, data = trainData)
attributes(tree)
tree
# to see the tree
# variable importance S2 class = 22, which is maximum. Hence the importnce of variables 
# are s2>L1>L2>S1>S3>L3
summary(tree)

#prediction on testData
datapredict = predict(tree, testData)
datapredict
#table(datapredict)
library(gmodels)
#CrossTable (testData$NRC_CLASS , datapredict)



datapredict = predict(tree,testData,type="class")
dtconfmat = table(true = testData[,7], pred = datapredict)
dtconfmat




# to see the tree plot
plot(tree)
text(tree, use.n=T)
plot(tree, uniform = T , compress= T)
text(tree, use.n = T, all = T , cex =0.7)




# ---------------------------------using C5.0---------------------------------
library("C50")
trainData = data.frame(L1 = trainData$L1_CLASS, L2 = trainData$L2_CLASS,
                       L3 = trainData$L3_CLASS, S1 = trainData$S1_CLASS,
                       S2 = trainData$S2_CLASS, S3 = trainData$S3_CLASS,
                       NRC_CLASS = as.factor(trainData$NRC_Class_Modified))

testData = data.frame(L1 = testData$L1_CLASS, L2 = testData$L2_CLASS,
                       L3 = testData$L3_CLASS, S1 = testData$S1_CLASS,
                       S2 = testData$S2_CLASS, S3 = testData$S3_CLASS,
                       NRC_CLASS = as.factor(testData$NRC_Class_Modified))


#building model
model = C5.0(trainData[-7],trainData$NRC_CLASS)

#checking model
model

# checking structure of model
summary(model)
# Attribute usage is 100% Hense the most important atrribute is S2

#Evaluating model performance
library("gmodels")
pred = predict(model, testData)
CrossTable (testData$NRC_CLASS , pred)

datapredict = predict(model,testData,type="class")
dtconfmat = table(true = testData[,7], pred = datapredict)
dtconfmat

#to build visual tree
plot(tree, uniform = T , compress= T)
text(tree, use.n = T, all = T , cex =0.7)

#--------------------------------Thank you--------------------------------------------




