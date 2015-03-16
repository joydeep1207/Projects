##
#      Author : Joydeep
#      Date   : 23/11/14
#      Topic  : Apply SVM , apply PCA then SVM to compare Accuracy 
##

## -----------------------------------------


setwd("C:/Users/joydeep/Desktop/DA/Project")
mydata = read.csv("DataPrep1.csv", header = T)

# preparing data
t = data.frame(L1 = mydata$L1_Marks,
               L2 = mydata$L2_MARKS,
               L3 = mydata$L3_MARKS,
               S1 = mydata$S1_MARKS,
               S2 = mydata$S2_MARKS,
               S3 = mydata$S3_MARKS,
               class = as.factor(mydata$NRC_Class_Numeric))

set.seed(1234)
ind = sample (2, nrow(t), replace = T , prob = c(70,30))
trainData = t[ind == 1 , ]
testData = t[ind ==2, ]

library("kernlab")
m = ksvm(class~.,data = trainData,kernel = "vanilladot")

m
#training error 0.017%

p = predict(m,testData)

# evaluating performance
agreement = p ==testData$class
prop.table(table(agreement))
 
# accuracy 96.3% 

#--------------------------------------------------------------------------------------

t = data.frame(L1 = mydata$L1_Marks,
               L2 = mydata$L2_MARKS,
               L3 = mydata$L3_MARKS,
               S1 = mydata$S1_MARKS,
               S2 = mydata$S2_MARKS,
               S3 = mydata$S3_MARKS,
               class = as.factor(mydata$NRC_Class_Numeric))

class = t$class

#applying PCA
k = prcomp(t[-7], scores = T, cor = T)

class(k$x)

attributes(k)

summary (k)
# taking 5 components with 97.7% proportion of varience


set.seed(1234)
f = data.frame(k$x[,1:5], class)

ind = sample (2, nrow(f), replace = T , prob = c(70,30))
trainData = f[ind == 1 , ]
testData = f[ind ==2, ]

library("kernlab")
m = ksvm(class~.,data = trainData,kernel = "vanilladot")

m
#training error 0.040715

p = predict(m,testData)

# evaluating performance
agreement = p ==testData$class
prop.table(table(agreement))

# Accuracy 95.94









