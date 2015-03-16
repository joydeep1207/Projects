###
#     author : Joydeep
#     Date   : 26/11/2014
#     Topic  : exp 7 (Association rules vs Decision trees)
###


setwd("C:/Users/joydeep/Desktop/DA/Project")
mydata = read.csv("DataPrep1.csv", header = T)


## ---------------------------applying Association rules---------------------------

t1 = data.frame(NRC_Class_Modified = as.factor(mydata$NRC_Class_Modified),
                L1_CLASS = as.factor(mydata$L1_CLASS),
                L2_CLASS = as.factor(mydata$L2_CLASS),
                L3_CLASS = as.factor(mydata$L3_CLASS),
                S1_CLASS = as.factor(mydata$S1_CLASS),
                S2_CLASS = as.factor(mydata$S2_CLASS),
                S3_CLASS = as.factor(mydata$S3_CLASS))

library("arules")

myrules = apriori(data = t1, 
                  parameter = list(support = 0.25, confidence = 0.5, minlen = 2))

inspect(myrules)
myrulesSub= is.subset(myrules,myrules)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules[!redundant]
inspect(myrulesPrun)

inspect(sort(myrulesPrun, by = "lift")[1:10])

#--------------------------------Making Decision Tree------------------------------------


t2 = data.frame(NRC_Class_Modified = mydata$NRC_Class_Modified,
                L1_CLASS = mydata$L1_CLASS,
                L2_CLASS = mydata$L2_CLASS,
                L3_CLASS = mydata$L3_CLASS,
                S1_CLASS = mydata$S1_CLASS,
                S2_CLASS = mydata$S2_CLASS,
                S3_CLASS = mydata$S3_CLASS)

set.seed(1234)
ind = sample(2, nrow(t2), replace = T , prob = c(70,30))
trainData = t2[ind == 1,]
testData = t2[ind == 2,]

myFormula = NRC_Class_Modified~L1_CLASS+L2_CLASS+L3_CLASS+S1_CLASS+S2_CLASS+S3_CLASS
#using C50 package
library("C50")
tree = C5.0(trainData[-1],trainData$NRC_Class_Modified)
print(tree)
summary(tree)
p = predict(tree,testData)
library("gmodels")
CrossTable(testData$NRC_Class_Modified,p)
summary(tree)


plot(tree, uniform=TRUE, compress=TRUE)
text(tree, use.n=TRUE, all=TRUE, cex=.7)


#------------------------------------Thank you-----------------------------------------------

