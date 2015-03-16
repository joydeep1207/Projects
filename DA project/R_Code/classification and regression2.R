## 
#     Author : Joydeep
#     Date   : 26/11/2014
#     Topic  : Exp 2 (Regression and classification)
##

#-------------------------------------------------------------------------------------------


setwd("C:/Users/joydeep/Desktop/DA/Project")
mydata = read.csv("DataPrep.csv",header = T)

L1 = mydata$L1_Marks
L2 = mydata$L2_MARKS
L3 = mydata$L3_MARKS
S1 = mydata$S1_MARKS
S2 = mydata$S2_MARKS
S3 = mydata$S3_MARKS

# getting z scores
L1_z=scale(L1)
L2_z=scale(L2)
L3_z=scale(L3)
S1_z=scale(S1)
S2_z=scale(S2)
S3_z=scale(S3)
Total_z = scale(mydata$TOTAL_MARKS)

mydata2 = data.frame(L1 = L1_z,L2 = L2_z,L3 = S3_z,L1 = S1_z,S2 = S2_z,S3 = S3_z, Total =Total_z)


regreData=lm(Total~L1*L2*L3*S1*S2*S3, data = mydata2)
summary(regreData)

# for five independent variables r sruared error 99.37
regreData=lm(TOTAL_MARKS~L1_Marks+L2_MARKS+L3_MARKS+S1_MARKS+S3_MARKS, data = mydata)
# for four independent variables r squared error 97.68
regreData=lm(TOTAL_MARKS~L1_Marks+L2_MARKS+L3_MARKS+S2_MARKS, data = mydata)
summary(regreData)


#importance of attributes S2>L1>L2>S1>S3>L3
#taking combination of    S2>L1>L2>S1>S3 R-squared:  0.9868
regreData=lm(TOTAL_MARKS~L1_Marks+L2_MARKS+S1_MARKS+S2_MARKS+S3_MARKS, data = mydata)

#taking combination of    S2>L1>L2>S1    R-squared:  0.9725
regreData=lm(TOTAL_MARKS~L1_Marks+L2_MARKS+S1_MARKS+S2_MARKS, data = mydata)

#taking combination of    S2>L1>L2       R-squared:  0.955
regreData=lm(TOTAL_MARKS~L2_MARKS+L1_Marks+S2_MARKS, data = mydata)
summary(regreData)


# regreData1=lm(TOTAL_MARKS~L1_Marks+L2_MARKS+L3_MARKS+S1_MARKS+S2_MARKS+S3_MARKS, data = mydata)
# regreData2=lm(TOTAL_MARKS~L1_Marks+L3_MARKS+S1_MARKS+S2_MARKS+S3_MARKS, data = mydata)
# anova(regreData1,regreData2)

L1 = mydata$L1_Marks
L2 = mydata$L2_MARKS
L3 = mydata$L3_MARKS
S1 = mydata$S1_MARKS
S2 = mydata$S2_MARKS
S3 = mydata$S3_MARKS

# getting z scores
L1_z=scale(L1)
L2_z=scale(L2)
L3_z=scale(L3)
S1_z=scale(S1)
S2_z=scale(S2)
S3_z=scale(S3)

mydata = read.csv("DataPrep1.csv",header = T)
marksClass= mydata$NRC_Class_Numeric
myNewData_z = data.frame(L1_z,L2_z,L3_z,S1_z,S2_z,S3_z,marksClass)

set.seed(1234)
ind = sample(2, nrow(myNewData_z), replace = T , prob = c(70,30))
trainData = myNewData_z[ind == 1,]
testData = myNewData_z[ind == 2,]

testData1=data.frame(L1 =testData$L1_z, S1 = testData$S1_z, 
                     marksClass = testData$marksClass)

trainData1=data.frame(L1 = trainData$L1_z,S1 = trainData$S1_z,
                      marksClass = trainData$marksClass)

trainDataLevels = trainData1$marksClass
testDataLevels = testData1$marksClass

library("class")
p = knn(train= trainData1,test = testData1,cl = trainDataLevels, k =149)

# manually checking accuracy
summary(p)
t =testData1$marksClass== 2
z = testData[t == T,]


library("gmodels")
CrossTable(x=testDataLevels, y = p, prop.chisq = F)

# ----------------------------part 2 ---------------------------------------------



