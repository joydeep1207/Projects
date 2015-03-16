###
#     author : Joydeep
#     Date   : 26/11/2014
#     Topic  : exp 3 (Cluster Analysis)
###


setwd("C:/Users/joydeep/Desktop/DA/Project")
mydata = read.csv("DataPrep1.csv", header = T)

L1 = scale(mydata$L1_Marks)
L2 = scale(mydata$L2_MARKS)
L3 = scale(mydata$L3_MARKS)
S1 = scale(mydata$S1_MARKS)
S2 = scale(mydata$S2_MARKS)
S3 = scale(mydata$S3_MARKS)
entireData = data.frame(L1,L2,L3,S1,S2,S3)

#building clusters using k means clustering Algorithm
entireDataCluster = kmeans(entireData,5)

#size of each clusters
entireDataCluster$size
# the output 4658 8550 6475 8169 4109 seems stable.

#co-ordinates of cluster centeroids
entireDataCluster$centers

entireData$clusters = entireDataCluster$cluster

L1 = as.factor(mydata$L1_CLASS)
L2 = as.factor(mydata$L2_CLASS)
L3 = as.factor(mydata$L3_CLASS)
S1 = as.factor(mydata$S1_CLASS)
S2 = as.factor(mydata$S2_CLASS)
S3 = as.factor(mydata$S3_CLASS)
entireData2 = data.frame(L1,L2,L3,S1,S2,S3,clusters = as.factor(entireData$clusters))

library("arules")

#generating rules 
myrules = apriori(data = entireData2, 
                  parameter = list(support = 0.21, confidence = 0.7, minlen = 2))

myrules1 = subset(myrules, (rhs %pin% "S2"))
myrules1= myrules
inspect(sort(myrules1, by = "lift") )

# pruning the rules
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)


#------------------------------------------Thank you---------------------------------------------
