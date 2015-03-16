###
#     author : Joydeep
#     Date   : 26/11/2014
#     Topic  : exp 8 (Cluster Analysis)
###


setwd("C:/Users/joydeep/Desktop/DA/Project")
mydata = read.csv("DataPrep1.csv", header = T)

# preparing data for entire data set
TOTAL_MARKS = scale(  mydata$TOTAL_MARKS)
L1 = scale(mydata$L1_Marks)
L2 = scale(mydata$L2_MARKS)
L3 = scale(mydata$L3_MARKS)
S1 = scale(mydata$S1_MARKS)
S2 = scale(mydata$S2_MARKS)
S3 = scale(mydata$S3_MARKS)
entireData = data.frame(L1,L2,L3,S1,S2,S3,TOTAL_MARKS)

#building clusters using k means clustering Algorithm
entireDataCluster = kmeans(entireData,5)

#size of each clusters
entireDataCluster$size
# the output 4609 8540 8210 6495 4107 seems stable.

#co-ordinates of cluster centeroids
entireDataCluster$centers

# cluster 1 probably are very bad performers
# cluster 2 probably are bad performers
# cluster 3 probably are ok-ok performers
# cluster 4 probably are positive but average performers
# cluster 5 probably are very good performers

entireData$clusters = entireDataCluster$cluster

# see which data point went in which cluster
head(entireData)

# aggregate can be found only on factored data hence factoring each n every column 
t1 = data.frame(L1 = as.factor(entireData$L1), L2 = as.factor(entireData$L2),
                L3 = as.factor(entireData$L3), S1 = as.factor(entireData$S1),
                S2 = as.factor(entireData$S2), S3 = as.factor(entireData$S3),
                TOTAL_MARKS = as.factor(entireData$TOTAL_MARKS),
                clusters = as.factor(entireData$clusters))


aggregate(clusters~ L1,t1, mean)

library("arules")
myrules = apriori(data = t1, 
                  parameter = list(support = 0.05, confidence = 0.4, minlen = 2))
inspect(myrules)

myrules1 = subset(myrules, (rhs %pin% "clusters"))
inspect(sort(myrules1, by = "lift") )

# run this if pruning is required
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)



#----------------------------------------------------------------------------------
#preparing data for male data set

boys <- mydata[ which( mydata$NRC_GENDER_CODE=='B') , ]
TOTAL_MARKS = scale(  boys$TOTAL_MARKS)
L1 = scale(boys$L1_Marks)
L2 = scale(boys$L2_MARKS)
L3 = scale(boys$L3_MARKS)
S1 = scale(boys$S1_MARKS)
S2 = scale(boys$S2_MARKS)
S3 = scale(boys$S3_MARKS)

boysData = data.frame(L1,L2,L3,S1,S2,S3,TOTAL_MARKS)

#building clusters using k means clustering Algorithm
boysDataCluster = kmeans(boysData,5)

#size of each clusters
boysDataCluster$size
# the output 1973 3246 3988 1907 3992 doesn't seems stable.

#co-ordinates of cluster centeroids
boysDataCluster$centers

# cluster 5 probably are very bad performers
# cluster 4 probably are bad performers
# cluster 3 probably are ok-ok performers
# cluster 2 probably are positive but average performers
# cluster 1 probably are very good performers

boysData$clusters = boysDataCluster$cluster

# aggregate can be found only on factored data hence factoring each n every column 
t1 = data.frame(L1 = as.factor(boysData$L1), L2 = as.factor(boysData$L2),
                L3 = as.factor(boysData$L3), S1 = as.factor(boysData$S1),
                S2 = as.factor(boysData$S2), S3 = as.factor(boysData$S3),
                TOTAL_MARKS = as.factor(boysData$TOTAL_MARKS),
                clusters = as.factor(boysData$clusters))


# aggregate(clusters~ L1,t1, mean)

library("arules")
myrules = apriori(data = t1, 
                  parameter = list(support = 0.05, confidence = 0.4, minlen = 2))
inspect(myrules)

myrules1 = subset(myrules, (rhs %pin% "clusters"))
inspect(sort(myrules1, by = "lift") )

# run this if pruning is required
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)






#------------------------------------------------------------------------------------
#preparing data for male data set

girls <- mydata[ which( mydata$NRC_GENDER_CODE=='G') , ]

TOTAL_MARKS = scale(  girls$TOTAL_MARKS)
L1 = scale(girls$L1_Marks)
L2 = scale(girls$L2_MARKS)
L3 = scale(girls$L3_MARKS)
S1 = scale(girls$S1_MARKS)
S2 = scale(girls$S2_MARKS)
S3 = scale(girls$S3_MARKS)


girlsData = data.frame(L1,L2,L3,S1,S2,S3,TOTAL_MARKS)

#building clusters using k means clustering Algorithm
girlsDataCluster = kmeans(girlsData,5)

#size of each clusters
girlsDataCluster$size
# the output 3221 1915 1958 3983 4029 doesn't seems stable.

#co-ordinates of cluster centeroids
girlsDataCluster$centers

# cluster 1 probably are positive average performers
# cluster 2 probably are very bad performers
# cluster 3 probably are very good performers
# cluster 4 probably are ok - ok performers
# cluster 5 probably are bad performers


girlsData$clusters = girlsDataCluster$cluster

# aggregate can be found only on factored data hence factoring each n every column 
t1 = data.frame(L1 = as.factor(girlsData$L1), L2 = as.factor(girlsData$L2),
                L3 = as.factor(girlsData$L3), S1 = as.factor(girlsData$S1),
                S2 = as.factor(girlsData$S2), S3 = as.factor(girlsData$S3),
                TOTAL_MARKS = as.factor(girlsData$TOTAL_MARKS),
                clusters = as.factor(girlsData$clusters))


library("arules")
myrules = apriori(data = t1, 
                  parameter = list(support = 0.05, confidence = 0.4, minlen = 2))
inspect(myrules)

myrules1 = subset(myrules, (rhs %pin% "clusters"))
inspect(sort(myrules1, by = "lift") )

# run this if pruning is required
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)


#--------------------------------------------Thank you-------------------------------------
