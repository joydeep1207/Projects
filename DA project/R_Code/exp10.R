
setwd("C:/Users/joydeep/Desktop/DA/Project/Actual Data")
mydata = read.csv("Group11Data.csv", header = T)
library("sqldf")

t = sqldf("select *
          from mydata a
          Where  L1_RESULT = 'A' or L2_RESULT = 'A' or L3_RESULT = 'A' 
                  or S1_RESULT = 'A' or S2_RESULT = 'A' or S3_RESULT = 'A'    
          ") 

write.csv(t, file = "foo.csv")

#importing data
mydata = read.csv("foo.csv", header = T)

k = mydata

#preparing data
k = data.frame(DIST_CODE = as.factor(k$DIST_CODE),TALUQ_CODE = as.factor(k$TALUQ_CODE),
               SCHOOL_TYPE = as.factor(k$SCHOOL_TYPE), URBAN_RURAL = as.factor(k$URBAN_RURAL),
               NRC_GENDER_CODE = as.factor(k$NRC_GENDER_CODE),NRC_MEDIUM = as.factor(k$NRC_MEDIUM),
               NRC_PHYSICAL_CONDITION = as.factor(k$NRC_PHYSICAL_CONDITION),
               L1_CLASS = as.factor(k$L1_CLASS),L2_CLASS = as.factor(k$L2_CLASS),
               L3_CLASS = as.factor(k$L3_CLASS),S1_CLASS = as.factor(k$S1_CLASS),
               S2_CLASS = as.factor(k$S2_CLASS),S3_CLASS = as.factor(k$S3_CLASS),
               NRC_Class = as.factor(k$NRC_CLASS),
               CANDIDATE_TYPE = as.factor(k$CANDIDATE_TYPE))


library("arules")
myrules = apriori(data = k , 
                  parameter = list(support = 0.1, confidence = 0.6, minlen = 2))


# If we want to see specific in rhs or lhs
myrules1 = subset(myrules, (rhs %pin% "DIST"))
myrules1

# removing redundant 
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
#inspect(myrulesPrun)
inspect(sort(myrulesPrun, by = "lift")[1:10])

#------------------------------------------------------------------------------------

#for Girls cluster
mydata2 <- k[  k$NRC_GENDER_CODE=='G', ]

myrules = apriori(data = mydata2 , 
                  parameter = list(support = 0.5, confidence = 0.6, minlen = 2))


# If we want to see specific in rhs or lhs
myrules1 = subset(myrules, (rhs %pin% "URBAN_RURAL"))
myrules1 = myrules

# removing redundant 
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)
inspect(sort(myrulesPrun, by = "lift")[1:10])

#---------------------------------------------------------------------------
#for Boyss cluster
mydata2 <- k[  k$NRC_GENDER_CODE=='B', ]

myrules = apriori(data = mydata2 , 
                  parameter = list(support = 0.5, confidence = 0.6, minlen = 2))


# If we want to see specific in rhs or lhs
myrules1 = subset(myrules, (rhs %pin% "URBAN_RURAL"))
myrules1 = myrules

# removing redundant 
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)
inspect(sort(myrulesPrun, by = "lift")[1:10])

