#
##  Author : Joydeep
##  Date   : 26/11/2014
##  Topic  : exp 5 (Urban / Rural characterization)
#

#-----------------------------------------Part 1----------------------------------------------

setwd("C:/Users/joydeep/Desktop/DA/Project")
mydata = read.csv("DataPrep1.csv", header = T)

t = data.frame(URBAN_RURAL = as.factor(mydata$URBAN_RURAL),
               SCHOOL_TYPE = as.factor(mydata$SCHOOL_TYPE),
               NRC_CASTE_CODE = as.factor(mydata$NRC_CASTE_CODE),
               NRC_GENDER_CODE = as.factor(mydata$NRC_GENDER_CODE),
               NRC_MEDIUM = as.factor(mydata$NRC_MEDIUM),
               NRC_PHYSICAL_CONDITION = as.factor(mydata$NRC_PHYSICAL_CONDITION),
               CANDIDATE_TYPE = as.factor(mydata$CANDIDATE_TYPE))

library("arules")
myrules = apriori(data = t, 
                  parameter = list(support = 0.4, confidence = 0.7, minlen = 2))

# 42 rules
# rules related to urban rural
myrules1 = subset(myrules, (rhs %pin% "URBAN_RURAL="))
inspect(sort(myrules1, by = "lift")[1:4] )

summary(myrules1)

inspect(sort(myrules, by = "lift")[1:5])

# removing redundant 
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)

#----------------------------------------------Part 2---------------------------------------------
t = data.frame(t,L1_CLASS = as.factor(mydata$L1_CLASS),
               L2_CLASS = as.factor(mydata$L2_CLASS),
               L3_CLASS = as.factor(mydata$L3_CLASS),
               S1_CLASS = as.factor(mydata$S1_CLASS),
               S2_CLASS = as.factor(mydata$S2_CLASS),
               S3_CLASS = as.factor(mydata$S3_CLASS))

myrules = apriori(data = t, 
                  parameter = list(support = 0.3, confidence = 0.5, minlen = 2))

#216 Rules
myrules1 = subset(myrules, (rhs %pin% "URBAN_RURAL="))
inspect(sort(myrules1, by = "lift") )

summary(myrules1)

# pruning the rules
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)

inspect(sort(myrulesPrun, by = "lift")[1:6])

# see the effect

#--------------------------------------Thank You------------------------------------------------------
