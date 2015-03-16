###
#     author : Joydeep
#     Date   : 26/11/2014
#     Topic  : exp 6 (Performance characteristics)
###

setwd("C:/Users/joydeep/Desktop/DA/Project")
mydata = read.csv("DataPrep1.csv", header = T)


newdata2 <- mydata[ which( (mydata$NRC_Class_Numeric==1) |(mydata$NRC_Class_Numeric ==2)), ]

t1 = data.frame(SCHOOL_TYPE = as.factor(newdata2$SCHOOL_TYPE),
                URBAN_RURAL = as.factor(newdata2$URBAN_RURAL),
                NRC_CASTE_CODE = as.factor(newdata2$NRC_CASTE_CODE),
                NRC_GENDER_CODE = as.factor(newdata2$NRC_GENDER_CODE),
                NRC_MEDIUM = as.factor(newdata2$NRC_MEDIUM),
                NRC_PHYSICAL_CONDITION = as.factor(newdata2$NRC_PHYSICAL_CONDITION),
                CANDIDATE_TYPE = as.factor(newdata2$CANDIDATE_TYPE),
                NRC_Class_Modified = as.factor(newdata2$NRC_Class_Modified))

library("arules")

#generating rules for FAIL
myrules = apriori(data = t1, 
                  parameter = list(support = 0.5, confidence = 0.7, minlen = 2))

myrules1 = subset(myrules, (rhs %pin% "NRC_Class_Modified=FAIL"))
inspect(sort(myrules1, by = "lift") )

# pruning the rules
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)



# generating rules for D
myrules = apriori(data = t1, 
                  parameter = list(support = 0.1, confidence = 0.5, minlen = 2))

myrules1 = subset(myrules, (rhs %pin% "NRC_Class_Modified=D"))
inspect(sort(myrules1, by = "lift") )

# run this if pruning is required
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)




#-----------------------------------------Part 2---------------------------------------------


t1 = data.frame(SCHOOL_TYPE = as.factor(newdata2$SCHOOL_TYPE),
                URBAN_RURAL = as.factor(newdata2$URBAN_RURAL),
                NRC_CASTE_CODE = as.factor(newdata2$NRC_CASTE_CODE),
                NRC_GENDER_CODE = as.factor(newdata2$NRC_GENDER_CODE),
                NRC_MEDIUM = as.factor(newdata2$NRC_MEDIUM),
                NRC_PHYSICAL_CONDITION = as.factor(newdata2$NRC_PHYSICAL_CONDITION),
                CANDIDATE_TYPE = as.factor(newdata2$CANDIDATE_TYPE),
                NRC_Class_Modified = as.factor(newdata2$NRC_Class_Modified))


t1 = data.frame(t1,L1_CLASS = as.factor(newdata2$L1_CLASS),
                L2_CLASS = as.factor(newdata2$L2_CLASS),
                L3_CLASS = as.factor(newdata2$L3_CLASS),
                S1_CLASS = as.factor(newdata2$S1_CLASS),
                S2_CLASS = as.factor(newdata2$S2_CLASS),
                S3_CLASS = as.factor(newdata2$S3_CLASS))

library("arules")


#generating rules for FAIL
myrules = apriori(data = t1, 
                  parameter = list(support = 0.5, confidence = 0.7, minlen = 2))

myrules1 = subset(myrules, (rhs %pin% "NRC_Class_Modified=FAIL"))
inspect(sort(myrules1, by = "lift") )

#pruning the rules to get best rules
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)



# generating rules for D
myrules = apriori(data = t1, 
                  parameter = list(support = 0.15, confidence = 0.6, minlen = 2))

myrules1 = subset(myrules, (rhs %pin% "NRC_Class_Modified=D"))
inspect(sort(myrules1, by = "lift") )

#pruning to get non redundant attributes in the rule 
myrulesSub= is.subset(myrules1,myrules1)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules1[!redundant]
inspect(myrulesPrun)



#------------------------------------------------Thank you-----------------------------------------------