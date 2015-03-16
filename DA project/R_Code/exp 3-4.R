## 
#     Author : Joydeep
#     Date   : 26/11/2014
#     Topic  : Exp 4 (Confidence Interval)
##

#---------------------------------part 1-----------------------------------------------


setwd("C:/Users/joydeep/Desktop/DA/Project")
mydata = read.csv("DataPrep1.csv", header = T)
library("sqldf")
t = sqldf("select distinct DIST_CODE , count(*) TotalStudent, (select count(*) from mydata b where a.DIST_CODE = b.DIST_CODE and NRC_RESULT = 'P') PASS
      from mydata a
      group by DIST_CODE
      ") 
x = 1
passPercent = t$PASS*100/t$TotalStudent
t = data.frame(t,passPercent)
t = sqldf("select * from t order  by passPercent desc")
hist(t$passPercent)
t.test(t$passPercent)
t.test(t$passPercent, conf.level = 0.99)

t$category = with(t, ifelse(passPercent > 81.0, 'High',
                             ifelse(passPercent <= 81.0 & passPercent >= 76.3,  'Medium',
                                    ifelse(passPercent < 76.3,  'Low',''))))

# include arules package
library("arules")
t1 = data.frame(as.factor(t$DIST_CODE),as.factor(t$TotalStudent),
                as.factor(t$PASS),as.factor(t$passPercent),
                as.factor(t$category))


# t1 length = 5
myrules = apriori(data = t1, 
                  parameter = list(support = 0.029, confidence = 0.5, minlen = 2))

summary(myrules)
#large value of lift => rule is imp

inspect(myrules[1:3])
#best results
inspect(myrules[251:253])
#another way
inspect(sort(myrules, by = "lift")[1:10])

tail(t)

myrulesSub= is.subset(myrules,myrules)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules[!redundant]
inspect(myrulesPrun)

inspect(sort(myrulesPrun, by = "lift")[1:10])

#----------------------generation rules based on top 2 districts------------------------

# extracting top 2 disticts that is PA and GA
newdata <- mydata[ which( (mydata$DIST_CODE == "PA" ) |(mydata$DIST_CODE =="GA" )), ]

newdata1 = newdata[,c(-6,-11,-16,-19,-22,-25,-28,-31,-34,-38)]

k = newdata1[ , c(1,2,4,5,10,11,12,15,17,19,21,23,25,28,29)]

k = data.frame(DIST_CODE = as.factor(k$DIST_CODE),TALUQ_CODE = as.factor(k$TALUQ_CODE),
               SCHOOL_TYPE = as.factor(k$SCHOOL_TYPE), URBAN_RURAL = as.factor(k$URBAN_RURAL),
               NRC_GENDER_CODE = as.factor(k$NRC_GENDER_CODE),NRC_MEDIUM = as.factor(k$NRC_MEDIUM),
               NRC_PHYSICAL_CONDITION = as.factor(k$NRC_PHYSICAL_CONDITION),
               L1_CLASS = as.factor(k$L1_CLASS),L2_CLASS = as.factor(k$L2_CLASS),
               L3_CLASS = as.factor(k$L3_CLASS),S1_CLASS = as.factor(k$S1_CLASS),
               S2_CLASS = as.factor(k$S2_CLASS),S3_CLASS = as.factor(k$S3_CLASS),
               NRC_Class_Modified = as.factor(k$NRC_Class_Modified),
               CANDIDATE_TYPE = as.factor(k$CANDIDATE_TYPE))

k

# distData = as( k , "transactions");

class(k$CANDIDATE_TYPE)

library(arules)
#getting rules of it
myrules = apriori(data = k , 
                  parameter = list(support = 0.5, confidence = 0.7, minlen = 2))


# If we want to see specific in rhs or lhs
myrules1 = subset(myrules, (rhs %pin% "DIST_CODE="))

# pruning the rules
myrulesSub= is.subset(myrules,myrules)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules[!redundant]
inspect(myrulesPrun)

inspect(sort(myrulesPrun, by = "lift")[1:10])

# it had 38 rules but after pruning it has 10 rules

#----------------------------------bottom 2 districts----------

newdata <- mydata[ which( (mydata$DIST_CODE == "QA" ) |(mydata$DIST_CODE =="SS" )), ]

newdata1 = newdata[,c(-6,-11,-16,-19,-22,-25,-28,-31,-34,-38)]

k = newdata1[ , c(1,2,4,5,10,11,12,15,17,19,21,23,25,28,29)]

k = data.frame(DIST_CODE = as.factor(k$DIST_CODE),TALUQ_CODE = as.factor(k$TALUQ_CODE),
               SCHOOL_TYPE = as.factor(k$SCHOOL_TYPE), URBAN_RURAL = as.factor(k$URBAN_RURAL),
               NRC_GENDER_CODE = as.factor(k$NRC_GENDER_CODE),NRC_MEDIUM = as.factor(k$NRC_MEDIUM),
               NRC_PHYSICAL_CONDITION = as.factor(k$NRC_PHYSICAL_CONDITION),
               L1_CLASS = as.factor(k$L1_CLASS),L2_CLASS = as.factor(k$L2_CLASS),
               L3_CLASS = as.factor(k$L3_CLASS),S1_CLASS = as.factor(k$S1_CLASS),
               S2_CLASS = as.factor(k$S2_CLASS),S3_CLASS = as.factor(k$S3_CLASS),
               NRC_Class_Modified = as.factor(k$NRC_Class_Modified),
               CANDIDATE_TYPE = as.factor(k$CANDIDATE_TYPE))



library(arules)
#getting rules of it
myrules = apriori(data = k , 
                  parameter = list(support = 0.5, confidence = 0.7, minlen = 2))


# If we want to see specific in rhs or lhs
myrules1 = subset(myrules, (rhs %pin% "DIST_CODE="))
inspect(myrules1)
# pruning the rules
myrulesSub= is.subset(myrules,myrules)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules[!redundant]
inspect(myrulesPrun)

inspect(sort(myrulesPrun, by = "lift")[1:10])



#------------------------------------------------Part 2--------------------------
# same for schools

library("sqldf")
t = sqldf("select distinct SCHOOL_CODE , count(*) TotalStudent, 
            (select count(*) from mydata b where a.SCHOOL_CODE = b.SCHOOL_CODE and NRC_RESULT = 'P') PASS
              from mydata a
                group by SCHOOL_CODE")


passPercent = t$PASS*100/t$TotalStudent
t = data.frame(t,passPercent)
t = sqldf("select * from t order  by passPercent desc")

t.test(t$passPercent, conf.level = 0.99)
category = passPercent
t = data.frame(t,category)
t$category <- with(t, ifelse(passPercent > 79.8 , 'High',
                            ifelse(passPercent <= 79.8 & passPercent >= 78.6, 'Medium',
                                   ifelse(passPercent < 78.6 , 'Low',""))))

# checking the order
topSchools = sqldf("select * from t where category = 'High' order  by passPercent desc")
bottomSchools = sqldf("select * from t where category = 'Low' order  by passPercent ")

# ----------------------------taking data based on the new top schools---------------------------
newdata <- mydata[ which( (mydata$SCHOOL_CODE == "CC0054" ) |(mydata$SCHOOL_CODE =="DD0038" ) |
                          (mydata$SCHOOL_CODE == "EE0101" ) |(mydata$SCHOOL_CODE =="DD0070" ) |
                          (mydata$SCHOOL_CODE == "FF0215" ) |(mydata$SCHOOL_CODE =="DD0283" ) |
                          (mydata$SCHOOL_CODE == "KK0047" ) |(mydata$SCHOOL_CODE =="AS0020" ) |
                          (mydata$SCHOOL_CODE == "LL0056" ) |(mydata$SCHOOL_CODE =="TT0003" ) |
                          (mydata$SCHOOL_CODE == "NA0011" ) |(mydata$SCHOOL_CODE =="II0028" ) |
                          (mydata$SCHOOL_CODE == "QQ0064" ) |(mydata$SCHOOL_CODE =="GG0021" ) |
                          (mydata$SCHOOL_CODE == "EE0040" ) |(mydata$SCHOOL_CODE =="NN0025" ) |
                          (mydata$SCHOOL_CODE == "PA0002" ) |(mydata$SCHOOL_CODE =="MM0038" ) |
                          (mydata$SCHOOL_CODE == "GG0030" ) |(mydata$SCHOOL_CODE =="FF0010" )), ]

newdata1 = newdata[,c(-6,-11,-16,-19,-22,-25,-28,-31,-34,-38)]

k = newdata1[ , c(1,2,4,5,10,11,12,15,17,19,21,23,25,28,29)]

k = data.frame(DIST_CODE = as.factor(k$DIST_CODE),TALUQ_CODE = as.factor(k$TALUQ_CODE),
               SCHOOL_TYPE = as.factor(k$SCHOOL_TYPE), URBAN_RURAL = as.factor(k$URBAN_RURAL),
               NRC_GENDER_CODE = as.factor(k$NRC_GENDER_CODE),NRC_MEDIUM = as.factor(k$NRC_MEDIUM),
               NRC_PHYSICAL_CONDITION = as.factor(k$NRC_PHYSICAL_CONDITION),
               L1_CLASS = as.factor(k$L1_CLASS),L2_CLASS = as.factor(k$L2_CLASS),
               L3_CLASS = as.factor(k$L3_CLASS),S1_CLASS = as.factor(k$S1_CLASS),
               S2_CLASS = as.factor(k$S2_CLASS),S3_CLASS = as.factor(k$S3_CLASS),
               NRC_Class_Modified = as.factor(k$NRC_Class_Modified),
               CANDIDATE_TYPE = as.factor(k$CANDIDATE_TYPE))



library(arules)
#getting rules of it
myrules = apriori(data = k , 
                  parameter = list(support = 0.3, confidence = 0.7, minlen = 2))


# If we want to see specific in rhs or lhs
myrules1 = subset(myrules, (rhs %pin% "sCHOOL_CODE="))

# pruning the rules
myrulesSub= is.subset(myrules,myrules)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules[!redundant]
inspect(myrulesPrun)

inspect(sort(myrulesPrun, by = "lift")[1:10])

#-------------------------------sCHOOLS PERFORMING BAD --------------------------------------

newdata <- mydata[ which(   (mydata$SCHOOL_CODE == "TT0004" ) |(mydata$SCHOOL_CODE =="KK0001" ) |
                            (mydata$SCHOOL_CODE == "TT0002" ) |(mydata$SCHOOL_CODE =="RA0011" ) |
                            (mydata$SCHOOL_CODE == "TT0015" ) |(mydata$SCHOOL_CODE =="AN0094" ) |
                            (mydata$SCHOOL_CODE == "JJ0006" ) |(mydata$SCHOOL_CODE =="AN0079" ) |
                            (mydata$SCHOOL_CODE == "OO0002" ) |(mydata$SCHOOL_CODE =="AN0100" ) |
                            (mydata$SCHOOL_CODE == "EE0019" ) |(mydata$SCHOOL_CODE =="OA0003" ) |
                            (mydata$SCHOOL_CODE == "EE0055" ) |(mydata$SCHOOL_CODE =="TT0005" ) |
                            (mydata$SCHOOL_CODE == "EE0061" ) |(mydata$SCHOOL_CODE =="GG0009" ) |
                            (mydata$SCHOOL_CODE == "RR0050" ) |(mydata$SCHOOL_CODE =="BB0047" ) |
                            (mydata$SCHOOL_CODE == "CA0001" ) |(mydata$SCHOOL_CODE =="KK0011" ) |
                            (mydata$SCHOOL_CODE == "CC0016" ) |(mydata$SCHOOL_CODE =="SS0023" ) |
                            (mydata$SCHOOL_CODE == "CC0009" ) |(mydata$SCHOOL_CODE =="AS0012" )), ]

newdata1 = newdata[,c(-6,-11,-16,-19,-22,-25,-28,-31,-34,-38)]

k = newdata1[ , c(1,2,4,5,10,11,12,15,17,19,21,23,25,28,29)]

k = data.frame(DIST_CODE = as.factor(k$DIST_CODE),TALUQ_CODE = as.factor(k$TALUQ_CODE),
               SCHOOL_TYPE = as.factor(k$SCHOOL_TYPE), URBAN_RURAL = as.factor(k$URBAN_RURAL),
               NRC_GENDER_CODE = as.factor(k$NRC_GENDER_CODE),NRC_MEDIUM = as.factor(k$NRC_MEDIUM),
               NRC_PHYSICAL_CONDITION = as.factor(k$NRC_PHYSICAL_CONDITION),
               L1_CLASS = as.factor(k$L1_CLASS),L2_CLASS = as.factor(k$L2_CLASS),
               L3_CLASS = as.factor(k$L3_CLASS),S1_CLASS = as.factor(k$S1_CLASS),
               S2_CLASS = as.factor(k$S2_CLASS),S3_CLASS = as.factor(k$S3_CLASS),
               NRC_Class_Modified = as.factor(k$NRC_Class_Modified),
               CANDIDATE_TYPE = as.factor(k$CANDIDATE_TYPE))



library(arules)
#getting rules of it
myrules = apriori(data = k , 
                  parameter = list(support = 0.5, confidence = 0.7, minlen = 2))


# If we want to see specific in rhs or lhs
myrules1 = subset(myrules, (rhs %pin% "sCHOOL_CODE="))

# pruning the rules
myrulesSub= is.subset(myrules,myrules)
myrulesSub[lower.tri(myrulesSub, diag=T)]=NA
redundant = colSums(myrulesSub, na.rm = T)>=1
which(redundant)

#removing the redundant
myrulesPrun =myrules[!redundant]
inspect(myrulesPrun)

inspect(sort(myrulesPrun, by = "lift")[1:10])

myrules1 = subset(myrulesPrun, (rhs %pin% "sCHOOL_CODE="))
inspect(sort(myrulesPrun, by = "lift")[1:10])
--------------------------------------------Thank You---------------------------------------------------
