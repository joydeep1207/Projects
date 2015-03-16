setwd("C:/Users/joydeep/Desktop/DA/Project")
source("Load.R")
source("Preprocessing.R")
source("Clustering.R")
source("RuleMining.R")
source("PLot.R")
library(lattice)
filename <- "D:/IIITB/SEM 3/DA/Project/Data/Group6DataV1.csv"
#load data
data <- load_Data(filename)

#Predictor variables
columns <- c(16,19,22,25,28,31)

#Running Kmeans Clustering 
centers <- 3
kclust <- mykmeans(data , columns , centers)
table(kclust$cluster , data$NRC_CLASS1)

antecedents1 <- c(18,21,24,27,30,33,37)

#Mining based on Discretized subject marks
cluster_data <- data[kclust$cluster == 1,]
rules <- rule_mining(cluster_data , antecedents1 )
plot(rules, method="matrix", measure="lift", control=list(reorder=TRUE))
#plot(rules, method="matrix", measure=c("lift", "confidence"),  control=list(reorder=TRUE))
#myplot(cluster_data,1)


cluster_data <- data[kclust$cluster == 2,]
rules <- rule_mining(cluster_data , antecedents1)
plot(rules, method="matrix", measure="lift", control=list(reorder=TRUE))
#plot(rules, method="matrix", measure=c("lift", "confidence"),  control=list(reorder=TRUE))
#myplot(cluster_data,2)


cluster_data <- data[kclust$cluster == 3,]
rules <- rule_mining(cluster_data, antecedents1)
plot(rules, method="matrix", measure="lift", control=list(reorder=TRUE))
#plot(rules, method="matrix", measure=c("lift", "confidence"),  control=list(reorder=TRUE))
#myplot(cluster_data,3)

