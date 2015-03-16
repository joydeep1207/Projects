library("arules")
library("arulesViz")

rule_mining <- function(cluster_data,antecedents)
{
  mining_data <- cluster_data[,antecedents]
  trans <- as(mining_data , "transactions")
  rules <- apriori(trans,parameter = list(minlen = 2 , supp = 0.005 , conf =0.8),appearance = 
                     list(rhs = c("NRC_CLASS1=PASS" ,"NRC_CLASS1=FIRST" , "NRC_CLASS1=SECOND","NRC_CLASS1=FAIL","NRC_CLASS1=3") , default = "lhs"))
  rules.sorted <- sort(rules , by = "lift")
  class(rules)
  #inspect(rules.sorted)
 
  
  subset.matrix <- is.subset(rules.sorted, rules.sorted)
  
  subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
  redundant <- colSums(subset.matrix, na.rm=T) >= 1
  which(redundant)
  
  # remove redundant rules
  rules.pruned <- rules.sorted[!redundant]
  
  print("printing rules")
  inspect(rules.pruned)
  return(rules.pruned)
  #plot(rules.sorted)
}