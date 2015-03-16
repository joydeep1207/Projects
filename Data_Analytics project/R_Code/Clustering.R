mykmeans <- function(data , columns_vector , Centers)
{
  kclust <- kmeans(data[,columns_vector],centers=Centers,nstart = 5)
  table(kclust$cluster , data$NRC_CLASS1)
  return(kclust)
}