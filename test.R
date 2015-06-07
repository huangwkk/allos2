data <- read.csv('test_new_model.csv',header=TRUE)
normalize <-  function(data){
  for (i in 1:ncol(data)){
    min <- min(data[,i])
    max <- max(data[,i])
    data[,i] <- (data[,i] - min)/(max-min)}
  return(data)
}
data<-normalize(data)
print(1)
normalize2 <- function(data){
  data <- (data - min(data))/(max(data)-min(data))
  return(data)
}
data1 <- apply(data,2,normalize2)
data1 <- apply(data,2,function(i))
data1 <- apply(data,2,function(i) (i-min(i))/(max(i)-min(i)))

pca_result <- princomp(data1[,-1],score=TRUE)
plot(pca_result$score[,1],pca_result$score[,2],col=data1$allo,xlab="PC1",ylab="PC2")
logit.fit <- glm(allo~X.Volume.+X.Number.of.Alpha.Spheres.,family=binomial,data=data1)
logit.predictions <- ifelse(predict(logit.fit) > 0,1, 0) 
table(data1$allo,logit.predictions)
qplot(pca_result$score[,1],pca_result$score[,2],color=factor(c))
par(mar=c(4,4,2,2)+1)
plot(pca_result$score[,1],pca_result$score[,2],col=factor(c),xlab='PC1',ylab='PC2')
qplot(data1$X.Volume.,geom="histogram",fill=factor(data1$allo))