library("e1071")

pima_data <- read.table("DataSet.csv", header=TRUE, sep=",")

Accuracies = 0
for(ite in 1 : 10)
{
#Dividing data set into Training(90%) and Test(10%)
index     <- 1:nrow(pima_data)
testindex <- sample (index, trunc (length (index)*.9))
pima_train <- pima_data[testindex,]
pima_test  <- pima_data[-testindex,]


model <- naiveBayes(as.factor(Result) ~ ., data = pima_train,na.action = na.pass)
pred <- predict(model, pima_test, type = "raw")


len = 0
len = (length(pred))/2
pred_new = 0
for(i in 1 : len)
{
  if(pred[i] >= 0.5)
  {
    pred_new[i] = 0
  }
  else
  {
    pred_new[i] = 1
  }
}

#Accuracy
count = 0

for (values in pima_test[9])
{
  test_val <- c(values)
  break
}

for(i in 1:len)
{
  if(pred_new[i] == test_val[i])
  {
    count <- count + 1
  }
}
Accuracy <- (count/len)*100
Accuracies[ite] <- Accuracy
Accuracy <- cat(Accuracy,"%  ")
test_val <-NULL
pred_new<- NULL
}
Avg_Acc <- sum(Accuracies)/10
Avg_Acc
