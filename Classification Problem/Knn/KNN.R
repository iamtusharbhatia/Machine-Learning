library("e1071")
library("FNN")

pima_data <- read.table("DataSet.csv", header=TRUE, sep=",")

Accuracies = 0
for(ite in 1 : 10)
{
  #Dividing data set into Training(90%) and Test(10%)
  index     <- 1:nrow(pima_data)
  testindex <- sample (index, trunc (length (index)*.9))
  pima_train <- pima_data[testindex,]
  pima_test  <- pima_data[-testindex,]
  
  model <- knn(train = pima_train, test = pima_test, cl = pima_train$Result , k = 11)
  
  len = 0
  len = length(model)
  pred_new = 0
  
  
  #Accuracy
  count = 0
  
  for (values in pima_test[9])
  {
    test_val <- c(values)
    break
  }
  
  for(i in 1:len)
  {
    if(model[i] == test_val[i])
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
