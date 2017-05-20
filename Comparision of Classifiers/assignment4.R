#INSTALLS
#install.packages("e1071", dependencies = TRUE)
#install.packages("rpart", dependencies = TRUE)
#install.packages("FNN", dependencies = TRUE)
#install.packages("mlbench", dependencies = TRUE)
#install.packages("nnet", dependencies = TRUE)
#install.packages("randomForest", dependencies = TRUE)
#installed.packages("ipred", dependencies="TRUE")
#install.packages("ada", dependencies = TRUE)

library("e1071")
library("rpart")
library("FNN")
library("mlbench")
library("nnet")
library("randomForest")
library("ipred")
library("MASS")
library("ada")

args <- commandArgs(TRUE)
dataURL<-as.character(args[1])
header<-as.logical(args[2])
d<-read.csv(dataURL,header = header)
c_id <- as.integer(args[3])

Class<-d[,as.integer(c_id)]  

for(i in 1:10)
{
  sampleInstances<-sample(1:nrow(d),size = 0.9*nrow(d))
  trainingData<-d[sampleInstances,]
  testData<-d[-sampleInstances,]
  
  for(j in 1:1)
  {
    cat("\n\nRunning sample ",i,":\n\n")
    set.seed(123)
    d[,c_id] <- as.numeric(d[,c_id])
    if(length(d) == 35 && c_id == 2)
    {
      d$V35 <- as.numeric(d$V35)
      trainingData$V35 <- as.numeric(trainingData$V35)
      testData$V35 <- as.numeric(testData$V35)
    }
    
    trainingData[,c_id] <- as.factor(trainingData[,c_id])
    testData[,c_id] <- as.factor(testData[,c_id])
    
    ClassName<- names(trainingData[c_id])
    c_names <- colnames(d)
    formula1 <- as.formula(paste(ClassName," ~ ."))
    
    
    #*******************************************************************************************************
    # Decision Trees 
    #*******************************************************************************************************
    
    model <- rpart (as.formula(formula1), data=trainingData,method="class")
    pred <- predict(model, testData, decision.values = TRUE, type = "class")
    
    method="Decision Trees"
    accuracy <- sum(testData[,c_id]==pred)/length(pred)
    cat("Method = ", method,", accuracy= ", accuracy,"\n")
    
    #*******************************************************************************************************
    # Support Vector Machines
    #*******************************************************************************************************
    
    model <- svm(as.formula(formula1), data = trainingData, na.action = na.pass, kernel="sigmoid")
    pred <- predict(model, testData, type = "class")
    
    method="Support Vector Machines"
    accuracy <- sum(testData[,c_id]==pred)/length(pred)
    cat("Method = ", method,", accuracy= ", accuracy,"\n")
    
    
    #*******************************************************************************************************
    # Naive Bayesian 
    #*******************************************************************************************************
    
    model <- naiveBayes(as.formula(formula1), data = trainingData,na.action = na.pass)
    pred <- predict(model, testData, type = "class")
    
    method="Naive Bayesian"
    accuracy <- sum(testData[,c_id]==pred)/length(pred)
    cat("Method = ", method,", accuracy= ", accuracy,"\n")
    
    #*******************************************************************************************************
    # kNN 
    #*******************************************************************************************************
    
    temp_trainingData <- trainingData
    temp_testData     <- testData
    temp_trainingData[,c_id] <- as.numeric(temp_trainingData[,c_id])
    temp_testData[,c_id] <- as.numeric(temp_testData[,c_id])
    
    model <- knn(train = temp_trainingData, test = temp_testData, cl = temp_trainingData[,c_id], k = 11)
    
    method="kNN"
    accuracy <- sum(temp_testData[,c_id]==model)/length(model)
    cat("Method = ", method,", accuracy= ", accuracy,"\n")
    
    #*******************************************************************************************************
    # Logistic Regression 
    #*******************************************************************************************************
    
    temp_trainingData <- trainingData
    temp_testData     <- testData
    temp_trainingData[,c_id] <- as.numeric(temp_trainingData[,c_id])
    temp_testData[,c_id] <- as.numeric(temp_testData[,c_id])
    
    temp_trainingData[,c_id] <- temp_trainingData[,c_id] - 1 
    temp_testData[,c_id] <- temp_testData[,c_id] - 1 
    
    model <- glm(as.formula(formula1), data = temp_trainingData, family = "binomial")
    p <- predict(model,newdata = temp_testData, type = "response")
    
    threshold=0.50
    pred<-sapply(p, FUN=function(x) if (x>threshold) 1 else 0)
    
    method="Logistic Regression"
    accuracy <- sum(temp_testData[,c_id]==pred)/length(pred)
    cat("Method = ", method,", accuracy= ", accuracy,"\n")
    
    
    #*******************************************************************************************************
    # Neural Network 
    #*******************************************************************************************************
    
    temp_trainingData <- trainingData
    temp_testData     <- testData
    temp_trainingData[,c_id] <- as.numeric(temp_trainingData[,c_id])
    temp_testData[,c_id] <- as.numeric(temp_testData[,c_id])
    
    out <-temp_trainingData[,c_id]
    input <- temp_trainingData[,-c_id]
    neur <- nnet(input,out,size=4,maxit=50) #function
    pred<-  predict(neur,temp_testData) 
    
    method="Neural Network"
    accuracy <- sum(temp_testData[,c_id]==pred)/length(pred)
    cat("Method = ", method,", accuracy= ", accuracy,"\n")
    
    #*******************************************************************************************************
    # Bagging 
    #*******************************************************************************************************
    
    library("ipred")
    library("MASS")
    
    model <- bagging( as.formula(formula1), data = trainingData)
    pred  <- predict(model, testData, type="class")
    
    method="Bagging"
    accuracy <- sum(testData[,c_id]==pred)/length(pred)
    cat("Method = ", method,", accuracy= ", accuracy,"\n")
    
    #*******************************************************************************************************
    # Random Forest 
    #*******************************************************************************************************
    
    model <-randomForest(as.formula(formula1), data=trainingData, mtry=2, ntree=1000, 
                         keep.forest=TRUE, importance=TRUE)
    pred = predict(model,newdata=testData,type="class")
    
    method="Random Forest"
    accuracy <- sum(testData[,c_id]==pred)/length(pred)
    cat("Method = ", method,", accuracy= ", accuracy,"\n")
    
    #*******************************************************************************************************
    # Boosting 
    #*******************************************************************************************************
    
    model <- ada(as.formula(formula1), data = trainingData, iter=20, nu=1, type="discrete")
    pred  <- predict(model,testData)
    
    method="Boosting"
    accuracy <- sum(testData[,c_id]==pred)/length(pred)
    cat("Method = ", method,", accuracy= ", accuracy,"\n")
    
  }  
}




