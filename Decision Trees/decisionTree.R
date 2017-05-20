library(rpart)
##DATA SET

#for command line arguments 
args <- commandArgs(TRUE) 

# Read Training Data 
train_data1 <- read.csv(file=args[1],header = TRUE, sep = ",") 

# Read Validation Data 
valid_data <- read.csv(file=args[2],header = TRUE, sep = ",") 

# Read Test Data 
test_data1 <- read.csv(file=args[3],header = TRUE, sep = ",") 


#train_data1 <- read.table(file ="E:\\MS in CS\\ML\\Assignments\\Assign2\\data_sets1\\training_set.csv", header=TRUE, sep=",")
#test_data1 <- read.table(file ="E:\\MS in CS\\ML\\Assignments\\Assign2\\data_sets1\\test_set.csv", header=TRUE, sep=",")

train_data1<- rbind(train_data1, valid_data)

fit1 <- rpart ( Class~XB+XC+XD+XE+XF+XG+XH+XI+XJ+XK+XL+XM+XN+XO+XP+XQ+XR+XS+XT+XU, data=train_data1, method="class")

# summary
printcp(fit1)

#Plotting Tree
plot(fit1, uniform=TRUE,    main = "Analysis on Data Set")
text(fit1, use.n=TRUE)

#Pruning Tree
p_tree <- prune(fit1, cp= 0.011667)
plot(p_tree, uniform=TRUE,    main = "Pruned Data Set")
text(p_tree, use.n=TRUE)

#Predicting values
model <- rpart(Class~XB+XC+XD+XE+XF+XG+XH+XI+XJ+XK+XL+XM+XN+XO+XP+XQ+XR+XS+XT+XU, data = train_data1, method = "class")
pred <- predict(model, test_data1, decision.values = TRUE, type = "class")

plot(pred, main="Predicted values for Data Set")

#Accuracy
for (values in test_data1[21])
{
  x <- c(values)
  break}

y <- NULL
for (values in pred) {
  y <- c(y,values) }

count = 0
for(i in 1:length(x))
{ 
  if(x[i] == y[i])
  {
    count <- count + 1
  }
}
Accuracy <- (count/length(x))*100
Accuracy <- cat("Accuracy of Data Set =",Accuracy,"%")
