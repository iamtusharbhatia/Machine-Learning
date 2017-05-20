library(rpart)

pima_data <- read.table("DataSet.csv", header=TRUE, sep=",")

summary(pima_data)
summary(pima_data$Result)

sort(pima_data$BP)

pima_data$Result <- factor(pima_data$Result) 
summary(pima_data$Result)

#Pregnancy
hist(pima_data$Pregnant,plot = TRUE, right = FALSE, labels = FALSE, main = "Pima Histo_Preg")
no <- table(pima_data$Pregnant)
barplot(no, main="Pregnancy Bar_Distribution") 

#Glucose
hist(pima_data$Glucose,plot = TRUE, right = FALSE, labels = FALSE, main = "Pima Histo_Glucose")
no <- table(pima_data$Glucose)
barplot(no, main="Glucose Bar_Distribution") 

#BP
hist(pima_data$BP,plot = TRUE, right = FALSE, labels = FALSE, main = "Pima Histo_BP")
no <- table(pima_data$BP)
barplot(no, main="BP Bar_Distribution") 

#Thickness
hist(pima_data$Thickness,plot = TRUE, right = FALSE, labels = FALSE, main = "Pima Histo_Thickness")
no <- table(pima_data$Thickness)
barplot(no, main="Thickness Bar_Distribution") 

#Insulin
hist(pima_data$Insulin,plot = TRUE, right = FALSE, labels = FALSE, main = "Pima Histo_Insulin")
no <- table(pima_data$Insulin)
barplot(no, main="Insulin Bar_Distribution") 

#BMI
hist(pima_data$BMI,plot = TRUE, right = FALSE, labels = FALSE, main = "Pima Histo_BMI")
no <- table(pima_data$BMI)
barplot(no, main="BMI Bar_Distribution") 

#Pedigree
hist(pima_data$Pedigree,plot = TRUE, right = FALSE, labels = FALSE, main = "Pima Histo_Pedigree")
no <- table(pima_data$Pedigree)
barplot(no, main="Pedigree Bar_Distribution") 

#Age
hist(pima_data$Age,plot = TRUE, right = FALSE, labels = FALSE, main = "Pima Histo_Age")
no <- table(pima_data$Age)
barplot(no, main="Age Bar_Distribution") 


pima_data$Result<-as.numeric(as.character(pima_data$Result))
cor(pima_data, use="complete.obs")

max = 0
a = 0
b = 0
for(x in 1:8)
{
  for(y in (x+1):8)
  {
    if( x != y)
    {
      temp <- cor(pima_data[x], pima_data[y])
      if( max < temp[1])
      {
        max <- temp[1]
        a <- x
        b <- y
      }
    }
  }
}
colName <- colnames(pima_data)
cat('Max correlation is between',colName[a],'and',colName[b],  'attribute: ',max)
