install.packages("naivebayes")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
train_salary<-read.csv(file.choose(),header=TRUE)
View(train_salary)
str(train_salary)
train_salary1<-train_salary

#Converting Salary column into factor
train_salary1$Salary[Salary==" <=50K"]<-0
train_salary1$Salary[Salary==" >50K"]<-1
train_salary1$Salary<-as.numeric(train_salary1$Salary)
train_salary1$Salary<-factor(train_salary1$Salary)

View(train_salary1)
str(train_salary1)

#data visualization
train_salary1 %>%
  ggplot(aes(x=Salary, y=age, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

train_salary1 %>% ggplot(aes(x=age, fill =Salary)) +
  geom_density(alpha=0.8, color= 'black') +
  ggtitle("Density Plot")

#creating model on training set
model_naives<-naive_bayes(Salary ~ ., data = train_salary1)
plot(model_naives)

#Predict
naives_train<-predict(model_naives, train_salary1, type = 'prob')
head(cbind(naives_train, train_salary1))

#confustion matrix on train data
naives1_train <- predict(model_naives, train_salary1)
(tab1_naives<- table(naives1_train, train_salary1$Salary))
sum(diag(tab1_naives)) / sum(tab1_naives)


#Importing Test Data
test_salary<-read.csv(file.choose(),header=TRUE)
View(test_salary)

test_salary1<-test_salary

#converting salary column into factor
test_salary1$Salary[test_salary1$Salary==" <=50K"]<-0
test_salary1$Salary[test_salary1$Salary==" >50K"]<-1
test_salary1$Salary<-as.numeric(test_salary1$Salary)
test_salary1$Salary<-factor(test_salary1$Salary)

#data visualization
test_salary1 %>%
  ggplot(aes(x=Salary, y=age, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

test_salary1 %>% ggplot(aes(x=age, fill =Salary)) +
  geom_density(alpha=0.8, color= 'black') +
  ggtitle("Density Plot")

#Confusion Matrix test data
naives_test<-predict(model_naives, test_salary1, type = 'prob')
head(cbind(naives_test, test_salary1))

#confusion matrix
naives1_test <- predict(model_naives, test_salary1)
(tab2_naives<- table(naives1_test, test_salary1$Salary))
sum(diag(tab2_naives)) / sum(tab2_naives)
