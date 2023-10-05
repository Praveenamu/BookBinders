# BookBinders_CaseStudy
Machine Learning Model

## Reading the Library
```{r}
library(MASS)
library(tidyverse)
library(corrplot)
library(car)
library(broom)
library(DescTools)
library(ROCR)
library(lmtest)
library(readxl)
library(ggplot2)
library(grid)
library(gridExtra)
library(e1071)
```

## Reading the dataset
```{r}
booktrain = read_excel("BBBC-Train.xlsx")
booktest = read_excel("BBBC-Test.xlsx")
```

```{r}
str(booktrain)
```
```{r}
str(booktest)
```

## Checking whether the data set have the null values

```{r}
sum(is.na(booktrain))
sum(is.na(booktest))
```

## Removing observation variable as it is irrelevant to our analysis

```{r}
booktest = dplyr::select(booktest,-Observation)
booktrain = dplyr::select(booktrain,-Observation)
```

```{r}
names(booktrain)
```
## Converting Choice and Gender variables in to factors

```{r}
booktrain$Choice = as.factor(booktrain$Choice)
booktrain$Gender = as.factor(booktrain$Gender)
booktest$Choice = as.factor(booktest$Choice)
booktest$Gender = as.factor(booktest$Gender)
```

```{r}
str(booktrain)
```

```{r}
levels(as.factor(booktrain$Choice))
```

## Explorating Data Analysis with booktrain data set

## Choice variable - Dependent variable

```{r}
Choicevar =  ggplot(booktrain,aes(x = factor(ifelse(Choice == 0,"Non-Purchase", "Purchase" )))) +
  geom_bar() + stat_count(geom = "text", colour = "pink", aes(label = paste("N =",..count..)),position=position_stack(vjust=0.5)) + xlab("Choice") + ylab("Count")
Choicevar
```

## color code based on Gender
```{r}
ggplot(data=booktrain)+ geom_bar(mapping = aes(x = Choice, fill = Gender), position = "dodge")
```
## Amount_Purchased

```{r}
p1= ggplot(booktrain) + geom_histogram(aes(x=Amount_purchased),color="black", fill="grey",bins=20) +
  ylab('Count') +  xlab('Amount_purchased') +  geom_vline(aes(xintercept = mean(Amount_purchased), color = "red"))
p3=ggplot(booktrain, aes(x=Amount_purchased, fill=Choice)) + geom_bar() 
grid.arrange(p1,p3,ncol=1)
```
## Frequency

```{r}
p2 = ggplot(booktrain) + geom_boxplot(aes(x='', y=Frequency))
p3=ggplot(booktrain, aes(x=Frequency, fill=Choice)) + geom_bar() + geom_vline(aes(xintercept = mean(Frequency), color = "grey"))
grid.arrange(p2,p3,ncol=2)
```
## Last_purchase

```{r}
p1 <- ggplot(booktrain,aes(x=Last_purchase,fill= factor(ifelse(Choice == 0, "Non-Purchase", "Purchase" )))) + geom_bar() + 
  stat_count(geom = "text", colour = "black",aes(label = paste("N =", ..count..)),position=position_stack(vjust=0.5),size=2) +  labs(x="Last_Purchase",y="Count",fill="Choice")
p2 <- ggplot(booktrain,aes(x= factor(ifelse(Choice == 0, "Non-Purchase", "Purchase" )))) + geom_bar() + 
  stat_count(geom = "text", colour = "black",aes(label = paste("N =", ..count..)),position=position_stack(vjust=0.5),size=4) +  labs(x="Choice",y="Count",fill="Last_Purchase")
grid.arrange(p1, p2,nrow=2)
```

## First_Purchase

```{r}
p2 = ggplot(booktrain) + geom_boxplot(aes(x='', y=First_purchase))
p3=ggplot(booktrain, aes(x=First_purchase, fill=Choice)) + geom_bar() +  geom_vline(aes(xintercept = mean(First_purchase), color = "black"))
grid.arrange(p2,p3,ncol=2)
```

## P_child

```{r}
p1 <- ggplot(booktrain,aes(x=P_Child,fill= factor(ifelse(Choice == 0, "Non-Purchase", "Purchase" )))) + geom_bar() + 
  stat_count(geom = "text", colour = "black",aes(label = paste("N =", ..count..)),position=position_stack(vjust=0.5),size=2) +  labs(x="P_Child",y="Count",fill="Choice")
p1
```
## P_Youth

```{r}
p1 <- ggplot(booktrain,aes(x=P_Youth,fill= factor(ifelse(Choice == 0, "Non-Purchase", "Purchase" )))) + geom_bar() + 
  stat_count(geom = "text", colour = "black",aes(label = paste("N =", ..count..)),position=position_stack(vjust=0.5),size=2) +  labs(x="P_Youth",y="Count",fill="Choice")
p1
```

## P_cook
```{r}
p1 <- ggplot(booktrain,aes(x=P_Cook,fill= factor(ifelse(Choice == 0, "Non-Purchase", "Purchase" )))) + geom_bar() + 
  stat_count(geom = "text", colour = "black",aes(label = paste("N =", ..count..)),position=position_stack(vjust=0.5),size=2) +  labs(x="P_Cook",y="Count",fill="Choice")
p1
```

## P_DIY

```{r}
p1 <- ggplot(booktrain,aes(x=P_DIY,fill= factor(ifelse(Choice == 0, "Non-Purchase", "Book Purchase" )))) + geom_bar() + 
  stat_count(geom = "text", colour = "black",aes(label = paste("N =", ..count..)),position=position_stack(vjust=0.5),size=3) +  labs(x="P_DIY",y="Count",fill="Choice")
p1
```
## P_Art

```{r}
p1 <- ggplot(booktrain,aes(x=P_Art,fill= factor(ifelse(Choice == 0, "Non-Purchase", "Book Purchase" )))) + geom_bar() + 
  stat_count(geom = "text", colour = "black",aes(label = paste("N =", ..count..)),position=position_stack(vjust=0.5),size=2) +  labs(x="P_Art",y="Count",fill="Choice")
p1
```
## Check for correlation
```{r}
Booktrain_num = select_if(booktrain, is.numeric)
C = cor(Booktrain_num)
```

# Plot the correlation matrix
```{r}
corrplot(C, method = "number")
```

# Overview of the data distribution

## Computing scatter plots for each variable

```{r}
pairs(Booktrain_num)
```


```{r}
set.seed(1)
```

## Linear Regression Model

```{r}
lr1 = lm(as.numeric(Choice) ~ ., data = booktrain)
summary(lr1)
```
```{r}
vif(lr1)
```

## Interpretation
 Last_purchase &  First_purchase are highly correlated.
 
## Removing Last_purchase as it has high correlation

```{r}
lr2 = lm(as.numeric(Choice) ~ Gender+Amount_purchased+Frequency+First_purchase+P_Child+P_Youth+P_Cook+P_DIY+P_Art, data = booktrain)
summary(lr2)
```
```{r}
vif(lr2)
```
## Removing First_purchase as it has high correlation

```{r}
lr3 = lm(as.numeric(Choice) ~ Gender+Amount_purchased+Frequency+P_Child+P_Youth+P_Cook+P_DIY+P_Art, data = booktrain)
summary(lr3)
```
```{r}
vif(lr3)
```

## Diagnostics plots for Linear Regression

```{r}
par(mfrow = c(2,2))
plot(lr3, which =c(1:4))
```
## Comments: 
We can’t use the linear regression model for the model prediction since the target variable is not continuous.


## Logistic Regression

```{r}
r1 = glm(Choice ~ ., data = booktrain, family = binomial)
```

```{r}
summary(r1)
```
```{r}
vif(r1)
```
## Removing Last_purchase as it has high correlation

```{r}
r2 = glm(Choice ~ Gender+Amount_purchased+Frequency+First_purchase+P_Child+P_Youth+P_Cook+P_DIY+P_Art, data = booktrain, family = binomial)
summary(r2)
```
```{r}
vif(r2)
```

## Removing First_purchase as it has high correlation

```{r}
r3 = glm(Choice ~ Gender+Amount_purchased+Frequency+P_Child+P_Youth+P_Cook+P_DIY+P_Art, data = booktrain, family = binomial)
summary(r3)
```
```{r}
vif(r3)
```
## Make predictions for logistic regressions

```{r}
predprob = predict.glm(r3, newdata = booktest, type = "response")
```

```{r}
predclass_log = ifelse(predprob >= 0.5, "1", "0")
```

```{r}
caret::confusionMatrix(as.factor(predclass_log), as.factor(booktest$Choice), positive = "1")
```
## ROC Curve

```{r}
PredProb1 = prediction(predict.glm(r3, newdata = booktest, type = "response"), booktest$Choice)
```


```{r}
# Computing threshold for cutoff to best trade off sensitivity and specificity
plot(unlist(performance(PredProb1,'sens')@x.values),unlist(performance(PredProb1,'sens')@y.values), type='l', lwd=2, ylab = "y", xlab = 'Cutoff')
mtext('Sensitivity',side=2)
mtext('Sensitivity vs. Specificity Plot for AIC Model', side=3)

# Second specificity in same plot
par(new=TRUE)
plot(unlist(performance(PredProb1,'spec')@x.values),unlist(performance(PredProb1,'spec')@y.values), type='l', lwd=2,col='red', ylab = "", xlab = 'Cutoff')
axis(4,at=seq(0,1,0.2)) 
mtext('Specificity',side=4, col='red')

par(new=TRUE)

min.diff <-which.min(abs(unlist(performance(PredProb1, "sens")@y.values) - unlist(performance(PredProb1, "spec")@y.values)))
min.x<-unlist(performance(PredProb1, "sens")@x.values)[min.diff]
min.y<-unlist(performance(PredProb1, "spec")@y.values)[min.diff]
optimal <-min.x

abline(h = min.y, lty = 3)
abline(v = min.x, lty = 3)
text(min.x,0,paste("optimal threshold=",round(optimal,5)), pos = 4)
```

## Making predictions with optimal threshold 

```{r}
predprob = predict.glm(r3, newdata = booktest, type = "response")
```


```{r}
predclass_log = ifelse(predprob >= 0.23, "1", "0")
```

```{r}
caret::confusionMatrix(as.factor(predclass_log), as.factor(booktest$Choice), positive = "1")
```

# Linear Discriminate analysis - Comparision for logistic regression model


```{r}
m1.lda = lda(as.factor(Choice) ~ ., data = booktrain)
```
## Make predictions for LDA

```{r}
predclass_lda = predict(m1.lda, newdata = booktest)
```

```{r}
caret::confusionMatrix(as.factor(predclass_lda$class), as.factor(booktest$Choice), positive = "1")
```

## Support Vector Machine

# SVM with linear kernel
```{r}
set.seed(100)
tuned.svm <- tune.svm(Choice ~ ., data=booktrain, kernel = 'linear',
                      gamma = seq(0.01, 0.1, by = 0.025),
                      cost = seq(0.1, 1, by=0.1), scale=TRUE)
tuned.svm$best.parameters
```

## Creating the SVM using the tuned parameters:


```{r}
svm1 <- svm(Choice ~ ., data = booktrain, kernel = 'linear',
            gamma = tuned.svm$best.parameters$gamma,
            cost = tuned.svm$best.parameters$cost)
```

```{r}
summary(svm1)
```

## Running SVM on testing data set

```{r}
svm.preds <- predict(svm1, booktest, type = "response")
caret::confusionMatrix(svm.preds, booktest$Choice, positive = "1")
```
## SVM using RBF kernel

```{r}
set.seed(100)
tuned.svm <- tune.svm(Choice ~ ., data=booktrain,
                      gamma = seq(0.01, 0.1, by = 0.025),
                      cost = seq(0.1, 1, by=0.1), scale=TRUE)
tuned.svm$best.parameters
```

```{r}
svm2 = svm(Choice ~ ., data = booktrain,
            gamma = tuned.svm$best.parameters$gamma,
            cost = tuned.svm$best.parameters$cost)
summary(svm2)
```

```{r}
svm.preds1 <- predict(svm2, booktest, type = "response")
caret::confusionMatrix(svm.preds1, booktest$Choice, positive = "1")
```

## Comparing the profit analysis based on models

```{r}
totalcustomerdata = 50000  #  where it has data for 50,000 customers
customerstest_data = 2300  # customers in testing data test 
mailingcost = 0.65         # cost of the mailing is $0.65/addressee
purchasecost = 15          # book costs $15 to purchase and mail
overheadcost = 0.45        # overhead to each book at 45% of cost
sellingprice = 31.95       # selling price of the book

profit = sellingprice - purchasecost - purchasecost*overheadcost
profit
```
## Analysing the profit on mail offer to the entire list

```{r}
percorder = 0.0900   #Percentage of books ordered not via mail
(entirecustomers = 50000*percorder*profit-50000*(1- percorder)*mailingcost)
```
## Profit analysis on logistic regression model


```{r}
pos_pred_value = 0.19522
(logreg_purchase_perc = (606 + 147)/2300)
```
```{r}
(logreg_customers = round((logreg_purchase_perc*50000),0))
```
#profit from LR model
```{r}
(profitlogreg = logreg_customers*pos_pred_value*profit - logreg_customers*(1-pos_pred_value)*mailingcost)
```
## Profit analysis on SVM model

```{r}
pos_pred_value_svm = 0.40141
(svm_purchase_perc = (85 + 57)/2300)
```

```{r}
(nr_customers_svm = round((svm_purchase_perc*50000),0))
```
#profit from SVM Model
```{r}
(profit_svm = nr_customers_svm*pos_pred_value_svm*profit - nr_customers_svm*(1-pos_pred_value_svm)*mailingcost) 
```


























