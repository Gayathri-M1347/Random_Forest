library(randomForest)


library(MASS)
library(caret)


set.seed(123)


FraudCheck <- read.csv(file.choose())
hist(FraudCheck$Taxable.Income)


hist(FraudCheck$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","violet"))


Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky else Good.
FCtemp= data.frame(FraudCheck,Risky_Good)
FC = FCtemp[,c(1:7)]

str(FC)


table(FC$Risky_Good)


set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
set.seed(213)
rf <- randomForest(as.factor(Risky_Good)~., data=train,ntree = 10,importance = T)
rf 


attributes(rf)


pred1 <- predict(rf, train)
head(pred1)



head(train$Risky_Good)


confusionMatrix(pred1, as.factor(train$Risky_Good))


pred2 <- predict(rf, test)
confusionMatrix(pred2, as.factor(test$Risky_Good))


plot(rf)


# Tune Random Forest Model mtry 
tuneRF <- tuneRF(x=train[,-6], y=as.factor(train[,6]), mtryStart = 5 ,stepFactor = 0.5, plot = TRUE, ntreeTry = 300, 
               trace = TRUE,improve = 0.05,doBest = TRUE,
               nodesize = 30, 
               importance=TRUE)
               


rf1 <- randomForest(as.factor(Risky_Good)~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1


pred1 <- predict(rf1, train)
confusionMatrix(pred1, as.factor(train$Risky_Good))  # 100 % accuracy on training data 



# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, as.factor(test$Risky_Good)) # 100 % accuracy on test data 



# no of nodes of trees

hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")

  

varImpPlot(rf1)             



varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")



# Quantitative values 
importance(rf1)



varUsed(rf)


# Partial Dependence Plot 
partialPlot(rf1, train, Taxable.Income, "Good")



# Extract single tree from the forest :
tr1 <- getTree(rf1, 2, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, as.factor(FC$Risky_Good))
