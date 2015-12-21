library(caret); data(GermanCredit); library(doParallel); library(ggplot2)


cl<- makeCluster(detectCores())
registerDoParallel(cl)

# Age seems like a natural starting point for some EDA
# looks like loan applicants in general tend to be younger and that bad loans
# appear to have a higher % of younger applicants. We see good loans 
# are more uniformly distributed in age
g <- ggplot(GermanCredit, aes(x = Class, y = Age)) 
g <- g + geom_violin(alpha = 0.5, color = "gray") 
g <- g + geom_jitter(alpha = 0.5, aes(color = Class), position = position_jitter(width = .2)) + coord_flip()


library(dplyr)

df <- GermanCredit %>% 
        tbl_df() %>%
        



inTrain <- createDataPartition(GermanCredit$Class, p = 0.6, list = FALSE)
training <- GermanCredit[inTrain,]
testing <- GermanCredit[-inTrain,]

## Random Forest Model
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

rf.fit <- train(Class ~ ., data = training,
                 method = "rf",
                 trControl = fitControl)

rf.preds <- predict(rf.fit, newdata = testing, type = "prob")
rf.preds$Class <- as.factor(ifelse(test = rf.preds$Bad > .45, 1, 2))
levels(rf.preds$Class) <- c("Bad", "Good")
confusionMatrix(rf.preds$Class, testing$Class)

## SVM Model
fitControl2 <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 10,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)

svm.fit <- train(Class ~ ., data = training,
                method = "svmRadial",
                trControl = fitControl2,
                tuneLength = 8, 
                metric = "ROC")

## Rpart Model

rpart.fit <- rpart(Class ~ ., data = training)
rpart.preds <- predict(rpart.fit, type = 'class', testing)

confusionMatrix(rpart.preds, testing$Class)

## Conditional inference tree plot

cfit <- ctree(Class ~ ., data = training)

stopCluster(cl)