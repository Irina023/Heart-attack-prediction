library(aod)
library(ggplot2)
library(pROC)
library(stats)
library(caret)
library(rpart.plot)
library(e1071)
library(randomForest)
library(gbm)
library(class)


data <- read.csv("Heart Attack Data Set.csv")

names(data)[14] <- 'HD'

#Checking for NA values
sum(apply(is.na(data), 2, which))

######################################
######## Correlation matrix ##########
######################################
corr_matrix <- round(cor(data, method = "spearman"), digits = 2)
corr_matrix
heatmap(corr_matrix[,14:1], Rowv = NA, Colv = NA)

######################################
######## Data conversion #############
######################################

data[, -c(1,4,5,8,10)] <- lapply(data[, -c(1,4,5,8,10)] , factor)


##################################################
##### Distributions of categorical predictors ####
######## and target variable               #######
##################################################

#Sex
#absolute
ggplot(data, aes(x = sex, fill = HD)) +
  geom_bar(stat = "count", position = "stack", width = 0.6, show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (Female)", "1 (Male)"))+
  geom_text(
    aes(label=..count..),
    stat='count',position = position_stack(vjust = 0.5))+
  labs(x = "Sex",  y="")+
  options(repr.plot.width = 10, repr.plot.height = 4)
  
  

#percent
ggplot(data, aes(y = sex, fill = HD)) +
  geom_bar(position = "fill", width = 0.6, show.legend = TRUE) + 
  scale_y_discrete(labels  = c("0 (Female)", "1 (Male)"))+
  labs(y = "Sex", x="")

#Chest Pain
#absolute
ggplot(data, aes(x = cp, fill = HD)) +
  geom_bar(stat = "count", position = "stack", width = 0.6, show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (typical angina)", "1 (atypical angina)", "2 (non-anginal pain)", "3 (asymptomatic)"))+
  geom_text(
    aes(label=..count..),
    stat='count',position = position_stack(vjust = 0.5))+
  labs(x = "Chest pain", y="")


#percent
ggplot(data, aes(y = cp, fill = HD)) +
  geom_bar(position = "fill", width = 0.6, show.legend = TRUE) + 
  scale_y_discrete(labels  = c("0 (typical angina)", "1 (atypical angina)", "2 (non-anginal pain)", "3 (asymptomatic)"))+
  labs(y = "Chest pain", x="")

#fasting blood sugar 
#absolute
ggplot(data, aes(x = fbs, fill = HD)) +
  geom_bar(stat = "count", position = "stack", width = 0.6, show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 ( < 120 mg/dL)", "1 ( > 120 mg/dL)"))+
  geom_text(
    aes(label=..count..),
    stat='count',position = position_stack(vjust = 0.5))+
  labs(x = "Fasting blood sugar ", y="")


#percent
ggplot(data, aes(y = fbs, fill = HD)) +
  geom_bar(position = "fill", width = 0.6, show.legend = TRUE) + 
  scale_y_discrete(labels  = c("0 ( < 120 mg/dL)", "1 ( > 120 mg/dL)"))+
  labs(y = "Fasting blood sugar ", x="")

#Resting electrocardiogram results
#absolute
ggplot(data, aes(x = restecg, fill = HD)) +
  geom_bar(stat = "count", position = "stack", width = 0.6, show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (normal)", "1 (ST-T wave abnormality)","2 (left ventricular hypertrophy)"))+
  geom_text(
    aes(label=..count..),
    stat='count',position = position_stack(vjust = 0.5))+
  labs(x = "Resting electrocardiogram results", y="")


#percent
ggplot(data, aes(y = restecg, fill = HD)) +
  geom_bar(position = "fill", width = 0.6, show.legend = TRUE) + 
  scale_y_discrete(labels  = c("0 (normal)", "1 (ST-T wave abnormality)","2 (left ventricular hypertrophy)"))+
  labs(y = "Resting electrocardiogram results", x="")

#Exercise-induced angina
#absolute
ggplot(data, aes(x = exang, fill = HD)) +
  geom_bar(stat = "count", position = "stack", width = 0.6, show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (No)", "1 (Yes)"))+
  geom_text(
    aes(label=..count..),
    stat='count',position = position_stack(vjust = 0.5))+
  labs(x = "Exercise-induced angina", y="")


#percent
ggplot(data, aes(y = exang, fill = HD)) +
  geom_bar(position = "fill", width = 0.6, show.legend = TRUE) + 
  scale_y_discrete(labels  = c("0 (No)", "1 (Yes)"))+
  labs(y = "Exercise-induced angina", x="")

#The slope of the peak exercise ST segment
#absolute
ggplot(data, aes(x = slope, fill = HD)) +
  geom_bar(stat = "count", position = "stack", width = 0.6, show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (Upsloping)", "1 (Flat)", "2 (Downsloping)"))+
  geom_text(
    aes(label=..count..),
    stat='count',position = position_stack(vjust = 0.5))+
  labs(x = "The slope of the peak exercise ST segment", y="")


#percent
ggplot(data, aes(y = slope, fill = HD)) +
  geom_bar(position = "fill", width = 0.6, show.legend = TRUE) + 
  scale_y_discrete(labels  = c("0 (Upsloping)", "1 (Flat)", "2 (Downsloping)"))+
  labs(y = "The slope of the peak exercise ST segment", x="")

#Number of major vessels (0-3) colored by fluoroscopy
#absolute
ggplot(data, aes(x = ca, fill = HD)) +
  geom_bar(stat = "count", position = "stack", width = 0.6, show.legend = TRUE) +
  scale_x_discrete(labels  = c("0","1", "2", "3", "N/A"))+
  geom_text(
    aes(label=..count..),
    stat='count',position = position_stack(vjust = 0.5))+
  labs(x = "Number of major vessels (0-3) colored by fluoroscopy", y="")


#percent
ggplot(data, aes(y = ca, fill = HD)) +
  geom_bar(position = "fill", width = 0.6, show.legend = TRUE) + 
  scale_y_discrete(labels  = c("0","1", "2", "3", "N/A"))+
  labs(y = "Number of major vessels (0-3) colored by fluoroscopy", x="")

#Thalassemia
#absolute
ggplot(data, aes(x = thal, fill = HD)) +
  geom_bar(stat = "count", position = "stack", width = 0.6, show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (N/A)","1 (Normal)", "2 (fixed defect)", "3 (reversible defect)"))+
  geom_text(
    aes(label=..count..),
    stat='count',position = position_stack(vjust = 0.5))+
  labs(x = "Thalassemia", y="")

#percent
ggplot(data, aes(y = thal, fill = HD)) +
  geom_bar(position = "fill", width = 0.6, show.legend = TRUE) + 
  scale_y_discrete(labels  = c("0 (N/A)","1 (Normal)", "2 (fixed defect)", "3 (reversible defect)"))+
  labs(y = "Thalassemia", x="")

#HD
#absolute
ggplot(data, aes(x = HD, fill = HD)) +
  geom_bar(stat = "count", width = 0.6, position = "stack") +
  geom_text(
    aes(label=..count..),
    stat='count',position = position_stack(vjust = 0.5))+
  labs(y="", x="")

###################################################
####### Distributions of numeric predictors #######
###################################################

#number of rows
n = dim(data)[1]
#number of bins
k = 1+3.322*log10(n)

#age
summary(data$age)
age_0 <- subset(data, HD == 0, select=age)
summary(age_0)
age_1 <- subset(data, HD == 1, select=age)
summary(age_1)

#binwidth
h = round(diff(range(data$age))/k)

#distribution
ggplot(data, aes(x=age)) + 
  geom_histogram(binwidth = h, color="black", fill = "grey")+
  geom_vline(aes(xintercept=mean(age)),
             color="black", linetype="dashed", size=1)+
  labs(x = "Age", y="")

#relationship between age target variable
ggplot(data, aes(x=age, color=HD, fill=HD)) + 
  geom_histogram(binwidth = h, alpha=0.5, position="identity")+
  labs(x = "Age", y="")

#Resting Blood pressure (mmHg)
summary(data$trestbps)
trestbps_0 <- subset(data, HD == 0, select=trestbps)
summary(trestbps_0)
trestbps_1 <- subset(data, HD == 1, select=trestbps)
summary(trestbps_1)

#binwidth
h = round(diff(range(data$trestbps))/k)

#distribution
ggplot(data, aes(x=trestbps)) + 
  geom_histogram(binwidth = h, color="black", fill = "grey")+
  geom_vline(aes(xintercept=mean(trestbps)),
             color="black", linetype="dashed", size=1)+
  labs(x = "Resting Blood pressure (mmHg)", y="")

#relationship between trestbps and target variable
ggplot(data, aes(x=trestbps, color=HD, fill=HD)) + 
  geom_histogram(binwidth = h, alpha=0.5, position="identity")+
  labs(x = "Resting Blood pressure (mmHg)", y="")

#Serum cholesterol in mg/dL
summary(data$chol)
chol_0 <- subset(data, HD == 0, select=chol)
summary(chol_0)
chol_1 <- subset(data, HD == 1, select=chol)
summary(chol_1)

#binwidth
h = round(diff(range(data$chol))/k)

#distribution
ggplot(data, aes(x=chol)) + 
  geom_histogram(binwidth = h, color="black", fill = "grey")+
  geom_vline(aes(xintercept=mean(chol)),
             color="black", linetype="dashed", size=1)+
  labs(x = "Serum cholesterol in mg/dL", y="")

#relationship between chol and target variable
ggplot(data, aes(x=chol, color=HD, fill=HD)) + 
  geom_histogram(binwidth = h, alpha=0.5, position="identity")+
  labs(x = "Serum cholesterol in mg/dL", y="")

#Maximum heart rate 
summary(data$thalach)
thalach_0 <- subset(data, HD == 0, select=thalach)
summary(thalach_0)
thalach_1 <- subset(data, HD == 1, select=thalach)
summary(thalach_1)

#binwidth
h = round(diff(range(data$thalach))/k)

#distribution
ggplot(data, aes(x=thalach)) + 
  geom_histogram(binwidth = h, color="black", fill = "grey")+
  geom_vline(aes(xintercept=mean(thalach)),
             color="black", linetype="dashed", size=1)+
  labs(x = "Maximum heart rate", y="")

#relationship between thalach and target variable
ggplot(data, aes(x=thalach, color=HD, fill=HD)) + 
  geom_histogram(binwidth = h, alpha=0.5, position="identity")+
  labs(x = "Maximum heart rate", y="")

#ST depression induced by exercise relative to rest 
summary(data$oldpeak)
oldpeak_0 <- subset(data, HD == 0, select=oldpeak)
summary(oldpeak_0)
oldpeak_1 <- subset(data, HD == 1, select=oldpeak)
summary(oldpeak_1)

#binwidth
h = round(diff(range(data$oldpeak))/k)

#distribution
ggplot(data, aes(x=oldpeak)) + 
  geom_histogram(binwidth = h, color="black", fill = "grey")+
  geom_vline(aes(xintercept=mean(oldpeak)),
             color="black", linetype="dashed", size=1)+
  labs(x = "ST depression induced by exercise relative to rest ", y="")

#relationship between thalach and target variable
ggplot(data, aes(x=oldpeak, color=HD, fill=HD)) + 
  geom_histogram(binwidth = h, alpha=0.5, position="identity")+
  labs(x = "ST depression induced by exercise relative to rest ", y="")

###########################################
######Box plots for numeric variable ######
###########################################

boxplot(age ~ HD, data = data, xlab = "Heart disease")
boxplot(trestbps ~ HD, data = data, xlab = "Heart disease", ylab = "Resting Blood pressure (mmHg)")
boxplot(chol ~ HD, data = data, xlab = "Heart disease", ylab = "Serum cholesterol (mg/dL)")
boxplot(thalach ~ HD, data = data, xlab = "Heart disease", ylab = "Maximum heart rate")
boxplot(oldpeak ~ HD, data = data, xlab = "Heart disease", ylab = "ST depression induced by exercise relative to rest")


###########################################
######Splitting data ######################
###########################################
set.seed(0)
index <- createDataPartition(y = data$HD, p = 0.75, list = FALSE)
train <- data[index,]
test <- data[-index,]
summary(train)
summary(test)

###########################################
############ Decision tree ################
###########################################

dt_fit <- rpart(formula = HD ~ ., data = train, method = "class")
print(dt_fit)
rpart.plot(dt_fit)
plotcp(dt_fit, upper = "splits")

prob <- predict(dt_fit, test, type = "prob")
pred <- ifelse(prob>=0.5,1,0)
cm<-confusionMatrix(factor(pred[,2]),test$HD)
cm
plot(roc(test$HD,pred[,2]), print.auc=TRUE)
dt_fit$variable.importance
barplot(dt_fit$variable.importance, horiz = TRUE, las = 1)

#final model
dt_fit <- rpart(formula = HD ~ thal + cp + ca, data = train, method = "class")
print(dt_fit)
rpart.plot(dt_fit)
plotcp(dt_fit, upper = "splits")

prob <- predict(dt_fit, test, type = "prob")
pred <- ifelse(prob>=0.5,1,0)
confusionMatrix(factor(pred[,2]),test$HD)

plot(roc(test$HD,pred[,2]), print.auc=TRUE)
dt_acc <- confusionMatrix(factor(pred[,2]),test$HD)$overall["Accuracy"]
###########################################
################### GBM ###################
###########################################

gbm_fit <- gbm(as.character(HD) ~ .,
               data = train,
               distribution = 'bernoulli',
               n.trees=200,
               shrinkage=0.01)
print(gbm_fit)
summary(gbm_fit)
prob <- predict.gbm(object = gbm_fit,
                    newdata = test,
                    n.trees = 200,
                    type = "response")
pred <- as.factor(ifelse(prob>0.5,1,0))
confusionMatrix(factor(pred),test$HD)
plot(roc(test$HD,factor(pred, ordered = TRUE)), print.auc=TRUE)


#final model
gbm_fit <- gbm(as.character(HD) ~ thal + ca +cp,
               data = train,
               distribution = 'bernoulli',
               n.trees=200,
               shrinkage=0.01)
print(gbm_fit)

prob <- predict.gbm(object = gbm_fit,
                    newdata = test,
                    n.trees = 200,
                    type = "response")
pred <- as.factor(ifelse(prob>0.5,1,0))
confusionMatrix(factor(pred),test$HD)
plot(roc(test$HD,factor(pred, ordered = TRUE)), print.auc=TRUE)

gbm_acc <- confusionMatrix(factor(pred, ordered = TRUE),test$HD)$overall["Accuracy"]

###########################################
###### Random forest ##### ################
###########################################

rf_fit <- randomForest(
  HD ~ .,
  data=train
)
rf_fit

pred <- predict(rf_fit, test)
confusionMatrix(factor(pred),test$HD)
plot(roc(test$HD,factor(pred, ordered = TRUE)), print.auc=TRUE)

rf_cv <- train(HD ~ .,
               data = train,
               method = "rf",
               metric = "Accuracy",
               tuneLength=10,
               trControl = trainControl(method = "repeatedcv", number = 5))
print(rf_cv)
rf_cv$resample
varImp(rf_cv)
accuracy = mean(as.numeric(rf_cv$resample[, 1]))
accuracy

#final model
rf_fit <- randomForest(
  HD ~ exang+slope+cp+ca+oldpeak,
  data=train,
  mtry = 2
)
rf_fit

pred <- predict(rf_fit, test)
confusionMatrix(factor(pred),test$HD)
rf_acc <-confusionMatrix(factor(pred),test$HD)$overall["Accuracy"]

plot(roc(test$HD,factor(pred, ordered = TRUE)), print.auc=TRUE)

###########################################
###### Support vector machine##############
###########################################

#data standartization
data[c(1,4,5,8,10)] <- as.data.frame(scale(data[c(1,4,5,8,10)]))

index <- createDataPartition(y = data$HD, p = 0.75, list = FALSE)
train <- data[index,]
test <- data[-index,]



svm_fit <- svm(HD ~ ., data = train, type = 'C-classification', kernel = 'linear')
print(svm_fit)
summary(svm_fit)

#confusion matrix
pred <- predict(svm_fit, test)
confusionMatrix(factor(pred),test$HD)
#ROC curve
plot(roc(test$HD,factor(pred, ordered = TRUE)), print.auc=TRUE)

# evaluating model
svm_cv <- train(HD ~ .,
             data = train,
             method = "svmLinear",
             trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3))

print(svm_cv)
svm_cv$resample
varImp(svm_cv)
accuracy = mean(as.numeric(svm_cv$resample[, 1]))
accuracy


#final model
svm_fit <- svm(HD ~ cp + thalach + ca + exang + thal, data = train,
             type = 'C-classification', 
             kernel = 'linear')

print(svm_fit)
summary(svm_fit)

pred <- predict(svm_fit, test)
confusionMatrix(factor(pred),test$HD)
plot(roc(test$HD,factor(pred, ordered = TRUE)), print.auc=TRUE)

svm_acc <- confusionMatrix(factor(pred),test$HD)$overall["Accuracy"]


###########################################
###### Logistic regression ################
###########################################

#initial model
full_model<-glm(HD~.,data=train,family='binomial')
summary(full_model)

prob <- predict(full_model, test)
pred <- ifelse(prob>=0.5,1,0)
confusionMatrix(factor(pred),test$HD)

plot(roc(test$HD,pred), print.auc=TRUE)

null_model<-glm(HD~1,data=train,family='binomial')
lr_fit <- step(full_model, 
                   scope = list(lower = null_model,
                                upper = full_model),
                   direction = "backward")
summary(lr_fit)
prob <- predict(lr_fit, test)
pred <- ifelse(prob>=0.5,1,0)
confusionMatrix(factor(pred),test$HD)

plot(roc(test$HD,pred), print.auc=TRUE)
lr_acc <- confusionMatrix(factor(pred),test$HD)$overall["Accuracy"]

#cross validation
lr_cv <- train(HD ~ oldpeak+thal+slope+cp+ca, data=train, 
               family = binomial(link = "logit"),
               method = "glm", 
               trControl = trainControl(method = "cv", number = 5))
print(lr_cv)
lr_cv$resample

###########################################
###### K nearest neighbors ################
###########################################

train_labels <- train[,14]
pred <- knn(train[,-14], test[,-14], cl=train_labels)
confusionMatrix(pred,test$HD)

plot(roc(test$HD,factor(pred, ordered = TRUE)), print.auc=TRUE)

knn_fit <- train(HD ~.,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trainControl(method = 'cv', number = 5),
             metric     = "Accuracy",
             data       = data)
knn_fit$results
varImp(knn_fit)

ggplot(data = knn_fit$results, aes(x = k, y = Accuracy, color = 'pink')) +
  geom_line()+
  scale_x_continuous("k", labels = as.character(knn_fit$results$k), breaks = knn_fit$results$k)


#final model
pred <- knn(train[c('cp','thalach','oldpeak','ca','exang','sex')],
            test[c('cp','thalach','oldpeak','ca','exang','sex')],
            cl=train_labels,k=5)
confusionMatrix(pred,test$HD)

plot(roc(test$HD,factor(pred, ordered = TRUE)), print.auc=TRUE)
knn_acc <- confusionMatrix(pred,test$HD)$overall["Accuracy"]

alg_names <- c("Decision Tree", "GBM", "Random Forest", "SVM", "Logistic Regression", "KNN")
acc <- c(dt_acc, gbm_acc, rf_acc, svm_acc,  lr_acc, knn_acc)
df_acc <- data.frame(alg_names, acc)

df_acc$alg_names <- factor(df_acc$alg_names, levels = df_acc$alg_names)

ggplot( mapping = aes(x=df_acc$alg_names)) +
  geom_bar(aes(y = ..acc.., fill = df_acc$alg_names),width = 0.6,show.legend = FALSE) +
  geom_text(aes( y = ..acc.., label = scales::percent(..acc..)), stat = "count", vjust = -1)+
  ylim(0, 1)+
  labs(y = "Accuracy", x="")

