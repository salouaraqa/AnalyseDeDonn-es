# Adresse du dossier où vous travaillez
setwd("C:/Analyse_donnees/Code")
library(kernlab)
library(knitr)
library(rpart)
library(e1071)
library(rtext)

library(caret)




# Lecture des données d'apprentissage
load("C:/Analyse_donnees/Data/Projets/spam_data_train.rda")

na.omit(data_train)
data_train$label <- as.factor(data_train$label)


sub <- sample(nrow(data_train), floor(nrow(data_train) * 0.8))
train <- data_train[sub, ]
test <- data_train[-sub, ]

svm_classifier <- svm(label~., data=train)
svm_classifier

svm_pred = predict(svm_classifier, test)
confusionMatrix(svm_pred,test$label)