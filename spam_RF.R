# Adresse du dossier où vous travaillez
setwd("C:/Analyse_donnees/Code")
library(kernlab)
library(knitr)
library(rpart)
library(e1071)
library(rtext)

library(caret)
library(randomForest)



# Lecture des données d'apprentissage
load("C:/Analyse_donnees/Data/Projets/spam_data_train.rda")

na.omit(data_train)
data_train$label <- as.factor(data_train$label)


sub <- sample(nrow(data_train), floor(nrow(data_train) * 0.8))
train <- data_train[sub, ]
test <- data_train[-sub, ]


rf_classifier = randomForest(x = train,
                             y = train$label,
                             ntree = 300)
rf_classifier

rf_pred = predict(rf_classifier, newdata=test)
confusionMatrix(table(rf_pred,test$label))