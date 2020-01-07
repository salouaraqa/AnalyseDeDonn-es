# Adresse du dossier où vous travaillez
setwd("C:/Analyse_donnees/Code")
library(kernlab)
library(knitr)
library(rpart)
library(e1071)
library(rtext)
library(e1071)

# Lecture des données d'apprentissage
load("C:/Analyse_donnees/Data/Projets/spam_data_train.rda")

na.omit(data_train)
data_train$label <- as.factor(data_train$label)


sub <- sample(nrow(data_train), floor(nrow(data_train) * 0.8))
train <- data_train[sub, ]
test <- data_train[-sub, ]

control <- trainControl(method="repeatedcv", number=10, repeats=3)
system.time( classifier_nb <- naiveBayes(train, train$label, laplace = 1,
                                         trControl = control,tuneLength = 7) )

nb_pred = predict(classifier_nb, type = 'class', newdata = test)

confusionMatrix(nb_pred,test$label)
