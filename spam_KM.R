# Adresse du dossier où vous travaillez
setwd("C:/Analyse_donnees/Code")
library(rpart)
library(e1071)
library(rtext)
library(FactoMineR)
library(caret)
library(PCAmixdata)
library("GGally")
library(MASS)

# Lecture des données d'apprentissage
load("C:/Analyse_donnees/Data/Projets/spam_data_train.rda")
load("C:/Analyse_donnees/Data/Projets/spam_data_test.rda")


# Création des données centrées ...
data_train_norm <- sweep(data_train,2,mean,"-")
# ... et réduites
data_train_norm <- sweep(data_train_norm,2,std,"/")


# Nombre de clusters souhaité
clusters <- 2

#kmeans sur données centrées-réduites
km_norm <- kmeans(na.omit(data_train_norm),clusters,nstart=60)
print(km_norm)


cluster_norm <- as.factor(km_norm$cluster)
data_train_nc <- data.frame(data_train_norm,cluster=cluster_norm)
col_clust <- length(data_train)+1
print(data_train_nc)

# ACP sur les données centrées-réduites
ACPnorm <- PCA(data_train_nc,graph=TRUE,quali.sup=col_clust)
# Nuage des individus et des variables dans le premier plan 
par(mfrow=c(1,2))
plot.PCA(ACPnorm,,axes=c(1,2),choix="ind",habillage=col_clust,invisible="quali")
plot.PCA(ACPnorm,axes=c(1,2),choix="var")
# Nuage des individus et des variables dans le deuxième plan 
par(mfrow=c(1,2))
plot.PCA(ACPnorm,axes=c(1,3),choix="ind",habillage=col_clust,invisible="quali")
plot.PCA(ACPnorm,axes=c(1,3),choix="var")

data_train$label <- as.factor(data_train$label)
LDA <- lda(label~., data = data_train)
data_test_x <- data.frame(data_train[-58], check.names = FALSE)
predict_test_spam <- predict(LDA, newdata=data_test_x, type="class")
print(table(predict_test_spam$class, data_train$label))

error <- mean(predict_test_spam$class != data_train$label)
cat("Error: ", error, "\n")

MSE <- 0
n <- length(data_train$label)
# Ne garder que 70%
sample_data_train <- sample(1:n, 0.7*n)
sample_data_test <- setdiff(1:n, sample_data_train)

data_train_sample <- data_train[c(sample_data_train), ]
data_test_sample <- data_train[c(sample_data_test), ]

LDA <- lda(label~., data_train_sample)
data_test_x <- data.frame(data_test_sample[-58], check.names = FALSE)
pred <- predict(LDA, data_test_x, type="class")

pred_class <- pred$class

transform(data_test_sample, label = as.numeric(label))

for(i in 1:length(sample_data_test)) {
  pred <- sum(numeric(pred_class[i]) + 1) - 1
  real <- sum(numeric(data_test_sample$label[i]) + 1) - 1
  MSE <- MSE + (real - pred)^2
}

MSE <- MSE / length(sample_data_test)
cat("Valeur du résidu avec la validation croisée", MSE, "\n")