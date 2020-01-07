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

#Moyenne et écart type
mean <- apply(data_train,2,mean)
std <- apply(data_train,2,sd)*sqrt(5/6)
stats <- rbind(mean,std)

print(stats,digits=4)

# ACP sur les données d'origine

acp <- PCA(data_train,graph=TRUE,scale.unit=FALSE)
# Figure individus
plot(acp,choix="ind",cex=1.5,title="")
# Figure variables
plot(acp,choix="var",cex=1.5,title="")

# ACP sur les données centrées-réduites
acpnorm <- PCA(data_train,graph=TRUE)
# Figure individus
plot(acpnorm,choix="ind",cex=1.5,title="")
# Figure variables
plot(acpnorm,choix="var",cex=1.5,title="")


# Inertie (variance) des composantes principales
acpnorm$eig
barplot(acpnorm$eig[,1])
warnings(50)