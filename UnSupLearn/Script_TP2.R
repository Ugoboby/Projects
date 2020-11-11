################# DATA MINING #################
############ TP2 #################


# Chargement des données
setwd("/home/hugo/Documents/Master BORDEAUX/M2/Semestre 1/Data Mining/TP/TP 2")
load("dogs.rda")
print(data[1:5,])


# Types de données du jeu de données
class(data)
data$Size # Donne toute les modalitées de la variable Size ainsi que les diffèrentes classes
class(data$Size)
levels(data$Size)

# Niveau des variables
lj <- unlist(lapply(data, function(x){length(levels(x))}))
l <- sum(lj)

# Construction de la matrice disjonctive K
install.packages("chron")
library(chron)
install.packages("FactoMineR", dependencies = T)
library(FactoMineR)
K <- tab.disjonctif(data)
print(K[1:4,])

# Calcul des frequences de chaque variable et leurs frequences relatives
ns <- apply(K,2,sum)
print(ns)
n <- nrow(K)x = fs <- ns/n
print(fs)

# Construction de la matrice disjonctives centrée Z
Z <- K-colMeans(K) 

# Calcul 
sqrt(sum((Z[12,]-Z[22,])^2/fs))