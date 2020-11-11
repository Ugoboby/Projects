######################## TP3 : BAYESIEN NAIF ########################


###### Exercice 1
## Question 1
install.packages("e1071")
install.packages("mlbench")
library(e1071)
## Données d'entrée  binaires
data(HouseVotes84, package = "mlbench")
help(HouseVotes84,package = "mlbench")
g <-naiveBayes(Class ~ ., data = HouseVotes84) # Compute la méthode de Bayesien naîf
g$apriori # Donne les probas à priori obtenues par la methode de bayésien naîf pour la classification des votants suivant leur parti
g$tables # Donne les repartition pour chaque vote des parts pour les démocrates et pour les républicains pour un vote oui/non
predict(g, HouseVotes84[1,]) # predit avec le model 1 le parti du 1er votant du jeu de donnée
predict(g, HouseVotes84[1,], type = "raw") # Donne les probas de classification du 1er votant avec le modèle
pred <-predict(g, HouseVotes84)
table(pred, HouseVotes84$Class) # Donne le tableau de confusion du modèle
# Données d'entrée quantitatives
data(iris)
g <-naiveBayes(Species ~ ., data = iris)
## ou encore:#
m <- naiveBayes(iris[,-5], iris[,5]) # On effectue une modélisation en bayésien naïf, cette fois-ci avec des données quanti;
g$apriori # Calcul des classes à priori
summary(iris[,5]) # On se rend compte que le modèle à priori retourne la table de contingence de la variable à expliquer
g$tables # Donne les moyennes et écrt types de chaque sous variables
table(predict(g, iris), iris[,5]) # Matrice de confusion du modèle g
install.packages("klaR")
library(klaR)
?NaiveBayes
m <-NaiveBayes(Species ~ ., data = iris) # Nouvelle fonction de création du modèle bayésien naïf
names(predict(m, iris))
table(predict(m, iris)$class, iris[,5]) # Matrice de confusion obtenue avec le modèle de la nouvelle fonction
m2 <-NaiveBayes(Species ~ ., data = iris, usekernel=TRUE) # On utilise la même fonction en precisiant l'estimation à noyau de la densité
names(predict(m2, iris))
table(predict(m2, iris)$class, iris[,5]) # On obtient la même matrice de confusion

## Question 2
# Avec le jeu de données Desbois
setwd("/home/hugo/Documents/Master BORDEAUX/M2/Semestre 1/Apprentissage Supervisée/TD/TD3")
load("Desbois.rda")
d <- NaiveBayes(data[,-1], grouping = as.factor(data[,1]))

