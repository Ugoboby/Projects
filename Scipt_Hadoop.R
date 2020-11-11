install.packages(c("rJava", "Rcpp", "RJSONIO", "bitops", "digest", "functional", "stringr", "plyr", "reshape2", "caTools"))
Sys.setenv(HADOOP_HOME="/home/hadoop/hadoop-1.1.2") 
Sys.setenv(HADOOP_CMD="/home/hadoop/hadoop-1.1.2/bin/hadoop")
Sys.setenv(HADOOP_STREAMING="/home/hadoop/work/hadoop-1.1.2/contrib/streaming/hadoop-streaming-1.1.2.jar")
Sys.setenv(JAVA_HOME="/usr/lib/jvm/java-1.6.0-openjdk-amd64")
library(rmr2)

library(rmr2)
rmr.options(backend = "local")
# création d'un objet de type big data
test <-to.dfs(1:10)
# retour à 
Rtest2 <-from.dfs(test)
# création d'une liste de (clef,valeur)
test3 <-keyval(1,1:10)
keys(test3)
values(test3)
# mtcars est un data frame contenant la variable
# nombre de cylindres. Cette variables est définie comme
# clé, la valeur associée est la ligne correspondante du data frame
keyval(mtcars[,"cyl"],mtcars)

# carrés d'entiers
entiers <-to.dfs(1:10)
calcul.map = function(k,v){cbind(v,v^2)}
calcul <-mapreduce(input = entiers,
                   map = calcul.map
                   # la fonction reduce est nulle par defaut
                   )
resultat <-from.dfs(calcul)
resultat


# somme des carrés d'entiers
calcul2.map = function(k,v){keyval(1,v^2)}
calcul2.reduce = function(k,v){sum(v)}
calcul2 <-mapreduce(input = entiers,
                    map = calcul2.map,
                    reduce = calcul2.reduce)
resultat2 <-from.dfs(calcul2)
resultat2

entiers3 <-to.dfs(1:1000000) # Création d'un element (objet) hdfs
calcul3.map = function(k,v){keyval(1,1)} #Affecte la combinaison clé|valeur (1,1)
calcul3.reduce = function(k,v){sum(v)}
calcul3 <-mapreduce(input = entiers3,
                    map = calcul3.map,
                    reduce = calcul3.reduce) # somme sur chaque bloc
resultat3 <-from.dfs(calcul3) # retourne la somme d'un bloc de k element,ici 5.
resultat3

### Question 1 : Comment expliquez-vous la valeur de resultat3? 
  # C'est la somme des deux premiers carrés.

# 4.2  Structuration des données en blocs

entiers4 <-to.dfs(1:1000000)
calcul4.map = function(k,v){keyval(v[1],v[length(v)])}
calcul4.reduce = function(k,v){keyval(k,v)}
calcul4 <-mapreduce(input = entiers4,
                    map = calcul4.map,
                    reduce = calcul4.reduce)
resultat4 <-from.dfs(calcul4)
resultat4 # renvoi la clé de la premiére valeur, puis prend la valeur du premier element du prochain bloc en clé et renvois la valeur du premier element du bloc suivant


matrice6 <-to.dfs(matrix(1:1000000,10,100000))
calcul6.map = function(k,v){keyval(v[1,1],dim(v))}
calcul6.reduce = function(k,v){keyval(k,v)}
calcul6 <-mapreduce(input = matrice6
                    ,map = calcul6.map,
                    reduce = calcul6.reduce)
resultat6 <-from.dfs(calcul6)
resultat6 # renvoi le nombre d'élement des matrices découpés depuis la matrice totale 10*10000
# recoupe là encore en conservant la structure tableau en decoupant par nombre de ligne max gardant les colones


matrice7 <-to.dfs(matrix(1:1000000,100000,10))
calcul7.map = function(k,v){keyval(v[1,1],dim(v))}
calcul7.reduce = function(k,v){keyval(k,v)}
calcul7 <-mapreduce(input = matrice7,
                    map = calcul7.map,
                    reduce = calcul7.reduce)
resultat7 <-from.dfs(calcul7)
resultat7


# Question 2 : Comment expliquez-vous les valeurs de resultat4, resultat6 et resultat7 ?

matrice8 <-to.dfs(list(matrix(1:1000000,1000000,1),
                       matrix(1:10,2,5),
                       matrix(1:10,5,2)))
calcul8.map = function(k,v){keyval(v[[1]][1],dim(v[[1]]))}
calcul8.reduce = function(k,v){keyval(k,v)}
calcul8 <-mapreduce(input = matrice8,map = calcul8.map,reduce = calcul8.reduce)
resultat8 <-from.dfs(calcul8)
resultat8

tirage <-to.dfs(rbinom(32,n=50,prob=0.4))# generation de 50 nombres avec une loi binomial
# le map associe à chaque entier une paire (entier,1)
comptage.map = function(k,v){keyval(v,1)}
comptage.reduce = function(k,v){keyval(k,length(v))}
comptage <-mapreduce(input = tirage,map = comptage.map,reduce = comptage.reduce)
from.dfs(comptage)
table(values(from.dfs(tirage)))

## 4.4 Salve, munde!

#On définit une fonction wordcount pour le comptage de mots
wordcount = function(input,pattern = " "){
  #input : texte à analyser au format big data
  #pattern : sigle utilisé pour la séparation des mots
  #          (" " par défaut)
  wordcount.map = function(k,texte){
    keyval(unlist(strsplit(x = texte, split = pattern)),1)
  }
  wordcount.reduce = function(word,count){
    keyval(word,sum(count))
  }
  resultat<-mapreduce(input = input,map = wordcount.map,reduce = wordcount.reduce)
  return(resultat)
}

#Un exemple d'utilisation avec un texte simple
texte =c("un petit texte pour l'homme mais un grand grand grand texte pour l'humanité")
from.dfs(wordcount(to.dfs(texte)))


# 5. Algorithmes des K-means

#fonction principale
kmeans = function(Pts,nbClusters,nbIter){
  #P : pts (au format big data) sur lesquels on fait un k-mean
  # nbClusters : nombre de clusters désirés
  # nbIter : nombre d'itérations dans le k-mean
  # retourne C : matrice des pts du k-mean
  #calcul de distance
  distance = function(c,P){
    #determine pour chaque point de P
    #la distance a chaque point de c
    resultat <-matrix(rep(0,nrow(P)*nrow(c)),nrow(P),nrow(c))
    for (i in 1:nrow(c)){
      resultat[,i] <-rowSums(
        (P-matrix(rep(c[i,],nrow(P)),nrow(P),ncol(P), byrow = TRUE))^2)
      }
    return(resultat)
  }
  #fonctions map : initialisation ou détermination des
  # centres les plus proches
  km.map = function(.,P){if (
    is.null(C)){
      #initialisation au premier tour
      pproche <-sample(1:nbClusters,nrow(P),replace = TRUE)
    }
    else {
      #sinon on cherche le plus proche
      D <-distance(C,P)
      pproche <-max.col(-D)
    }
    keyval(pproche,P)
  }
  #fonction reduce : calcul des barycentres
  km.reduce = function(.,G){
    t(as.matrix(apply(G,2,mean) ))
  }
  #programme principal
  # initialisation
  C <- NULL
  # iterations
  for(i in 1:nbIter){
    resultat =from.dfs(mapreduce(
      input = Pts,
      map = km.map,
      reduce = km.reduce))
    C <-values(resultat)
    # si des centres ont disparu
    # on en remet aléatoirement
    if (nrow(C) < nbClusters){
      C =rbind(C,matrix(rnorm((nbClusters-nrow(C))*nrow(C)),
                        ncol =nrow(C))%*%C)
    }
  }
  return(C)
}


#simulation de 5 centres plus bruit gaussien
#on reinitialise le generateur pour faciliter le debogage
set.seed(1)
P <-matrix(rep(0,200),100,2)
for (i in 1:5){
  P[((i-1)*20+1):(i*20),] <- (matrix(rep(rnorm(2, sd = 20),20),ncol=2,byrow = TRUE) +
                                matrix(rnorm(40),ncol=2))}
#test
resultat <-kmeans(to.dfs(P),5,8)
plot(P)
points(resultat,col="blue",pch=16)


## Même si le code fonctionne sur notre simulation, il faut faire attention lors de la compilation de ce genre de méthode.
# En effet après l'étape de shuffle  le calcul des barycentres, reviennent à un calcul classique.
# Pour contourner ce problème, il serait nécessaire de parralèliser ces calculs de moyenne, en créant de nouveaux blocs,
#en enregistrant, pour chaque classe, la somme des distances au barycentre de chacune des obsérvations, ainsi que le nombre d'observation
#de la classe.

#fonction principale
kmeans2 = function(Pts,nbClusters,nbIter){
  #P : pts (au format big data) sur lesquels on fait un k-mean# nbClusters : nombre de clusters désirés
  # nbIter : nombre d'itérations dans le k-mean# retourne C : matrice des pts du k-mean
  #calcul de distance
  distance = function(c,P){#determine pour chaque point de P
    #la distance a chaque point de c
    resultat <-matrix(rep(0,nrow(P)*nrow(c)),nrow(P),nrow(c))
    for (i in 1:nrow(c)){
      resultat[,i] <-rowSums((P-matrix(rep(c[i,],nrow(P)),nrow(P),ncol(P), byrow = TRUE))^2)}
    return(resultat)
  }
#fonctions map : initialisation ou détermination des# centres les plus proches
  km.map = function(.,P){if (is.null(C)){#initialisation au premier tour
    pproche <-sample(1:nbClusters,nrow(P),replace = TRUE)
    }else {
      #sinon on cherche le plus proche
      D <-distance(C,P)
      pproche <-max.col(-D)
    }
    #On ajoute un poids 1 à chaque points
    keyval(pproche,cbind(1,P))
  }
#fonction reduce : calcul des barycentres
  km.reduce = function(k,G){
    keyval( k ,t(as.matrix(apply(G,2,sum))) )
  }
  #programme principal
  # initialisation
  C <- NULL
  # iterations
  for(i in 1:nbIter){
    resultat =from.dfs(mapreduce(input = Pts,map = km.map,reduce = km.reduce,combine = TRUE,in.memory.combine = TRUE))
    C <-values(resultat)
    # calcul des moyennes : on divise les sommes par le nombre d'éléments sommés
    C <- C[,-1]/C[,1]
    # si des centres ont disparu# on en remet aléatoirement
    if (nrow(C) < nbClusters){
      C =rbind(C,matrix(rnorm((nbClusters-nrow(C))*nrow(C)),ncol =nrow(C))%*%C)
    }
  }
  return(C)
}

#simulation de 5 centres plus bruit gaussien#on reinitialise le generateur pour faciliter le debogage
set.seed(1)
P <-matrix(rep(0,200),100,2)
for (i in 1:5){
  P[((i-1)*20+1):(i*20),] <- (matrix(rep(rnorm(2, sd = 20),20),ncol=2,byrow = TRUE) +
                                matrix(rnorm(40),ncol=2))
  }
#test
resultat <-kmeans2(to.dfs(P),5,8)
plot(P)
points(resultat,col="blue",pch=16)



# 5.2  Régression linéaire

#fonction principale
regression = function(data){
  #fonction map pour XtX
  xtx.map = function(.,v){
    X <-as.matrix(cbind(matrix(rep(1,nrow(v)),ncol=1) , v[,-1] ))
    keyval(1,list(t(X)%*%X))
  }
  #fonction reduce pour XtX
  xtx.reduce = function(.,v){
    keyval(1,Reduce("+", v))
    #keyval(1,v)
  }
  #Calcul de XtX
  xtx =mapreduce(
    input = data,
    map = xtx.map,
    reduce = xtx.reduce)
  # Il est possible de rajouter un combine ici, qui nous permettras de diminuer le nombre de calcul
  #fonction map pour Xty
  xty.map = function(.,v){
    y <- v[,1]
    X <-cbind(matrix(rep(1,nrow(v)),ncol=1) , v[,-1] )
    keyval(1,list(t(X)%*%y))
  }
  #Calcul de Xty
  xty =mapreduce(
    input = data,
    map = xty.map,
    reduce = xtx.reduce)
  #Regression linéaire
  xtx <-values(from.dfs(xtx))
  xty <-values(from.dfs(xty))
  return(solve(xtx,xty))
}

#on reinitialise le generateur pour faciliter le debogage
set.seed(1)
#generation des variables explicatives
X <-matrix(rnorm(3000), ncol=10)
#generation des variables à expliquer
y <- X%*%(as.matrix(rep(c(1,2),5)) ) +
  matrix(rnorm(300),ncol=1)
# data frame
base <-data.frame("y"=y,X)
resultat1 <-regression(to.dfs(base))
resultat1
resultat2 <-lm(y~.,data=base)
resultat2

## 6  Exemples d’analyse d’un jeu de données réelles avec Rhadoop

# Importation du jeu de données
web_google <-read.table("web-Google.txt", header=TRUE)
# On transforme l'ensemble de notre jeu de données en un objet "big.data.object"
web_google_bigdata <-to.dfs(web_google)

#la fonction map fait remonter la taille de chaque bloc
bloc.map = function(k,v){keyval(1,dim(v))}
#la fonction reduce ne fait rien
bloc.reduce = function(k,v){keyval(k,v)}
calculbloc <-mapreduce(
  input = web_google_bigdata,
  map = bloc.map,
  reduce = bloc.reduce)
resultatbloc <-from.dfs(calculbloc)
#resultatbloc permet de voir le nombre de blocs
#et la taille de chaque blocresultatbloc

# Nombre de liens
# Transformation des données en big.data.object
temp <- web_google[,1]
web_google_bigdata2 <-to.dfs(temp)
arc.map = function(k,v){keyval(v,1)
  # La'valeur'de notre paire vaut 1
}
arc <-mapreduce(input = web_google_bigdata2,map = arc.map)
# On somme toutes les'valeurs'de nos paires, on obtiendra le nombre de ligne :
nombre_arc <-sum(values(from.dfs(arc)))
nombre_arc
arc2.map = function(k,v){
  keyval(1,v[,1])
  # La clé vaut 1 et la valeur vaut la première colonne des données
  }
arc2 <-mapreduce(input = web_google_bigdata,map = arc2.map)
#la clé 1 est reproduite autant de fois
#qu'il y a de lignes. On a juste à sommer
#pour obtenir le nombre total de lignes
nombre_arc2 <-sum(keys(from.dfs(arc2)))
nombre_arc2

arc3.map = function(k,v){
  keyval(1,0*v[,1])
  # La clé vaut 1 et la valeur vaut la première colonne des données
  # où les valeurs sont remplacées par des 0
  }
arc3 <-mapreduce(input = web_google_bigdata,map = arc3.map)
#la clé 1 est reproduite autant de fois
#qu'il y a de lignes. On a juste à sommer
#pour obtenir le nombre total de lignes
nombre_arc3 <-sum(keys(from.dfs(arc3)))
nombre_arc3

arc4.map = function(k,v){
  keyval(1,length(v[,1]))
  # La clé vaut 1 et la valeur vaut la longueur de la première colonne des données
  }
arc4 <-mapreduce(input = web_google_bigdata,map = arc4.map)
#On a juste a sommer les valeurs
nombre_arc <-sum(values(from.dfs(arc4)))
nombre_arc