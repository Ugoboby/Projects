## Exercice 1 : The k-means algorithm



# Question 2
X1 <- c(5,4,1,0)
X2 <- c(4,5,-2,-3)
X <- cbind(X1,X2)
#rownames(X) <- c(1,2,3,4)
rownames(X) <- seq(1:4)

kmeans <- kmeans(X, centers = 2)
kmeans
kmeans$cluster
# On regard Clustering vector et la première ligne correspond aux points alors que la ligne 2 
# correspond aux classes. On a donc les points 1 et 2 appartiennent à la classe 2 et les points 3 
# et 4 appartiennent à la classe 1

# Question 3
# Construction de g
w <- rep(1,4)
mu <- sum(w)
g.x <- 1/mu*sum(w%*%X1)
g.y <- 1/mu*sum(w%*%X2)
g <- c(g.x,g.y)

# Rappelons que T correspond à l'inertie totale
dcar <- dist(rbind(X,g))^2
T <- w%*%(as.matrix(dcar)[5,-5])
# Autre manière

T <- kmeans$totss
B <- kmeans$betweenss
W <- kmeans$tot.withinss
# On a bien T = B + W

# Question 4 

Propvar <- (1-W/T)*100
Propvar
# La proportion d'inertie expliquée par ma partition en deux classes est d'environ 97%

## Exercice 2
# Question 2 
library(cluster)
plot(X, asp = 1, pch = 3, xlim = c(-6,6), ylim= c(-6,6))
X_hc <- hclust(dist(X))
plot(X_hc)

# Question 3

dist_man <- dist(X, method = "manhattan")
X_hc <- hclust(dist_man)
plot(X_hc)

## Exercice 3

dist_ward <- (dist(X))^2/2*sum(w)
X_hc <- hclust(dist_ward, method="ward.D")
plot(X_hc)

#Si les poids des individus ne sont pas les mêmes
#for (i in 1:(n-1)) {
#  for (j in (i+1):n) {
#    Delta[n*(i-1) - i*(i-1)/2 + j-i] <-
#     Delta[n*(i-1) - i*(i-1)/2 + j-i]^2*w[i]*w[j]/(w[i]+w[j])}}
# tree <- hclust(Dist(X),method="ward.D",members=w)



