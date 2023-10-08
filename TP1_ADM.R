### TP1 - ANALYSE DES DONNÉES MULTIDIMENTIONNELLES

## Partie 1 - 

# On importe les données :
data <- read.csv("~/Desktop/wine.csv", stringsAsFactors = FALSE)

# On selectionne les variables quantitatives :
data <- data [4:32]

# On initialise une matrice A :
A <- matrix(0,nrow = 21, ncol=29)
for (i in 1:29){
  A[,i] <- (data[[i]])
}

# On crée la matrice des poids :
W <- diag(1/21, 21)

# On crée le vecteur 1_21 :
un <- rep(1, 21)

# Question 1 -

# Centrer-réduire les variables quantitatives :
xb <- t(A)%*%W%*%un                 # Vecteur des moyennes des variables
Z <- A - un%*%t(xb)                 # On projete sur l'orthogonale
var <- diag(diag(t(Z)%*%W%*%Z))     # On calcule les normes
Z <- Z%*%solve(sqrt(var))           # On normalise les projetés

# Calcul du barycentre :
bary <- t(Z)%*%W%*%un
print (bary)

# Calcul de l'intertie : 
inertie <- sum(diag(Z%*%t(Z)%*%W))
print (inertie)

# Question 2 -

# On commence par recoder les variables qualitatives :
label <- data$Label
#bourgueuil <- 1. *(label=="Bourgueuil")
#chinon <- 1. *(label=="Chinon")
#saumur <- 1. *(label=="Saumur")

bourgueuil <- ifelse(label == "Bourgueuil", 1, 0)
chinon <- ifelse(label == "Chinon", 1, 0)
saumur <- ifelse(label == "Saumur", 1, 0)

B <- cbind(bourgueuil,chinon,saumur)

# On calcule les poids et les barycentres au sein des classes :
poids <- t(B)%*%W%*%B
bary_class <- solve(poids)%*%t(B)%*%W%*%Z
