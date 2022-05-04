# TDR6 - JEU DE DONNEES Iris

# Import des données
iris1 <- read.table("DesIris.txt", header = TRUE, dec = ".", sep = " ", stringsAsFactors = TRUE)
iris1

## Test des commandes

iris1[ ,1] # sélection d'une colonne par son numéro avec [,n]
iris1[1, ] # sélection d'une ligne par son numéro avec [n,]
iris1[1,2] # sélection d'une case avec [nlig,ncol]
iris1$SepalLength # sélection d'une colonne par son nom
iris1[c(1,5,10), ] # sélection de plusieurs lignes par leur numéro
iris1[-(5:30), ] # sélection des lignes qui ne sont pas citées par leur numéro, ":" signifiant "à"
iris1[ ,1:4] # sélection des colonnes par leur numéro, ":" signifiant "à"
iris1[c(5,1,10) , c(2,1,4,3)] # sélection de certaines lignes et certaines colonnes par leur numéro
iris1[iris1$Species=="virginica", ] # sélection conditionnelle, égal à "..."
iris1[iris1$SepalLength > 7 , ] # sélection conditionnelle, valeur supérieure à "..."
iris1[iris1$SepalLength < 6  &  iris1$Species=="virginica" , ] # sélection conditionnelle, valeur inférieur à "..."
iris1[(iris1$SepalLength < 6  &  iris1$Species=="virginica") | iris1$Species=="setosa"  , ] # sélection conditionnelle

iris1$SepalLength[iris1$Species=="virginica"] # on n'utilise plus de virgule car on pose la condition sur un vecteur qui n'a ni lignes ni colonnes. 

# on peut aussi faire :
iris1[iris1$Species=="virginica",]$SepalLength # ici on utilise la virgule car on pose d'abord la condition sur le tableau

apply(iris1[ ,1:4], MARGIN=2, mean) # moyenne de chaque colonne à propos des colonnes nommées par le numéro (ici les colonnes de 1 à 4)
apply(iris1[ ,1:4], MARGIN=1, mean) # moyenne de chaque ligne à propos des colonnes nommées par leur numéro (ici les colonnes de 1 à 4)
tapply(iris1$SepalLength, iris1$Species, mean) # moyenne d'une valeur par rapport à une autre (ici moyenne de la longueur des sépales par rapport à l'espèce)

## Statistique
slv <- iris1$SepalLength[iris1$Species=="virginica"]
slv

mean(slv) # calcul de la moyenne
var(slv) # calcul de la variance
sqrt(var(slv)) # calcul de l'écart-type
sd(slv) # calcul de 
min(slv) # afficher la plus petite valeur
max(slv) # afficher la plus grande valeur
range(slv) #
sort(slv) # classement des valeurs de la plus petite à la plus grande
rev(slv) # 
sum(slv) # calcul de la somme
cumsum(slv) #
median(slv) # calcul de la médiane
quantile(slv,0.25) # calcul du 1e quartile (25% ici)
quantile(slv) # calcul des quartiles
length(slv) # calcul du nombre de valeur contenue

## Création de données

n <- nrow(iris1) # nrow() : nombre de lignes
Noms <- paste("iris", 1:n) # paste() : coller
head(Noms)

Rapport <- iris1$PetalLength/iris1$PetalWidth  
head(Rapport)

length(Noms)
length(Rapport)

names(iris1)

iris1$Noms <- Noms
names(iris1)

iris2 <- data.frame(iris1, Rapport)
head(iris2)

# quelle est l'espèce pour laquelle les pétales ont la forme la plus étroite ?

tapply(iris2$Rapport, iris2$Species, mean) 
#ou
Rapport_m<-tapply(iris2$Rapport, iris2$Species, mean) 
names(Rapport_m[Rapport_m==max(Rapport_m)])


# TDR6 - JEU DE DONNEES tvar

# Import du jeu de données
t3var <- read.table("t3var.txt", header = TRUE, sep = "\t", stringsAsFactors = TRUE)
t3var

names(t3var)
dim(t3var)
str(t3var)

# Sélections
t3var[c(1,10,20), ]

t3var[t3var$sexe == "f" & t3var$tai > 170, ]
f170 <- t3var[t3var$sexe == "f" & t3var$tai > 170, ]
nrow(f170)

t3var[10:20, -1]

fMoy <- mean(t3var$tai[t3var$sexe=="f"])
fMoy
fSupMoy <- t3var[t3var$tai[t3var$sexe=="f"] > fMoy, ]
fSupMoy
nrow(fSupMoy)

# Calculs

poidsMoy <- mean(t3var$poi)
poidsMoy
poidsMoySex <- tapply(t3var$poi, t3var$sexe, mean)
poidsMoySex

poidsVar <- var(t3var$poi)
poidsVar
poidsVarSex <- tapply(t3var$poi, t3var$sexe, var)
poidsVarSex

## Création de données

IMC <- t3var$poi / (t3var$tai)^2 

t3var$IMC <- IMC
head(t3var)
