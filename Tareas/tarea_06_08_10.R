library(tidyverse)
library(scales)
library(cluster)

df <- read_csv("Datos/companies79.csv")
head(df)

df[,"X1"] <- NULL

colnames(df) <- c("Name","Goods","Sells","MarketValue","Profits","CashFlow","Employees","Department")

df

dept_fct_levels <- unique(df[,"Department"])

df$Department <- as_factor(df$Department)
df$Department

Xf <- df[,2:7]
Xf

View(df)

## TAREA 06 ##

Xtest <- scale(Xf)
head(Xtest)

R <- cor(Xf)
covM <- cov(Xtest)
R
covM

eR <- eigen(R)
eR

eigen.val <- eR$values
eigen.val
prop.var.acum <- cumsum(eigen.val) / sum(eigen.val) 
prop.var.acum

pca_obj <- princomp(Xf, cor = T)
var_explained_df <- data.frame(PC = paste0("PC",1:6),
                               var_explained=(pca_obj$sdev)^2/sum((pca_obj$sdev)^2))

var_explained_df %>%
  ggplot(aes(x = PC, y = var_explained, group = "pca")) +
  geom_point(size = 3) +
  geom_line(color = "blue") +
  labs(y = "Variance Explained") +
  ggtitle("Scree plot") + 
  theme(plot.title = element_text(hjust = 0.5))

mu <- colMeans(Xf)
mu

n <- nrow(Xf)
X.cen <- as.matrix(Xf) - mu
X.cen

Dx <- diag(diag(R))
Dx

Y <- X.cen %*% solve(Dx)^(1/2)
Y

eigen.vec <- eR$vectors

scores <- Y %*% eigen.vec
colnames(scores) <- c("PC1","PC2","PC3","PC4","PC5","PC6")
scores

Corr <- diag(eigen.val[1:3]^(1/2)) %*% t(eigen.vec[,1:3])
Corr

## TAREA 08 ##

L.est.1 <- eigen.vec[,1:3] %*% diag(sqrt(eigen.val[1:3]))
L.est.1

L.est.1.var <- varimax(L.est.1)
L.est.1.var

# PCFA
prop.var.accum.pcfa <- cumsum(eigen.val) / sum(eigen.val) 
prop.var.accum.pcfa

Psi.est.1 <- diag(diag(R - as.matrix(L.est.1.var$loadings) %*% t(as.matrix(L.est.1.var$loadings))))
Psi.est.1

RP <- R - Psi.est.1

eRP <- eigen(RP)
eRP

eigen.val <- eRP$values # Auto-valores 
eigen.val

eigen.vec <- eRP$vectors # Auto-vectores
eigen.vec

L.est.2 <- eigen.vec[,1:3] %*% diag(sqrt(eigen.val[1:3]))
L.est.2

L.est.2.var <- varimax(L.est.2)
L.est.2.var

# PCA
prop.var.accum.pca <- cumsum(eigen.val) / sum(eigen.val) 
prop.var.accum.pca

Psi.est.2 <- diag(diag(R - as.matrix(L.est.2.var$loadings) %*% t(as.matrix(L.est.2.var$loadings))))
Psi.est.2

# Scores

# PCFA
FS.est.1 <- scale(Xf) %*% as.matrix(L.est.1.var$loadings)
FS.est.1

# PFA
FS.est.2 <- scale(Xf) %*% as.matrix(L.est.2.var$loadings)
FS.est.2

L.est.1.var$loadings
L.est.2.var$loadings

par(mfrow=c(1,1))

Xf$Name = df$Name

plot(FS.est.1[,1],FS.est.1[,2],xlab="First factor",ylab="Second factor",main="Scores with first method",pch=19,col="blue")
text(FS.est.1[,1],FS.est.1[,2],labels=Xf$Name,pos = 4,col="blue")

plot(FS.est.2[,1],-FS.est.2[,2],xlab="First factor",ylab="Second factor",main="Scores with second method",pch=19,col="blue")
text(FS.est.2[,1],-FS.est.2[,2],labels=Xf$Name,pos = 4,col="blue")

plot(FS.est.1[,1],FS.est.1[,3],xlab="First factor",ylab="Third factor",main="Scores with first method",pch=19,col="blue")
text(FS.est.1[,1],FS.est.1[,3],labels=Xf$Name,pos = 4,col="blue")

plot(FS.est.2[,1],FS.est.2[,3],xlab="First factor",ylab="Third factor",main="Scores with second method",pch=19,col="blue")
text(FS.est.2[,1],FS.est.2[,3],labels=Xf$Name,pos = 4,col="blue")

plot(FS.est.1[,2],FS.est.1[,3],xlab="Second factor",ylab="Third factor",main="Scores with first method",pch=19,col="blue")
text(FS.est.1[,2],FS.est.1[,3],labels=rownames(Xf),pos = 4,col="blue")

plot(-FS.est.2[,2],FS.est.2[,3],xlab="Second factor",ylab="Third factor",main="Scores with second method",pch=19,col="blue")
text(-FS.est.2[,2],FS.est.2[,3],labels=rownames(Xf),pos = 4,col="blue")

df[c(22,38,40),]

## TAREA 10 ##

df <- df[-c(38,40),]

df %>% 
  filter(Name %in% c("GeneralElectric","IBM"))

###

df.scaled <- scale(df[,2:7])

#################
# Single linkage
#################

Agnes.Euc.Single <- agnes(df.scaled,metric="euclidean",method="single")
plot(Agnes.Euc.Single,main="Single linkage")
rect.hclust(Agnes.Euc.Single,k=2,border="blue")
rect.hclust(Agnes.Euc.Single,k=3,border="blue")
rect.hclust(Agnes.Euc.Single,k=4,border="blue")
rect.hclust(Agnes.Euc.Single,k=5,border="blue")

##################
# Complete linkage
##################

Agnes.Euc.Complete <- agnes(df.scaled,metric="euclidean",method="complete")
plot(Agnes.Euc.Complete,main="Complete linkage")
rect.hclust(Agnes.Euc.Complete,k=2,border="blue")
rect.hclust(Agnes.Euc.Complete,k=3,border="blue")
rect.hclust(Agnes.Euc.Complete,k=4,border="blue")
rect.hclust(Agnes.Euc.Complete,k=5,border="blue")

#################
# Average linkage
#################

Agnes.Euc.Average <- agnes(df.scaled,metric="euclidean",method="average")
plot(Agnes.Euc.Average,main="Average linkage")
rect.hclust(Agnes.Euc.Average,k=2,border="blue")
rect.hclust(Agnes.Euc.Average,k=3,border="blue")
rect.hclust(Agnes.Euc.Average,k=4,border="blue")
rect.hclust(Agnes.Euc.Average,k=5,border="blue")

#############################
#  Método jerárquico divisivo
#############################

Diana.Euc <- diana(df.scaled,metric="euclidean")
plot(Diana.Euc,main="Diana")
rect.hclust(Diana.Euc,k=2,border="blue")
rect.hclust(Diana.Euc,k=3,border="blue")
rect.hclust(Diana.Euc,k=4,border="blue")
rect.hclust(Diana.Euc,k=5,border="blue")

df[22,]

###

Kmeans.3 <- kmeans(df.scaled,3,nstart=25)
Cl.kmeans <- Kmeans.3$cluster
clusplot(df.scaled,Cl.kmeans)
text(princomp(df.scaled)$scores[,1:2],labels=rownames(df.scaled),pos = 1,col="blue")

dist.Euc <- dist(df.scaled,method="euclidean")
Sil.kmeans <- silhouette(Cl.kmeans,dist.Euc)
plot(Sil.kmeans,main="Silhouette for k-means",col="blue")

pam.3 <- pam(df.scaled,3)
Cl.pam <- pam.3$clustering
clusplot(df.scaled,Cl.pam)
text(princomp(df.scaled)$scores[,1:2],labels=rownames(df.scaled),pos = 1,col="blue")

Sil.pam <- silhouette(Cl.pam,dist.Euc)
plot(Sil.pam,main="Silhouette for pam",col="blue")

###

col.cluster <- c("blue","red","green")[Cl.kmeans]
pairs(df.scaled,col=col.cluster,main="k-means",pch=19)

col.cluster <- c("blue","red","green")[Cl.pam]
pairs(df.scaled,col=col.cluster,main="pam",pch=19)

###

df[which(Cl.kmeans==1),]
df[which(Cl.pam==1),]
