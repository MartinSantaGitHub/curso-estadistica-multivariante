library(MASS) 
library(HSAUR2)
library(tidyverse)

df <- read.csv("Datos/spaincities.csv",header = T)
df

colnames(df) <- c("X",as.character(df[,"X"]))
df

d_matrix <- as.matrix(df)
D <- as.dist(d_matrix[,2:7])
D <- as.matrix(D)

mds.cities <- cmdscale(D,eig=TRUE)
mds.cities$eig

###

ggplot() + 
  geom_point(aes(1:6,mds.cities$eig), color = "blue", size = 2.5) + 
  geom_line(aes(1:6,mds.cities$eig), color = "blue") +
  geom_hline(yintercept = 0, color = "red") + 
  labs(x = "Number", y = "Eigenvalue")

plot(mds.cities$eig,pch=19,col="blue",xlab="Number",ylab="Eigenvalue",type="o")
abline(a=0,b=0,col="red")

gr_bondad <- sum(abs(mds.cities$eig[1:2])) / sum(abs(mds.cities$eig))
gr_bondad

###

mds.cities <- cmdscale(D,eig=TRUE,k=2)

x1 <- mds.cities$points[,1]
x2 <- mds.cities$points[,2]
x1
x2

###

ggplot() +
  geom_point(aes(x=x1,y=x2),color = "blue") +
  geom_text(aes(x=x1,y=x2),label = rownames(D), color = "red", nudge_x = 70) +
  scale_x_continuous(limits = range(x1) + c(0,600))

###

x1 <- -x1

ggplot() +
  geom_point(aes(x=x1,y=x2),color = "blue") +
  geom_text(aes(x=x1,y=x2),label = rownames(D), color = "red", nudge_x = 70) +
  scale_x_continuous(limits = range(x1) + c(0,600))
