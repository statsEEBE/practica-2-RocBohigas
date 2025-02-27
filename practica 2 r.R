mis_dades <- iris
dim(mis_dades)
names(mis_dades)

mean(mis_dades$Petal.Length)
sd(mis_dades$Petal.Length) #desviació tipica
hist(mis_dades$Petal.Length) #histograma

x <- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length
plot(x,y) #dibuix

m <- sum(x-mean(x))*(y-mean(y))/sum((x-mean(x))^2)
b <- mean(y)-mean(x)

m*1.5+b

mod <- lm(y~x) #regresio lineal --lm
summary(mod) #resumen estadístico del modelo de regresión lineal

ypredict <- predict(mod, data.frame(x=x))

plot(x,y, col="blue",pch=16)
lines(x,ypredict,col="black")

#coeficiente de determinación
Rsq=sum((yppredict-mean(y))^2)/sum((y-mean(y))^2)
Rsq


