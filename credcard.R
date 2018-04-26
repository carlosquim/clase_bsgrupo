
# Reducción de dimensionalidad y clasificación

# Carga paquetes
library(ggplot2) # visualización
library(caret)
library('xda') # Análisis Exploratorio de Datos

# Importa datos
ucicc.dat <- read.csv("/home/kirito/data/UCI_Credit_Card.csv")
names(ucicc.dat) <- tolower(names(ucicc.dat))

#Estadísticos de resumen
summary(ucicc.dat)
#numSummary(ucicc.dat)
#charSummary(ucicc.dat)

#Modificando sexo y educación
table(ucicc.dat$sex)
table(ucicc.dat$education)
table(ucicc.dat$marriage)

# Conversión a pseudobinaria
# Por ahora lo haremos manual, sin embargo existen varios paquetes que lo hacen automático

dat.2 <- ucicc.dat
dat.2$hombre <- ifelse(dat.2$sex==1,1,0)
dat.2$mujer <- ifelse(dat.2$sex==2,1,0)

# Educación tiene varios niveles X3: Education (1 = graduate school; 2 = university; 3 = high school; 4 = others).
# Agrupando 0 y 4 a 6 en uno...
dat.2$e.pos <- ifelse(dat.2$education==1,1,0)
dat.2$e.uni <- ifelse(dat.2$education==2,1,0)
dat.2$e.prp <- ifelse(dat.2$education==3,1,0)
dat.2$e.otr <- ifelse(!(dat.2$education %in% 1:3),1,0)

head(dat.2)
dat.3 <- dat.2[,c(2,6:24,26:31,25)]
head(dat.3)


#Quitando correlaciones

dat.cor <- round(cor(dat.3),3)
abs(dat.cor)

#Quitemos sólo sexo
cor(dat.3$hombre,dat.3$mujer)
dat.4 <- dat.3[,-which(names(dat.3)=='hombre')]      


#Se usará la función prcomp para fines didácticos. 
#Se recomienda usar las funciones en el paquete caret, ya que integran el PCA dentro de los parámetros para entrenamiento
dat.pca <- prcomp(dat.4[,1:25]) 
summary(dat.pca)

#Genial. Podemos explicar >95% de los datos con sólo cuatro variables!
plot(dat.pca)
plot(dat.pca$x[,1:2],col=factor(dat.4$default.payment.next.month,levels=c(0,1)))



