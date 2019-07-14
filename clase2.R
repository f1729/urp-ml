
banco<-read.csv("https://raw.githubusercontent.com/VictorGuevaraP/MachineLearning/master/prestamo.csv", sep = ";")
#Visualizamos lo 6 primeros registros 
head(banco)
#Obtenemos los nombres de los atributos
names(banco)
#Visualizamos la estructura de los datos (tipos de datos)
str(banco)
#Realizamos un resumen de los atributos del dataset
summary(banco)

#Ordenar niveles de variables categoricas ordinales si se requiere
banco$Edad = factor(banco$Edad,levels = c("Menor 34","34-56","Mayor de 56"), ordered=TRUE)
str(banco)

#Ordenar niveles de variables categoricas ordinales si se requiere
banco$Ingreso = factor(banco$Ingreso,levels = c("Menor a 25000","25000-45000","Mayor a 45000"), ordered=TRUE)

#Análisis univariado
#Iniciamos el an?lisis con la variable principal de estudio (TARGET)
table(banco$Prestamo)
table(banco$Prestamo)/length(banco$Prestamo)
#Mejoramos la tabla con librerias
install.packages("gmodels") #instalando libreria
library(gmodels)
CrossTable(banco$Prestamo, format = "SPSS")
CrossTable(banco$Prestamo, format = "SAS")
#Gráficamos 
barplot(table(banco$Prestamo), col = 2)
pie(table(banco$Prestamo))

barplot(prop.table(table(banco$Prestamo))*100,
        main="Distribución de la apróbación de préstamos", 
        col=3,xlab="TARGET",ylab="% de Clientes")

lbls1 <- paste(names(table(banco$Prestamo)), "\n",
               round(prop.table(table(banco$Prestamo))*100,2),"%", sep="")
library(plotrix)
pie3D(table(banco$Prestamo),labels=lbls1,explode=0.2, 
      main="Distribución de la apróbación de préstamos")

#Mejorando la visualización con la libraria ggplot2
library(ggplot2)
ggplot(data = banco) + 
  geom_bar(mapping = aes(x = Prestamo))

#Graficamos las demas variables
library(ggplot2)
ggplot(data = banco) + 
  geom_bar(mapping = aes(x = Edad))
ggplot(data = banco) + 
  geom_bar(mapping = aes(x = Edad, color = Prestamo))

#Si hay mucha diferencia entre los graficos con respecto al target se podria buscar correlacion
ggplot(data = banco) + 
  geom_bar(mapping = aes(x = Edad, fill = Prestamo))

#fill pintar todo
ggplot(data = banco) + 
  geom_bar(mapping = aes(x = Edad, fill = Prestamo))+coord_flip()

#position separa las barras y coord_flip cambia las coordenadas
ggplot(data = banco) + 
  geom_bar(mapping = aes(x = Edad, fill = Prestamo), position="dodge")+coord_flip()

ggplot(data = banco) + 
  geom_bar(mapping = aes(x = Hijos, fill = Prestamo))

hist(banco$Otros_ingreso)
ggplot(data = banco) + 
  geom_histogram(mapping = aes(x = Otros_ingreso, fill = Prestamo))

ggplot(data = banco) + 
  geom_density(mapping = aes(x = Otros_ingreso, fill =Prestamo))

#color solo los bordes, theme_minimal fondo
ggplot(data = banco) + 
  geom_density(mapping = aes(x = Otros_ingreso, color =Prestamo))+theme_minimal()
#Covariables: Todas las x que tratan de explicar a y
ggplot(data = banco) + 
  geom_freqpoly(mapping = aes(x = Otros_ingreso, color=Prestamo))
names(banco)

ggplot(data=banco) + 
  geom_histogram(mapping = aes(x=Tiempo_meses, fill=Prestamo))

# Dot Plots
dotchart(table(banco$Edad), cex=.9,
         main="Distribución de la Edad de los Clientes", 
         xlab="# de Clientes")
dotchart(prop.table(table(banco$Edad))*100,
         main="Distribución de la Edad de los Clientes", 
         xlab="% de Clientes")
##################################################
#Análisis bivariado
# Tablas de contingencia  
tablacon<-table(banco$Edad,banco$Prestamo)
tablacon
#Porcentajes (Condicionales)
tablaconprop<-prop.table(tablacon,margin=1)
tablaconprop
library(gmodels)
CrossTable(banco$Edad,banco$Prestamo)
?CrossTable
CrossTable(banco$Edad,banco$Prestamo,prop.r = 
             T, prop.c = F,prop.t = F,prop.chisq = F)

#Barras agrupadas
barplot(tablacon,col=c(2,3,4),beside = T,
        xlab="Edad",
        ylab="Proporción de Clientes",
        main="Distribución de la edad de los clientes según grupo aprobación")
legend("topright",legend=levels(banco$Edad),col=c(2,3,4),
       pch=15,title="Edades")

#Barras Componentes
barplot(tablacon,col=c(2,3,4),
        xlab="Edad",
        ylab="Proporción de Clientes",
        main="Distribución de la edad de los clientes según grupo aprobación")
legend("topright",legend=levels(banco$Edad),col=c(2,3,4),
       pch=15,title="Edades")

library(agricolae)

#Tallo y Hojas
stem(banco$Tiempo_meses)

# Sesgo y curtosis
library(e1071)
skewness(banco$Tiempo_meses)
kurtosis(banco$Tiempo_meses)

library(Hmisc)
h3<-banco$Otros_ingreso
hn<-h3[banco$Prestamo=="No"]
hy<-h3[banco$Prestamo=="Si"]

ds<-list(h3,hn,hy)
bpplot(...=ds,name=c("All","No","Yes"),
       ylab="Humidity3pm",xlab="RainTomorrow")


str(banco)
library(rgl)
plot3d(banco$Tiempo_meses, banco$Otros_ingreso,banco$Hijos, col=c(banco$Prestamo))

iris = read.csv("https://raw.githubusercontent.com/VictorGuevaraP/MachineLearning/master/irismod.csv", sep =";")
head(iris)
str(iris)
iris$Especies = as.factor(iris$Especies)

attach(iris)
B = plot3d(Sépalo.longitud, Sépalo.ancho, Pétalo.longitud, col =c(Especies))
#Hace que gire la imagen
play3d(spin3d(B))

publicidad = read.csv("https://raw.githubusercontent.com/VictorGuevaraP/MachineLearning/master/publicidad.csv",sep = ";")
head(publicidad)
B = plot3d(Sépalo.longitud, Sépalo.ancho, Pétalo.longitud, col =c(Especies))
#Hace que gire la imagen
play3d(spin3d(B))
#obtenemos la matriz de varianza y covarianzas
var(publicidad)

#obtenemos la matriz de correlaciones
cor(publicidad)
library(PerformanceAnalytics)
chart.Correlation(publicidad)

###########
attach(publicidad)
promy = sum(Ventas)/length(Ventas)
promx = sum(Television)/length(Ventas)

numerador = sum((Television - promx)*(Ventas-promy))
denominador = sum((Television-promx)^2)
numerador  
denominador

b1 = numerador/denominador
b1

b0 = promy - b1*promx
b0 

#y^ es la venta y x es la television
#y^ = 7.033 + 0.048x

###
y_estimado = b0+b1*Television
y_estimado

Ventas

errores = Ventas - y_estimado
errores

costo = (Ventas - y_estimado)^2
costo

##########################
modelo1 = lm(Ventas~Television,data = publicidad)
modelo2 = lm(Ventas~Television + Radio,data = publicidad)
modelo3 = lm(Ventas~.,data = publicidad)


coef(modelo1)
coef(modelo2)
coef(modelo3)



summary(modelo1)
summary(modelo2)
summary(modelo3)


plot(Ventas~Television)
abline(modelo1, col=2, pch=20)

par(mfrow=c(2,2))
plot(modelo1)
par(mfrow=c(1,1))

predichos =predict(modelo1, publicidad)
predichos

library(scatterplot3d)
scatterplot3d(Ventas,Television,Radio)

par(mfrow=c(1,2))
hist(Television)
hist(sqrt(Television))


#Transformacion de variables
#Interpretacion:Cuando se aumenta una unidad a la transformada de la tv y se mantiene constante las otras variables  
modelo4 = lm(Ventas~sqrt(Television)+Radio, data = publicidad)
summary(modelo4)

#estimar un modelo que ayude a explicar la grasa que uno tiene, basado en el peso, la talla, el rodillo, el tobillo,etc

cuerpo = read.csv("https://raw.githubusercontent.com/VictorGuevaraP/MachineLearning/master/Grasa.txt",sep="\t")
head(cuerpo)

summary(cuerpo)
tablac = table(cuerpo$Edad,cuerpo$Grasa)
tablac
chart.Correlation(cuerpo)
summary(cuerpo)

modelo5 = lm(Grasa~Peso, data = cuerpo) #37.51%
summary(modelo5)


modelo5 = lm(Grasa~Abdomen, data = cuerpo) #66%
summary(modelo5)

plot(cuerpo)
plot(cuerpo$Altura, cuerpo$Grasa)

#permite evaluar donde esta el valor atipico
identify(cuerpo$Altura, cuerpo$Grasa, labels=row.names(cuerpo))
fix(cuerpo)
cuerpo = cuerpo[-42,]


#construir un modelo de regresion multiple con la dara cuerpo con todo lo aprendido


