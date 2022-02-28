#PUNTO 1

#municipionombre - cualitativo nominal

#medidas de tendencia centra y dispersión
var1<-Antioquia$municipionombre
mean(var1) #Media
median(var1) #mediana
hist(var1) #histograma
var(var1) #varianza
sd(var1) #desviacion estandar
max(var1)-min(var1) #rango
sd(var1)/mean(var1) #coeficiente de variación
quantile(var1, c(0.3,0.5,0.7,0.9))

#Frecuencia absouluta
fa<-table(Antioquia$municipionombre)
print(fa)

#Frecuencia absoluta acumulada
cumsum(table(Antioquia$municipionombre))


#Frecuencia relativa
fr<-table(Antioquia$municipionombres)/length(Antioquia$municipionombre)
print(fr)


#Frecuancia relativa acumulada
cumsum(table(Antioquia$municipionombre)/length(Antioquia$municipionombre))

#Gráfica
bart1<-barplot(fa,main = "Gráfica Frecuencias Absolutas" ,ylim = c(0, 300),col = rainbow(5))
text(bart,x+2,labels=x)

#bandera - cualitativo nominal

#medidas de tendencia centra y dispersión
var2<-Antioquia$bandera
mean(var2) #Media
median(var2) #mediana
hist(var2) #histograma
var(var2) #varianza
sd(var2) #desviacion estandar
max(var2)-min(var2) #rango
sd(var2)/mean(var2) #coeficiente de variación
quantile(var2, c(0.3,0.5,0.7,0.9))

#frecuencia absoluta
fa2<-table(Antioquia$bandera)
print(fa2)

#Frecuencia absoluta acumulada
cumsum(table(Antioquia$bandera))


#Frecuencia relativa
fr2<-table(Antioquia$bandera)/length(Antioquia$bandera)
print(fr2)


#Frecuancia relativa acumulada
cumsum(table(Antioquia$bandera)/length(Antioquia$bandera))

#Gráfica
bart2<-barplot(fa2,main = "Gráfica Frecuencias Absolutas" ,ylim = c(0, 500),col = rainbow(5))
text(bart,x+2,labels=x)

#precio - cuantitativa continua
var3<-Antioquia$precio
mean(var3) #Media
median(var3) #mediana
hist(var3) #histograma
var(var3) #varianza
sd(var3) #desviacion estandar
max(var3)-min(var3) #rango
sd(var3)/mean(var3) #coeficiente de variación
quantile(var3, c(0.3,0.5,0.7,0.9))

x<-hist(Antioquia$precio, col=rainbow(10), xlab="Precios")
plot(x)


#PROBLEMA 3
p3<-Antioquia$precio
auxp<-Antioquia$precio
jj<-boxplot(auxp, horizontal=TRUE, main="Tiempo", xlab="Minutos", col=rainbow(1))


boxplot.stats(p3)
aux<-jj$out
print(aux)

p3<-p3[! p3 %in% aux]
boxplot(p3, horizontal=TRUE,main="Tiempo",col="purple", xlab="Precios")

#Problema 3.a

p1=hist(p3)
p2=hist(auxp)

plot(p2, col=rgb(1,0,0, 1/4), main="precios con Variables atipicas vs. sin variables atipicas", xlab="Precios" )
plot(p1, col=rgb(1,0,0, 1/4), add=T)

#Problema 3.b
media<-mean(p3) #Media
print(media)
mediana<-median(p3) #mediana
print(mediana)

#install.packages("modeest")
library(modeest)
moda<-mlv(p3, method = "mfv") #moda
print(moda)



#PUNTO 4

#4.1
print(table(Antioquia$producto))

var1<-Antioquia[Antioquia$producto=="BIODIESEL EXTRA",]$precio
var2<-Antioquia[Antioquia$producto=="GASOLINA CORRIENTE OXIGENADA",]$precio
var3<-Antioquia[Antioquia$producto=="GASOLINA EXTRA OXIGENADA",]$precio


plot(density(var1),
     main='Precio producto',xlab='precios' )
lines(density(var2), col="red")
lines(density(var3), col="green")
legend(x = "topright", legend = c("BIOD. EX.", "GAS. C.",'GAS. EX.'),
       fill = c("black", "red","green"))


#4.2
table1<-Antioquia[(Antioquia$producto=="BIODIESEL EXTRA")|(Antioquia$producto=="GASOLINA CORRIENTE OXIGENADA")
                        |(Antioquia$producto=="GASOLINA EXTRA OXIGENADA"),]
print(table1)

table2<-table1[(table1$precio>=8700)&(table1$precio<=9200),]
print(table2)
pie(table(table2$producto), main="Productos entre 8700 y 9200", col=c("slategray3","aquamarine2","turquoise4"))

#4.3
table1<-Antioquia[(Antioquia$producto=="BIODIESEL EXTRA")|(Antioquia$producto=="GASOLINA CORRIENTE OXIGENADA")
                  |(Antioquia$producto=="GASOLINA EXTRA OXIGENADA"),]
print(table1)

table2<-table1[(table1$precio>=11000),]
print(table2)
barplot(table(table2$producto), main="Productos mayor a 11000", col=c("slategray3","aquamarine2","turquoise4"))

#PUNTO 5
gas<-Antioquia[(Antioquia$producto=="GASOLINA EXTRA OXIGENADA")|(Antioquia$producto=="GASOLINA CORRIENTE OXIGENADA"),]
mediaGas<-mean(gas$precio)
pivote<-sd(gas$precio)
limite<-mediaGas+(2*pivote)
print(limite)
#5.1

conAtipicosTabla<-gas[gas$precio>=limite,]
sinAtipicosTabla<-gas[gas$precio>=limite,]
conAtipicosVector<-conAtipicosTabla$precio
resultado1<-(length(conAtipicosVector)*100)/nrow(gas)
print(resultado1)
string1<-"Porcentaje de precios elevados (con datos atipicos): "
print(paste(string1, resultado1))
bpAtipicos<-boxplot(conAtipicosVector, horizontal=TRUE, main="Tabla que no deben incluir", xlab="Precio", col=rainbow(1))  

aux2<-bpAtipicos$out
sinAtipicosTabla<-sinAtipicosTabla[! sinAtipicosTabla %in% aux2]
sinAtipicosVector<-sinAtipicosTabla$precio
resultado2<-(length(sinAtipicosVector)*100)/nrow(gas)
string2<-"Porcentaje de precios elevados (sin datos atipicos): "
print(paste(string2, resultado2))

#NOTA: Es el mismo porcentaje. Rectificar manualmente.

#5.2
productosElevados<-gas[gas$precio>=limite,]$producto
print(productosElevados)
pie(table(productosElevados), main="Productos elevados", col=c("slategray3","aquamarine2","turquoise4"))

#5.3
install.packages("lubridate")
library(lubridate)
gas$mes<-rep(0,length(gas$fecharegistro))
prueba<-gas$fecharegistro
fechas<-as.Date(prueba, "%m/%d/%Y")
print(fechas)
aux3<-month(fechas)
gas$mes<-month.abb[aux3]
print(gas$mes)
mesDesv<-gas[gas$precio>=limite,]$mes
print(mesDesv)
pie(table(mesDesv), main="Meses con productos elevados", col=c("slategray3","aquamarine2","turquoise4"))

#PUNTO 6
municip<-Antioquia[(Antioquia$producto=="BIODIESEL EXTRA")|(Antioquia$producto=="BIODIESEL CORRIENTE"),]

sort(municip)
print(municip)


muniPrecio1<-municip[(municip$municipionombre=="MEDELLIN"),]$precio
muniPrecio2<-municip[(municip$municipionombre=="ITAGUI"),]$precio
muniPrecio3<-municip[(municip$municipionombre=="BELLO"),]$precio

plot(density(muniPrecio1),
     main='Precio producto',xlab='Precios' )
lines(density(muniPrecio2), col="red")
lines(density(muniPrecio3), col="green")
legend(x = "topright", legend = c("Medellín", "Itagui",'Bello'),
       fill = c("black", "red","green"))


