
#datos precio OPEP en los ultimos 12 meses
Ultimos12Meses <- c('1','2','3','4','5','6','7','8','9','10','11','12');
EurosOPEP <- c(52.80,55.09,59.67,62.19,59.71,62.78,70.74,70.39,65.79,75.48,83.06,103.19);
DolaresOPEP <- c(63.25,66.91,71.89,73.52,70.33,73.89,82.07,80.34,74.37,85.40,94.21,117.04);

#grafico Opep Separados
plot(Ultimos12Meses,EurosOPEP,type="l",col="red",main="Euros OPEP",lwd=3,ylab='Euros')
plot(Ultimos12Meses,DolaresOPEP,type="l",col="red",main="Dólares OPEP",lwd=3,ylab='Dólares')

#grafico Opep juntos
plot(Ultimos12Meses,EurosOPEP,type="l",col="red",main="Gráfico Euros y Dólares OPEP",lwd=3,ylab='');
lines(Ultimos12Meses,DolaresOPEP,col="green",lwd=3);
legend("bottomright",col=c("red","green"),legend =c("Euros","Dólares"), lwd=3, bty = "n");

#Estadistica Descriptiva OPEP Euros
media<-mean(EurosOPEP)
rango <-range(EurosOPEP)
mediana <- median(EurosOPEP)
varianza <- var(EurosOPEP)
DesviacionTipica <- sd(EurosOPEP)
CoefVariacion1 <- sd(EurosOPEP)/mean(EurosOPEP)
Asimetria <-skewness(EurosOPEP)
Curtosis <-kurtosi(EurosOPEP)
boxplot(EurosOPEP,main="BoxPlot Euros OPEP",ylab='Euros')

#datos precio Brent en los ultimos 12 meses
EurosBrent <- c(54.10,56.42,60.73,63.58,60.10,63.29,72.01,71.01,65.61,76.46,85.64,105.00);
DolaresBrent <- c(64.81,66.53,73.16,75.17,70.75,74.49,83.54,81.05,74.17,86.51,97.13,119.09);
#grafico Brent Separados

plot(Ultimos12Meses,EurosBrent,type="l",col="red",main="Euros Brent",lwd=3,ylab='Euros')
plot(Ultimos12Meses,DolaresBrent,type="l",col="red",main="Dólares Brent",lwd=3,ylab='Dólares')

#grafico Brent Juntos
plot(Ultimos12Meses,EurosBrent,type="l",col="red",main="Gráfico Euros y Dólares Brent",lwd=3,ylab='');
lines(Ultimos12Meses,DolaresBrent,col="green",lwd=3);
legend("bottomright",col=c("red","green"),legend =c("Euros","Dólares"), lwd=3, bty = "n");


#Estadistica Descriptiva BRENT Euros
media2<-mean(EurosBrent)
mediana2<- median(EurosBrent)
rango2 <-range(EurosBrent)
varianza2 <- var(EurosBrent)
DesviacionTipica2 <- sd(EurosBrent)
CoefVariacion2 <- sd(EurosBrent)/mean(EurosBrent)
Asimetria2 <-skewness(EurosBrent)
Curtosis2 <-kurtosi(EurosBrent)
boxplot(EurosBrent,main="BoxPlot Euros Brent",ylab='Euros')


#grafico Anual OPEP 1960-2020

Anios <- c(1960,1962,1964,1966,1968,1970,1972,1974,1976,1978,1980,1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020);
precioAnioO <- c(1.63,1.57,1.45,1.36,1.32,1.21,1.82,11,11.7,12.89,35.52,32.28,28.2,13.5,14.6,22.2,18.46,15.5,20.29,12.2,27.6,24.36,36.05,61,95.1,77.3,110.5,96.3,40.76,70,41);

barplot(precioAnioO,col="yellow",main="Grafico Anual OPEP 1960-2020 (De 2 en 2)",lwd=3,ylab='Dólares',names.arg=Anios,space =0.4);

#grafico Anual Brent 2000-2021

Anios2 <- c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021);
precioAnioB <-c (28.4,24.45,25.01,28.83,38.1,54.38,65.14,72.52,96.99,61.51,79.47,111.26,111.63,108.56,98.97,52.32,43.67,54.25,71.34,64.3,41.96,70.68);
barplot(precioAnioB,main='Grafico Anual Brent 2000-2021 ',ylab='Dólares',names.arg =  Anios2,space =0.4)

#grafico TOP Exportadores

paises <- c ('EEUU','Rusia','ASaudi','Canada','Irak','China','Brasil','Emiratos');
barrilespordia <- c(10.2,9.7,9.3,4.3,4.2,3.9,2.9,2.7);
barplot(barrilespordia,col='pink',main="Ranking mundial de los principales países productores de petróleo 2021",lwd=3,ylab='Millones de Barriles Diarios',names.arg=paises,space =1.2)









