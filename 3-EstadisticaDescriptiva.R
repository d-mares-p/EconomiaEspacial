#
##########################################


plot(ENSANUT2012c$entidad.x,ENSANUT2012c$IMC)
plot(ENSANUT2018c$ENT.x,ENSANUT2018c$IMC)
plot(ENSANUT2021c$EDO,ENSANUT2021c$IMC)
plot(ENSANUT2022c$EDO,ENSANUT2022c$IMC)

#2012
boxplot(ENSANUT2012c$IMC ~ ENSANUT2012c$edad_i, data=ENSANUT2012c, col="green",  
        ylab="IMC", xlab="Edad")

boxplot(ENSANUT2012c$IMC ~ ENSANUT2012c$entidad.x, data=ENSANUT2012c, col="red",  
        ylab="IMC", xlab="Estado")

boxplot(ENSANUT2012c$IMC ~ ENSANUT2012c$sexo.x, data=ENSANUT2012c, col="blue",  
        ylab="IMC", xlab="Sexo")


library(ggplot2)
ggplot(ENSANUT2012c, aes(ENSANUT2012c$IMC, ENSANUT2012c$sexo.x))

plot(IMC~ingreso, data=ENSANUT2012c,pch=16)
abline(lm(IMC~ingreso, data=ENSANUT2012c),col="red")
modelo2018 <-lm(IMC~ingreso, data=ENSANUT2012c)
summary(modelo2018)

qplot(IMC, ingreso, data=ENSANUT2012c, geom=c("point", "line"))
qplot(IMC, ingreso, data=ENSANUT2012c, geom = c("point", "smooth"))
qplot(IMC, ingreso, data=ENSANUT2012c, 
      geom=c("dotplot"), 
      stackdir = "center", binaxis = "y") 




#2018
boxplot(ENSANUT2018c$IMC ~ ENSANUT2018c$EDAD.x, data=ENSANUT2018c, col="green",  
        ylab="IMC", xlab="Edad")

boxplot(IMC ~ ENT.x, data=ENSANUT2018c, col="red",  
        ylab="IMC", xlab="Estado")

boxplot(IMC ~ SEXO.x, data=ENSANUT2018c, col="blue",  
        ylab="IMC", xlab="Sexo")


#2021
boxplot(IMC ~ h0303, data=ENSANUT2021c, col="green",  
        ylab="IMC", xlab="Edad")

boxplot(IMC ~ EDO, data=ENSANUT2021c, col="red",  
        ylab="IMC", xlab="Estado")

boxplot(IMC ~ h0302, data=ENSANUT2021c, col="blue",  
        ylab="IMC", xlab="Sexo")


#2022
boxplot(IMC ~ h0303, data=ENSANUT2022c, col="green",  
        ylab="IMC", xlab="Edad")

boxplot(IMC ~ EDO, data=ENSANUT2022c, col="red",  
        ylab="IMC", xlab="Estado")

boxplot(IMC ~ h0302, data=ENSANUT2022c, col="blue",  
        ylab="IMC", xlab="Sexo")




# para el 2012
#submuestra IMC

ENSANUT2012c <- select(ENSANUT2018, entidad.x, desc_ent,munici.x,desc_mun, 
                       sexo.x, edad_i, peso, talla, h226a, h226b, h219)

