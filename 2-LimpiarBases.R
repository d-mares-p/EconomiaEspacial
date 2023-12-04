#Preparar bases de datos

#####CHECAR SI TAMBIEN ADJUNTO ESCOLARIDAD
#####CHECAR SI TAMBIEN QUITO LOS QUE TIENEN IMC MAYOR A 100
#####checar que al ingreso le quite los que tienen valores de 88888, 99999


#2012
View(ENSANUT2012)
library(dplyr)

#Variables básicas: ingreso, edad, situación conyugal, IMC
ENSANUT2012a <- select(ENSANUT2012, entidad.x, desc_ent,munici.x,desc_mun, 
                       sexo.x, edad_i, peso, talla, h226a, h226b, h219)
View(ENSANUT2012a)
ENSANUT2012b<-cbind(ENSANUT2012a,IMC=ENSANUT2012a$peso/(ENSANUT2012a$talla/100)^2)
View(ENSANUT2012b)
ENSANUT2012b <- na.omit(ENSANUT2012b)
View(ENSANUT2012b)

ENSANUT2012b$Peri_ingr <- ENSANUT2012b$h226a 

library(stringr)
ENSANUT2012b$Peri_ingr = str_replace(ENSANUT2012b$Peri_ingr, "1" , "28")
ENSANUT2012b$Peri_ingr = str_replace(ENSANUT2012b$Peri_ingr, "4" , "1")
ENSANUT2012b$Peri_ingr = str_replace(ENSANUT2012b$Peri_ingr, "2" , "4")
ENSANUT2012b$Peri_ingr = str_replace(ENSANUT2012b$Peri_ingr, "3" , "2")
ENSANUT2012b$Peri_ingr = str_replace(ENSANUT2012b$Peri_ingr, "5" , "0.083333333")

ENSANUT2012b$sexo.x = str_replace(ENSANUT2012b$sexo.x, "1" , "hombre")
ENSANUT2012b$sexo.x = str_replace(ENSANUT2012b$sexo.x, "2" , "mujer")

ENSANUT2012b$h219 = str_replace(ENSANUT2012b$h219, "1" , "union_libre")
ENSANUT2012b$h219 = str_replace(ENSANUT2012b$h219, "2" , "separado")
ENSANUT2012b$h219 = str_replace(ENSANUT2012b$h219, "3" , "divorciado")
ENSANUT2012b$h219 = str_replace(ENSANUT2012b$h219, "4" , "viudo")
ENSANUT2012b$h219 = str_replace(ENSANUT2012b$h219, "5" , "casado")
ENSANUT2012b$h219 = str_replace(ENSANUT2012b$h219, "6" , "soltero")


#se cambian los valores de 999999 (no se acuerdan de su ingreso) por un estandar de $2800 pesos)
ENSANUT2012b$h226b <- replace(ENSANUT2012b$h226b, ENSANUT2012b$h226b==999999,2800) 


View(ENSANUT2012b)
ENSANUT2012b$Peri_ingr<- as.numeric(ENSANUT2012b$Peri_ingr)

#se agrega columna de ingreso
ENSANUT2012c<-cbind(ENSANUT2012b,ingreso=ENSANUT2012b$Peri_ingr*ENSANUT2012b$h226b)
View(ENSANUT2012c)


#Esta es mi base de datos trabajada:
summary(ENSANUT2012c)

########verificar si hay repetidos!!!!!!!!!!!!! y quitarlos

library(janitor)
ENSANUT2012c %>% 
  janitor::get_dupes()


ENSANUT2012c %>% 
  distinct( .keep_all = TRUE) 



ENSANUT2012c <- ENSANUT2012c %>% 
  distinct()

View(ENSANUT2012c)





#2018
View(ENSANUT2018)
library(dplyr)

#Variables básicas: ingreso, edad, situación conyugal, IMC. No  cuenta con variable municipio
ENSANUT2018a <- select(ENSANUT2018, ENT.x,  
                       SEXO.x, EDAD.x, PESO1_1, TALLA4_1, P3_26_1, P3_26_2, P3_19)
View(ENSANUT2018a)


ENSANUT2018b<-cbind(ENSANUT2018a,IMC=ENSANUT2018a$PESO1_1/(ENSANUT2018a$TALLA4_1/100)^2)
View(ENSANUT2018b)
ENSANUT2018b <- na.omit(ENSANUT2018b)
View(ENSANUT2018b)

ENSANUT2018b$Peri_ingr <- ENSANUT2018b$P3_26_1 
summary(ENSANUT2018b)

library(stringr)

ENSANUT2018b$Peri_ingr = str_replace(ENSANUT2018b$Peri_ingr, "3" , "4")
ENSANUT2018b$Peri_ingr = str_replace(ENSANUT2018b$Peri_ingr, "5" , "28")


ENSANUT2018b$SEXO.x = str_replace(ENSANUT2018b$SEXO.x, "1" , "hombre")
ENSANUT2018b$SEXO.x = str_replace(ENSANUT2018b$SEXO.x, "2" , "mujer")

ENSANUT2018b$P3_19 = str_replace(ENSANUT2018b$P3_19, "1" , "union_libre")
ENSANUT2018b$P3_19 = str_replace(ENSANUT2018b$P3_19, "2" , "separado_union_libre")
ENSANUT2018b$P3_19 = str_replace(ENSANUT2018b$P3_19, "3" , "separado_matrimonio")
ENSANUT2018b$P3_19 = str_replace(ENSANUT2018b$P3_19, "4" , "divorciado")
ENSANUT2018b$P3_19 = str_replace(ENSANUT2018b$P3_19, "5" , "viudo")
ENSANUT2018b$P3_19 = str_replace(ENSANUT2018b$P3_19, "6" , "casado")
ENSANUT2018b$P3_19 = str_replace(ENSANUT2018b$P3_19, "7" , "soltero")

#se cambia peridicidad de los que no se acuerdan 
ENSANUT2018b$Peri_ingr = str_replace(ENSANUT2018b$Peri_ingr, "9" , "0.083333333")

#se cambian los valores de 999999 (no se acuerdan de su ingreso) por un estandar de $3000 pesos)
ENSANUT2018b$P3_26_2 <- replace(ENSANUT2018b$P3_26_2, ENSANUT2018b$P3_26_2==999999,3000) 


View(ENSANUT2018b)
ENSANUT2018b$Peri_ingr<- as.numeric(ENSANUT2018b$Peri_ingr)

#se agrega columna de ingreso
ENSANUT2018c<-cbind(ENSANUT2018b,ingreso=ENSANUT2018b$Peri_ingr*ENSANUT2018b$P3_26_2)
View(ENSANUT2018c)


#Esta es mi base de datos trabajada:
summary(ENSANUT2018c)


#agregarle columna con nombre de la entidad
########verificar si hay repetidos!!!!!!!!!!!!! y quitarlos

ENSANUT2018c <- ENSANUT2018c %>% 
  distinct()
ENSANUT2018c %>% 
  janitor::get_dupes() 

View(ENSANUT2018c)




#2021
View(ENSANUT2021)
library(dplyr)

#Variables básicas: ingreso, edad, situación conyugal, IMC.  h0302=sexo, h0303=edad, an01_1=peso
# an04_1=talla, h0327=rango ingreso, no se pudo encontrar h0319=edo civil porque está en cuestionario de individuos
ENSANUT2021a <- select(ENSANUT2021, desc_ent.x, municipio.x, desc_mun.x,  
                       h0302, h0303, an01_1, an04_1, h0327)
View(ENSANUT2021a)


ENSANUT2021b<-cbind(ENSANUT2021a,IMC=ENSANUT2021a$an01_1/(ENSANUT2021a$an04_1/100)^2)
View(ENSANUT2021b)
ENSANUT2021b <- na.omit(ENSANUT2021b)
View(ENSANUT2021b)

ENSANUT2021b$ingreso <- ENSANUT2021b$h0327 
summary(ENSANUT2021b)


ENSANUT2021b$ingreso[ENSANUT2021b$ingreso == 1] <- 3000 
ENSANUT2021b$ingreso[ENSANUT2021b$ingreso == 2] <- 8000 
ENSANUT2021b$ingreso[ENSANUT2021b$ingreso == 3] <- 12000 
ENSANUT2021b$ingreso[ENSANUT2021b$ingreso == 4] <- 17500 
ENSANUT2021b$ingreso[ENSANUT2021b$ingreso == 5] <- 30000
ENSANUT2021b$ingreso[ENSANUT2021b$ingreso == 6] <- 0
ENSANUT2021b$ingreso[ENSANUT2021b$ingreso == 8] <- 20000 
ENSANUT2021b$ingreso[ENSANUT2021b$ingreso == 9] <- 3000 


ENSANUT2021b$h0302 = str_replace(ENSANUT2021b$h0302, "1" , "hombre")
ENSANUT2021b$h0302 = str_replace(ENSANUT2021b$h0302, "2" , "mujer")

#ENSANUT2021b$h0319 = str_replace(ENSANUT2021b$h0319, "1" , "union_libre")
#ENSANUT2021b$h0319 = str_replace(ENSANUT2021b$h0319, "2" , "separado_union_libre")
#ENSANUT2021b$h0319 = str_replace(ENSANUT2021b$h0319, "3" , "separado_matrimonio")
#ENSANUT2021b$h0319 = str_replace(ENSANUT2021b$h0319, "4" , "divorciado")
#ENSANUT2021b$h0319 = str_replace(ENSANUT2021b$h0319, "5" , "viudo")
#ENSANUT2021b$h0319 = str_replace(ENSANUT2021b$h0319, "6" , "casado")
#ENSANUT2021b$h0319 = str_replace(ENSANUT2021b$h0319, "7" , "soltero")


ENSANUT2021c <- ENSANUT2021b 
ENSANUT2021c <- ENSANUT2021c[,-10]
View(ENSANUT2021c)


ENSANUT2021c$EDO <- str_sub(ENSANUT2021c$desc_ent.x, 1, 2)

#Esta es mi base de datos trabajada:
summary(ENSANUT2021c)



#agregarle columna con nombre de la entidad
########verificar si hay repetidos!!!!!!!!!!!!! y quitarlos

ENSANUT2021c <- ENSANUT2021c %>% 
  distinct()

View(ENSANUT2021c)
ENSANUT2021c %>% 
  janitor::get_dupes() 



#2022
View(ENSANUT2022)
library(dplyr)

#Variables básicas: ingreso, edad, situación conyugal, IMC.  h0302=sexo, h0303=edad, an01_1=peso
# an04_1=talla, h0327=rango ingreso, no se pudo encontrar h0319=edo civil porque está en cuestionario de individuos
ENSANUT2022a <- select(ENSANUT2022, entidad.x, desc_ent.x, municipio.x, desc_mun.x,  
                       h0302, h0303, an01_1, an04_1, h0327)
View(ENSANUT2022a)


ENSANUT2022b<-cbind(ENSANUT2022a,IMC=ENSANUT2022a$an01_1/(ENSANUT2022a$an04_1/100)^2)
View(ENSANUT2022b)
ENSANUT2022b <- na.omit(ENSANUT2022b)
View(ENSANUT2022b)

ENSANUT2022b$ingreso <- ENSANUT2022b$h0327 
summary(ENSANUT2022b)


ENSANUT2022b$ingreso[ENSANUT2022b$ingreso == 1] <- 3000 
ENSANUT2022b$ingreso[ENSANUT2022b$ingreso == 2] <- 8000 
ENSANUT2022b$ingreso[ENSANUT2022b$ingreso == 3] <- 12000 
ENSANUT2022b$ingreso[ENSANUT2022b$ingreso == 4] <- 17500 
ENSANUT2022b$ingreso[ENSANUT2022b$ingreso == 5] <- 30000
ENSANUT2022b$ingreso[ENSANUT2022b$ingreso == 6] <- 0
ENSANUT2022b$ingreso[ENSANUT2022b$ingreso == 8] <- 20000 
ENSANUT2022b$ingreso[ENSANUT2022b$ingreso == 9] <- 3000 


ENSANUT2022b$h0302 = str_replace(ENSANUT2022b$h0302, "1" , "hombre")
ENSANUT2022b$h0302 = str_replace(ENSANUT2022b$h0302, "2" , "mujer")

#ENSANUT2022b$h0319 = str_replace(ENSANUT2022b$h0319, "1" , "union_libre")
#ENSANUT2022b$h0319 = str_replace(ENSANUT2022b$h0319, "2" , "separado_union_libre")
#ENSANUT2022b$h0319 = str_replace(ENSANUT2022b$h0319, "3" , "separado_matrimonio")
#ENSANUT2022b$h0319 = str_replace(ENSANUT2022b$h0319, "4" , "divorciado")
#ENSANUT2022b$h0319 = str_replace(ENSANUT2022b$h0319, "5" , "viudo")
#ENSANUT2022b$h0319 = str_replace(ENSANUT2022b$h0319, "6" , "casado")
#ENSANUT2022b$h0319 = str_replace(ENSANUT2022b$h0319, "7" , "soltero")


ENSANUT2022c <- ENSANUT2022b 


View(ENSANUT2022c)

ENSANUT2022c$EDO <- str_sub(ENSANUT2022c$desc_ent.x, 1, 2)


#Esta es mi base de datos trabajada:
summary(ENSANUT2022c)



#agregarle columna con nombre de la entidad
########verificar si hay repetidos!!!!!!!!!!!!! y quitarlos

ENSANUT2022c <- ENSANUT2022c %>% 
  distinct()

ENSANUT2022c %>% 
  janitor::get_dupes() 

View(ENSANUT2022c)



###Limpiar base nuevamente
##a) quitar casos de IMC mayor a 100
##b) quitar casos con ingresos de 88888, 9999, etc.



#2012


#quitar duplicados
library(dplyr)

Base2012 <- ENSANUT2012c %>%
  distinct(desc_mun, sexo.x, IMC, .keep_all = TRUE)
View(Base2012)

#modificar ingreso a 7500
Base2012$h226b[Base2012$h226b == 899999] <- 7500 
Base2012$h226b[Base2012$h226b == 99999] <- 7500 
Base2012$h226b[Base2012$h226b == 9999] <- 7500 
Base2012$h226b[Base2012$h226b == 25000] <- 7500 


Base2012$ingreso <- Base2012$Peri_ingr*Base2012$h226b

View(Base2012)


#2018


#quitar duplicados
library(dplyr)

Base2012 <- ENSANUT2012c %>%
  distinct(desc_mun, sexo.x, IMC, .keep_all = TRUE)
View(Base2012)

#modificar ingreso a 7500
Base2012$h226b[Base2012$h226b == 899999] <- 7500 
Base2012$h226b[Base2012$h226b == 99999] <- 7500 
Base2012$h226b[Base2012$h226b == 9999] <- 7500 
Base2012$h226b[Base2012$h226b == 25000] <- 7500 


Base2012$ingreso <- Base2012$Peri_ingr*Base2012$h226b

View(Base2012)