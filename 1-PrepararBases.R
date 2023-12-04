#ENSANUT del año 2012
Antropometria<-cbind(Antropometria,clave=Antropometria$folio+Antropometria$intp)
Hogar_integrantes<-cbind(Hogar_integrantes,clave=Hogar_integrantes$folio+Hogar_integrantes$intp)
ENSANUT2012<-merge(Antropometria, Hogar_integrantes, by="clave", all.y=TRUE)
View(ENSANUT2012)
ENSANUT2012b<-cbind(ENSANUT2012a,IMC=ENSANUT2012a$peso/(ENSANUT2012a$talla/100)^2)
View(ENSANUT2012b)
#FALTA eliminar casillas donde hay NA en IMC
#FALTA eliminar duplicados
library(dplyr)
print(distinct(ENSANUT2012b))




CS_RESIDENTES<-cbind(CS_RESIDENTES,clave=CS_RESIDENTES$UPM+CS_RESIDENTES$VIV_SEL+CS_RESIDENTES$HOGAR+CS_RESIDENTES$NUMREN)
CN_ANTROPOMETRIA<-cbind(CN_ANTROPOMETRIA,clave=CN_ANTROPOMETRIA$UPM+CN_ANTROPOMETRIA$VIV_SEL+CN_ANTROPOMETRIA$HOGAR+CN_ANTROPOMETRIA$NUMREN)
ENSANUT2018<-merge(CS_RESIDENTES, CN_ANTROPOMETRIA, by="clave", all.x=TRUE)
#FALTA eliminar casillas donde hay NA en IMC
#FALTA eliminar duplicados
ENSANUT2018a <- na.omit(ENSANUT2018)
View(ENSANUT2018a)
ENSANUT2018b<-cbind(ENSANUT2018a,IMC=ENSANUT2018a$PESO1_1/(ENSANUT2018a$TALLA4_2/100)^2)
View(ENSANUT2018b)


ENSANUT2021<-merge(hogar_ensanut2021_w_14_12_2021.2, ensaantro21_entrega_w_17_12_2021.2, by="FOLIO_I", all.x=TRUE)
View(ENSANUT2021)
#seleccionar solo algunas columnas#
library(dplyr)
ENSANUT2021a <- select(ENSANUT2021, entidad.x,desc_ent.x,municipio.x,desc_mun.x,
                       h0302, h0303, an01_1, an04_1, h0327)
View(ENSANUT2021a)
ENSANUT2021b<-cbind(ENSANUT2021a,IMC=ENSANUT2021a$an01_1/(ENSANUT2021a$an04_1/100)^2)
View(ENSANUT2021b)
ENSANUT2021b <- na.omit(ENSANUT2021b)
View(ENSANUT2021b)
ENSANUT2021b%>% rename(sexo=h0302,edad=h0303, ingreso=h0327)
View(ENSANUT2021b)


ENSANUT2022<-merge(hogar_ensanut2022_w, ensaantro2022_entrega_w, by="FOLIO_I", all.x=TRUE)
View(ENSANUT2022)
#seleccionar solo algunas columnas#
library(dplyr)
ENSANUT2022a <- select(ENSANUT2022, entidad.x,desc_ent.x,municipio.x,desc_mun.x,
                       h0302, h0303, an01_1, an04_1, h0327)
View(ENSANUT2022a)
ENSANUT2022b<-cbind(ENSANUT2022a,IMC=ENSANUT2022a$an01_1/(ENSANUT2022a$an04_1/100)^2)
View(ENSANUT2022b)
ENSANUT2022b <- na.omit(ENSANUT2022b)
View(ENSANUT2022b)

