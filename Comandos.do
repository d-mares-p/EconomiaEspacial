**Estimación con panel espacial datos para obesidad**
*verificar que la matriz de pesos espaciales esté condicionada para trabajarse en dta**
**Instalar los siguientes paquetes**

ssc install spwmatrix
ssc install sppack
ssc install xsmle


net search spwmatrix
net install spwmatrix, from(http://fmwww.bc.edu/repec/bocode/s/spwmatrix.ado)

**activar la matriz creada en dta**
use "/Users/dmares/Downloads/wrn.dta", clear

. spmat dta wrn X*, normalize(row)

. spmat summarize wrn, links

  **importar la base que está en excel y crear el identificador
import excel "/Users/dmares/Downloads/RegresionPanel-C.xlsx", sheet("APanel") firstrow clear

#no procesa nombre de las entidades por lo que renombramos
egen ident=group(ent)

*Declarar el identicador individual y del tiempo*
encode ent, gen(id_ent)
xtset id_ent t




                           *Parte I, regresiones directas*
*prueba de Hausman*
xtreg Inciden ginesp_i, fe
estimates store FE1

xtreg Inciden ginesp_i, re
estimates store RE1

hausman FE1 RE1
	   
xtreg Inciden giniesp_p, fe
estimates store FE2

xtreg Inciden giniesp_p, re
estimates store RE2

hausman FE2 RE2
	   
	   
xtreg Inciden dessalud_i, fe
estimates store FE3

xtreg Inciden dessalud_i, re
estimates store RE3

hausman FE3 RE3
	
xtreg Inciden dessalud_p, fe
estimates store FE4

xtreg Inciden dessalud_p, re
estimates store RE4

hausman FE4 RE4

xtreg Inciden GiniT_i, fe
estimates store FE5

xtreg Inciden GiniT_i, re
estimates store RE5

hausman FE5 RE5

**reste codigo siguiente se usa porque prueba de Hausman de estas variables salió con Chi negativa **
xtreg Inciden GiniT_i, fe
est store fe_model

xtreg Inciden GiniT_i, re
est store re_model

suest fe_model re_model

hausman fe_model re_model, sigmamore




xtreg Inciden GiniT_p, fe
estimates store FE6

xtreg Inciden GiniT_p, re
estimates store RE6

hausman FE6 RE6


*Regresiones simples*

xtreg Inciden GiniT_i, fe

eststo m1: xtreg Inciden ginesp_i, fe
eststo m2: xtreg Inciden giniesp_p, fe
eststo m3: xtreg Inciden dessalud_i, fe
eststo m4: xtreg Inciden dessalud_p, fe
eststo m5: xtreg Inciden GiniT_i, fe
eststo m6: xtreg Inciden GiniT_p, fe

* Exportar tabla en LaTeX o Word (requiere instalar estout)
ssc install estout
esttab m1 m2 m3 m4 m5 m6 using resultados.rtf, replace se label title(Regresiones tipo panel con efectos fijos)

*pruebas de los modelos simples*
*1. prueba de autocorrelacion de errores*
*2. prueba de heterocedasticidad grupal*

net install xtserial, from("http://www.stata.com/users/ddrukker")
xtserial Inciden ginesp_i
xtserial Inciden giniesp_p
xtserial Inciden dessalud_i
xtserial Inciden dessalud_p
xtserial Inciden GiniT_i
xtserial Inciden GiniT_p

ssc install xttest3

xtreg Inciden ginesp_i, fe
xttest3
xtreg Inciden giniesp_p, fe
xttest3
xtreg Inciden dessalud_i, fe
xttest3
xtreg Inciden dessalud_p, fe
xttest3
xtreg Inciden GiniT_i, fe
xttest3
xtreg Inciden GiniT_p, fe
xttest3

*cuando hay heterocedasticidad y autocorrelacion, se requiere errores estandar robustos*
eststo M1:xtreg Inciden ginesp_i, fe robust
eststo M2:xtreg Inciden giniesp_p, fe robust
eststo M3:xtreg Inciden dessalud_i, fe robust
eststo M4:xtreg Inciden dessalud_p, fe robust
eststo M5:xtreg Inciden GiniT_i, fe robust
eststo M6:xtreg Inciden GiniT_p, fe  robust

ssc install estout
esttab M1 M2 M3 M4 M5 M6 using resultados.rtf, replace se label title(Regresiones tipo panel con efectos fijos robustos)


                                *Parte II, regresiones espaciales*
								
*modelo sar con efectos fijos espaciales*
eststo model_base: xsmle Inciden ginesp_i giniesp_p dessalud_i dessalud_p GiniT_i GiniT_p , ///
    wmat(wrn) model(sar) fe model(sar) fe vce(cluster id_ent)
	
eststo model_ext:xsmle Inciden ginesp_i giniesp_p dessalud_i dessalud_p GiniT_i GiniT_p escolh escolm gtosal Verdulerias, ///
    wmat(wrn) model(sar) fe vce(cluster id_ent)

esttab model_base model_ext using resultadosSAR.rtf, replace se label
							
*modelo sem con efectos fijos espaciales*
eststo MSEM: xsmle Inciden ginesp_i giniesp_p dessalud_i dessalud_p GiniT_i GiniT_p, ///
    emat(wrn) model(sem) fe  vce(cluster id_ent) nolog
	
eststo MSEM1: xsmle  Inciden ginesp_i giniesp_p dessalud_i dessalud_p GiniT_i GiniT_p escolh escolm gtosal Verdulerias, ///
    emat(wrn) model(sem) fe  vce(cluster id_ent) nolog

esttab MSEM MSEM1 using resultadosSEM.rtf, replace se label
							
*modelo sdm con efectos fijos espaciales*
eststo MSEM2:xsmle Inciden ginesp_i giniesp_p dessalud_i dessalud_p GiniT_i GiniT_p, ///
    wmat(wrn) model(sdm) fe vce(cluster id_ent)
eststo MSEM3:xsmle Inciden ginesp_i giniesp_p dessalud_i dessalud_p GiniT_i GiniT_p ///
    escolh escolm gtosal Verdulerias, ///
    wmat(wrn) model(sdm) fe vce(cluster id_ent)

esttab MSEM2 MSEM3 using resultadosSDM.rtf, replace se label

*efectos directos*
xsmle Inciden ginesp_i giniesp_p dessalud_i dessalud_p GiniT_i GiniT_p , ///
    wmat(wrn) model(sdm) fe vce(cluster id_ent) ///
    effects impacts

xsmle Inciden ginesp_i giniesp_p dessalud_i dessalud_p GiniT_i GiniT_p escolh escolm gtosal Verdulerias, ///
    wmat(wrn) model(sdm) fe vce(cluster id_ent) ///
    effects impacts




                           