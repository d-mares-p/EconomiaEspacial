

import excel "/Users/dmares/Downloads/RegresionFinalA.xlsx", ///
sheet("bdatos") firstrow clear

gen id_ent = real(CVE)
sort id_ent

isid id_ent

reshape long Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud, ///
i(id_ent Estado CVE) j(year)

sort id_ent year

isid id_ent year
xtset id_ent year

count
xtdescribe

summarize Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud

* Confirmar tasas nuevas
list Estado year CrecimientoVA CrecimientoGastosalud ///
if id_ent==1, noobs


save "/Users/dmares/Downloads/panel_limpio.dta", replace



*--------------------------------------------------
* MATRIZ ESPACIAL QUEEN
*--------------------------------------------------

use "/Users/dmares/Downloads/wrn.dta", clear

gen id_ent = _n

spset id_ent

spmatrix clear

spmatrix fromdata Wqueen = X1-X32, normalize(none)

spmatrix dir

spmatrix save Wqueen using ///
"/Users/dmares/Downloads/Wqueen.stswm", replace

*--------------------------------------------------
* REGRESAR AL PANEL
*--------------------------------------------------

use "/Users/dmares/Downloads/panel_limpio.dta", clear

xtset id_ent year
spset id_ent

spmatrix clear
spmatrix use Wqueen using ///
"/Users/dmares/Downloads/Wqueen.stswm"

spmatrix matafromsp W id = Wqueen

mata:
rs = rowsum(W)
min(rs)
max(rs)
end

mata: id[1..32]

mata: st_matrix("Wqueen_xsmle", W)
matrix list Wqueen_xsmle


*prueba Hausman
xtreg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, fe
estimates store FE

xtreg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, re
estimates store RE

hausman FE RE, sigmamore

*Mundlak
bysort id_ent: egen mgini = mean(gini)
bysort id_ent: egen mpobreza = mean(pobreza)
bysort id_ent: egen mescolaridad = mean(escolaridad)
bysort id_ent: egen mIDS = mean(IDS)
bysort id_ent: egen mdensidad = mean(densidad)
bysort id_ent: egen mVA = mean(CrecimientoVA)
bysort id_ent: egen mGasto = mean(CrecimientoGastosalud)

xtreg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud ///
mgini mpobreza mescolaridad mIDS mdensidad mVA mGasto ///
i.year, re vce(cluster id_ent)

test mgini mpobreza mescolaridad mIDS mdensidad mVA mGasto


xtreg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, ///
re vce(cluster id_ent)

xtreg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, re

xttest0



*panel dos vías cluster sin espacio
* FE dos vías, benchmark principal
xtreg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, ///
fe vce(cluster id_ent)



*prueba wooldridge
xtserial Inci gini pobreza escolaridad IDS densidad CrecimientoVA CrecimientoGastosalud
*pesaran
xtreg Inci gini pobreza escolaridad IDS densidad CrecimientoVA CrecimientoGastosalud i.year, fe
xtcsd, pesaran abs

reg Inci gini pobreza escolaridad IDS densidad CrecimientoVA CrecimientoGastosalud i.year i.id_ent
estat hettest, rhs


*SAR dos vias cluster
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud, ///
fe type(both) model(sar) ///
wmat(Wqueen_xsmle) ///
vce(cluster id_ent)

estimates store SAR_FE2

matrix list e(b)	   //***para obtener valores aic y bic  

*SEM
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud, ///
fe type(both) model(sem) ///
emat(Wqueen_xsmle) ///
vce(cluster id_ent)

estimates store SEM_FE2

matrix list e(b)  //***para obtener valores aic y bic 
	   
*SAC
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud, ///
fe type(both) model(sac) ///
wmat(Wqueen_xsmle) emat(Wqueen_xsmle) ///
vce(cluster id_ent)

estimates store SAC_FE2

matrix list e(b)    ///***para obtener valores aic y bic 
	   
*SDM

xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud, ///
fe type(both) model(sdm) ///
wmat(Wqueen_xsmle) ///
vce(cluster id_ent)

estimates store SDM_FE2

matrix list e(b) //***para obtener valores aic y bic 


test [Wx]gini [Wx]pobreza [Wx]escolaridad [Wx]IDS [Wx]densidad ///
     [Wx]CrecimientoVA [Wx]CrecimientoGastosalud

estimates stats SAR_FE2 SEM_FE2 SAC_FE2 SDM_FE2

	   
*efectos SAR
xsmle Inci gini pobreza escolaridad IDS densidad CrecimientoVA CrecimientoGastosalud, fe type(both) model(sar) wmat(Wqueen_xsmle) vce(cluster id_ent) effects
	
*efectos SAC 
xsmle Inci gini pobreza escolaridad IDS densidad CrecimientoVA CrecimientoGastosalud, fe type(both) model(sac) wmat(Wqueen_xsmle) emat(Wqueen_xsmle) vce(cluster id_ent) effects

*efectos SDM
xsmle Inci gini pobreza escolaridad IDS densidad CrecimientoVA CrecimientoGastosalud, fe type(both) model(sdm) wmat(Wqueen_xsmle) vce(cluster id_ent) effects




*OLS básica
reg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud



*SAR con RE
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, ///
re model(sar) ///
wmat(Wqueen_xsmle) ///
vce(cluster id_ent)

*SEM con RE
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, ///
re model(sem) ///
emat(Wqueen_xsmle) ///
vce(cluster id_ent)


*SDM con RE
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, ///
re model(sdm) ///
wmat(Wqueen_xsmle) ///
vce(cluster id_ent)


xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, ///
re model(sdm) ///
wmat(Wqueen_xsmle) ///
durbin(gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud) ///
vce(cluster id_ent)

test ([Wx]gini = 0) ///
     ([Wx]pobreza = 0) ///
     ([Wx]escolaridad = 0) ///
     ([Wx]IDS = 0) ///
     ([Wx]densidad = 0) ///
     ([Wx]CrecimientoVA = 0) ///
     ([Wx]CrecimientoGastosalud = 0)
testparm [Wx]:



tab year, gen(yr)
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud ///
yr2 yr3 yr4 yr5 yr6 yr7 yr8 yr9 yr10, ///
re model(sdm) ///
wmat(Wqueen_xsmle) ///
durbin(gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud) ///
vce(cluster id_ent) effects



*SAC con RE?  hay una variante porque no hay SAC con RE
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud, ///
fe type(both) model(sac) ///
wmat(Wqueen_xsmle) emat(Wqueen_xsmle) ///
vce(cluster id_ent)


*AIC y BIC
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, ///
re model(sar) wmat(Wqueen_xsmle) ///
vce(cluster id_ent)
estimates store SAR_RE
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, ///
re model(sem) emat(Wqueen_xsmle) ///
vce(cluster id_ent)
estimates store SEM_RE
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud ///
yr2 yr3 yr4 yr5 yr6 yr7 yr8 yr9 yr10, ///
re model(sdm) wmat(Wqueen_xsmle) ///
durbin(gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud) ///
vce(cluster id_ent)
estimates store SDM_RE

estimates stats SAR_RE SEM_RE SDM_RE


*autocorrelación serial
xtserial Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud

*dependencia espacial
xtreg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, re
xtcsd, pesaran abs

*heterocedasticidad
xtreg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, re

xtreg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, fe
xttest3

*Breush Pagan
reg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year

estat hettest, rhs



xsmle Inci gini pobreza escolaridad IDS densidad ///
    CrecimientoVA CrecimientoGastosalud, ///
    fe type(both) model(sdm) ///
    wmat(Wqueen_xsmle) ///
    vce(cluster id_ent) effects
	
	
*Criterios LM, lag y err
preserve

collapse (mean) Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud, by(id_ent)
reg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud
spmatrix dir
restore
	

xtreg Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud i.year, ///
fe vce(cluster id_ent)

*impactos
xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud, ///
fe type(both) model(sar) ///
wmat(Wqueen_xsmle) ///
vce(cluster id_ent) effects

xsmle Inci gini pobreza escolaridad IDS densidad ///
CrecimientoVA CrecimientoGastosalud, ///
fe type(both) model(sdm) ///
wmat(Wqueen_xsmle) ///
vce(cluster id_ent) effects



*spillovers:
mata:

W = st_matrix("Wqueen_xsmle")

n = rows(W)
I = I(n)

rho   = -0.0475481
beta  =  0.3797834
theta = -1.128407

A = luinv(I - rho*W)

S = A * (beta*I + theta*W)

directo   = diagonal(S)
total_rec = rowsum(S)
ind_rec   = total_rec :- directo

impactos = (directo, ind_rec, total_rec)

st_matrix("impactos_gasto", impactos)

mean(directo)
mean(ind_rec)
mean(total_rec)

end

matrix colnames impactos_gasto = Directo Indirecto_Recibido Total
matrix list impactos_gasto
spmatrix dir
spmatrix summarize Wqueen_xsmle

clear
set obs 32

gen id_ent = _n

svmat double impactos_gasto, names(col)

rename Directo Directo_gasto
rename Indirecto_Recibido Spillover_recibido_gasto
rename Total Total_gasto



*impactos
clear
set obs 32
gen id_ent = _n

svmat double impactos_gasto, names(imp)

describe

rename imp1 Directo_gasto
rename imp2 Spillover_recibido_gasto
rename imp3 Total_gasto

list id_ent Directo_gasto Spillover_recibido_gasto Total_gasto, sep(0)
matrix list Wqueen_xsmle
local rn : rownames Wqueen_xsmle
display "`rn'"
W = st_matrix("Wqueen_xsmle")
use panel_limpio.dta, clear
keep id_ent Estado
duplicates drop
sort id_ent
list id_ent Estado, sep(0)

use panel_limpio.dta, clear
keep id_ent Estado
duplicates drop
sort id_ent
save estados_nombres.dta, replace


clear
set obs 32

gen id_ent = _n

svmat double impactos_gasto, names(imp)

rename imp1 Directo_gasto
rename imp2 Spillover_recibido_gasto
rename imp3 Total_gasto
merge 1:1 id_ent using estados_nombres.dta
drop _merge

sort id_ent
*spillovers recibidos
list id_ent Estado Directo_gasto ///
Spillover_recibido_gasto Total_gasto, sep(0)

export excel using "spillovers_gasto_SDM.xlsx", ///
firstrow(variables) replace

*emitidos
mata:

W = st_matrix("Wqueen_xsmle")

n = rows(W)
I = I(n)

rho   = -0.0475481
beta  =  0.3797834
theta = -1.128407

A = luinv(I - rho*W)
S = A * (beta*I + theta*W)

directo = diagonal(S)

/* Spillovers recibidos: suma por fila, sin diagonal */
recibido = rowsum(S) :- directo

/* Spillovers emitidos: suma por columna, sin diagonal */
emitido = colsum(S)' :- directo

/* Total asociado a cada estado como emisor */
total_emitido = emitido :+ directo

impactos_emision = (directo, recibido, emitido, total_emitido)

st_matrix("impactos_emision", impactos_emision)

mean(recibido)
mean(emitido)

end

clear
set obs 32
gen id_ent = _n

svmat double impactos_emision, names(imp)

rename imp1 Directo_gasto
rename imp2 Spillover_recibido_gasto
rename imp3 Spillover_emitido_gasto
rename imp4 Total_emitido_gasto

merge 1:1 id_ent using estados_nombres.dta
drop _merge

sort id_ent
list id_ent Estado ///
Directo_gasto ///
Spillover_recibido_gasto ///
Spillover_emitido_gasto ///
Total_emitido_gasto, sep(0)

export excel using "spillovers_gastoemitidos_SDM.xlsx", ///
firstrow(variables) replace
