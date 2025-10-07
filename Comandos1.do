**Estimación con panel espacial datos para obesidad**
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
   
*no procesa nombre de las entidades por lo que renombramos
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

xtreg Inciden ginesp_i i.t, re vce(cluster id_ent)
xttest0   // LM de Breusch–Pagan tras xtreg, re

*validar Efectos aleatorios
bys id_ent: egen mgini = mean(ginesp_i)
xtreg Inciden ginesp_i mgini i.t, re vce(cluster id_ent)
test mgini   // H0: mgini = 0  (si no se rechaza, apoya RE)




	   
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



*prueba de CRE porque tenemos muchos modelos básicos donde Hausman salió para RE
*medias por cada estado
local vars ginesp_i ginesp_p giniesp_p dessalud_i dessalud_p GiniT_i GiniT_p

foreach v of local vars {
    capture confirm var `v'
    if !_rc by id_ent: egen m_`v' = mean(`v')
}

* Lista "limpia" con solo las que existen en tu base
local L
foreach v of local vars {
    capture confirm var `v'
    if !_rc local L `L' `v'
}

foreach x of local L {
    di as res "=== CRE: Inciden ~ `x'  (RE con Mundlak) ==="
    xtreg Inciden `x' m_`x' i.t, re vce(cluster id_ent)
    test m_`x'
    * Si p<=0.10, corre también FE para comparar:
    xtreg Inciden `x' i.t, fe vce(cluster id_ent)
}

xtreg Inciden ginesp_i dessalud_i m_ginesp_i m_dessalud_i i.t, re vce(cluster id_ent)
test m_ginesp_i m_dessalud_i   // H0: ambas medias = 0  → apoya RE si NO se rechaza
* Si se rechaza ⇒ corre FE:
xtreg Inciden ginesp_i dessalud_i i.t, fe vce(cluster id_ent)



xtreg Inciden GiniT_i GiniT_p m_GiniT_i m_GiniT_p i.t, re vce(cluster id_ent)
test m_GiniT_i m_GiniT_p
xtreg Inciden GiniT_i GiniT_p i.t, fe vce(cluster id_ent)

xtreg Inciden giniesp_p dessalud_p m_giniesp_p m_dessalud_p i.t, re vce(cluster id_ent)
test m_giniesp_p m_dessalud_p   // H0: ambas medias = 0  → apoya RE si NO se rechaza
xtreg Inciden giniesp_p dessalud_p i.t, fe vce(cluster id_ent)


*si se quiere reescalar coeficientes de giniespac_i y de salud_i
gen dessalud_i100 = 100*dessalud_i
gen ginesp_i100   = 100*ginesp_i

bys id_ent: egen m_dessalud_i100 = mean(dessalud_i100)
bys id_ent: egen m_ginesp_i100   = mean(ginesp_i100)

xtreg Inciden ginesp_i100 dessalud_i100 m_ginesp_i100 m_dessalud_i100 i.t, re vce(cluster id_ent)
test m_ginesp_i100 m_dessalud_i100

*reescalar
gen GiniT_i100 = 100*GiniT_i
gen GiniT_p100 = 100*GiniT_p
xtreg Inciden GiniT_i100 GiniT_p100 i.t, fe vce(cluster id_ent)

*reescalar
gen giniesp_p100 = 100*giniesp_p
gen dessalud_p100 = 100*dessalud_p
xtreg Inciden giniesp_p100 dessalud_p100 i.t, re vce(cluster id_ent)




* Lista con nombres exactos
local Z "ginesp_i giniesp_p dessalud_i dessalud_p GiniT_i GiniT_p"

foreach X of local Z {
    capture confirm variable `X'
    if _rc {
        di as error "⚠️ `X' no existe, se omite."
        continue
    }

    * Re-escalar a puntos SOLO si está en 0–1 (verifica con: summarize `X')
    gen `X'100 = 100*`X'
    bys id_ent: egen m_`X'100 = mean(`X'100)
    di as txt "=== CRE: Inciden ~ `X' (puntos) ==="
    xtreg Inciden `X'100 m_`X'100 i.t, re vce(cluster id_ent)
    test m_`X'100

    * Si el test rechaza ⇒ FE:
    * xtreg Inciden `X'100 i.t, fe vce(cluster id_ent)
}



*pruebas y correcciones de errores robustos
xttest3                      // heterocedasticidad groupwise en FE
xtserial y x1 x2             // Wooldridge AR(1)
xtcsd, pesaran abs           // dependencia transversal
xtscc y x i.t, fe            // * Sensibilidad: Driscoll–Kraay (instalar xtscc si hace falta)



*unica variable que salio con EF es GiniT_i y se hacen pruebas de robustez
xtreg Inciden GiniT_i i.t, fe
xttest3
xtreg Inciden GiniT_i i.t, fe vce(cluster id_ent)
xtserial Inciden GiniT_i  // Autocorrelación AR(1) dentro de cada estado
xtreg Inciden GiniT_i i.t, fe   //se vuelve a correr para la siguiente prueba
xtcsd, pesaran abs        //Dependencia transversal (correlación entre estados)
ssc install xtscc   //paquete necesario para Driscoll
xtscc Inciden GiniT_i i.t, fe  // Driscoll–Kraay


*con GiniT_p y errores robustos por estado
xtreg Inciden GiniT_p i.t, re vce(cluster id_ent)   //corrige heterocedasticidad y correlacion serial dentro de c/estado
xtcsd, pesaran abs        //Dependencia transversal (correlación entre estados)
xttest0
xtserial Inciden GiniT_p  // Autocorrelación AR(1) dentro de cada estado
xtgls Inciden GiniT_p i.t, panels(hetero) corr(ar1)  // si hay AR(1) muy fuerte


*con  ginesp_i y errores robustos por estado
xtreg Inciden  ginesp_i i.t, re vce(cluster id_ent)   //corrige heterocedasticidad y correlacion serial dentro de c/estado
xtcsd, pesaran abs        //Dependencia transversal (correlación entre estados)
xttest0
xtserial Inciden  ginesp_i  // Autocorrelación AR(1) dentro de cada estado
xtgls Inciden  ginesp_i i.t, panels(hetero) corr(ar1)  // si hay AR(1) muy fuerte


*con  giniesp_p y errores robustos por estado
xtreg Inciden  giniesp_p i.t, re vce(cluster id_ent)   //corrige heterocedasticidad y correlacion serial dentro de c/estado
xtcsd, pesaran abs        //Dependencia transversal (correlación entre estados)
xttest0
xtserial Inciden  giniesp_p  // Autocorrelación AR(1) dentro de cada estado
xtgls Inciden  giniesp_p i.t, panels(hetero) corr(ar1)  // si hay AR(1) muy fuerte


*con  dessalud_i y errores robustos por estado
xtreg Inciden  dessalud_i i.t, re vce(cluster id_ent)   //corrige heterocedasticidad y correlacion serial dentro de c/estado
xtcsd, pesaran abs        //Dependencia transversal (correlación entre estados)
xttest0
xtserial Inciden  dessalud_i  // Autocorrelación AR(1) dentro de cada estado
xtgls Inciden  dessalud_i i.t, panels(hetero) corr(ar1)  // si hay AR(1) muy fuerte

             
 
*con  dessalud_p y errores robustos por estado
xtreg Inciden  dessalud_p i.t, re vce(cluster id_ent)   //corrige heterocedasticidad y correlacion serial dentro de c/estado
xtcsd, pesaran abs        //Dependencia transversal (correlación entre estados)
xttest0
xtserial Inciden  dessalud_p  // Autocorrelación AR(1) dentro de cada estado
xtgls Inciden  dessalud_p i.t, panels(hetero) corr(ar1)  // si hay AR(1) muy fuerte


 





*autocorrelacion dentro del panel
xtserial Inciden ginesp_i dessalud_i
xtserial Inciden giniesp_p dessalud_p
xtserial Inciden GiniT_i GiniT_p


* Ingreso – índices espaciales
xtscc Inciden ginesp_i dessalud_i i.t, fe lag(1)

* Productividad – índices espaciales
xtscc Inciden giniesp_p dessalud_p i.t, fe lag(1)

* Gini tradicional (ingreso y productividad)
xtscc Inciden GiniT_i GiniT_p i.t, fe lag(1)




*Regresiones simples o regresiones base*

*con errores robustos*
xtreg Inciden GiniT_i i.t, fe
eststo FE_GiniTi
xtreg Inciden GiniT_i i.t, fe vce(cluster id_ent)
eststo FEc_GiniTi   // FE con clúster por estado
xtscc Inciden GiniT_i i.t, fe
eststo DK_GiniTi    // FE con Driscoll–Kraay


*con errores robustos para RE
foreach x in GiniT_p ginesp_i giniesp_p dessalud_i dessalud_p {
    xtreg Inciden `x' i.t, re vce(cluster id_ent)
    eststo RE_`x'
}
* (Opcional) Robustez AR(1)+hetero:
foreach x in GiniT_p ginesp_i giniesp_p dessalud_i dessalud_p {
    xtgls Inciden `x' i.t, panels(hetero) corr(ar1)
    eststo GLS_`x'
}
* --- 3) Exporta tabla de coeficientes base
esttab FEc_GiniTi DK_GiniTi RE_GiniT_p RE_ginesp_i RE_giniesp_p RE_dessalud_i RE_dessalud_p ///
      using "coef_basicos.tex", replace b(%9.3f) se(%9.3f) ///
      star(* 0.10 ** 0.05 *** 0.01) label title(Coeficientes base (panel sin espacial))


xtreg Inciden GiniT_i GiniT_p ginesp_i giniesp_p dessalud_i dessalud_p i.t, re  vce(cluster id_ent)


***COmparativo de los modelos que se acaban de correr respecto a los modelos sin errores robustos
*a) Gini tradicional_i
* FE básica con dummies de año (tu especificación base, sin vce robusto)
xtreg Inciden GiniT_i i.t, fe
* (Opcional) FE aún más "básica": sin dummies de año
xtreg Inciden GiniT_i, fe


*Ahora comparativo de los modelos de RE
foreach x in GiniT_p ginesp_i giniesp_p dessalud_i dessalud_p {
    * RE "clásico"
    xtreg Inciden `x' i.t, re
    eststo RE_plain_`x'

    * RE con robustos cluster por estado
    xtreg Inciden `x' i.t, re vce(cluster id_ent)
    eststo RE_clu_`x'
}


* ---------- Vars y etiquetas ----------
local dep   Inciden
local xs_FE   GiniT_i
local xs_RE   GiniT_p ginesp_i giniesp_p dessalud_i dessalud_p
label var Inciden "log(Incidencia)"
label var GiniT_i "Gini tradicional (ingreso)"
label var GiniT_p "Gini tradicional (productividad)"
label var ginesp_i "Gini espacial (ingreso)"
label var giniesp_p "Gini espacial (productividad)"
label var dessalud_i "Desigualdad salud (ingreso)"
label var dessalud_p "Desigualdad salud (productividad)"

* ---------- FE (baseline y robustos) ----------
xtreg `dep' `xs_FE' i.t, fe                    // FE simple (anexo)
eststo FE_plain

xtreg `dep' `xs_FE' i.t, fe vce(cluster id_ent) // FE con clúster (reporte)
eststo FE_cluster

xtscc `dep' `xs_FE' i.t, fe                   // FE con Driscoll-Kraay (robustez)
eststo FE_DK

* ---------- RE (baseline y robustos) ----------
foreach x of local xs_RE {
    xtreg `dep' `x' i.t, re                   // RE simple (anexo)
    eststo RE_plain_`x'

    xtreg `dep' `x' i.t, re vce(cluster id_ent) // RE con clúster (reporte)
    eststo RE_clu_`x'

    * Robustez adicional con FGLS (hetero entre paneles + AR(1) común)
    xtgls `dep' `x' i.t, panels(hetero) corr(ar1)
    eststo GLS_`x'
}
esttab FE_cluster RE_clu_GiniT_p RE_clu_ginesp_i RE_clu_giniesp_p RE_clu_dessalud_i RE_clu_dessalud_p ///
    using "tabla_coef_principal.tex", replace b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    label title("Coeficientes panel (errores robustos)") ///
    addnotes("Todas las regresiones incluyen dummies anuales." ///
             "FE reporta errores robustos agrupados por estado; robustez Driscoll–Kraay no altera conclusiones." ///
             "RE reporta errores robustos agrupados por estado (Arellano).")
esttab FE_plain FE_cluster FE_DK ///
      RE_plain_GiniT_p RE_clu_GiniT_p GLS_GiniT_p ///
      RE_plain_ginesp_i RE_clu_ginesp_i GLS_ginesp_i ///
      RE_plain_giniesp_p RE_clu_giniesp_p GLS_giniesp_p ///
      RE_plain_dessalud_i RE_clu_dessalud_i GLS_dessalud_i ///
      RE_plain_dessalud_p RE_clu_dessalud_p GLS_dessalud_p ///
      using "tabla_robustez.tex", replace b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
      label title("Robustez: simple vs clúster vs FGLS") ///
      addnotes("Los coeficientes son idénticos entre simple y clúster; cambian solo los errores estándar/p-valores." ///
               "FGLS impone heterocedasticidad entre paneles y AR(1) común.")

			 
	  






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


*modelos espaciales individuales
*SAR
xsmle Inciden GiniT_i i.t, ///
    model(sar) wmat(wrn) fe vce(cluster id_ent)
estat impact, values(GiniT_i)    // para ver impactos


xtset id_ent t
spset id_ent
capture confirm numeric variable id_ent
if _rc {
    encode id_ent, gen(id_ent_num)
    drop id_ent
    rename id_ent_num id_ent
}
spset id_ent
spmatrix dir   // debe listar 'wrn'


   *declarar dataset espacial
cd "/Users/dmares/Downloads"

spshape2dta using "areas_geoestadisticas_estatales.shp", ///
    data("mx_data") ///
    coordinates("mx_coords") ///
    replace

* Traer centroides (ajusta la clave si no se llama CVE_ENT)
preserve
use "mx_data.dta", clear
keep _CX _CY CVE_ENT
destring CVE_ENT, replace force
rename CVE_ENT id_ent
tempfile cent
save `cent', replace
restore

* Unir centroides al panel
capture confirm numeric variable id_ent
if _rc {
    encode id_ent, gen(id_ent_num)
    drop id_ent
    rename id_ent_num id_ent
}
merge m:1 id_ent using `cent', nogen

* Panel + espacial y crear W
xtset id_ent t
spset id_ent, coord(_CX _CY)

spmatrix create contiguity wrn, queen normalize(row) replace
* (o k vecinos) spmatrix create knearneigh wrn, k(4) normalize(row) replace
spmatrix dir

* SAR con impactos
xtset id_ent t
spxtregress Inciden GiniT_i i.t, fe dvarlag(wrn) vce(oim)
estat impact, dvarlag


* Partimos de tu panel ya cargado (id_ent, t, Inciden, GiniT_i, ...)

* 1) Tabla de centroides rápidos (lon, lat) por id_ent 1..32
preserve
clear
input id_ent lon lat
1  -102.295 21.883    // AGS
2  -115.454 30.840    // BC   (aprox)
3  -110.312 24.142    // BCS
4  -90.534  19.830    // CAM
5  -101.005 25.426    // COAH
6  -103.724 19.244    // COL
7  -93.115  16.753    // CHIS
8  -106.069 28.634    // CHIH
9  -99.133  19.433    // CDMX
10 -104.653 24.027    // DGO
11 -101.257 21.019    // GTO
12 -99.505  17.553    // GRO
13 -98.762  20.101    // HGO
14 -103.347 20.676    // JAL
15 -99.653  19.286    // EDOMEX
16 -101.195 19.706    // MICH
17 -99.221  18.924    // MOR
18 -104.895 21.509    // NAY
19 -100.316 25.686    // NL
20 -96.726  17.073    // OAX
21 -98.207  19.043    // PUE
22 -100.389 20.588    // QRO
23 -88.305  18.503    // QROO (Chetumal)
24 -100.985 22.156    // SLP
25 -107.394 24.809    // SIN
26 -110.955 29.073    // SON
27 -92.947  17.989    // TAB
28 -99.143  23.741    // TAMPS
29 -98.241  19.313    // TLAX
30 -96.910  19.543    // VER (Xalapa)
31 -89.592  20.967    // YUC
32 -102.583 22.770    // ZAC
end
tempfile cent
save `cent'
restore

* 2) Unir centroides al panel
merge m:1 id_ent using `cent', nogen

* 3) Declarar panel + espacial (coords = lon/lat) y crear W k-vecinos
xtset id_ent t
spset id_ent, coords(lon lat)

spmatrix create knearneigh wrn, k(4) normalize(row) replace
spmatrix dir   // debe listar 'wrn'

* 4) Estimar SAR + impactos
xtset id_ent t
spxtregress Inciden GiniT_i i.t, fe dvarlag(wrn) vce(oim)
estat impact, dvarlag



* Asegura panel
xtset id_ent t

* spset con tus coords (si aún no lo hiciste en esta sesión)
capture noisily spset, clear
spset id_ent
spset, modify coord(lon lat)
spset, modify coordsys(latlong, kilometers)   // opcional, útil para distancias

* --- Crear W a partir de un solo año (evita duplicados de coords) ---
preserve
keep if t == 2014            // o el año que prefieras
bys id_ent: keep if _n==1    // una fila por entidad

spmatrix create idistance wrn, normalize(none)

spmatrix normalize wrn, normalize(row)

spmatrix dir                  // debe listar 'wrn'
restore


*ecuaciones
*fixed effects en spxtregress no acepta dummies de tiempo (se quita i.t)
xtset id_ent t
spxtregress Inciden GiniT_i, fe dvarlag(wrn) vce(oim)
estat impact  //calcula impactos directos, indiectos y totales

*resto de ecuaciones
local Xlist GiniT_i ginesp_i giniesp_p dessalud_i dessalud_p GiniT_p

* RE con dummies de año e impactos (una por una, bivariadas)
foreach X of local Xlist {
    di as text "=== SAR-RE Bivariada: Inciden `X' i.t ==="
    spxtregress Inciden `X' i.t, re dvarlag(wrn) vce(oim)

    * Impactos directo/indirecto/total (no tiene opción post, así que guardemos en un log)
    log using "impact_`X'.smcl", replace
    estat impact
    log close
}






**hacer matriz de contiguidad
* 0) Muévete a la carpeta del shape (ya con .shp, .shx, .dbf, .prj)
cd "Usuarios/dmares/Descargas"

* 1) Verifica que existan los 3 archivos con el mismo basename
dir "areas_geoestadisticas_estatales.*"
dir "areas_geoestadisticas_estatales.dbf"

* 2) Convertir shapefile a .dta  (usa database() y coordinates())
spshape2dta using "areas_geoestadisticas_estatales", ///
    database("mx_data") ///
    coordinates("mx_coords") ///
    replace

use "/Users/dmares/Downloads/wrn.dta", clear
* 2) Crea un id de filas que represente cada entidad (ajusta si tu .dta trae el id real)
capture confirm variable id_ent
if _rc {
    gen id_ent = _n
}

* 3) Pasa X1-X32 a una matriz de Stata y luego a Mata
mkmat X1-X32, matrix(Wst)

mata:
    // toma la matriz desde Stata
    W = st_matrix("Wst")
    n = rows(W)

    // limpia la diagonal por si acaso
    for (i=1; i<=n; i++) W[i,i] = 0

    // vector de ids (1..n) — si tus ids reales están en la columna id_ent:
    // id = st_data(., "id_ent")  // descomenta esta línea y comenta la siguiente
    id = (1..n)'
end

* 4) Crea un objeto spmatrix llamado 'wrn' y normalízalo por filas
spmatrix spfrommata wrn = W id, normalize(row) replace

* 5) Verifica / guarda para reuso
spmatrix dir
spmatrix summarize wrn
spmatrix export wrn using "/Users/dmares/Downloads/wrn_from_dta.txt", replace	
	
* Guarda tu panel actual por si lo tienes en memoria
preserve
tempfile __panel
save `__panel', replace

* Carga la matriz cuadrada (32x32) que abriste
use "/Users/dmares/Downloads/wrn.dta", clear

* Asegura 1 fila por entidad y ordénalas por id_ent
isid id_ent
sort id_ent

* Pasa las columnas X1..X32 a una matriz Stata
ds X*
mkmat `r(varlist)', matrix(Wst)

* Crea 'id' desde la columna id_ent y reordena filas/cols por ese id (robusto)
mata:
    W = st_matrix("Wst")
    id = st_data(., "id_ent")
    ord = order(id, 1)
    id  = id[ord]
    W   = W[ord, ord]
    /* limpia diagonal por si acaso */
    for (i=1; i<=rows(W); i++) W[i,i] = 0
end

* Crea spmatrix 'wrn' y normaliza por filas
spmatrix spfrommata wrn = W id, normalize(row) replace

* (opcional) guarda para reusar
spmatrix export wrn using "/Users/dmares/Downloads/wrn_from_dta.txt", replace

* Verifica
spmatrix dir
spmatrix summarize wrn
restore
* Guarda tu panel si lo tienes cargado
preserve
tempfile __panel
save `__panel', replace

* Carga la matriz cuadrada
use "/Users/dmares/Downloads/wrn.dta", clear





* 1) Declara panel y el id espacial
xtset id_ent t
capture noisily spset, clear
spset id_ent

* 2) Asegúrate de que la W 'wrn' esté en memoria
spmatrix dir
* Si NO aparece 'wrn', impórtala (o vuelve a crearla):
spmatrix import wrn using "/Users/dmares/Downloads/wrn_from_dta.txt", replace
* (o el path donde la guardaste)

*RE o EF con dummies de año + impactos
*dvar se refiere a SAR
spxtregress Inciden GiniT_i , fe dvarlag(wrn) vce(oim)  //no acepta dummies de tiempo
estat impact

spxtregress Inciden GiniT_p i.t, re dvarlag(wrn) vce(oim)
estat impact

spxtregress Inciden ginesp_i i.t, re dvarlag(wrn) vce(oim)
estat impact

spxtregress Inciden giniesp_p i.t, re dvarlag(wrn) vce(oim)
estat impact

spxtregress Inciden dessalud_i i.t, re dvarlag(wrn) vce(oim)
estat impact

spxtregress Inciden dessalud_p i.t, re dvarlag(wrn) vce(oim)
estat impact


*SEM
spxtregress Inciden GiniT_i, fe errorlag(wrn) vce(oim) //no acepta dummies de tiempo

spxtregress Inciden GiniT_p i.t, re errorlag(wrn) vce(oim)

spxtregress Inciden ginesp_i i.t, re errorlag(wrn) vce(oim)

spxtregress Inciden giniesp_p i.t, re errorlag(wrn) vce(oim)

spxtregress Inciden dessalud_i i.t, re errorlag(wrn) vce(oim)

spxtregress Inciden dessalud_p i.t, re errorlag(wrn) vce(oim)
estat moran



*test de Wooldridge AR(1)
* Úsalo con la MISMA especificación temporal (con o sin i.t) que estás usando:
xtserial Inciden GiniT_i            // bivariada FE sin años
xtserial Inciden GiniT_p i.t        // bivariada RE con años
xtserial Inciden ginesp_i i.t
xtserial Inciden giniesp_p i.t
xtserial Inciden dessalud_i i.t
xtserial Inciden dessalud_p i.t


*SDM
* SDM bivariada FE (sin dummies de tiempo)
spxtregress Inciden GiniT_i, fe dvarlag(wrn) ivarlag(wrn: GiniT_i) vce(oim)
estat impact
* (Opcional) ver nombres de los coeficientes para probar si WX=0


foreach X in GiniT_p ginesp_i giniesp_p dessalud_i dessalud_p {
    di as text "=== SDM-RE: Inciden `X' + i.t ==="
    spxtregress Inciden `X' i.t, re dvarlag(wrn) ivarlag(wrn: `X') vce(oim)
    estat impact

}


*mapear efectos
clear all
cd "/Users/dmares/Downloads"

* PANEL desde Excel
import excel using "RegresionPanel-C.xlsx", sheet("APanel") firstrow clear

* Asegura tipos y crea id_ent a partir de 'ent'
capture confirm numeric variable t
if _rc destring t, replace
capture confirm numeric variable Inciden
if _rc destring Inciden, replace force

capture confirm numeric variable ent
if !_rc {
    rename ent id_ent
}
else {
    destring ent, replace
    rename ent id_ent
}

* --- Deja 1 fila por estado ---
keep id_ent Inciden
drop if missing(id_ent) | missing(Inciden)

collapse (mean) Inciden, by(id_ent)
isid id_ent   // <-- ahora debe pasar

* --- Llaves del shapefile (si no las tienes) ---
capture confirm file "mx_keys.dta"
if _rc {
    use "mx_data.dta", clear
    capture confirm numeric variable id_ent
    if _rc {
        destring CVE_ENT, replace
        rename CVE_ENT id_ent
    }
    keep id_ent _ID
    save "mx_keys.dta", replace
}

* --- Une 1:m (un estado → varios polígonos) ---
merge 1:m id_ent using "mx_keys.dta", keep(match) nogen

* --- Mapa ---
which spmap
capture ssc install spmap
spmap Inciden using "mx_coords.dta", id(_ID) ///
      clmethod(quantile) clnumber(5) fcolor(Blues) ndfcolor(gs14) ///
      ocolor(white ..) osize(vthin ..)
graph export "/Users/dmares/Downloads/map_inciden_prom.png", as(png) width(2200) replace


* Re-estima el modelo que quieras auditar (ej. SDM-RE con GiniT_p)
*Mapa de residuales (diagnóstico del modelo)
xtset id_ent t
spxtregress Inciden GiniT_p i.t, re dvarlag(wrn) ivarlag(wrn: GiniT_p)
predict ehat, residuals

preserve
collapse (mean) ehat, by(id_ent)
merge 1:m id_ent using "/Users/dmares/Downloads/mx_keys.dta", nogen keep(match)
spmap ehat using "/Users/dmares/Downloads/mx_coords.dta", id(_ID) ///
      clmethod(quantile) clnumber(5) fcolor(Reds) ndfcolor(gs14) ///
      ocolor(white ..) osize(vthin ..) title("Residuales promedio SDM (GiniT_p)")
graph export "/Users/dmares/Downloads/map_residuales_sdm_ginitp.png", as(png) width(2200) replace
restore


*Mapa del efecto contextual (WX) para SDM
* Tras el SDM:
spgenerate WX_GiniTp = wrn*GiniT_p
scalar theta = _b[wrn:GiniT_p]
gen contrib_WX = theta * WX_GiniTp

preserve
collapse (mean) contrib_WX, by(id_ent)
merge 1:m id_ent using "/Users/dmares/Downloads/mx_keys.dta", nogen keep(match)
spmap contrib_WX using "/Users/dmares/Downloads/mx_coords.dta", id(_ID) ///
      clmethod(quantile) clnumber(5) fcolor(Greens) ndfcolor(gs14) ///
      ocolor(white ..) osize(vthin ..) title("Contribución contextual (θ·W·GiniT_p)")
graph export "/Users/dmares/Downloads/map_contexto_ginitp.png", as(png) width(2200) replace
restore

*Loop para series por año (si quieres mini-atlas por t)
levelsof t, local(YY)
foreach y of local YY {
    preserve
    keep if t==`y'
    collapse (mean) Inciden, by(id_ent)
    merge 1:m id_ent using "/Users/dmares/Downloads/mx_keys.dta", nogen keep(match)
    spmap Inciden using "/Users/dmares/Downloads/mx_coords.dta", id(_ID) ///
          clmethod(quantile) clnumber(5) fcolor(Blues) ndfcolor(gs14) ///
          ocolor(white ..) osize(vthin ..) title("Incidencia `y'")
    graph export "/Users/dmares/Downloads/map_inciden_`y'.png", as(png) width(1800) replace
    restore
}

*Pasar a multivariadas (controles) manteniendo la lógica SAR/SDM
spxtregress Inciden GiniT_i escolaridad gasto_salud verdulerias hospitales ///
    , fe dvarlag(wrn) vce(oim)
estat impact

spxtregress Inciden GiniT_p escolaridad gasto_salud verdulerias hospitales i.t, ///
    re dvarlag(wrn) ivarlag(wrn: GiniT_p escolaridad gasto_salud verdulerias hospitales) vce(oim)
estat impact







* --- Llave con nombres (desde mx_data.dta) ---
preserve
use "/Users/dmares/Downloads/mx_data.dta", clear
capture confirm string variable CVE_ENT
if !_rc destring CVE_ENT, gen(id_ent)
keep id_ent NOM_ENT
tempfile keys
save `keys'
restore

* --- MATA: impactos por estado (SAR y SDM) ---
mata:
    W = st_matrix("W")
    n = rows(W)
    I = I(n)

    // ========== SAR–FE: Inciden ~ GiniT_i ==========
    rho  = 0.5830016
    beta = -3.264424
    S = invsym(I - rho*W)
    M = S*(beta*I)
    dir  = diagonal(M)
    indir = rowsum(M) :- dir
    tot  = rowsum(M)
    st_matrix("sar_dir", dir)
    st_matrix("sar_ind", indir)
    st_matrix("sar_tot", tot)

    // ========== SDM–RE (+ i.t): Inciden ~ GiniT_p ==========
    rho   = 0.0419957
    beta  = 0.7172278
    theta = 0.8970245
    S = invsym(I - rho*W)
    M = S*(beta*I + theta*W)
    dir  = diagonal(M)
    indir = rowsum(M) :- dir
    tot  = rowsum(M)
    st_matrix("sdm1_dir", dir)
    st_matrix("sdm1_ind", indir)
    st_matrix("sdm1_tot", tot)

    // ========== SDM–RE (+ i.t): Inciden ~ giniesp_p ==========
    rho   = 0.0123751
    beta  = 2.024394
    theta = 2.02896
    S = invsym(I - rho*W)
    M = S*(beta*I + theta*W)
    dir  = diagonal(M)
    indir = rowsum(M) :- dir
    tot  = rowsum(M)
    st_matrix("sdm2_dir", dir)
    st_matrix("sdm2_ind", indir)
    st_matrix("sdm2_tot", tot)
end

* --- Vuelca matrices a datos y exporta CSV para QGIS ---
clear
set obs 32
gen id_ent = _n
svmat double sar_dir,  names(SARdir_)
svmat double sar_ind,  names(SARind_)
svmat double sar_tot,  names(SARtot_)
svmat double sdm1_dir, names(SDM1dir_)
svmat double sdm1_ind, names(SDM1ind_)
svmat double sdm1_tot, names(SDM1tot_)
svmat double sdm2_dir, names(SDM2dir_)
svmat double sdm2_ind, names(SDM2ind_)
svmat double sdm2_tot, names(SDM2tot_)

rename (SARdir_1 SARind_1 SARtot_1) (dir_SAR_GiniTi ind_SAR_GiniTi tot_SAR_GiniTi)
rename (SDM1dir_1 SDM1ind_1 SDM1tot_1) (dir_SDM_GiniTp ind_SDM_GiniTp tot_SDM_GiniTp)
rename (SDM2dir_1 SDM2ind_1 SDM2tot_1) (dir_SDM_giniesp ind_SDM_giniesp tot_SDM_giniesp)

merge 1:1 id_ent using `keys', nogen
order id_ent NOM_ENT dir_SAR_GiniTi ind_SAR_GiniTi tot_SAR_GiniTi ///
                  dir_SDM_GiniTp ind_SDM_GiniTp tot_SDM_GiniTp   ///
                  dir_SDM_giniesp ind_SDM_giniesp tot_SDM_giniesp

export delimited using "/Users/dmares/Downloads/impactos_indirectos_por_estado.csv", replace


*lo que exporta cada estado a los demás si cambia su propia desigualdad.
* Asume W (32x32) ya normalizada y tu archivo de nombres `keys` (id_ent, NOM_ENT)
* Coefs del SAR (GiniT_i) que reportaste:
local rho  = 0.5830016
local beta = -3.264424

mata:
    W = st_matrix("W"); n = rows(W); I = I(n)
    rho  = strtoreal(st_local("rho"))
    beta = strtoreal(st_local("beta"))
    S = invsym(I - rho*W)
    M = S*(beta*I)

    expo  = rowsum(M) :- diagonal(M)   // ya mapeado (spillover recibido)
    influ = colsum(M)' :- diagonal(M)  // NUEVO: spillover exportado
    net   = influ - expo               // balance exportador neto

    st_matrix("sar_expo",  expo)
    st_matrix("sar_influ", influ)
    st_matrix("sar_net",   net)
end

clear
set obs 32
gen id_ent = _n
svmat sar_expo,  names(EXPO_)
svmat sar_influ, names(INFLU_)
svmat sar_net,   names(NET_)
rename (EXPO_1 INFLU_1 NET_1) (expo_SAR_GiniTi influ_SAR_GiniTi net_SAR_GiniTi)

* Une nombres y exporta
merge 1:1 id_ent using `keys', nogen
order id_ent NOM_ENT expo_SAR_GiniTi influ_SAR_GiniTi net_SAR_GiniTi
export delimited using "/Users/dmares/Downloads/SAR_GiniTi_influencia.csv", replace





*bloque para impactos robustos
******************************
clear all
set more off

* ========= 0) Cargar y normalizar W =========
use "/Users/dmares/Downloads/wrn.dta", clear
capture confirm variable id_ent
if _rc gen id_ent = _n
sort id_ent

ds X*
local Wvars `r(varlist)'
mkmat `Wvars', matrix(W)

mata:
    W = st_matrix("W")
    // normalización por filas
    rs = rowsum(W)
    for (i=1; i<=rows(W); i++) {
        s = rs[i]
        if (s!=0) W[i,.] = W[i,.] :/ s
    }
    st_matrix("W", W)
end

* ========= 1) Coefs desde tus resultados =========
* SDM – Gini tradicional (productividad)
local rho_SDMgp   = 0.0419957
local beta_SDMgp  = 0.7172278
local theta_SDMgp = 0.8970245

* SDM – Gini espacial (productividad)
local rho_SDMge   = 0.0123751
local beta_SDMge  = 2.024394
local theta_SDMge = 2.02896

* (Opcional) SAR – Gini ingreso (por si quieres tabla completa)
local rho_SAR   = 0.5830016
local beta_SAR  = -3.264424
local theta_SAR = 0

* ========= 2) Función de impactos (matriz M y vectores) =========
mata:
    void sdm_impacts(string scalar tag, real matrix W, real scalar rho, real scalar beta, real scalar theta)
    {
        real scalar n
        real matrix I, S, M
        real colvector d, ind, expo

        n = rows(W)
        I = I(n)
        S = invsym(I - rho*W)
        M = S*(beta*I + theta*W)

        d   = diagonal(M)
        ind = rowsum(M) :- d      // spillover recibido por estado
        st_matrix(tag+"_ind", ind)
    }

    W = st_matrix("W")

    sdm_impacts("SDMgp", W, strtoreal(st_local("rho_SDMgp")), strtoreal(st_local("beta_SDMgp")), strtoreal(st_local("theta_SDMgp")))
    sdm_impacts("SDMge", W, strtoreal(st_local("rho_SDMge")), strtoreal(st_local("beta_SDMge")), strtoreal(st_local("theta_SDMge")))
    sdm_impacts("SAR__" , W, strtoreal(st_local("rho_SAR"  )), strtoreal(st_local("beta_SAR"  )), strtoreal(st_local("theta_SAR" )))
end

* ========= 3) Pasar a dataset y construir diferencias =========
clear
set obs 32
gen id_ent = _n

svmat SDMgp_ind, names(ind_SDMgp_)
svmat SDMge_ind, names(ind_SDMge_)
svmat SAR___ind, names(ind_SAR__)

rename (ind_SDMgp_1 ind_SDMge_1 ind_SAR__1) ///
       (ind_SDM_GiniTp ind_SDM_giniesp ind_SAR_GiniTi)

* Diferencias (espacial – tradicional) y porcentaje
gen ind_SDM_delta     = ind_SDM_giniesp - ind_SDM_GiniTp
gen ind_SDM_delta_pct = 100*(ind_SDM_giniesp/ind_SDM_GiniTp - 1)

* ========= 4) Unir nombres y exportar =========
capture confirm file "/Users/dmares/Downloads/mx_keys.dta"
if !_rc {
    merge 1:1 id_ent using "/Users/dmares/Downloads/mx_keys.dta", nogen
}

order id_ent NOM_ENT ind_SAR_GiniTi ind_SDM_GiniTp ind_SDM_giniesp ind_SDM_delta ind_SDM_delta_pct

* Verificación rápida (debe haber valores no missing):
summ ind_SDM_GiniTp ind_SDM_giniesp ind_SDM_delta

export delimited using "/Users/dmares/Downloads/spillovers_por_estado_3modelos.csv", replace
export delimited id_ent NOM_ENT ind_SDM_delta ind_SDM_delta_pct ///
    using "/Users/dmares/Downloads/spillovers_SDM_diferencia.csv", replace

	
	
*******
* 0) Asegura que existen id_ent y t (si t es texto: destring)
capture confirm variable id_ent
capture confirm variable t
if _rc {
    di as error "Falta la variable de tiempo t. Revisa nombres."
}

* 1) Declara el panel otra vez
xtset id_ent t

* 2) Limpia marca espacial previa y vuelve a declararla solo con el ID
capture noisily spset, clear
spset id_ent

* 3) Verifica: ahora debe decir "Data: Panel"
spset




	
***** regresiones ¿robustas?
***************************
cd "/Users/dmares/Downloads"

* 1) Convierte el shapefile a .dta (atributos + coordenadas)
*    (NO uses genc(), sólo genid() )
ssc install shp2dta, replace
shp2dta using "mx", database("mx_data") coordinates("mx_coords") genid(_ID) replace

* 2) Abre los atributos y crea tu id_ent si viene como string/código
use "mx_data.dta", clear
capture confirm numeric variable id_ent
if _rc {
    capture confirm string variable CVE_ENT
    if !_rc destring CVE_ENT, gen(id_ent)
}

* 3) Marca espacial ENLAZANDO el archivo de coordenadas
*    (esta sintaxis SÍ aplica para shp2dta)
capture noisily spset, clear
spset id_ent using "mx_coords.dta"

* 4) Crea la matriz de contigüidad (queen) y déjala en memoria como 'wrn'
spmatrix create contiguity wrn, queen replace
spmatrix dir

* Crea W de contigüidad tipo rook (default)
spmatrix create contiguity wrn, replace
spmatrix dir    // debe listar 'wrn'
spmatrix summarize wrn



spxtregress Inciden GiniT_i escolh escolm gtosal Verdulerias hosp i.t, ///
    re dvarlag(wrn) ivarlag(wrn: GiniT_i escolh escolm gtosal Verdulerias hosp) vce(oim)

estat impact



*ya que se tiene la matriz espacial, se procede a calcular la regresión
* Regresa a tu panel
import excel using "/Users/dmares/Downloads/RegresionPanel-C.xlsx", ///
    sheet("APanel") firstrow clear

capture confirm numeric variable id_ent
if _rc rename ent id_ent
capture confirm numeric variable t
if _rc destring t, replace force

xtset id_ent t
spset id_ent
spmatrix dir   // verifica que 'wrn' sigue en memoria



* Limpia espacios y convierte a numérico
replace id_ent = strtrim(id_ent)
destring id_ent, replace   // ← esto hace id_ent numérico

* Asegura t numérico
capture confirm numeric variable t
if _rc destring t, replace force

* Declara panel y espacial
xtset id_ent t
spset id_ent
spmatrix dir   // verifica que 'wrn' sigue en memoria




di as res "Y = `Y'"
di as res "XZ_ok = `XZ_ok'"
di as res "Mlist = `Mlist'"



* Elige X y controles que EXISTAN en tu base
* ——— Asume que ya tienes xtset id_ent t, spset id_ent y wrn en memoria ———
* === Definición persistente ===
global Y       Inciden
global X       dessalud_p
global Z       "escolh escolm gtosal Verdulerias hosp"

* === Mundlak/CRE: crea medias solo si faltan ===
foreach v in $X $Z {
    capture confirm variable m_`v'
    if _rc bysort id_ent: egen m_`v' = mean(`v')
}

* Por claridad, arma listas "planas" (no-macro) para inspección
local XZ_ok   $X $Z
local Mlist   m_$X m_escolh m_escolm m_gtosal m_Verdulerias m_hosp

di as res "Y = $Y"
di as res "XZ_ok = `XZ_ok'"
di as res "Mlist = `Mlist'"

* === SDM–RE con Mundlak + dummies de año ===
spxtregress $Y `XZ_ok' `Mlist' i.t, re ///
    dvarlag(wrn) ivarlag(wrn: `XZ_ok') vce(oim)

* Decisión RE vs FE vía CRE (Mundlak):
testparm `Mlist'      // p>=0.10 ⇒ RE válido; p<0.10 ⇒ mejor FE

* Impactos (directo, indirecto, total)
estat impact
* SDM–FE (sin dummies de año)
spxtregress Inciden dessalud_i escolh escolm gtosal Verdulerias hosp, ///
    fe dvarlag(wrn) ivarlag(wrn: ginesp_i escolh escolm gtosal Verdulerias hosp) vce(oim)

estat impact
* SAR–FE
spxtregress Inciden dessalud_i escolh escolm gtosal Verdulerias hosp, ///
    fe dvarlag(wrn) vce(oim)

* SEM–FE
spxtregress Inciden dessalud_i escolh escolm gtosal Verdulerias hosp, ///
    fe errorlag(wrn) vce(oim)

	* Demean por año (para DV y X)
foreach v in Inciden dessalud_i escolh escolm gtosal Verdulerias hosp {
    bysort t: egen mean_`v' = mean(`v')
    gen `v'_dm = `v' - mean_`v'
}

* SDM–FE con variables "time-demeaned"
spxtregress Inciden_dm dessalud_i_dm escolh_dm escolm_dm gtosal_dm Verdulerias_dm hosp_dm, ///
    fe dvarlag(wrn) ivarlag(wrn: dessalud_i_dm escolh_dm escolm_dm gtosal_dm Verdulerias_dm hosp_dm) vce(oim)

estat impact

	
*algoritmo para correr SEM, SAR y SDM de multivariadas con correcciones de errores
*******************************************************
* MULTIVARIADAS con "corrección" y robustez SAR/SEM/SDM
*******************************************************
local Y     Inciden
local Z     escolh escolm gtosal Verdulerias hosp

* Decisiones FE/RE ya tomadas por CRE:
* (ajusta si cambian)
local FElist  GiniT_i giniesp_i giniesp_p dessalud_i
local RElist  GiniT_p dessalud_p

* ---------- helper: residualiza por año (para FE) ----------
program drop _all
program define _twfe_time_resid
    syntax varlist(min=1) [if], panel(id_ent) time(t)
    marksample touse
    quietly levelsof `time' if `touse', local(TS)
    foreach v of local varlist {
        regress `v' i.`time' if `touse'
        predict double r_`v' if `touse', resid
    }
end

* ---------- SDM (principal) ----------
display as text "===== SDM (Durbin general) ====="
* FE: residualizar tiempo y estimar SDM-FE (sin i.t)
foreach X of local FElist {
    display as result "SDM–FE: `X'"
    preserve
        * construye residuales por año para Y, X y controles
        _twfe_time_resid `Y' `X' `Z', panel(id_ent) time(t)

        * corre SDM en residuales
        spxtregress r_`Y' r_`X' r_`Z', ///
            fe dvarlag(wrn) ivarlag(wrn: r_`X' r_`Z') vce(oim)

        estat impact   // impactos directo/indirecto/total
    restore
}

* RE: SDM con i.t
foreach X of local RElist {
    display as result "SDM–RE: `X'"
    spxtregress `Y' `X' `Z' i.t, ///
        re dvarlag(wrn) ivarlag(wrn: `X' `Z') vce(oim)
    estat impact
}

* ---------- SAR (rezago en Y) ----------
display as text "===== SAR (solo Wy) ====="
* FE: residualizar tiempo
foreach X of local FElist {
    display as result "SAR–FE: `X'"
    preserve
        _twfe_time_resid `Y' `X' `Z', panel(id_ent) time(t)
        spxtregress r_`Y' r_`X' r_`Z', fe dvarlag(wrn)
        estat impact
    restore
}
* RE
foreach X of local RElist {
    display as result "SAR–RE: `X'"
    spxtregress `Y' `X' `Z' i.t, re dvarlag(wrn)
    estat impact
}

* ---------- SEM (error espacial, no hay impactos) ----------
display as text "===== SEM (error espacial) ====="
* FE: residualizar tiempo
foreach X of local FElist {
    display as result "SEM–FE: `X'"
    preserve
        _twfe_time_resid `Y' `X' `Z', panel(id_ent) time(t)
        spxtregress r_`Y' r_`X' r_`Z', fe errorlag(wrn)
        * aquí no hay estat impact
    restore
}
* RE
foreach X of local RElist {
    display as result "SEM–RE: `X'"
    spxtregress `Y' `X' `Z' i.t, re errorlag(wrn)
}

	
	*******************************************************
* SUPONE: xtset id_ent t  +  spmatrix dir muestra 'wrn'
*         Y = Inciden; controles Z existen en la base
*******************************************************
local Y     Inciden
local Z     escolh escolm gtosal Verdulerias hosp

* Decisiones FE/RE (tuyas, vía CRE/Mundlak)
local FElist  GiniT_i giniesp_i giniesp_p dessalud_i
local RElist  GiniT_p dessalud_p

* ---------- helper: residualizar por año (para FE) ----------
program drop _all
program define _twfe_time_resid
    syntax varlist(min=1) [if], panel(id_ent) time(t)
    marksample touse
    foreach v of local varlist {
        regress `v' i.`time' if `touse'
        predict double r_`v' if `touse', resid
    }
end

* Contenedores de resultados
tempname fh1 fh2 fh3 fh4
tempfile T_SDM T_SAR T_SEM T_RHO
postfile `fh1' str12(modelo) str6(spec) str8(effect) str32(var) ///
        dy_dx se z p ll ul using "`T_SDM'", replace
postfile `fh2' str12(modelo) str6(spec) str8(effect) str32(var) ///
        dy_dx se z p ll ul using "`T_SAR'", replace
postfile `fh3' str12(modelo) str6(spec) str12(param)  coef se z p ll ul using "`T_SEM'", replace
postfile `fh4' str12(modelo) str6(spec) str12(param)  coef se z p ll ul using "`T_RHO'", replace

* ---------- SDM (principal) ----------
* FE: sin i.t (residualizamos tiempo)
foreach X of local FElist {
    preserve
        _twfe_time_resid `Y' `X' `Z', panel(id_ent) time(t)
        spxtregress r_`Y' r_`X' r_`Z', fe dvarlag(wrn) ivarlag(wrn: r_`X' r_`Z') vce(oim)
        estat impact
        foreach eff in direct indirect total {
            matrix M = r(`eff')
            local k = colsof(M)
            local cn : colnames M
            forvalues j=1/`k' {
                local vname : word `j' of `cn'
                post `fh1' ("SDM") ("FE") ("`eff'") ("`vname'") ///
                    (M[1,`j']) (M[2,`j']) (M[3,`j']) (M[4,`j']) (M[5,`j']) (M[6,`j'])
            }
        }
    restore
}
* RE: con i.t
foreach X of local RElist {
    spxtregress `Y' `X' `Z' i.t, re dvarlag(wrn) ivarlag(wrn: `X' `Z') vce(oim)
    estat impact
    foreach eff in direct indirect total {
        matrix M = r(`eff')
        local k = colsof(M)
        local cn : colnames M
        forvalues j=1/`k' {
            local vname : word `j' of `cn'
            post `fh1' ("SDM") ("RE") ("`eff'") ("`vname'") ///
                (M[1,`j']) (M[2,`j']) (M[3,`j']) (M[4,`j']) (M[5,`j']) (M[6,`j'])
        }
    }
}

* ---------- SAR (Wy) ----------
foreach X of local FElist {
    preserve
        _twfe_time_resid `Y' `X' `Z', panel(id_ent) time(t)
        spxtregress r_`Y' r_`X' r_`Z', fe dvarlag(wrn) vce(oim)
        estat impact
        foreach eff in direct indirect total {
            matrix M = r(`eff')
            local k = colsof(M)
            local cn : colnames M
            forvalues j=1/`k' {
                local vname : word `j' of `cn'
                post `fh2' ("SAR") ("FE") ("`eff'") ("`vname'") ///
                    (M[1,`j']) (M[2,`j']) (M[3,`j']) (M[4,`j']) (M[5,`j']) (M[6,`j'])
            }
        }
        * guarda rho
        matrix b = e(b)
        capture scalar rho = b[1,"wrn: `Y'"]
        if _rc!=0 capture scalar rho = b[1,"wrn: r_`Y'"]
        if _rc==0 post `fh4' ("SAR") ("FE") ("rho") (rho) (.)(.)(.)(.)(.)
    restore
}
foreach X of local RElist {
    spxtregress `Y' `X' `Z' i.t, re dvarlag(wrn) vce(oim)
    estat impact
    foreach eff in direct indirect total {
        matrix M = r(`eff')
        local k = colsof(M)
        local cn : colnames M
        forvalues j=1/`k' {
            local vname : word `j' of `cn'
            post `fh2' ("SAR") ("RE") ("`eff'") ("`vname'") ///
                (M[1,`j']) (M[2,`j']) (M[3,`j']) (M[4,`j']) (M[5,`j']) (M[6,`j'])
        }
    }
    matrix b = e(b)
    capture scalar rho = b[1,"wrn: `Y'"]
    if _rc==0 post `fh4' ("SAR") ("RE") ("rho") (rho) (.)(.)(.)(.)(.)
}

* ---------- SEM (errorlag) ----------
foreach X of local FElist {
    preserve
        _twfe_time_resid `Y' `X' `Z', panel(id_ent) time(t)
        spxtregress r_`Y' r_`X' r_`Z', fe errorlag(wrn) vce(oim)
        matrix b = e(b)
        capture scalar lam = b[1,"wrn: e.`Y'"]
        if _rc!=0 capture scalar lam = b[1,"wrn: e.r_`Y'"]
        if _rc==0 post `fh3' ("SEM") ("FE") ("lambda") (lam) (.)(.)(.)(.)(.)
    restore
}
foreach X of local RElist {
    spxtregress `Y' `X' `Z' i.t, re errorlag(wrn) vce(oim)
    matrix b = e(b)
    capture scalar lam = b[1,"wrn: e.`Y'"]
    if _rc==0 post `fh3' ("SEM") ("RE") ("lambda") (lam) (.)(.)(.)(.)(.)
}

* Limpia cualquier post pendiente y (re)declara los handles
* === 0) Carpeta de salida persistente ===
local out "/Users/dmares/Downloads"

* Limpia sistema de post y reabre handles
capture postutil clear
tempname fh1 fh2 fh3 fh4

postfile `fh1' str12(modelo) str6(spec) str8(effect) str32(var) ///
        dy_dx se z p ll ul using "`out'/T_SDM.dta", replace
postfile `fh2' str12(modelo) str6(spec) str8(effect) str32(var) ///
        dy_dx se z p ll ul using "`out'/T_SAR.dta", replace
postfile `fh3' str12(modelo) str6(spec) str12(param) coef se z p ll ul ///
        using "`out'/T_SEM.dta", replace
postfile `fh4' str12(modelo) str6(spec) str12(param) coef se z p ll ul ///
        using "`out'/T_RHO.dta", replace

* === 1) << AQUÍ VAN TUS BUCLES que hacen post `fh1' `fh2' `fh3' `fh4' >>
* Ejemplo de "forzar" al menos 1 renglón si quieres probar:
* post `fh1' ("SDM") ("RE") ("direct") ("GiniT_p") (.) (.) (.) (.) (.) 

* === 2) Cierra los handles (protegido) ===
capture postclose `fh1'
capture postclose `fh2'
capture postclose `fh3'
capture postclose `fh4'






*--- limpia medias Mundlak si ya existían ---
capture drop m_GiniT_p m_escolh m_escolm m_gtosal m_Verdulerias m_hosp

*--- define X y controles explícitos ---
local X  GiniT_p
local Z  escolh escolm gtosal Verdulerias hosp
local XZ_ok `X' `Z'

*--- medias por panel (Mundlak) para RE ---
foreach v in `XZ_ok' {
    by id_ent: egen m_`v' = mean(`v')
}
local Mlist
foreach v in `XZ_ok' {
    local Mlist `Mlist' m_`v'
}

*--- corre SDM-RE + años ---
spxtregress Inciden `XZ_ok' `Mlist' i.t, re ///
    dvarlag(wrn) ivarlag(wrn: `XZ_ok') vce(oim)

estat impact
testparm `Mlist'   // (verificación CRE)



****--- define X y controles ---
local X  giniesp_p
local Z  escolh escolm gtosal Verdulerias hosp
local XZ_ok `X' `Z'

*--- FE (sin i.t y sin m_*) ---
spxtregress Inciden `XZ_ok', fe ///
    dvarlag(wrn) ivarlag(wrn: `XZ_ok') vce(oim)

estat impact


* Controles (ajusta si quieres añadir/quitar alguno)
local Z escolh escolm gtosal Verdulerias hosp

* Gini tradicional (ingreso) + controles — FE
spxtregress Inciden GiniT_i `Z', fe ///
    dvarlag(wrn) ivarlag(wrn: GiniT_i `Z') vce(oim)

estat impact
estimates store FE_GiniTi_SDM

* Gini espacial (ingreso) + controles — FE
spxtregress Inciden ginesp_i `Z', fe ///
    dvarlag(wrn) ivarlag(wrn: ginesp_i `Z') vce(oim)

estat impact
estimates store FE_giniespI_SDM

* Desigualdad salud (ingreso) + controles — FE
spxtregress Inciden dessalud_i `Z', fe ///
    dvarlag(wrn) ivarlag(wrn: dessalud_i `Z') vce(oim)

estat impact
estimates store FE_saludI_SDM

* Limpia Mundlak previos para evitar "already defined"
capture drop m_*

* Arma la lista XZ_ok (X + controles)
local XZ_ok dessalud_p `Z'

* Medias por panel (Mundlak)
foreach v in `XZ_ok' {
    by id_ent: egen m_`v' = mean(`v')
}
local Mlist
foreach v in `XZ_ok' {
    local Mlist `Mlist' m_`v'
}

* SDM–RE + años
spxtregress Inciden `XZ_ok' `Mlist' i.t, re ///
    dvarlag(wrn) ivarlag(wrn: `XZ_ok') vce(oim)

estat impact
testparm `Mlist'     // (verificación CRE)
estimates store RE_saludP_SDM

xtset id_ent t
spset id_ent
spmatrix dir    // debe listar 'wrn'





***codigo
* Y y controles (ajusta si quieres añadir/quitar)
local Y  Inciden
local Z  escolh escolm gtosal Verdulerias hosp

* Estructura panel y W (solo verificación)
xtset id_ent t
spset id_ent
spmatrix dir   // debe listar 'wrn'

* ===== SDM–FE: X + controles =====
capture drop m_*
foreach X in GiniT_i ginesp_i giniesp_p dessalud_i {
    di as text "== SDM–FE con `X' =="
    spxtregress `Y' `X' `Z', fe ///
        dvarlag(wrn) ivarlag(wrn: `X' `Z') vce(oim)
    estat impact
    estimates store SDM_FE_`X'
}

* ===== SDM–RE (CRE): X + controles + Mundlak + años =====
capture drop m_*
foreach X in GiniT_p dessalud_p {
    local XZ_ok `X' `Z'
    * Medias por panel (Mundlak)
    foreach v in `XZ_ok' {
        by id_ent: egen m_`v' = mean(`v')
    }
    local Mlist
    foreach v in `XZ_ok' {
        local Mlist `Mlist' m_`v'
    }

    di as text "== SDM–RE (CRE) con `X' =="
    spxtregress `Y' `XZ_ok' `Mlist' i.t, re ///
        dvarlag(wrn) ivarlag(wrn: `XZ_ok') vce(oim)
    estat impact
    testparm `Mlist'   // re-chequeo CRE
    estimates store SDM_RE_`X'
    capture drop m_*
}


* --- SAR–FE (para los de FE) ---
foreach X in GiniT_i ginesp_i giniesp_p dessalud_i {
    di as text "== SAR–FE con `X' =="
    spxtregress `Y' `X' `Z', fe dvarlag(wrn) vce(oim)
    estat impact
    estimates store SAR_FE_`X'
}

* --- SAR–RE (para los de RE) ---
capture drop m_*
foreach X in GiniT_p dessalud_p {
    local XZ_ok `X' `Z'
    foreach v in `XZ_ok' {
        by id_ent: egen m_`v' = mean(`v')
    }
    local Mlist
    foreach v in `XZ_ok' {
        local Mlist `Mlist' m_`v'
    }

    di as text "== SAR–RE (CRE) con `X' =="
    spxtregress `Y' `XZ_ok' `Mlist' i.t, re dvarlag(wrn) vce(oim)
    estat impact
    testparm `Mlist'
    estimates store SAR_RE_`X'
    capture drop m_*
}


* --- SEM–FE (para los de FE) ---
foreach X in GiniT_i ginesp_i giniesp_p dessalud_i {
    di as text "== SEM–FE con `X' =="
    spxtregress `Y' `X' `Z', fe errorlag(wrn) vce(oim)
    estimates store SEM_FE_`X'
}

* --- SEM–RE (para los de RE) ---
capture drop m_*
foreach X in GiniT_p dessalud_p {
    local XZ_ok `X' `Z'
    foreach v in `XZ_ok' {
        by id_ent: egen m_`v' = mean(`v')
    }
    local Mlist
    foreach v in `XZ_ok' {
        local Mlist `Mlist' m_`v'
    }

    di as text "== SEM–RE (CRE) con `X' =="
    spxtregress `Y' `XZ_ok' `Mlist' i.t, re errorlag(wrn) vce(oim)
    testparm `Mlist'
    estimates store SEM_RE_`X'
    * Extra: extrae lambda
    matrix b = e(b)
    capture di as res "lambda = " b[1,"wrn: e.`Y'"]
    capture drop m_*
}






*******************************************************
* MULTIVARIADAS ESPACIALES (SDM / SAR / SEM) CON CRE
* Autor: tú + yo :)
* Requisitos previos:
*   - Excel con hoja APanel (id_ent, t, Inciden, regresores)
*   - spmatrix 'wrn' en memoria (o .stswm en disco)
*******************************************************

*RE o EF con dummies de año + impactos
*dvar se refiere a SAR
spxtregress Inciden GiniT_i escolh escolm gtosal Verdulerias enferm, fe dvarlag(wrn) vce(oim)  //no acepta dummies de tiempo
estat impact

spxtregress Inciden ginesp_i escolh escolm gtosal Verdulerias hosp, fe dvarlag(wrn) vce(oim)  //no acepta dummies de tiempo
estat impact

spxtregress Inciden giniesp_p escolh escolm gtosal Verdulerias hosp, fe dvarlag(wrn) vce(oim)  //no acepta dummies de tiempo
estat impact

spxtregress Inciden dessalud_i escolh escolm gtosal Verdulerias hosp, fe dvarlag(wrn) vce(oim)  //no acepta dummies de tiempo
estat impact


spxtregress Inciden GiniT_p escolh escolm gtosal Verdulerias hosp i.t, re dvarlag(wrn) vce(oim)
estat impact


spxtregress Inciden dessalud_p escolh escolm gtosal Verdulerias hosp i.t, re dvarlag(wrn) vce(oim)
estat impact


*SEM
spxtregress Inciden GiniT_i, fe errorlag(wrn) vce(oim) //no acepta dummies de tiempo

spxtregress Inciden GiniT_p i.t, re errorlag(wrn) vce(oim)

spxtregress Inciden ginesp_i i.t, re errorlag(wrn) vce(oim)

spxtregress Inciden giniesp_p i.t, re errorlag(wrn) vce(oim)

spxtregress Inciden dessalud_i i.t, re errorlag(wrn) vce(oim)

spxtregress Inciden dessalud_p i.t, re errorlag(wrn) vce(oim)
estat moran




*SDM
* SDM bivariada FE (sin dummies de tiempo)
spxtregress Inciden GiniT_i, fe dvarlag(wrn) ivarlag(wrn: GiniT_i) vce(oim)
estat impact
spxtregress Inciden `X' i.t, re dvarlag(wrn) ivarlag(wrn: `X') vce(oim)
estat impact





*análisis de robustez
*******************
*============================================================*
*  MULTIVARIADAS ESPACIALES "AL ESTILO BIVARIADO" (SIN AÑOS) *
*  Requiere: wrn ya registrado; panel xtset id_ent t         *
*============================================================*

* 0) Asegura panel + W existentes
xtset id_ent t
spset id_ent
spset id_ent t
spmatrix dir
spmatrix summarize wrn

* 1) Elige variable focal y controles (sin años)
local keyvar  GiniT_i
local ctrls1  escolh escolm gtosal Verdulerias enferm        // versión con enfermeras
local ctrls2  escolh escolm gtosal Verdulerias hosp          // versión con hospitales
* (si tienes "territorio", agrégalo: territorio)

* 2) Mantén muestra consistente (lista) y vuelve a XTSET
preserve
keep id_ent t Inciden `keyvar' `ctrls1' `ctrls2'
keep if !missing(Inciden, `keyvar')
foreach v of varlist `ctrls1' `ctrls2' {
    capture drop _miss`v'
    capture gen byte _miss`v' = missing(`v')
}
* (opcional) exige no-missing en la versión que vas a correr:
keep if !missing(`ctrls1')
xtset id_ent t

*=====================*
*  A) SAR-FE (sin t)  *
*=====================*
spxtregress Inciden `keyvar' `ctrls1', fe dvarlag(wrn)
est store SAR_enferm
estat impact    // en tu build: sin opciones

*=====================*
*  B) SEM-FE (sin t)  *
*=====================*
spxtregress Inciden `keyvar' `ctrls1', fe errorlag(wrn)
est store SEM_enferm
* (no hay impactos para SEM)

*=====================*
*  C) SDM-FE (sin t)  *
*=====================*
spxtregress Inciden `keyvar' `ctrls1', fe dvarlag(wrn) ///
    ivarlag(wrn: `keyvar' `ctrls1')
est store SDM_enferm
estat impact    // sin opciones

*===========*
*  Extra:   *
*===========*
* (Comparar hosp vs enferm en SAR)
spxtregress Inciden `keyvar' `ctrls2', fe dvarlag(wrn)
est store SAR_hosp
estat impact

* (Tabla comparativa rápida: β de la focal y ρ)
esttab SAR_enferm SEM_enferm SDM_enferm SAR_hosp, ///
  keep(`keyvar' wrn:Inciden) ///
  coeflabels(`keyvar' "β (`keyvar')" "wrn:Inciden" "ρ") ///
  stats(N ll, labels("N" "Log Lik.")) ///
  b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) compress

restore




**mi propuesta
*RE o EF con dummies de año + impactos
*dvar se refiere a SAR
spxtregress Inciden GiniT_i escolh escolm gtosal Verdulerias enferm, fe dvarlag(wrn) vce(oim)  
estat impact

spxtregress Inciden ginesp_i escolh escolm gtosal Verdulerias enferm, fe dvarlag(wrn) vce(oim)
estat impact

spxtregress Inciden giniesp_p escolh escolm gtosal Verdulerias enferm, fe dvarlag(wrn) vce(oim)
estat impact

spxtregress Inciden dessalud_i escolh escolm gtosal Verdulerias enferm, fe dvarlag(wrn) vce(oim)
estat impact


spxtregress Inciden GiniT_p escolh escolm gtosal Verdulerias enferm i.t, re dvarlag(wrn) vce(oim)
estat impact


spxtregress Inciden dessalud_p  escolh escolm gtosal Verdulerias enferm i.t, re dvarlag(wrn) vce(oim)
estat impact


*SEM
spxtregress Inciden GiniT_i escolh escolm gtosal Verdulerias enferm, fe errorlag(wrn) vce(oim) 
estat impact

spxtregress Inciden GiniT_p escolh escolm gtosal Verdulerias enferm i.t, re errorlag(wrn) vce(oim)
estat impact

spxtregress Inciden ginesp_i escolh escolm gtosal Verdulerias enferm, fe errorlag(wrn) vce(oim)
estat impact

spxtregress Inciden giniesp_p escolh escolm gtosal Verdulerias enferm, fe errorlag(wrn) vce(oim)
estat impact

spxtregress Inciden dessalud_i escolh escolm gtosal Verdulerias enferm, fe errorlag(wrn) vce(oim)
estat impact

spxtregress Inciden dessalud_p escolh escolm gtosal Verdulerias enferm i.t, re errorlag(wrn) vce(oim)
estat impact


*SDM
* SDM multibivariada FE (sin dummies de tiempo)
spxtregress Inciden GiniT_i escolh escolm gtosal Verdulerias enferm, fe dvarlag(wrn) ivarlag(wrn: GiniT_i escolh escolm gtosal Verdulerias enferm) vce(oim)
estat impact

spxtregress Inciden GiniT_p escolh escolm gtosal Verdulerias enferm i.t, re dvarlag(wrn) ivarlag(wrn: GiniT_p escolh escolm gtosal Verdulerias enferm) vce(oim)
estat impact

spxtregress Inciden ginesp_i escolh escolm gtosal Verdulerias enferm, fe dvarlag(wrn) ivarlag(wrn: ginesp_i escolh escolm gtosal Verdulerias enferm) vce(oim)
estat impact

spxtregress Inciden giniesp_p escolh escolm gtosal Verdulerias enferm, fe dvarlag(wrn) ivarlag(wrn: giniesp_p escolh escolm gtosal Verdulerias enferm) vce(oim)
estat impact

spxtregress Inciden dessalud_i escolh escolm gtosal Verdulerias enferm, fe dvarlag(wrn) ivarlag(wrn: dessalud_i escolh escolm gtosal Verdulerias enferm) vce(oim)
estat impact

spxtregress Inciden dessalud_p escolh escolm gtosal Verdulerias enferm i.t, re dvarlag(wrn) ivarlag(wrn: dessalud_p escolh escolm gtosal Verdulerias enferm) vce(oim)
estat impact



**comparativos para identificar los mejores modelos de los regresores cuando son multivariadas:
*****************************************
* SAR-RE con años
spxtregress Inciden GiniT_p escolh escolm gtosal Verdulerias enferm i.t, re dvarlag(wrn) vce(oim)
est store SAR_RE

* SDM-RE con WX(GiniT_p)
spxtregress Inciden GiniT_p escolh escolm gtosal Verdulerias enferm i.t, re dvarlag(wrn) ivarlag(wrn: GiniT_p) vce(oim)
est store SDM_RE

**comparaciones de modelo (LR, AIC/BIC) con vce(oim).
lrtest SDM_RE SAR_RE
esttab SAR_RE SDM_RE, ///
  keep(GiniT_p wrn:Inciden wrn:GiniT_p) ///
  coeflabels(GiniT_p "β(GiniT_p)" wrn:Inciden "ρ" wrn:GiniT_p "WX(GiniT_p)") ///
  stats(ll N, labels("Log Lik." "N")) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) compress
** Tras cada modelo, relacion AIC/BIC
est restore SAR_RE
scalar AIC_SAR = -2*e(ll) + 2*e(k)
scalar BIC_SAR = -2*e(ll) + ln(e(N))*e(k)
est restore SDM_RE
scalar AIC_SDM = -2*e(ll) + 2*e(k)
scalar BIC_SDM = -2*e(ll) + ln(e(N))*e(k)
display AIC_SAR AIC_SDM
display BIC_SAR BIC_SDM
*y testear En el SDM el spillover
test [wrn]GiniT_p = 0
estat moran, errorlag(wrn)   // autocorrelación espacial de los residuos


*comparar Moran
*si modelo es efectos fijos
* tras: spxtregress ... , fe dvarlag(wrn)
predict double yhat_til, rform
bys id_ent: egen inc_bar = mean(Inciden)
gen double inc_within = Inciden - inc_bar
gen double e_within   = inc_within - yhat_til

preserve
collapse (mean) e_cs = e_within, by(id_ent)
spset id_ent, replace
estat moran e_cs, errorlag(wrn)
restore




*si modelo es efectos aleatorios
* tras: spxtregress ... , re dvarlag(wrn)
predict double yhat, rform              // predicción de y
* 1) Residuo a nivel i,t
predict double yhat, rform
gen double e_it = Inciden - yhat

* 2) Colapsa a 32 estados (residuo medio por estado)
preserve
collapse (mean) e_cs = e_it, by(id_ent)

* 3) Re-activar el contexto espacial (recrea _ID)
spset id_ent, replace

* 4) Moran-I sobre el residuo cross-section (no hace falta regress)
estat moran e_cs, errorlag(wrn)

restore



*************************





* (opcional) restaurar orden original
sort __ord
drop __ord



levelsof id_ent, local(EST)
tempname results
postfile `results' str20 id b_verds b_dsal using jackknife.dta, replace

foreach e of local EST {
    preserve
    keep if id_ent!="`e'"
    spxtregress Inciden dessalud_i escolh escolm gtosal Verdulerias enferm, fe dvarlag(wrn)
    estat impact
    matrix M = r(table)
    local bV = M["b","Verdulerias"]   // directo (ajusta el índice si tomas total)
    local bD = M["b","dessalud_i"]
    post `results' ("`e'") (`bV') (`bD')
    restore
}
postclose `results'
use jackknife.dta, clear
summ

