*use 
global dir_input"\Users\User\OneDrive - Universidad Nacional de Costa Rica\Desktop\Stata curso\Clase_4_STATA\"
use "$dir_input/BD_replicar_rdos_capitulo5.dta", clear

describe
tab Year, gen(year_) // le estoy pidiendo que por cada a;o me de cree una dummy
**manualmente
gen d2011=0
replace d2011=1 if Year==2011
///lo tengo que hacer para cada a;o
**Crear una global por sectores y a;os, controlar la heterogenidad por sectores y por tiempo, los controlo pero no los capturo

global sectores Sector1 Sector2 Sector3 Sector4 Sector5 Servi_alt
global years year_1-year_4

**guarda las covariables en un global

#delimit
global covariables
ln_emp_lag tam200 ln_edad Exp_bi_lag
grupo_nac publica multinacional
patnum_lag remusup_lag
inv_apl_porc_lag inv_fun_porc_lag destec_porc_lag coopera
fac_cono11_bi_lag fac_cono21_bi_lag  otrofac11_bi_lag otrofac21_bi_lag fac_eco11_bi_lag fac_eco21_bi_lag
;
#delimit cr

***Lo que se quiere conocer es si una empresa recibe ayuda cuanto es el incremento en inversion
*** test de igualdad medias entre grupos grupos ***
**test de medias entre los grupos que recibieron o no el subsidio

ttest ln_emp_lag, by(sub_tot_bi) unequal 
//H0: es que las medias no difieren
///se rezaga porque el subsidio se emplea en base al pasado
//se revisa la prueba del medio, nos fijamos si hay un balance o no, 
//qui es para que no me aparezca el programa en stata

//voy a hacerlo para la covariable del empleo, si quiero con todas es necesario un loop
qui sum ln_emp_lag if sub_tot_bi==1
//ereturn list me dice como se llama la media y la desviacion en stata
qui local mean1=`r(mean)'
qui local n1=`r(N)'
qui local sd1=`r(sd)'
qui sum ln_emp_lag if sub_tot_bi==0
qui local mean0=`r(mean)'
qui local n0=`r(sum_w))'
qui local sd0=`r(sd)'
ttesti `n1' `mean1' `sd1' `n0' `mean0' `sd0'      /*considerando varinzas iguales*/
ttesti  `n0' `mean0' `sd0' `n1' `mean1' `sd1', une /*considerando varinzas desiguales*/

 
******** Análisis del sesgo de selección - dos grupos muy diferentes *********
******** test de medias para el grupo de variables *********

mat def AA=[9999]

local j=2 
while `j'<3 {
foreach i of global covariables {
qui sum `i' if sub_tot_bi==1
qui local mean1=`r(mean)'
qui local n1=`r(N)'
qui local sd1=`r(sd)'
qui sum `i' if sub_tot_bi==0
qui local mean0=`r(mean)'
qui local n0=`r(sum_w))'
qui local sd0=`r(sd)'
qui ttesti `n1' `mean1' `sd1' `n0' `mean0' `sd0', une
mat AA=AA\r(p)

qui local j = `j' + 1
	}
}
******* p-value del test de medias ******* Los resultados del Loop
mat rownames AA=mean_test $covariables
mat list AA, format(%4.3f)

putexcel set mean_test, sheet(antes_matching, replace) replace /* fija el fichero al que se exportara la informacion*/

putexcel B2=mat(AA),  overwr names nformat(number_d2) 


**
kdensity ln_emp_lag if sub_tot_bi==1, lcolor(blue) lpattern(solid) addplot((kdensity ln_emp_lag if sub_tot_bi==0, lcolor(purple) lpattern(longdash))) 
//addplot es para a;adir en el comando
//buscamos que las diferencias se acorten 

*** test de igualdad de distribución ****** otra prueba de medias
*ksmirnov ln_emp_lag, by(sub_tot_bi) exact /*especifica que se calcula el pvalor-exact*/
ksmirnov ln_emp_lag, by(sub_tot_bi)  /*especifica que se calcula el pvalor-exact*/


**************************************************
********* PROPENSITY SCORE MATCHING **************
**************************************************
**Paso 0: Teste de medias de las covariables 

** PASO 1: CALCULO DEL PROPENSITY SCORE

*** TABLA con efectos marginales, porque es lo que se presenta
**INCLUYE Los efectos 
probit sub_tot_bi $sectores $years, vce(cluster ident) //hazme el robusto por cluster pero por individuos, me asegura un mayor control de la heterocedasticidad 
margins, dydx(*) atmeans post
estimates store model1

probit sub_tot_bi $covariables $years, vce(cluster ident)
margins, dydx(*) atmeans post
estimates store model2

probit sub_tot_bi $covariables $sectores $years, vce(cluster ident)
margins, dydx(*) atmeans post
estimates store model3
///post es para guardar 
*ssc install coefplot, replace
*coefplot model1 model3 , drop(_cons) xline(0)

esttab model1 model2 model3 using "dydx_probit.xls", se(3) b(3) mtitles nonumbers pr2 /*wide noparentheses*/  title(Average Marginal Effects) replace compress nogaps order($covariables $sectores $years)
//cambiar la direccion del directorio
 ///me da problemas para exportar porque me pone cosas en una misma fila
///me fijo cuales son significativas 
esttab model1 model2 model3, se(3) b(3) mtitles nonumbers pr2 

*** predicción del PROPENSITY SCORE y análisis
//se utiliza el mejor modelo
//obtener los valores estimados, del mejor modelo con predic
probit sub_tot_bi $covariables $sectores $years, vce(cluster ident)
*cap noi drop pscore
predict pscore, p
//para comprobar el commun support
sum pscore if sub_tot_bi==0
local max_control=r(max)
local min_control=r(min)
sum pscore if sub_tot_bi==1
local max_treat=r(max)
local min_treat=r(min)

//Le a;adimos el prpoensity score
***No me funciona
kdensity pscore if sub_tot_bi==1, lcolor(blue) lpattern(solid) addplot((kdensity pscore if sub_tot_bi==0, lcolor(purple) lpattern(longdash))) xline(`max_control' `min_treat', lpattern(solid/*dot*/) lwidth(medium/*thick*/)  lcolor(green)) xmtick(-0.2(0.2)1.2)

//el cummun suppor dice que para cada grupo de control tenga un grupo de tratamiento, y me aseguro que cumpla la independencia

//dos graficos en un solo grafico, es un grafico de dispersion 
twoway (scatter gid_ventas pscore if sub_tot_bi==1) (scatter gid_ventas pscore if sub_tot_bi==0) , ytitle(gid_ventas) //
xtitle(Propensity Score)
//se observa que la parte de azul es las ventas para el grupo de tratamiento =1, y la de rojo para el grupo de control=0, se puede observar el soporte comun, y tambien se observan los emparejamientos
//el propensity score te da la probabilidad de que recibas ayuda aunque no la tengas
//con el grafico se puede apreciar esas observaciones que se pueden unir con el metodo de vecino mas cercano 
//el caliper hace circulos y te va a buscar valores cercanos dentro de esos circulo
//ademas se puede utilizar un control mas de una vez


twoway (scatter gid_ventas pscore if sub_tot_bi==1 & gid_ventas>=4) (scatter gid_ventas pscore if sub_tot_bi==0 & gid_ventas>=4) , ytitle(gid_ventas) xtitle(Propensity Score)

*** test de igualdad de distribución ******
*ksmirnov pscore, by(sub_tot_bi) exact /*especifica que se calcula el pvalor-exact*/
ksmirnov pscore, by(sub_tot_bi)  /*especifica que se calcula el pvalor-exact*/



*** PASO 2: MATCHING

ssc install kmatch, replace
ssc install psmatch2, replace

////Propensity score 
 ***** A). Matching basico - NNM(1) un control - CON reemplazamiento 
 
**** TEFFECTS ** se altera solo un vecino cercano
teffects psmatch (gid_ventasw1) (sub_tot_bi $covariables $sectores $years , probit), atet
**** PSMATCH2 ** se altera el numero de vecinos mas cercanos PROBIT
psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1) n(1)  /* con desviaciones típicas normales*/
*bootstrap r(att) : psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1)   /* desviaciones típicas mediante bootstrap*/
**** KMATCH ** se recomienda utilizar este comando porque tienes mas opciones para personalizar, LOGIT
kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) 
*kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) vce(bootstrap, r(50))
//bootstrap es repetir el modelo con distintas muestras, y de ahi encontrar un error estandar ponderado, generalmente se hace siempre 
kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) 
kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) pscmd(probit) //le digo que lo estimo por probit

 

***** B). Matching Soporte común - NNM(1) - CON reemplazamiento //estamos haciendo emparejamientos, aplicamos el commun support, se recomienda usarlo siempre
//te asegura que siempre va a haber un control para cada tratamiento y se cumple el supuesto de independiencia

**** TEFFECTS **
teffects psmatch (gid_ventasw1) (sub_tot_bi $covariables $sectores $years , probit), atet
//me indica cual es elfecto final, del subsidio en las ventas 
//atet es el avearage treatment effect

 /*no permite*/ 
teffects psmatch (gid_ventasw1) (sub_tot_bi $covariables $sectores $years , probit) if _support==1, atet /*nos apoyamos en otro programa*/ 

**** PSMATCH2 **
psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1) comm  /* con desviaciones típicas normales*/
*bootstrap r(att) : psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1) comm  /* desviaciones típicas mediante bootstrap*/
**** KMATCH **
kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) comsup pscmd(probit) 
//analizamos el att
//nos da resultados diferentes porque lo estima por logit


***** C). Matching Soporte común - NNM(1) - CON reemplazamiento - caliper
//emparejamiento mas riguroso 

**** TEFFECTS **
cap noi teffects psmatch (gid_ventasw1) (sub_tot_bi $covariables $sectores $years , probit) if _support==1, atet cal(0.003) /*problemas con la opcion caliper*/
cap noi teffects psmatch (gid_ventasw1) (sub_tot_bi $covariables $sectores $years , probit) if _support==1, atet cal(0.03) /*hay valores admitidos por el modelo*/
////////
**** PSMATCH2 **
psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1) comm cal(0.003) /* con desviaciones típicas normales*/
//entre mas se acerca a cero mayor presicion

*bootstrap r(att) : psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1) comm cal(0.003) /* desviaciones típicas mediante bootstrap*/
**** KMATCH **
kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) comsup cal(0.003) sh 

////hasta aqui vimos el matching aproximado
///los modelos se interpretan igual

***** D). Matching Soporte común - NNM(1) - CON reemplazamiento - caliper - exact matching
//con el matching exacto perdemos muestra, porque eliminamos a las observaciones que no tienen pareja
//el matchin lo quiero hacer para sectores que pertenezcan a un solo sector,

**** TEFFECTS **
cap noi  teffects psmatch (gid_ventasw1) (sub_tot_bi $covariables $sectores $years , probit) if _support==1, atet cal(0.003) /*no permite, posible en otro tipo de emparejamiento*/ 
**** PSMATCH2 **
psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1) comm cal(0.003) /*no permite*/ 
*bootstrap r(att) : psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1) comm cal(0.003) 
**** KMATCH **
kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) comsup cal(0.003) sh ematch($years $sectores) /*recordar que lo hace con logit
//la variable de tratamiento busca una variable de control que tenga un mismo a;o de fundacion y que pertenezcan al mismo sectores


////Ver si el balance es valido
//Se tiene que hacer el test de medias, tomando en cuenta el valor del propensity score
/*se espera que con el propensity score no hayan diferencias
//se comparan dos muestras exactamente iguales y elimino el sesgo 
//se puede volver hacer el grafico 
//correr de nuevo el modelo probit y ver que no sea significativo, esto me asegura un balance correcto


*** PASO 3: COMPROBAR QUE EL MATCHING ES VALIDO

**** TEFFECTS **
cap noi drop numero_obs_control
teffects psmatch (gid_ventasw1) (sub_tot_bi $covariables $sectores $years , probit) if _support==1, atet /*no permite*/ generate(numero_obs_control)
tebalance sum
tebalance density

PSMATCH2
//una vez con el propensity score stata ya tiene la prueba no es necearia hacarla manual

psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1) comm /*no permite*/ 
pstest $covariables $sectores $years
pstest $covariables $sectores $years, only //me muestra las que no estan balanceadas 
pstest  _pscore, dens both //me muestra un grafico 
//both es antes y despues del match  
//el modelo no esta balanceado porque todavia existen diferencias en las medias,

**** KMATCH **
kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) comsup  sh
kmatch summarize // la primera parte me dice la diferencias y la segunda es si es match exacto o no
kmatch density _pscore




/////////////////////////////////FIN//////////////////////////////////////
*********** PASO 3.A PROBIT DESPUES DEL MATCHING

psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1) comm  /*no permite*/ 
probit sub_tot_bi $covariables $sectores $years [fw=_weight], vce(cluster ident)

kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) comsup  sh gen(_KM) dygen(_W*) replace  
replace _KM_mw=1 if _KM_nc==1
probit sub_tot_bi $covariables $sectores $years [fw=_KM_mw], vce(cluster ident)

kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) comsup cal(0.003) ematch($years $sectores) sh gen(_KM) dygen(_W*) replace  
replace _KM_mw=1 if _KM_nc==1
probit sub_tot_bi $covariables $sectores $years [fw=_KM_mw], vce(cluster ident)

/*
/* CON TEFFECTS NO HYA PONDERACION AUTOMATICA - SE DEBE CONSTRUIR*/

psmatch2 sub_tot_bi $covariables $sectores $years, out(gid_ventasw1)  /*no permite*/ 
probit sub_tot_bi $covariables $sectores $years [fw=_weight], vce(cluster ident)

cap noi drop match1
teffects psmatch (gid_ventasw1) (sub_tot_bi $covariables $sectores $years , probit), atet /*no permite*/ generate(match)

preserve 

predict y0 y1, po
predict te
**_weight
gen ob=_n
save fulldata, replace 
keep if sub_tot_bi 
keep match1 
bysort match1: gen weight=_N 
by match1: keep if _n==1 
ren match1 ob 

merge 1:m ob using fulldata 
replace weight=1 if sub_tot_bi 
probit sub_tot_bi $covariables $sectores $years [fw=weight], vce(cluster ident)
restore
erase fulldata.dta 
*/


*********** PASO 3.B TEST DE MEDIAS ENTRE LOS GRUPOS

kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) comsup  sh gen(_KM) dygen(_W*) replace  
replace _KM_mw=1 if _KM_nc==1
probit sub_tot_bi $covariables $sectores $years [fw=_KM_mw], vce(cluster ident)



kmatch ps sub_tot_bi $covariables $sectores $years (gid_ventasw1), att nn(1) comsup cal(0.003) ematch($years $sectores) sh gen(_KM) dygen(_W*) replace  
replace _KM_mw=1 if _KM_nc==1
probit sub_tot_bi $covariables $sectores $years [fw=_KM_mw], vce(cluster ident)

mat def BB=[9999]

local j=2 
while `j'<3 {
foreach i of global covariables {
qui sum `i' [w=_KM_nc] if sub_tot_bi==1
qui local mean1=`r(mean)'
qui local n1=`r(N)'
qui local sd1=`r(sd)'
qui sum `i' [w=_KM_mw]  if sub_tot_bi==0
qui local mean0=`r(mean)'
qui local n0=`r(sum_w))'
qui local sd0=`r(sd)'
qui ttesti `n1' `mean1' `sd1' `n0' `mean0' `sd0', une
mat BB=BB\r(p)

qui local j = `j' + 1
	}
}
******* p-value del test de medias *******
mat rownames BB=mean_test $covariables
mat list BB, format(%4.3f)

putexcel set mean_test, sheet(despues_matching, replace) replace /* fija el fichero al que se exportara la informacion*/

putexcel B2=mat(BB),  overwr names nformat(number_d2) 


******************************************************************
***** MATCHING CONS DSITINTOS TRATAMIENTO - EXCLUYENTES **********
******************************************************************
global dis_ayudas soloreg solonac soloue reg_nac reg_ue nac_ue reg_nac_ue

foreach v of global dis_ayudas {
di " **************** `v' *************"
kmatch ps `v' $covariables $sectores $years (gid_ventas_netw1), att comsup cal(0.001) nn(1)
}


foreach v of global dis_ayudas {
di " **************** `v' *************"
/*bootstrap r(att) : psmatch2 `v' $covariables $sectores $years , logit out(gid_ventas_netw1) cal(0.001)*/
psmatch2 `v' $covariables $sectores $years , logit out(gid_ventas_netw1) cal(0.001)
pstest $covariables
}


*****************


psmatch2 nac_ue $covariables $sectores $years , logit qui out(gid_ventas_netw1) comm cal(0.01)
pstest $covariables, only
kdensity _pscore [fw=_weight] if sub_tot_bi==1, addplot(kdensity _pscore [fw=_weight] if sub_tot_bi==0) name(cal01, replace) legend(off) nodraw
*
psmatch2 nac_ue $covariables $sectores $years , logit qui out(gid_ventas_netw1) comm cal(0.001)
pstest $covariables, only
kdensity _pscore [fw=_weight] if sub_tot_bi==1, addplot(kdensity _pscore [fw=_weight] if sub_tot_bi==0) name(cal001, replace) legend(off) nodraw
*
psmatch2 nac_ue $covariables $sectores $years , logit qui out(gid_ventas_netw1) comm cal(0.0001)
pstest $covariables, only
kdensity _pscore [fw=_weight] if sub_tot_bi==1, addplot(kdensity _pscore [fw=_weight] if sub_tot_bi==0) name(cal0003, replace) legend(off) nodraw


graph combine cal01 cal001 cal0003

*****************







