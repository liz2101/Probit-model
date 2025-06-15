//Datos panel 
global dir_input"\Users\User\Downloads\"
use "$dir_input\Panel101_new.dta"

**En edit puedo ver si mi base es panel
describe
//Se declara panel
xtset country year
xtline y /*me permite ver la heterogeneidad entre los individuos, en muestras grandes ya no sirve*/

xtsum y x1 /*tenemos 70 observaciones para cada variable, between es la cantidad de individuos, within es la cantidad de a;os*/
//heterogeneidad por between es efectos fijos

reg y x1 x2
estimate store ols //no me esta tomando en cuenta la heterogeneidad

///Estimacion por efectos fijos

xtreg y x1 x2, fe //ahora me contempla el numero de grupos 7
//tenemos distintas R2, se reporta la general
//siempre se reporta ols con efectos fijos
//se debe controlar por el tiempo

xtreg y x1 x2 i.year, fe //i.year me crea una variable dummy para cada a;observaciones, estoy controlando por el tiempo
//los coeficientes 
//como saber si debo incluir el tiempo? stata ya me lo dice con el siguiente comando
xtreg y x1 x2 i.year, fe
testparm i.year //H0:no son necesarias las dummys del tiempo, en este caso se acepta H0
//Vuelvo a la original porque no necesito el tiempo
xtreg y x1 x2, fe
estimates store fe

reg y x1 x2 i.country //utilizo los paises para controlar la heterogeneidad, porque con el tiempo no pude
estimates store LSDV 
esttab fe LSDV //los coeficientes son iguales porque es lo mismo

///Estimando Random Effects
xtreg y x1 x2 //random effects
//vamos a intentar capturar 
xtreg y x1 x2 i.country //para analizar la heterogeneidad entre los paises utilizo random effects, fix effects no me deja ver esa heterogeneidad
//puedo utilizar interacciones
estimate store feb 
xtreg y x1 x2 i.country, fe
estimate store reb
esttab feb reb 
//Prueba de Hausman
xtreg y x1 x2, fe
estimate store fixed
xtreg y x1 x2, re //no es necesario especificarlo
estimate store random
hausman fixed random