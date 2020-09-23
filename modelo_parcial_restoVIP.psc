Funcion comensalesPromedio <- calculaPromedio(contadorComensales, numeroMesa)
	definir comensalesPromedio Como Real
	
	comensalesPromedio = contadorComensales/numeroMesa
	
Fin Funcion

Funcion recargoSegunCuotas <- recargoCuotas(cantidadCuotas)
	
Fin Funcion

Funcion costoFinalMesa<-calculaCostoTotal(costoBase, medioPago, recargoSegunCuotas, RECARGO_DEBITO)
	definir costoFinalMesa Como Real
	
	Si medioPago == "CRE" Entonces
		costoFinalMesa = costoBase
	SiNo
		Si medioPago == "DEB" Entonces
			costoFinalMesa = costoBase + (costoBase*RECARGO_DEBITO)
		SiNo
			costoFinalMesa = costoBase + recargoSegunCuotas
		Fin Si
	Fin Si
	
Fin Funcion

Funcion costoBase<-calculaCostoBase(cantidadCom, COSTO_MENU)
	definir costoBase Como Entero
	
	costoBase = cantidadCom * COSTO_MENU
	
Fin Funcion

Funcion cantidadCuotas <- calculaCuotas(numeroMesa, CUOTA_MIN, CUOTA_MAX)
	definir cantidadCuotas Como Entero
	
	Escribir Sin Saltar "La cantidad de cuotas puede ser entre [1] y [12]. Elija la opción deseada para la mesa ", numeroMesa, ": "
	leer cantidadCuotas
	Mientras cantidadCuotas < 1 | cantidadCuotas > 12 Hacer
		Escribir "La cantidad de cuotas ingresada es incorrecta."
		Escribir Sin Saltar "La cantidad de cuotas puede ser entre [1] y [12]. Elija la opción deseada para la mesa ", numeroMesa, ": "
		leer cantidadCuotas
	FinMientras
	
Fin Funcion

Funcion medioPago <- validaMedio(numeroMesa)
	definir medioPago como cadena
	
	Escribir "Ingrese el medio de pago correspondiente a la mesa ", numeroMesa
	Escribir "Elija la opción correspondiente: "
	Escribir "[EFE] para Efectivo"
	Escribir "[DEB] para Debito"
	Escribir "[CRE] para Credito"
	leer medioPago
	
	medioPago <- Mayusculas(medioPago)
	
	Mientras medioPago <> "EFE" & medioPago <> "DEB" & medioPago <> "CRE" Hacer
		Escribir "La opción ingresada es incorrecta."
		Escribir "Ingrese el medio de pago correspondiente a la mesa ", numeroMesa
		Escribir "Elija la opción correspondiente: "
		Escribir "[EFE] para Efectivo"
		Escribir "[DEB] para Debito"
		Escribir "[CRE] para Credito"
		leer medioPago
	FinMientras
	
	medioPago <- Mayusculas(medioPago)
	
Fin Funcion


Funcion cantidadCom <- calculaCantidad(numeroMesa, COM_MIN, COM_MAX)
	definir cantidadCom Como Entero
	
	Escribir Sin Saltar "Ingrese la cantidad de comensales de la mesales de la mesa ", numeroMesa, " "
	leer cantidadCom
	
	mientras cantidadCom < COM_MIN | cantidadCom > COM_MAX Hacer
		Escribir "La cantidad de comensales no puede ser inferior a 1(uno) ni superior a 4(cuatro)"
		Escribir Sin Saltar "Ingrese la cantidad de comensales de la mesales de la mesa ", numeroMesa, " "
		leer cantidadCom
	FinMientras
	
	
Fin Funcion

Algoritmo restoVIP
	definir cantidadCom, cantidadCuotas, numeroMesa, COM_MIN, COM_MAX, CUOTA_MIN, CUOTA_MAX, COSTO_MENU, i, costoBase, costoMayor, contadorComensales, mesa Como Entero
	definir medioPago, pagoMayor como cadena
	definir costoFinalMesa, recargoSegunCuotas, RECARGO_DEBITO, comensalesPromedio Como Real
	
	COM_MIN = 1
	COM_MAX = 4	
	CUOTA_MIN = 1
	CUOTA_MAX = 12
	COSTO_MENU = 1200
	numeroMesa = 1
	costoMayor = 0
	pagoMayor = " "
	contadorComensales = 0
	mesa = 0
	RECARGO_DEBITO = 0.05
	recargoSegunCuotas = 0.0
	
	Para i = 1 Hasta 3 Con Paso 1 Hacer
		cantidadCom <- calculaCantidad(numeroMesa, COM_MIN, COM_MAX)
		contadorComensales = contadorComensales + cantidadCom
		medioPago <- validaMedio(numeroMesa)
		
		si medioPago == "CRE" Entonces
			cantidadCuotas <- calculaCuotas(numeroMesa, CUOTA_MIN, CUOTA_MAX)	
		FinSi
		
		costoBase<-calculaCostoBase(cantidadCom, COSTO_MENU)
		costoFinalMesa<-calculaCostoTotal(costoBase, medioPago, recargoSegunCuotas, RECARGO_DEBITO)
		
		Si costoMayor < costoFinalMesa Entonces
			costoMayor = costoFinalMesa
			pagoMayor = medioPago
			mesa = numeroMesa
		FinSi
		
		numeroMesa = numeroMesa + 1
		
	Fin Para
	
	comensalesPromedio <- calculaPromedio(contadorComensales, numeroMesa)
	
	Escribir "El costo mayor fue $", costoMayor, " se pago con ", pagoMayor, " y lo realizó la mesa ", mesa
	Escribir "La cantidad de comensales promedio por mesa fue de ", comensalesPromedio
	
FinAlgoritmo
