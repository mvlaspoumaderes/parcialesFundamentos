Funcion calculaObjetivo <- noObjetivo(contadorTotal, contadorObjetivo)
	definir calculaObjetivo Como Real
	
	calculaObjetivo = (contadorObjetivo * 100)/contadorTotal
	
Fin Funcion

Funcion calcularSueldo<-sueldoTotal(nombreVendedor, gananciaVendedor, calcularBonificacion)
	definir calcularSueldo Como Real
	calcularSueldo = gananciaVendedor + calcularBonificacion
	
	Escribir "El sueldo total del vendedor ", nombreVendedor, " es $", calcularSueldo
	
Fin Funcion

Funcion calcularBonificacion<-bonoExtra(comisionVendedor)
	
Fin Funcion

Funcion gananciaVendedor<-calculaGanancia(cantidadVendida, PURIFICADOR, comisionVendedor, nombreVendedor)
	definir gananciaVendedor Como Real
	
	gananciaVendedor = (cantidadVendida * PURIFICADOR)*(0.01*comisionVendedor)
	
	Escribir "La ganacia del vendedor ", nombreVendedor, " es: $", gananciaVendedor
	
Fin Funcion

Funcion consultaCarga <- continuaCarga()
	definir consultaCarga como cadena
	
	Escribir "¿Deseás ingresar otro vendedor?"
	Escribir Sin Saltar "Ingrese [SI] para continuar o [NO] para salir"
	leer consultaCarga
	
	consultaCarga <- Mayusculas(consultaCarga)
	
	Si consultaCarga == "NO" Entonces
		Escribir "FIN DE LA CARGA DE DATOS."
	FinSi
	
Fin Funcion

Funcion comisionVendedor <- comisionVentas(nombreVendedor, COM_MIN, COM_MAX)
	definir comisionVendedor como entero
	
	Escribir Sin Saltar "Ingrese el procentaje de comisión del vendedor ", nombreVendedor, " "
	leer comisionVendedor
	
	Mientras comisionVendedor < COM_MIN | comisionVendedor > COM_MAX Hacer
		Escribir "El valor ingresado es incorrecto. La comisión no puede ser menor a 4% ni mayor a 7%."
		Escribir Sin Saltar "Ingrese el procentaje de comisión del vendedor ", nombreVendedor, " "
		leer comisionVendedor
	FinMientras
	
Fin Funcion

Funcion cantidadVendida <- ingresoCantidad(nombreVendedor)
	definir cantidadVendida Como Entero
	
	Escribir Sin Saltar "Ingrese la cantidad de purificadores vendida por ", nombreVendedor, " "
	leer cantidadVendida
	
	Mientras cantidadVendida < 0 Hacer
		Escribir "La cantidad de purificadores no puede ser menor a 0(cero)"
		Escribir Sin Saltar "Ingrese la cantidad de purificadores vendida por ", nombreVendedor, " "
		leer cantidadVendida
	FinMientras
	
Fin Funcion

Funcion nombreVendedor <- ingresaNombre()
	definir nombreVendedor como cadena
	
	Escribir Sin Saltar "Ingrese el nombre del vendedor: "
	leer nombreVendedor
	
	mientras Longitud(nombreVendedor) < 3 Hacer
		Escribir "El valor ingresado es incorrecto. El nombre debe contener al menos 3 caracteres."
		Escribir Sin Saltar "Ingrese el nombre del vendedor: "
		leer nombreVendedor
	FinMientras
	
Fin Funcion

Algoritmo parcialPurificadores
	definir nombreVendedor, consultaCarga como cadena
	definir cantidadVendida, PURIFICADOR, COM_MIN, COM_MAX, comisionVendedor, gananciaVendedor, OBJETIVO, contadorTotal, contadorObjetivo como entero
	definir calcularBonificacion, calcularSueldo, totalSueldos, calculaVendedores, calculaObjetivo como real 
	
	PURIFICADOR = 4500
	COM_MIN = 4
	COM_MAX = 7
	OBJETIVO = 100
	totalSueldos = 0
	contadorTotal = 0
	contadorObjetivo = 0

	nombreVendedor<-ingresaNombre()
	cantidadVendida<-ingresoCantidad(nombreVendedor)
	comisionVendedor<-comisionVentas(nombreVendedor, COM_MIN, COM_MAX)
	gananciaVendedor<-calculaGanancia(cantidadVendida, PURIFICADOR, comisionVendedor, nombreVendedor)
	Si cantidadVendida > OBJETIVO Entonces
		calcularBonificacion<-bonoExtra(comisionVendedor)
	SiNo
		Escribir "El vendedor ", nombreVendedor, " no logró el objetivo."
		contadorObjetivo = contadorObjetivo + 1
	FinSi
	calcularSueldo<-sueldoTotal(nombreVendedor, gananciaVendedor, calcularBonificacion)
	totalSueldos = totalSueldos + calcularSueldo
	contadorTotal = contadorTotal +1
	consultaCarga<-continuaCarga()
	
	Mientras consultaCarga == "SI" Hacer
		nombreVendedor<-ingresaNombre()
		cantidadVendida<-ingresoCantidad(nombreVendedor)
		comisionVendedor<-comisionVentas(nombreVendedor, COM_MIN, COM_MAX)
		gananciaVendedor<-calculaGanancia(cantidadVendida, PURIFICADOR, comisionVendedor, nombreVendedor)
		Si cantidadVendida > OBJETIVO Entonces
			calcularBonificacion<-bonoExtra(comisionVendedor)
		SiNo
			Escribir "El vendedor ", nombreVendedor, " no logró el objetivo."
			contadorObjetivo = contadorObjetivo + 1
		FinSi
		calcularSueldo<-sueldoTotal(nombreVendedor, gananciaVendedor, calcularBonificacion)
		totalSueldos = totalSueldos + calcularSueldo
		contadorTotal = contadorTotal + 1
		consultaCarga<-continuaCarga()
	Fin Mientras
	
	calculaObjetivo <- noObjetivo(contadorTotal, contadorObjetivo)
	
	Escribir "El costo total abonado en sueldos es $", totalSueldos
	Escribir "El Porcentaje de vendedores que no logro el objetivo es %", calculaObjetivo	
	
FinAlgoritmo
