Algoritmo aeropuerto
	
	Definir codigoDeVuelo, nombre, procedencia, clase, mayorProcedenia Como Caracter
	Definir asiento, miami, rio, madrid, errores, sinError, totalCarga, porcentaje Como Entero
	Definir registro Como Logico
	miami = 0
	rio = 0
	madrid = 0
	errores = 0
	sinError = 0
	
	codigoDeVuelo = ingrsoCodigo()
	
	Mientras codigoDeVuelo <> ""
		nombre = ingresoNombre()
		asiento = ingresoAsiento()
		procedencia = ingresoProcedencia()
		
		clase = obtenerClaseAsiento(asiento)
		registro = registrarEstadistica(procedencia, clase, codigoDeVuelo)
		
		Si !registro Entonces
			errores = errores + 1
			Escribir "No se pudo registrar la estadística para la procedencia ", procedencia, " y la clase de asiento ", clase, " perteneciente al vuelo ", codigoDeVuelo
		SiNo
			sinError = sinError + 1
		Fin Si
		
		Segun procedencia Hacer
			"MI":
				miami = miami + 1
			"RJ":
				rio = rio + 1
			"MA":
				madrid = madrid + 1
		Fin Segun
		
		codigoDeVuelo = ingrsoCodigo()
	FinMientras
	
	totalCarga = errores + sinError
	Si totalCarga > 0 Entonces
		porcentaje = ( (errores * 100) / totalCarga )
		mayorProcedenia = chequeoMayorProcedencia( miami, rio, madrid)
		Escribir "---- INFORME ----"
		Escribir "La mayor cantidad de pasajeros llegaron desde ", mayorProcedenia
		Escribir "Se obtuvieron ", errores, " errores al intentar registrar estadisticas. Esto representa un ", porcentaje, "% de la carga total."
		Escribir "---- INFORME ----"
	FinSi
	
FinAlgoritmo

Funcion codigo <- ingrsoCodigo()
	Definir codigo Como Caracter
	Definir datoValido Como Logico
	Repetir
		Escribir "Ingrese codigo de vuelo"
		Leer codigo
		datoValido = validarCodigo( codigo )
	Mientras Que !datoValido
FinFuncion

Funcion valido <- validarCodigo( codigo )
	Definir valido Como Logico
	valido = Verdadero
	Si ( (Longitud(codigo) < 6 o Longitud(codigo) > 8) y (codigo <> "") ) Entonces
		Escribir "ERROR. El codigo debe contener entre 6 y 8 caracteres"
		valido = Falso
	FinSi
FinFuncion

Funcion nombre <- ingresoNombre()
	Definir nombre Como Caracter
	Definir datoValido Como Logico
	Repetir
		Escribir "Ingrese nombre"
		Leer nombre
		datoValido = validarNombre( nombre )
	Mientras Que !datoValido
FinFuncion

Funcion valido <- validarNombre ( nombre )
	Definir valido Como Logico
	valido = Verdadero
	Si ( Longitud(nombre) < 1 ) Entonces
		Escribir "ERROR. El nombre debe contener al menos un caracter"
		valido = Falso
	FinSi
FinFuncion

Funcion num <- ingresoAsiento()
	Definir num Como Entero
	Definir datoValido Como Logico
	Repetir
		Escribir "Ingrese asiento"
		Leer num
		datoValido = validarAsiento( num )
	Mientras Que !datoValido
FinFuncion

Funcion valido <- validarAsiento( num )
	Definir valido Como Logico
	valido = Verdadero
	Si ( num<1 o num>600 ) Entonces
		Escribir "ERROR. El asiento debe estar entre 1 y 600"
		valido = Falso
	FinSi
FinFuncion

Funcion ciudad <- ingresoProcedencia()
	Definir ciudad Como Caracter
	Definir datoValido Como Logico
	Repetir
		Escribir "Ingrese la ciudad de procedencia"
		Leer ciudad
		ciudad = Mayusculas(ciudad)
		datoValido = validarProcedencia( ciudad )
	Mientras Que !datoValido
FinFuncion

Funcion valido <- validarProcedencia( ciudad )
	Definir valido Como Logico
	valido = Verdadero
	Si ( ciudad<>"MI" y ciudad<>"RJ" y ciudad<>"MA") Entonces
		Escribir "ERROR. Ingrese una ciudad valida"
		valido = Falso
	FinSi
FinFuncion

Funcion clase <- obtenerClaseAsiento( asiento )
	Definir clase Como Caracter
	Si asiento >= 1 y asiento <= 20 Entonces
		clase = "PRI"
	SiNo
		Si asiento > 20 y asiento <= 60 Entonces
			clase = "EJE"
		SiNo
			clase = "TUR"
		Fin Si
	Fin Si
FinFuncion

Funcion valido <- registrarEstadistica( procedencia, clase, vuelo )
	Definir valido Como Logico
	Segun Azar(2) Hacer
		0:
			valido = Falso
		1:
			valido = Verdadero
	Fin Segun
FinFuncion

Funcion ciudad <- chequeoMayorProcedencia( miami, rio, madrid)
	Definir ciudad Como Caracter
	Si ( (miami > rio) y (miami > madrid) ) Entonces
		ciudad = "MI"
	SiNo
		Si ( (rio > miami) y (rio > madrid) ) Entonces
			ciudad = "RI"
		SiNo
			ciudad = "MA"
		Fin Si
	Fin Si
FinFuncion
