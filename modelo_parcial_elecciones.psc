Funcion guardarEleccion(cantHabitantes, cantVotos, nivelConocimiento)
	
Fin Funcion

Funcion nivelConocimiento <- nivel()
	definir nivelConocimiento como cadena
	
	Escribir "Ingrese el nivel de conocimiento del partido segun las siguientes opciones: "
	Escribir "[M] Mucho"
	Escribir "[R] Regular"
	Escribir "[P] Poco"
	Leer nivelConocimiento
	
	nivelConocimiento <- Mayusculas(nivelConocimiento)	
	
	Mientras (nivelConocimiento <> "M") & (nivelConocimiento != "R") & (nivelConocimiento <> "P") Hacer
		Escribir "La opcion ingresada es incorrecta."
		Escribir "Ingrese el nivel de conocimiento del partido segun las siguientes opciones: "
		Escribir "[M] Mucho"
		Escribir "[R] Regular"
		Escribir "[P] Poco"
		Leer nivelConocimiento
		nivelConocimiento <- Mayusculas(nivelConocimiento)	
	FinMientras
	
Fin Funcion

Funcion cantVotos <- votos(cantHabitantes)
	definir cantVotos Como Entero
	
	Escribir Sin Saltar "Ingrese la cantidad de votos de la Jurisdicción: "
	leer cantVotos
	
	Mientras cantVotos < 0 | cantVotos > cantHabitantes Hacer
		Escribir "La cantidad de votos no puede ser menor a 0(cero) ni mayor a la cantidad de habitantes de la Jurisdicción"
		Escribir Sin Saltar "Ingrese la cantidad de votos de la Jurisdicción: "
		leer cantVotos
	FinMientras
	
Fin Funcion

Funcion cantHabitantes <- habitantes(MIN_HABITANTES, MAX_HABITANTES)
	definir cantHabitantes Como Entero
	
	Escribir Sin Saltar "Ingrese la cantidad de habitantes de la Jurisdicción: "
	leer cantHabitantes
	
	Mientras  cantHabitantes < MIN_HABITANTES | cantHabitantes > MAX_HABITANTES Hacer
		Escribir "La cifra de habitantes ingresada es INCORRECTA."
		Escribir Sin Saltar "Ingrese la cantidad de habitantes de la Jurisdicción: "
		leer cantHabitantes
	Fin Mientras
	
Fin Funcion

Algoritmo parcialElecciones
	definir MIN_HABITANTES, MAX_HABITANTES, MIN_VOTOS, MAX_VOTOS, cantHabitantes, cantVotos, jurisdiccionesOk, minVotoTotal, cantVotosTotal, i Como Entero
	definir nivelConocimiento como cadena
	definir objetivo Como Real
	
	MIN_HABITANTES = 100
	MAX_HABITANTES = 1000000
//	cantVotos = 0
//	MAX_VOTOS <- cantVotos
	MIN_VOTOS = 0
	jurisdiccionesOk = 0
	minVotoTotal = 1000000
	cantVotosTotal = 0
	
	Para i = 0 Hasta 24 Con Paso 1 Hacer
		cantHabitantes <- habitantes(MIN_HABITANTES, MAX_HABITANTES)
		
		cantVotos <- votos(cantHabitantes)
		
		Si cantVotos < minVotoTotal Entonces
			minVotoTotal <- cantVotos
		FinSi
		cantVotosTotal = cantVotosTotal + cantVotos
		
		nivelConocimiento <- nivel()
		guardarEleccion(cantHabitantes, cantVotos, nivelConocimiento)
		
		objetivo = (cantVotos * 100)/cantHabitantes
		
		Si objetivo > 10 & nivelConocimiento == "P" Entonces
			jurisdiccionesOK = jurisdiccionesOK + 1
		FinSi
		
	Fin Para	
	
	Escribir "La cantidad de Jurisdicciones que cumplieron el objetivo: ", jurisdiccionesOK
	Escribir "La cantidad mínima de votos obtenidos en una Jurisdicción: ", minVotoTotal
	Escribir "La cantidad total de votos obtenida entre todas las Jurisdicciones: ", cantVotosTotal
	
FinAlgoritmo
