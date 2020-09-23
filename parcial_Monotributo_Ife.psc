//////////////////// DEVUELVE UN MENSAJE DE APROBACION O RECHAZO SEGUN MOTIVO //////////////////
Funcion mostrarMensaje(aprobado)
	
	Si aprobado == 0 Entonces
		Escribir "SOLICITUD APROBADA"
	FinSi
	Si aprobado == 1 Entonces
		Escribir "NO APROBADO X CATEGORIA"
	FinSi
	Si aprobado == 2 Entonces
		Escribir "NO APROBADO X AFIP"
	FinSi
	Si aprobado == 3 Entonces
		Escribir "NO APROBADO X RESIDENCIA"
	FinSi
	Si aprobado == 4 Entonces
		Escribir "NO APROBADO X RANGO EDAD"
	FinSi
	
Fin Funcion

////////////////////////// VALIDA SI LA SOLICITUD ESTA APROBADA O NO ///////////////////////

Funcion aprobado <- validaAprobacion(validaCat, validaEdad, apruebaAfip, validaResidencia)
	definir aprobado Como Entero
	aprobado = 0
	
	Si validaCat == 1 Entonces
		aprobado <- 1
	SiNo
		Si validaCat == 0 & apruebaAfip == 2 Entonces
			aprobado <- 2
		SiNo 
			Si validaCat == 0 & apruebaAfip == 0 & validaResidencia == 3 Entonces
				aprobado <- 3
			Sino 
				Si validaCat == 0 & apruebaAfip == 0 & validaResidencia == 0 & validaEdad == 4 Entonces
					aprobado <- 4
				SiNo 
					aprobado <- 0
				FinSi
			FinSi
		FinSi
	Fin Si
	
Fin Funcion

///////////////// GUARDA AL DE MENOR EDAD ///////////////

Funcion menor <- validaMenor(edad, menor)

	Si menor > edad Entonces
		menor = edad	
	FinSi
	
Fin Funcion

//////////////////// VALIDA SI RANGO DE EDAD CORRESPONDE ///////////////////////

Funcion validaEdad <- validacionEdad(edad, validaCat, apruebaAfip, validaResidencia)
	definir validaEdad Como Entero
	
	Si validaCat == 0 & apruebaAfip == 0 & validaResidencia == 0 Entonces
		Si edad >= 18 & edad <= 65 Entonces
			validaEdad <- 0
		SiNo
			validaEdad <- 4
		Fin Si
	FinSi 
	
Fin Funcion

///////////////////////// VALIDA SI NACIONALIDAD ES ARGEN O SI RESIDENCIA ES MAYOR A 2 AÑOS /////////////////////

Funcion validaResidencia <- validacionResidencia(residencia, nacionalidad, validaCat, apruebaAfip)
	definir validaResidencia Como Entero
	
	Si validaCat == 0 & apruebaAfip == 0 Entonces
		Si residencia >= 2 | nacionalidad == "ARGEN" Entonces
			validaResidencia <- 0
		SiNo
			validaResidencia <- 3
		Fin Si
	FinSi
	
Fin Funcion

///////////////// VALIDA SI AFIP APRUEBA O NO SEGUN VALOR LOGICO DE VARIABLE AFIP ////////////////////

Funcion apruebaAfip <- validacionAfip(validaCat, afip)
	definir apruebaAfip Como Entero
	
	Si validaCat == 0 Entonces
		Si afip = Verdadero Entonces
			apruebaAfip <- 0
		SiNo
			apruebaAfip <- 2
		Fin Si
	FinSi
	
Fin Funcion

////////////////// VALIDA EL DATO DE CATEGORIA ///////////////

Funcion validaCat <- validacionCategoria(getCateg)
	definir validaCat Como Entero
	
	Si getCateg == "A" | getCateg == "B" Entonces
		validaCat <- 0
	SiNo
		validaCat <- 1
	Fin Si
	
Fin Funcion

/////////////// ASIGNA VALOR LOGICO A LA VARIABLE PARA PRUEBA DE SISTEMA ////////////

Funcion afip <- validaAfip(cuit)
	definir afip Como Logico
	
	afip = Verdadero
	
Fin Funcion

//////////// ASIGNA A,B,C,ETC A LA CATEGORIA PARA PRUEBA DE SISTEMA /////////////

Funcion getCateg <- categoria(cuit)
	definir getCateg Como caracter

	getCateg = "A"
	
Fin Funcion

////////////////// PIDE LA EDAD  /////////////////////

Funcion edad <- ingresoEdad()
	definir edad Como Entero
	edad = 0

	Escribir Sin Saltar "Ingrese su edad: "
	leer edad
	
	Mientras edad <= 0 Hacer
		Escribir "El valor ingresado es incorrecto."
		Escribir Sin Saltar "Ingrese su edad: "
		leer edad
	FinMientras
	
Fin Funcion

/////////////  SI LA NACIONALIDAD ES EXTRA, PIDE AÑOS DE RESIDENCIA //////////////////

Funcion residencia <- ingresoResidencia(nacionalidad)
	definir residencia Como Real
	residencia = 0
	
	Si nacionalidad <> "ARGEN" Entonces
		Escribir Sin Saltar "Ingrese los años de residencia: "
		leer residencia
	FinSi
	
	Mientras nacionalidad <> "ARGEN" & residencia <= 0 Hacer
		Escribir "El valor ingresado es incorrecto. El valor debe ser mayor a 0(cero)"
		Escribir Sin Saltar "Ingrese los años de residencia: "
		leer residencia
	FinMientras
	
Fin Funcion

/////////////////  PIDE LA NACIONALIDAD, CONVIERTE EL DATO EN MAYUSCULA //////////////////

Funcion nacionalidad <- ingresoNacionalidad()
	definir nacionalidad como cadena
	nacionalidad = ""
	
	Escribir "Ingrese su nacionalidad de acuerdo a las siguientes opciones: "
	Escribir "[ARGEN] si es Argentino"
	Escribir "[EXTRA] si es Extranjero"
	leer nacionalidad
	
	
	Si (Longitud(nacionalidad) <> 5) & (nacionalidad <> "ARGEN") & (nacionalidad <> "EXTRA") Entonces
		Repetir 
			Escribir "El codigo ingresado es incorrecto"
			Escribir "Ingrese su nacionalidad de acuerdo a las siguientes opciones: "
			Escribir "[ARGEN] si es Argentino"
			Escribir "[EXTRA] si es Extranjero"
			leer nacionalidad
		Hasta Que (Longitud(nacionalidad) == 5 | nacionalidad == "ARGEN" | nacionalidad == "EXTRA")
	FinSi
	
	nacionalidad <- Mayusculas(nacionalidad)
	
Fin Funcion

////////// VALIDA SI EL INGRESO FUE UN * PARA FINALIZAR LA CARGA ////////////

Funcion final <- finalCarga(cuit)
	
	Escribir "FIN DE LA CARGA DE DATOS."
	
Fin Funcion

//////// PIDE INGRESO CUIT ///////////////

Funcion cuit <- ingresoCuit()
	definir cuit como cadena
	
	Escribir Sin Saltar "Ingrese su numero de CUIT, sin PUNTOS ni GUIONES: "
	leer cuit
	
	Mientras cuit <> "*" & Longitud(cuit) <> 11 hacer
		Escribir "El CUIT ingresado es incorrecto. Recuerde que debe tener 11 digitos."
		Escribir Sin Saltar "Ingrese su numero de CUIT como numeros sin PUNTOS ni GUIONES: "
		leer cuit
	Fin Mientras 

Fin Funcion

Algoritmo parcialIFE
	definir cuit, nacionalidad, cat como cadena
	definir edad, menor, menorEdad, validaCat, apruebaAfip, validaResidencia, validaEdad, aprobado, solicitudesAprobadas, catidadDeSolicitudes Como Entero
	definir residencia Como Real
	definir final, getCateg Como Caracter
	definir afip Como Logico
	solicitudesAprobadas = 0
	catidadDeSolicitudes = 0
	menorEdad = 18
	
	cuit <- ingresoCuit()
	si cuit == "*" Entonces
		final <- finalCarga(cuit)
	FinSi

	Mientras Longitud(cuit) == 11  Hacer
		
		nacionalidad <- ingresoNacionalidad()
		edad <- ingresoEdad()
		residencia <- ingresoResidencia(nacionalidad)
		
		getCateg <- categoria(cuit)
		afip <- validaAfip(cuit)
		validaCat <- validacionCategoria(getCateg)
		
		apruebaAfip <- validacionAfip(validaCat, afip)
		validaResidencia <- validacionResidencia(residencia, nacionalidad, validaCat, apruebaAfip)
		validaEdad <- validacionEdad(edad, validaCat, apruebaAfip, validaResidencia)
		
		aprobado <- validaAprobacion(validaCat, validaEdad, apruebaAfip, validaResidencia)
		
		Si aprobado == 0 Entonces 
			menorEdad <- validaMenor(edad, menorEdad)
			solicitudesAprobadas = solicitudesAprobadas + 1
		FinSi
		
		catidadDeSolicitudes = catidadDeSolicitudes + 1
		
		mostrarMensaje(aprobado)
		
		cuit <- ingresoCuit()	
		si cuit == "*" Entonces
			final <- finalCarga(cuit)
		FinSi
	Fin Mientras
	
	Si catidadDeSolicitudes > 0 Entonces
        Escribir "La cantidad de solicitudes aprobadas sobre el total es de :", (solicitudesAprobadas * 100)/catidadDeSolicitudes, "%"
        Escribir "El menor en solicitar: ", menorEdad
    FinSi
	
FinAlgoritmo