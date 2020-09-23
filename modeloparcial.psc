Funcion aprobados <- validaAprobado(cuit, aprobado)
	
	Si cuit <> "*" Entonces
		Si aprobado == 1 Entonces
			Escribir "La solicitud fue Denegada por ser la Categoría errónea."
		SiNo
			Si aprobado == 2 Entonces
				Escribir "La solicitud fue Denegada por no ser Pre Aprobada por la Afip."
			SiNo
				Si aprobado == 3 Entonces
					Escribir "La solicitud fue Denegada por no tener la residencia suficiente."
				SiNo
					Si aprobado == 4 Entonces
						Escribir "La solicitud fue Denegada por estar la edad fuera de rango."
					SiNo
						Escribir " La solicitud fue Aprobada."
					Fin Si
				Fin Si
			Fin Si
		Fin Si
	FinSi
	
Fin Funcion

Funcion aprobado <- validaAprobacion(cuit, validaCat, validaEdad, apruebaAfip, validaResidencia)
	
	definir aprobado Como Entero
	aprobado = 0
	
	Si cuit <> "*" Entonces
		
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
	FinSi	
	
	Escribir aprobado
	
Fin Funcion

///////////////// GUARDA AL DE MENOR EDAD ///////////////

Funcion menorEdad <- validaMenor(cuit, edad, validaCat, validaEdad, apruebaAfip, validaResidencia)
	definir menorEdad Como Entero
	
	menorEdad <- 0
	
	Si cuit <> "*" & validaCat == 0 & apruebaAfip == 0 & validaResidencia == 0 & validaEdad == 0 Entonces
		Si menorEdad < edad Entonces
			menorEdad <- edad
		Fin Si
	FinSi
	
Fin Funcion

//////////////////// VALIDA SI RANGO DE EDAD CORRESPONDE ///////////////////////

Funcion validaEdad <- validacionEdad(cuit, edad, validaCat, apruebaAfip, validaResidencia)
	definir validaEdad Como Entero
	
	Si cuit <> "*" Entonces
		Si validaCat == 0 & apruebaAfip == 0 & validaResidencia == 0 Entonces
			Si edad >= 18 & edad <= 65 Entonces
				validaEdad <- 0
			SiNo
				validaEdad <- 4
			Fin Si
		FinSi 
	FinSi
	
Fin Funcion

///////////////////////// VALIDA SI NACIONALIDAD ES ARGEN O SI RESIDENCIA ES MAYOR A 2 AÑOS /////////////////////

Funcion validaResidencia <- validacionResidencia(cuit, residencia, nacionalidad, validaCat, apruebaAfip)
	definir validaResidencia Como Entero
	
	Si cuit <> "*" Entonces
		Si validaCat == 0 & apruebaAfip == 0 Entonces
			Si residencia == 2 | nacionalidad == "ARGEN" Entonces
				validaResidencia <- 0
			SiNo
				validaResidencia <- 3
			Fin Si
		FinSi
	FinSi
	
Fin Funcion

///////////////// VALIDA SI AFIP APRUEBA O NO SEGUN VALOR LOGICO DE VARIABLE AFIP ////////////////////

Funcion apruebaAfip <- validacionAfip(cuit, validaCat, afip)
	definir apruebaAfip Como Entero
	
	Si cuit <> "*" Entonces
		Si validaCat == 0 Entonces
			Si afip = Verdadero Entonces
				apruebaAfip <- 0
			SiNo
				apruebaAfip <- 2
			Fin Si
		FinSi
	FinSi
	
Fin Funcion

////////////////// VALIDA EL DATO DE CATEGORIA ///////////////

Funcion validaCat <- validacionCategoria(cuit, getCateg)
	definir validaCat Como Entero
	
	Si cuit <> "*" Entonces
		Si getCateg == "A" | getCateg == "B" Entonces
			validaCat <- 0
		SiNo
			validaCat <- 1
		Fin Si
	FinSi
	
Fin Funcion

/////////////// ASIGNA VALOR LOGICO A LA VARIABLE PARA PRUEBA DE SISTEMA ////////////

Funcion afip <- validaAfip(cuit)
	definir afip Como Logico
	
	Si cuit <> "*" Entonces
		afip = Verdadero
	FinSi
	
Fin Funcion

//////////// ASIGNA A,B,C,ETC A LA CATEGORIA PARA PRUEBA DE SISTEMA /////////////

Funcion getCateg <- categoria(cuit)
	definir getCateg Como caracter
	
	Si cuit <> "*" Entonces
		getCateg = "C"
	FinSi
	
Fin Funcion

////////////////// PIDE LA EDAD  /////////////////

Funcion edad <- ingresoEdad(cuit)
	definir edad Como Entero
	edad = 0
	
	Si cuit <> "*" Entonces
		Escribir Sin Saltar "Ingrese su edad: "
		leer edad
	FinSi
	
	Mientras edad <= 0  & cuit <> "*" Hacer
		Escribir "El valor ingresado es incorrecto."
		Escribir Sin Saltar "Ingrese su edad: "
		leer edad
	FinMientras
	
Fin Funcion

/////////////  SI LA NACIONALIDAD ES EXTRA, PIDE AÑOS DE RESIDENCIA //////////////////

Funcion residencia <- ingresoResidencia(cuit, nacionalidad)
	definir residencia Como Real
	residencia = 0
	
	Si cuit <> "*"  & nacionalidad <> "ARGEN" Entonces
		Escribir Sin Saltar "Ingrese los años de residencia: "
		leer residencia
	FinSi
	
	Mientras nacionalidad <> "ARGEN" & residencia <= 0 & cuit <> "*" Hacer
		Escribir "El valor ingresado es incorrecto."
		Escribir Sin Saltar "Ingrese los años de residencia: "
		leer residencia
	FinMientras
	
Fin Funcion

/////////////////  PIDE LA NACIONALIDAD, CONVIERTE EL DATO EN MAYUSCULA //////////////////

Funcion nacionalidad <- ingresoNacionalidad(cuit)
	definir nacionalidad como cadena
	nacionalidad = ""
	
	Si cuit <> "*" Entonces
		Escribir "Ingrese su nacionalidad de acuerdo a las siguientes opciones: "
		Escribir "[ARGEN] si es Argentino"
		Escribir "[EXTRA] si es Extranjero"
		leer nacionalidad
	FinSi
	
	Si (Longitud(nacionalidad) <> 5) & (nacionalidad <> "ARGEN") & (nacionalidad <> "EXTRA") & (cuit <> "*") Entonces
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
	
	Si cuit == "*" Entonces
		Escribir "FIN DE LA CARGA DE DATOS."
	Fin Si
	
Fin Funcion

//////// PIDE INGRESO CUIT ///////////////

Funcion cuit <- ingresoCuit()
	definir cuit como cadena
	
	Escribir Sin Saltar "Ingrese su numero de CUIT, sin PUNTOS ni GUIONES: "
	leer cuit
	
	Mientras Longitud(cuit) <> 11 & cuit <> "*" Hacer
		Escribir "El CUIT ingresado es incorrecto. Recuerde que debe tener 11 digitos."
		Escribir Sin Saltar "Ingrese su numero de CUIT como numeros sin PUNTOS ni GUIONES: "
		leer cuit
	Fin Mientras
	
Fin Funcion

Algoritmo parcialIFE
	definir cuit, nacionalidad, cat, aprobados como cadena
	definir edad, menor, validaCat, apruebaAfip, validaResidencia, validaEdad, aprobado Como Entero
	definir residencia Como Real
	definir final, getCateg Como Caracter
	definir afip Como Logico
	
	cuit <- ingresoCuit()
	final <- finalCarga(cuit)
	nacionalidad <- ingresoNacionalidad(cuit)
	edad <- ingresoEdad(cuit)
	residencia <- ingresoResidencia(cuit, nacionalidad)
	getCateg <- categoria(cuit)
	afip <- validaAfip(cuit)
	validaCat <- validacionCategoria(cuit, getCateg)
	apruebaAfip <- validacionAfip(cuit, validaCat, afip)
	validaResidencia <- validacionResidencia(cuit, residencia, nacionalidad, validaCat, apruebaAfip)
	validaEdad <- validacionEdad(cuit, edad, validaCat, apruebaAfip, validaResidencia)
	menor <- validaMenor(cuit, edad, validaCat, validaEdad, apruebaAfip, validaResidencia)
	aprobado <- validaAprobacion(cuit, validaCat, validaEdad, apruebaAfip, validaResidencia)
	aprobados <- validaAprobado(cuit, aprobado)
	
FinAlgoritmo