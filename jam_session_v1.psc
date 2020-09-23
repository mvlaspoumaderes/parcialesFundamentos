Algoritmo jam_session_v1
	Definir nombre Como Cadena
	Definir edadMusico, cantidadHoras, sumatoriaEdades, totalMusicos Como Entero
	sumatoriaEdades = 0
	totalMusicos = 0
	
	Definir tipoInstrumento, continuarProcesamiento Como Caracter
	Definir costoEntrada, entradaMasCara, edadPromedio Como Real
	entradaMasCara = 0.0
	Definir datoValido Como Logico
	
	Repetir
		Escribir "Ingrese el nombre del músico: "
		Leer nombre
		
		Repetir
			Escribir "Ingrese la edad del músico [entre 18 y 75]"
			Leer edadMusico
			datoValido = validarEdad(edadMusico)
		Hasta Que datoValido
		
		Repetir
			Escribir "Ingrese la cantidad de horas de sesión [entre 1 y 6]"
			Leer cantidadHoras
			datoValido = validarCantidadHoras(cantidadHoras)
		Hasta Que datoValido
		
		Repetir
			Escribir "Ingrese el tipo de instrumento ejecutado por el músico [G/B/D/K]"
			Leer tipoInstrumento
			datoValido = validarTipoInstrumento(tipoInstrumento)
		Hasta Que datoValido
		
		costoEntrada = obtenerCostoEntrada(tipoInstrumento, cantidadHoras, edadMusico)
		
		Si costoEntrada > entradaMasCara Entonces
			entradaMasCara = costoEntrada
		Fin Si
		
		totalMusicos = totalMusicos + 1
		sumatoriaEdades = sumatoriaEdades + edadMusico
		
		Repetir
			Escribir "¿Deseás ingresar otro músico? [S/N]"
			Leer continuarProcesamiento
			datoValido = validarContinuarProcesamiento(continuarProcesamiento)
		Hasta Que datoValido
		
	Mientras Que (continuarProcesamiento == "S") | (continuarProcesamiento == "s")
	
	edadPromedio = calcularPromedioEdadMusicos(sumatoriaEdades, totalMusicos)
	
	Escribir "------------------------------- INFORME -------------------------------"
	Escribir "* La entrada abonada más cara fue $", entradaMasCara
	Escribir "* La edad promedio de los músicos que asistieron a la Jam Session es de ", edadPromedio, " años"
	Escribir "-----------------------------------------------------------------------"
	
FinAlgoritmo


Funcion costoEntrada <- obtenerCostoEntrada(tipoInstrumento, cantidadHoras, edad) 
	Definir VALOR_ENTRADA Como Entero
	VALOR_ENTRADA = 100
	Definir PORCENTAJE_DESCUENTO_BAJITA Como Real
	PORCENTAJE_DESCUENTO_BAJITA = 0.9
	
	Definir costoEntrada Como Real
	
	Segun tipoInstrumento Hacer
		"G":
			costoEntrada = cantidadHoras * VALOR_ENTRADA
		"B":
			costoEntrada = cantidadHoras * (PORCENTAJE_DESCUENTO_BAJITA * VALOR_ENTRADA)
		"D":
			costoEntrada = calcularCostoBasterista(cantidadHoras, VALOR_ENTRADA)
		"K":
			costoEntrada = calcularCostoTecladista(cantidadHoras, VALOR_ENTRADA, edad)
	Fin Segun
	
FinFuncion


Funcion costoBasterista <- calcularCostoBasterista(cantidadHoras, valorEntrada)
	Definir AUMENTO, DOS, horasExtras Como Entero
	AUMENTO = 20
	DOS = 2
	
	Definir costoBasterista Como Real
	
	Si cantidadHoras > DOS Entonces
		horasExtras = cantidadHoras - DOS
		costoBasterista = (horasExtras * (valorEntrada + AUMENTO)) + (valorEntrada * DOS)
	SiNo
		costoBasterista = cantidadHoras * valorEntrada
	Fin Si
FinFuncion


Funcion costoTecladista <- calcularCostoTecladista(cantidadHoras, valorEntrada, edadTecladista)
	Definir EDAD_LIMITE Como Entero
	EDAD_LIMITE = 35
	Definir PORCENTAJE_DESCUENTO Como Real
	PORCENTAJE_DESCUENTO = 0.8
	
	Definir costoTecladista Como Real
	
	Si edadTecladista > EDAD_LIMITE Entonces
		costoTecladista = cantidadHoras * (PORCENTAJE_DESCUENTO * valorEntrada)
	SiNo
		costoTecladista = cantidadHoras * VALOR_ENTRADA
	Fin Si
FinFuncion


Funcion promedio <- calcularPromedioEdadMusicos(sumatoriaEdades, cantidadTotalDeMusicos)
	Definir promedio Como Real
	promedio = sumatoriaEdades / cantidadTotalDeMusicos
FinFuncion


Funcion edadValida <- validarEdad(edad)
	Definir LIMITE_INFERIOR_EDAD, LIMITE_SUPERIOR_EDAD Como Entero
	LIMITE_INFERIOR_EDAD = 18; 
	LIMITE_SUPERIOR_EDAD = 75;
	
	Definir edadValida Como Logico
	edadValida = Falso
	
	Si (edad >= LIMITE_INFERIOR_EDAD) & (edad <= LIMITE_SUPERIOR_EDAD) Entonces
		edadValida = Verdadero
	FinSi
FinFuncion


Funcion horasValidas <- validarCantidadHoras(horas)
	Definir LIMITE_INFERIOR_HORAS, LIMITE_SUPERIOR_HORAS Como Entero
	LIMITE_INFERIOR_HORAS = 1; 
	LIMITE_SUPERIOR_HORAS = 6;
	
	Definir horasValidas Como Logico
	horasValidas = Falso
	
	Si (horas >= LIMITE_INFERIOR_HORAS) & (horas <= LIMITE_SUPERIOR_HORAS) Entonces
		horasValidas = Verdadero
	FinSi
FinFuncion


Funcion instrumentoValido <- validarTipoInstrumento(instrumento)
	Definir instrumentoValido Como Logico
	instrumentoValido = Falso
	
	Si ((instrumento == "G") | (instrumento == "B") | (instrumento == "D") | (instrumento == "K"))Entonces
		instrumentoValido = Verdadero
	FinSi
FinFuncion


Funcion ok <- validarContinuarProcesamiento(continuar)
	Definir ok Como Logico
	ok = Falso
	
	Si ((continuar == "S") | (continuar == "N") | (continuar == "s") | (continuar == "n"))Entonces
		ok = Verdadero
	FinSi
FinFuncion