Funcion promedio <-calculaPromedio(acumuladorVentas, contadorVentas)
	definir promedio Como Real
	
	promedio = acumuladorVentas/contadorVentas 
	
Fin Funcion

Funcion lineaMayorGanancia<-calculaLinea(acumS, acumP, acumN)
	definir lineaMayorGanancia Como cadena
	
	Si acumS > acumP & acumS > acumN Entonces
		lineaMayorGanancia = "Servidores"
	SiNo
		Si acumP > acumS & acumP > acumN Entonces
			lineaMayorGanancia = "PC"
		SiNo
			lineaMayorGanancia = "Notebook"
		Fin Si
	Fin Si
	
Fin Funcion

Funcion getImporteVta<- calculaImporte(getLinea, numeroFactura, cantidad)
	definir getImporteVta Como Real
	
	getImporteVta = Aleatorio(1, 300000)
	
	Escribir "El importe de venta de la factura ", numeroFactura, " es de $", getImporteVta

Fin Funcion

Funcion getLinea <- validaLineaProducto (codigoProducto)
	definir getLinea Como Caracter
	
	Si codigoProducto == "PCS" Entonces
		getLinea = "P"
	SiNo
		Si codigoProducto == "SERVERS" Entonces
			getLinea = "S"
		SiNo
			getLinea = "N"
		Fin Si
	Fin Si
	
Fin Funcion

Funcion cantidad<-ingresoCantidad()
	definir cantidad Como Entero
	
	Escribir "Ingrese la cantidad: "
	leer cantidad
	
	mientras cantidad <= 0 Hacer
		Escribir "[ERROR] La cantidad no puede ser 0(cero) o negativa."
		Escribir "Ingrese la cantidad: "
		leer cantidad
	FinMientras
	
Fin Funcion

Funcion numeroFactura<-cargaFactura()
	definir numeroFactura como cadena
	
	Escribir Sin Saltar "Ingrese el número de la factura correspondiente: "
	leer numeroFactura
	
	mientras Longitud(numeroFactura) <> 15 Hacer
		Escribir "[ERROR] El número de factura debe tener 15 caracteres."
		Escribir Sin Saltar "Ingrese el número de la factura correspondiente: "
		leer numeroFactura
	FinMientras
	
Fin Funcion

Funcion descripcionProducto <-describeProducto(codigoProducto)
	definir descripcionProducto como cadena
	
	Escribir "Ingrese la descripción del producto ", codigoProducto
	leer descripcionProducto
	
	Mientras Longitud(descripcionProducto) < 1 & Longitud(descripcionProducto) > 100 Hacer
		Escribir "[ERROR] La cantidad de caracteres de la descripción no puede ser menor a 1(uno) ni mayor a 100(cien)."
		Escribir " "
		Escribir "Ingrese la descripción del producto ", codigoProducto
		leer descripcionProducto
	FinMientras
	
Fin Funcion

Funcion finCarga <- finCargaProductos(codigoProducto)
	
	Escribir "FIN DE LA CARGA DE DATOS."
	
Fin Funcion

Funcion codigoProducto<-ingresoCodigo()
	definir codigoProducto, finCarga como cadena
	
	Escribir "Ingrese el código del producto según las siguientes opciones: "
	Escribir "[PCS] para la venta de PCs"
	Escribir "[NOTES] para la venta de Notebooks"
	Escribir "[SERVERS] para la venta de servidores"
	leer codigoProducto
	
	codigoProducto<-Mayusculas(codigoProducto)
	
	si codigoProducto == "FIN" Entonces
		finCarga <- finCargaProductos(codigoProducto)
	SiNo
		Mientras Longitud(codigoProducto) < 3 | Longitud(codigoProducto) > 7 Hacer
			Escribir "[ERROR] El código ingresado no puede ser menor a 3(tres) ni mayor a 7(siete)."
			Escribir " "
			Escribir "Ingrese el código del producto según las siguientes opciones: "
			Escribir "[PCS] para la venta de PCs"
			Escribir "[NOTES] para la venta de Notebooks"
			Escribir "[SERVERS] para la venta de servidores"
			leer codigoProducto
		FinMientras
		
		codigoProducto<-Mayusculas(codigoProducto)
		
		Mientras codigoProducto <> "PCS" & codigoProducto <> "NOTES" & codigoProducto <> "SERVERS" Hacer
			Escribir "[ERROR] El código ingresado es INCORRECTO."
			Escribir " "
			Escribir "Ingrese el código del producto según las siguientes opciones: "
			Escribir "[PCS] para la venta de PCs"
			Escribir "[NOTES] para la venta de Notebooks"
			Escribir "[SERVERS] para la venta de servidores"
			leer codigoProducto
		FinMientras
	FinSi
	
	codigoProducto<-Mayusculas(codigoProducto)
	
Fin Funcion

Algoritmo ventaComputadoras
	definir codigoProducto, descripcionProducto, numeroFactura, finCarga, facturaMayor, lineaMayorGanancia como cadena
	definir cantidad, contadorVentas Como Entero
	definir getImporteVta, ventaMayor, acumuladorVentas, acumS, acumP, acumN, promedio Como Real
	definir getLinea, S, P, N Como Caracter
	
	ventaMayor = 0
	contadorVentas = 0
	acumuladorVentas = 0
	acumS = 0
	acumP = 0
	acumN = 0
	facturaMayor = " "
	promedio = 0
	
	codigoProducto<-ingresoCodigo()
	
	Mientras codigoProducto <> "FIN" Hacer
		
		descripcionProducto <-describeProducto(codigoProducto)
		numeroFactura<-cargaFactura()
		cantidad<-ingresoCantidad()
		getLinea <- validaLineaProducto (codigoProducto)
		getImporteVta<- calculaImporte(getLinea, numeroFactura, cantidad)		
		
		Segun getLinea Hacer
			"S":
				acumS = acumS + getImporteVta
			"P":
				acumP = acumP + getImporteVta
			"N":
				acumN = acumN + getImporteVta
		Fin Segun
		
		Si ventaMayor < getImporteVta Entonces
			ventaMayor = getImporteVta
			facturaMayor = numeroFactura
		Fin Si
		
		contadorVentas = contadorVentas + 1
		acumuladorVentas = acumuladorVentas + getImporteVta
		
		codigoProducto<-ingresoCodigo()
		
		promedio <-calculaPromedio(acumuladorVentas, contadorVentas)
	Fin Mientras
	
	lineaMayorGanancia<-calculaLinea(acumS, acumP, acumN)
	
	Escribir "La venta de mayor valor fue $", ventaMayor, " y se hizo en la factura ", facturaMayor
	Escribir "El total ventas fue $", acumuladorVentas
	Escribir "La línea de productos que obtuvo la mayor ganancia fue ", lineaMayorGanancia 
	Escribir "El promedio entre todas las ventas fue $", promedio
	
FinAlgoritmo
