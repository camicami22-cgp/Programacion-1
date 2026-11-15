// Nombre del archivo: CalculadoraIMC_Adultos.psc //
// Descripción: Algoritmo que calcula el IMC de una persona adulta e indica su rango de peso //
// Autores: Camila Galindo Páramo y Victor Ailem Martinez Martinez //
// Fecha: 28/08/2026 //

Proceso CalculadoraIMC 
	//Guarda el peso, estatura y el IMC como números con decimal//
	Definir Peso, Estatura, IMC Como real 
	//La edad se guarda como número entero//
	Definir Edad Como Entero 
	
	
	//Le da la bienvenida al usuario y le pide sus datos//
	Escribir "Este programa sirve para calcular tu IMC, por lo que introduce los datos que te solicitamos a continuacion. "
	Escribir Sin Saltar "Introduce tu edad: "
	Leer Edad 
	Escribir Sin Saltar "Introduce tu peso en kilogramos: "
	Leer Peso 
	Escribir Sin Saltar "Introduce tu estatura en metros: "
	Leer Estatura 
	
	//Se checa que el usuario tenga al menos 20 años y que no introduzca valores en 0 o negativos//
	Si Edad >= 20 Y Peso > 0 Y Estatura > 0 Entonces 
	//Se aplica la fórmula para calcular el IMC//
	IMC <- (Peso/Estatura^2) 
	//Se redondea el resultado para que quede en 2 decimales y no dejarlo tan largo//
	IMC <- Redon(IMC * 100) / 100 
	
	//Se evalua los rangos del IMC para su categoría//
	Si IMC < 16.5 Entonces
		Imprimir "Tu IMC es de " IMC " (Bajo de peso severo)."
	FinSi
	
	Si IMC >= 16.5  y IMC < 18.5 Entonces
		Imprimir "Tu IMC es de " IMC " (Bajo de peso)."
	FinSi

	Si IMC >= 18.5 y IMC < 25.0 Entonces 
		Imprimir "Tu IMC es de " IMC " (Peso saludable)."
	FinSi
	
	Si IMC >= 25.0 y IMC < 30.0 Entonces 
		Imprimir "Tu IMC es de " IMC " (Sobrepeso)."
	FinSi
	
	Si IMC >= 30.0 y IMC < 35.0 Entonces 
		Imprimir "Tu IMC es de " IMC " (Obesidad tipo I / moderada)."
	FinSi
	
	Si IMC >= 35.0 y IMC < 40 Entonces 
		Imprimir "Tu IMC es de " IMC " (Obesidad tipo  II / severa)."
	FinSi
	
	Si IMC >= 40 Entonces 
		Imprimir "Tu IMC es de " IMC " (Obesidad tipo III / mórbida)."
	FinSi
	
	SINO 
		//Si ingreso datos inválidos o es menor de 20 años, se muestra el siguiente aviso//
		Imprimir "Para utilizar el servicio debes ser mayor a 20 años e ingresar datos superiores a 0 en tu edad y estatura para calcular tu IMC, intenta nuevamente."
    FinSi 
	
FinProceso
	