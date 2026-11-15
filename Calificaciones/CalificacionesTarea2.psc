Proceso CalificacionesTarea2
	// Nombre del archivo: CalificacionesTarea2
	// Descripci�n: Algoritmo que calcula las calificai�nes de los estudiantes y los clasifica
	// Autores: Camila Galindo P�ramo y Victor Ailem Martinez Martinez 
	// Fecha: 16/09/2026 
	//Practica de Introduccion
	
	Definir Estudiante, EstudiantesC, ContadorSobre, ContadorRegulares, ContadorRescatar, ContadorR Como Entero
	Definir Examen1, Examen2, Examen3, SumatoriaE, PromedioAct, Sobresaliente, Suficiente Como Real
	
	ContadorSobre <- 0
	ContadorRegulares <- 0
	ContadorRescatar <- 0
	ContadorR <- 0
	
	Escribir "---------------------------------------------------------------------------------------------------------------------------"
	Escribir "Algoritmo que ayuda a calcular cu�l es la calificaci�n que se necesita para aprobar, al igual que los clasifica y promedia."
	Escribir "---------------------------------------------------------------------------------------------------------------------------"
	Escribir SinSaltar "Ingrese la cantidad de estudiantes: "
	Leer EstudiantesC
	
	Mientras EstudiantesC <= 0 Hacer
		Escribir "Error: Se debe ingresar un n�mero entero mayor a 0."
		Escribir SinSaltar "Por favor, ingrese la cantidad de estudiantes nuevamente: "
		Leer EstudiantesC
	FinMientras
	
	Estudiante <- 1
	
	Mientras Estudiante <= EstudiantesC Hacer
		Escribir ""
		Escribir "Registro de estudiante ", Estudiante, " de ", EstudiantesC
		Escribir ""
		
		Escribir Sin Saltar "Ingrese la calificaci�n del Examen 1 (0 a 100): "
		Leer Examen1
		Mientras Examen1 < 0 O Examen1 > 100 Hacer
			Escribir "Error: La calificaci�n debe estar entre 0 y 100"
			Escribir Sin Saltar "Ingrese la calificaci�n del Examen 1 nuevamente: "
			Leer Examen1
		FinMientras
		
		Escribir Sin Saltar "Ingrese la calificaci�n del Examen 2 (0 a 100): " 
		Leer Examen2
		Mientras Examen2 < 0 O Examen2 > 100 Hacer
			Escribir "Error: La calificaci�n debe estar entre 0 y 100"
			Escribir Sin Saltar "Ingrese la calificaci�n del Examen 2 nuevamente: " 
			Leer Examen2
		FinMientras
		
		Escribir Sin Saltar "Ingrese la calificaci�n del Examen 3 (0 a 100): " 
		Leer Examen3
		Mientras Examen3 < 0 O Examen3 > 100 Hacer
			Escribir "Error: La calificaci�n debe estar entre 0 y 100"
			Escribir Sin Saltar "Ingrese la calificaci�n del Examen 3 nuevamente: " 
			Leer Examen3
		FinMientras
		
		SumatoriaE <- Examen1 + Examen2 + Examen3
		
		// Se redondea al entero que este m�s cerca 
		PromedioAct <- trunc((SumatoriaE / 3) + 0.5)
		
		Sobresaliente <- 360 - SumatoriaE
		Suficiente <- 240 - SumatoriaE
		
		Escribir ""
		Escribir "Resultado del estudiante ", Estudiante
		Escribir ""
		Escribir "Promedio actual: ", PromedioAct
		
		//Se evalua para la meta de 90
		Si Sobresaliente <= 0 Entonces
			Escribir "Calificaci�n necesaria para promedio 90: 0 (Meta alcanzable)"
		Sino
			Si Sobresaliente <= 100 Entonces
				Escribir "Calificaci�n necesaria para promedio 90: ", Sobresaliente, " (Meta alcanzable)"
			Sino
				Escribir "Calificaci�n necesaria para promedio 90: ", Sobresaliente, " (Meta NO alcanzable)"
			FinSi
		FinSi
		
		//Se evalua para la meta de 60
		Si Suficiente <= 0 Entonces
			Escribir "Calificaci�n necesaria para promedio 60: 0 (Meta alcanzable)"
		Sino
			Si Suficiente <= 100 Entonces
				Escribir "Calificaci�n necesaria para promedio 60: ", Suficiente, " (Meta alcanzable)"
			Sino
				Escribir "Calificaci�n necesaria para promedio 60: ", Suficiente, " (Meta NO alcanzable)"
			FinSi
		FinSi
		
		//Se categoriza
		Si Sobresaliente <= 100 Entonces
			Escribir "Categor�a: Sobresaliente potencial"
			ContadorSobre <- ContadorSobre + 1
		Sino
			Si Suficiente > 100 Entonces
				Escribir "Categor�a: Reprobado inevitable"
				ContadorR <- ContadorR + 1
			Sino
				Si PromedioAct < 60 Entonces
					Escribir "Categor�a: Puede rescatar el semestre"
					ContadorRescatar <- ContadorRescatar + 1
				Sino
					Escribir "Categor�a: Rendimiento regular potencial"
					ContadorRegulares <- ContadorRegulares + 1
				FinSi
			FinSi
		FinSi
		
		Estudiante <- Estudiante + 1
	FinMientras
	
	Escribir "--------------------------------------------------------"
	Escribir "RESULTADO GENERAL DE LA CLASE"
	Escribir ""
	Escribir "Estudiantes con posibilidad de promedio Sobresaliente: ", ContadorSobre
	Escribir "Estudiantes con posibilidad de Rendimiento Regular: ", ContadorRegulares
	Escribir "Estudiantes que pueden rescatar el semestre: ", ContadorRescatar
	Escribir "Estudiantes reprobados inevitablemente: ", ContadorR
	
FinProceso