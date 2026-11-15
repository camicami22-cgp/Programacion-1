Algoritmo PrimerExamen
	//Nombre del archivo: DiadelaSemana
	//Descripción: Determinar el día de la semana de una fecha
	//Autor: Camila Galindo Páramo
	//Fecha: 3/09/2026
	
	Definir a, b, c, Dia, Mes, Anio, indicador, resultado, Aux Como Entero
	Definir NombreMes, NombreDia, Palabra Como Caracter
	
	Escribir "Algoritmo que determina el día de la semana en que cayó o caerá ese día en ese mes de ese año."
	Escribir ""
	
	Escribir "1) Enero"
	Escribir "2) Febrero"
	Escribir "3) Marzo"
	Escribir "4) Abril"
	Escribir "5) Mayo"
	Escribir "6) Junio"
	Escribir "7) Julio"
	Escribir "8) Agosto"
	Escribir "9) Septiembre"
	Escribir "10) Octubre"
	Escribir "11) Noviembre"
	Escribir "12) Diciembre"
	Escribir ""
	
	//Especificar el mínimo del año que se puede ingresar//
	Escribir "Ingrese el año (mayor a 1582):"
	Leer Anio
	
	Si Anio <= 1582 Entonces
		Escribir "La fecha no es válida."
	SiNo
		
		Escribir "Ingrese el mes (1-12):"
		Leer Mes
		
		Si Mes < 1 O Mes > 12 Entonces
			Escribir "La fecha no es válida."
		SiNo
			
			Escribir "Ingrese el día:"
			Leer Dia
			
			Aux <- 1
			
			Si Mes = 2 Entonces
				Si (Anio MOD 400 = 0) O ((Anio MOD 4 = 0) Y (Anio MOD 100 <> 0)) Entonces
					Si Dia < 1 O Dia > 29 Entonces
						Aux <- 0
					FinSi
				SiNo
					Si Dia < 1 O Dia > 28 Entonces
						Aux <- 0
					FinSi
				FinSi
			SiNo
				Si Mes = 4 O Mes = 6 O Mes = 9 O Mes = 11 Entonces
					Si Dia < 1 O Dia > 30 Entonces
						Aux <- 0
					FinSi
				SiNo
					Si Dia < 1 O Dia > 31 Entonces
						Aux <- 0
					FinSi
				FinSi
			FinSi
			
			Si Aux = 0 Entonces
				Escribir "La fecha no es válida."
			SiNo
				
				a <- trunc((14 - Mes) / 12)
				b <- Anio - a
				c <- Mes + 12 * a - 2
				
				indicador <- Dia + b + trunc(b / 4) - trunc(b / 100) + trunc(b / 400) + trunc((31 * c) / 12)
				
				resultado <- indicador MOD 7
				
				Segun Mes Hacer
					1:
						NombreMes <- "enero"
					2:
						NombreMes <- "febrero"
					3:
						NombreMes <- "marzo"
					4:
						NombreMes <- "abril"
					5:
						NombreMes <- "mayo"
					6:
						NombreMes <- "junio"
					7:
						NombreMes <- "julio"
					8:
						NombreMes <- "agosto"
					9:
						NombreMes <- "septiembre"
					10:
						NombreMes <- "octubre"
					11:
						NombreMes <- "noviembre"
					12:
						NombreMes <- "diciembre"
				FinSegun
				
				Segun resultado Hacer
					0:
						NombreDia <- "domingo"
					1:
						NombreDia <- "lunes"
					2:
						NombreDia <- "martes"
					3:
						NombreDia <- "miércoles"
					4:
						NombreDia <- "jueves"
					5:
						NombreDia <- "viernes"
					6:
						NombreDia <- "sábado"
				FinSegun
				
				//Determinar si fue, es o será
				Si Anio < 2026 Entonces
					Palabra <- "fue"
				SiNo
					Si Anio > 2026 Entonces
						Palabra <- "será"
					SiNo
						Si Mes < 9 Entonces
							Palabra <- "fue"
						SiNo
							Si Mes > 9 Entonces
								Palabra <- "será"
							SiNo
								Si Dia < 3 Entonces
									Palabra <- "fue"
								SiNo
									Si Dia > 3 Entonces
										Palabra <- "será"
									SiNo
										Palabra <- "es"
									FinSi
								FinSi
							FinSi
						FinSi
					FinSi
				FinSi
				
				Escribir "El ", Dia, " de ", NombreMes, " de ", Anio, " ", Palabra, " ", NombreDia
				
			FinSi
			
		FinSi
		
	FinSi
	
FinAlgoritmo
