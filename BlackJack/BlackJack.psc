//Nombre del archivo:Adivinar un número
//Descripción:DHacer un algoritmo que el usuario adivina un número mágico
//Autor:Chen Huiru 
//Fecha:17/09/2026
Proceso AdivinarUnNumero
	Definir num , numUsuario Como Entero
	
	num <- Aleatorio (0,100) 
	
	Escribir "Adivinar un número mágico"
	Escribir ""
	Repetir 
		Escribir Sin Saltar"Ingresar el número (0 ~100):"
		Leer numUsuario
		
		Mientras numUsuario < 0 Hacer
			Escribir "Error , el número debe ser entre 0 a 100 "
			Escribir Sin Saltar"Ingresar el número (0 ~100):"
			Leer numUsuario
		FinMientras
		Si numUsuario > num Entonces 
			Escribir ""
			Escribir "El número es menor que " , numUsuario
		FinSi
		Si numUsuario < num Entonces 
			Escribir ""
			Escribir "El número es mayor que " , numUsuario
		FinSi
	Hasta Que numUsuario == num  
	
	Escribir ""
	Escribir "El número es " ,numUsuario ".Felicidades , lo adivinaste ^v^ "
	
FinProceso
