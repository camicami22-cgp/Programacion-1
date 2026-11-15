#Nombre del archivo: FigurasTarea4
#Descripción: Algoritmo que imprime una figura deseada por el usuario.
#Autores: Camila Galindo Páramo y Victor Ailem Martinez Martinez 
#Fecha: 04/11/2026

def pedir_dimension():
    #Filtro para solicitar y validar que la dimensión sea un número impar mayor o igual a 5
    while True:
        try:
            n = int(input("Ingresa la dimensión (impar y mayor o igual a 5): "))
            if n >= 5 and n % 2 != 0:
                if n > 15:
                    print("Muy grande, pon una menor o igual a 15 para que quepa bien en la pantalla.")
                    continue
                return n
            else:
                print("Dato inválido. Debe ser un número impar y mínimo de 5.")
        except ValueError:
            print("Error: Tienes que escribir un número entero, no letras.")

def pedir_simbolo():
    #Solicitamos el carácter a utilizar y asegurar que se tome el primer elemento
    simbolo = input("Ingresa el símbolo que quieres usar (ej. *): ")
    if len(simbolo) == 0:
        simbolo = "*"
    return simbolo[0]

def reloj_arena_relleno(n, s):
    print("\n--- Reloj de arena relleno ---")
    espacios_ext = 0
    espacios_int = n
    #Dibuja la mitad superior del reloj de arena
    for i in range(n // 2 + 1):
        print(" " * espacios_ext + s * espacios_int)
        espacios_ext += 1
        espacios_int -= 2
    espacios_ext = n // 2 - 1
    espacios_int = 3
    #Se dibuja la mitad inferior del reloj de arena
    for i in range(n // 2):
        print(" " * espacios_ext + s * espacios_int)
        espacios_ext -= 1
        espacios_int += 2

def reloj_arena_hueco(n, s):
    print("\n--- Reloj de arena hueco ---")
    mitad = n // 2
    print(s * n)
    #Se dibuja la parte superior hueca
    for i in range(1, mitad):
        print(" " * i + s + " " * (n - 2 * i - 2) + s)
    print(" " * mitad + s)
    #Dibuja la parte inferior hueca
    for i in range(mitad - 1, 0, -1):
        print(" " * i + s + " " * (n - 2 * i - 2) + s)
    print(s * n)

def rombo_relleno(n, s):
    print("\n--- Rombo relleno ---")
    mitad = n // 2
    #Mitad superior del rombo
    for i in range(mitad + 1):
        print(" " * (mitad - i) + s * (2 * i + 1))
    #Mitad inferior del rombo
    for i in range(mitad - 1, -1, -1):
        print(" " * (mitad - i) + s * (2 * i + 1))

def rombo_hueco(n, s):
    print("\n--- Rombo hueco ---")
    mitad = n // 2
    print(" " * mitad + s)
    #Mitad superior hueca
    for i in range(1, mitad + 1):
        print(" " * (mitad - i) + s + " " * (2 * i - 1) + s)
    #Mitad inferior hueca
    for i in range(mitad - 1, 0, -1):
        print(" " * (mitad - i) + s + " " * (2 * i - 1) + s)
    print(" " * mitad + s)

def mono_relleno(n, s):
    print("\n--- Moño relleno ---")
    mitad = n // 2
    #Parte superior del moño
    for i in range(mitad + 1):
        estrellas = i + 1
        espacios = n - (2 * estrellas)
        print(s * estrellas + " " * espacios + s * estrellas)
    #Parte inferior del moño
    for i in range(mitad - 1, -1, -1):
        estrellas = i + 1
        espacios = n - (2 * estrellas)
        print(s * estrellas + " " * espacios + s * estrellas)

def mono_hueco(n, s):
    print("\n--- Moño hueco ---")
    mitad = n // 2
    #Parte superior hueca del moño
    for i in range(mitad + 1):
        if i == 0:
            print(s + " " * (n - 2) + s)
        else:
            print(s + " " * (i - 1) + s + " " * (n - 2 * (i + 1)) + s + " " * (i - 1) + s)
    #Parte inferior hueca del moño
    for i in range(mitad - 1, -1, -1):
        if i == 0:
            print(s + " " * (n - 2) + s)
        else:
            print(s + " " * (i - 1) + s + " " * (n - 2 * (i + 1)) + s + " " * (i - 1) + s)

def copa_rellena(n, s):
    print("\n--- Copa rellena ---")
    print(s * n)
    mitad = n // 2
    #Cuerpo de la copa rellena
    for i in range(1, mitad + 1):
        print(" " * i + s * (n - 2 * i))
    tallo_alto = n // 2
    #Tallo y base de la copa
    for i in range(tallo_alto):
        print(" " * (n // 2) + s)
    print(s * n)

def copa_hueca(n, s):
    print("\n--- Copa hueca ---")
    print(s * n)
    mitad = n // 2
    #Cuerpo de la copa hueca
    for i in range(1, mitad + 1):
        espacios_ext = i
        espacios_int = n - 2 - (i * 2)
        if espacios_int >= 0:
            print(" " * espacios_ext + s + " " * espacios_int + s + " " * espacios_ext)
        else:
            print(" " * espacios_ext + s + " " * espacios_ext)
    tallo_alto = n // 2
    #Tallo y base
    for i in range(tallo_alto):
        print(" " * (n // 2) + s)
    print(s * n)

def imprimir_todas(n, s):
    #Se ejecutan todas las funciones de las figuras en secuencia
    reloj_arena_relleno(n, s)
    reloj_arena_hueco(n, s)
    rombo_relleno(n, s)
    rombo_hueco(n, s)
    mono_relleno(n, s)
    mono_hueco(n, s)
    copa_rellena(n, s)
    copa_hueca(n, s)

def main():
    print("--------------------------------------------------------")
    print("Algoritmo que imprime una figura deseada por el usuario.")
    print("--------------------------------------------------------")
    
    #Menú principal interactivo
    while True:
        print("\n====================")
        print("        MENÚ        ")
        print("====================")
        print("1. Reloj de arena relleno")
        print("2. Reloj de arena hueco")
        print("3. Rombo relleno")
        print("4. Rombo hueco")
        print("5. Moño relleno")
        print("6. Moño hueco")
        print("7. Copa rellena")
        print("8. Copa hueca")
        print("9. Imprimir todas")
        print("10. Salir")
        
        opcion = input("Elige una opción (1-10): ")
        
        if opcion == '10':
            print("Saliendo del programa... ¡Adiós!")
            break
            
        elif opcion in ['1', '2', '3', '4', '5', '6', '7', '8', '9']:
            n = pedir_dimension()
            s = pedir_simbolo()
            
            # Condicionales para llamar a la figura seleccionada
            if opcion == '1':
                reloj_arena_relleno(n, s)
            elif opcion == '2':
                reloj_arena_hueco(n, s)
            elif opcion == '3':
                rombo_relleno(n, s)
            elif opcion == '4':
                rombo_hueco(n, s)
            elif opcion == '5':
                mono_relleno(n, s)
            elif opcion == '6':
                mono_hueco(n, s)
            elif opcion == '7':
                copa_rellena(n, s)
            elif opcion == '8':
                copa_hueca(n, s)
            elif opcion == '9':
                imprinter = imprimir_todas(n, s)
        else:
            print("Opción no válida. Inténtalo de nuevo con un número del 1 al 10.")

if __name__ == "__main__":
    main()