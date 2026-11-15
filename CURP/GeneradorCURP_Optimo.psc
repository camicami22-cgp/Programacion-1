// Nombre del archivo: GeneradorCURP_Optimo
// Descripción: Algoritmo que obtiene datos personales y con ello genera la CURP 
// Autores: Camila Galindo Páramo y Victor Ailem Martinez Martinez 
// Fecha: 10/10/2026 



// ----------------------------------------------------------------------------
// FUNCIONES DE LIMPIEZA DE TEXTO Y LETRAS
// ----------------------------------------------------------------------------

// Esta función nos sirve para quitarle los acentos y diéresis a cualquier texto.
// Si el usuario mete "PÉREZ", lo deja como "PEREZ". Es IMPORTANTE para 
// que no nos salgan símbolos raros en la CURP final.
Funcion textoLimpio <- NormalizarTexto(texto)
    Definir textoLimpio, c, temp Como Cadena
    Definir i Como Entero
    textoLimpio <- ""
    Para i <- 1 Hasta Longitud(texto) Hacer
        c <- Subcadena(texto, i, i)
        Segun Mayusculas(c) Hacer
            "Á", "Ä": temp <- "A"
            "É", "Ë": temp <- "E"
            "Í", "Ï": temp <- "I"
            "Ó", "Ö": temp <- "O"
            "Ú", "Ü": temp <- "U"
            De Otro Modo: temp <- Mayusculas(c)
        FinSegun
        // Acuérdate: usamos Concatenar en vez de "+" porque el perfil 
        // estricto cree que queremos sumar números y marca error.
        textoLimpio <- Concatenar(textoLimpio, temp)
    FinPara
FinFuncion

// Validamos si la letra es vocal. Lo hice con Segun en vez de operadores 
// lógicos (como >= o <=) porque a veces la tabla de caracteres da lata.
Funcion es_V <- EvaluarVocal(caracter)
    Definir es_V Como Logico
    Segun Mayusculas(caracter) Hacer
        "A", "E", "I", "O", "U": es_V <- Verdadero
        De Otro Modo: es_V <- Falso
    FinSegun
FinFuncion

// Lo mismo pero para consonantes. Se metio la Ñ porque aunque la CURP la trata 
// distinto después, gramaticalmente sigue siendo consonante.
Funcion es_C <- EvaluarConsonante(caracter)
    Definir es_C Como Logico
    Segun Mayusculas(caracter) Hacer
        "B", "C", "D", "F", "G", "H", "J", "K", "L", "M", "N", "Ñ", "P", "Q", "R", "S", "T", "V", "W", "X", "Y", "Z": es_C <- Verdadero
        De Otro Modo: es_C <- Falso
    FinSegun
FinFuncion

// ----------------------------------------------------------------------------
// REGLAS ESPECÍFICAS DE LA CURP (PREPOSICIONES, NOMBRES COMUNES, ETC)
// ----------------------------------------------------------------------------

// Aquí le quitamos cosas como "DE", "LA", "DEL", "MAC", etc. a los apellidos.
// En lugar de hacer un arreglo gigante que nos crashee la memoria, lo metí 
// en un ciclo que va cortando la palabra si encuentra estas sílabas al inicio.
Funcion textoLimpio <- FiltrarPreposiciones(texto)
    Definir textoLimpio, p3, p4 Como Cadena
    Definir modificado Como Logico
    textoLimpio <- NormalizarTexto(texto)
    
    Repetir
        modificado <- Falso
        Si Longitud(textoLimpio) > 3 Entonces
            p3 <- Subcadena(textoLimpio, 1, 3)
            // Revisa si empieza con prefijos de 2 letras + espacio
            Si p3 = "DA " O p3 = "DE " O p3 = "DI " O p3 = "DD " O p3 = "EL " O p3 = "LA " O p3 = "LE " O p3 = "MC " O p3 = "Y  " Entonces
                textoLimpio <- Subcadena(textoLimpio, 4, Longitud(textoLimpio))
                modificado <- Verdadero
            FinSi
        FinSi
        Si Longitud(textoLimpio) > 4 Entonces
            p4 <- Subcadena(textoLimpio, 1, 4)
            // Revisa si empieza con prefijos de 3 letras + espacio
            Si p4 = "DAS " O p4 = "DEL " O p4 = "DER " O p4 = "DIE " O p4 = "LAS " O p4 = "LOS " O p4 = "LES " O p4 = "MAC " O p4 = "VAN " O p4 = "VON " Entonces
                textoLimpio <- Subcadena(textoLimpio, 5, Longitud(textoLimpio))
                modificado <- Verdadero
            FinSi
        FinSi
    Hasta Que modificado = Falso
    
    // Este While es solo por si quedaron espacios al principio después de limpiar
    Mientras Longitud(textoLimpio) > 1 Y Subcadena(textoLimpio, 1, 1) = " " Hacer
        textoLimpio <- Subcadena(textoLimpio, 2, Longitud(textoLimpio))
    FinMientras
FinFuncion

// La regla dice que si te llamas "MARIA FERNANDA" o "JOSE LUIS", la CURP se
// hace con el segundo nombre. Aquí checamos eso.
Funcion nomValido <- FiltrarNombre(nombres)
    Definir nomValido, nom, p1 Como Cadena
    Definir posEspacio, i Como Entero
    nom <- FiltrarPreposiciones(nombres)
    posEspacio <- 0
    
    // Buscamos dónde está el primer espacio para separar el primer nombre
    Para i <- 1 Hasta Longitud(nom) Hacer
        Si Subcadena(nom, i, i) = " " Y posEspacio = 0 Entonces
            posEspacio <- i
        FinSi
    FinPara
    
    // Si hay dos nombres, checamos si el primero es José, María o sus atajos
    Si posEspacio > 0 Entonces
        p1 <- Subcadena(nom, 1, posEspacio - 1)
        Si p1 = "MARIA" O p1 = "MA" O p1 = "MA." O p1 = "JOSE" O p1 = "J" O p1 = "J." Entonces
            nomValido <- Subcadena(nom, posEspacio + 1, Longitud(nom))
        Sino
            nomValido <- nom // Si no es José/María, usamos el primer nombre normal
        FinSi
    Sino
        nomValido <- nom // Si solo tiene un nombre, se queda ese
    FinSi
    
    nomValido <- FiltrarPreposiciones(nomValido)
FinFuncion

// Funciones para sacar la primera vocal o consonante INTERNA (de la posición 2 en adelante).
// Le puse un condicional para que si la consonante interna es Ñ, me devuelva una X
// (así lo pide la regla oficial de la CURP).
Funcion voc <- VocalInterna(palabra)
    Definir voc, c Como Cadena
    Definir i Como Entero
    voc <- "X" // Valor por defecto si no encuentra nada
    Si Longitud(palabra) >= 2 Entonces
        Para i <- 2 Hasta Longitud(palabra) Hacer
            c <- Subcadena(palabra, i, i)
            Si EvaluarVocal(c) Y voc = "X" Entonces
                voc <- c
            FinSi
        FinPara
    FinSi
FinFuncion

Funcion cons <- ConsonanteInterna(palabra)
    Definir cons, c Como Cadena
    Definir i Como Entero
    cons <- "X"
    Si Longitud(palabra) >= 2 Entonces
        Para i <- 2 Hasta Longitud(palabra) Hacer
            c <- Subcadena(palabra, i, i)
            Si EvaluarConsonante(c) Y cons = "X" Entonces
                Si c = "Ñ" Entonces
                    cons <- "X" // Regla CURP: Ñ interna pasa como X
                Sino
                    cons <- c
                FinSi
            FinSi
        FinPara
    FinSi
FinFuncion

// El filtro de malas palabras. Si las primeras 4 letras forman una palabra
// de esta lista, cambiamos la segunda letra por una X (ej. PUTA -> PXTA).
// Puedes agregarle más palabras al Segun si sientes que faltan.
Funcion segura <- EvitarInconvenientes(codigo)
    Definir segura Como Cadena
    segura <- codigo
    Segun codigo Hacer
        "BACA", "BAKA", "BUEI", "BUEY", "CACA", "CACO", "CAGA", "CAGO", "COGE", "CULO", "FALO", "FETO", "JOTO", "KACA", "LOCO", "MAME", "MEAR", "MOCO", "MULA", "PEDA", "PEDO", "PENE", "PUTA", "PUTO", "RATA", "ROBO", "RUIN", "SENO", "TETA", "VACA", "VAGO":
            // OJO: Anidé los Concatenar para no usar el símbolo +
            segura <- Concatenar(Concatenar(Subcadena(codigo, 1, 1), "X"), Subcadena(codigo, 3, 4))
    FinSegun
FinFuncion

// Diccionario de los estados. Solo mapea el texto que mete el usuario
// a la clave de 2 letras de la RENAPO.
Funcion clave <- ClaveEntidad(entidad)
    Definir clave, ent Como Cadena
    ent <- NormalizarTexto(entidad)
    Segun ent Hacer
        "AGUASCALIENTES": clave <- "AS"
        "BAJA CALIFORNIA": clave <- "BC"
        "BAJA CALIFORNIA SUR": clave <- "BS"
			// ... (resto de estados) ...
        "SONORA": clave <- "SR" // ¡Aquí andamos!
        "VERACRUZ": clave <- "VZ"
        "YUCATAN": clave <- "YN"
        "ZACATECAS": clave <- "ZS"
        De Otro Modo: clave <- "NE" // Nacido en el Extranjero o no reconocido
    FinSegun
FinFuncion

// ----------------------------------------------------------------------------
// BLOQUE PRINCIPAL DEL PROGRAMA
// ----------------------------------------------------------------------------
Algoritmo GeneradorCURP_Optimo
    Definir nombres, apPaterno, apMaterno, entidad, sexo Como Cadena
    Definir anioTexto, mesTexto, diaTexto Como Cadena
    Definir patLimpio, matLimpio, nomLimpio Como Cadena
    Definir pos1, pos2, pos3, pos4, pos14, pos15, pos16 Como Cadena
    Definir iniciales, curp16 Como Cadena
    
    // Interfaz básica para pedir datos
    Escribir "--- GENERADOR DE CURP ---"
    Escribir "Nombre(s): "
    Leer nombres
    Escribir "Apellido Paterno: "
    Leer apPaterno
    Escribir "Apellido Materno (Si no tiene, escriba X): "
    Leer apMaterno
	
    // Se piden las fechas ya como texto para ya poder convertirlo 
    // Números enteros a texto y que se pierdan los ceros a la izquierda (ej. 05)
    Escribir "Año de Nacimiento (Solo 2 digitos, ej. 03 o 98): "
    Leer anioTexto
    Escribir "Mes de Nacimiento (2 digitos, ej. 05): "
    Leer mesTexto
    Escribir "Dia de Nacimiento (2 digitos, ej. 09): "
    Leer diaTexto
    Escribir "Sexo (H/M): "
    Leer sexo
    Escribir "Entidad de Nacimiento (Ej. Sonora): "
    Leer entidad
    
    // Limpiamos los datos de entrada
    patLimpio <- FiltrarPreposiciones(apPaterno)
    matLimpio <- FiltrarPreposiciones(apMaterno)
    nomLimpio <- FiltrarNombre(nombres)
    
    // --- LÓGICA DE APELLIDO PATERNO ---
    Si Longitud(patLimpio) > 0 Entonces
        pos1 <- Subcadena(patLimpio, 1, 1)
        Si pos1 = "Ñ" Entonces
            pos1 <- "X" // Si el apellido empieza con Ñ, va X
        FinSi
        pos2 <- VocalInterna(patLimpio)
        pos14 <- ConsonanteInterna(patLimpio)
    Sino
        // En teoría siempre hay paterno, pero por si acaso meten cadena vacía
        pos1 <- "X"; pos2 <- "X"; pos14 <- "X"
    FinSi
    
    // --- LÓGICA DE APELLIDO MATERNO ---
    // Si no tiene apellido materno (pusieron X), todas sus posiciones se vuelven X
    Si Longitud(matLimpio) > 0 Y Mayusculas(matLimpio) <> "X" Entonces
        pos3 <- Subcadena(matLimpio, 1, 1)
        Si pos3 = "Ñ" Entonces
            pos3 <- "X"
        FinSi
        pos15 <- ConsonanteInterna(matLimpio)
    Sino
        pos3 <- "X"; pos15 <- "X"
    FinSi
    
    // --- LÓGICA DE NOMBRE ---
    Si Longitud(nomLimpio) > 0 Entonces
        pos4 <- Subcadena(nomLimpio, 1, 1)
        Si pos4 = "Ñ" Entonces
            pos4 <- "X"
        FinSi
        pos16 <- ConsonanteInterna(nomLimpio)
    Sino
        pos4 <- "X"; pos16 <- "X"
    FinSi
    
    // --- ENSAMBLE DE LA CURP ---
    
    // Primero armamos las 4 iniciales con puro Concatenar para evitar el error de tipos numéricos
    iniciales <- Concatenar(pos1, pos2)
    iniciales <- Concatenar(iniciales, pos3)
    iniciales <- Concatenar(iniciales, pos4)
    
    // Pasamos las 4 iniciales por el filtro de groserías
    iniciales <- EvitarInconvenientes(iniciales)
    
    // Ahora vamos armando la CURP completa de 16 dígitos, igual, línea por línea.
    curp16 <- Concatenar(iniciales, anioTexto)
    curp16 <- Concatenar(curp16, mesTexto)
    curp16 <- Concatenar(curp16, diaTexto)
    curp16 <- Concatenar(curp16, Mayusculas(Subcadena(sexo, 1, 1)))
    curp16 <- Concatenar(curp16, ClaveEntidad(entidad))
    curp16 <- Concatenar(curp16, pos14)
    curp16 <- Concatenar(curp16, pos15)
    curp16 <- Concatenar(curp16, pos16)
    
    Escribir ""
    Escribir "-------------------------------------------"
    Escribir "CURP Calculada (16 posiciones): ", curp16
    Escribir "-------------------------------------------"
FinAlgoritmo