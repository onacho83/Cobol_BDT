      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CURSOS ASSIGN TO "..\cursos.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ALUMNOS ASSIGN TO "..\alumnos.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MATERIAS ASSIGN TO "..\materias.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-ARCH ASSIGN TO "SORTWORK"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.

       FD  CURSOS.
       01  CUR-REG.
           03  CUR-CURSO   PIC XX.
           03  CUR-ALUMNO  PIC 9(6).

       FD  ALUMNOS.
       01  ALU-REG-CAB.
           03 ALU-CAB-TIPOREG PIC X.
           03 ALU-CAB-COD     PIC 9(6).
           03 ALU-CAB-NOMBRE  PIC X(20).
       01  ALU-REG-DET.
           03 ALU-DET-TIPOREG PIC X.
           03 ALU-DET-MATERIA PIC 99.
           03 ALU-DET-NOTA    PIC 99.

       FD  MATERIAS.
       01  MAT-REG.
           03  MAT-COD     PIC 99.
           03  MAT-NOMBRE  PIC X(20).

       SD  SORT-ARCH.
       01  SORT-REG.
           03 SORT-MAT-COD     PIC 99.
           03 SORT-NOTA        PIC 99.

       WORKING-STORAGE SECTION.

       77  WSS-CURSOS-EOF    PIC 9 VALUE IS 0.
       77  WSS-MATERIAS-EOF  PIC 9 VALUE IS 0.
       77  WSS-ALUMNOS-EOF   PIC 9 VALUE IS 0.
       77  WSS-SORT-EOF      PIC 9 VALUE IS 0.


       77  WSS-CONT-NOTA    PIC 99.
       77  WSS-ACUM-NOTA    PIC 99.
       77  WSS-MAT-ANT      PIC 99.
       77  WSS-PROMEDIO     PIC 99V99.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            SORT SORT-ARCH ON ASCENDING KEY SORT-MAT-COD
               INPUT PROCEDURE 1000-PROCESO-ENTRADA
               OUTPUT PROCEDURE 2000-PROCESO-SALIDA.
            STOP RUN.
       1000-PROCESO-ENTRADA.
           PERFORM 1100-INICIO-ENTRADA.
           PERFORM 1200-LEER-CURSO.
      ****************   LEO CABECERA   *******************
           PERFORM 1300-LEER-ALUMNO.

           PERFORM UNTIL WSS-CURSOS-EOF = 1
               PERFORM 1400-PROCESO-ALUMNO
               PERFORM 1200-LEER-CURSO
           END-PERFORM.



       1100-INICIO-ENTRADA.

           PERFORM 1110-ABRIR-ARCHIVOS.


       1110-ABRIR-ARCHIVOS.
           OPEN INPUT MATERIAS, CURSOS, ALUMNOS.


       1200-LEER-CURSO.
           READ CURSOS AT END MOVE 1 TO WSS-CURSOS-EOF.

       1300-LEER-ALUMNO.
           READ ALUMNOS AT END MOVE 1 TO WSS-ALUMNOS-EOF.

       1400-PROCESO-ALUMNO.
      *    LEO DETALLE
           PERFORM 1300-LEER-ALUMNO
           PERFORM UNTIL ALU-CAB-TIPOREG = "C" OR WSS-ALUMNOS-EOF = 1
               PERFORM 1430-GRABAR-SORT
               PERFORM 1300-LEER-ALUMNO
           END-PERFORM.


       1430-GRABAR-SORT.
           MOVE ALU-DET-MATERIA TO SORT-MAT-COD.
           MOVE ALU-DET-NOTA TO SORT-NOTA.
           RELEASE SORT-REG.


      ********************    salida  * ******************************
       2000-PROCESO-SALIDA.


           PERFORM 2100-LEER-SORT.

           PERFORM UNTIL WSS-SORT-EOF = 1

               PERFORM 2300-INICIO-MATERIA
               PERFORM UNTIL SORT-MAT-COD <> WSS-MAT-ANT OR
                     WSS-MATERIAS-EOF = 1 OR  WSS-SORT-EOF = 1

                       PERFORM 2200-PROCESO-SORT
                       PERFORM 2100-LEER-SORT
               END-PERFORM

               PERFORM 2400-FIN-MATERIA
           END-PERFORM.

           PERFORM 2600-CIERRE-ARCHIVOS.

       2300-INICIO-MATERIA.

           MOVE SORT-MAT-COD TO WSS-MAT-ANT.


       2100-LEER-SORT.
           RETURN SORT-ARCH AT END MOVE 1 TO WSS-SORT-EOF.

       2200-PROCESO-SORT.

           ADD 1 TO WSS-CONT-NOTA.
           COMPUTE WSS-ACUM-NOTA = SORT-NOTA + WSS-ACUM-NOTA.

       2400-FIN-MATERIA.

           PERFORM 2410-BUSCAR-MATERIA.

           DISPLAY "CODIGO ".
           DISPLAY WSS-MAT-ANT.
           DISPLAY "MATERIA".
           DISPLAY MAT-NOMBRE.
           DISPLAY "PROMEDIO".
           IF WSS-CONT-NOTA > 0
             COMPUTE WSS-PROMEDIO = WSS-ACUM-NOTA / WSS-CONT-NOTA
           ELSE
             MOVE 0 TO WSS-PROMEDIO
           END-IF.
           DISPLAY WSS-PROMEDIO.

       2410-BUSCAR-MATERIA.

           PERFORM 2415-LEER-MATERIA.
           PERFORM UNTIL WSS-MAT-ANT = MAT-COD OR WSS-MATERIAS-EOF = 1

               PERFORM 2415-LEER-MATERIA
           END-PERFORM.

       2415-LEER-MATERIA.
           READ MATERIAS AT END MOVE 1 TO WSS-MATERIAS-EOF.


       2600-CIERRE-ARCHIVOS.
           CLOSE  MATERIAS, CURSOS, ALUMNOS.


       END PROGRAM YOUR-PROGRAM-NAME.
