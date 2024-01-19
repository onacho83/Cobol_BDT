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
           SELECT ARTICULOS
           ASSIGN TO '../articulos.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS art-codigo.
       DATA DIVISION.
       FILE SECTION.
       FD  ARTICULOS.
       01  art-reg.
           03 art-codigo                       PIC X(8).
           03 art-descrip                      PIC X(30).
           03 art-costo                        PIC 9(10).

       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-INGRESAR-ART.
            PERFORM UNTIL art-codigo = 0
               PERFORM 300-INGRESAR-RESTO
               PERFORM 400-PROCESO
               PERFORM 200-INGRESAR-ART
            END-PERFORM.
            PERFORM 500-FIN-GENERAL.
            STOP RUN.

       100-INICIO-GENERAL.
           OPEN OUTPUT ARTICULOS.

       200-INGRESAR-ART.
           DISPLAY 'Ingrese codigo (para finalizar ingrese cero):'.
           ACCEPT art-codigo.

       300-INGRESAR-RESTO.
           DISPLAY 'Ingrese descripcion:'.
           ACCEPT art-descrip.
           DISPLAY 'Ingrese el costo del articulo:'.
           ACCEPT art-costo.

       400-PROCESO.
           WRITE art-reg INVALID KEY
                               DISPLAY 'Registro existente.'.

       500-FIN-GENERAL.
           CLOSE ARTICULOS.

       END PROGRAM YOUR-PROGRAM-NAME.
