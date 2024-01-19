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
           SELECT FACTURAS
           ASSIGN TO '../facturas.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS FACT-CLAVE.
       DATA DIVISION.
       FILE SECTION.
       FD  FACTURAS.
       01  fact-reg.
           03 FACT-CLAVE.
               05 fact-factura                 PIC 9(12).
               05 fact-art                     PIC X(8).
           03 fact-precio                      PIC 9(10).
           03 fact-cantidad                    PIC 9(4).

       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-INGRESAR-FACT.
            PERFORM UNTIL fact-factura = 0
               PERFORM 300-INGRESAR-RESTO
               PERFORM 400-PROCESO
               PERFORM 200-INGRESAR-FACT
            END-PERFORM.
            PERFORM 500-FIN-GENERAL.
            STOP RUN.

       100-INICIO-GENERAL.
           OPEN OUTPUT FACTURAS.

       200-INGRESAR-FACT.
           DISPLAY 'Ingrese nro. de factura (para finalizar = cero):'.
           ACCEPT fact-factura.

       300-INGRESAR-RESTO.
           DISPLAY 'Ingrese el codigo del articulo:'.
           ACCEPT fact-art.
           DISPLAY 'Ingrese el precio unitario:'.
           ACCEPT fact-precio.
           DISPLAY 'Ingrese la cantidad:'.
           ACCEPT fact-cantidad.

       400-PROCESO.
           WRITE fact-reg INVALID KEY
                               DISPLAY 'Registro existente.'.

       500-FIN-GENERAL.
           CLOSE FACTURAS.

       END PROGRAM YOUR-PROGRAM-NAME.
