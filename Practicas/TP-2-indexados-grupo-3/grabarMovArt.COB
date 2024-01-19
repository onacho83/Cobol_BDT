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
           SELECT MOVART ASSIGN TO '..\movart.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  MOVART.
       01  mov-reg.
           03 mov-vend                         PIC 9(3).
           03 mov-cliente                      PIC 9(6).
           03 mov-factura                      PIC 9(12).
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-INGRESAR-VENDEDOR.
            PERFORM UNTIL mov-vend = 0
               PERFORM 300-INGRESAR-RESTO
               PERFORM 400-PROCESO-GRABAR
               PERFORM 200-INGRESAR-VENDEDOR
            END-PERFORM.
            PERFORM 500-FIN-GENERAL.
            STOP RUN.

       100-INICIO-GENERAL.
           OPEN OUTPUT MOVART.

       200-INGRESAR-VENDEDOR.
           DISPLAY
           'Ingrese numero de vendedor (para salir ingrese cero): '.
           ACCEPT mov-vend.

       300-INGRESAR-RESTO.
           DISPLAY 'Ingrese numero de cliente:'.
           ACCEPT mov-cliente.
           DISPLAY 'Ingrese numero de factura:'.
           ACCEPT mov-factura.

       400-PROCESO-GRABAR.
           WRITE mov-reg.

       500-FIN-GENERAL.
           CLOSE MOVART.

       END PROGRAM YOUR-PROGRAM-NAME.
