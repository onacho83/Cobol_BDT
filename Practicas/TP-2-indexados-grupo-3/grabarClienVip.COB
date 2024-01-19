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
           SELECT CLIENVIP ASSIGN TO '../clienvip.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-CLAVE.

       DATA DIVISION.
       FILE SECTION.
       FD  CLIENVIP.
       01  cli-reg.
           03 CLI-CLAVE.
               05 cli-vend                     PIC 9(3).
               05 cli-codcli                   PIC 9(6).
           03 cli-ganancia                     PIC 9(10).
           03 cli-nombre                       PIC X(30).

       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-INGRESAR-CLI-VEND.
            PERFORM UNTIL cli-vend = 0
               PERFORM 300-INGRESAR-RESTO
               PERFORM 400-PROCESO
               PERFORM 200-INGRESAR-CLI-VEND
            END-PERFORM.
            PERFORM 500-FIN-GENERAL.
            STOP RUN.

       100-INICIO-GENERAL.
           OPEN OUTPUT CLIENVIP.

       200-INGRESAR-CLI-VEND.
           DISPLAY
           'Ingrese codigo del vendedor (para finalizar ingrese cero):'.
           ACCEPT cli-vend.

       300-INGRESAR-RESTO.
           DISPLAY 'Ingrese codigo del cliente:'.
           ACCEPT cli-codcli.
           DISPLAY 'Ingrese ganancia del cliente:'.
           ACCEPT cli-ganancia.
           DISPLAY 'Ingrese nombre del cliente:'.
           ACCEPT cli-nombre.


       400-PROCESO.
           WRITE cli-reg INVALID KEY
                               DISPLAY 'Registro existente.'.

       500-FIN-GENERAL.
           CLOSE CLIENVIP.

       END PROGRAM YOUR-PROGRAM-NAME.
