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
       SELECT CUOTAS ASSIGN TO '../cuotas.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUO-RECIBO
               ALTERNATE KEY IS CUO-PASAJERO WITH DUPLICATES.

           SELECT PASAJEROS ASSIGN TO '../pasajeros.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PAS-CLAVE.
       DATA DIVISION.
       FILE SECTION.
       FD  CUOTAS.
       01  CUOTA-REG.
           03 CUO-RECIBO PIC 9(10).
           03 CUO-PASAJERO PIC 9(5).
           03 CUO-FECHA PIC 9(8).
           03 CUO-IMPORTE PIC 9(8).

       FD  PASAJEROS.
       01  PAS-REG.
           03 PAS-CLAVE.
               05 PAS-PASAJERO PIC 9(5).
               05 PAS-MES PIC 99.
           03 PAS-ESTADO PIC A.
           03 PAS-CUOTA PIC 9(6).
           03 PAS-PAGADO PIC 9(6).
           03 PAS-DEBE PIC 9(6).

       WORKING-STORAGE SECTION.
      ********LINEAS DE IMPRESION
       01  TITULO-1.
           03 FILLER pic X(24) VALUE SPACES.
           03 FILLER pic x(32) VALUE "Examen Performante Omar Barboza".

       01  CUOTAMES.
           03 FILLER         PIC 9(8) VALUE 455.
           03 FILLER         PIC 9(8) VALUE 655.
           03 FILLER         PIC 9(8) VALUE 755.
           03 FILLER         PIC 9(8) VALUE 855.
           03 FILLER         PIC 9(8) VALUE 955.
           03 FILLER         PIC 9(8) VALUE 1055.
           03 FILLER         PIC 9(8) VALUE 1155.
           03 FILLER         PIC 9(8) VALUE 1255.
           03 FILLER         PIC 9(8) VALUE 1355.
           03 FILLER         PIC 9(8) VALUE 1455.
           03 FILLER         PIC 9(8) VALUE 1555.
           03 FILLER         PIC 9(8) VALUE 1655.
       01  WST-CUOTA REDEFINES CUOTAMES.
           03 VEC-CUOTAMES            PIC 9(8) OCCURS 12 TIMES.

       01  VEC-ACUM-CUOTAS PIC 9(8) OCCURS 12 TIMES.
       01  ACUM-CUOTAS-AUX  PIC 9(8) VALUE 0.

       77  EOF-CUOTA           PIC 9.
       77  EOF-PAS             PIC 9.
       01  ANT-PASAJERO        PIC 9(5).
       01  ACUMCUOTAS          PIC 9(8).
       01  MES-BUSCAR                          PIC X.
           88 MES-ENCONTRADO                   VALUE 'S'.
           88 MES-NO-ENCONTRADO                VALUE 'N'.
       01  ws-acum-pas-pagado                   pic 9(8).
       77  I                             PIC 99 VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 200-POSICIONAMIENTO-CUOTA.
            PERFORM 205-LEER-CUOTA.
            PERFORM UNTIL EOF-CUOTA =1
                   PERFORM 300-INICIO-CUOTA
      *    ---- Acumulamos todos los pagos del socio
                   PERFORM UNTIL CUO-PASAJERO <> ANT-PASAJERO
                                 OR EOF-CUOTA =1
                       COMPUTE ACUMCUOTAS = ACUMCUOTAS + CUO-IMPORTE
                       PERFORM 205-LEER-CUOTA
                   END-PERFORM
                   DISPLAY "Total Credito Acumulado: ", ACUMCUOTAS
      *    ---- con los pagos acumulados, procesamos los meses a cancelar
                   PERFORM 400-BUSCO-PASAJERO-MES-0
      *    ----pasajero mes 0, guardo el pas-pagado y consulto su estado
                   move PAS-Pagado to ws-acum-pas-pagado

                   IF Pas-Estado = "C"
                       DISPLAY "SU DEUDA YA ESTABA CANCELADA."
                       DISPLAY "NO SE PROCESO EL PAGO"
                   ELSE
                     PERFORM 600-CALCULO-MES-IMPAGO
      *    ----Bucle de cancelacion de meses
                     PERFORM 605-PROCESO-MESES
                   END-IF
            END-PERFORM.

            PERFORM 800-fin-general.

            STOP RUN.

      *-----------------  RUTINAS --------------------------------------
       100-INICIO-GENERAL.
           DISPLAY TITULO-1.
           OPEN
               INPUT CUOTAS
               I-O PASAJEROS.
           MOVE 1 TO I.

           PERFORM UNTIL I > 12
               ADD VEC-CUOTAMES(I) TO ACUM-CUOTAS-AUX
               MOVE ACUM-CUOTAS-AUX TO VEC-ACUM-CUOTAS(I)

               ADD 1 TO I
           END-PERFORM.

       200-POSICIONAMIENTO-CUOTA.

           move 0 to CUO-PASAJERO.
           START CUOTAS KEY IS > CUO-PASAJERO INVALID KEY
                                       DISPLAY "Error-CUOTA".

       205-LEER-CUOTA.

           READ CUOTAS next AT END MOVE 1 TO EOF-CUOTA.

       300-INICIO-CUOTA.
           DISPLAY "lEEMOS PAGOS DEL PASAJERO ", CUO-PASAJERO.
           MOVE CUO-PASAJERO TO ANT-PASAJERO.
           move 0 to ACUMCUOTAS.

       400-BUSCO-PASAJERO-MES-0.
           MOVE ANT-PASAJERO  TO PAS-PASAJERO.
           MOVE ZEROS TO PAS-MES.
           PERFORM 500-LEO-PASAJERO.

       500-LEO-PASAJERO.
           READ PASAJEROS INVALID KEY
                               PERFORM 520-AGREGAR-REGISTRO.

       510-REGRABAR.
           REWRITE PAS-REG.

       520-AGREGAR-REGISTRO.
           MOVE "A" TO PAS-ESTADO.
           MOVE VEC-CUOTAMES(PAS-MES) TO PAS-CUOTA.
           MOVE 0 TO PAS-PAGADO.
           MOVE VEC-CUOTAMES(PAS-MES) TO PAS-DEBE.
           WRITE PAS-REG.


       530-actualizar-mes-cero.
           move 0 to PAS-MES.
           move 0 to PAS-Debe.
           move 0 to PAS-Cuota.
           move ws-acum-pas-pagado to PAS-Pagado.
           PERFORM 510-REGRABAR.
           IF PAS-ESTADO = "C"
             DISPLAY "SE CANCELO SU DEUDA TOTAL"
             DISPLAY "SALDO A FAVOR: ", ACUMCUOTAS
           END-IF.

       540-acum-soc-pagado.
            compute ws-acum-pas-pagado =
                                ws-acum-pas-pagado + pas-Pagado.


       600-CALCULO-MES-IMPAGO.
           DISPLAY "CALCULO MES IMPAGO"
           MOVE 1 TO I

           PERFORM WITH TEST BEFORE UNTIL
                   ws-acum-pas-pagado < VEC-ACUM-CUOTAS(I)
                           ADD 1 TO I
           END-PERFORM.

           MOVE I TO PAS-MES.

       605-PROCESO-MESES.
           PERFORM 500-LEO-PASAJERO

           PERFORM UNTIL ACUMCUOTAS = 0 OR
                           (PAS-MES = 12 AND PAS-debe = 0)
                 PERFORM 700-MES-EN-PROCESO
                 IF ACUMCUOTAS >= PAS-Debe
                     COMPUTE ACUMCUOTAS = ACUMCUOTAS - PAS-DEBE
                     MOVE 0 TO PAS-Debe
                     MOVE PAS-Cuota TO PAS-Pagado
                     MOVE "C" TO PAS-Estado
                 ELSE
                     compute PAS-debe = PAS-debe - ACUMCUOTAS
                     ADD ACUMCUOTAS to PAS-Pagado
                     move 0 to ACUMCUOTAS
                 END-IF

                 PERFORM 540-acum-soc-pagado

                 PERFORM 705-MES-PROCESADO
                 PERFORM 510-REGRABAR


                 IF PAS-Mes < 12 and ACUMCUOTAS > 0
                       ADD 1 TO PAS-MES
                       PERFORM 520-AGREGAR-REGISTRO

                 END-IF

           END-PERFORM.

           PERFORM 530-actualizar-mes-cero.

       700-MES-EN-PROCESO.
           DISPLAY "*********MES EN PROCESADO*******"
           DISPLAY "*PASAJERO ", PAS-PASAJERO, " *MES ", PAS-MES.
           DISPLAY "*ESTADO: ", PAS-ESTADO, " *IMPORTE ", PAS-CUOTA.
           DISPLAY "*PAGADO ", PAS-PAGADO," *DEBE ", PAS-DEBE.


       705-MES-PROCESADO.
           DISPLAY "------MES PROCESADO-----"
           DISPLAY "*PASAJERO ", PAS-PASAJERO, " *MES ", PAS-MES.
           DISPLAY "*ESTADO ", PAS-ESTADO, " *IMPORTE ", PAS-CUOTA.
           DISPLAY "*PAGADO ", PAS-PAGADO," *DEBE ", PAS-DEBE.


       800-fin-general.
           close CUOTAS , PASAJEROS.
       END PROGRAM YOUR-PROGRAM-NAME.
