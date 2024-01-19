      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RECITALES ASSIGN TO "../recitales.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORT-ARCH ASSIGN TO "SORTWORK".

           SELECT ESPECTADORES ASSIGN TO "../espectadores.dat"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS RANDOM
               RELATIVE KEY IS REL-CLAVE.

           SELECT DETALLE ASSIGN TO "../DETALLE.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS RANDOM
                   RECORD KEY IS Det-Clave.

       DATA DIVISION.
       FILE SECTION.

       FD  RECITALES.
       01  Res-Cab-Reg.
           03 Res-Cab-Tipo pic 9.
           03 Res-Cab-Anio pic 9(4).
           03 Res-Cab-Recital pic AAA.
           03 Res-Cab-nombre pic A(15).
       01  Res-Det-Reg.
           03 Res-Det-Tipo pic 9.
           03 Res-Det-Recibo pic 9(10).
           03 Res-Det-Espectador pic 9(5).
           03 Res-Det-Cuota pic 9.
           03 Res-Det-Importe pic 9(8).

       SD  SORT-ARCH.
       01  sort-reg.
           03 sort-espectador pic 9(5).
           03 sort-cuota pic 9.
           03 sort-importe pic 9(8).

       FD  ESPECTADORES.
       01  ESP-REG.
           03 Esp-espectador PIC 9(5).
           03 Esp-Dni PIC 9(8).
           03 Esp-Cuotas PIC 99.
           03 Esp-Estado PIC A.
           03 Esp-Puntero PIC 99.

       FD  DETALLE.
       01  Det-Reg .
           03 Det-Clave.
              05 Det-Dni pic 9(8).
              05 Det-Cuota pic 99.
           03 Det-Fecha  pic 9(8).
           03 Det-Estado pic A.
           03 Det-Importe pic 9(8).
           03 Det-Pagado pic 9(8).
           03 Det-Debe pic 9(8).

       WORKING-STORAGE SECTION.
       01  REL-CLAVE   PIC 9(5).
       77  acum-pagos-esp pic 9(8).
       77  WS-RECITALES-EOF PIC 9.
       77  WS-SORT-EOF PIC 9.
       77  ws-ant-espectador pic 9(5).
       77  CUOTA-ANT PIC 99.
       01  ESP-BUSCAR PIC A.
           88 ESP-ENCONTRADO VALUE "S".
           88 ESP-NO-ENCONTRADO VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            SORT SORT-ARCH ON ASCENDING KEY sort-espectador,
                     ON ASCENDING KEY sort-cuota
                   INPUT PROCEDURE 1000-PROCESO-ENTRADA
                   OUTPUT PROCEDURE 2000-PROCESO-SALIDA
            STOP RUN.


       1000-PROCESO-ENTRADA.
           PERFORM 1100-inicio-Entreda.
           *> Leo Cabecera
           PERFORM 1200-LEER-RECITALES.
           PERFORM UNTIL WS-RECITALES-EOF = 1
               IF Res-Cab-Tipo = 1 AND Res-Cab-Recital = "RST"
                   PERFORM 1300-PROCESO-RECITALES-SORT
               END-IF
               PERFORM 1200-LEER-RECITALES
           END-PERFORM.
           PERFORM 1400-FIN-ENTRADA.
       1100-inicio-Entreda.
           OPEN INPUT RECITALES.
           MOVE 0 TO WS-RECITALES-EOF.

       1200-LEER-RECITALES.
           READ RECITALES AT END MOVE 1 TO WS-RECITALES-EOF.

       1300-PROCESO-RECITALES-SORT.
           PERFORM 1200-LEER-RECITALES.
           MOVE Res-Det-Espectador TO sort-espectador.
           MOVE RES-Det-Cuota to sort-cuota.
           MOVE Res-Det-Importe TO sort-importe.
           PERFORM 1350-GRABAR-SORT.

       1350-GRABAR-SORT.
           RELEASE sort-reg.

       1400-FIN-ENTRADA.
           CLOSE RECITALES.

      *********************   SALIDA   *********************************

       2000-PROCESO-SALIDA.
           PERFORM 2100-INICIO-SALIDA.
           PERFORM 2200-LEER-SORT.

           PERFORM UNTIL WS-SORT-EOF = 1
             PERFORM 2300-INICIO-ESPECTADOR

             PERFORM 2500-LEER-ESPECTADORES
             IF ESP-ENCONTRADO AND ESP-ESTADO = "A"
                 PERFORM UNTIL WS-SORT-EOF = 1 OR
                               sort-espectador <> ws-ant-espectador
                               OR Det-Cuota <> CUOTA-ANT

                     PERFORM 2400-ACUMULAR-PAGOS
                     PERFORM 2200-LEER-SORT

                 END-PERFORM

                 PERFORM 2600-PROCESO-DETALLE
             END-IF

             PERFORM 2700-FIN-ESPECTADOR
           END-PERFORM.
           PERFORM 2800-FIN-SALIDA.


       2100-INICIO-SALIDA.
           DISPLAY "Inicio Proceso Salida"
           OPEN I-O ESPECTADORES, DETALLE.

       2200-LEER-SORT.
           RETURN SORT-ARCH AT END MOVE 1 TO WS-SORT-EOF.
       2300-INICIO-ESPECTADOR.
           MOVE sort-espectador TO ws-ant-espectador.
           MOVE sort-cuota TO CUOTA-ANT.
           DISPLAY "Socio Nro.: ", sort-espectador.
           MOVE 0 to acum-pagos-esp.
           MOVE sort-espectador TO REL-CLAVE.

       2400-ACUMULAR-PAGOS.
           ADD sort-importe TO acum-pagos-esp.

       2500-LEER-ESPECTADORES.
           READ ESPECTADORES INVALID KEY
                                DISPLAY "ESPECTADOR NO ENCONTRADO"
                                MOVE "N" TO ESP-BUSCAR
                             NOT INVALID KEY
                                MOVE "S" TO ESP-BUSCAR.

       2600-PROCESO-DETALLE.
           MOVE Esp-Dni TO Det-Dni.
           MOVE sort-cuota TO Det-Cuota.

           PERFORM 2650-LEER-DETALLE.
           IF acum-pagos-esp >= Det-Pagado
                    COMPUTE acum-pagos-esp = acum-pagos-esp - Det-pagado
                     MOVE 0 TO det-Debe
                     MOVE Det-Importe TO det-Pagado
                     MOVE "C" TO det-Estado
                 ELSE
                     compute det-debe = det-debe - acum-pagos-esp
                     ADD acum-pagos-esp to det-Pagado
                     move 0 to acum-pagos-esp
                 END-IF.

                 PERFORM 2660-Regrabar.

       2650-LEER-DETALLE.
           READ DETALLE INVALID KEY
                           DISPLAY "ESPECTADOR NO ENCONTRADO".

       2660-Regrabar.
           REWRITE Det-Reg.

       2700-FIN-ESPECTADOR.
            if Esp-Cuotas = Det-Cuota
              and Det-Estado = "C"
              move "c" to Esp-Estado
              PERFORM 2750-regrabar-espectador
             end-if.
       2750-regrabar-espectador.
           REWRITE ESP-REG.

       2800-FIN-SALIDA.
           CLOSE ESPECTADORES, DETALLE.


       END PROGRAM YOUR-PROGRAM-NAME.
