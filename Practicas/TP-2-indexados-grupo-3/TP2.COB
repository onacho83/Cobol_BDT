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
           SELECT MOVART ASSIGN TO '../movart.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARTICULOS
           ASSIGN TO '../articulos.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS art-codigo.
           SELECT FACTURAS
           ASSIGN TO '../facturas.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS fact-CLAVE.
           SELECT CLIENVIP ASSIGN TO '../clienvip.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-CLAVE.

       DATA DIVISION.
       FILE SECTION.
       FD  MOVART.
       01  mov-reg.
           03 mov-vend                         PIC 9(3).
           03 mov-cliente                      PIC 9(6).
           03 mov-factura                      PIC 9(12).
       FD  ARTICULOS.
       01  art-reg.
           03 art-codigo                       PIC X(8).
           03 art-descrip                      PIC X(30).
           03 art-costo                        PIC 9(10).
       FD  FACTURAS.
       01  fact-reg.
           03 FACT-CLAVE.
               05 fact-factura                 PIC 9(12).
               05 fact-art                     PIC X(8).
           03 fact-precio                      PIC 9(10).
           03 fact-cantidad                    PIC 9(4).
       FD  CLIENVIP.
       01  cli-reg.
           03 CLI-CLAVE.
               05 cli-vend                     PIC 9(3).
               05 cli-codcli                   PIC 9(6).
           03 cli-ganancia                     PIC 9(10).
           03 cli-nombre                       PIC X(30).



       WORKING-STORAGE SECTION.

       77  EOF-MOVART                          PIC 9.
       77  ANT-VEND                            PIC 9(3).
       77  ANT-CLI                             PIC 9(6).
       77  ANT-FACT                            PIC 9(12).
       77  EOF-FACTURAS                        PIC 9.
       77  ACUM-GANANCIAS-TOTAL                PIC 9(10).
       77  GANANCIA-ART                        PIC 9(10).
       77  ART-ENCONTRADO                      PIC X.
       01  VIP-BUSCAR                          PIC X.
           88 VIP-ENCONTRADO                   VALUE 'S'.
           88 VIP-NO-ENCONTRADO                VALUE 'N'.
       77  ANT-CLI-VEND                        PIC 9(3).
       77  EOF-CLIENVIP                        PIC 9.
       77  CONT-CLIVIP                         PIC 99.
       77  MENOR-CLI                           PIC 9(6).
       77  MENOR-GANANCIA                      PIC 9(10).
       77  CONT                                PIC 999.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 1000-INICIO-GENERAL.
            PERFORM 2000-LEER-MOVIMIENTOS.
            PERFORM UNTIL EOF-MOVART = 1
               PERFORM 3000-INICIO-VENDEDOR
               PERFORM UNTIL EOF-MOVART = 1 OR mov-vend <> ANT-VEND
                   PERFORM 4000-INICIO-CLIENTE
                   PERFORM UNTIL EOF-MOVART = 1 OR mov-vend <> ANT-VEND
                                 OR mov-cliente <> ANT-CLI
                                 PERFORM 5000-PROCESO
                                 PERFORM 2000-LEER-MOVIMIENTOS
                   END-PERFORM
                   PERFORM 6000-FIN-CLIENTE
               END-PERFORM
               PERFORM 7000-FIN-VENDEDOR
            END-PERFORM.
            PERFORM 8000-FIN-GENERAL.
            STOP RUN.

       1000-INICIO-GENERAL.
           OPEN
               INPUT MOVART, FACTURAS, ARTICULOS
               I-O CLIENVIP.
           DISPLAY "Abrimos archivos".

       2000-LEER-MOVIMIENTOS.
           DISPLAY "lEEMOS MOVART"
           READ MOVART AT END MOVE 1 TO EOF-MOVART.

       3000-INICIO-VENDEDOR.

           MOVE mov-vend TO ANT-VEND.
           MOVE 0 TO CONT.
      **   contador de cantidad de registros de clientes por vendedor
           DISPLAY "procesando el vendedor:  ", mov-vend.
       4000-INICIO-CLIENTE.
           ADD 1 TO CONT.
           MOVE mov-cliente TO ANT-CLI.
           MOVE 0 TO ACUM-GANANCIAS-TOTAL.
           DISPLAY "proceso el cliente: ", mov-cliente.
       5000-PROCESO.
           DISPLAY "Procesamos Cliente".
           PERFORM 5100-BUSCAR-FACTURA.
           PERFORM 5200-LEER-FACTURA.
           PERFORM 5230-INICIO-FACTURA.
           PERFORM UNTIL fact-factura <> ANT-FACT OR EOF-FACTURAS = 1
               PERFORM 5400-BUSCAR-ART
               IF ART-ENCONTRADO = 'S'
                   PERFORM 5500-ACUM-GANANCIAS
               END-IF
               PERFORM 5200-LEER-FACTURA
           END-PERFORM.

       5100-BUSCAR-FACTURA.
           DISPLAY "Buscamos Factura".
           MOVE mov-factura TO fact-factura.
           MOVE ZEROS TO fact-art.
           PERFORM 5110-POSICIONAMIENTO-F THRU 5110-F-POSICIONAMIENTO-F.

       5110-POSICIONAMIENTO-F.
           DISPLAY "Inicializamos Clave FACT-CLAVE"
           START FACTURAS KEY IS > FACT-CLAVE
                                  INVALID KEY
                                      DISPLAY "NO SE ENCONTRO FACTURA"
                                  GO TO 5110-F-POSICIONAMIENTO-F.
       5110-F-POSICIONAMIENTO-F.
           DISPLAY "Finalizamos puntero por invalid key"
           EXIT.

       5200-LEER-FACTURA.
           DISPLAY "Leemos Factura"
           READ FACTURAS NEXT AT END MOVE 1 TO EOF-FACTURAS.

       5230-INICIO-FACTURA.
           MOVE fact-factura TO ANT-FACT.
           DISPLAY "FACTURA: ", fact-factura.
       5400-BUSCAR-ART.
           DISPLAY "Buscamos Articulo"
           MOVE FACT-ART TO art-codigo.
           READ ARTICULOS INVALID KEY
                          DISPLAY "ARTICULO INEXISTENTE"
                          MOVE 'N' TO ART-ENCONTRADO.

           MOVE 'S' TO ART-ENCONTRADO.

       5500-ACUM-GANANCIAS.
           DISPLAY "Acumulamos Ganancias"
           COMPUTE GANANCIA-ART =
           (FACT-PRECIO - art-costo) * fact-cantidad.

           COMPUTE ACUM-GANANCIAS-TOTAL = ACUM-GANANCIAS-TOTAL +
                                           GANANCIA-ART.
           DISPLAY "LAS GANANCIAS POR ARTICULO ", GANANCIA-ART.
       6000-FIN-CLIENTE.
           DISPLAY "Fin Cliente"
           PERFORM 6100-BUSCAR-CLIENTEVIP.

           IF VIP-ENCONTRADO
               DISPLAY VIP-BUSCAR
               DISPLAY "Actualizamos CLIENVIP"
               PERFORM 6200-ACTUALIZAR-CLIENVIP
           ELSE
               PERFORM 6300-POSICIONAMIENTO-C THRU
                                               6300-F-POSICIONAMIENTO-C
               PERFORM 6400-LEER-CLIENVIP
               PERFORM 6400-INICIO-VIP
               PERFORM UNTIL CLI-VEND <> ant-vend
                             OR EOF-CLIENVIP = 1
                   PERFORM 6500-COMPARAR-GANANCIAS
                   PERFORM 6400-LEER-CLIENVIP
               END-PERFORM
               PERFORM 6600-GRABAR-CLIENTE
           END-IF.



       6100-BUSCAR-CLIENTEVIP.
           DISPLAY "Buscar Cliente VIP"
           MOVE ant-vend TO CLI-VEND.
           MOVE ant-cli  TO cli-codcli.

           READ CLIENVIP INVALID KEY
                             DISPLAY "CLIENTE NO ENCONTRADO"
                             MOVE 'N' TO VIP-BUSCAR
                         NOT INVALID KEY
                             MOVE 'S' TO VIP-BUSCAR.



       6200-ACTUALIZAR-CLIENVIP.
           ADD ACUM-GANANCIAS-TOTAL TO cli-ganancia.
           REWRITE CLI-REG.

       6300-POSICIONAMIENTO-C.
           DISPLAY "Posicionamos ClienVIP"
           MOVE ZERO TO cli-codcli.
           START CLIENVIP KEY IS > CLI-CLAVE
                     INVALID KEY
                         DISPLAY "NO SE ENCONTRO"
                         GO TO 6300-F-POSICIONAMIENTO-C.
       6300-F-POSICIONAMIENTO-C.
           EXIT.

       6400-LEER-CLIENVIP.
           DISPLAY "Leemos CLIENVIP"
           READ CLIENVIP NEXT AT END MOVE 1 TO EOF-CLIENVIP.

       6400-INICIO-VIP.
           DISPLAY "Inicio ClienteVip".
           MOVE 0 TO CONT-CLIVIP.
           MOVE 9999999999 TO MENOR-GANANCIA.

       6500-COMPARAR-GANANCIAS.
           DISPLAY "COMPARAMOS GANANCIAS".
           ADD 1 TO CONT-CLIVIP.

           IF cli-ganancia < MENOR-GANANCIA
               MOVE cli-codcli   TO MENOR-CLI
               MOVE cli-ganancia TO MENOR-GANANCIA
           END-IF.

       6600-GRABAR-CLIENTE.
           DISPLAY "Contador CLIENVIP", CONT-CLIVIP.
           DISPLAY "Ganancia Total", acum-ganancias-total.
           DISPLAY "Menor Ganancia", MENOR-GANANCIA.

           IF CONT-CLIVIP < 10
             PERFORM 6650-agregar-nuevo-cli
           else
             IF ACUM-GANANCIAS-TOTAL > MENOR-GANANCIA
                 DISPLAY "Vendedor con mas de 10 VIP"
                 move MENOR-CLI to cli-codcli
                 move ANT-VEND to cli-vend
                 DISPLAY "Borramos Cliente Menor", cli-codcli
                 DELETE CLIENVIP
                 PERFORM 6650-agregar-nuevo-cli
             END-IF
           END-IF.

       6650-agregar-nuevo-cli.

           MOVE ANT-CLI TO cli-codcli
           MOVE ANT-CLI-VEND TO CLI-VEND
           MOVE ACUM-GANANCIAS-TOTAL TO cli-ganancia
           WRITE CLI-REG INVALID KEY
                           DISPLAY "No se agrego cliente"
                         NOT INVALID KEY
                           DISPLAY "Cliente VIP Agregado".

       7000-FIN-VENDEDOR.
           DISPLAY "SE PROCESARON: ", CONT, " REGISTROS.".

       8000-FIN-GENERAL.
           DISPLAY "Programa Finalizado"
           CLOSE MOVART, ARTICULOS, CLIENVIP, FACTURAS.
       END PROGRAM YOUR-PROGRAM-NAME.
