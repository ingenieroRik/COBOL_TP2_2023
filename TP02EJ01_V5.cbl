      ******************************************************************
      * Authors: Noemi Berge, Claudia Perdiguera, Ricardo Balsimelli,
      *          Ricardo Garcia, Senen Urdaneta.
      * Date: 14/11/2023
      * Purpose: TP2 - AULA 3 - GRUPO 1
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP02EJ01.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENT-CONSUMOS
           ASSIGN TO '../CONSUMOS.SEQ'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-CONSUMOS.

       SELECT SAL-RESUMENES
           ASSIGN TO '../RESUMENES.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-RESUMENES.

        SELECT SAL-ERRORES
           ASSIGN TO '../ERROR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ERRORES.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENT-CONSUMOS.
       01 WS-ENT-CONSUMOS.
          05 WS-ENT-ID-CONSUMO                        PIC 9(10).
          05 WS-ENT-NUM-CUENTA                        PIC 9(10).
          05 WS-ENT-NUM-TARJETA                       PIC X(19).
          05 WS-ENT-FECHA.
             07 WS-ENT-ANIO                           PIC X(04).
             07 FILLER                                PIC X VALUE '-'.
             07 WS-ENT-MES                            PIC X(02).
             07 FILLER                                PIC X VALUE '-'.
             07 WS-ENT-DIA                            PIC X(02).
          05 WS-ENT-DETALLE                               PIC X(32).
          05 WS-ENT-NUMERO-ACTUAL                     PIC 9(02).
          05 WS-ENT-CANT-CUOTAS                       PIC 9(02).
          05 WS-ENT-MONEDA                            PIC X(03).
               88 WS-MT-DOLARES                         VALUE 'USD'.
               88 WS-MT-PESOS                           VALUE 'ARS'.
          05 WS-ENT-IMPORTE                           PIC 9(08)V9(02).

       FD SAL-RESUMENES.
       01 WS-SAL-RESUMENES                            PIC X(91).


       FD SAL-ERRORES.
       01 WS-SAL-ERRORES.
201123       05 WS-SAL-ERRORES-REG                       PIC X(99).
201123       05 WS-SAL-ERRORES-COD-ERR                   PIC X(20).
201123       05 WS-SAL-ERRORES-DES-ERR                   PIC X(40).

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    FORMATO DE LOS ARCHIVOS DE ENTRADA/SALIDA                           *
      *----------------------------------------------------------------*
           COPY RESUMEN.

       01 WS-VAL-FECHA.
           COPY VALFECIO.
      *----------------------------------------------------------------*
      *    VARIABLES FILE STATUS  ENTRADA/SALIDA                       *
      *----------------------------------------------------------------*
       01 FS-STATUS.
      *----------------------------------------------------------------*
      *   ** FILE STATUS DE CONSUMOS                                   *
      *----------------------------------------------------------------*
          05 FS-CONSUMOS                    PIC X(2).
             88 FS-CONSUMOS-OK                          VALUE '00'.
             88 FS-CONSUMOS-EOF                         VALUE '10'.
             88 FS-CONSUMOS-NFD                         VALUE '35'.

      *----------------------------------------------------------------*
      *   ** FILE STATUS DE RESUMENES                                  *
      *----------------------------------------------------------------*
          05 FS-RESUMENES                  PIC X(2).
             88 FS-RESUMENES-OK                        VALUE '00'.
             88 FS-RESUMENES-EOF                       VALUE '10'.

      *----------------------------------------------------------------*
      *   ** FILE STATUS DE ERROR                                      *
      *----------------------------------------------------------------*
          05 FS-ERRORES                  PIC X(2).
             88 FS-ERRORES-OK                        VALUE '00'.
             88 FS-ERRORES-EOF                       VALUE '10'.

      *----------------------------------------------------------------*
      *    DECLARACION DE VARIABLES DEL PROGRAMA                       *
      *----------------------------------------------------------------*
       01 WS-VARIABLES.
           02 WS-RESUMEN-TARJETA           PIC X(19)       VALUE SPACES.
           02 WS-RESUMEN-SALDO-ARS         PIC 9(08)V9(02) VALUE ZEROES.
           02 WS-RESUMEN-SALDO-USD         PIC 9(08)V9(02) VALUE ZEROES.
171123     02 WS-RESUMEN-DESCUENTO         PIC 9(08)V9(02) VALUE ZEROES.
           02 WS-RESUMEN-PAGO-MINIMO       PIC 9(08)V9(02) VALUE ZEROES.
           02 WS-RESUMEN-TOTAL-CONSUMOS    PIC 9(06)       VALUE ZEROES.
           02 WS-RESUMEN-TOTAL-RESUMENES   PIC 9(06)       VALUE ZEROES.
           02 WS-RESUMEN-TOTAL-ERRORES     PIC 9(06)       VALUE ZEROES.

       77 WS-TIPO-DE-CAMBIO                PIC 9(04)V9(02) VALUE 365,50.
       77 WS-MENSAJE                       PIC X(44)
                   VALUE '* Este mes ha superado su límite de compra *'.
       77 WS-DESCUENTO                     PIC 9(02) VALUE 10.
       77 WS-PAGO-MINIMO                   PIC 9(02) VALUE 5.
       77 WS-LIM-COMPRA                    PIC 9(08)V9(02).
       77 WS-MONTO-TOTAL                   PIC 9(08)V9(02).
       77 WS-RESUMEN-PESOSxTIPO-DE-CAMBIO  PIC 9(08)V9(02) VALUE ZEROES.

141123 01 WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOURS        PIC 9(02).
               10  WS-CURRENT-MINUTE       PIC 9(02).
               10  WS-CURRENT-SECOND       PIC 9(02).
               10  WS-CURRENT-MILLISECONDS PIC 9(02).
141123     05  WS-DIFF-FROM-GMT            PIC S9(4).
      *----------------------------------------------------------------*
      * LINKAGE SECTION.
       01 LK-ENTRADA.
      *   Area de datos de Entrada
           05 LK-ENT-NUM-TARJETA           PIC X(19).

      *   Area de datos de Salida
       01 LK-MAESTRO-TARJETAS-REG.
           05 LK-NUM-CUENTA                PIC 9(10).
           05 LK-NUM-TARJETA               PIC X(19).
           05 LK-NOMBRE                    PIC X(20).
           05 LK-APELLIDO                  PIC X(20).
           05 LK-DIRECCION                 PIC X(40).
           05 LK-COD-POSTAL                PIC 9(04).
           05 LK-MONEDA-TARJETA            PIC X(03).
              88 LK-DOLARES                                VALUE 'USD'.
              88 LK-PESOS                                  VALUE 'ARS'.
           05 LK-LIMITE-TARJETA            PIC 9(08)V9(02).
           05 LK-MOTIVO-ERROR-O.
              10 LK-COD-ERROR-O            PIC X(20)       VALUE SPACES.
              10 LK-DES-ERROR-O            PIC X(100)      VALUE SPACES.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           PERFORM 2000-PROCESAR-PROGRAMA
              THRU 2000-PROCESAR-PROGRAMA-FIN
             UNTIL FS-CONSUMOS-EOF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

           DISPLAY '#RESUMENES: ' WS-RESUMEN-TOTAL-RESUMENES
           DISPLAY '#CONSUMOS:  ' WS-RESUMEN-TOTAL-CONSUMOS
           DISPLAY '#ERRORES:   ' WS-RESUMEN-TOTAL-ERRORES

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           PERFORM 1100-ABRIR-ARCHIVOS
              THRU 1100-ABRIR-ARCHIVOS-FIN.

           PERFORM 1200-INICIALIZAR-VARIABLES
              THRU 1200-INICIALIZAR-VARIABLES-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVOS.

           PERFORM 1110-ABRIR-ENT-CONSUMOS
              THRU 1110-ABRIR-ENT-CONSUMOS-FIN.

           PERFORM 1120-ABRIR-SAL-RESUMENES
              THRU 1120-ABRIR-SAL-RESUMENES-FIN.

           PERFORM 1130-ABRIR-SAL-ERRORES
              THRU 1130-ABRIR-SAL-ERRORES-FIN.

       1100-ABRIR-ARCHIVOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1200-INICIALIZAR-VARIABLES.

141123     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           INITIALIZE WS-VARIABLES.

       1200-INICIALIZAR-VARIABLES-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1110-ABRIR-ENT-CONSUMOS.

           OPEN INPUT ENT-CONSUMOS.

           EVALUATE TRUE
               WHEN FS-CONSUMOS-OK
                    CONTINUE
               WHEN FS-CONSUMOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE CONSUMOS'
                    DISPLAY 'FILE STATUS: ' FS-CONSUMOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE CONSUMOS'
                    DISPLAY 'FILE STATUS: ' FS-CONSUMOS
      * SI NO ABRE EL ARCHIVO DE ENTRADA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1110-ABRIR-ENT-CONSUMOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1120-ABRIR-SAL-RESUMENES.

           OPEN OUTPUT SAL-RESUMENES.

           EVALUATE TRUE
               WHEN FS-RESUMENES-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO RESUMENES'
                    DISPLAY 'FILE STATUS: ' FS-RESUMENES
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1120-ABRIR-SAL-RESUMENES-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1130-ABRIR-SAL-ERRORES.

           OPEN OUTPUT SAL-ERRORES.

           EVALUATE TRUE
               WHEN FS-ERRORES-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO ERROR'
                    DISPLAY 'FILE STATUS: ' FS-ERRORES
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1130-ABRIR-SAL-ERRORES-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.

           PERFORM 2200-LEER-CONSUMOS
              THRU 2200-LEER-CONSUMOS-FIN.

171123*    *** si hay cambio de tarjeta validamos la tarjeta
171123     IF WS-RESUMEN-TARJETA <> WS-ENT-NUM-TARJETA THEN

              PERFORM 2300-VALIDAR-TARJETA
                 THRU 2300-VALIDAR-TARJETA-FIN

171123     END-IF.

171123*    *** si no hay error validamos la fecha del consumo
171123     IF LK-COD-ERROR-O = SPACES THEN

              PERFORM 2400-VALIDAR-FECHA
                 THRU 2400-VALIDAR-FECHA-FIN

171123     END-IF.

171123*    *** si no hay errores o no es EOF, se procesa el consumo
171123     IF LK-COD-ERROR-O = SPACES AND
              WS-VALIDACION-O <> 'N'  AND
              NOT FS-CONSUMOS-EOF

              ADD 1                TO WS-RESUMEN-TOTAL-CONSUMOS

              PERFORM 2600-ARMAR-RESUMENES
                 THRU 2600-ARMAR-RESUMENES-FIN

171123     END-IF.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2200-LEER-CONSUMOS.

141123     INITIALIZE LK-MAESTRO-TARJETAS-REG.
141123     INITIALIZE WS-SALIDA.

           READ ENT-CONSUMOS.

           EVALUATE TRUE
               WHEN FS-CONSUMOS-OK
               CONTINUE

               WHEN FS-CONSUMOS-EOF
      *    *** se hace el cierre con el último consumo válido leído
171123              PERFORM 2710-FINALIZAR-RESUMEN
171123                 THRU 2710-FINALIZAR-RESUMEN-FIN

               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE CONSUMOS'
                    DISPLAY 'FILE STATUS: ' FS-CONSUMOS
      * SI NO  LEE EL ARCHIVO DE ENTRADA DETENGO EL PROCESO
                    STOP RUN

           END-EVALUATE.

       2200-LEER-CONSUMOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2300-VALIDAR-TARJETA.

           INITIALIZE LK-MAESTRO-TARJETAS-REG.
171123     MOVE WS-ENT-NUM-TARJETA TO LK-ENT-NUM-TARJETA

           CALL 'MAESTARJ' USING LK-ENTRADA, LK-MAESTRO-TARJETAS-REG.

           EVALUATE TRUE
               WHEN LK-COD-ERROR-O <> SPACES

               PERFORM 2310-ERROR-TARJETA
                  THRU 2310-ERROR-TARJETA-FIN

           END-EVALUATE.

       2300-VALIDAR-TARJETA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2310-ERROR-TARJETA.

201123     MOVE WS-ENT-CONSUMOS TO WS-SAL-ERRORES-REG.
201123     MOVE LK-COD-ERROR-O TO WS-SAL-ERRORES-COD-ERR
201123     MOVE LK-DES-ERROR-O TO WS-SAL-ERRORES-DES-ERR

           PERFORM 2500-GRABAR-ERRORES
              THRU 2500-GRABAR-ERRORES-FIN.

       2310-ERROR-TARJETA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2400-VALIDAR-FECHA.

           INITIALIZE WS-SALIDA.
171123     MOVE WS-ENT-DIA         TO WS-DD-I.
171123     MOVE WS-ENT-MES         TO WS-MM-I.
171123     MOVE WS-ENT-ANIO        TO WS-AAAA-I.

           CALL 'CLVALFEC' USING WS-ENTRADA, WS-SALIDA.

           EVALUATE TRUE
             WHEN WS-VALIDACION-O = 'N'

               PERFORM 2410-ERROR-FECHA
                  THRU 2410-ERROR-FECHA-FIN

           END-EVALUATE.

       2400-VALIDAR-FECHA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2410-ERROR-FECHA.

201123     MOVE WS-ENT-CONSUMOS TO WS-SAL-ERRORES-REG.
201123     MOVE WS-COD-ERROR-O  TO WS-SAL-ERRORES-COD-ERR
201123     MOVE WS-DES-ERROR-O  TO WS-SAL-ERRORES-DES-ERR

           PERFORM 2500-GRABAR-ERRORES
              THRU 2500-GRABAR-ERRORES-FIN.

       2410-ERROR-FECHA-FIN.
           EXIT.


      *----------------------------------------------------------------*
       2500-GRABAR-ERRORES.

           WRITE WS-SAL-ERRORES.
           ADD 1 TO WS-RESUMEN-TOTAL-ERRORES.

       2500-GRABAR-ERRORES-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2600-ARMAR-RESUMENES.

171123     EVALUATE TRUE
171123         WHEN WS-RESUMEN-TARJETA = SPACES
171123*    *** si es primer consumo del archivo se hace el encabezado
171123              MOVE WS-ENT-NUM-TARJETA TO WS-RESUMEN-TARJETA
171123              PERFORM 2610-ENCABEZAR-RESUMEN
171123                 THRU 2610-ENCABEZAR-RESUMEN-FIN
171123              CONTINUE

171123         WHEN WS-RESUMEN-TARJETA <> WS-ENT-NUM-TARJETA
171123*    *** hay que finalizar el resumen por los consumos anteriores
171123              PERFORM 2710-FINALIZAR-RESUMEN
171123                 THRU 2710-FINALIZAR-RESUMEN-FIN

171123*    *** y ahora se hace el encabezado para la nueva tarjeta
171123              MOVE WS-ENT-NUM-TARJETA TO WS-RESUMEN-TARJETA
171123              PERFORM 2610-ENCABEZAR-RESUMEN
171123                 THRU 2610-ENCABEZAR-RESUMEN-FIN

171123     END-EVALUATE.

171123*    *** se imprime el detalle del consumo y suma saldos
171123     PERFORM 2630-DETALLAR-RESUMEN
171123        THRU 2630-DETALLAR-RESUMEN-FIN.

171123*    *** se hace el descuento, si corresponde
171123     IF WS-DESCUENTO-O = 'S' THEN

171123        PERFORM 2650-DESCONTAR-RESUMEN
171123           THRU 2650-DESCONTAR-RESUMEN-FIN

171123     END-IF.

       2600-ARMAR-RESUMENES-FIN.
           EXIT.


      *----------------------------------------------------------------*
       2610-ENCABEZAR-RESUMEN.

           MOVE  WS-RESUMEN-TARJETA TO WS-SAL-RESUMENES.
           ADD 1 TO WS-RESUMEN-TOTAL-RESUMENES.
           INITIALIZE WS-RESUMEN-SALDO-ARS, WS-RESUMEN-SALDO-USD,
                      WS-RESUMEN-DESCUENTO,WS-RESUMEN-PAGO-MINIMO

           PERFORM 2810-IMP-RES-ENCABEZADO
              THRU 2810-IMP-RES-ENCABEZADO-FIN.

       2610-ENCABEZAR-RESUMEN-FIN.
           EXIT.

      *----------------------------------------------------------------*
171123 2630-DETALLAR-RESUMEN.

181123     INITIALIZE WS-SAL-RESUMENES
181123*    *** suma los saldos en pesos o en dólares
181123     EVALUATE TRUE
181123              WHEN WS-ENT-MONEDA = 'ARS'
181123                   ADD WS-ENT-IMPORTE TO WS-RESUMEN-SALDO-ARS
181123              WHEN WS-ENT-MONEDA = 'USD'
181123                   ADD WS-ENT-IMPORTE TO WS-RESUMEN-SALDO-USD
181123     END-EVALUATE.
           IF WS-ENT-MONEDA  EQUAL 'ARS'
               THEN
                  MOVE   WS-ENT-IMPORTE TO  WS-RES-PESOS

               ELSE
                   MOVE WS-ENT-IMPORTE TO  WS-RES-DOLARES

           END-IF.
           MOVE WS-ENT-NUMERO-ACTUAL TO WS-RES-CUOTA.
           MOVE  WS-ENT-CANT-CUOTAS TO  WS-RES-CANT-CUOTAS.
           MOVE  WS-ENT-DETALLE TO  WS-RES-DETALLE.
      * fecha en el formato dd/mm/aaaa
           MOVE WS-ENT-ANIO TO WS-RES-ANIO-COMP.
           MOVE WS-ENT-MES  TO WS-RES-MES-COMP.
           MOVE WS-ENT-DIA  TO WS-RES-DIA-COMP.
      *    MOVE WS-ENT-FECHA TO WS-RES-FECHA-COMPRA.

      *    MOVE WS-ENT-DETALLE  TO  WS-RES-PESOS.
           MOVE WS-RES-COMPRAS TO WS-SAL-RESUMENES.

           INITIALIZE WS-RES-COMPRAS.



171123     PERFORM 2800-IMPRIMIR-RESUMEN
171123        THRU 2800-IMPRIMIR-RESUMEN-FIN.

171123 2630-DETALLAR-RESUMEN-FIN.
171123     EXIT.

      *----------------------------------------------------------------*
171123 2650-DESCONTAR-RESUMEN.

171123     INITIALIZE WS-SAL-RESUMENES
171123*    *** vamos a calcular el descuento
171123     DIVIDE WS-ENT-IMPORTE BY WS-DESCUENTO
171123                             GIVING WS-RESUMEN-DESCUENTO

171123*    *** resta el descuento de los saldos en pesos o en dólares
181123     EVALUATE TRUE
181123              WHEN WS-ENT-MONEDA = 'ARS'
181123                   SUBTRACT WS-RESUMEN-DESCUENTO FROM
181123                            WS-RESUMEN-SALDO-ARS GIVING
181123                            WS-RESUMEN-SALDO-ARS
181123              WHEN WS-ENT-MONEDA = 'USD'
181123                   SUBTRACT WS-RESUMEN-DESCUENTO FROM
181123                            WS-RESUMEN-SALDO-USD GIVING
181123                            WS-RESUMEN-SALDO-USD
181123     END-EVALUATE.

           IF WS-ENT-MONEDA  EQUAL 'ARS'
               THEN
      * Multiplico por  -1 para convertirlo en negativo para imprimirlo
                  MULTIPLY WS-RESUMEN-DESCUENTO BY -1
                                GIVING WS-RES-VALOR-DTO-ARS
               ELSE
                   MULTIPLY WS-RESUMEN-DESCUENTO BY -1
                      GIVING WS-RES-VALOR-DTO-USD
           END-IF.



           MOVE WS-RES-DESCUENTO   TO  WS-SAL-RESUMENES.

           INITIALIZE WS-RES-DESCUENTO.

171123     PERFORM 2800-IMPRIMIR-RESUMEN
171123        THRU 2800-IMPRIMIR-RESUMEN-FIN.

171123 2650-DESCONTAR-RESUMEN-FIN.
171123     EXIT.

      *----------------------------------------------------------------*
       2710-FINALIZAR-RESUMEN.

171123     PERFORM 2720-MOSTRAR-SALDOS-RESUMEN
171123        THRU 2720-MOSTRAR-SALDOS-RESUMEN-FIN.

171123     PERFORM 2740-CALCULAR-MIN-RESUMEN
171123        THRU 2740-CALCULAR-MIN-RESUMEN-FIN.

           PERFORM 2760-VER-LIMITE-RESUMEN
              THRU 2760-VER-LIMITE-RESUMEN-FIN.



       2710-FINALIZAR-RESUMEN-FIN.
           EXIT.

      *----------------------------------------------------------------*
171123 2720-MOSTRAR-SALDOS-RESUMEN.


      * imprimimos linea de DIVISION
           MOVE  WS-RES-SEPARADOR TO WS-SAL-RESUMENES.
           PERFORM 2800-IMPRIMIR-RESUMEN
              THRU 2800-IMPRIMIR-RESUMEN-FIN.

171123     INITIALIZE WS-SAL-RESUMENES

           MOVE  WS-RESUMEN-SALDO-ARS  TO  WS-RES-TOT-PESOS.
           MOVE  WS-RESUMEN-SALDO-USD  TO  WS-RES-TOT-DOLARES.
           MOVE  WS-RES-TOTALES  TO WS-SAL-RESUMENES.

171123     PERFORM 2800-IMPRIMIR-RESUMEN
171123        THRU 2800-IMPRIMIR-RESUMEN-FIN.

171123 2720-MOSTRAR-SALDOS-RESUMEN-FIN.
           EXIT.

      *----------------------------------------------------------------*
171123 2740-CALCULAR-MIN-RESUMEN.

      * Hacemos el Cambio de Dolares a Pesos Y Totalizamos los pesos
201123     MOVE WS-RESUMEN-SALDO-USD TO WS-RESUMEN-PESOSxTIPO-DE-CAMBIO
201123     MULTIPLY WS-TIPO-DE-CAMBIO BY WS-RESUMEN-PESOSxTIPO-DE-CAMBIO
201123     MOVE WS-RESUMEN-SALDO-ARS TO WS-RESUMEN-PAGO-MINIMO
201123     ADD WS-RESUMEN-PESOSxTIPO-DE-CAMBIO TO WS-RESUMEN-PAGO-MINIMO
201123     MULTIPLY 0,05 BY WS-RESUMEN-PAGO-MINIMO

      * Imprimimos el renglon de pago minimo
           MOVE WS-RESUMEN-PAGO-MINIMO  TO WS-RES-MIN-PESOS.
           MOVE WS-RES-PAGOMIN TO WS-SAL-RESUMENES.

171123     PERFORM 2800-IMPRIMIR-RESUMEN
171123        THRU 2800-IMPRIMIR-RESUMEN-FIN.
171123 2740-CALCULAR-MIN-RESUMEN-FIN.
171123     EXIT.

      *----------------------------------------------------------------*
171123 2760-VER-LIMITE-RESUMEN.

171123
           PERFORM 2770-IMP-LIMITE-COMPRA
              THRU 2770-IMP-LIMITE-COMPRA-FIN.

      * Imprimimos las lines de # final del resumen
           MOVE WS-RES-NUMERALES TO WS-SAL-RESUMENES
           PERFORM 2800-IMPRIMIR-RESUMEN
              THRU 2800-IMPRIMIR-RESUMEN-FIN.

171123 2760-VER-LIMITE-RESUMEN-FIN.
171123     EXIT.

      *-----------------------------------------------------------------
       2770-IMP-LIMITE-COMPRA.

           MULTIPLY  WS-RESUMEN-SALDO-USD  BY  WS-TIPO-DE-CAMBIO
                          GIVING  WS-MONTO-TOTAL.

           ADD   WS-RESUMEN-SALDO-ARS  TO  WS-MONTO-TOTAL.


           IF WS-MONTO-TOTAL > LK-LIMITE-TARJETA
               THEN
      *  Imprimo linea de asteriscos superior
                  MOVE WS-ASTERISCOS TO WS-SAL-RESUMENES
                  PERFORM 2800-IMPRIMIR-RESUMEN
                  THRU 2800-IMPRIMIR-RESUMEN-FIN
      * Imprimo el mensaje si supero el limite
                  MOVE WS-MENSAJE TO WS-RES-MENSAJE
                  MOVE WS-MENSAJE-ASTERISCOS TO WS-SAL-RESUMENES
                  PERFORM 2800-IMPRIMIR-RESUMEN
                  THRU 2800-IMPRIMIR-RESUMEN-FIN
      * Imprimo la linea de asteriscos inferior
                  MOVE WS-ASTERISCOS TO WS-SAL-RESUMENES
                  PERFORM 2800-IMPRIMIR-RESUMEN
                  THRU 2800-IMPRIMIR-RESUMEN-FIN

           END-IF.
      * Dejo una linea en blanco para el proximo resumen
           MOVE SPACES TO WS-SAL-RESUMENES
           PERFORM 2800-IMPRIMIR-RESUMEN
              THRU 2800-IMPRIMIR-RESUMEN-FIN.

           INITIALIZE WS-MONTO-TOTAL.

       2770-IMP-LIMITE-COMPRA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2800-IMPRIMIR-RESUMEN.

           WRITE WS-SAL-RESUMENES.

       2800-IMPRIMIR-RESUMEN-FIN.
           EXIT.

      *----------------------------------------------------------------*
      * Aca imprimimos el encabezado de cada resumen con los datos
      * proporcionados con la rutina MAESTARJ

       2810-IMP-RES-ENCABEZADO.

           MOVE LK-NOMBRE TO WS-RES-APELLIDO.
           MOVE LK-APELLIDO TO WS-RES-NOMBRE.
           MOVE WS-CURRENT-DAY TO WS-CURRENT-DIA.
           MOVE WS-CURRENT-MONTH TO WS-CURRENT-MES.
           MOVE WS-CURRENT-YEAR TO WS-CURRENT-ANIO.
           MOVE  WS-RES-ENCABEZADO-1 TO WS-SAL-RESUMENES.
           WRITE WS-SAL-RESUMENES.


           MOVE LK-DIRECCION  TO  WS-RES-DIRECCION.
           MOVE WS-ENT-NUM-CUENTA  TO WS-RES-NUM-CUENTA.
           MOVE  WS-RES-ENCABEZADO-2 TO WS-SAL-RESUMENES.
           WRITE WS-SAL-RESUMENES.


           MOVE LK-COD-POSTAL  TO  WS-RES-COD-POSTAL.
           MOVE LK-NUM-TARJETA  TO  WS-RES-NUM-TARJ.
           MOVE  WS-RES-ENCABEZADO-3 TO WS-SAL-RESUMENES.
           WRITE WS-SAL-RESUMENES.

           MOVE LK-LIMITE-TARJETA  TO WS-RES-LIM-COMPRA.
      *    MULTIPLY WS-LIM-COMPRA BY 1000 GIVING WS-RES-LIM-COMPRA.
           MOVE  WS-RES-ENCABEZADO-4 TO WS-SAL-RESUMENES.
           WRITE WS-SAL-RESUMENES.


           MOVE  WS-RES-SEPARADOR TO WS-SAL-RESUMENES.
             WRITE WS-SAL-RESUMENES.

           MOVE  WS-RES-DESCRIPCION TO WS-SAL-RESUMENES.
           WRITE WS-SAL-RESUMENES.

           MOVE  WS-RES-SEPARADOR TO WS-SAL-RESUMENES.
           WRITE WS-SAL-RESUMENES.


       2810-IMP-RES-ENCABEZADO-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-CONSUMOS
                 SAL-RESUMENES
                 SAL-ERRORES

           IF NOT FS-CONSUMOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO CONSUMOS: ' FS-CONSUMOS
           END-IF.

           IF NOT FS-RESUMENES-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO RESUMENES: ' FS-RESUMENES
           END-IF.

           IF NOT FS-ERRORES-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ERRORES: ' FS-ERRORES
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       END PROGRAM TP02EJ01.
