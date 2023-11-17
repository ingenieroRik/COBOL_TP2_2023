      ******************************************************************
      * Authors: Noemï¿½ Berge, Claudia Perdiguera, Ricardo Balsimelli,
      *          Ricardo Garcï¿½a, Senen Urdaneta.
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
          05 WS-DETALLE                               PIC X(32).
          05 WS-ENT-NUMERO-ACTUAL                     PIC 9(02).
          05 WS-ENT-CANT-CUOTAS                       PIC 9(02).
          05 WS-ENT-MONEDA                            PIC X(03).
               88 WS-MT-DOLARES                         VALUE 'USD'.
               88 WS-MT-PESOS                           VALUE 'ARS'.
          05 WS-ENT-IMPORTE                           PIC 9(08)V9(02).

       FD SAL-RESUMENES.
       01 WS-SAL-RESUMENES                            PIC X(84).

       FD SAL-ERRORES.
          01 WS-SAL-ERRORES                           PIC X(88).

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    FORMATO DE LOS ARCHIVOS DE ENTRADA/SALIDA                           *
      *----------------------------------------------------------------*
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
           02 WS-RESUMEN-SALDO-ARG         PIC 9(08)V9(02) VALUE ZEROES.
           02 WS-RESUMEN-SALDO-USD         PIC 9(08)V9(02) VALUE ZEROES.
           02 WS-RESUMEN-PAGO-MINIMO       PIC 9(08)V9(02) VALUE ZEROES.
           02 WS-RESUMEN-TOTAL-CONSUMOS    PIC 9(06)       VALUE ZEROES.
           02 WS-RESUMEN-TOTAL-TARJETAS    PIC 9(06)       VALUE ZEROES.
           02 WS-RESUMEN-TOTAL-RESUMENES   PIC 9(06)       VALUE ZEROES.
           02 WS-RESUMEN-TOTAL-ERRORES     PIC 9(06)       VALUE ZEROES.

       77 WS-TIPO-DE-CAMBIO                PIC 9(04)V9(02) VALUE 365,50.
       77 WS-MENSAJE                       PIC X(44)
                   VALUE '"Este mes ha superado su límite de compra."'.
       77 WS-DESCUENTO                     PIC 9(02) VALUE 10.
       77 WS-PAGO-MINIMO                   PIC 9(02) VALUE 5.


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

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
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

           PERFORM 2300-VALIDAR-TARJETA
              THRU 2300-VALIDAR-TARJETA-FIN.

           PERFORM 2400-VALIDAR-FECHA
              THRU 2400-VALIDAR-FECHA-FIN.

           PERFORM 2600-ARMAR-RESUMENES
              THRU 2600-ARMAR-RESUMENES-FIN.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2200-LEER-CONSUMOS.

           INITIALIZE LK-MAESTRO-TARJETAS-REG.
           INITIALIZE WS-SALIDA

           READ ENT-CONSUMOS.

             EVALUATE TRUE
               WHEN FS-CONSUMOS-OK
                    ADD 1  TO   WS-RESUMEN-TOTAL-CONSUMOS

141123              IF WS-RESUMEN-TARJETA <> WS-ENT-NUM-TARJETA THEN
                       ADD  1               TO WS-RESUMEN-TOTAL-TARJETAS
                    END-IF

141123              MOVE WS-ENT-NUM-TARJETA TO WS-RESUMEN-TARJETA
141123              MOVE WS-ENT-NUM-TARJETA TO LK-ENT-NUM-TARJETA
141123              MOVE WS-ENT-DIA         TO WS-DD-I
141123              MOVE WS-ENT-MES         TO WS-MM-I
141123              MOVE WS-ENT-ANIO        TO WS-AAAA-I

               WHEN FS-CONSUMOS-EOF
                    DISPLAY '#RESUMENES: ' WS-RESUMEN-TOTAL-RESUMENES
                    DISPLAY '#TARJETAS: '  WS-RESUMEN-TOTAL-TARJETAS
                    DISPLAY '#CONSUMOS: '  WS-RESUMEN-TOTAL-CONSUMOS
                    DISPLAY '#ERRORES: '   WS-RESUMEN-TOTAL-ERRORES

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

      *    INITIALIZE LK-MAESTRO-TARJETAS-REG.

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

           MOVE WS-ENT-CONSUMOS TO WS-SAL-ERRORES.

           PERFORM 2500-GRABAR-ERRORES
              THRU 2500-GRABAR-ERRORES-FIN.

       2310-ERROR-TARJETA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2400-VALIDAR-FECHA.

      *     INITIALIZE WS-SALIDA.

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

           MOVE WS-SALIDA TO WS-SAL-ERRORES.

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

           MOVE  WS-ENT-CONSUMOS TO WS-SAL-RESUMENES.

           PERFORM 2700-IMPRIMIR-RESUMEN
              THRU 2700-IMPRIMIR-RESUMEN-FIN.

       2600-ARMAR-RESUMENES-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2700-IMPRIMIR-RESUMEN.

           WRITE WS-SAL-RESUMENES.
           ADD 1 TO WS-RESUMEN-TOTAL-RESUMENES.

       2700-IMPRIMIR-RESUMEN-FIN.
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

      *     IF NOT FS-MAESTRO-TARJETAS-FILE-OK
      *        DISPLAY 'ERROR AL CERRAR ARCHIVO MAESTRO TARJETAS: '
      *                                              FS-MAESTRO-TARJETAS
      *     END-IF.


       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       END PROGRAM TP02EJ01.
