      ******************************************************************
      * Authors: Noemï¿½ Berge, Claudia Perdiguera, Ricardo Balsimelli,
      *          Ricardo Garcï¿½a, Senen Urdaneta.
      * Date: 04/10/2023
      * Purpose: TP1 - AULA 3 - GRUPO 1
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
       01 WS-SAL-PROMEDIOS                     PIC X(84).

       FD SAL-ERRORES.
          01 WS-SAL-ERRORES                      PIC X(88).



      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    FORMATO DE LOS ARCHIVOS DE ENTRADA/SALIDA                           *
      *----------------------------------------------------------------*
       01 WS-VAL-FECHA.
           COPY VALFECIO.


       01 WS-MAESTRO-TARJETAS-REG.
          05 WS-NUM-CUENTA                       PIC 9(10).
          05 WS-NUM-TARJETA                      PIC X(19).
          05 WS-NOMBRE                           PIC X(20).
          05 WS-APELLIDO                         PIC X(20).
          05 WS-DIRECCION                        PIC X(40).
          05 WS-COD-POSTAL                       PIC 9(04).
          05 WS-MONEDA-TARJETA                   PIC X(03).
               88 WS-DOLARES                      VALUE 'USD'.
               88 WS-PESOS                        VALUE 'ARS'.
          05 WS-LIMITE-TARJETA                   PIC 9(04)V9(02).

      *    salida de datos para el modulo MAESTARJ
       01 WS-SALIDA-TARJETA.
         05 WS-SAL-NUM-TARJETA  PIC X(19) VALUE '9012-3456-1234-5678'.


      *----------------------------------------------------------------*
      *    VARIABLES FILE STATUS  ENTRADA/SALIDA                       *
      *----------------------------------------------------------------*
       01 FS-STATUS.
      *----------------------------------------------------------------*
      *   ** FILE STATUS DE CONSUMOS                                    *
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
      *   ** FILE STATUS DE ERROR                                  *
      *----------------------------------------------------------------*
          05 FS-ERRORES                  PIC X(2).
             88 FS-ERRORES-OK                        VALUE '00'.
             88 FS-ERRORES-EOF                       VALUE '10'.

      *----------------------------------------------------------------*
      *    DECLARACION DE VARIABLES DEL PROGRAMA                       *
      *----------------------------------------------------------------*


       77 WS-TIPO-DE-CAMBIO                PIC 9(04)V9(02) VALUE 365,50.
       77 WS-MENSAJE                       PIC X(44)
                   VALUE '"Este mes ha superado su límite de compra."'.
       77 WS-DESCUENTO                     PIC 9(02) VALUE 10.
       77 WS-PAGO-MINIMO                   PIC 9(02) VALUE 5.

      *----------------------------------------------------------------*



      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           CALL 'MAESTARJ' USING   WS-SAL-NUM-TARJETA,
                              WS-MAESTRO-TARJETAS-REG.
           DISPLAY ' VOLVIO DEL CALL '
           DISPLAY ' ENCONTRE : ' WS-NOMBRE.

      *    CALL 'CLVALFEC' USING WS-.

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
      *    INITIALIZE WS-VAR-AUX.


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
