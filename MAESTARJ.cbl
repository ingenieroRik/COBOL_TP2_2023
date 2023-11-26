      ******************************************************************
      * Authors: NoemI Berge, Claudia Perdiguera, Ricardo Balsimelli,
      *          Ricardo GarcIa, Senen Urdaneta.
      * Date: 14/11/2023
      * Purpose: TP2 - AULA 3 - GRUPO 1
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAESTARJ.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENT-MAESTRO-TARJETAS
           ASSIGN TO '../MAESTRO-TARJETAS.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-MAESTRO-TARJETAS
           RECORD KEY IS WS-MT-NUM-TARJETA.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENT-MAESTRO-TARJETAS.
       01 WS-ENT-MAESTRO-TARJETAS-REG.
          05 WS-MT-NUM-CUENTA                       PIC 9(10).
          05 WS-MT-NUM-TARJETA                      PIC X(19).
          05 WS-MT-NOMBRE                           PIC X(20).
          05 WS-MT-APELLIDO                         PIC X(20).
          05 WS-MT-DIRECCION                        PIC X(40).
          05 WS-MT-COD-POSTAL                       PIC 9(04).
          05 WS-MT-MONEDA-TARJETA                   PIC X(03).
               88 WS-MT-DOLARES                      VALUE 'USD'.
               88 WS-MT-PESOS                        VALUE 'ARS'.
          05 WS-MT-LIMITE-TARJETA                   PIC 9(08)V9(02).


       01 FS-STATUS.
      *----------------------------------------------------------------*
      *   ** FILE STATUS DE MAESTRO-TARJETAS                           *
      *----------------------------------------------------------------*
          05 FS-MAESTRO-TARJETAS                  PIC X(2).
             88 FS-MAESTRO-TARJETAS-FILE-OK            VALUE '00'.
             88 FS-MAESTRO-TARJETAS-FILE-EOF           VALUE '10'.
             88 FS-MAESTRO-TARJETAS-FILE-NFD           VALUE '35'.
             88 FS-MAESTRO-TARJETAS-CLAVE-INV          VALUE '21'.
             88 FS-MAESTRO-TARJETAS-CLAVE-DUP          VALUE '22'.
             88 FS-MAESTRO-TARJETAS-CLAVE-NFD          VALUE '23'.

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       77 WS-NUM-TARJETA                             PIC X(19).

      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01 LK-ENTRADA.
      *   Area de datos de Entrada
           05 LK-ENT-NUM-TARJETA                     PIC X(19).

      *   Area de datos de Salida
           COPY MAESTRO-TARJETAS.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING LK-ENTRADA, LK-MAESTRO-TARJETAS-REG.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           PERFORM 2000-PROCESAR-PROGRAMA
              THRU 2000-PROCESAR-PROGRAMA-FIN.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

           GOBACK.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           PERFORM 1100-ABRIR-ARCHIVOS
              THRU 1100-ABRIR-ARCHIVOS-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVOS.

           PERFORM 1200-ABRIR-ENT-MAESTRO-TARJ
              THRU 1200-ABRIR-ENT-MAESTRO-TARJ-FIN.

       1100-ABRIR-ARCHIVOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1200-ABRIR-ENT-MAESTRO-TARJ.

           OPEN INPUT ENT-MAESTRO-TARJETAS.

           EVALUATE TRUE
               WHEN FS-MAESTRO-TARJETAS-FILE-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO MAESTRO TARJETAS'
                    DISPLAY 'FILE STATUS: ' FS-MAESTRO-TARJETAS
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1200-ABRIR-ENT-MAESTRO-TARJ-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.

           PERFORM 2300-BUSCAR-TARJETA
              THRU 2300-BUSCAR-TARJETA-FIN.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2300-BUSCAR-TARJETA.

           INITIALISE LK-MAESTRO-TARJETAS-REG.
           MOVE LK-ENT-NUM-TARJETA  TO  WS-MT-NUM-TARJETA.

           READ ENT-MAESTRO-TARJETAS KEY IS WS-MT-NUM-TARJETA.

           EVALUATE TRUE
               WHEN FS-MAESTRO-TARJETAS-FILE-OK
                    MOVE WS-ENT-MAESTRO-TARJETAS-REG
                      TO LK-MAESTRO-TARJETAS-REG
                   CONTINUE

               WHEN FS-MAESTRO-TARJETAS-CLAVE-INV
                    MOVE FS-MAESTRO-TARJETAS        TO LK-COD-ERROR-O
                    MOVE "ERROR: EL ID INGRESADO ES INVALIDO"
                                                    TO LK-DES-ERROR-O
               WHEN FS-MAESTRO-TARJETAS-CLAVE-DUP
                    MOVE FS-MAESTRO-TARJETAS        TO LK-COD-ERROR-O
                    MOVE "ERROR: EL ID INGRESADO ESTA DUPLICADO"
                                                    TO LK-DES-ERROR-O
               WHEN FS-MAESTRO-TARJETAS-CLAVE-NFD
                    MOVE FS-MAESTRO-TARJETAS        TO LK-COD-ERROR-O
                    MOVE "ERROR: EL ID INGRESADO NO EXISTE"
                                                    TO LK-DES-ERROR-O
               WHEN OTHER
                    MOVE FS-MAESTRO-TARJETAS        TO LK-COD-ERROR-O
                    MOVE 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                                                    TO LK-DES-ERROR-O
           END-EVALUATE.

       2300-BUSCAR-TARJETA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE     ENT-MAESTRO-TARJETAS.

           IF NOT FS-MAESTRO-TARJETAS-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO MAESTRO TARJETAS: '
                                                     FS-MAESTRO-TARJETAS
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       EXIT PROGRAM.
