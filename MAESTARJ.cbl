      ******************************************************************
      * Authors: NoemI Berge, Claudia Perdiguera, Ricardo Balsimelli,
      *          Ricardo GarcIa, Senen Urdaneta.
      * Date: 04/10/2023
      * Purpose: TP1 - AULA 3 - GRUPO 1
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
          05 WS-MT-LIMITE-TARJETA                   PIC 9(04)V9(02).


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
       01 LK-MAESTRO-TARJETAS-REG.
          05 LK-NUM-CUENTA                       PIC 9(10).
          05 LK-NUM-TARJETA                      PIC X(19).
          05 LK-NOMBRE                           PIC X(20).
          05 LK-APELLIDO                         PIC X(20).
          05 LK-DIRECCION                        PIC X(40).
          05 LK-COD-POSTAL                       PIC 9(04).
          05 LK-MONEDA-TARJETA                   PIC X(03).
               88 LK-DOLARES                      VALUE 'USD'.
               88 LK-PESOS                        VALUE 'ARS'.
          05 LK-LIMITE-TARJETA                   PIC 9(04)V9(02).


      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING LK-ENTRADA, LK-MAESTRO-TARJETAS-REG.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.



           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           PERFORM 1100-ABRIR-ARCHIVOS
              THRU 1100-ABRIR-ARCHIVOS-FIN.

           PERFORM 1200-INICIALIZAR-VARIABLES
              THRU 1200-INICIALIZAR-VARIABLES-FIN.

           PERFORM 1300-BUSCAR-TARJETA
              THRU 1300-BUSCAR-TARJETA-FIN.


       1000-INICIAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVOS.



           PERFORM 1140-ABRIR-ENT-MAESTRO-TARJ
              THRU 1140-ABRIR-ENT-MAESTRO-TARJ-FIN.


       1100-ABRIR-ARCHIVOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1200-INICIALIZAR-VARIABLES.
      *    INITIALIZE WS-VAR-AUX.


       1200-INICIALIZAR-VARIABLES-FIN.
           EXIT.


      *----------------------------------------------------------------*
       1140-ABRIR-ENT-MAESTRO-TARJ.

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

       1140-ABRIR-ENT-MAESTRO-TARJ-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1300-BUSCAR-TARJETA.

      *    DISPLAY 'INGRESA NUMERO DE TARJETA CON GUIONES: '
      *    ACCEPT WS-NUM-TARJETA

           MOVE LK-ENTRADA  TO  WS-MT-NUM-TARJETA

           READ ENT-MAESTRO-TARJETAS KEY IS WS-MT-NUM-TARJETA.

           EVALUATE TRUE
               WHEN FS-MAESTRO-TARJETAS-FILE-OK
                    PERFORM 2105-MOSTRAR-DATOS
                       THRU 2105-MOSTRAR-DATOS-FIN
               WHEN FS-MAESTRO-TARJETAS-CLAVE-INV
                   DISPLAY "ERROR: EL ID INGRESADO ES INVALIDO"
               WHEN FS-MAESTRO-TARJETAS-CLAVE-DUP
                   DISPLAY "ERROR: EL ID INGRESADO SE ENCUENTRA "-
                           "DUPLICADO"
               WHEN FS-MAESTRO-TARJETAS-CLAVE-NFD
                   DISPLAY "ERROR: EL ID INGRESADO NO EXISTE"
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-MAESTRO-TARJETAS
           END-EVALUATE.



       1300-BUSCAR-TARJETA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2105-MOSTRAR-DATOS.

           DISPLAY 'NUMERO DE TARJETA : ' WS-MT-NUM-TARJETA.
           DISPLAY 'NUMERO DE CUENTA : '  WS-MT-NUM-CUENTA.
           DISPLAY 'NOMBRE : '            WS-MT-NOMBRE.
           DISPLAY 'APELLIDO : '          WS-MT-APELLIDO.
           DISPLAY 'LIMITE DE TARJETA: '  WS-MT-LIMITE-TARJETA.


           MOVE WS-MT-NOMBRE TO LK-NOMBRE.

       2105-MOSTRAR-DATOS-FIN.
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
