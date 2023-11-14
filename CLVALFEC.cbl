      ******************************************************************
      * Authors: NoemI Berge, Claudia Perdiguera, Ricardo Balsimelli,
      *          Ricardo GarcIa, Senen Urdaneta.
      * Date: 04/10/2023
      * Purpose: TP1 - AULA 3 - GRUPO 1
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.

       PROGRAM-ID. CLVALFEC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.


       DATA DIVISION.


       WORKING-STORAGE SECTION.



       01 WS-VAR-AUX.
          05 WS-VALIDAR-DATOS                 PIC X(01) VALUE SPACES.
             88 VALIDACION-OK                 VALUE 'S'.
             88 VALIDACION-NOTOK              VALUE 'N'.

       LINKAGE SECTION.
       01 LK-VAL-FECHA.
          05 LK-ENTRADA.
             10 LK-FEC-I.
                15 LK-DD-I                     PIC 9(02).
                15 LK-MM-I                     PIC 9(02).
                15 LK-AAAA-I                   PIC 9(04).
          05 LK-SALIDA.
             10 LK-DESCUENTO-O                 PIC X(01).
             10 LK-VALIDACION-O                PIC X(01).
             10 LK-MOTIVO-ERROR-O.
                15 LK-COD-ERROR-O              PIC X(20).
                15 LK-DES-ERROR-O              PIC X(100).


       PROCEDURE DIVISION USING LK-VAL-FECHA.
      *----------------------------------------------------------------*

           PERFORM 1000-
              THRU 1000--EXIT.



           STOP RUN.
