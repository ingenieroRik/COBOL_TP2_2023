      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRUEBA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.



       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMERO       PIC ZZZ.ZZ9,99 VALUE 123,45.
       01 WS-CADENA       PIC X(10).

       PROCEDURE DIVISION.
       MOVE FUNCTION NUMVAL (WS-NUMERO) TO WS-CADENA.
       DISPLAY 'Número antes de la conversión: ' WS-NUMERO.
       DISPLAY 'Cadena sin formato: ' WS-CADENA.



       DISPLAY 'Cadena con formato: ' WS-CADENA.

        DISPLAY 'La cadena es: ' WS-CADENA.
       STOP RUN.
