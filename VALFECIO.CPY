          05 WS-ENTRADA.
             10 WS-FEC-I.
                15 WS-DD-I                     PIC 9(02).
                15 WS-MM-I                     PIC 9(02).
                15 WS-AAAA-I                   PIC 9(04).
          05 WS-SALIDA.
             10 WS-DESCUENTO-O                 PIC X(01)   VALUE SPACES.
             10 WS-VALIDACION-O                PIC X(01)   VALUE SPACES.
             10 WS-MOTIVO-ERROR-O.
                15 WS-COD-ERROR-O              PIC X(20)   VALUE SPACES.
                15 WS-DES-ERROR-O              PIC X(100)  VALUE SPACES.
