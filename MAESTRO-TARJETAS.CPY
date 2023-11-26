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
              10 LK-COD-ERROR-O            PIC X(02)       VALUE SPACES.
              10 LK-DES-ERROR-O            PIC X(40)      VALUE SPACES.
