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
