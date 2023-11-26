
       01 WS-SALIDA-RESUMENES.
          05 WS-RES-SEPARADOR             PIC X(91) VALUE ALL '-'.

          05 WS-RES-ENCABEZADO-1.
             10 FILLER                    PIC X(9) VALUE 'Titular:'.
             10 WS-RES-APELLIDO-NOMBRE    PIC X(42)  VALUE SPACES.
             10 FILLER                    PIC X(19)
                                            VALUE 'Fecha de emisión : '.
             10 WS-RES-FECHA.
               15  WS-CURRENT-DIA        PIC 9(02).
               15  FILLER                PIC X VALUE '/'.
               15  WS-CURRENT-MES        PIC 9(02).
               15  FILLER                PIC X VALUE '/'.
               15 WS-CURRENT-ANIO         PIC 9(04).

          05 WS-RES-ENCABEZADO-2.
             10 WS-RES-DIRECCION          PIC X(38).
             10 FILLER                    PIC X(13) VALUE SPACES.
             10 FILLER                    PIC X(19)
                                           VALUE 'Numero de cuenta : '.
             10 WS-RES-NUM-CUENTA         PIC 9(10).
             10 FILLER                    PIC X(11) VALUES SPACES.

          05 WS-RES-ENCABEZADO-3.
             10 FILLER                    PIC x(04) VALUE 'CP: '.
             10 WS-RES-COD-POSTAL         PIC 9(04).
             10 FILLER                    PIC X(43) VALUE SPACES.
             10 FILLER                    PIC X(19)
                                           VALUE 'Numero de tarjeta: '.
             10 WS-RES-NUM-TARJ           PIC X(19).

          05 WS-RES-ENCABEZADO-4.
             10 FILLER                    PIC X(51) VALUE SPACES.
             10 FILLER                    PIC X(19)
                                           VALUE 'Limite de compra : '.
             10 FILLER                    PIC XX VALUE '$ '.
             10 WS-RES-LIM-COMPRA         PIC ZZ.ZZZ.ZZ9,99.
             10 FILLER                    PIC X(08) VALUE SPACES.


          05 WS-RES-DESCRIPCION.
             10 FILLER                    PIC X(12) VALUE 'Fecha'.
             10 FILLER                    PIC X(32) VALUE 'Detalle'.
             10 FILLER                    PIC X(5) VALUE 'Cuota'.
             10 FILLER                    PIC X(22)
                                           VALUE '              Pesos '.
             10 FILLER                    PIC X(19)
                                            VALUE '            Dólares'.


      * Aqui se debe llenar cada linea con los detalles de compras
      * Esta linea se repite varias veces
          05 WS-RES-COMPRAS.
            10 WS-RES-FECHA-COMPRA.
               15  WS-RES-DIA-COMP          PIC 9(02).
               15  FILLER                   PIC X VALUE '/'.
               15  WS-RES-MES-COMP          PIC 9(02).
               15  FILLER                   PIC X VALUE '/'.
               15 WS-RES-ANIO-COMP          PIC 9(04).
               15 FILLER                    PIC X VALUE SPACE.


            10 WS-RES-DETALLE             PIC X(33).
            10 WS-RES-CUOTA               PIC X(02).
            10 FILLER                     PIC X VALUE '/'.
            10 WS-RES-CANT-CUOTAS         PIC X(02).
            10 FILLER                     PIC X(9) VALUE SPACE.
      *  Al poner zz.zzz.zzz,zz queda en blanco al imprimir
            10 WS-RES-PESOS               PIC ZZ.ZZZ.ZZZ,ZZ.
            10 FILLER                     PIC X(7) VALUE SPACE.
            10 WS-RES-DOLARES             PIC ZZ.ZZZ.ZZZ,ZZ.

          05 WS-RES-DESCUENTO.
             10 FILLER                    PIC X(11) VALUES SPACES.
             10 FILLER                    PIC X(38)
                      VALUE 'Descuento 10,00%                   '.
             10 FILLER                     PIC X(8)  VALUE SPACES.

      * al poner --.---.---,-- pone el signo +/- al inicio
             10 WS-RES-VALOR-DTO-ARS       PIC ---.---.---,--
                                                     VALUE ZEROES.
             10 FILLER                     PIC X(6)  VALUE SPACES.
             10 WS-RES-VALOR-DTO-USD       PIC ---.---.---,--
                                                     VALUE ZEROES.

          05 WS-RES-TOTALES.
             10 FILLER                     PIC X(32) VALUE SPACES.
             10 FILLER                     PIC X(15)
                                             VALUE'Saldo actual: $'.
             10 FILLER                     PIC X(8) VALUE SPACE.
             10 WS-RES-TOT-PESOS           PIC ZZ.ZZZ.ZZZ,ZZ.
             10 FILLER                     PIC X(06) VALUE '   u$s'.
             10 FILLER                     PIC X(4) VALUE SPACE.
             10 WS-RES-TOT-DOLARES         PIC ZZ.ZZZ.ZZZ,ZZ.

          05 WS-RES-PAGOMIN.
             10 FILLER                    PIC X(32) VALUE SPACES.
             10 FILLER                    PIC X(15)
                                            VALUE'Pago mínimo : $'.
             10 FILLER                     PIC X(8) VALUE SPACE.
             10 WS-RES-MIN-PESOS          PIC ZZ.ZZZ.ZZZ,ZZ.
             10 FILLER                    PIC X(25) VALUE SPACES.

          05 WS-ASTERISCOS.
             10 FILLER                    PIC X(24) VALUE SPACES.
             10 FILLER                    PIC X(44) VALUE ALL '*'.
             10 FILLER                    PIC X(23) VALUE SPACES.

          05 WS-MENSAJE-ASTERISCOS.
            10 FILLER                    PIC X(24) VALUE SPACES.
            10 WS-RES-MENSAJE            PIC X(44).
            10 FILLER                    PIC X(23) VALUE SPACES.


          05 WS-RES-NUMERALES            PIC X(91) VALUES ALL '#'.
