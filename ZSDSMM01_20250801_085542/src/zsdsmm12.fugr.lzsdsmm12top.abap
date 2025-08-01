FUNCTION-POOL ZSDSMM12.                     "MESSAGE-ID ..

* INCLUDE LZSDSMM12D...                      " Local class definition


CONSTANTS : BEGIN OF GC_CON,
              SEARCH_COSTCENTER TYPE CHAR10     VALUE 'SCOST',
              SEARCH_LOCATION   TYPE CHAR10     VALUE 'SLOCL',
              SEARCH_PERSON     TYPE CHAR10     VALUE 'SPERS',
              SEARCH_MAT        TYPE CHAR10     VALUE 'SMAT',
              GET_DESC          TYPE CHAR10     VALUE 'ADESC',
              GET_DATA          TYPE CHAR10     VALUE 'GET',
              INSERT_DATA       TYPE CHAR10     VALUE 'INSER',
              REPORT            TYPE CHAR10     VALUE 'REPOT',
              GET_ECC           TYPE CHAR10     VALUE 'OLDNO',
              GET_AUTO          TYPE CHAR10     VALUE 'CAUTO',
              CONA              TYPE CSKT-KOKRS VALUE '1000',
              I                 TYPE CHAR1      VALUE 'I',
              EQ                TYPE CHAR2      VALUE 'EQ',
              CP                TYPE CHAR2      VALUE 'CP',
              START             TYPE CHAR1      VALUE '*',
              E                 TYPE CHAR1      VALUE 'E',
              S                 TYPE CHAR1      VALUE 'S',
            END OF GC_CON.
