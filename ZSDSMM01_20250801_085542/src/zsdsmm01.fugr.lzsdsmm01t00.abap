*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMC001......................................*
DATA:  BEGIN OF STATUS_ZSDSMMC001                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMC001                    .
CONTROLS: TCTRL_ZSDSMMC001
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMC001                    .
TABLES: ZSDSMMC001                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
