*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMC009......................................*
DATA:  BEGIN OF STATUS_ZSDSMMC009                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMC009                    .
CONTROLS: TCTRL_ZSDSMMC009
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDSMMC009                    .
TABLES: ZSDSMMC009                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
